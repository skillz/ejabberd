%%%----------------------------------------------------------------------
%%% File    : ejabberd_rdbms.erl
%%% Author  : Mickael Remond <mickael.remond@process-one.net>
%%% Purpose : Manage the start of the database modules when needed
%%% Created : 31 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2019   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_rdbms).

-behaviour(supervisor).
-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-export([start_link/0, init/1, opt_type/1, config_reloaded/0, start_host/1, stop_host/1]).

-export([add_sql_cache_pid/2, remove_cache_pid/2, get_cache_pid/1, refresh_cache_pid/1,
  put_max_room_timestamp_cache_item/3, get_max_room_timestamp_cache_item/2,
  get_subscribed_rooms_cache_key/2, get_subscribed_rooms_cache_item/1,
  put_subscribed_rooms_cache_item/2, invalidate_subscribed_rooms/2,
  remove_subscribed_rooms_by_room/2]).

-include("logger.hrl").

-define(SQL_CACHES, [
  {get_subscribed_rooms_cache, 10000},
  {get_max_room_timestamp_cache, 10000}
]).

-record(sql_cache, {name :: atom(), pid  :: pid()}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    file:delete(ejabberd_sql:freetds_config()),
    file:delete(ejabberd_sql:odbc_config()),
    file:delete(ejabberd_sql:odbcinst_config()),
    ejabberd_hooks:add(host_up, ?MODULE, start_host, 20),
    ejabberd_hooks:add(host_down, ?MODULE, stop_host, 90),
    ejabberd_hooks:add(config_reloaded, ?MODULE, config_reloaded, 20),

    %% sql cache pids
    mnesia:create_table(ejabberd_cache:get_node_table_name("sql_cache"), [{attributes, [ name, pid ]}, {type, set}, {local_content, true}]),

    {ok, {{one_for_one, 10, 1}, get_specs() ++ sql_cache_child_specs()}}.

sql_cache_child_specs() ->
  lists:map(
    fun({ CacheName, MaxCacheSize }) ->
      {CacheName, {ejabberd_cache, start_link, [CacheName, MaxCacheSize]}, transient, 2000, worker, [?MODULE]}
    end,
    ?SQL_CACHES
  )
.

-spec get_specs() -> [supervisor:child_spec()].
get_specs() ->
    lists:flatmap(
      fun(Host) ->
        case get_spec(Host) of
      {ok, Spec} -> [Spec];
      undefined -> []
        end
      end, ejabberd_config:get_myhosts()).

-spec get_spec(binary()) -> {ok, supervisor:child_spec()} | undefined.
get_spec(Host) ->
    case needs_sql(Host) of
  {true, App} ->
      ejabberd:start_app(App),
      SupName = gen_mod:get_module_proc(Host, ejabberd_sql_sup),
      {ok, {SupName, {ejabberd_sql_sup, start_link, [Host]},
      transient, infinity, supervisor, [ejabberd_sql_sup]}};
  false ->
      undefined
    end.

-spec config_reloaded() -> ok.
config_reloaded() ->
    lists:foreach(fun reload_host/1, ejabberd_config:get_myhosts()).

-spec start_host(binary()) -> ok.
start_host(Host) ->
    case get_spec(Host) of
  {ok, Spec} ->
      case supervisor:start_child(?MODULE, Spec) of
    {ok, _PID} ->
        ok;
    {error, {already_started, _}} ->
        ok;
    {error, _} = Err ->
        erlang:error(Err)
      end;
  undefined ->
      ok
    end.

-spec stop_host(binary()) -> ok.
stop_host(Host) ->
    SupName = gen_mod:get_module_proc(Host, ejabberd_sql_sup),
    supervisor:terminate_child(?MODULE, SupName),
    supervisor:delete_child(?MODULE, SupName),
    ok.

-spec reload_host(binary()) -> ok.
reload_host(Host) ->
    case needs_sql(Host) of
  {true, _} -> ejabberd_sql_sup:reload(Host);
  false -> ok
    end.

%% Returns {true, App} if we have configured sql for the given host
needs_sql(Host) ->
    LHost = jid:nameprep(Host),
    case ejabberd_config:get_option({sql_type, LHost}, undefined) of
        mysql -> {true, p1_mysql};
        pgsql -> {true, p1_pgsql};
        sqlite -> {true, sqlite3};
  mssql -> {true, odbc};
        odbc -> {true, odbc};
        undefined -> false
    end.

%% GENERIC CACHE FUNS

add_sql_cache_pid(Name, Pid) ->
  mnesia:dirty_write({ejabberd_cache:get_node_table_name("sql_cache"), Name, Pid})
.

refresh_cache_pid(Name) ->
  supervisor:terminate_child(ejabberd_rdbms, Name),
  supervisor:delete_child(ejabberd_rdbms, Name),
  supervisor:start_child(?MODULE, hd(lists:filter(fun({CacheName, _, _, _, _, _}) -> CacheName == Name end, sql_cache_child_specs())))
.

get_cache_pid(Name) ->
  Rs = mnesia:dirty_read(ejabberd_cache:get_node_table_name("sql_cache"), Name),
  try case [Pid || { _TableName, _CacheName, Pid } <- Rs, gen_server:call(Pid, {is_alive})] of
        [] ->
          refresh_cache_pid(Name),
          none;
        Pids -> hd(Pids)
      end of
    Value -> Value
  catch _:_ ->
    refresh_cache_pid(Name),
    none
  end
.

remove_cache_pid(Name, Pid) ->
  mnesia:dirty_delete_object({ejabberd_cache:get_node_table_name("sql_cache"), Name, Pid})
.

get_cache_item(Name, Key) ->
  case get_cache_pid(Name) of
    none -> none;
    Pid -> gen_server:call(Pid, {get_item, Key})
  end
.

get_cache(Name) ->
  case get_cache_pid(Name) of
    none -> none;
    Pid -> gen_server:call(Pid, {get_all})
  end
.

put_cache(Name, Dict) ->
  case get_cache_pid(Name) of
    none -> none;
    Pid -> gen_server:cast(Pid, {put_all, Dict})
  end
.

put_cache_item(Name, Key, Value) ->
  case get_cache_pid(Name) of
    none -> none;
    Pid -> gen_server:cast(Pid, {put_item, Key, Value})
  end
.

delete_cache_item(Name, Key) ->
  case get_cache_pid(Name) of
    none -> none;
    Pid -> gen_server:cast(Pid, {delete_item, Key})
  end
.
%% END GENERIC CACHE FUNS





%% GET_SUBSCRIBED_ROOMS CACHE FUNS
get_subscribed_rooms_cache_key(JidS, Host) ->
  unicode:characters_to_list("get_subscribed_rooms&") ++
  unicode:characters_to_list(JidS) ++
  unicode:characters_to_list("&") ++
  unicode:characters_to_list(Host)
.

get_subscribed_rooms_cache_item(Key) ->
  get_cache_item(get_subscribed_rooms_cache, Key)
.

put_subscribed_rooms_cache_item(Key, Value) ->
  put_cache_item(get_subscribed_rooms_cache, Key, Value)
.

invalidate_subscribed_rooms(Key) ->
  delete_cache_item(get_subscribed_rooms_cache, Key)
.

invalidate_subscribed_rooms(JidS, Host) ->
  invalidate_subscribed_rooms(get_subscribed_rooms_cache_key(JidS, Host))
.

%% filter InputRoom and InputHost out of any cache entries with them
remove_subscribed_rooms_by_room(InputRoom, InputHost) ->
  case get_cache(get_subscribed_rooms_cache) of
    none -> none;
    Dict ->
      NewDict = dict:map(
        fun(_, Value) ->
          %% list of rooms and nodes from get_subscribed_rooms(...) call
          case is_list(Value) of
            true ->
              %% filter entries to remove ones containing both InputRoom, InputHost
              lists:filter(
                fun(Entry) ->
                  case Entry of
                    %% jid:make from jid.erl has signature: { jid, User, Server, Resource, LUser, LServer, LResource }
                    { { jid, Room, Host, _, _, _, _ }, _ } ->
                      %% only keep entries where Room != InputRoom or Host != InputHost
                      %% which is equiv to only removing entries where Room == InputRoom and Host == InputHost
                      Room /= InputRoom orelse Host /= InputHost;
                    OtherValue ->
                      %% unknown entry signature so keep it included to be safe
                      ?DEBUG("Unknown get_subscribed_rooms_cache entry signature: ~p", [OtherValue]),
                      true
                  end
                end,
                Value
              );
            %% not a list so keep it in the dict to be safe
            _ -> Value
          end
        end,
        Dict
      ),

      %% put back the new cache with InputRoom entries removed
      put_cache(get_subscribed_rooms_cache, NewDict)
  end
.
%% END GET_SUBSCRIBED_ROOMS FUNS

%% GET_MAX_ROOM_TIMESTAMP CACHE FUNS
get_max_room_timestamp_cache_key(Room, Host) ->
  unicode:characters_to_list("get_max_room_timestamp_&") ++
  unicode:characters_to_list(Room) ++
  unicode:characters_to_list("&") ++
  unicode:characters_to_list(Host)
.

get_max_room_timestamp_cache_item(Key) ->
  get_cache_item(get_max_room_timestamp_cache, Key)
.

get_max_room_timestamp_cache_item(Room, Host) ->
  get_max_room_timestamp_cache_item(get_max_room_timestamp_cache_key(Room, Host))
.

put_max_room_timestamp_cache_item(Room, Host, Timestamp) ->
  Value = case Timestamp of
    Timestamp when is_list(Timestamp) -> list_to_integer(Timestamp);
    Timestamp when is_binary(Timestamp) -> list_to_integer(binary_to_list(Timestamp));
    Timestamp -> Timestamp
  end,
  Key = get_max_room_timestamp_cache_key(Room, Host),
  case get_max_room_timestamp_cache_item(Key) of
    none ->
      ?DEBUG("Max Room Key Not Found: Put New Key: ~p, Timestamp: ~p", [Key, Value]),
      put_cache_item(get_max_room_timestamp_cache, Key, Value);
    CurrentValue ->
      if Value > CurrentValue ->
        ?DEBUG("New Max Room Value: Key: ~p, New Timestamp: ~p, Old Timestamp: ~p", [Key, Value, CurrentValue]),
        put_cache_item(get_max_room_timestamp_cache, Key, Value);
      true ->
        ?DEBUG("Old Max Room Value Ignored: Key: ~p, New Timestamp: ~p, Old Timestamp: ~p", [Key, Value, CurrentValue]),
        none
      end
  end
.
%% END GET_MAX_ROOM_TIMESTAMP CACHE FUNS

-spec opt_type(atom()) -> fun((any()) -> any()) | [atom()].
opt_type(sql_type) ->
    fun (mysql) -> mysql;
  (pgsql) -> pgsql;
  (sqlite) -> sqlite;
  (mssql) -> mssql;
  (odbc) -> odbc
    end;
opt_type(_) -> [sql_type].
