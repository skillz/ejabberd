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

-export([put_max_room_timestamp_cache_item/3, get_max_room_timestamp_cache_item/2]).

-include("logger.hrl").

-define(MNESIA_SQL_CACHES, [
  {get_max_room_timestamp_cache, 50000}
]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  file:delete(ejabberd_sql:freetds_config()),
  file:delete(ejabberd_sql:odbc_config()),
  file:delete(ejabberd_sql:odbcinst_config()),
  ejabberd_hooks:add(host_up, ?MODULE, start_host, 20),
  ejabberd_hooks:add(host_down, ?MODULE, stop_host, 90),
  ejabberd_hooks:add(config_reloaded, ?MODULE, config_reloaded, 20),
  {ok, {{one_for_one, 10, 1}, get_specs() ++ sql_cache_child_specs()}}.

sql_cache_child_specs() ->
  lists:map(
    fun({ CacheName, MaxCacheSize }) ->
      {CacheName, {ejabberd_cache_mnesia, start_link, [CacheName, MaxCacheSize]}, transient, 2000, worker, [?MODULE]}
    end,
    ?MNESIA_SQL_CACHES
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
get_mnesia_cache_item(Name, Key) ->
  try
    gen_server:call(ejabberd_cache_mnesia:get_proc_name(Name), {get_item, Key})
  catch _:_ ->
    none
  end
.

put_mnesia_cache_item(Name, Key, Value) ->
  try
    gen_server:cast(ejabberd_cache_mnesia:get_proc_name(Name), {put_item, Key, Value})
  catch _:_ ->
    none
  end
.
%% END GENERIC CACHE FUNS





%% GET_MAX_ROOM_TIMESTAMP CACHE FUNS
get_max_room_timestamp_cache_key(Room, Host) ->
  binary:list_to_bin(
    unicode:characters_to_list("get_max_room_timestamp&") ++
    unicode:characters_to_list(Room) ++
    unicode:characters_to_list("&") ++
    unicode:characters_to_list(Host)
  )
.

get_max_room_timestamp_cache_item(Key) ->
  get_mnesia_cache_item(get_max_room_timestamp_cache, Key)
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
      put_mnesia_cache_item(get_max_room_timestamp_cache, Key, Value);
    CurrentValue ->
      if Value > CurrentValue ->
        ?DEBUG("New Max Room Value: Key: ~p, New Timestamp: ~p, Old Timestamp: ~p", [Key, Value, CurrentValue]),
        put_mnesia_cache_item(get_max_room_timestamp_cache, Key, Value);
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
