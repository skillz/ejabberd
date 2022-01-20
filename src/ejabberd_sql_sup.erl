%%%----------------------------------------------------------------------
%%% File    : ejabberd_sql_sup.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : SQL connections supervisor
%%% Created : 22 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_sql_sup).

-behaviour(ejabberd_config).

-author('alexey@process-one.net').

-export([start_link/1, init/1, add_pid/2, remove_pid/2,
  get_pids/1, get_random_pid/1, get_pids/2, get_random_pid/2, transform_options/1,
  reload/1, opt_type/1,

  add_sql_cache_pid/2,
  get_subscribed_rooms_cache_key/2, get_subscribed_rooms_cache_item/1,
  put_subscribed_rooms_cache_item/2, invalidate_subscribed_rooms/2, flush_subscribed_rooms_cache/0,
  remove_subscribed_rooms_by_room/2
]).

-include("logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(PGSQL_PORT, 5432).
-define(MYSQL_PORT, 3306).
-define(DEFAULT_POOL_SIZE, 10).
-define(DEFAULT_SQL_START_INTERVAL, 30).
-define(CONNECT_TIMEOUT, 500).
-define(SQL_CACHES, [
  {"get_subscribed_rooms_cache", 10000}
]).

-record(sql_pool, {host :: binary(),
		   pid  :: pid()}).

-record(sql_cache, {name :: binary(), pid  :: pid()}).

start_link(Host) ->
    ejabberd_mnesia:create(?MODULE, sql_pool,
      [{ram_copies, [node()]}, {type, bag},
       {local_content, true},
       {attributes, record_info(fields, sql_pool)}]),
    F = fun() -> mnesia:delete({sql_pool, Host}) end,
    mnesia:ets(F),

    %% sql cache pids
    ejabberd_mnesia:create(?MODULE, sql_cache,
      [{ram_copies, [node()]}, {type, bag},
        {local_content, true},
        {attributes, record_info(fields, sql_cache)}]),

    %% delete any entries in any sql caches
    lists:foreach(fun(C) ->
      mnesia:ets(fun() -> mnesia:delete({sql_cache, C}) end)
    end, ?SQL_CACHES),

    supervisor:start_link({local,
			   gen_mod:get_module_proc(Host, ?MODULE)},
			  ?MODULE, [Host]).

init([Host]) ->
    Type = ejabberd_config:get_option({sql_type, Host}, odbc),
    PoolSize = get_pool_size(Type, Host),
    case Type of
        sqlite ->
            check_sqlite_db(Host);
	mssql ->
	    ejabberd_sql:init_mssql(Host);
        _ ->
            ok
    end,
    AllHosts = [Host] ++ ejabberd_config:get_option({sql_secondary_servers, Host}, []),
    {ok,
      {
        {one_for_one, PoolSize * 10 * length(AllHosts), 1},
        %% list of ejabberd_sql specs; the total count would be number of hosts * pool size
        sql_cache_child_specs() ++ lists:foldl(
          fun(H, Acc) ->
            %% first iter: length(Acc) is 0, so HostIndex bc (0 / PoolSize) + 1 = 1
            %% next iter : Acc length will be exactly PoolSize, so we have (PoolSize  / PoolSize) + 1 = 2
            %% next iter : Acc length is now PoolSize * 2,      so we have (2PoolSize / PoolSize) + 1 = 3
            %% and so on...
            HostIndex = (length(Acc) div PoolSize) + 1,

            %% if secondary server
            if HostIndex > 1 ->
              %% save configs for each secondary using the configs for the primary
              %% except for of course the sql_server address, which matches the secondary host address provided
              SqlType = ejabberd_config:get_option({sql_type, Host}, odbc),
              ejabberd_config:add_option({sql_type, H}, SqlType),
              ejabberd_config:add_option({sql_server, H}, H),
              ejabberd_config:add_option({sql_port, H}, ejabberd_config:get_option({sql_port, Host})),
              ejabberd_config:add_option({sql_database, H}, ejabberd_config:get_option({sql_database, Host}, <<"ejabberd">>)),
              ejabberd_config:add_option({sql_username, H}, ejabberd_config:get_option({sql_username, Host}, <<"ejabberd">>)),
              ejabberd_config:add_option({sql_password, H}, ejabberd_config:get_option({sql_password, Host}, <<"">>)),
              ejabberd_config:add_option({sql_start_interval, H}, ejabberd_config:get_option({sql_start_interval, Host}, ?DEFAULT_SQL_START_INTERVAL)),
              ejabberd_config:add_option({sql_keepalive_interval, H}, ejabberd_config:get_option({sql_keepalive_interval, Host})),
              ejabberd_config:add_option({sql_connect_timeout, H}, ejabberd_config:get_option({sql_connect_timeout, Host}, 5)),
              ejabberd_config:add_option({sql_query_timeout, H}, ejabberd_config:get_option({sql_query_timeout, Host}, 60)),
              ejabberd_config:add_option({sql_queue_type, H}, ejabberd_config:get_option({sql_queue_type, Host})),
              ejabberd_config:add_option({sql_pool_size, H}, get_pool_size(SqlType, Host)),
              ejabberd_config:add_option({sql_ssl, H}, ejabberd_config:get_option({sql_ssl, Host}, false)),
              ejabberd_config:add_option({sql_ssl_verify, H}, ejabberd_config:get_option({sql_ssl_verify, Host}, false)),
              ejabberd_config:add_option({sql_ssl_certfile, H}, ejabberd_config:get_option({sql_ssl_certfile, Host})),
              ejabberd_config:add_option({sql_ssl_cafile, H}, ejabberd_config:get_option({sql_ssl_cafile, Host}));
            true -> ""
            end,

            %% accumulate the ejabberd_sql specs for this host (PoolSize number of specs per host).
            %% since we saved configs for secondary H above and secondary H points to a secondary server,
            %% the ejabberd_sql instance will always have the database context of a
            %% secondary: its DB reference, any queries it runs, etc, will all be against that secondary
            Acc ++ [
              child_spec(
                %% name the child using the host index and the pool index
                integer_to_list(HostIndex) ++ "-" ++ integer_to_list(I),
                H
              ) || I <- lists:seq(1, PoolSize)
            ]
          end,
          [],
          AllHosts
        )
      }
    }.

reload(Host) ->
    Type = ejabberd_config:get_option({sql_type, Host}, odbc),
    NewPoolSize = get_pool_size(Type, Host),
    OldPoolSize = ets:select_count(
		    sql_pool,
		    ets:fun2ms(
		      fun(#sql_pool{host = H}) when H == Host ->
			      true
		      end)),
    reload(Host, NewPoolSize, OldPoolSize).

reload(Host, NewPoolSize, OldPoolSize) ->
    Sup = gen_mod:get_module_proc(Host, ?MODULE),
    AllHosts = [Host] ++ ejabberd_config:get_option({sql_secondary_servers, Host}, []),
    if NewPoolSize == OldPoolSize ->
	    ok;
    %% add more sql connections
    NewPoolSize > OldPoolSize ->
      %% for each host, get the host and make the extra ejabberd_sql specs needed
      lists:foreach(
        fun(HostIndex) ->
          H = lists:nth(HostIndex, AllHosts),
          lists:foreach(
            fun(I) ->
              supervisor:start_child(Sup, child_spec(integer_to_list(HostIndex) ++ "-" ++ integer_to_list(I), H))
            end,
            lists:seq(OldPoolSize + 1, NewPoolSize)
          )
        end,
        lists:seq(1, length(AllHosts))
      );
    %% remove sql connections
    OldPoolSize > NewPoolSize ->
      %% for each host, get the host and remove the necessary ejabberd_sql specs
      lists:foreach(
        fun(HostIndex) ->
          lists:foreach(
            fun(I) ->
              supervisor:terminate_child(Sup, integer_to_list(HostIndex) ++ "-" ++ integer_to_list(I)),
              supervisor:delete_child(Sup, integer_to_list(HostIndex) ++ "-" ++ integer_to_list(I))
            end, lists:seq(NewPoolSize + 1, OldPoolSize)
          )
        end,
        lists:seq(1, length(AllHosts))
      )
    end.





%% GENERIC CACHE FUNS
sql_cache_child_specs() ->
  lists:map(fun({ CacheName, MaxCacheSize }) ->
    {CacheName, {ejabberd_cache, start_link, [CacheName, MaxCacheSize]}, transient, 2000, worker, [?MODULE]}
  end, ?SQL_CACHES)
.

add_sql_cache_pid(Name, Pid) ->
  F = fun () ->
    mnesia:write(#sql_cache{name = Name, pid = Pid})
  end,
  mnesia:ets(F)
.

get_cache_pid(Name) ->
  Rs = mnesia:dirty_read(sql_cache, Name),
  case [R#sql_cache.pid || R <- Rs, is_process_alive(R#sql_cache.pid)] of
    [] -> none;
    Pids -> hd(Pids)
  end
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

flush_cache(Name) ->
  case get_cache_pid(Name) of
    none -> none;
    Pid -> gen_server:cast(Pid, {flush})
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
  get_cache_item("get_subscribed_rooms_cache", Key)
.

put_subscribed_rooms_cache_item(Key, Value) ->
  put_cache_item("get_subscribed_rooms_cache", Key, Value)
.

delete_subscribed_rooms_cache_item(Key) ->
  delete_cache_item("get_subscribed_rooms_cache", Key)
.

flush_subscribed_rooms_cache() ->
  flush_cache("get_subscribed_rooms_cache")
.

invalidate_subscribed_rooms(JidS, Host) ->
  delete_subscribed_rooms_cache_item(get_subscribed_rooms_cache_key(JidS, Host))
.

%% filter InputRoom and InputHost out of any cache entries with them
remove_subscribed_rooms_by_room(InputRoom, InputHost) ->
  case get_cache("get_subscribed_rooms_cache") of
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
                    _ ->
                      %% unknown entry signature so keep it included to be safe
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
      put_cache("get_subscribed_rooms_cache", NewDict)
  end
.
%% END GET_SUBSCRIBED_ROOMS FUNS





%% backwards compatible get_pids
get_pids(Host) ->
  get_pids(Host, primary).

%% get_pids that can accept primary or secondary NodeType
get_pids(Host, NodeType) ->
  %% if secondary, pick a random secondary host and get its list of pids (ejabberd_sql instances)
  %% else default to primary
  SearchHost = if NodeType == secondary orelse NodeType == any ->
    %% make list of secondary servers and optionally include primary if NodeType is 'any' (meaning primary or secondary)
    HostList = ejabberd_config:get_option({sql_secondary_servers, Host}, []) ++ (if NodeType == any -> [Host]; true -> [] end),
    %% only use host list if there actually are some
    if length(HostList) > 0 ->
      I = rand:uniform(length(HostList)),
      lists:nth(I, HostList);
    true ->
      Host
    end;
  true ->
    Host
  end,
  Rs = mnesia:dirty_read(sql_pool, SearchHost),
  [R#sql_pool.pid || R <- Rs, is_process_alive(R#sql_pool.pid)].

%% backwards compatible get_random_pid
get_random_pid(Host) ->
  get_random_pid(Host, primary).

%% get_random_pid that can accept primary or secondary NodeType
get_random_pid(Host, NodeType) ->
    case get_pids(Host, NodeType) of
      [] -> none;
      Pids ->
	    I = p1_rand:round_robin(length(Pids)) + 1,
	    lists:nth(I, Pids)
    end.

add_pid(Host, Pid) ->
    F = fun () ->
		  mnesia:write(#sql_pool{host = Host, pid = Pid})
	  end,
    mnesia:ets(F).

remove_pid(Host, Pid) ->
    F = fun () ->
		mnesia:delete_object(#sql_pool{host = Host, pid = Pid})
	end,
    mnesia:ets(F).

-spec get_pool_size(atom(), binary()) -> pos_integer().
get_pool_size(SQLType, Host) ->
    PoolSize = ejabberd_config:get_option(
                 {sql_pool_size, Host},
		 case SQLType of
		     sqlite -> 1;
		     _ -> ?DEFAULT_POOL_SIZE
		 end),
    if PoolSize > 1 andalso SQLType == sqlite ->
	    ?WARNING_MSG("it's not recommended to set sql_pool_size > 1 for "
			 "sqlite, because it may cause race conditions", []);
       true ->
	    ok
    end,
    PoolSize.

child_spec(I, Host) ->
    StartInterval = ejabberd_config:get_option(
                      {sql_start_interval, Host},
                      ?DEFAULT_SQL_START_INTERVAL),
    {I, {ejabberd_sql, start_link, [Host, timer:seconds(StartInterval)]},
     transient, 2000, worker, [?MODULE]}.

transform_options(Opts) ->
    lists:foldl(fun transform_options/2, [], Opts).

transform_options({odbc_server, {Type, Server, Port, DB, User, Pass}}, Opts) ->
    [{sql_type, Type},
     {sql_server, Server},
     {sql_port, Port},
     {sql_database, DB},
     {sql_username, User},
     {sql_password, Pass}|Opts];
transform_options({odbc_server, {mysql, Server, DB, User, Pass}}, Opts) ->
    transform_options({odbc_server, {mysql, Server, ?MYSQL_PORT, DB, User, Pass}}, Opts);
transform_options({odbc_server, {pgsql, Server, DB, User, Pass}}, Opts) ->
    transform_options({odbc_server, {pgsql, Server, ?PGSQL_PORT, DB, User, Pass}}, Opts);
transform_options({odbc_server, {sqlite, DB}}, Opts) ->
    transform_options({odbc_server, {sqlite, DB}}, Opts);
transform_options(Opt, Opts) ->
    [Opt|Opts].

check_sqlite_db(Host) ->
    DB = ejabberd_sql:sqlite_db(Host),
    File = ejabberd_sql:sqlite_file(Host),
    Ret = case filelib:ensure_dir(File) of
	      ok ->
		  case sqlite3:open(DB, [{file, File}]) of
		      {ok, _Ref} -> ok;
		      {error, {already_started, _Ref}} -> ok;
		      {error, R} -> {error, R}
		  end;
	      Err ->
		  Err
	  end,
    case Ret of
        ok ->
	    sqlite3:sql_exec(DB, "pragma foreign_keys = on"),
            case sqlite3:list_tables(DB) of
                [] ->
                    create_sqlite_tables(DB),
                    sqlite3:close(DB),
                    ok;
                [_H | _] ->
                    ok
            end;
        {error, Reason} ->
            ?WARNING_MSG("Failed open sqlite database, reason ~p", [Reason])
    end.

create_sqlite_tables(DB) ->
    SqlDir = misc:sql_dir(),
    File = filename:join(SqlDir, "lite.sql"),
    case file:open(File, [read, binary]) of
        {ok, Fd} ->
            Qs = read_lines(Fd, File, []),
            ok = sqlite3:sql_exec(DB, "begin"),
            [ok = sqlite3:sql_exec(DB, Q) || Q <- Qs],
            ok = sqlite3:sql_exec(DB, "commit");
        {error, Reason} ->
            ?WARNING_MSG("Failed to read SQLite schema file: ~s",
			 [file:format_error(Reason)])
    end.

read_lines(Fd, File, Acc) ->
    case file:read_line(Fd) of
        {ok, Line} ->
            NewAcc = case str:strip(str:strip(Line, both, $\r), both, $\n) of
                         <<"--", _/binary>> ->
                             Acc;
                         <<>> ->
                             Acc;
                         _ ->
                             [Line|Acc]
                     end,
            read_lines(Fd, File, NewAcc);
        eof ->
            QueryList = str:tokens(list_to_binary(lists:reverse(Acc)), <<";">>),
            lists:flatmap(
              fun(Query) ->
                      case str:strip(str:strip(Query, both, $\r), both, $\n) of
                          <<>> ->
                              [];
                          Q ->
                              [<<Q/binary, $;>>]
                      end
              end, QueryList);
        {error, _} = Err ->
            ?ERROR_MSG("Failed read from lite.sql, reason: ~p", [Err]),
            []
    end.

-spec opt_type(atom()) -> fun((any()) -> any()) | [atom()].
opt_type(sql_pool_size) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(sql_start_interval) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(_) ->
    [sql_pool_size, sql_start_interval].
