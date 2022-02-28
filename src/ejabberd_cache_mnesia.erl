%%%-------------------------------------------------------------------
%%% File    : ejabberd_cache_mnesia.erl
%%% Author  : Devin Sills <dsills@skillz.com>
%%% Created : 18 Jan 2022 by Devin Sills <dsills@skillz.com>
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_cache_mnesia).

-behaviour(gen_server).

%% External exports
-export([start_link/2, get_proc_name/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/3, handle_info/3]).

-include("logger.hrl").

start_link(CacheName, MaxCacheSize) ->
  %% sql cache table
  mnesia:create_table(get_cache_table_name(CacheName), [{attributes, [ key, value ]}, {type, set}, {ram_copies, [node()]}, {local_content, false}]),

  case gen_server:start_link({local, get_proc_name(CacheName)}, ?MODULE, [CacheName, MaxCacheSize], []) of
    {ok, Pid} ->
      {ok, Pid};
    {error, {already_started, Pid}} ->
      {ok, Pid};
    ignore -> ignore;
    {error, Error} -> {error, Error};
    Other -> Other
  end
.

init([CacheName, MaxCacheSize]) ->
  FullCacheName = get_cache_table_name(CacheName),
  {ok, {FullCacheName, MaxCacheSize, get_size(FullCacheName, MaxCacheSize)}}.

handle_call({get_item, Key}, _From, { CacheName, MaxCacheSize, Size }) ->
  {reply, get_item(CacheName, Key), { CacheName, MaxCacheSize, Size }}
.

handle_cast({put_item, Key, Value}, { CacheName, MaxCacheSize, Size }) ->
  NewSize = put_item(CacheName, Key, Value, MaxCacheSize, Size),
  {noreply, { CacheName, MaxCacheSize, NewSize }}
;

handle_cast({delete_item, Key}, { CacheName, MaxCacheSize, Size }) ->
  NewSize = delete_item(CacheName, Key, Size),
  {noreply, { CacheName, MaxCacheSize, NewSize }}
;

handle_cast({delete_items_by_fun, ShouldRemoveFun}, { CacheName, MaxCacheSize, Size }) ->
  try
    KeysToRemove = lists:filtermap(
      fun({ _TableName, Key, Item }) ->
        try
          case ShouldRemoveFun(Key, Item) of
            true -> {true, Key};
            _ -> false
          end
        catch _:_ ->
          false
        end
      end,
      ets:tab2list(CacheName)
    ),
    lists:foreach(
      fun(Key) ->
        mnesia:dirty_delete(CacheName, Key)
      end,
      KeysToRemove
    ),
    {noreply, { CacheName, MaxCacheSize, lists:max([0, Size - length(KeysToRemove)]) }}
  catch _:_ ->
    {noreply, { CacheName, MaxCacheSize, Size }}
  end
.

% got stop message
handle_info(stop, _StateName, StateData) ->
  {stop, normal, StateData}
;

%% either linked process or peer process died
handle_info({'EXIT', _, _}, _, StateData) ->
  {stop, normal, StateData}
;

handle_info({'DOWN', _, _, _, _}, _StateName, StateData) ->
  {stop, normal, StateData}
.

terminate(_Reason, _StateName, _StateData) ->
  ok
.

get_proc_name(CacheName) ->
  list_to_atom(atom_to_list(?MODULE) ++ "_" ++ atom_to_list(CacheName))
.

get_cache_table_name(CacheName) ->
  list_to_atom("sql_cache_mnesia_" ++ atom_to_list(CacheName))
.

% O(N); expensive bc it syncs with other nodes ONLY at start up to get most accurate size possible before fully staring the cache
get_size(CacheName, MaxCacheSize) ->
  Keys = try
    {atomic, AllKeys} = mnesia:sync_transaction(fun() -> mnesia:all_keys(CacheName) end),
    AllKeys
  catch _:_ ->
    mnesia:dirty_all_keys(CacheName)
  end,
  lists:min([length(Keys), MaxCacheSize])
.

%% avg O(1)
put_item(CacheName, Key, Value, MaxCacheSize, Size) ->
  case get_item(CacheName, Key) of
    none ->
      NewSize = case Size + 1 > MaxCacheSize of
                  true ->
                    prune(CacheName, Size);
                  _ -> Size
                end,
      mnesia:dirty_write({CacheName, Key, {cache_item, Value}}),
      NewSize + 1;
    _ ->
      mnesia:dirty_write({CacheName, Key, {cache_item, Value}}),
      Size
  end
.

%% avg O(1)
get_item(CacheName, Key) ->
  try
    case mnesia:dirty_read(CacheName, Key) of
      [{_TableName, _Key, {cache_item, CacheItem}}] -> CacheItem;
      _ -> none
    end
  catch _:_ ->
    none
  end
.

%% avg O(1)
delete_item(CacheName, Key, Size) ->
  try
    case get_item(CacheName, Key) of
      none -> Size;
      _ ->
        mnesia:dirty_delete(CacheName, Key),
        case Size > 0 of
          true -> Size - 1;
          _ -> 0
        end
    end
  catch _:_ ->
    Size
  end
.

%% avg O(1)
prune(CacheName, Size) ->
  try
    Key = mnesia:dirty_last(CacheName),
    mnesia:dirty_delete(CacheName, Key),
    case Size > 0 of
      true -> Size - 1;
      _ -> 0
    end
  catch _:_ ->
    Size
  end
.
