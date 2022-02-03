%%%-------------------------------------------------------------------
%%% File    : ejabberd_cache.erl
%%% Author  : Devin Sills <dsills@skillz.com>
%%% Created : 18 Jan 2022 by Devin Sills <dsills@skillz.com>
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_cache).

-behaviour(gen_server).

%% External exports
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/3, handle_info/3, get_node_table_name/1]).

-include("logger.hrl").

get_node_table_name(TableName) ->
  list_to_atom(TableName ++ "_" ++ atom_to_list(node()))
.

start_link(CacheName, MaxCacheSize) ->
  %% delete any entries in the sql caches
  mnesia:dirty_delete(get_node_table_name("sql_cache"), CacheName),

  case gen_server:start_link({global, list_to_atom(atom_to_list(?MODULE) ++ atom_to_list(CacheName))}, ?MODULE, [CacheName, MaxCacheSize], []) of
    {ok, Pid} ->
      ejabberd_rdbms:add_sql_cache_pid(CacheName, Pid),
      {ok, Pid};
    {ok, {Pid, Mon}} ->
      ejabberd_rdbms:add_sql_cache_pid(CacheName, Pid),
      {ok, {Pid, Mon}};
    {error, {already_started, Pid}} ->
      ejabberd_rdbms:add_sql_cache_pid(CacheName, Pid),
      {ok, Pid};
    ignore -> ignore;
    {error, Error} -> {error, Error};
    Other -> Other
  end
.

init([CacheName, MaxCacheSize]) ->
  {ok, {CacheName, queue:new(), dict:new(), 0, MaxCacheSize}}.

handle_call({is_alive}, _From, { CacheName, KeyQueue, CacheDict, Size, MaxCacheSize }) ->
  {reply, true, { CacheName, KeyQueue, CacheDict, Size, MaxCacheSize }}
;

handle_call({get_item, Key}, _From, { CacheName, KeyQueue, CacheDict, Size, MaxCacheSize }) ->
  {reply, get_item(Key, CacheDict), { CacheName, KeyQueue, CacheDict, Size, MaxCacheSize }}
;

handle_call({get_all}, _From, { CacheName, KeyQueue, CacheDict, Size, MaxCacheSize }) ->
  {reply, CacheDict, { CacheName, KeyQueue, CacheDict, Size, MaxCacheSize }}
.

handle_cast({put_item, Key, Value}, { CacheName, KeyQueue, CacheDict, Size, MaxCacheSize }) ->
  {noreply, put_item(CacheName, Key, Value, KeyQueue, CacheDict, Size, MaxCacheSize)}
;

handle_cast({put_all, Dict}, { CacheName, _, _, _, MaxCacheSize }) ->
  {noreply, { CacheName, queue:from_list(dict:fetch_keys(Dict)), Dict, dict:size(Dict), MaxCacheSize }}
;

handle_cast({delete_item, Key}, { CacheName, KeyQueue, CacheDict, Size, MaxCacheSize }) ->
  {noreply, delete_item(CacheName, Key, KeyQueue, CacheDict, Size, MaxCacheSize)}
;

handle_cast({flush}, { CacheName, _, _, _, MaxCacheSize }) ->
  {noreply, { CacheName, queue:new(), dict:new(), 0, MaxCacheSize} }
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

terminate(_Reason, _StateName, { CacheName, _, _, _, _ }) ->
  ejabberd_rdbms:remove_cache_pid(CacheName, self())
.

%% avg O(1)
put_item(CacheName, Key, Value, KeyQueue, CacheDict, Size, MaxCacheSize) ->
  case dict:is_key(Key, CacheDict) of
    true ->
      { CacheName, KeyQueue, dict:store(Key, Value, CacheDict), Size, MaxCacheSize };
    _ ->
      if Size < MaxCacheSize ->
        { CacheName, queue:in(Key, KeyQueue), dict:store(Key, Value, CacheDict), Size + 1, MaxCacheSize };
      true ->
        KeyToRemove = queue:get(KeyQueue),
        { CacheName, queue:in(Key, queue:drop(KeyQueue)), dict:store(Key, Value, dict:erase(KeyToRemove, CacheDict)), Size, MaxCacheSize }
      end
  end
.

%% avg O(1)
get_item(Key, CacheDict) ->
  case dict:is_key(Key, CacheDict) of
    true ->
      dict:fetch(Key, CacheDict);
    _ ->
      none
  end
.

%% O(N) due to queue entry removal
%% Note: this is expected to be called less frequently than put/get so this is a trade-off and it is a cast so it will not slow down callers
%% Idea: we could keep KeyQueue unchanged here and update put_item to test for KeyToRemove existence in CacheDict before
%%       removing (if not exists then rebuild queue as here) and that could get us avg O(1) delete as well but the older
%%       keys are more likely to get deleted so this idea could actually degrade performance
delete_item(CacheName, Key, KeyQueue, CacheDict, Size, MaxCacheSize) ->
  case dict:is_key(Key, CacheDict) of
    true ->
      %% to_list -> delete -> from_list is much faster than queue:filter
      { CacheName, queue:from_list(lists:delete(Key, queue:to_list(KeyQueue))), dict:erase(Key, CacheDict), Size - 1, MaxCacheSize };
    _ ->
      { CacheName, KeyQueue, CacheDict, Size, MaxCacheSize }
  end
.
