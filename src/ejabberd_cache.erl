%%%-------------------------------------------------------------------
%%% File    : ejabberd_cache.erl
%%% Author  : Devin Sills <dsills@skillz.com>
%%% Created : 18 Jan 2022 by Devin Sills <dsills@skillz.com>
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_cache).

-behaviour(gen_server).

%% External exports
-export([start/2, start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

start(CacheName, MaxCacheSize) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [CacheName, MaxCacheSize], []).

start_link(CacheName, MaxCacheSize) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [CacheName, MaxCacheSize], []).

init([CacheName, MaxCacheSize]) ->
  ejabberd_sql_sup:add_sql_cache_pid(CacheName, self()),
  {ok, {queue:new(), dict:new(), 0, MaxCacheSize}}.

handle_call({get_item, Key}, _From, { KeyQueue, CacheDict, Size, MaxCacheSize }) ->
  {reply, get_item(Key, CacheDict), { KeyQueue, CacheDict, Size, MaxCacheSize }}
;

handle_call({get_all}, _From, { KeyQueue, CacheDict, Size, MaxCacheSize }) ->
  {reply, CacheDict, { KeyQueue, CacheDict, Size, MaxCacheSize }}
.

handle_cast({put_item, Key, Value}, { KeyQueue, CacheDict, Size, MaxCacheSize }) ->
  {noreply, put_item(Key, Value, KeyQueue, CacheDict, Size, MaxCacheSize)}
;

handle_cast({put_all, Dict}, { _, _, _, MaxCacheSize }) ->
  {noreply, { queue:from_list(dict:fetch_keys(Dict)), Dict, dict:size(Dict), MaxCacheSize }}
;

handle_cast({delete_item, Key}, { KeyQueue, CacheDict, Size, MaxCacheSize }) ->
  {noreply, delete_item(Key, KeyQueue, CacheDict, Size, MaxCacheSize)}
;

handle_cast({flush}, { _, _, _, MaxCacheSize }) ->
  {noreply, {queue:new(), dict:new(), 0, MaxCacheSize}}
.

%% avg O(1)
put_item(Key, Value, KeyQueue, CacheDict, Size, MaxCacheSize) ->
  case dict:is_key(Key, CacheDict) of
    true ->
      { KeyQueue, dict:store(Key, Value, CacheDict), Size, MaxCacheSize };
    _ ->
      if Size < MaxCacheSize ->
        { queue:in(Key, KeyQueue), dict:store(Key, Value, CacheDict), Size + 1, MaxCacheSize };
      true ->
        KeyToRemove = queue:get(KeyQueue),
        { queue:in(Key, queue:drop(KeyQueue)), dict:store(Key, Value, dict:erase(KeyToRemove, CacheDict)), Size, MaxCacheSize }
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
delete_item(Key, KeyQueue, CacheDict, Size, MaxCacheSize) ->
  %% to_list -> delete -> from_list is much faster than queue:filter
  { queue:from_list(lists:delete(Key, queue:to_list(KeyQueue))), dict:erase(Key, CacheDict), Size - 1, MaxCacheSize }
.
