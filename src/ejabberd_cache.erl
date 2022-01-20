%%%-------------------------------------------------------------------
%%% File    : ejabberd_cache.erl
%%% Author  : Devin Sills <dsills@skillz.com>
%%% Created : 18 Jan 2022 by Devin Sills <dsills@skillz.com>
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_cache).

-behaviour(gen_server).

%% External exports
-export([start/1, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-define(MAX_SIZE, 10000).

start(Name) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [Name], []).

start_link(Name) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [Name], []).

init([Name]) ->
  ejabberd_sql_sup:add_sql_cache_pid(Name, self()),
  {ok, {queue:new(), dict:new(), 0}}.

handle_call({put_item, Key, Value}, _From, { KeyQueue, CacheDict, Size }) ->
  {reply, ok, put_item(Key, Value, KeyQueue, CacheDict, Size)}
;

handle_call({get_item, Key}, _From, { KeyQueue, CacheDict, Size }) ->
  {reply, get_item(Key, CacheDict), { KeyQueue, CacheDict, Size }}
.

handle_cast({delete_item, Key}, { KeyQueue, CacheDict, Size }) ->
  {noreply, delete_item(Key, KeyQueue, CacheDict, Size)}
;

handle_cast({flush}, { KeyQueue, CacheDict, Size }) ->
  {noreply, {queue:new(), dict:new(), 0}}
.

%% avg O(1)
put_item(Key, Value, KeyQueue, CacheDict, Size) ->
  case dict:is_key(Key, CacheDict) of
    true ->
      { KeyQueue, dict:store(Key, Value, CacheDict), Size };
    _ ->
      if Size < ?MAX_SIZE ->
        { queue:in(Key, KeyQueue), dict:store(Key, Value, CacheDict), Size + 1 };
      true ->
        KeyToRemove = queue:get(KeyQueue),
        { queue:in(Key, queue:drop(KeyQueue)), dict:store(Key, Value, dict:erase(KeyToRemove, CacheDict)), Size }
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
delete_item(Key, KeyQueue, CacheDict, Size) ->
  %% to_list -> delete -> from_list is much faster than queue:filter
  { queue:from_list(lists:delete(Key, queue:to_list(KeyQueue))), dict:erase(Key, CacheDict), Size - 1 }
.
