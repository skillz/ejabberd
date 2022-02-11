%%%-------------------------------------------------------------------
%%% File    : mod_block_incoming.erl
%%% Author  : Devin Sills <dsills@skillz.com>
%%% Created : 31 Jan 2022 by Devin Sills <dsills@skillz.com>
%%%
%%%----------------------------------------------------------------------

-module(mod_block_incoming).
-author('dsills@skillz.com').

-behaviour(gen_server).
-behaviour(gen_mod).

%% gen_mod callbacks.
-export([start/2, stop/1, reload/3, depends/2, mod_options/1, mod_opt_type/1]).

%% gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ejabberd_commands callbacks.
-export([get_commands_spec/0, add_blocking_user/2, remove_blocking_user/2, get_blocking_users/1, reset_blocking_users/1, clear_blocking_users/1]).

%% hook handlers
-export([filter_packet/1, filter_offline_msg/1, filter_subscription/2]).

-include("ejabberd_commands.hrl").
-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").

-define(HOOK_PRIORITY, 25).

-define(COMMAND_TIMEOUT, timer:seconds(2)).

%%%===================================================================
%%% Callbacks and hooks
%%%===================================================================
start(Host, Opts) ->
  ejabberd_commands:register_commands(get_commands_spec()),
  gen_mod:start_child(?MODULE, Host, Opts)
.

init([Host, _Opts]) ->
  ejabberd_hooks:add(user_receive_packet, Host, ?MODULE, filter_packet, ?HOOK_PRIORITY),
  ejabberd_hooks:add(roster_in_subscription, Host, ?MODULE, filter_subscription, ?HOOK_PRIORITY),
  ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, filter_offline_msg, ?HOOK_PRIORITY),
  {ok, { Host, get_config_users_dict(Host) } }
.

stop(Host) ->
  case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
    false -> ejabberd_commands:unregister_commands(get_commands_spec());
    true -> ok
  end,
  gen_mod:stop_child(?MODULE, Host)
.

terminate(Reason, { Host, _BlockingUsers }) ->
  ?DEBUG("Terminating mod_block_incoming process for [~p] with reason: [~p]", [Host, Reason]),
  ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE, filter_packet, ?HOOK_PRIORITY),
  ejabberd_hooks:delete(roster_in_subscription, Host, ?MODULE, filter_subscription, ?HOOK_PRIORITY),
  ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, filter_offline_msg, ?HOOK_PRIORITY)
.

reload(_Host, _NewOpts, _OldOpts) ->
  ok
.

handle_call({get_blocking_users}, _From, {_Host, BlockingUsers} = State) ->
  {reply, lists:sort(lists:map(fun({Key, _}) -> Key end, dict:to_list(BlockingUsers))), State}
;

handle_call({is_blocking_user, User}, _From, {_Host, BlockingUsers} = State) ->
  {reply, dict:is_key(User, BlockingUsers), State}
;

handle_call({add_blocking_user, User}, _From, {Host, BlockingUsers}) ->
  {reply, ok, { Host, dict:store(User, User, BlockingUsers) }}
;

handle_call({remove_blocking_user, User}, _From, {Host, BlockingUsers}) ->
  {reply, ok, {Host, dict:erase(User, BlockingUsers)}}
;

handle_call({reset_blocking_users}, _From, {Host, _BlockingUsers}) ->
  {reply, ok, { Host, get_config_users_dict(Host) }}
;

handle_call({clear_blocking_users}, _From, {Host, _BlockingUsers}) ->
  {reply, ok, { Host, dict:new() }}
;

handle_call(Request, From, State) ->
  ?ERROR_MSG("Unexpected handle_call from [~p] for [~p]", [From, Request]),
  {noreply, State}
.

handle_cast(Request, State) ->
  ?ERROR_MSG("Unexpected handle_cast: [~p]", [Request]),
  {noreply, State}
.

handle_info(Info, State) ->
  ?ERROR_MSG("Unexpected handle_info: [~p]", [Info]),
  {noreply, State}
.

code_change(_OldVsn, { Host, _BlockingUsers } = State, _Extra) ->
  ?DEBUG("Updating code for mod_block_incoming process on host [~p]", [Host]),
  {ok, State}
.

%% filter messages
filter_packet({#message{from = From} = Msg, State} = Acc) ->
  LFrom = jid:tolower(From),
  LBareFrom = jid:remove_resource(LFrom),
  #{pres_a := PresA} = State,
  FromUserInPresencePacket = gb_sets:is_element(LFrom, PresA) orelse gb_sets:is_element(LBareFrom, PresA) orelse sets_bare_member(LBareFrom, PresA),
  case FromUserInPresencePacket of
    false -> %% from-user not in presence struct so this is an outgoing DM to someone else who might be blocking incoming
      case check_message(Msg) of
        allow -> Acc;
        deny -> {stop, {drop, State}}
      end;
    true -> %% from-user is in the presence struct so either a self-message or a room chat with them in it
      Acc
  end
;

filter_packet(Acc) -> Acc.

filter_offline_msg({_Action, #message{} = Msg} = Acc) ->
  case check_message(Msg) of
    allow -> Acc;
    deny -> {stop, {drop, Msg}}
  end
.

filter_subscription(Acc, #presence{from = From, to = To, type = subscribe} = Pres) ->
  case is_blocking(Pres) of
    true ->
      case check_subscription(From, To) of
        false -> {stop, false};
        true -> Acc
      end;
    false -> Acc
  end
;

filter_subscription(Acc, _) -> Acc.

%%%===================================================================
%%% Internal functions
%%%===================================================================
check_message(#message{from = From, to = To, lang = Lang} = Msg) ->
  case is_blocking(Msg) of
    true ->
      case check_subscription(From, To) of
        false ->
          Txt = <<"Messages from strangers are rejected for this recipient">>,
          Err = xmpp:err_policy_violation(Txt, Lang),
          Msg1 = maybe_adjust_from(Msg),
          ejabberd_router:route_error(Msg1, Err),
          deny;
        true -> allow
      end;
    false -> allow
  end
.

check_subscription(From, To) ->
  mod_roster:is_subscribed(From, To)
.

is_blocking(Pkt) ->
  To = xmpp:get_to(Pkt),
  From = xmpp:get_from(Pkt),

  IsToAnotherUser = (To#jid.luser /= From#jid.luser orelse To#jid.lserver /= From#jid.lserver),
  HasMessageContent = case Pkt of
      #message{body = [], subject = []} -> false;
      _ -> true
    end,
  IsFromActualUserOrRemoteServer = (From#jid.luser /= <<"">> orelse not ejabberd_router:is_my_host(From#jid.lserver)),
  IsToUserBlockingIncoming = is_blocking_user(To#jid.lserver, jid:encode(To)),

  IsToAnotherUser andalso HasMessageContent andalso IsFromActualUserOrRemoteServer andalso IsToUserBlockingIncoming
.

maybe_adjust_from(#message{type = groupchat, from = From} = Msg) -> Msg#message{from = jid:remove_resource(From)};

maybe_adjust_from(#message{} = Msg) -> Msg.

sets_bare_member({User, Server, <<"">>} = LBareJID, Set) ->
  case gb_sets:next(gb_sets:iterator_from(LBareJID, Set)) of
    {{User, Server, _}, _} -> true;
    _ -> false
  end
.

get_proc_name(Host) ->
  gen_mod:get_module_proc(Host, ?MODULE)
.

get_config_users_dict(Host) ->
  dict:from_list(lists:map(fun(User) -> {User, User} end, gen_mod:get_module_opt(Host, ?MODULE, blocking_users, [])))
.

get_blocking_users(Host) ->
  try gen_server:call(get_proc_name(Host), {get_blocking_users}, ?COMMAND_TIMEOUT)
  catch _:_ ->
    ?ERROR_MSG("Failed to get blocking users. Falling back to initial list from mod opts.", []),
    gen_mod:get_module_opt(Host, ?MODULE, blocking_users, [])
  end
.

is_blocking_user(Host, User) ->
  try gen_server:call(get_proc_name(Host), {is_blocking_user, User}, ?COMMAND_TIMEOUT)
  catch _:_ ->
    ?ERROR_MSG("Failed to check if user is blocking. Falling back to false.", []),
    false
  end
.

reset_blocking_users(Host) ->
  gen_server:call(get_proc_name(Host), {reset_blocking_users}, ?COMMAND_TIMEOUT)
.

clear_blocking_users(Host) ->
  gen_server:call(get_proc_name(Host), {clear_blocking_users}, ?COMMAND_TIMEOUT)
.

add_blocking_user(Host, User) ->
  gen_server:call(get_proc_name(Host), {add_blocking_user, User}, ?COMMAND_TIMEOUT)
.

remove_blocking_user(Host, User) ->
  gen_server:call(get_proc_name(Host), {remove_blocking_user, User}, ?COMMAND_TIMEOUT)
.

get_commands_spec() ->
  [
    #ejabberd_commands{
      name = add_blocking_user,
      tags = [block],
      desc = "Add a user to the list of users blocking incoming subscriptions (friend) requests and unsolicited messages",
      module = ?MODULE,
      function = add_blocking_user,
      args = [{host, binary}, {jid, binary}],
      result = {res, rescode}
    },

    #ejabberd_commands{
      name = remove_blocking_user,
      tags = [block],
      desc = "Remove a user from the list of users blocking incoming subscriptions (friend) requests and unsolicited messages",
      module = ?MODULE,
      function = remove_blocking_user,
      args = [{host, binary}, {jid, binary}],
      result = {res, rescode}
    },

    #ejabberd_commands{
      name = reset_blocking_users,
      tags = [block],
      desc = "Reset the list of blocking users to the initial module configuration",
      module = ?MODULE,
      function = reset_blocking_users,
      args = [{host, binary}],
      result = {res, rescode}
    },

    #ejabberd_commands{
      name = clear_blocking_users,
      tags = [block],
      desc = "Clear the list of blocking users such that the list is empty",
      module = ?MODULE,
      function = clear_blocking_users,
      args = [{host, binary}],
      result = {res, rescode}
    },

    #ejabberd_commands{
      name = get_blocking_users,
      tags = [block],
      desc = "Get the list of blocking users",
      module = ?MODULE,
      function = get_blocking_users,
      args = [{host, binary}],
      result = {blocking_users, {list, {blocking_user, string}}}
    }
  ]
.

depends(_Host, _Opts) -> [].

mod_opt_type(blocking_users) ->
  fun(I) when is_list(I) -> I end
.

mod_options(_Host) -> [
  blocking_users
]
.
