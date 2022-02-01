%%%-------------------------------------------------------------------
%%% File    : mod_block_incoming.erl
%%% Author  : Devin Sills <dsills@skillz.com>
%%% Created : 31 Jan 2022 by Devin Sills <dsills@skillz.com>
%%%
%%%----------------------------------------------------------------------

-module(mod_block_incoming).
-author('dsills@skillz.com').

-behaviour(gen_mod).

%% gen_mod callbacks.
-export([start/2, stop/1, reload/3, depends/2, mod_options/1, mod_opt_type/1]).

%% hook handlers
-export([filter_packet/1, filter_offline_msg/1, filter_subscription/2]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").

-define(HOOK_PRIORITY, 25).

%%%===================================================================
%%% Callbacks and hooks
%%%===================================================================
start(Host, _Opts) ->
  ejabberd_hooks:add(user_receive_packet, Host, ?MODULE, filter_packet, ?HOOK_PRIORITY),
  ejabberd_hooks:add(roster_in_subscription, Host, ?MODULE, filter_subscription, ?HOOK_PRIORITY),
  ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, filter_offline_msg, ?HOOK_PRIORITY)
.

stop(Host) ->
  ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE, filter_packet, ?HOOK_PRIORITY),
  ejabberd_hooks:delete(roster_in_subscription, Host, ?MODULE, filter_subscription, ?HOOK_PRIORITY),
  ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, filter_offline_msg, ?HOOK_PRIORITY)
.

reload(_Host, _NewOpts, _OldOpts) ->
  ok
.

%% filter messages
filter_packet({#message{from = From} = Msg, State} = Acc) ->
  LFrom = jid:tolower(From),
  LBareFrom = jid:remove_resource(LFrom),
  #{pres_a := PresA} = State,
  FromUserInPresencePacket = gb_sets:is_element(LFrom, PresA) orelse gb_sets:is_element(LBareFrom, PresA) orelse sets_bare_member(LBareFrom, PresA),
  case FromUserInPresencePacket of
    false -> %% from-user not in presence struct so this is an outgoing DM to someone else
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
  case need_check(Pres) of
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
  case need_check(Msg) of
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

need_check(Pkt) ->
  To = xmpp:get_to(Pkt),
  ToLServer = To#jid.lserver,
  BlockingUsers = gen_mod:get_module_opt(ToLServer, ?MODULE, blocking_users, []),
  From = xmpp:get_from(Pkt),

  IsToUserBlockingIncoming = lists:member(jid:encode(To), BlockingUsers),
  IsToAnotherUser = To#jid.luser /= From#jid.luser orelse To#jid.lserver /= From#jid.lserver,
  HasMessageContent = case Pkt of
    #message{body = [], subject = []} -> false;
    _ -> true
  end,
  IsFromActualUserOrRemoteServer = From#jid.luser /= <<"">> orelse not ejabberd_router:is_my_host(From#jid.lserver),

  IsToUserBlockingIncoming andalso IsToAnotherUser andalso HasMessageContent andalso IsFromActualUserOrRemoteServer
.

maybe_adjust_from(#message{type = groupchat, from = From} = Msg) -> Msg#message{from = jid:remove_resource(From)};

maybe_adjust_from(#message{} = Msg) -> Msg.

sets_bare_member({User, Server, <<"">>} = LBareJID, Set) ->
  case gb_sets:next(gb_sets:iterator_from(LBareJID, Set)) of
    {{User, Server, _}, _} -> true;
    _ -> false
  end
.

depends(_Host, _Opts) -> [].

mod_opt_type(blocking_users) ->
  fun(I) when is_list(I) -> I end
.

mod_options(_Host) -> [
  blocking_users
]
.
