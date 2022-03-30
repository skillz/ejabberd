%%%-------------------------------------------------------------------
%%% File    : mod_block_nonfriends.erl
%%% Author  : Devin Sills <dsills@skillz.com>
%%% Created : 28 Mar 2022 by Devin Sills <dsills@skillz.com>
%%%
%%%----------------------------------------------------------------------

-module(mod_block_nonfriends).
-author('dsills@skillz.com').

-behaviour(gen_mod).

%% gen_mod callbacks.
-export([start/2, stop/1, reload/3, depends/2, mod_options/1]).

%% hook handlers
-export([filter_packet/1]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").

-define(HOOK_PRIORITY, 0).

%%%===================================================================
%%% Callbacks and hooks
%%%===================================================================
start(Host, _Opts) ->
  ejabberd_hooks:add(user_send_packet, Host, ?MODULE, filter_packet, ?HOOK_PRIORITY)
.

stop(Host) ->
  ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, filter_packet, ?HOOK_PRIORITY)
.

reload(_Host, _NewOpts, _OldOpts) ->
  ok
.

%% returns if this is a grouchat message between only two users
is_duo_direct_message(Msg) ->
  Recipient = xmpp:get_to(Msg),
  case binary:matches(Recipient#jid.luser, [<<"-">>]) of
    [] -> false;
    Matches -> length(Matches) == 1
  end
.

%% the To#jid.luser is a dash separated list of all users in this duo DM
%% and a duo DM is just a DM between two users so we replace the From with blank
%% to extract the true ToUser recipient
extract_duo_direct_message_to(user_send_packet, From, To) ->
  FromUser = From#jid.luser,
  ToUser = binary:replace(
    binary:replace(To#jid.luser, <<"-", FromUser/binary>>, <<"">>),
    <<FromUser/binary, "-">>, <<"">>
  ),
  %% groupchat server name has conference in it so we take that out
  Server = get_host_from_server(To#jid.lserver),
  jid:make(ToUser, Server)
.

get_host_from_server(Server) ->
  binary:replace(Server, <<"conference.">>, <<"">>)
.

filter_packet({#message{} = Msg, State} = Acc) ->
  case is_duo_direct_message(Msg) of
    false -> Acc;
    _ ->
      case check_message(Msg) of
        allow -> Acc;
        deny ->
          ?INFO_MSG("Auto dropped packet", []),
          {stop, {drop, State}}
      end
  end
;

filter_packet(Acc) -> Acc.

%%%===================================================================
%%% Internal functions
%%%===================================================================
check_subscription(From, To) ->
  NewTo = extract_duo_direct_message_to(user_send_packet, From, To),
  mod_roster:is_subscribed(From, NewTo)
.

check_message(#message{type = groupchat, from = From, to = To, lang = Lang} = Msg) ->
  case check_subscription(From, To) of
    false -> deny;
    true -> allow
  end
;

check_message(_Msg) ->
  allow
.

depends(_Host, _Opts) ->
  []
.

mod_options(_Host) ->
  []
.
