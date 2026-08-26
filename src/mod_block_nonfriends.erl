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
-export([start/2, stop/1, reload/3, depends/2, mod_options/1, mod_opt_type/1]).

%% hook handlers
-export([user_send_packet_handler/1]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").

-define(HOOK_PRIORITY, 0).

%%%===================================================================
%%% Callbacks and hooks
%%%===================================================================
start(Host, _Opts) ->
  ejabberd_hooks:add(user_send_packet, Host, ?MODULE, user_send_packet_handler, ?HOOK_PRIORITY)
.

stop(Host) ->
  ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, user_send_packet_handler, ?HOOK_PRIORITY)
.

reload(_Host, _NewOpts, _OldOpts) ->
  ok
.

%% DM room localparts are the participants' numeric user ids joined by "-",
%% for 1:1 and group DMs alike. Game channels are a bare numeric game id (no
%% dash) and Clubz / live-event rooms carry non-numeric segments, so requiring
%% every segment to be numeric identifies DM rooms without matching either.
%% Returns the participant id list, or false if this is not a DM room.
dm_participants(LUser) ->
  case binary:split(LUser, <<"-">>, [global]) of
    [_] -> false;
    Parts ->
      case lists:all(fun is_numeric_id/1, Parts) of
        true -> Parts;
        false -> false
      end
  end
.

is_numeric_id(<<"">>) -> false;
is_numeric_id(Bin) ->
  lists:all(fun(C) -> C >= $0 andalso C =< $9 end, binary_to_list(Bin))
.

get_host_from_server(Server) ->
  binary:replace(Server, <<"conference.">>, <<"">>)
.

user_send_packet_handler({#message{} = Msg, State} = Acc) ->
  case dm_participants((xmpp:get_to(Msg))#jid.luser) of
    false -> Acc;
    Participants ->
      case check_message(Msg, Participants) of
        allow -> Acc;
        deny -> {stop, {drop, State}}
      end
  end
;

user_send_packet_handler(Acc) -> Acc.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Every participant other than the sender must be a friend. The sender's own
%% JID carries the vhost the module is loaded on; the recipient's is the MUC
%% service, so the option lookup and roster lookups both key off the sender.
all_participants_subscribed(From, To, Participants) ->
  Server = get_host_from_server(To#jid.lserver),
  Others = [P || P <- Participants, P =/= From#jid.luser],
  lists:all(
    fun(P) -> mod_roster:is_subscribed(From, jid:make(P, Server)) end,
    Others
  )
.

check_message(#message{type = groupchat, from = From, to = To}, Participants) ->
  case gen_mod:get_module_opt(From#jid.lserver, ?MODULE, block_all_direct_messages) of
    true ->
      ?INFO_MSG("Dropped DM packet: block_all_direct_messages enabled", []),
      deny;
    _ ->
      case all_participants_subscribed(From, To, Participants) of
        true -> allow;
        false ->
          ?INFO_MSG("Auto dropped packet", []),
          deny
      end
  end
;

check_message(_Msg, _Participants) ->
  allow
.

depends(_Host, _Opts) ->
  []
.

mod_opt_type(block_all_direct_messages) ->
  fun (B) when is_boolean(B) -> B end
.

mod_options(_Host) ->
  [{block_all_direct_messages, false}]
.
