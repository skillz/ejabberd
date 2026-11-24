%%%----------------------------------------------------------------------
%%% The fork's MUC/Sub delivery rules for a groupchat message that a room sends to a
%%% subscriber who is not in the room (online or not):
%%%   - it goes to offline storage through offline_message_hook (so mod_push_skillz
%%%     sends a push), never as a live event, after a privacy check that uses the
%%%     occupant JID (room/nick) as the sender, so a block on <id>@conference.<host>
%%%     and a room mute both apply;
%%%   - when the message's skillz_sdk has a message_type and its user_id is the
%%%     subscriber (the sender's own copy), it is routed normally instead;
%%%   - a message with no body (a chat state) is dropped.
%%% skillz_sdk is found by name; the fork read it as the 3rd child and crashed the
%%% room process on any groupchat with fewer children (chat states).
%%%----------------------------------------------------------------------
-module(mod_mucsub_offline).
-behaviour(gen_mod).

-export([start/2, stop/1, depends/2, mod_options/1, mod_doc/0, filter_packet/1]).

-include_lib("xmpp/include/xmpp.hrl").

start(_Host, _Opts) ->
    ejabberd_hooks:add(filter_packet, ?MODULE, filter_packet, 50).

stop(_Host) ->
    ejabberd_hooks:delete(filter_packet, ?MODULE, filter_packet, 50).

depends(_Host, _Opts) -> [{mod_offline, hard}].

mod_options(_Host) -> [].

mod_doc() -> #{desc => ["Fork MUC/Sub offline delivery rules for subscribers not in the room."]}.

filter_packet(#message{to = #jid{lresource = <<>>, lserver = LServer} = To,
                       sub_els = [#ps_event{items = #ps_items{
                                    node = ?NS_MUCSUB_NODES_MESSAGES,
                                    items = [#ps_item{sub_els = [#message{type = groupchat, from = #jid{}} = Inner]}]}}]} = Pkt) ->
    case ejabberd_router:is_my_host(LServer) andalso not is_own_copy(Inner, To#jid.luser) of
        true ->
            Check = xmpp:set_from(Pkt, Inner#message.from),
            Inner#message.body /= [] andalso
                allow == ejabberd_hooks:run_fold(privacy_check_packet, LServer, allow, [To, Check, in]) andalso
                ejabberd_hooks:run_fold(offline_message_hook, LServer, {bounce, Pkt}, []),
            drop;
        false ->
            Pkt
    end;
filter_packet(Pkt) ->
    Pkt.

is_own_copy(#message{sub_els = Els}, LUser) ->
    case [El || #xmlel{name = <<"skillz_sdk">>} = El <- Els] of
        [Sdk | _] ->
            fxml:get_subtag(Sdk, <<"message_type">>) /= false
                andalso fxml:get_path_s(Sdk, [{elem, <<"user_id">>}, cdata]) == LUser;
        [] ->
            false
    end.
