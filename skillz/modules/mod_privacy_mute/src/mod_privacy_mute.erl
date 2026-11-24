%%%----------------------------------------------------------------------
%%% The fork's privacy-list semantics for incoming messages, which shipped clients
%%% write into skillz_default_privacy_list:
%%%   - an item that matches messages only is a "mute": it does not stop live delivery
%%%     to a session, only offline storage (and so push);
%%%   - an item that matches message + presence-in + presence-out is a "block";
%%%   - a jid item user@server also matches any sender whose resource is that user,
%%%     so blocking <id>@conference.<host> hides that user's messages in every room.
%%% Runs before mod_privacy and decides incoming messages itself; everything else is
%%% left to mod_privacy.
%%%----------------------------------------------------------------------
-module(mod_privacy_mute).
-behaviour(gen_mod).

-export([start/2, stop/1, depends/2, mod_options/1, mod_doc/0, check_packet/4]).

-include_lib("xmpp/include/xmpp.hrl").
-include("mod_privacy.hrl").

start(_Host, _Opts) ->
    {ok, [{hook, privacy_check_packet, check_packet, 40}]}.

stop(_Host) -> ok.

depends(_Host, _Opts) -> [{mod_privacy, hard}].

mod_options(_Host) -> [].

mod_doc() -> #{desc => ["Fork-compatible mute/block privacy semantics for Skillz clients."]}.

%% A c2s state means live delivery to a session; a bare JID means ejabberd_sm is
%% routing to an account with no session that takes it (offline storage).
check_packet(Acc, #{jid := JID} = State, #message{} = Pkt, in) ->
    decide(Acc, JID, maps:get(privacy_active_list, State, default), Pkt, live);
check_packet(Acc, #jid{} = JID, #message{} = Pkt, in) ->
    decide(Acc, JID, default, Pkt, offline);
check_packet(Acc, _, _, _) ->
    Acc.

decide(Acc, #jid{luser = LUser, lserver = LServer}, ListName, Pkt, Path) ->
    case mod_privacy:get_user_list(LUser, LServer, ListName) of
        {ok, {_, Items}} when Items /= [] ->
            case lists:all(fun(#listitem{type = T}) -> T == jid end, Items) of
                true -> {stop, first_match(Items, jid:tolower(xmpp:get_from(Pkt)), Path)};
                %% shortcut: group/subscription items fall back to stock semantics; Skillz clients only write jid items.
                false -> Acc
            end;
        _ ->
            Acc
    end.

first_match([], _From, _Path) ->
    allow;
first_match([#listitem{value = Value, action = Action} = Item | Rest], From, Path) ->
    case applies(Item, Path) andalso jid_match(Value, From) of
        true -> Action;
        false -> first_match(Rest, From, Path)
    end.

applies(#listitem{match_all = true}, _Path) -> true;
applies(#listitem{match_message = true, match_presence_in = true, match_presence_out = true}, _Path) -> true;
applies(#listitem{match_message = true, match_presence_in = false, match_presence_out = false}, Path) -> Path == offline;
applies(_Item, _Path) -> false.

jid_match({User, Server, <<>>}, {U, S, R}) when User /= <<>> ->
    (User == U andalso Server == S) orelse R == User;
jid_match({<<>>, Server, <<>>}, {_, S, _}) -> Server == S;
jid_match({<<>>, Server, Resource}, {_, S, R}) -> Server == S andalso Resource == R;
jid_match(Value, From) -> Value == From.
