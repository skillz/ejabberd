%%%----------------------------------------------------------------------
%%% Erlang port of the ModPushSkillz Elixir module (skillz/mod_push_skillz). Posts
%%% trigger events to push-notification-service (POST <push_notification_url>):
%%%   - offline_message_hook, for a MUC/Sub message stored for an offline subscriber,
%%%     by skillz_sdk message_type: 0 chat (9/137), 1 VsFriends invite (10/142),
%%%     3 tournament nudge (10/159), 2 and anything else: none;
%%%   - roster_out_subscription: friend request (9/138) and accept (9/139), alternated
%%%     per pair through the double_push table, none to mod_block_incoming users.
%%% Fields are read by element name; the Elixir module read them by position.
%%%----------------------------------------------------------------------
-module(mod_push_skillz).
-behaviour(gen_mod).

-export([start/2, stop/1, depends/2, mod_options/1, mod_opt_type/1, mod_doc/0,
         on_offline_msg/1, on_roster_out_subscription/1]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("mod_privacy.hrl").

-record(double_push, {pair :: {integer(), integer()}, expires :: integer()}).

-define(MAX_TEXT, 200).

start(_Host, _Opts) ->
    ejabberd_mnesia:create(?MODULE, double_push,
                           [{disc_only_copies, [node()]}, {attributes, record_info(fields, double_push)}]),
    {ok, [{hook, offline_message_hook, on_offline_msg, 50},
          {hook, roster_out_subscription, on_roster_out_subscription, 60}]}.

stop(_Host) -> ok.

depends(_Host, _Opts) -> [].

mod_opt_type(push_notification_url) -> econf:binary().

mod_options(_Host) -> [{push_notification_url, <<>>}].

mod_doc() -> #{desc => ["Skillz push notifications for chat, invites, nudges and friend requests."]}.

on_offline_msg({_Action, #message{to = #jid{lserver = Host}} = Pkt} = Acc) ->
    try trigger(Pkt) of
        none -> ok;
        Event -> publish(Host, Event)
    catch _:Reason ->
        ?DEBUG("mod_push_skillz: no push for ~p: ~p", [Pkt, Reason])
    end,
    Acc;
on_offline_msg(Acc) ->
    Acc.

trigger(#message{sub_els = [#ps_event{items = #ps_items{
                   node = ?NS_MUCSUB_NODES_MESSAGES,
                   items = [#ps_item{sub_els = [#message{body = [_ | _]} = Inner]} | _]}} | _]}) ->
    [Sdk | _] = [El || #xmlel{name = <<"skillz_sdk">>} = El <- Inner#message.sub_els],
    ToId = binary_to_integer(Inner#message.to#jid.luser),
    case sdk_int(Sdk, <<"message_type">>, 0) of
        0 -> chat(Inner, ToId);
        1 -> invite(Sdk, ToId, #{trigger_category => 10, trigger_condition => 142});
        3 -> invite(Sdk, ToId, #{trigger_category => 10, trigger_condition => 159,
                                 recipient_player_name => maps:get(<<"recipientPlayerName">>, sdk_data(Sdk))});
        _ -> none
    end;
trigger(_) ->
    none.

chat(#message{from = #jid{luser = Room, lresource = Nick}, to = To} = Inner, ToId) ->
    FromId = binary_to_integer(Nick),
    Server = binary:replace(Inner#message.from#jid.lserver, <<"conference.">>, <<>>),
    case is_blocking(To#jid.luser, Nick, Server) of
        true -> none;
        false ->
            %% A correction (XEP-0308) is pushed with empty text, as the Elixir module did.
            Text = case lists:keymember(<<"replace">>, #xmlel.name, [E || #xmlel{} = E <- Inner#message.sub_els]) of
                       true -> <<>>;
                       false -> unicode:characters_to_binary(string:slice(xmpp:get_text(Inner#message.body), 0, ?MAX_TEXT))
                   end,
            #{from_user_id => FromId, user_ids => [ToId], muc_jid => Room, chat_message_text => Text,
              trigger_category => 9, trigger_condition => 137}
    end.

%% VsFriends invite and nudge: never to the sender's own copy.
invite(Sdk, ToId, Event) ->
    case binary_to_integer(fxml:get_path_s(Sdk, [{elem, <<"user_id">>}, cdata])) of
        ToId -> none;
        _ ->
            Data = sdk_data(Sdk),
            DisplayName = fxml:get_path_s(Sdk, [{elem, <<"username">>}, cdata]),
            Event#{user_ids => [ToId],
                   game_id => maps:get(<<"gameId">>, Data),
                   tournament_prize => maps:get(<<"tournamentPrize">>, Data, null),
                   inviting_players_name => maps:get(<<"hostContactName">>, Data, DisplayName)}
    end.

sdk_int(Sdk, Name, Default) ->
    case fxml:get_subtag(Sdk, Name) of
        false -> Default;
        El -> binary_to_integer(fxml:get_tag_cdata(El))
    end.

sdk_data(Sdk) ->
    Raw = fxml:get_path_s(Sdk, [{elem, <<"data">>}, cdata]),
    misc:json_decode(binary:replace(Raw, <<"&quot;">>, <<"\"">>, [global])).

%% The fork's mod_blocking:is_blocking/3: a full-block deny item for that user in the default list.
is_blocking(LUser, BlockedUser, Server) ->
    case mod_privacy:get_user_list(LUser, Server, default) of
        {ok, {_, Items}} ->
            lists:any(fun(#listitem{type = jid, action = deny, value = {U, _, _}} = I) ->
                              U == BlockedUser andalso
                                  (I#listitem.match_all orelse
                                   (I#listitem.match_iq andalso I#listitem.match_message andalso
                                    I#listitem.match_presence_in andalso I#listitem.match_presence_out));
                         (_) -> false
                      end, Items);
        _ -> false
    end.

on_roster_out_subscription(#presence{type = Type, from = #jid{luser = F}, to = #jid{luser = T, lserver = ToHost}}) ->
    try {binary_to_integer(F), binary_to_integer(T)} of
        {From, To} when Type == subscribe ->
            case mod_block_incoming:is_blocking_user(ToHost, T) of
                true -> ok;
                false -> alternate(ToHost, From, To, 138)
            end;
        {From, To} when Type == subscribed ->
            alternate(ToHost, From, To, 139);
        {From, To} ->
            mnesia:dirty_delete(double_push, {From, To}),
            mnesia:dirty_delete(double_push, {To, From})
    catch error:badarg ->
        ok
    end;
on_roster_out_subscription(_) ->
    ok.

%% shortcut: `expires` is written but never read, as in the Elixir module, so a pair alternates
%% push / no push forever; read it if pushes should resume after an hour.
alternate(Host, From, To, Condition) ->
    Expires = erlang:system_time(second) + 3600,
    case mnesia:dirty_read(double_push, {From, To}) of
        [_ | _] ->
            mnesia:dirty_write(#double_push{pair = {To, From}, expires = Expires}),
            mnesia:dirty_delete(double_push, {From, To});
        [] ->
            mnesia:dirty_write(#double_push{pair = {From, To}, expires = Expires}),
            mnesia:dirty_delete(double_push, {To, From}),
            publish(Host, #{from_user_id => From, user_ids => [To], chat_message_text => <<>>,
                            trigger_category => 9, trigger_condition => Condition})
    end.

publish(Host, Event) ->
    case gen_mod:get_module_opt(Host, ?MODULE, push_notification_url) of
        <<>> -> ok;
        Url ->
            httpc:request(post, {binary_to_list(Url), [], "application/json", misc:json_encode(Event)},
                          [{timeout, 5000}], [{sync, false}, {receiver, fun log_reply/1}])
    end.

log_reply({_Ref, {error, Reason}}) ->
    ?ERROR_MSG("Error posting to push notification service: ~p", [Reason]);
log_reply(_) ->
    ok.
