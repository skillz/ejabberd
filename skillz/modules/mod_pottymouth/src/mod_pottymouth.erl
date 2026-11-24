%%%----------------------------------------------------------------------
%%% Port of skillz/mod_pottymouth (mod_pottymouth + mod_filter_moderators):
%%%   - filter_packet: chat/groupchat words found in the message language's list
%%%     (after leet normalization) are replaced by ****;
%%%   - user_send_packet (automod): in a game room (no '-') a bad word adds strikes in
%%%     user_warning (public_ban/public_mute lists: 2, other: 1); at 3 strikes in 90
%%%     days the user is quarantined through user-details-service and a support ticket
%%%     is opened, otherwise "Skillz Admin" posts a warning in the room. In a DM room a
%%%     private_ban_en word opens a support ticket;
%%%   - user_send_packet: skillz_sdk user_role 1/2/3 (staff, moderator, admin) from a
%%%     user that user-details-service does not confirm is dropped, banned and ticketed.
%%% The fork derived its environment from the node name; this module derives it from
%%% the vhost (chat.skillz.com, chat.staging.*, chat.qa.*).
%%%----------------------------------------------------------------------
-module(mod_pottymouth).
-behaviour(gen_mod).

-export([start/2, stop/1, depends/2, mod_options/1, mod_opt_type/1, mod_doc/0,
         on_filter_packet/1, on_user_send_packet/1, on_user_send_packet_staff/1, send_warning/3]).

-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

-define(QUARANTINE_URL, "http://user-details-service.user-details-service/chat/quarantines").
-define(USER_PROFILE_URL, "http://user-details-service.user-details-service/profile/show").
-define(AUTO_MOD_ID, <<"105432430">>).
-define(MUTE, <<"1">>).
-define(BAN, <<"0">>).
-define(PUBLIC_QUARANTINE_REASON, <<"Inappropriate Language">>).
-define(PRIVATE_QUARANTINE_REASON, <<"Report Abuse">>).
-define(MAX_STRIKES, 3).
-define(LOOKBACK_DAYS, 90).
-define(WARN_DELAY, 1000).
-define(ROLE_TTL, 3600).

-record(chat_moderator, {user_id, is_moderator, ttl}).
-record(skillz_staff, {user_id, is_staff, ttl}).

start(_Host, Opts) ->
    [persistent_term:put({?MODULE, list, atom_to_binary(Lang)}, load_words(File)) || {Lang, File} <- gen_mod:get_opt(blacklists, Opts)],
    [persistent_term:put({?MODULE, charmap, atom_to_binary(Lang)}, load_charmap(File)) || {Lang, File} <- gen_mod:get_opt(charmaps, Opts)],
    ejabberd_mnesia:create(?MODULE, chat_moderator, [{ram_copies, [node()]}, {attributes, record_info(fields, chat_moderator)}]),
    ejabberd_mnesia:create(?MODULE, skillz_staff, [{ram_copies, [node()]}, {attributes, record_info(fields, skillz_staff)}]),
    ejabberd_hooks:add(filter_packet, ?MODULE, on_filter_packet, 30),
    {ok, [{hook, user_send_packet, on_user_send_packet_staff, 29},
          {hook, user_send_packet, on_user_send_packet, 31}]}.

stop(_Host) ->
    ejabberd_hooks:delete(filter_packet, ?MODULE, on_filter_packet, 30).

depends(_Host, _Opts) -> [{mod_admin_extra, hard}, {mod_vcard, soft}].

mod_opt_type(blacklists) -> econf:map(econf:atom(), econf:file());
mod_opt_type(charmaps) -> econf:map(econf:atom(), econf:file());
%% Declared so the chart's block validates; the fork never read it either.
mod_opt_type(automod) -> econf:any().

mod_options(_Host) -> [{blacklists, []}, {charmaps, []}, {automod, []}].

mod_doc() -> #{desc => ["Skillz word masking, automod strikes and staff-impersonation filter."]}.

%%%===================================================================
%%% Masking
%%%===================================================================
on_filter_packet(#message{type = groupchat, to = #jid{lresource = R}} = Msg) when R /= <<>> ->
    Msg; % a room's copy to an occupant: the text was masked on its way into the room
on_filter_packet(#message{type = Type, body = [First | _] = Body} = Msg) when Type == chat; Type == groupchat ->
    case xmpp:get_text(Body) of
        <<>> -> Msg;
        Text ->
            {_Action, Masked} = filter_text(lang(Msg), Text, none),
            Msg#message{body = [First#text{data = Masked}]}
    end;
on_filter_packet(Pkt) ->
    Pkt.

lang(#message{lang = <<>>}) -> <<"default">>;
lang(#message{lang = L}) -> L.

filter_text(Lang, Text, Ctx) ->
    Words = binary:split(Text, <<" ">>, [global, trim_all]),
    {Action, Out} = lists:foldl(fun(W, Acc) -> censor(Lang, Ctx, W, Acc) end, {{none, 0, <<>>}, []}, Words),
    {Action, iolist_to_binary(lists:join(<<" ">>, lists:reverse(Out)))}.

censor(Lang, Ctx, Word, {{OldAction, OldMax, _} = Old, Out}) ->
    Normalized = case skip_normalization(Word) of
                     true -> Word;
                     false -> normalize(Lang, Word)
                 end,
    Full = re:replace(string:lowercase(Normalized), "[^a-zA-Z0-9 ]", "", [global, unicode, {return, binary}]),
    IsBad = member(Lang, Full),
    Out2 = [case IsBad of true -> <<"****">>; false -> Word end | Out],
    Action = case Ctx of
                 none -> Old;
                 {null, _, _, _} ->
                     case member(<<"private_ban_en">>, Full) of
                         true -> {private_ban, OldMax, Word};
                         false -> Old
                     end;
                 _ when not IsBad -> Old;
                 _ ->
                     case {member(<<"public_ban_en">>, Full), member(<<"public_mute_en">>, Full)} of
                         {true, _} when OldMax < 2; OldAction /= public_ban -> {public_ban, 2, Word};
                         {true, _} -> Old;
                         {false, true} when OldMax < 2 -> {public_mute, 2, Word};
                         {false, true} -> Old;
                         {false, false} when OldMax < 1 -> {public_censor, 1, Word};
                         {false, false} -> Old
                     end
             end,
    {Action, Out2}.

%% A word with no letters is left alone, unless it is only @, $, 5 or spaces.
skip_normalization(Word) ->
    re:replace(Word, "[^a-zA-Z ]", "", [global, unicode, {return, binary}]) == <<>>
        andalso re:replace(Word, "[@$5 ]", "", [global, unicode, {return, binary}]) /= <<>>.

normalize(Lang, Word) ->
    case persistent_term:get({?MODULE, charmap, Lang}, undefined) of
        undefined -> Word;
        CharMap -> unicode:characters_to_binary(normalize_leet:normalize(CharMap, unicode:characters_to_list(Word)))
    end.

%% A language without a list of its own uses the default list.
member(Lang, Word) ->
    Words = persistent_term:get({?MODULE, list, Lang}, persistent_term:get({?MODULE, list, <<"default">>}, #{})),
    maps:is_key(Word, Words).

load_words(File) ->
    {ok, Bin} = file:read_file(File),
    maps:from_keys(binary:split(Bin, [<<"\n">>, <<"\r">>], [global, trim_all]), true).

load_charmap(File) ->
    {ok, [Pairs]} = file:consult(File),
    maps:from_list(Pairs).

%%%===================================================================
%%% Automod
%%%===================================================================
%% Only for c2s sessions (`sid`): 26.09's send_stanza command also runs user_send_packet,
%% and the fork never checked service posts.
on_user_send_packet({#message{type = Type, body = [_ | _] = Body, from = From, to = To} = Pkt, #{sid := _}} = Acc)
  when Type == chat; Type == groupchat ->
    Users = binary:split(To#jid.user, [<<"-">>, <<"\r">>, <<"\n">>], [global, trim_all]),
    Game = case Users of [G] -> G; _ -> null end,
    case xmpp:get_text(Body) of
        <<>> -> ok;
        Text ->
            {{Action, Strikes, Word}, _} = filter_text(lang(Pkt), Text, {Game, From#jid.user, Users, From#jid.lserver}),
            act(Action, From#jid.user, From#jid.lserver, Game, Users, Word, Text, Strikes)
    end,
    Acc;
on_user_send_packet(Acc) ->
    Acc.

act(private_ban, User, Host, null, Users, Word, Text, _) ->
    With = case [U || U <- Users, U /= User] of
               [] -> <<>>;
               Others -> [<<"with ">>, lists:join(<<", ">>, Others)]
           end,
    create_support_ticket(Host, null, ?PRIVATE_QUARANTINE_REASON,
                          [User, <<" was flagged by the Auto-moderator for posting \"">>, Word,
                           <<"\" in a Direct Message ">>, With, <<"\n Whole message: \"">>, Text, <<"\"">>]);
act(public_ban, User, Host, Game, _, Word, Text, Strikes) -> strike(User, Host, Game, ?BAN, Word, Text, Strikes);
act(public_mute, User, Host, Game, _, Word, Text, Strikes) -> strike(User, Host, Game, ?MUTE, Word, Text, Strikes);
act(public_censor, User, Host, Game, _, Word, Text, Strikes) -> strike(User, Host, Game, ?MUTE, Word, Text, Strikes);
act(_, _, _, _, _, _, _, _) -> ok.

strike(User, Host, Game, Status, Word, Text, Strikes) ->
    Current = sql_count(Host, ?SQL("select @(coalesce(sum(strikes), 0))d from user_warning where user_id = %(User)s"
                                   " and enabled = 1 and date_created > now() - interval 90 day")),
    Comment = iolist_to_binary([User, <<" was warned with ">>, integer_to_binary(Strikes),
                                <<" strikes by the Auto-moderator for posting \"">>, Word,
                                <<"\" in the public game room for game ID ">>, Game]),
    warn_user(Host, User, Strikes, Comment),
    case Current + Strikes >= ?MAX_STRIKES of
        true ->
            StatusString = case Status of ?MUTE -> <<"muted">>; ?BAN -> <<"banned">> end,
            quarantine(User, Game, Status, expiration(quarantine_hours(User, Host)), Word, Current),
            create_support_ticket(Host, Game, ?PUBLIC_QUARANTINE_REASON,
                                  [User, <<" was ">>, StatusString, <<" by the Auto-moderator for posting \"">>, Word,
                                   <<"\" in the public game room for game ID ">>, Game, <<". \n Whole message: \"">>, Text,
                                   <<"\" \n This Player had ">>, integer_to_binary(Current), <<" strikes prior to this offense.">>]);
        false ->
            timer:apply_after(?WARN_DELAY, ?MODULE, send_warning, [Host, Game, User])
    end.

warn_user(Host, User, Strikes, Comment) ->
    Reason = ?PUBLIC_QUARANTINE_REASON,
    ModeratorId = binary_to_integer(?AUTO_MOD_ID),
    UserId = binary_to_integer(User),
    case ejabberd_sql:sql_query(Host,
           ?SQL("insert into user_warning (comment, date_created, enabled, expiration_date, last_updated,"
                " moderator_user_id, reason, strikes, user_id) values (%(Comment)s, now(), 1, now(), now(),"
                " %(ModeratorId)d, %(Reason)s, %(Strikes)d, %(UserId)d)")) of
        {updated, _} -> ok;
        Err -> ?ERROR_MSG("Cannot insert warning in user_warning: ~p", [Err])
    end.

%% 72 hours for a first quarantine in 90 days, then 168, then 720.
quarantine_hours(User, Host) ->
    case sql_count(Host, ?SQL("select @(count(*))d from user_affiliation where nick = %(User)s"
                              " and date_created > now() - interval 90 day")) of
        0 -> 72;
        1 -> 168;
        _ -> 720
    end.

sql_count(Host, Query) ->
    case catch ejabberd_sql:sql_query(Host, Query) of
        {selected, [{N}]} when is_integer(N) -> N;
        _ -> 0
    end.

expiration(Hours) ->
    {{Y, M, D}, {H, Mi, S}} = calendar:gregorian_seconds_to_datetime(
                                calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + Hours * 3600),
    iolist_to_binary(io_lib:format("~B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ", [Y, M, D, H, Mi, S])).

quarantine(User, Game, Status, Expiration, Word, Current) ->
    {Reason, StatusString, StatusEnum} = case Status of
                                             ?MUTE -> {<<"public_room_mute">>, <<"muted">>, <<"MUTE">>};
                                             ?BAN -> {<<"public_room_ban">>, <<"banned">>, <<"BAN">>}
                                         end,
    Comment = iolist_to_binary([User, <<" was ">>, StatusString, <<" by the Auto-moderator for posting ">>,
                                re:replace(Word, "[^a-zA-Z0-9 ]", "", [global, unicode, {return, binary}]),
                                <<" in the public game room for game ID ">>, Game, <<". This Player had ">>,
                                integer_to_binary(Current), <<" strikes prior to this offense">>]),
    post_quarantine(#{status => StatusEnum, expiration_date => Expiration, reason => Reason, comment => Comment,
                      user_id => User, announce_in_chat_game_id => Game}).

post_quarantine(Fields) ->
    post(?QUARANTINE_URL, [], Fields#{moderator_id => ?AUTO_MOD_ID, moderator_username => <<"auto_moderator">>}).

create_support_ticket(Host, Game, Subject, Message) ->
    Body = #{user_id => ?AUTO_MOD_ID, game_id => Game, subject => Subject, message => iolist_to_binary(Message)},
    case lists:member(Game, [<<"81">>, <<"8626">>]) of
        true ->
            post("http://user-support-service.user-support-service/v1/users/105432430/support_tickets",
                 [{"skillz-user-id", "105432430"}, {"skillz-game-id", binary_to_list(Game)}], Body);
        false ->
            Env = case env(Host) of production -> "prod"; staging -> "staging"; qa -> "qa" end,
            post("http://lb-api." ++ Env ++ ".skillz.com:9100/communication/v1/support_tickets", [], Body)
    end.

post(Url, Headers, Fields) ->
    httpc:request(post, {Url, Headers, "application/json", misc:json_encode(Fields)}, [{timeout, 5000}], [{sync, false}]).

send_warning(Host, Game, User) ->
    Nick = case catch mod_vcard:get_vcard(User, Host) of
               [VCard | _] -> fxml:get_path_s(VCard, [{elem, <<"NICKNAME">>}, cdata]);
               _ -> <<>>
           end,
    Body = <<"@", Nick/binary, ", profanity or the use of numbers, letters, or symbols to disguise such words is not permitted in chat.">>,
    {Logo, Flag} = case env(Host) of
                       production -> {<<"https://s3.amazonaws.com/skillz-content-prod">>, <<"https://cdn.skillz.com">>};
                       staging -> {<<"https://s3.amazonaws.com/skillz-content-stage">>, <<"https://cdn.staging.skillz.com">>};
                       qa -> {<<"https://s3.amazonaws.com/skillz-content-dev">>, <<"https://cdn.qa.skillz.com">>}
                   end,
    To = <<Game/binary, "@conference.", Host/binary>>,
    Msg = #message{id = p1_rand:get_string(), type = groupchat, body = [#text{data = Body}],
                   sub_els = [#xmlel{name = <<"skillz_sdk">>, attrs = [{<<"xmlns">>, <<"xmpp:skillz">>}],
                                     children = [#xmlel{name = N, children = [{xmlcdata, V}]} || {N, V} <-
                                                 [{<<"avatar_url">>, <<Logo/binary, "/default-profile-pics/Skillz-Profile-Picture-20220706.png">>},
                                                  {<<"flag_url">>, <<Flag/binary, "/flags/US.png">>},
                                                  {<<"user_id">>, <<"-1">>}, {<<"username">>, <<"Skillz Admin">>},
                                                  {<<"user_role">>, <<"2">>}, {<<"user_mentions">>, <<"@none">>},
                                                  {<<"message_type">>, <<"0">>}]]}]},
    mod_admin_extra:send_stanza(cas_jid(Host), To, fxml:element_to_binary(xmpp:encode(Msg))).

%% chat.skillz.com -> production, chat.staging.<...> -> staging, chat.qa.<...> -> qa.
env(Host) ->
    case binary:split(Host, <<".">>, [global]) of
        [_, <<"staging">> | _] -> staging;
        [_, <<"qa">> | _] -> qa;
        _ -> production
    end.

cas_jid(<<"chat.", Rest/binary>>) -> <<"skillz-cas@chat-admin.", Rest/binary>>;
cas_jid(Host) -> <<"skillz-cas@", Host/binary>>.

%%%===================================================================
%%% Staff / moderator impersonation
%%%===================================================================
on_user_send_packet_staff({#message{type = Type, from = From, to = To} = Msg, #{sid := _} = State} = Acc)
  when Type == chat; Type == groupchat ->
    case impersonation(Msg) of
        false -> Acc;
        Kind ->
            User = From#jid.user,
            Game = case binary:split(To#jid.user, [<<"-">>, <<"\r">>, <<"\n">>], [global, trim_all]) of
                       [G] -> G;
                       _ -> <<>>
                   end,
            Where = case Game of <<>> -> <<"a DM">>; _ -> <<"the public game room for game ID ", Game/binary>> end,
            Comment = iolist_to_binary([User, <<" was banned by the Auto-moderator for ">>, Kind, <<" in ">>, Where,
                                        <<". Whole message: ">>, xmpp:get_text(Msg#message.body)]),
            Reason = <<Kind/binary, "ImpersonationAttempt">>,
            post_quarantine(#{status => <<"BAN">>, expiration_date => expiration(quarantine_hours(User, From#jid.lserver)),
                              reason => Reason, comment => Comment, user_id => User}),
            create_support_ticket(From#jid.lserver, Game, Reason, Comment),
            ?ERROR_MSG("Message dropped by mod_pottymouth: ~ts", [Comment]),
            {stop, {drop, State}}
    end;
on_user_send_packet_staff(Acc) ->
    Acc.

impersonation(#message{from = #jid{user = User, lserver = Domain}, sub_els = Els}) ->
    case [El || #xmlel{name = <<"skillz_sdk">>} = El <- Els] of
        [Sdk | _] ->
            case fxml:get_path_s(Sdk, [{elem, <<"user_role">>}, cdata]) of
                <<"1">> -> not is_role(skillz_staff, User, Domain) andalso <<"Staff">>;
                <<"2">> -> not is_role(chat_moderator, User, Domain) andalso <<"ChatModerator">>;
                <<"3">> -> not is_role(skillz_staff, User, Domain) andalso not is_role(chat_moderator, User, Domain)
                               andalso <<"SkillzAdmin">>;
                _ -> false
            end;
        [] ->
            false
    end.

%% The chat-moderation app's web-moderator user on the chat-admin vhost is always both.
is_role(_Table, <<"web-moderator">>, <<"chat-admin.", _/binary>>) ->
    true;
is_role(Table, User, _Domain) ->
    Now = erlang:system_time(second),
    case mnesia:dirty_read(Table, User) of
        [{Table, _, Is, Ttl}] when Ttl >= Now -> Is;
        _ ->
            Field = case Table of skillz_staff -> <<"skillz_staff">>; chat_moderator -> <<"chat_moderator">> end,
            Body = iolist_to_binary(["{\"current_user_id\": ", User, ", \"target_user_id\": ", User, "}"]),
            case httpc:request(post, {?USER_PROFILE_URL, [], "application/json", Body}, [{timeout, 5000}], []) of
                {ok, {{_, 200, _}, _, Resp}} ->
                    Is = maps:get(Field, misc:json_decode(iolist_to_binary(Resp)), false) == true,
                    mnesia:dirty_write({Table, User, Is, Now + ?ROLE_TTL}),
                    Is;
                Failure ->
                    ?ERROR_MSG("User Profile Request Failure: ~p", [Failure]),
                    false
            end
    end.
