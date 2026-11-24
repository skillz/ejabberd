%%%----------------------------------------------------------------------
%%% The fork's admin HTTP API as our services call it, in front of mod_http_api:
%%%   request_handlers:
%%%     /api: mod_admin_api_compat
%%%
%%% - Pins every call to API version `api_version` (default 2): callers send the
%%%   pre-v3 argument names (name/service/jid). mod_http_api takes its default
%%%   version from the vhost named in the Host header, and callers address the
%%%   admin Service by its cluster DNS name, so its default_version never applies.
%%% - Command kick_user_sessions(user, host, reason), served on the fork's
%%%   POST /api/kick_session path: ends every session of the user with a
%%%   <conflict/> stream error whose text is the reason. Shipped native clients
%%%   read that text as the quarantine status, so stock kick_user ("has been
%%%   kicked") cannot replace it, and core owns the kick_session name.
%%% - Command start_room(name, title): user-details-service creates a game room with
%%%   its title, or sets the title of an existing room that has none.
%%% - Command get_room_summary(room, limit, last_message_id): the room's newest join-history
%%%   messages after last_message_id (at most limit, 5 by default, 50 at most) with their
%%%   skillz_sdk username, user_role and avatar_url. Public through the Istio gateway. Its
%%%   HTTP path is served here, oldest first: mod_http_api sorts every list result.
%%%----------------------------------------------------------------------
-module(mod_admin_api_compat).
-behaviour(gen_mod).

-export([start/2, stop/1, depends/2, mod_options/1, mod_opt_type/1, mod_doc/0,
         process/2, kick_user_sessions/3, start_room/2, get_room_summary/3]).

-include("ejabberd_commands.hrl").
-include("ejabberd_http.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("mod_muc_room.hrl").

start(_Host, _Opts) ->
    {ok, [{commands, [#ejabberd_commands{
                         name = kick_user_sessions, tags = [session],
                         desc = "Kick every session of a user, with a reason",
                         module = ?MODULE, function = kick_user_sessions,
                         args = [{user, binary}, {host, binary}, {reason, binary}],
                         args_example = [<<"1234">>, <<"chat.skillz.com">>, <<"muted">>],
                         args_desc = ["User name", "Server name", "Stream error text (quarantine status)"],
                         result = {res, rescode}},
                      #ejabberd_commands{
                         name = start_room, tags = [muc_room],
                         desc = "Create a game room with a title, or set the title if it has none",
                         module = ?MODULE, function = start_room,
                         args = [{name, binary}, {title, binary}],
                         args_example = [<<"19369">>, <<"Skillz Chat">>],
                         args_desc = ["Room name", "Room title"],
                         result = {res, rescode}},
                      #ejabberd_commands{
                         name = get_room_summary, tags = [muc_room],
                         desc = "Get room summary: last x messages after a message id",
                         module = ?MODULE, function = get_room_summary,
                         args = [{room, binary}, {limit, binary}, {last_message_id, binary}],
                         args_example = [<<"81">>, <<"10">>, <<"abcd-1234">>],
                         args_desc = ["Room", "Limit", "Last Message Id"],
                         result = {messages, {list, {message, {tuple, [{id, string}, {from, string}, {body, string},
                                                                      {user_role, integer}, {avatar_url, string}]}}}}}]}]}.

stop(_Host) -> ok.

depends(_Host, _Opts) -> [{mod_http_api, soft}].

mod_opt_type(api_version) -> econf:non_neg_int().

mod_options(_Host) -> [{api_version, 2}].

mod_doc() -> #{desc => ["Fork-compatible admin HTTP API for Skillz services."]}.

kick_user_sessions(User, Host, Reason) ->
    [ejabberd_sm:route(jid:make(User, Host, R), {exit, Reason})
     || R <- ejabberd_sm:get_user_resources(User, Host)],
    ok.

%% shortcut: the first vhost is the external one (chart order); make it an option if that changes.
start_room(Name, Title) ->
    Host = hd(ejabberd_option:hosts()),
    Service = hd(gen_mod:get_module_opt_hosts(Host, mod_muc)),
    case mod_muc_admin:get_room_options(Name, Service) of
        [] -> mod_muc_admin:create_room_with_opts(Name, Service, Host, [{<<"title">>, Title}]);
        Opts ->
            case proplists:get_value(<<"title">>, Opts) of
                <<>> -> mod_muc_admin:change_room_option(Name, Service, <<"title">>, Title);
                _ -> ok
            end
    end.

get_room_summary(Room, LimitIn, LastId) ->
    Limit = case catch binary_to_integer(LimitIn) of N when is_integer(N), N >= 0 -> min(N, 50); _ -> 5 end,
    Host = hd(ejabberd_option:hosts()),
    case mod_muc_admin:get_room_pid(Room, hd(gen_mod:get_module_opt_hosts(Host, mod_muc))) of
        Pid when is_pid(Pid) ->
            {ok, #state{history = #lqueue{queue = Q}}} = mod_muc_room:get_state(Pid),
            Newest = lists:reverse([Msg || {_Nick, Msg, _, _, _} <- p1_queue:to_list(Q)]),
            [summary_item(M) || M <- after_id(Newest, LastId, Limit)];
        _ ->
            []
    end.

%% Newest first: stop at last_message_id; if it is the newest message, return just that one.
after_id(_Msgs, _LastId, 0) -> [];
after_id([#message{id = Id} = M | _], Id, _Limit) -> [M];
after_id(Msgs, LastId, Limit) ->
    lists:reverse(lists:takewhile(fun(#message{id = Id}) -> Id /= LastId end, lists:sublist(Msgs, Limit))).

summary_item(#message{id = Id, from = #jid{user = User, server = Server}, body = Body, sub_els = Els}) ->
    Sdk = case [E || #xmlel{name = <<"skillz_sdk">>} = E <- Els] of [S | _] -> S; [] -> #xmlel{} end,
    Field = fun(Name, Default) -> case fxml:get_subtag(Sdk, Name) of false -> Default; El -> fxml:get_tag_cdata(El) end end,
    Role = case catch binary_to_integer(Field(<<"user_role">>, <<"0">>)) of R when is_integer(R) -> R; _ -> 0 end,
    Username = case Field(<<"username">>, undefined) of
                   undefined ->
                       case catch mod_vcard:get_vcard(User, Server) of
                           [VCard | _] -> case fxml:get_path_s(VCard, [{elem, <<"NICKNAME">>}, cdata]) of <<>> -> User; N -> N end;
                           _ -> User
                       end;
                   U -> U
               end,
    {Id, Username, xmpp:get_text(Body), Role, Field(<<"avatar_url">>, <<>>)}.

process([<<"get_room_summary">>], #request{method = Method, q = Q, data = Data}) ->
    Args = case Method of
               'POST' -> [{K, V} || {K, V} <- maps:to_list(misc:json_decode(Data)), is_binary(V)];
               _ -> Q
           end,
    case proplists:get_value(<<"room">>, Args) of
        undefined ->
            {400, [], <<"\"Request have missing argument: room\"">>};
        Room ->
            Summary = get_room_summary(Room, proplists:get_value(<<"limit">>, Args, <<>>),
                                       proplists:get_value(<<"last_message_id">>, Args, <<>>)),
            {200, [{<<"Content-Type">>, <<"application/json">>}],
             misc:json_encode([#{id => I, from => F, body => B, user_role => R, avatar_url => A} || {I, F, B, R, A} <- Summary])}
    end;
process([<<"kick_session">>], Request) ->
    process([<<"kick_user_sessions">>], Request);
process(Path, #request{path = Full} = Request) ->
    Version = gen_mod:get_module_opt(hd(ejabberd_option:hosts()), ?MODULE, api_version),
    mod_http_api:process(Path, Request#request{path = Full ++ [<<"v", (integer_to_binary(Version))/binary>>]}).
