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
%%%----------------------------------------------------------------------
-module(mod_admin_api_compat).
-behaviour(gen_mod).

-export([start/2, stop/1, depends/2, mod_options/1, mod_opt_type/1, mod_doc/0,
         process/2, kick_user_sessions/3, start_room/2]).

-include("ejabberd_commands.hrl").
-include("ejabberd_http.hrl").

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
                         result = {res, rescode}}]}]}.

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

process([<<"kick_session">>], Request) ->
    process([<<"kick_user_sessions">>], Request);
process(Path, #request{path = Full} = Request) ->
    Version = gen_mod:get_module_opt(hd(ejabberd_option:hosts()), ?MODULE, api_version),
    mod_http_api:process(Path, Request#request{path = Full ++ [<<"v", (integer_to_binary(Version))/binary>>]}).
