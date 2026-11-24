%%%----------------------------------------------------------------------
%%% Command kick_user_sessions(user, host, reason): ends every session of the user
%%% with a <conflict/> stream error whose text is the reason. Shipped native clients
%%% read that text as the quarantine status, so stock kick_user ("has been kicked")
%%% cannot replace it.
%%%
%%% Also serves the fork's 3-argument POST /api/kick_session when mounted on the
%%% admin listener ahead of mod_http_api:
%%%   request_handlers:
%%%     /api/kick_session: mod_kick_sessions
%%%     /api: mod_http_api
%%%----------------------------------------------------------------------
-module(mod_kick_sessions).
-behaviour(gen_mod).

-export([start/2, stop/1, depends/2, mod_options/1, mod_doc/0,
         process/2, kick_user_sessions/3]).

-include("ejabberd_commands.hrl").

start(_Host, _Opts) ->
    {ok, [{commands, [#ejabberd_commands{
                         name = kick_user_sessions, tags = [session],
                         desc = "Kick every session of a user, with a reason",
                         module = ?MODULE, function = kick_user_sessions,
                         args = [{user, binary}, {host, binary}, {reason, binary}],
                         args_example = [<<"1234">>, <<"chat.skillz.com">>, <<"muted">>],
                         args_desc = ["User name", "Server name", "Stream error text (quarantine status)"],
                         result = {res, rescode}}]}]}.

stop(_Host) -> ok.

depends(_Host, _Opts) -> [{mod_http_api, soft}].

mod_options(_Host) -> [].

mod_doc() -> #{desc => ["Fork-compatible kick_session for Skillz callers."]}.

kick_user_sessions(User, Host, Reason) ->
    [ejabberd_sm:route(jid:make(User, Host, R), {exit, Reason})
     || R <- ejabberd_sm:get_user_resources(User, Host)],
    ok.

process(_LocalPath, Request) ->
    mod_http_api:process([<<"kick_user_sessions">>], Request).
