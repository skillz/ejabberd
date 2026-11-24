%%%----------------------------------------------------------------------
%%% auth_method: http. The SASL PLAIN password (the Skillz chat JWT) is checked by
%%% GET <host><path_prefix>check_password?user=&server=&pass= and accounts by
%%% GET .../user_exists; a 200 response with body "true" means yes.
%%% Options come from the top-level auth_opts (declared by ejabberd core):
%%%   host, path_prefix, connection_pool_size, basic_auth (connection_opts is ignored).
%%%----------------------------------------------------------------------
-module(ejabberd_auth_http).
-behaviour(ejabberd_auth).

-export([start/1, stop/1, reload/1, plain_password_required/1, store_type/1,
         check_password/4, user_exists/2]).

-include("logger.hrl").

-define(TIMEOUT, 5000).

start(Host) ->
    case inets:start(httpc, [{profile, profile(Host)}]) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    reload(Host).

stop(Host) ->
    inets:stop(httpc, profile(Host)).

reload(Host) ->
    Size = proplists:get_value(connection_pool_size, opts(Host), 10),
    httpc:set_options([{max_sessions, Size}, {keep_alive_timeout, 60000}], profile(Host)).

plain_password_required(_) -> true.

store_type(_) -> external.

check_password(User, AuthzId, _Server, _Password) when AuthzId /= <<>>, AuthzId /= User ->
    {nocache, false};
check_password(_User, _AuthzId, _Server, <<>>) ->
    {nocache, false};
check_password(User, _AuthzId, Server, Password) ->
    case request(Server, <<"check_password">>, User, Password) of
        {ok, Result} -> {cache, Result};
        error -> {nocache, false}
    end.

user_exists(User, Server) ->
    case request(Server, <<"user_exists">>, User, <<>>) of
        {ok, Result} -> {cache, Result};
        error -> {nocache, {error, db_failure}}
    end.

request(Server, Method, User, Pass) ->
    Opts = opts(Server),
    Query = uri_string:compose_query([{<<"user">>, User}, {<<"server">>, Server}, {<<"pass">>, Pass}]),
    Url = binary_to_list(<<(proplists:get_value(host, Opts))/binary,
                           (proplists:get_value(path_prefix, Opts, <<"/">>))/binary,
                           Method/binary, "?", Query/binary>>),
    Headers = case proplists:get_value(basic_auth, Opts) of
                  undefined -> [];
                  Basic -> [{"authorization", "Basic " ++ base64:encode_to_string(Basic)}]
              end,
    case httpc:request(get, {Url, Headers}, [{timeout, ?TIMEOUT}, {connect_timeout, ?TIMEOUT}],
                       [{body_format, binary}], profile(Server)) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, string:trim(Body) == <<"true">>};
        {ok, {{_, Code, _}, _, _}} when Code >= 400, Code < 500 ->
            {ok, false};
        {ok, {{_, Code, _}, _, _}} ->
            ?WARNING_MSG("auth http ~ts for ~ts@~ts: HTTP ~B", [Method, User, Server, Code]),
            error;
        {error, Reason} ->
            ?WARNING_MSG("auth http ~ts for ~ts@~ts: ~p", [Method, User, Server, Reason]),
            error
    end.

opts(Host) ->
    ejabberd_option:auth_opts(Host).

profile(Host) ->
    binary_to_atom(<<"ejabberd_auth_http_", Host/binary>>).
