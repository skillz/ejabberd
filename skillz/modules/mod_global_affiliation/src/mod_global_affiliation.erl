%%%----------------------------------------------------------------------
%%% Global mute/ban for game rooms (MUC rooms whose name has no "-"), kept in the
%%% fork's user_affiliation table so existing rows, mod_pottymouth and the
%%% set_user_affiliation callers keep working.
%%%   muted   -> may join and read; any groupchat it sends is refused (forbidden)
%%%   outcast -> may not join (forbidden presence error)
%%%   none    -> clears the row
%%% Enforced on the sender's c2s (user_send_packet), not inside mod_muc_room, so a
%%% change applies to the user's next stanza with no rejoin.
%%%----------------------------------------------------------------------
-module(mod_global_affiliation).
-behaviour(gen_mod).

-export([start/2, stop/1, depends/2, mod_options/1, mod_opt_type/1, mod_doc/0,
         user_send_packet/1, set_user_affiliation/3, get_user_affiliation/2]).

-include_lib("xmpp/include/xmpp.hrl").
-include("ejabberd_commands.hrl").
-include("ejabberd_sql_pt.hrl").
-include("logger.hrl").
-include("translate.hrl").

-define(CACHE, global_affiliation_cache).

start(Host, Opts) ->
    ets_cache:new(?CACHE, [{max_size, gen_mod:get_opt(cache_size, Opts)},
                           {life_time, gen_mod:get_opt(cache_life_time, Opts)}]),
    create_table(Host),
    {ok, [{hook, user_send_packet, user_send_packet, 40},
          {commands, commands()}]}.

stop(_Host) -> ok.

depends(_Host, _Opts) -> [{mod_muc, hard}].

mod_opt_type(cache_size) -> econf:pos_int(infinity);
mod_opt_type(cache_life_time) -> econf:timeout(second, infinity).

mod_options(_Host) -> [{cache_size, 100000}, {cache_life_time, timer:minutes(10)}].

mod_doc() -> #{desc => ["Global mute/ban for Skillz game rooms."]}.

commands() ->
    [#ejabberd_commands{name = set_user_affiliation, tags = [muc_room],
                        desc = "Set a user's affiliation in every game room (muted, outcast or none)",
                        module = ?MODULE, function = set_user_affiliation,
                        args = [{host, binary}, {user, binary}, {affiliation, binary}],
                        args_example = [<<"chat.skillz.com">>, <<"1234">>, <<"muted">>],
                        args_desc = ["Server host", "User name", "muted, outcast or none"],
                        result = {res, rescode}},
     #ejabberd_commands{name = get_user_affiliation, tags = [muc_room],
                        desc = "Get a user's game-room affiliation",
                        module = ?MODULE, function = get_user_affiliation,
                        args = [{host, binary}, {user, binary}],
                        args_example = [<<"chat.skillz.com">>, <<"1234">>],
                        args_desc = ["Server host", "User name"],
                        result = {affiliation, string}}].

%%% Enforcement

user_send_packet({#message{type = groupchat} = Pkt, State} = Acc) ->
    check(Pkt, State, Acc, [muted, outcast]);
user_send_packet({#presence{type = available} = Pkt, State} = Acc) ->
    check(Pkt, State, Acc, [outcast]);
user_send_packet(Acc) ->
    Acc.

check(Pkt, #{lserver := LServer, jid := #jid{luser = LUser}} = State, Acc, Refused) ->
    case is_game_room(xmpp:get_to(Pkt), LServer)
         andalso lists:member(affiliation(LUser, LServer), Refused) of
        true ->
            Lang = xmpp:get_lang(Pkt),
            ejabberd_router:route_error(Pkt, xmpp:err_forbidden(?T("You are not allowed to do this"), Lang)),
            {stop, {drop, State}};
        false ->
            Acc
    end.

is_game_room(#jid{luser = Room, lserver = Service}, LServer) when Room /= <<>> ->
    binary:match(Room, <<"-">>) == nomatch
        andalso lists:member(Service, gen_mod:get_module_opt_hosts(LServer, mod_muc));
is_game_room(_To, _LServer) ->
    false.

%%% Storage (the fork's MySQL table, read through a cluster-wide cache)

affiliation(LUser, LServer) ->
    case ets_cache:lookup(?CACHE, {LUser, LServer}, fun() -> {ok, select(LUser, LServer)} end) of
        {ok, Aff} -> Aff;
        error -> none
    end.

get_user_affiliation(Host, User) ->
    atom_to_list(select(jid:nodeprep(User), jid:nameprep(Host))).

set_user_affiliation(Host, User, Aff) when Aff == <<"muted">>; Aff == <<"outcast">>; Aff == <<"none">> ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Host),
    ejabberd_sql:sql_query(
      LServer,
      ?SQL("update user_affiliation set version = version + 1, enabled = 0, last_updated = now() "
           "where nick = %(LUser)s and enabled = 1")),
    case Aff of
        <<"none">> -> ok;
        _ ->
            ejabberd_sql:sql_query(
              LServer,
              ?SQL("insert into user_affiliation (version, enabled, nick, affiliation, date_created, last_updated) "
                   "values (0, 1, %(LUser)s, %(Aff)s, now(), now())"))
    end,
    ets_cache:delete(?CACHE, {LUser, LServer}, ejabberd_cluster:get_nodes()),
    ok;
set_user_affiliation(_Host, _User, _Aff) ->
    {error, "affiliation must be muted, outcast or none"}.

select(LUser, LServer) ->
    case ejabberd_sql:sql_query(
           LServer,
           ?SQL("select @(affiliation)s from user_affiliation where nick = %(LUser)s and enabled = 1")) of
        {selected, [{<<"muted">>} | _]} -> muted;
        {selected, [{<<"outcast">>} | _]} -> outcast;
        _ -> none
    end.

%% shortcut: MySQL DDL copied from the fork's 2019-07-26 migration; prod already has the table.
create_table(Host) ->
    ejabberd_sql:sql_query(
      Host,
      [<<"CREATE TABLE IF NOT EXISTS user_affiliation ("
         " id BIGINT NOT NULL AUTO_INCREMENT, version BIGINT NOT NULL, enabled BIT DEFAULT 1 NOT NULL,"
         " nick VARCHAR(127) NOT NULL COLLATE utf8mb4_unicode_ci,"
         " affiliation VARCHAR(30) NOT NULL COLLATE utf8mb4_unicode_ci,"
         " date_created DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,"
         " last_updated DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,"
         " PRIMARY KEY(id), INDEX user_affiliation_enabled_nick_idx (enabled, nick),"
         " INDEX user_affiliation_date_created_idx (date_created))"
         " ENGINE = InnoDB DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci">>]).
