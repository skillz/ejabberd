%%%----------------------------------------------------------------------
%%% With mod_mam `clear_archive_on_room_destroy: false`, mod_mam also refuses to
%%% create a room that still has archived messages, so a game room destroyed by the
%%% delete-unused-rooms cron could never be joined again. This module removes that
%%% refusal: the room is created again on the next join and keeps its archive (MAM).
%%%----------------------------------------------------------------------
-module(mod_muc_keep_archive).
-behaviour(gen_mod).

-export([start/2, stop/1, depends/2, mod_options/1, mod_doc/0]).

start(Host, _Opts) ->
    ejabberd_hooks:delete(check_create_room, Host, mod_mam, check_create_room, 50).

stop(Host) ->
    ejabberd_hooks:add(check_create_room, Host, mod_mam, check_create_room, 50).

depends(_Host, _Opts) -> [{mod_mam, hard}].

mod_options(_Host) -> [].

mod_doc() -> #{desc => ["Keep MUC archives on room destroy and allow the room again."]}.
