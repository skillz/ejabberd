%%%----------------------------------------------------------------------
%%% Keeps room subject changes (groupchat with a subject and no body) out of the
%%% MAM archive, as on the 19.02 fork: shipped native clients may read archive rows
%%% from MAM, and the fork crashes when it rebuilds join history from a body-less
%%% row (rollback). Marks the message mam_ignore before mod_mam (priority 50) sees it.
%%%----------------------------------------------------------------------
-module(mod_muc_subject_unarchived).
-behaviour(gen_mod).

-export([start/2, stop/1, depends/2, mod_options/1, mod_doc/0, muc_filter_message/3]).

-include_lib("xmpp/include/xmpp.hrl").

start(_Host, _Opts) ->
    {ok, [{hook, muc_filter_message, muc_filter_message, 40}]}.

stop(_Host) -> ok.

depends(_Host, _Opts) -> [{mod_mam, hard}].

mod_options(_Host) -> [].

mod_doc() -> #{desc => ["Do not archive MUC subject changes."]}.

muc_filter_message(#message{subject = [_ | _], body = []} = Pkt, _State, _Nick) ->
    xmpp:put_meta(Pkt, mam_ignore, true);
muc_filter_message(Acc, _State, _Nick) ->
    Acc.
