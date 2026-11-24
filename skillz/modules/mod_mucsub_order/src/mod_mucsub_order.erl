%%%----------------------------------------------------------------------
%%% Answers the MUC/Sub <subscriptions/> query with rooms ordered by their latest
%%% archived message (newest first), then by subscription time, as the fork did.
%%% Shipped native clients show that list as the room list in this order.
%%% Takes over mod_muc's NS_MUCSUB handler on each MUC service host (start after mod_muc).
%%%----------------------------------------------------------------------
-module(mod_mucsub_order).
-behaviour(gen_mod).

-export([start/2, stop/1, depends/2, mod_options/1, mod_doc/0, process_mucsub/1]).

-include_lib("xmpp/include/xmpp.hrl").
-include("ejabberd_sql_pt.hrl").
-include("translate.hrl").

start(Host, _Opts) ->
    [gen_iq_handler:add_iq_handler(ejabberd_local, MucHost, ?NS_MUCSUB, ?MODULE, process_mucsub)
     || MucHost <- gen_mod:get_module_opt_hosts(Host, mod_muc)],
    ok.

stop(Host) ->
    [gen_iq_handler:add_iq_handler(ejabberd_local, MucHost, ?NS_MUCSUB, mod_muc, process_mucsub)
     || MucHost <- gen_mod:get_module_opt_hosts(Host, mod_muc)],
    ok.

depends(_Host, _Opts) -> [{mod_muc, hard}].

mod_options(_Host) -> [].

mod_doc() -> #{desc => ["MUC/Sub subscriptions ordered by recent activity."]}.

process_mucsub(#iq{type = get, from = From, to = #jid{lserver = Host}, lang = Lang,
                   sub_els = [#muc_subscriptions{}]} = IQ) ->
    JidS = jid:encode(jid:remove_resource(From)),
    case ejabberd_sql:sql_query(
           ejabberd_router:host_of_route(Host),
           ?SQL("select @(m.room)s, @(m.nick)s, @(m.nodes)s from muc_room_subscribers m"
                " left join archive a on a.username = concat(m.room, '@', %(Host)s)"
                " where m.jid = %(JidS)s and m.host = %(Host)s"
                " group by m.room, m.nick, m.nodes, m.created_at"
                " order by max(a.timestamp) desc, m.created_at desc")) of
        {selected, Rows} ->
            List = [#muc_subscription{jid = jid:make(Room, Host), nick = Nick,
                                      events = ejabberd_sql:decode_term(Nodes)}
                    || {Room, Nick, Nodes} <- Rows],
            xmpp:make_iq_result(IQ, #muc_subscriptions{list = List});
        _ ->
            xmpp:make_error(IQ, xmpp:err_internal_server_error(?T("Database failure"), Lang))
    end;
process_mucsub(IQ) ->
    mod_muc:process_mucsub(IQ).
