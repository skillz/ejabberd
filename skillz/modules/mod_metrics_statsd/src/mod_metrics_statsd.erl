%%%----------------------------------------------------------------------
%%% mod_metrics' hook counters in statsd format (<host>/<node>.<event>:1|c), plus
%%% the gauges open_tcp_ports, erlang_processes and connected_users on each new
%%% session, as the fork's mod_metrics sent them. The chart's statsd-exporter sidecar
%%% maps them to chat_<event> for Datadog's openmetrics check (chat*).
%%%----------------------------------------------------------------------
-module(mod_metrics_statsd).
-behaviour(gen_mod).

-export([start/2, stop/1, depends/2, mod_options/1, mod_opt_type/1, mod_doc/0,
         offline_message_hook/1, sm_register_connection_hook/3, sm_remove_connection_hook/3,
         user_send_packet/1, user_receive_packet/1, s2s_send_packet/1, s2s_receive_packet/1,
         remove_user/2, register_user/2]).

-include_lib("xmpp/include/xmpp.hrl").

-define(SOCKET_NAME, mod_metrics_statsd_socket).

start(_Host, _Opts) ->
    {ok, [{hook, H, H, 20} || H <- [offline_message_hook, sm_register_connection_hook, sm_remove_connection_hook,
                                   user_send_packet, user_receive_packet, s2s_send_packet, s2s_receive_packet,
                                   remove_user, register_user]]}.

stop(_Host) -> ok.

depends(_Host, _Opts) -> [].

mod_opt_type(ip) -> econf:ipv4();
mod_opt_type(port) -> econf:port().

mod_options(_Host) -> [{ip, {127, 0, 0, 1}}, {port, 11111}].

mod_doc() -> #{desc => ["mod_metrics events and gauges in statsd format."]}.

offline_message_hook({_, #message{to = #jid{lserver = S}}} = Acc) -> push(S, offline_message), Acc.

sm_register_connection_hook(_SID, #jid{lserver = S}, _Info) ->
    push(S, {open_tcp_ports, erlang:system_info(port_count)}),
    push(S, {erlang_processes, erlang:system_info(process_count)}),
    push(S, {connected_users, ejabberd_sm:connected_users_number()}),
    push(S, sm_register_connection).

sm_remove_connection_hook(_SID, #jid{lserver = S}, _Info) -> push(S, sm_remove_connection).

user_send_packet({_, #{jid := #jid{lserver = S}}} = Acc) -> push(S, user_send_packet), Acc;
user_send_packet(Acc) -> Acc.

user_receive_packet({_, #{jid := #jid{lserver = S}}} = Acc) -> push(S, user_receive_packet), Acc;
user_receive_packet(Acc) -> Acc.

s2s_send_packet(Pkt) -> push((xmpp:get_from(Pkt))#jid.lserver, s2s_send_packet), Pkt.

s2s_receive_packet({Pkt, _} = Acc) ->
    push(ejabberd_router:host_of_route((xmpp:get_to(Pkt))#jid.lserver), s2s_receive_packet), Acc.

remove_user(_User, Server) -> push(jid:nameprep(Server), remove_user).

register_user(_User, Server) -> push(jid:nameprep(Server), register_user).

push(Host, Probe) ->
    [_, FQDN] = binary:split(atom_to_binary(node()), <<"@">>),
    [Node | _] = binary:split(FQDN, <<".">>),
    Line = case Probe of
               {Key, Val} -> [Host, $/, Node, $., atom_to_binary(Key), $:, integer_to_binary(Val), "|g"];
               Key -> [Host, $/, Node, $., atom_to_binary(Key), ":1|c"]
           end,
    gen_udp:send(socket(), gen_mod:get_module_opt(Host, ?MODULE, ip), gen_mod:get_module_opt(Host, ?MODULE, port), Line).

socket() ->
    case whereis(?SOCKET_NAME) of
        undefined ->
            {ok, S} = gen_udp:open(0),
            try register(?SOCKET_NAME, S), S
            catch error:badarg -> gen_udp:close(S), socket()
            end;
        S ->
            S
    end.
