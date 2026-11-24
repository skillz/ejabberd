%%%----------------------------------------------------------------------
%%% A vCard get whose request carries <JABBERID>a+b+c</JABBERID> is answered with one
%%% <vCards xmlns="jabber:client"> holding the stored vCard of each listed user that has
%%% one (each tagged username="<user>"); shipped native clients batch vCard fetches this way.
%%% Any other vCard IQ goes to mod_vcard unchanged.
%%%----------------------------------------------------------------------
-module(mod_vcard_multi).
-behaviour(gen_mod).

-export([start/2, stop/1, depends/2, mod_options/1, mod_doc/0, process_sm_iq/1]).

-include_lib("xmpp/include/xmpp.hrl").

%% Takes over mod_vcard's user-JID handler, so this module must start after mod_vcard.
start(Host, _Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_VCARD, ?MODULE, process_sm_iq).

stop(Host) ->
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_VCARD, mod_vcard, process_sm_iq).

depends(_Host, _Opts) -> [{mod_vcard, hard}].

mod_options(_Host) -> [].

mod_doc() -> #{desc => ["Multi-vCard fetch for Skillz native clients."]}.

process_sm_iq(#iq{type = get, sub_els = [#vcard_temp{jabberid = Ids}], to = #jid{lserver = LServer}} = IQ)
  when is_binary(Ids), Ids /= <<>> ->
    Cards = [tag(VCard, User)
             || User <- binary:split(Ids, <<"+">>, [global]),
                LUser <- [jid:nodeprep(User)], is_binary(LUser),
                [VCard] <- [mod_vcard:get_vcard(LUser, LServer)]],
    xmpp:make_iq_result(IQ, #xmlel{name = <<"vCards">>, attrs = [{<<"xmlns">>, <<"jabber:client">>}],
                                   children = Cards});
process_sm_iq(IQ) ->
    mod_vcard:process_sm_iq(IQ).

tag(#xmlel{attrs = Attrs} = VCard, User) ->
    VCard#xmlel{attrs = Attrs ++ [{<<"username">>, User}]}.
