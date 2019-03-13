%%%-------------------------------------------------------------------
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created :  4 Dec 2018 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2018   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------
-module(mod_mix_pam).
-behaviour(gen_mod).
-protocol({xep, 405, '0.2.1'}).

%% gen_mod callbacks
-export([start/2, stop/1, reload/3, depends/2, mod_opt_type/1, mod_options/1]).
%% Hooks and handlers
-export([bounce_sm_packet/1,
	 disco_sm_features/5,
	 remove_user/2,
	 process_iq/1]).

-include("xmpp.hrl").
-include("logger.hrl").

-define(MIX_PAM_CACHE, mix_pam_cache).

-callback init(binary(), gen_mod:opts()) -> ok | {error, db_failure}.
-callback add_channel(jid(), jid(), binary()) -> ok | {error, db_failure}.
-callback del_channel(jid(), jid()) -> ok | {error, db_failure}.
-callback get_channel(jid(), jid()) -> {ok, binary()} | {error, notfound | db_failure}.
-callback get_channels(jid()) -> {ok, [{jid(), binary()}]} | {error, db_failure}.
-callback del_channels(jid()) -> ok | {error, db_failure}.
-callback use_cache(binary()) -> boolean().
-callback cache_nodes(binary()) -> [node()].

-optional_callbacks([use_cache/1, cache_nodes/1]).

%%%===================================================================
%%% API
%%%===================================================================
start(Host, Opts) ->
    Mod = gen_mod:db_mod(Host, Opts, ?MODULE),
    case Mod:init(Host, Opts) of
	ok ->
	    init_cache(Mod, Host, Opts),
	    ejabberd_hooks:add(bounce_sm_packet, Host, ?MODULE, bounce_sm_packet, 50),
	    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE, disco_sm_features, 50),
	    ejabberd_hooks:add(remove_user, Host, ?MODULE, remove_user, 50),
	    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_MIX_PAM_0,
					  ?MODULE, process_iq);
	Err ->
	    Err
    end.

stop(Host) ->
    ejabberd_hooks:delete(bounce_sm_packet, Host, ?MODULE, bounce_sm_packet, 50),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE, disco_sm_features, 50),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE, remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_MIX_PAM_0).

reload(Host, NewOpts, OldOpts) ->
    NewMod = gen_mod:db_mod(Host, NewOpts, ?MODULE),
    OldMod = gen_mod:db_mod(Host, OldOpts, ?MODULE),
    if NewMod /= OldMod ->
            NewMod:init(Host, NewOpts);
       true ->
            ok
    end,
    init_cache(NewMod, Host, NewOpts).

depends(_Host, _Opts) ->
    [].

mod_opt_type(db_type) -> fun(T) -> ejabberd_config:v_db(?MODULE, T) end;
mod_opt_type(O) when O == cache_life_time; O == cache_size ->
    fun (I) when is_integer(I), I > 0 -> I;
        (infinity) -> infinity
    end;
mod_opt_type(O) when O == use_cache; O == cache_missed ->
    fun (B) when is_boolean(B) -> B end.

mod_options(Host) ->
    [{db_type, ejabberd_config:default_db(Host, ?MODULE)},
     {use_cache, ejabberd_config:use_cache(Host)},
     {cache_size, ejabberd_config:cache_size(Host)},
     {cache_missed, ejabberd_config:cache_missed(Host)},
     {cache_life_time, ejabberd_config:cache_life_time(Host)}].

-spec bounce_sm_packet({term(), stanza()}) -> {term(), stanza()}.
bounce_sm_packet({_, #message{to = #jid{lresource = <<>>} = To,
			      from = From,
			      type = groupchat} = Msg} = Acc) ->
    case xmpp:has_subtag(Msg, #mix{}) of
	true ->
	    {LUser, LServer, _} = jid:tolower(To),
	    case get_channel(To, From) of
		{ok, _} ->
		    lists:foreach(
		      fun(R) ->
			      To1 = jid:replace_resource(To, R),
			      ejabberd_router:route(xmpp:set_to(Msg, To1))
		      end, ejabberd_sm:get_user_resources(LUser, LServer)),
		    {pass, Msg};
		_ ->
		    Acc
	    end;
	false ->
	    Acc
    end;
bounce_sm_packet(Acc) ->
    Acc.

-spec disco_sm_features({error, stanza_error()} | empty | {result, [binary()]},
			jid(), jid(), binary(), binary()) ->
			       {error, stanza_error()} | empty | {result, [binary()]}.
disco_sm_features({error, _Error} = Acc, _From, _To, _Node, _Lang) ->
    Acc;
disco_sm_features(Acc, _From, _To, <<"">>, _Lang) ->
    {result, [?NS_MIX_PAM_0 |
	      case Acc of
		  {result, Features} -> Features;
		  empty -> []
	      end]};
disco_sm_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

-spec process_iq(iq()) -> iq() | ignore.
process_iq(#iq{from = #jid{luser = U1, lserver = S1},
	       to = #jid{luser = U2, lserver = S2}} = IQ)
  when {U1, S1} /= {U2, S2} ->
    xmpp:make_error(IQ, forbidden_query_error(IQ));
process_iq(#iq{type = set,
	       sub_els = [#mix_client_join{} = Join]} = IQ) ->
    case Join#mix_client_join.channel of
	undefined ->
	    xmpp:make_error(IQ, missing_channel_error(IQ));
	_ ->
	    process_join(IQ)
    end;
process_iq(#iq{type = set,
	       sub_els = [#mix_client_leave{} = Leave]} = IQ) ->
    case Leave#mix_client_leave.channel of
	undefined ->
	    xmpp:make_error(IQ, missing_channel_error(IQ));
	_ ->
	    process_leave(IQ)
    end;
process_iq(IQ) ->
    xmpp:make_error(IQ, unsupported_query_error(IQ)).

-spec remove_user(binary(), binary()) -> ok | {error, db_failure}.
remove_user(LUser, LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    JID = jid:make(LUser, LServer),
    Chans = case Mod:get_channels(JID) of
		{ok, Channels} ->
		    lists:map(
		      fun({Channel, _}) ->
			      ejabberd_router:route(
				#iq{from = JID,
				    to = Channel,
				    id = p1_rand:get_string(),
				    type = set,
				    sub_els = [#mix_leave{}]}),
			      Channel
		      end, Channels);
		_ ->
		    []
	    end,
    Mod:del_channels(jid:make(LUser, LServer)),
    lists:foreach(
      fun(Chan) ->
	      delete_cache(Mod, JID, Chan)
      end, Chans).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec process_join(iq()) -> ignore.
process_join(#iq{from = From,
		 sub_els = [#mix_client_join{channel = Channel,
					     join = Join}]} = IQ) ->
    ejabberd_router:route_iq(
      #iq{from = jid:remove_resource(From),
	  to = Channel, type = set, sub_els = [Join]},
      fun(ResIQ) -> process_join_result(ResIQ, IQ) end),
    ignore.

-spec process_leave(iq()) -> iq() | error.
process_leave(#iq{from = From,
		  sub_els = [#mix_client_leave{channel = Channel,
					       leave = Leave}]} = IQ) ->
    case del_channel(From, Channel) of
	ok ->
	    ejabberd_router:route_iq(
	      #iq{from = jid:remove_resource(From),
		  to = Channel, type = set, sub_els = [Leave]},
	      fun(ResIQ) -> process_leave_result(ResIQ, IQ) end),
	    ignore;
	{error, db_failure} ->
	    xmpp:make_error(IQ, db_error(IQ))
    end.

-spec process_join_result(iq(), iq()) -> ok.
process_join_result(#iq{from = Channel,
			type = result, sub_els = [#mix_join{id = ID} = Join]},
		    #iq{to = To} = IQ) ->
    case add_channel(To, Channel, ID) of
	ok ->
	    ChanID = make_channel_id(Channel, ID),
	    Join1 = Join#mix_join{id = <<"">>, jid = ChanID},
	    ResIQ = xmpp:make_iq_result(IQ, #mix_client_join{join = Join1}),
	    ejabberd_router:route(ResIQ);
	{error, db_failure} ->
	    ejabberd_router:route_error(IQ, db_error(IQ))
    end;
process_join_result(Err, IQ) ->
    process_iq_error(Err, IQ).

-spec process_leave_result(iq(), iq()) -> ok.
process_leave_result(#iq{type = result, sub_els = [#mix_leave{} = Leave]}, IQ) ->
    ResIQ = xmpp:make_iq_result(IQ, #mix_client_leave{leave = Leave}),
    ejabberd_router:route(ResIQ);
process_leave_result(Err, IQ) ->
    process_iq_error(Err, IQ).

-spec process_iq_error(iq(), iq()) -> ok.
process_iq_error(#iq{type = error} = ErrIQ, #iq{sub_els = [El]} = IQ) ->
    case xmpp:get_error(ErrIQ) of
	undefined ->
	    %% Not sure if this stuff is correct because
	    %% RFC6120 section 8.3.1 bullet 4 states that
	    %% an error stanza MUST contain an <error/> child element
	    IQ1 = xmpp:make_iq_result(IQ, El),
	    ejabberd_router:route(IQ1#iq{type = error});
	Err ->
	    ejabberd_router:route_error(IQ, Err)
    end;
process_iq_error(timeout, IQ) ->
    Txt = <<"Request has timed out">>,
    Err = xmpp:err_recipient_unavailable(Txt, IQ#iq.lang),
    ejabberd_router:route_error(IQ, Err).

-spec make_channel_id(jid(), binary()) -> jid().
make_channel_id(JID, ID) ->
    {U, S, R} = jid:split(JID),
    jid:make(<<ID/binary, $#, U/binary>>, S, R).

%%%===================================================================
%%% Error generators
%%%===================================================================
-spec missing_channel_error(stanza()) -> stanza_error().
missing_channel_error(Pkt) ->
    Txt = <<"Attribute 'channel' is required for this request">>,
    xmpp:err_bad_request(Txt, xmpp:get_lang(Pkt)).

-spec forbidden_query_error(stanza()) -> stanza_error().
forbidden_query_error(Pkt) ->
    Txt = <<"Query to another users is forbidden">>,
    xmpp:err_forbidden(Txt, xmpp:get_lang(Pkt)).

-spec unsupported_query_error(stanza()) -> stanza_error().
unsupported_query_error(Pkt) ->
    Txt = <<"No module is handling this query">>,
    xmpp:err_service_unavailable(Txt, xmpp:get_lang(Pkt)).

-spec db_error(stanza()) -> stanza_error().
db_error(Pkt) ->
    Txt = <<"Database failure">>,
    xmpp:err_internal_server_error(Txt, xmpp:get_lang(Pkt)).

%%%===================================================================
%%% Database queries
%%%===================================================================
get_channel(JID, Channel) ->
    {LUser, LServer, _} = jid:tolower(JID),
    {Chan, Service, _} = jid:tolower(Channel),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case use_cache(Mod, LServer) of
	false -> Mod:get_channel(JID, Channel);
	true ->
	    case ets_cache:lookup(
		   ?MIX_PAM_CACHE, {LUser, LServer, Chan, Service},
		   fun() -> Mod:get_channel(JID, Channel) end) of
		error -> {error, notfound};
		Ret -> Ret
	    end
    end.

add_channel(JID, Channel, ID) ->
    Mod = gen_mod:db_mod(JID#jid.lserver, ?MODULE),
    case Mod:add_channel(JID, Channel, ID) of
	ok -> delete_cache(Mod, JID, Channel);
	Err -> Err
    end.

del_channel(JID, Channel) ->
    Mod = gen_mod:db_mod(JID#jid.lserver, ?MODULE),
    case Mod:del_channel(JID, Channel) of
	ok -> delete_cache(Mod, JID, Channel);
	Err -> Err
    end.

%%%===================================================================
%%% Cache management
%%%===================================================================
-spec init_cache(module(), binary(), gen_mod:opts()) -> ok.
init_cache(Mod, Host, Opts) ->
    case use_cache(Mod, Host) of
	true ->
	    CacheOpts = cache_opts(Opts),
	    ets_cache:new(?MIX_PAM_CACHE, CacheOpts);
	false ->
	    ets_cache:delete(?MIX_PAM_CACHE)
    end.

-spec cache_opts(gen_mod:opts()) -> [proplists:property()].
cache_opts(Opts) ->
    MaxSize = gen_mod:get_opt(cache_size, Opts),
    CacheMissed = gen_mod:get_opt(cache_missed, Opts),
    LifeTime = case gen_mod:get_opt(cache_life_time, Opts) of
		   infinity -> infinity;
		   I -> timer:seconds(I)
	       end,
    [{max_size, MaxSize}, {cache_missed, CacheMissed}, {life_time, LifeTime}].

-spec use_cache(module(), binary()) -> boolean().
use_cache(Mod, Host) ->
    case erlang:function_exported(Mod, use_cache, 1) of
	true -> Mod:use_cache(Host);
	false -> gen_mod:get_module_opt(Host, ?MODULE, use_cache)
    end.

-spec cache_nodes(module(), binary()) -> [node()].
cache_nodes(Mod, Host) ->
    case erlang:function_exported(Mod, cache_nodes, 1) of
	true -> Mod:cache_nodes(Host);
	false -> ejabberd_cluster:get_nodes()
    end.

-spec delete_cache(module(), jid(), jid()) -> ok.
delete_cache(Mod, JID, Channel) ->
    {LUser, LServer, _} = jid:tolower(JID),
    {Chan, Service, _} = jid:tolower(Channel),
    case use_cache(Mod, LServer) of
	true ->
	    ets_cache:delete(?MIX_PAM_CACHE,
			     {LUser, LServer, Chan, Service},
			     cache_nodes(Mod, LServer));
	false ->
	    ok
    end.
