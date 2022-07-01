%%%-------------------------------------------------------------------
%%% Author  : Devin Sills <dsills@skillz.com>
%%%----------------------------------------------------------------------

-module(skillz_tests).

%% API
-compile(export_all).
-import(suite, [disconnect/1, muc_room_jid/1, my_jid/1, muc_jid/1, server_jid/1, send_recv/2, recv_message/1, recv_iq/1,
    send/2, recv/1, wait_for_master/1, wait_for_slave/1, close_socket/1, set_opt/3]).

-include("suite.hrl").
-include("jid.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%%%===================================================================
%%% Single user tests
%%%===================================================================
single_cases() ->
    {skillz_single, [sequence], [
        single_test(get_value_by_tag),
        single_test(get_resource_of_jid_in_room),
        single_test(dynamic_batch),
        single_test(get_subscribed_rooms),
        single_test(get_vcards)
    ]}
.

get_value_by_tag(Config) ->
    <<"my-role-1">> = skillz_util:get_value_by_tag(
        {xmlel, <<"root">>, [], [{xmlel, <<"foo">>, [], []}, {xmlel, <<"user_role">>, [], [{xmlcdata, <<"my-role-1">>}]}]},
        <<"user_role">>
    ),

    <<"my-role-2">> = skillz_util:get_value_by_tag(
        [{xmlel, <<"root">>, [], [{xmlel, <<"foo">>, [], []}, {xmlel, <<"user_role">>, [], [{xmlcdata, <<"my-role-2">>}]}]}],
        <<"user_role">>
    ),

    <<"my-role-3">> = skillz_util:get_value_by_tag(
        [foo, bar, {xmlel, <<"root">>, [], [{xmlel, <<"foo">>, [], []}, {xmlel, <<"user_role">>, [], [{xmlcdata, <<"my-role-3">>}]}]}],
        <<"user_role">>
    ),

    <<"my-role-4">> = skillz_util:get_value_by_tag(
        [{xmlel, <<"root">>, [], []}, bar, {xmlel, <<"root">>, [], [{xmlel, <<"foo">>, [], []}, {xmlel, <<"user_role">>, [], [{xmlcdata, <<"my-role-4">>}]}]}],
        <<"user_role">>
    )
.

get_resource_of_jid_in_room(Config) ->
    MyJidStr = jid:to_string(my_jid(Config)),
    MyNick = ?config(nick, Config),
    RoomJid = {jid, Name, Host, _, _, _, _} = muc_room_jid(Config),
    muc_tests:join_new(Config, RoomJid),
    MyNick = skillz_util:get_resource_of_jid_in_room(Host, MyJidStr, Name, none),
    muc_tests:leave(Config),
    disconnect(Config)
.

dynamic_batch(Config) ->
    ok = skillz_util:dynamic_batch(fun(Batch) -> ok end, [1,2,3,4,5,6,7,8,9,10], 1000, 10, 0, 1, 1),

    ets:new(dynamic_batch, [named_table, public]),

    skillz_util:dynamic_batch(
        fun(Batch) ->
            timer:sleep(100),
            case ets:lookup(dynamic_batch, batches) of
                [] -> ets:insert(dynamic_batch, {batches, [Batch]});
                [{batches, AccBatches}] -> ets:insert(dynamic_batch, {batches, AccBatches ++ [Batch]})
            end
        end,
        [1,2,3,4,5,6,7,8,9,10], 1000, 10, 0, 1, 1
    ),
    Batches = ets:tab2list(dynamic_batch),
    %% it should keep expanding (last one is just leftover final batch)
    [{batches, [[1], [2], [3, 4], [5, 6, 7, 8, 9], [10]]}] = Batches,
    ets:delete_all_objects(dynamic_batch),

    skillz_util:dynamic_batch(
        fun(Batch) ->
            timer:sleep(100),
            case ets:lookup(dynamic_batch, batches) of
                [] -> ets:insert(dynamic_batch, {batches, [Batch]});
                [{batches, AccBatches}] -> ets:insert(dynamic_batch, {batches, AccBatches ++ [Batch]})
            end
        end,
        [1,2,3,4,5,6,7,8,9,10], 10, 0, 0, 1, 1
    ),
    Batches2 = ets:tab2list(dynamic_batch),
    %% it should only run batch sizes of 1
    [{batches, [[1], [2], [3], [4], [5], [6], [7], [8], [9], [10]]}] = Batches2,
    ets:delete_all_objects(dynamic_batch),

    skillz_util:dynamic_batch(
        fun(Batch) ->
            case length(Batch) of
                4 -> timer:sleep(50);
                Len when Len > 4 -> timer:sleep(70);
                _ -> timer:sleep(1)
            end,
            case ets:lookup(dynamic_batch, batches) of
                [] -> ets:insert(dynamic_batch, {batches, [Batch]});
                [{batches, AccBatches}] -> ets:insert(dynamic_batch, {batches, AccBatches ++ [Batch]})
            end
        end,
        lists:seq(1, 100), 50, 10, 0, 1, 1
    ),
    [{batches, Batches3}] = ets:tab2list(dynamic_batch),
    %% it should find `4` as the best batch (last one of 2 is just leftover final batch)
    [1,1,2,5,3,6,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,2] = lists:map(fun(Entry) -> length(Entry) end, Batches3),
    ets:delete_all_objects(dynamic_batch),

    ets:delete(dynamic_batch),
    disconnect(Config)
.

get_subscribed_rooms(Config) ->
    subscribe_to_rooms(Config),
    Server = ?config(server, Config),
    #jid{server = Service} = muc_jid(Config),
    MyJid = jid:remove_resource(my_jid(Config)),
    SubscribedRooms = mod_muc_sql:get_subscribed_rooms(Server, Service, MyJid),
    5 = length(SubscribedRooms),
    unsubscribe_from_rooms(Config),
    disconnect(Config)
.

get_vcards(Config) ->
    PrimaryJID = my_jid(Config),
    NoOneJID = jid:decode(<<"doesnotmatter@mysql.localhost">>),
    #iq{type = result, sub_els = SubEls} = send_recv(Config, #iq{type = get, from = PrimaryJID, to = NoOneJID, sub_els = [
        #vcard_temp{jabberid = <<"2247494055+2247494059">>}
    ]}),
    [{xmlel, <<"vCards">>, [], []}] = SubEls
.

%%%===================================================================
%%% Master-slave tests
%%%===================================================================
master_slave_cases() ->
    {skillz_master_slave, [sequence], []}
.

%%%===================================================================
%%% Internal functions
%%%===================================================================
single_test(T) ->
    list_to_atom("skillz_" ++ atom_to_list(T))
.

master_slave_test(T) ->
    {list_to_atom("skillz_" ++ atom_to_list(T)), [parallel],
    [list_to_atom("skillz_" ++ atom_to_list(T) ++ "_master"),
    list_to_atom("skillz_" ++ atom_to_list(T) ++ "_slave")]}
.

subscribe_to_rooms(Config) ->
    MUC = #jid{server = Service} = muc_jid(Config),
    Rooms = lists:sort(
        lists:map(
            fun(I) ->
                RoomName = integer_to_binary(I),
                jid:make(RoomName, Service)
            end, lists:seq(7, 11))), %% 7-11 to not interfere w/ muc_tests' 1-5
    lists:foreach(
        fun(Room) ->
            ok = muc_tests:join_new(Config, Room),
            [104] = muc_tests:set_config(Config, [{allow_subscription, true}], Room),
            [104] = muc_tests:set_config(Config, [{persistentroom, true}], Room),
            [] = muc_tests:subscribe(Config, [], Room)
        end, Rooms),
    send_recv(Config, #iq{type = get, to = MUC, sub_els = [#muc_subscriptions{}]})
.

unsubscribe_from_rooms(Config) ->
    #jid{server = Service} = muc_jid(Config),
    Rooms = lists:sort(
        lists:map(
            fun(I) ->
                RoomName = integer_to_binary(I),
                jid:make(RoomName, Service)
            end, lists:seq(7, 11))), %% 7-11 to not interfere w/ muc_tests' 1-5
    lists:foreach(
        fun(Room) ->
            ok = muc_tests:unsubscribe(Config, Room),
            ok = muc_tests:leave(Config, Room)
        end, Rooms)
.
