%%%-------------------------------------------------------------------
%%% Author  : Devin Sills <dsills@skillz.com>
%%%----------------------------------------------------------------------

-module(skillz_tests).

%% API
-compile(export_all).
-import(suite, [disconnect/1, muc_room_jid/1, my_jid/1, server_jid/1, send_recv/2, recv_message/1,
    send/2, recv/1, wait_for_master/1, wait_for_slave/1, close_socket/1, set_opt/3]).

-include("suite.hrl").

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
        single_test(dynamic_batch)
    ]}
.

get_value_by_tag(Config) ->
    <<"my-role">> = skillz_util:get_value_by_tag(
        {xmlel, <<"root">>, [], [{xmlel, <<"foo">>, [], []}, {xmlel, <<"user_role">>, [], [{xmlcdata, <<"my-role">>}]}]},
        <<"user_role">>
    )
.

get_resource_of_jid_in_room(Config) ->
    MyJidStr = jid:to_string(my_jid(Config)),
    MyNick = ?config(nick, Config),
    RoomJid = {jid, Name, Host, _, _, _, _} = muc_room_jid(Config),
    muc_tests:join_new(Config, RoomJid),
    MyNick = skillz_util:get_resource_of_jid_in_room(Host, MyJidStr, Name, "moderator", none),
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
