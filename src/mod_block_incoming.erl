%%%-------------------------------------------------------------------
%%% File    : mod_block_incoming.erl
%%% Author  : Devin Sills <dsills@skillz.com>
%%% Created : 31 Jan 2022 by Devin Sills <dsills@skillz.com>
%%%
%%%----------------------------------------------------------------------

-module(mod_block_incoming).
-author('dsills@skillz.com').

-behaviour(gen_server).
-behaviour(gen_mod).

%% gen_mod callbacks.
-export([start/2, stop/1, reload/3, depends/2, mod_options/1, mod_opt_type/1]).

%% gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, get_proc_name/1]).

%% ejabberd_commands callbacks.
-export([get_commands_spec/0, add_blocking_user/2, remove_blocking_user/2, get_blocking_users/1, reset_blocking_users/1, clear_blocking_users/1,
  add_blocking_user_all/2, remove_blocking_user_all/2, reset_blocking_users_all/1, clear_blocking_users_all/1, is_blocking_user/2]).

%% hook handlers
-export([filter_subscription/2]).

-include("ejabberd_commands.hrl").
-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").

-define(HOOK_PRIORITY, 25).
-define(COMMAND_TIMEOUT, timer:seconds(2)).

%%%===================================================================
%%% Callbacks and hooks
%%%===================================================================
start(Host, Opts) ->
  ejabberd_commands:register_commands(get_commands_spec()),
  gen_mod:start_child(?MODULE, Host, Opts)
.

init([Host, _Opts]) ->
  ejabberd_hooks:add(roster_in_subscription, Host, ?MODULE, filter_subscription, ?HOOK_PRIORITY),
  {ok, { Host, get_config_users_dict(Host) } }
.

stop(Host) ->
  case gen_mod:is_loaded_elsewhere(Host, ?MODULE) of
    false -> ejabberd_commands:unregister_commands(get_commands_spec());
    true -> ok
  end,
  gen_mod:stop_child(?MODULE, Host)
.

terminate(Reason, { Host, _BlockingUsers }) ->
  ?DEBUG("Terminating mod_block_incoming process for [~p] with reason: [~p]", [Host, Reason]),
  ejabberd_hooks:delete(roster_in_subscription, Host, ?MODULE, filter_subscription, ?HOOK_PRIORITY)
.

reload(_Host, _NewOpts, _OldOpts) ->
  ok
.

handle_call({get_blocking_users}, _From, {_Host, BlockingUsers} = State) ->
  {reply, lists:sort(lists:map(fun({Key, _}) -> Key end, dict:to_list(BlockingUsers))), State}
;

handle_call({is_blocking_user, User}, _From, {_Host, BlockingUsers} = State) ->
  {reply, dict:is_key(User, BlockingUsers), State}
;

handle_call({add_blocking_user, User}, _From, {Host, BlockingUsers}) ->
  {reply, ok, { Host, dict:store(User, User, BlockingUsers) }}
;

handle_call({remove_blocking_user, User}, _From, {Host, BlockingUsers}) ->
  {reply, ok, {Host, dict:erase(User, BlockingUsers)}}
;

handle_call({reset_blocking_users}, _From, {Host, _BlockingUsers}) ->
  {reply, ok, { Host, get_config_users_dict(Host) }}
;

handle_call({clear_blocking_users}, _From, {Host, _BlockingUsers}) ->
  {reply, ok, { Host, dict:new() }}
;

handle_call(Request, From, State) ->
  ?ERROR_MSG("Unexpected handle_call from [~p] for [~p]", [From, Request]),
  {noreply, State}
.

handle_cast(Request, State) ->
  ?ERROR_MSG("Unexpected handle_cast: [~p]", [Request]),
  {noreply, State}
.

handle_info(Info, State) ->
  ?ERROR_MSG("Unexpected handle_info: [~p]", [Info]),
  {noreply, State}
.

code_change(_OldVsn, { Host, _BlockingUsers } = State, _Extra) ->
  ?DEBUG("Updating code for mod_block_incoming process on host [~p]", [Host]),
  {ok, State}
.

filter_subscription(Acc, #presence{from = From, to = To, type = subscribe} = Pres) ->
  case is_blocking(Pres) of
    true ->
      case check_subscription(From, To) of
        false ->
          ?INFO_MSG("Auto denied subscription request from [~p] to [~p]", [From#jid.luser, To#jid.luser]),
          {stop, false};
        true -> Acc
      end;
    false -> Acc
  end
;

filter_subscription(Acc, _) -> Acc.

%%%===================================================================
%%% Internal functions
%%%===================================================================
check_subscription(From, To) ->
  mod_roster:is_subscribed(From, To)
.

is_blocking(Pkt) ->
  To = xmpp:get_to(Pkt),
  From = xmpp:get_from(Pkt),

  IsToAnotherUser = (To#jid.luser /= From#jid.luser orelse To#jid.lserver /= From#jid.lserver),
  HasMessageContent = case Pkt of
      #message{body = [], subject = []} -> false;
      _ -> true
    end,
  IsFromActualUserOrRemoteServer = (From#jid.luser /= <<"">> orelse not ejabberd_router:is_my_host(From#jid.lserver)),
  IsToUserBlockingIncoming = is_blocking_user(To#jid.lserver, To#jid.luser),
  IsFromUserBlockingIncoming = is_blocking_user(From#jid.lserver, From#jid.luser),

  %% if to someone else with content and from a real user and to-user is blocking and from user is not blocking, then block msg
  %% basically we block if to-user is blocking and everything else is normal
  %% also we do not block if both users are blocking bc both are then higher priv moderators and can be trusted
  IsToAnotherUser andalso HasMessageContent andalso IsFromActualUserOrRemoteServer andalso IsToUserBlockingIncoming andalso not IsFromUserBlockingIncoming
.

get_proc_name(Host) ->
  gen_mod:get_module_proc(Host, ?MODULE)
.

get_config_users_dict(Host) ->
  dict:from_list(lists:map(fun(User) -> {User, User} end, gen_mod:get_module_opt(Host, ?MODULE, blocking_users, [])))
.

get_blocking_users(Host) ->
  try gen_server:call(get_proc_name(Host), {get_blocking_users}, ?COMMAND_TIMEOUT)
  catch TypeOfError:Error ->
    ?ERROR_MSG("Failed to get blocking users. Falling back to initial list from mod opts. Type of Error: [~p]. Error: [~p].", [TypeOfError, Error]),
    gen_mod:get_module_opt(Host, ?MODULE, blocking_users, [])
  end
.

is_blocking_user(Host, User) ->
  try gen_server:call(get_proc_name(Host), {is_blocking_user, User}, ?COMMAND_TIMEOUT)
  catch TypeOfError:Error ->
    ?ERROR_MSG("Failed to check if user is blocking. Falling back to false. Type of Error: [~p]. Error: [~p].", [TypeOfError, Error]),
    false
  end
.

reset_blocking_users_all(Host) ->
  lists:foreach(fun(Node) ->
    rpc:call(Node, ?MODULE, reset_blocking_users, [Host], ?COMMAND_TIMEOUT)
  end, ejabberd_admin:list_cluster())
.

reset_blocking_users(Host) ->
  gen_server:call(get_proc_name(Host), {reset_blocking_users}, ?COMMAND_TIMEOUT)
.

clear_blocking_users_all(Host) ->
  lists:foreach(fun(Node) ->
    rpc:call(Node, ?MODULE, clear_blocking_users, [Host], ?COMMAND_TIMEOUT)
  end, ejabberd_admin:list_cluster())
.

clear_blocking_users(Host) ->
  gen_server:call(get_proc_name(Host), {clear_blocking_users}, ?COMMAND_TIMEOUT)
.

add_blocking_user_all(Host, User) ->
  lists:foreach(fun(Node) ->
    rpc:call(Node, ?MODULE, add_blocking_user, [Host, User], ?COMMAND_TIMEOUT)
  end, ejabberd_admin:list_cluster())
.

add_blocking_user(Host, User) ->
  gen_server:call(get_proc_name(Host), {add_blocking_user, User}, ?COMMAND_TIMEOUT)
.

remove_blocking_user_all(Host, User) ->
  lists:foreach(fun(Node) ->
    rpc:call(Node, ?MODULE, remove_blocking_user, [Host, User], ?COMMAND_TIMEOUT)
  end, ejabberd_admin:list_cluster())
.

remove_blocking_user(Host, User) ->
  gen_server:call(get_proc_name(Host), {remove_blocking_user, User}, ?COMMAND_TIMEOUT)
.

get_commands_spec() ->
  [
    #ejabberd_commands{
      name = add_blocking_user,
      tags = [block],
      desc = "Add a user to the list of users blocking incoming subscriptions (friend) requests and unsolicited messages",
      module = ?MODULE,
      function = add_blocking_user_all,
      args = [{host, binary}, {jid, binary}],
      result = {res, rescode}
    },

    #ejabberd_commands{
      name = remove_blocking_user,
      tags = [block],
      desc = "Remove a user from the list of users blocking incoming subscriptions (friend) requests and unsolicited messages",
      module = ?MODULE,
      function = remove_blocking_user_all,
      args = [{host, binary}, {jid, binary}],
      result = {res, rescode}
    },

    #ejabberd_commands{
      name = reset_blocking_users,
      tags = [block],
      desc = "Reset the list of blocking users to the initial module configuration",
      module = ?MODULE,
      function = reset_blocking_users_all,
      args = [{host, binary}],
      result = {res, rescode}
    },

    #ejabberd_commands{
      name = clear_blocking_users,
      tags = [block],
      desc = "Clear the list of blocking users such that the list is empty",
      module = ?MODULE,
      function = clear_blocking_users_all,
      args = [{host, binary}],
      result = {res, rescode}
    },

    #ejabberd_commands{
      name = get_blocking_users,
      tags = [block],
      desc = "Get the list of blocking users",
      module = ?MODULE,
      function = get_blocking_users,
      args = [{host, binary}],
      result = {blocking_users, {list, {blocking_user, string}}}
    }
  ]
.

depends(_Host, _Opts) -> [].

mod_opt_type(blocking_users) ->
  fun(I) when is_list(I) -> I end
.

mod_options(_Host) -> [
  blocking_users
]
.
