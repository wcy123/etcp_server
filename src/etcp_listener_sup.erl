%%%-------------------------------------------------------------------
%%% @author chunywan <wcy123@gmail.com>
%%% @copyright (C) 2015, chunywan
%%% @doc
%%%    a supervisor for a tcp listener.
%%% @end
%%% Created :  3 Aug 2015 by chunywan <>
%%%-------------------------------------------------------------------
-module(etcp_listener_sup).

-behaviour(supervisor).

%% API
-export([start_link/4]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the listener supervisor
%%
%%   Name - the name of the server
%%   Port - the port which is listening on
%%   Opts - socket options
%%   Fun - call back function.
%%
%%   When there is a new comer, the call back function is invoked in
%%   the context of a worker process without any argument, the
%%   callback function is expected to return {ok, Pid} where Pid is
%%   the worker process id.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(Name, Port, Opts, Fun) -> {ok, pid()} | ignore | {error, term()} when
      Name :: atom(),
      Port :: non_neg_integer(),
      Opts :: [inet:socket_setopt()],
      Fun :: function().
start_link(Name, Port,Opts,Fun) ->
    supervisor:start_link({local, Name}, ?MODULE, [Port,Opts,Fun]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([Port, Opts, Fun]) ->
    SupFlags = {one_for_one, 1,5},
    ListenerChild = {etcp_listener,
                      { etcp_listener, start_link, [Port, Opts, Fun]},
                      permanent,
                      5000,
                      worker,
                      [etcp_listener]},
    {ok, {SupFlags, [ListenerChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
