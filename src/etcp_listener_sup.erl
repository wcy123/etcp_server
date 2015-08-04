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
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Port,Opts,Module) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port,Opts,Module]).

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
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    ListenerChild = #{id => etcp_listener,
                      start => { etcp_listener, start_link, [Port, Opts, Fun]},
                      restart => permanent,
                      shutdown => 5000,
                      type => worker,
                      modules => [etcp_listener]},

    {ok, {SupFlags, [ListenerChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
