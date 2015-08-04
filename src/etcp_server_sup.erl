-module(etcp_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Port]) ->
    ChildSpec = {undefined,                     %id
                 {etcp_server,start_link,[]},   %MFA
                 temporary,                     %Restart
                 2000,                          %2 second shutdown
                 worker,
                 [etcp_server]},
    {ok, { {simple_one_for_one, 5, 10}, [ChildSpec]} }.
