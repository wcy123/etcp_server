-module(etcp_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = etcp_listener_sup:start_link(5000,[{reuseaddr,true}],example_echo),
    %%{ok, _Pid} = supervisor:start_child(Pid,[1234,[]]),
    {ok,Pid}.

stop(_State) ->
    ok.
