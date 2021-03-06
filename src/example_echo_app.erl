-module(example_echo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, RootPid} = etcp_server_sup:start_link(), % dummy root, do nothing.
    WorkerChildSpec = #{ id => example_echo_sup,
                         start => { example_echo_sup, start_link, [example_echo] },
                         restart => permanent,
                         shutdown => infinity,
                         type => supervisor,
                         modules => [ example_echo_sup ]
                       },
    {ok, WorkerSupPid} = etcp_server_sup:start_child(WorkerChildSpec),
    Fun = fun() -> supervisor:start_child(WorkerSupPid,[]) end,
    {ok, _Pid} = etcp_listener_sup:start_link(echo,5000,[{reuseaddr,true}],Fun),
    {ok,RootPid}.

stop(_State) ->
    ok.
