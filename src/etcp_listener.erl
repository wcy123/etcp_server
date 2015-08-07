-module(etcp_listener).
-export([
         start_link/3,
         init/3, %% this is actually a private function
         start_worker/1  %% this is actually a private function
        ]).


start_link(Port, Opts, StartChild) ->
    proc_lib:start_link(etcp_listener,init,[Port,Opts,StartChild]).

init(Port,Opts,StartChild) ->
    WorkerPid = proc_lib:spawn_link(?MODULE, start_worker,[StartChild]),
    Deb = sys:debug_options([]),
    {ok, Socket} = gen_tcp:listen(Port,Opts),
    proc_lib:init_ack({ok,self()}),
    loop(Socket,StartChild, WorkerPid, Deb).

loop(Socket,StartChild,WorkerPid, Deb) ->
    try
        {ok, AcceptedSocket} = gen_tcp:accept(Socket),
        ok = gen_tcp:controlling_process(AcceptedSocket,WorkerPid),
        WorkerPid ! { new_conection, AcceptedSocket},
        NewWorkerPid = proc_lib:spawn_link(?MODULE, start_worker,[StartChild]),
        %% restart the loop as soon as possible.
        loop(Socket,StartChild,NewWorkerPid, Deb)
    catch E:M ->
            error_logger:error_report({self(),E,M,erlang:get_stacktrace()})
    end.

start_worker(StartChild) ->
    receive
        { new_conection, Socket } ->
            %% when match faild, the pair of process aborted,
            %% hopefully, listener_supervisor will restart it.
            try
                {ok, WorkerPid} = StartChild(),
                ok = gen_tcp:controlling_process(Socket, WorkerPid),
                %%ok = gen_server:cast(WorkerPid, {become_controller, Socket}),

                %% in order to be more general, because the worker
                %% might not be a gen_server
                WorkerPid ! {become_controller, Socket}
            catch
                E:X -> error_logger:info_report({8, self(), E,X})
            end
    end.
