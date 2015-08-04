-module(etcp_listener).
-export([
         start_link/3,
         init/3, %% this is actually a private function
         start_worker/1  %% this is actually a private function
        ]).


start_link(Port, Opts, StartChild) ->
    error_logger:info_report({self(), start_listener, Port, Opts, StartChild}),
    proc_lib:start_link(etcp_listener,init,[Port,Opts,StartChild]).

init(Port,Opts,StartChild) ->
    error_logger:info_report({self(), start_worker, Port, Opts, StartChild}),
    WorkerPid = proc_lib:spawn_link(?MODULE, start_worker,[StartChild]),
    Deb = sys:debug_options([]),
    error_logger:info_report({self(), gen_listen, Port, Opts, StartChild}),
    XX = gen_tcp:listen(Port,Opts),
    error_logger:info_report({self(), listen_ok, Port, Opts, StartChild}),
    {ok, Socket} = XX,
    proc_lib:init_ack({ok,self()}),
    loop(Socket,StartChild, WorkerPid, Deb).

loop(Socket,StartChild,WorkerPid, Deb) ->
    try
        error_logger:info_report({self(), start_to_accept, Socket,StartChild, WorkerPid}),
        {ok, AcceptedSocket} = gen_tcp:accept(Socket),
        error_logger:info_report({self(), new_connection, AcceptedSocket, Socket,StartChild, WorkerPid}),
        ok = gen_tcp:controlling_process(AcceptedSocket,WorkerPid),
        error_logger:info_report({self(), handover_controller, AcceptedSocket, Socket,StartChild, WorkerPid}),
        WorkerPid ! { new_conection, AcceptedSocket},
        error_logger:info_report({self(), signal_controller, AcceptedSocket, Socket,StartChild, WorkerPid}),
        NewWorkerPid = proc_lib:spawn_link(?MODULE, start_worker,[StartChild]),
        error_logger:info_report({self(), start_new_worker, AcceptedSocket, Socket,StartChild, NewWorkerPid}),
        %% restart the loop as soon as possible.
        loop(Socket,StartChild,NewWorkerPid, Deb)
    catch E:M ->
            error_logger:error_report({self(),E,M,erlang:get_stacktrace()})
    end.
start_worker(StartChild) ->
    error_logger:info_report({self(), waiting_for_controller}),
    receive
        { new_conection, Socket } ->
            %% when match faild, the pair of process aborted,
            %% hopefully, listener_supervisor will restart it.
            try
                error_logger:info_report({self(), start_new_child}),
                {ok, WorkerPid} = StartChild(),
                error_logger:info_report({self(), new_child_is, WorkerPid}),
                ok = gen_tcp:controlling_process(Socket, WorkerPid),
                %%ok = gen_server:cast(WorkerPid, {become_controller, Socket}),

                %% in order to be more general, because the worker
                %% might not be a gen_server
                WorkerPid ! {become_controller, Socket},
                error_logger:info_report({self(), handover_controller_to_child, WorkerPid})
            catch
                E:X -> error_logger:info_report({8, self(), E,X})
            end
    end,
    error_logger:info_report({self(), "fake worker ended"}).
