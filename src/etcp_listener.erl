-module(etcp_listener).
-export([
         start_link/3,
         init/3, %% this is actually a private function
         start_worker/1  %% this is actually a private function
         ]).


start_link(Port, Opts, ListenerSupervisor) ->
    error_logger:info_report({1, Port, Opts, ListenerSupervisor}),
    proc_lib:start_link(etcp_listener,init,[Port,Opts,ListenerSupervisor]).

init(Port,Opts,ListenerSupervisor) ->
    error_logger:info_report({2, Port, Opts, ListenerSupervisor}),
    WorkerPid = proc_lib:spawn_link(?MODULE, start_worker,[ListenerSupervisor]),
    Deb = sys:debug_options([]),
    XX = gen_tcp:listen(Port,Opts),
    error_logger:info_report({3, XX, Port, Opts, ListenerSupervisor}),
    {ok, Socket} = XX,
    proc_lib:init_ack({ok,self()}),
    loop(Socket,ListenerSupervisor, WorkerPid, Deb).

loop(Socket,ListenerSupervisor,WorkerPid, Deb) ->
    error_logger:info_report({4, self(), Socket,ListenerSupervisor, WorkerPid}),
    {ok, AcceptedSocket} = gen_tcp:accept(Socket),
    error_logger:info_report({5, self(), AcceptedSocket, Socket,ListenerSupervisor, WorkerPid}),
    ok = gen_tcp:controlling_process(AcceptedSocket,WorkerPid),
    WorkerPid ! { new_conection, AcceptedSocket},
    error_logger:info_report({6, self(), AcceptedSocket, Socket,ListenerSupervisor, WorkerPid}),
    NewWorkerPid = proc_lib:spawn_link(?MODULE, start_worker,[ListenerSupervisor]),
    error_logger:info_report({7, AcceptedSocket, Socket,ListenerSupervisor, NewWorkerPid}),
    %% restart the loop as soon as possible.
    loop(Socket,ListenerSupervisor,NewWorkerPid, Deb).


start_worker(ListenerSupervisor) ->
    receive
        { new_conection, Socket } ->

            %% when match faild, the pair of process aborted,
            %% hopefully, listener_supervisor will restart it.
            Children = supervisor:which_children(ListenerSupervisor),
            try
              {_Id, WorkerSupPid, _Type, _Modules} = lists:keyfind(etcp_worker_sup,1,Children),
              true = is_process_alive(WorkerSupPid),
              {ok, WorkerPid} = supervisor:start_child(WorkerSupPid,[Socket]),
              error_logger:info_report({8, self(), WorkerPid, Children}),
              ok = gen_tcp:controlling_process(Socket, WorkerPid),
              ok = gen_server:cast(WorkerPid, {become_controller, Socket})
            catch
                E:X -> error_logger:info_report({8, self(), E,X, Children})
            end

    end,
    error_logger:info_report({self(), "fake worker ended"}).
