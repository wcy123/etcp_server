# etcp_server
Simple TCP server framework in Erlang


`etcp_listener.erl` and `etcp_listener_sup.erl` are useful. I don't
plan to write any document, because they are very short and heavily
commented, I copy and past the core part as below

here is an example how to use it

```erlang
main() ->
    %% start a listner on 5000, name is `hello`, Opts = [{reuseaddr,true}],
    %% callback function is `hello/0`
    etcp_listener_sup:start_link(hello, 5000,[{reuseaddr, true}], fun hello/0).

hello() ->
    %% `hello` returns {ok, Pid}.
    {ok ,
     spawn(
       fun () ->
               receive
                   %% when become controller, print hello world, close it and exit.
                   {become_controller, Socket} ->
                       ok = gen_tcp:send(Socket,"hello world!\n"),
                       gen_tcp:close(Socket)
               end
       end)}.
```


```
-module(etcp_listener).
-export([
         start_link/3,
         init/3, %% this is actually a private function
         start_worker/1  %% this is actually a private function
        ]).


start_link(Port, Opts, StartChild) ->
    %% etcp_listener is blocked at `gen_tcp:accept` so that it cannot
    %% be a normal `gen_server`.
    proc_lib:start_link(etcp_listener,init,[Port,Opts,StartChild]).

init(Port,Opts,StartChild) ->
    %% start a worker process, ready to go, waiting for `new_connection`
    WorkerPid = proc_lib:spawn_link(?MODULE, start_worker,[StartChild]),
    %% create a listening socket, in case of error, it crashes. and
    %% `etcp_listener_sup` will restart it.
    {ok, Socket} = gen_tcp:listen(Port,Opts),
    %% acknowledge the `etc_listener:start_link`, so that it continue.
    proc_lib:init_ack({ok,self()}),
    %% enter the main loop
    loop(Socket,StartChild, WorkerPid).

loop(Socket,StartChild,WorkerPid) ->
    %% block here, in case of error, crash
    {ok, AcceptedSocket} = gen_tcp:accept(Socket),
    %% pass controller to the worker process before passing the
    %% controller to it, in case of error, crahs
    ok = gen_tcp:controlling_process(AcceptedSocket,WorkerPid),
    %% tell the worker process, there is a new comer.
    WorkerPid ! { new_conection, AcceptedSocket},
    %% create a new worker and
    NewWorkerPid = proc_lib:spawn_link(?MODULE, start_worker,[StartChild]),
    %% restart the loop as soon as possible.
    loop(Socket,StartChild,NewWorkerPid).

start_worker(StartChild) ->
    receive
        { new_conection, Socket } ->
            %% this process is linked with the listener process, in
            %% case of any error/exception here, the listener will
            %% also died.
            %%
            %% invoke the callback function, to find out the real
            %% worker process. crash and restart in case of error.
            {ok, WorkerPid} = StartChild(),
            %% pass the ball to the new worker process, if the real
            %% worker died right after it is created, this process
            %% also failed, the listener process is alse dying because
            %% of linking.
            ok = gen_tcp:controlling_process(Socket, WorkerPid),
            %% in order to be more general, use the plain Erlang message
            WorkerPid ! {become_controller, Socket}
            %% ok the temporary worker process ends normally, the
            %% listening process won't notice that, because it does
            %% not care. The real worker process takes over from here.
    end.
```
