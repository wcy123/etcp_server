-module(hello_world).
-compile(export_all).



main() ->
    etcp_listener_sup:start_link(hello, 5000,[{reuseaddr, true}], fun hello/0).

hello() ->
    {ok ,
     spawn(
       fun () ->
               receive
                   {become_controller, Socket} ->
                       ok = gen_tcp:send(Socket,"hello world!\n"),
                       gen_tcp:close(Socket)
               end
       end)}.
