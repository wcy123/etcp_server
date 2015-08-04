-module(user_default).
-compile({export_all}).
say () ->
    io:format("good~n").

reload_me() ->
    shell_default:nl(?MODULE).

setup_env() ->
    code:add_patha("./ebin").

start(ejabberd_config) ->
    mnesia:start(),
    randoms:start(),
    p1_yaml:start(),
    stringprep:start(),
    application:load(ejabberd),
    p1_sha:load_nif(),
    ejabberd_config:srart().

table(T) ->
    mnesia:transaction(fun () -> mnesia:select(T,[{'_',[],['$_']}]) end).
%%ejabberd_config:set_opts().

p(Pid) ->
    case (catch is_process_alive(Pid)) of
        true ->
            io:format("~p~n",
                      [[erlang:process_info(Pid), proc_lib:translate_initial_call(Pid)]]);
        Msg -> io:format("pupu: ~p~n", [Msg])
    end.
show_port_info_header( Port ) ->
    lists:foreach(
      fun({Key,_}) ->
              io:format("~15w ",[Key])
      end,erlang:port_info(Port)),
    io:format("~n").
show_port_info( Port ) ->
    lists:foreach(
      fun({_,Info}) ->
              io:format("~15w ",[Info])
      end,erlang:port_info(Port)),
    io:format("~n").
ports() ->
    show_port_info_header(hd(erlang:ports())),
    lists:foreach(fun show_port_info/1 , erlang:ports()).

tpl(Module) ->
    dbg:tpl(Module,[{'$1',[],[{return_trace}]}]).

%% CatchAll = [{'_',[],['$_']}].
%% mnesia:dirty_select(TableName, CatchAll).
