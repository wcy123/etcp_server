-module(user_default).
-compile({export_all}).
setup_env() ->
    code:add_patha("./ebin").
table(T) ->
    mnesia:transaction(fun () -> mnesia:select(T,[{'_',[],['$_']}]) end).
p(Pid) ->
    case (catch is_process_alive(Pid)) of
        true ->
            io:format("~p~n",
                      [[erlang:process_info(Pid), proc_lib:translate_initial_call(Pid)]]);
        Msg -> io:format("pupu: ~p~n", [Msg])
    end.
tpl(Module) ->
    dbg:tpl(Module,[{'$1',[],[{return_trace}]}]).

%% CatchAll = [{'_',[],['$_']}].
%% mnesia:dirty_select(TableName, CatchAll).
