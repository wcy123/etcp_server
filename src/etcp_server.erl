%%%-------------------------------------------------------------------
%%% @author chunywan <>
%%% @copyright (C) 2015, chunywan
%%% @doc
%%%
%%% @end
%%% Created :  2 Aug 2015 by chunywan <>
%%%-------------------------------------------------------------------
-module(etcp_server).

-behaviour(gen_server).

%% API
-export([start_link/4, start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {module,socket,parent,state=undefined,opts=[]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Module, Port, ListenOpts, Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,  [Module, Port, ListenOpts, Opts, self()],[]).
start_link('$listen_again', State) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,  ['$listen_again', State],[]).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([M, Port, ListenOpts, Opts, Parent]) ->
    {ok , Socket} = gen_tcp:listen(Port,ListenOpts),
    {ok, #state{module = M, parent=Parent, socket=Socket, state=undefined, opts = Opts}, 0};
init(['$listen_again',State]) ->
    {ok, State , 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Request, From, State) ->
    case (State#state.module):handle_call(Request,From,State#state.state) of
        {Reply,S,TimeOut} ->
            {Reply, State#state{state=S}, TimeOut};
        {Reply,S} ->
            {Reply, State#state{state=S}}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    case (State#state.module):handle_cast(Msg,State#state.state) of
        {Reply,S,TimeOut} ->
            {Reply, State#state{state=S}, TimeOut};
        {Reply,S} ->
            {Reply, State#state{state=S}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, #state{module=Module, opts=Opts, socket=Socket, state=undefined} = State) ->
    {ok, AcceptSocket} = gen_tcp:accept(State#state.socket),
    case process_info(self(),registered_name) of
        {registered_name,Name} -> unregister(Name);
        _Otherwise -> ok
    end,
    {ok, Pid} = supervisor:start_child(State#state.parent,['$listen_again', State]), % continue to listen
    ok = gen_tcp:controlling_process(Socket,Pid),
    ok = inet:setopts(AcceptSocket,Opts),
    case Module:init([Socket]) of
       {ok, S} -> {noreply, State#state{state=S}};
       {ok, S, Timeout}  -> {noreply, State#state{state=S}, Timeout}
    end;
handle_info(Info,State) ->
    case (State#state.module):handle_info(Info,State#state.state) of
        {noreply,S,TimeOut} ->
            {noreply, State#state{state=S}, TimeOut};
        {noreply,S} ->
            {noreply, State#state{state=S}};
        {stop,Reason,NewState} -> {stop, Reason, State#state{state=NewState}}
    end.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    io:format("state is ~p~n",[State]),
    case State#state.state of
        undefined ->
            ok;
        Else -> (State#state.module):terminate(Reason,Else)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok,State}.



%%%===================================================================
%%% internal functions
%%%===================================================================
