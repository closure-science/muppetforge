-module(muppet_forge_handler_websocket).
-behaviour(cowboy_websocket_handler).
-export([init/3, websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_info/2, handle_call/2, code_change/3, terminate/2]).
-record(state, { handler_ref }).

 
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.
 
websocket_init(_Transport, Req, _Opts) ->
    HandlerRef = make_ref(),
    ok = muppet_repository:add_handler(?MODULE, HandlerRef, [self()]),
    {ok, Req, #state{handler_ref = HandlerRef }, hibernate}.
 
websocket_info({new_release, {Author, Module, Version, File}}, Req, State) ->
    Msg = jiffy:encode({[
        {author, Author},
        {module, Module},
        {version, versions:to_binary(Version)},
        {path, File}
    ]}),
    {reply, {text, Msg}, Req, State, hibernate};

websocket_info(_Info, Req, State) ->
    io:format("WINFO ~p~n", [_Info]),
    {ok, Req, State, hibernate}.

websocket_handle({ping, _Data}, Req, State) ->
    {reply, pong, Req, State, hibernate};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, State) ->
    muppet_repository:delete_handler(?MODULE, State#state.handler_ref, [self()]),
    ok.


% gen_event callbacks
init([Pid]) -> 
    {ok, Pid}.

handle_event(Msg, Pid) -> 
    Pid ! Msg, 
    {ok, Pid}.

handle_info(_Msg, State) -> 
    {ok , State}.

handle_call(_Req, State) -> 
    {ok, {error, badrequest}, State}.

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

terminate(_Arg, _State) ->  
    ok.