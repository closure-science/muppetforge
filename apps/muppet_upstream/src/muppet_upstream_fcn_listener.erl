-module(muppet_upstream_fcn_listener).
-behaviour(websocket_client_handler).
-export([init/2, websocket_handle/3, websocket_info/3, websocket_terminate/3]).
-export([start_link/1, close/1]).

-record(state, { parent, upstream } ).

start_link(Upstream) when is_list(Upstream) ->
    start_link(list_to_binary(Upstream));
start_link(Upstream) ->
    <<"http", Rest/binary>> = Upstream,
    Endpoint = "ws" ++ binary_to_list(Rest) ++ "/api/mf/listen",
    websocket_client:start_link(Endpoint, ?MODULE, [self(), Upstream]).

close(Pid) ->
    Pid ! close.

init([ParentPid, Upstream], _ConnState) ->
    {ok, #state{ parent = ParentPid, upstream = Upstream }, 5000}.

websocket_handle({pong, _Msg}, _ConnState, State) ->
    {ok, State};
websocket_handle({text, Msg}, _ConnState, State) ->
    {ModuleInfo} = jiffy:decode(Msg),
    {<<"author">>, Author} = proplists:lookup(<<"author">>, ModuleInfo),
    {<<"module">>, Module} = proplists:lookup(<<"module">>, ModuleInfo),
    {<<"version">>, Version} = proplists:lookup(<<"version">>, ModuleInfo),
    {<<"path">>, TarballPath} = proplists:lookup(<<"path">>, ModuleInfo),
    muppet_upstream ! {new_release, State#state.upstream, Author, Module, versions:version(Version), TarballPath},
    {ok, State};
websocket_handle({_Other, _Msg}, _ConnState, State) ->
    {ok, State}.

websocket_info(close, _ConnState, State) ->
    {close, <<>>, State};
websocket_info(_Any, _ConnState, State) ->
    {ok, State}.

websocket_terminate(_Reason, _ConnState, _State) ->
    ok.
