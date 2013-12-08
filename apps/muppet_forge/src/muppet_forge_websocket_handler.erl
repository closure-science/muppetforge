-module(muppet_forge_websocket_handler).
-behaviour(cowboy_websocket_handler).
-export([init/3, websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).
 
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.
 
websocket_init(_Transport, Req, _Opts) ->
    muppet_repository_observable:register(),
    {ok, Req, undefined, hibernate}.
 
websocket_info({new_release, {Author, Module, Version, File}}, Req, State) ->
    Msg = jiffy:encode({[
        {author, Author},
        {module, Module},
        {version, versions:to_binary(Version)},
        {path, File}
    ]}),
    {reply, {text, Msg}, Req, State, hibernate};

websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_handle({ping, _Data}, Req, State) ->
    {reply, pong, Req, State, hibernate};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    muppet_repository_observable:deregister(),
    ok.
 
