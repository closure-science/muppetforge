-module(muppetforge_root_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).
-include("muppetforge.hrl").

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(ok), Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

