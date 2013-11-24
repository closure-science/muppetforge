-module(muppetforge_module_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).
 
init(_Type, Req, []) ->
        {ok, Req, undefined}.

handle(Req, State) ->
    {ok, RequestBody, _} = cowboy_req:body(Req),
    % Request = jiffy:decode(RequestBody),
    %
    Response = ok,
    Headers = [
        {<<"content-type">>, <<"application/json">>}
    ],
    {ok, Req2} = cowboy_req:reply(200, Headers, jiffy:encode(Response), Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

