-module(muppet_forge_handler_static).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, [AssetsDir]) ->
    case muppet_auth:is_authorized(Req) of
        false -> {ok, Req, should_auth};
        true ->  cowboy_static:rest_init(Req, {dir, AssetsDir })
    end.

handle(Req, should_auth) ->
    H = muppet_auth:challenge(Req),
    {ok, Req2} = cowboy_req:reply(401, H, Req),
    {ok, Req2, undefined};


handle(Req, error) ->
    {ok, Req2} = cowboy_req:reply(500, Req),
    {ok, Req2, undefined};

handle(Req, CowboyStaticState) ->
    {ok, NewReq} = case cowboy_static:resource_exists(Req, CowboyStaticState) of
        {false, _, _} -> 
            cowboy_req:reply(404, Req);
        {true, _, _} ->
            {{stream, Len, StreamFun}, Req2, _} = cowboy_static:get_file(Req, CowboyStaticState),
            cowboy_req:reply(200, cowboy_req:set_resp_body_fun(Len, StreamFun, Req2))
    end,
    {ok, NewReq, undefined}.

terminate(_Reason, _Req, _State) ->
    ok.
