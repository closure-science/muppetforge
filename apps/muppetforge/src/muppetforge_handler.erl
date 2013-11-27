-module(muppetforge_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).
-include("muppetforge.hrl").

init({tcp, http}, Req, [module]) ->
    {Method, _} = cowboy_req:method(Req),
    {Author, _} = cowboy_req:binding(author, Req),
    {ModuleName, _} = cowboy_req:binding(modulename, Req),
    {ok, Req, {module, Method, [Author, ModuleName]}};
    
init({tcp, http}, Req, [deploy]) ->
    {Method, _} = cowboy_req:method(Req),
    {ok, Body, _} = cowboy_req:body(Req),
    {ok, Req, {deploy, Method, [Body]}};

init({tcp, http}, Req, [modules]) ->
    {Method, _} = cowboy_req:method(Req),
    {ok, Req, {modules, Method, []}};

init({tcp, http}, Req, [releases]) ->
    {Method, _} = cowboy_req:method(Req),
    {ok, Req, {releases, Method, []}}.

handle(Req, {module, <<"GET">>, [Author, ModuleName]} = State) ->
    {true, Module} = muppet_repository:find(<<Author/binary, <<"/">>/binary, ModuleName/binary>>),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(muppet_repository:serializable_module(Module)), Req),
    {ok, Req2, State};

handle(Req, {modules, <<"GET">>, []} = State) ->
    {Query, _} = cowboy_req:qs_val(<<"q">>, Req, <<"">>),
    Modules = muppet_repository:search(query_terms(Query)),
    Serializable = [muppet_repository:serializable_module(M) || M <- Modules],
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(Serializable), Req),
    {ok, Req2, State};

handle(Req, {deploy, <<"POST">>, [Body]} = State) ->
    Got = muppet_repository:store_module(Body),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(ok), Req),
    {ok, Req2, State};

handle(Req, {releases, <<"GET">>, []} = State) ->
    {Query, _} = cowboy_req:qs_val(<<"module">>, Req),
    [Author, Module] = re:split(Query, "/", [{return, binary}]),
    {Version, _} = cowboy_req:qs_val(<<"version">>, Req, any),    
    Dict = muppet_repository:find_release({Author, Module}, Version),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(muppet_repository:serializable_releases(Dict)), Req),
    {ok, Req2, State};

handle(Req, {_, Method, _} = State) ->
    {ok, Req2} = cowboy_req:reply(405, ?HEADERS, jiffy:encode([<<"unsupported method">>, list_to_binary(io_lib:format("~w", [State]))]), Req),
    {ok, Req2, State}.


terminate(_Reason, _Req, _State) ->
    ok.




query_terms(Query) ->
    case re:split(Query, "[/\\-]", [{return, binary}]) of
        [<<"">>] -> [];
        E -> E
    end.