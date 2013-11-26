-module(muppetforge_modules_handler).
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
    {ok, Req, {modules, Method, []}}.

handle(Req, {module, <<"GET">>, [Author, ModuleName]} = State) ->
    {true, Module} = muppet_repository:search_module([{full_name, <<Author/binary, <<"/">>/binary, ModuleName/binary>>}]),
    {ok, _} = cowboy_req:reply(200, ?HEADERS, jiffy:encode({Module}), Req),
    {ok, Req, State};
handle(Req, {modules, <<"GET">>, []} = State) ->
    {Query, _} = cowboy_req:qs_val(<<"q">>, Req, <<"">>),
    Modules = muppet_repository:search_modules(query_terms(Query)),
    {ok, _} = cowboy_req:reply(200, ?HEADERS, jiffy:encode([{M} || M <- Modules]), Req),
    {ok, Req, State};
handle(Req, {deploy, <<"POST">>, [Body]} = State) ->
    io:format("about to parse"),
    Got = muppet_repository:read_metadata({binary , Body}),
    io:format("parsed ~p~n", [Got]),
    {ok, _} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(ok), Req),
    {ok, Req, State};
handle(Req, {_, Method, _} = State) ->
    {ok, _} = cowboy_req:reply(405, ?HEADERS, jiffy:encode([<<"unsupported method">>, list_to_binary(io_lib:format("~w", [State]))]), Req),
    {ok, Req, State}.


terminate(_Reason, _Req, _State) ->
    ok.




query_terms(Query) ->
    case re:split(Query, "[/\\-]", [{return, binary}]) of
        [<<"">>] -> [];
        E -> E
    end.