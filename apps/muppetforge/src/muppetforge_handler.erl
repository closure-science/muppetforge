-module(muppetforge_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).
-define(HEADERS, [{<<"content-type">>, <<"application/json">>}]).


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
    {Code, Response} = case muppet_repository:find(<<Author/binary, <<"/">>/binary, ModuleName/binary>>) of
        {true, Module} -> {200, muppet_driver:serializable(Module)};
        {false, FullName} -> { 404, {[{module_not_found, FullName}]} }
    end,
    {ok, Req2} = cowboy_req:reply(Code, ?HEADERS, jiffy:encode(Response), Req),
    {ok, Req2, State};

handle(Req, {modules, <<"GET">>, []} = State) ->
    {Query, _} = cowboy_req:qs_val(<<"q">>, Req, <<"">>),
    Modules = muppet_repository:search(query_terms(Query)),
    Serializable = [muppet_driver:serializable(M) || M <- Modules],
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(Serializable), Req),
    {ok, Req2, State};

handle(Req, {deploy, <<"POST">>, [TarballBinary]} = State) ->
    muppet_repository:store(TarballBinary),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(ok), Req),
    {ok, Req2, State};

handle(Req, {releases, <<"GET">>, []} = State) ->
    {Query, _} = cowboy_req:qs_val(<<"module">>, Req),
    [Author, Module] = re:split(Query, "/", [{return, binary}]),
    {VersionConstraintsBin, _} = cowboy_req:qs_val(<<"version">>, Req, ""),  
    VersionConstraints = versions:constraints(VersionConstraintsBin),
    {Code, Response} = case muppet_repository:find_release({Author, Module}, VersionConstraints) of
        {ok, Dict} ->  {200, muppet_driver:serializable(Dict)};
        {missing_dependency, _} = Error ->{ 500, {[Error]} }
    end,
    {ok, Req2} = cowboy_req:reply(Code, ?HEADERS, jiffy:encode(Response), Req),
    {ok, Req2, State};

handle(Req, {_, Method, _} = State) ->
    {ok, Req2} = cowboy_req:reply(405, ?HEADERS, jiffy:encode([<<"unsupported method">>, Method]), Req),
    {ok, Req2, State}.


terminate(_Reason, _Req, _State) ->
    ok.




query_terms(Query) ->
    case re:split(Query, "[/\\-]", [{return, binary}]) of
        [<<"">>] -> [];
        E -> E
    end.