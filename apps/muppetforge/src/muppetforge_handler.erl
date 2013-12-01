-module(muppetforge_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).
-define(HEADERS, [{<<"content-type">>, <<"application/json">>}]).


init({tcp, http}, Req, [Atom]) ->
    {Method, _} = cowboy_req:method(Req),
    {ok, Req, {Atom, Method}}.

handle(Req, {module, <<"GET">>}) ->
    {Author, _} = cowboy_req:binding(author, Req),
    {ModuleName, _} = cowboy_req:binding(modulename, Req),
    {Code, Response} = case muppet_repository:find(<<Author/binary, <<"/">>/binary, ModuleName/binary>>) of
        {true, Module} -> {200, muppet_driver:serializable(Module)};
        {false, FullName} -> { 404, {[{module_not_found, FullName}]} }
    end,
    {ok, Req2} = cowboy_req:reply(Code, ?HEADERS, jiffy:encode(Response), Req),
    {ok, Req2, undefined};

handle(Req, {modules, <<"GET">>}) ->
    {Query, _} = cowboy_req:qs_val(<<"q">>, Req, <<"">>),
    Modules = muppet_repository:search(query_terms(Query)),
    Serializable = [muppet_driver:serializable(M) || M <- Modules],
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(Serializable), Req),
    {ok, Req2, undefined};

handle(Req, {releases, <<"GET">>}) ->
    {Query, _} = cowboy_req:qs_val(<<"module">>, Req),
    [Author, Module] = re:split(Query, "/", [{return, binary}]),
    {VersionConstraintsBin, _} = cowboy_req:qs_val(<<"version">>, Req, ""),  
    VersionConstraints = versions:constraints(VersionConstraintsBin),
    {Code, Response} = case muppet_repository:find_release({Author, Module}, VersionConstraints) of
        {ok, Dict} ->  {200, muppet_driver:serializable(Dict)};
        {missing_dependency, _} = Error ->{ 500, {[Error]} }
    end,
    {ok, Req2} = cowboy_req:reply(Code, ?HEADERS, jiffy:encode(Response), Req),
    {ok, Req2, undefined};

handle(Req, {deploy, <<"POST">>}) ->
    {ok, TarballBinary, _} = cowboy_req:body(Req),
    muppet_repository:store(TarballBinary),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(ok), Req),
    {ok, Req2, undefined};

handle(Req, {blacklist, <<"GET">>}) ->
    BlackList = muppet_mirror_agent:fetch_blacklist(),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(BlackList), Req),
    {ok, Req2, undefined};

handle(Req, {blacklist, <<"PUT">>}) ->
    {ok, BlacklistBinary, _} = cowboy_req:body(Req),
    BlackList = jiffy:decode(BlacklistBinary),
    ok = muppet_mirror_agent:store_blacklist(BlackList),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(ok), Req),
    {ok, Req2, undefined};


handle(Req, {errors, <<"GET">>}) ->
    Errors = muppet_mirror_agent:fetch_errors(),
    Serializable = muppet_mirror_agent:serializable_errors(Errors),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(Serializable), Req),
    {ok, Req2, undefined};

handle(Req, {errors, <<"DELETE">>}) ->
    ok = muppet_mirror_agent:reset_errors(),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(ok), Req),
    {ok, Req2, undefined};


handle(Req, {upstream, <<"GET">>}) ->
    Upstream = muppet_mirror_agent:fetch_upstream(),
    Serializable = dict:fetch_keys(Upstream),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(Serializable), Req),
    {ok, Req2, undefined};

handle(Req, {upstream, <<"PUT">>}) ->
    {ok, UpstreamBinary, _} = cowboy_req:body(Req),
    Upstream = jiffy:decode(UpstreamBinary),
    ok = muppet_mirror_agent:store_upstream(Upstream),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(ok), Req),
    {ok, Req2, undefined};


handle(Req, {_, Method }) ->
    {ok, Req2} = cowboy_req:reply(405, ?HEADERS, jiffy:encode([<<"unsupported method">>, Method]), Req),
    {ok, Req2, undefined}.


terminate(_Reason, _Req, _State) ->
    ok.




query_terms(Query) ->
    case re:split(Query, "[/\\-]", [{return, binary}]) of
        [<<"">>] -> [];
        E -> E
    end.