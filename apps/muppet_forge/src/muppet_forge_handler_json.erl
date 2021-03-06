-module(muppet_forge_handler_json).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).
-define(HEADERS, [{<<"content-type">>, <<"application/json">>}]).



init({_, _}, Req, [Atom]) ->
    Msg = case muppet_auth:is_authorized(Req) of
        false -> should_auth;
        true ->
            {Method, _} = cowboy_req:method(Req),
            {Atom, Method}        
    end,
    {ok, Req, Msg}.

handle(Req, should_auth) ->
    H = muppet_auth:challenge(Req),
    {ok, Req2} = cowboy_req:reply(401, H ++ ?HEADERS, jiffy:encode({[{message, auth_required}]}), Req),
    {ok, Req2, undefined};

handle(Req, {module, <<"GET">>}) ->
    {Author, _} = cowboy_req:binding(author, Req),
    {ModuleName, _} = cowboy_req:binding(modulename, Req),
    {Code, Response} = case muppet_repository:find(<<Author/binary, <<"/">>/binary, ModuleName/binary>>) of
        {true, Module} -> {200, muppet_driver:serializable(Module)};
        {false, FullName} -> { 404, {[{module_not_found, FullName}]} }
    end,
    {ok, Req2} = cowboy_req:reply(Code, ?HEADERS, jiffy:encode(Response), Req),
    {ok, Req2, undefined};

handle(Req, {release, <<"DELETE">>}) ->
    {Author, _} = cowboy_req:binding(author, Req),
    {ModuleName, _} = cowboy_req:binding(modulename, Req),
    {Version, _} = cowboy_req:binding(version, Req),
    ok = muppet_repository:delete(Author, ModuleName, versions:version(Version)),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode({[{message, ok}]}), Req),
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
        {not_found, FullName} -> { 410, {[{error, <<  <<"Module ">>/binary, FullName/binary, <<" not found">>/binary >> }]} }
    end,
    {ok, Req2} = cowboy_req:reply(Code, ?HEADERS, jiffy:encode(Response), Req),
    {ok, Req2, undefined};

handle(Req, {deploy, <<"POST">>}) ->
    {ok, TarballBinary, _} = cowboy_req:body(Req),
    {Code, Resp} = case muppet_repository:store(TarballBinary) of
        {ok, _ReleaseCoords} ->  {200, {[{message, ok}]}};
        {error, Error} -> {500, lists:flatten(io_lib:format("~p", [Error]))}
    end,
    {ok, Req2} = cowboy_req:reply(Code, ?HEADERS, jiffy:encode(Resp), Req),
    {ok, Req2, undefined};

handle(Req, {blacklist, <<"GET">>}) ->
    BlackList = muppet_upstream:fetch_blacklist(),
    PropLists = muppet_upstream:serializable_blacklist(BlackList),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(PropLists), Req),
    {ok, Req2, undefined};

handle(Req, {blacklist, <<"PUT">>}) ->
    {ok, BlacklistBinary, _} = cowboy_req:body(Req),
    PropLists = jiffy:decode(BlacklistBinary),
    BlackList = muppet_upstream:proplists_to_blacklist(PropLists),
    ok = muppet_upstream:store_blacklist(BlackList),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode({[{message, ok}]}), Req),
    {ok, Req2, undefined};


handle(Req, {errors, <<"GET">>}) ->
    Errors = muppet_upstream:fetch_errors(),
    Serializable = muppet_upstream:serializable_errors(Errors),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(Serializable), Req),
    {ok, Req2, undefined};

handle(Req, {errors, <<"DELETE">>}) ->
    ok = muppet_upstream:reset_errors(),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode({[{message, ok}]}), Req),
    {ok, Req2, undefined};


handle(Req, {upstream, <<"GET">>}) ->
    UpstreamDict = muppet_upstream:fetch_upstream(),
    Serializable = lists:map(fun({BaseUrl, {UseFastChangeNotification, Time}}) ->
        TimeMillis = timer:now_diff(Time, {0,0,0}) div 1000,
        {[ {base_url, BaseUrl}, {use_fast_change_notification, UseFastChangeNotification}, {time, TimeMillis} ]}
    end, dict:to_list(UpstreamDict)),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(Serializable), Req),
    {ok, Req2, undefined};

handle(Req, {upstream, <<"PUT">>}) ->
    {ok, Body, _} = cowboy_req:body(Req),
    UpstreamInfos = lists:map(fun({UpstreamInfo}) -> 
        UseFastChangeNotification = proplists:get_value(<<"use_fast_change_notification">>, UpstreamInfo, false),
        {_, UpstreamUrl} = proplists:lookup(<<"base_url">>, UpstreamInfo),
        {UpstreamUrl, UseFastChangeNotification}
    end, jiffy:decode(Body)),
    ok = muppet_upstream:store_upstream(UpstreamInfos),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode({[{message, ok}]}), Req),
    {ok, Req2, undefined};

handle(Req, {info, <<"GET">>}) ->
    MirrorAgentInfo = muppet_upstream:info(),
    RepositoryInfo = muppet_repository:info(),
    Serializable = {MirrorAgentInfo ++ RepositoryInfo},
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(Serializable), Req),
    {ok, Req2, undefined};


handle(Req, {_, Method }) ->
    {ok, Req2} = cowboy_req:reply(405, ?HEADERS, jiffy:encode({[{unsupported_method, Method}]}), Req),
    {ok, Req2, undefined}.


terminate(_Reason, _Req, _State) ->
    ok.




query_terms(Query) ->
    case re:split(Query, "[/\\-]", [{return, binary}]) of
        [<<"">>] -> [];
        E -> E
    end.