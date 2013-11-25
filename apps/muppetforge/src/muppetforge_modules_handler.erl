-module(muppetforge_modules_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).
-include("muppetforge.hrl").

init({tcp, http}, Req, [module]) ->
    {Method, _} = cowboy_req:method(Req),
    {Author, _} = cowboy_req:binding(author, Req),
    {ModuleNameAndSuffix, _} = cowboy_req:binding(modulename, Req),
    ModuleName = module_name(ModuleNameAndSuffix),
    {ok, Req, {module, Method, [Author, ModuleName]}};
init({tcp, http}, Req, [modules]) ->
    {Method, _} = cowboy_req:method(Req),
    {ok, Req, {modules, Method, []}}.

handle(Req, {module, <<"GET">>, [Author, ModuleName]} = State) ->
    {true, Module} = muppet_repository:search_module([{full_name, <<Author/binary, <<"/">>/binary, ModuleName/binary>>}]),
    {ok, _} = cowboy_req:reply(200, ?HEADERS, jiffy:encode({Module}), Req),
    {ok, Req, State};
handle(Req, {modules, <<"GET">>, []} = State) ->
    Modules = muppet_repository:search_modules([]),
    {ok, _} = cowboy_req:reply(200, ?HEADERS, jiffy:encode([{M} || M <- Modules]), Req),
    {ok, Req, State};
handle(Req, {_, Method, _} = State) ->
    {ok, _} = cowboy_req:reply(405, ?HEADERS, jiffy:encode([<<"unsupported method">>, Method]), Req),
    {ok, Req, State}.


terminate(_Reason, _Req, _State) ->
    ok.

module_name(NameAndSuffix) ->
    binary:part(NameAndSuffix, {0, byte_size(NameAndSuffix)-5}).
