-module(muppet_repository).

-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).

-export([search_module/1, search_modules/1, assets_dir/0]).
-export([start_link/0]).

-record(release, {
    version,
    file, 
    dependencies = [] % [FullName, Version] -> ["Mario/mario", "0.1.1"]
}).

-record(module, {
    full_name,
    author,
    name,
    desc,
    project_url,
    releases = [], % [#release{}]
    tag_list = []
}).


-compile(export_all).

%public interface

start_link() ->
    process_flag(trap_exit, true),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

assets_dir() ->
    code:priv_dir(?MODULE) ++ "/assets".


search_module(Terms) ->
    gen_server:call(?MODULE, {search_one, Terms}).

search_modules(Terms) ->
    gen_server:call(?MODULE, {search_all, Terms}).

% gen_server callbacks

init([]) ->
    filelib:ensure_dir(assets_dir()),
    state_from_priv_metadata().

handle_cast(Request, State) ->
    {noreply, State}.

handle_call({add_module, Module}, From, State) ->
    % TODO: precond(does not exist)
    {reply, ok, [Module|State]};
handle_call({search_one, Terms}, From, State) ->
    Result = search_one(State, Terms),
    {reply, Result, State};
handle_call({search_all, Terms}, From, State) ->
    Result = search_all(State, Terms),
    {reply, Result, State};
handle_call(status, From, State) ->
    {reply, State, State}.

handle_info(Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


%priv
state_from_priv_metadata() ->
    {ok, Files} = file:list_dir(assets_dir()),
    State = lists:map(fun read_metadata/1, Files), % todo: filter for tgz only?
    {ok, State}.

read_metadata(File) ->
    {ok, FileNamesInTar} = erl_tar:table(File, [compressed]),
    DirName = hd(FileNamesInTar),
    {ok, [{_, BinaryJson}]} = erl_tar:extract(File, [memory, compressed, {files,[DirName++"/metadata.json"]}]),
    {Decoded} = jiffy:decode(BinaryJson),
    Author = element(2, lists:keyfind(<<"author">>, 1, Decoded)),
    Name = element(2, lists:keyfind(<<"name">>, 1, Decoded)), % FIXME: name in metadata.json is the dash-separated fullname 
    Version = element(2, lists:keyfind(<<"version">>, 1, Decoded)),
    FileName = <<Author/binary, <<"-">>/binary, Name/binary, <<"-">>/binary, Version/binary, <<".tar.gz">>/binary >>,
    #module{
        author = Author,
        name = Name,
        full_name = <<Author/binary, <<"/">>/binary, Name/binary>>,
        desc = element(2, lists:keyfind(<<"description">>, 1, Decoded)),
        project_url = element(2, lists:keyfind(<<"project_page">>, 1, Decoded)),
        releases = [#release{version=Version, file=FileName, dependencies=[]}], % TODO: dependencies
        tag_list = [] % TODO: not in metadata, should be added to the post request. or /cares.
    }.

search_one([Module|T], Terms) ->
    case module_matches(Module, Terms) of
        true -> {true, Module};
        false -> search_one(T, Terms)
    end;
search_one([], Terms) ->
    {false, Terms}.


search_all(Modules, Terms) ->
    search_all(Modules, Terms,[]).
search_all([Module|Rest], Terms, Matching) ->
    case value_matches(Module, Terms) of
        true -> search_all(Rest, Terms, [Module|Matching]);
        false -> search_all(Rest, Terms, Matching)
    end;
search_all([], Terms, Matching) ->
    Matching.

module_matches(Module, Terms) ->
    lists:all(fun(Term) -> 
        lists:any(fun(Tuple) -> Term =:= Tuple end, Module)
    end, Terms).


value_matches(Module, Terms) ->
    lists:all(fun(Term) -> 
        lists:any(fun({_ , Value}) -> Term =:= Value end, Module)
    end, Terms).

