-module(muppet_repository).

-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).

-export([find/1, search/1, assets_dir/0]).
-export([start_link/0]).
-define(FULL_NAME(Author, ModuleName), <<Author/binary, <<"/">>/binary, ModuleName/binary>>).
-define(FILE_NAME(Author, ModuleName, Version), << <<"/">>/binary , Author/binary, <<"-">>/binary, ModuleName/binary, <<"-">>/binary, Version/binary, <<".tar.gz">>/binary >>).

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


find_release({Author, Name}, Version) ->
    gen_server:call(?MODULE, {find_release, Author, Name, Version}).
%% priv
find_release([], Modules, Dict) ->
    Dict;
find_release([[FullName, Version] = Current|Others], Modules, Dict) ->
    Dict2 = case dict:is_key(FullName, Dict) of 
        false -> dict:store(FullName, sets:new(), Dict);
        _ -> Dict
    end,
    {true, Module} = find(Modules, FullName),
    ViableReleases = search_matching_versions(Module#module.releases, Version),
    NewSet = sets:union(ViableReleases, dict:fetch(FullName, Dict2)),
    Dict3 = dict:store(FullName, NewSet, Dict2),
    NewDependencies = sets:from_list(lists:flatten([V#release.dependencies || V <- sets:to_list(ViableReleases)])),
    NewQueue = sets:to_list(sets:union(NewDependencies,sets:from_list(Others))),
    find_release(NewQueue, Modules, Dict3).


search_matching_versions(Releases, any) ->
    sets:from_list(Releases);
search_matching_versions(Releases, Version) ->
    sets:from_list(lists:filter(fun(R) -> 
        R#release.version =:= Version
    end, Releases)).


find(FullName) ->
    gen_server:call(?MODULE, {find_module, FullName}).
%% priv
find([Module|T], FullName) ->
    case binary:match(Module#module.full_name, FullName) of
        nomatch -> find(T, FullName);
        _ -> {true, Module}
    end;
find([], FullName) ->
    {false, FullName}.




search(Terms) ->
    gen_server:call(?MODULE, {search_modules, Terms}).
%% priv
search(Modules, Terms) ->
    search(Modules, Terms,[]).
search(Modules, [], _) ->
    Modules;
search([], Terms, Matching) ->
    Matching;
search([Module|Rest], Terms, Matching) ->
    case binary:match(Module#module.full_name, Terms) of
        nomatch -> search(Rest, Terms, Matching);
        _ -> search(Rest, Terms, [Module|Matching])
    end.

% gen_server callbacks

init([]) ->
    AssetsDir = assets_dir(),
    filelib:ensure_dir(AssetsDir),
    {ok, Files} = file:list_dir(AssetsDir),
    ModulesToBeMerged = [read_metadata(AssetsDir ++ "/" ++ F) || F <- Files ],
    {ok, ModulesToBeMerged}.


read_metadata(File) ->
    {ok, FileNamesInTar} = erl_tar:table(File, [compressed]),
    DirName = hd(FileNamesInTar),
    CleanDirName = case hd(lists:reverse(DirName)) of 
        $/ -> DirName;
        _ -> DirName ++ "/"
    end,    
    {ok, [{_, BinaryJson}]} = erl_tar:extract(File, [memory, compressed, {files,[CleanDirName++"metadata.json"]}]),
    {Decoded} = jiffy:decode(BinaryJson),
    Author = element(2, lists:keyfind(<<"author">>, 1, Decoded)),
    FullName = element(2, lists:keyfind(<<"name">>, 1, Decoded)),
    [_, Name] = re:split(FullName, <<"-">>, [{return, binary}]),
    Version = element(2, lists:keyfind(<<"version">>, 1, Decoded)),
    FileName = ?FILE_NAME(Author, Name, Version),
    #module{
        author = Author,
        name = Name,
        full_name = ?FULL_NAME(Author, Name),
        desc = element(2, lists:keyfind(<<"description">>, 1, Decoded)),
        project_url = element(2, lists:keyfind(<<"project_page">>, 1, Decoded)),
        releases = [#release{version=Version, file=FileName, dependencies=[]}], % TODO: dependencies
        tag_list = [] % TODO: not in metadata, should be added to the post request. or /cares.
    }.


handle_cast(Request, State) ->
    {noreply, State}.

handle_call({add_module, Module}, From, State) ->
    % TODO: precond(does not exist)
    {reply, ok, [Module|State]};
handle_call({find_module, FullName}, From, State) ->
    Result = find(State, FullName),
    {reply, Result, State};
handle_call({search_modules, Terms}, From, State) ->
    Result = search(State, Terms),
    {reply, Result, State};
handle_call({find_release, Author, Name, Version}, From, State) ->
    Result = find_release([[?FULL_NAME(Author, Name), Version]], State, dict:new()),
    {reply, Result, State};
handle_call(status, From, State) ->
    {reply, State, State}.

handle_info(Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

dependency(FullName, Version) ->
    [FullName, Version].


release(Version, File, Dependencies) ->
    #release {
        version = Version,
        file = File,
        dependencies = Dependencies
    }.

module(Author, Name, Desc, ProjectUrl, Releases) ->
    #module{
        full_name = ?FULL_NAME(Author, Name),
        author = Author,
        name = Name,
        desc = Desc,
        project_url = ProjectUrl,
        releases = Releases
        % TODO: tag_list
    }.

serializable_module(Module) ->
    Latest = hd(Module#module.releases),
    {[
        {full_name, Module#module.full_name},
        {author, Module#module.author},
        {name, Module#module.name},
        {desc, Module#module.desc},
        {project_url, Module#module.project_url},
        {version, Latest#release.version }, % TODO: check order
        {releases, [serializable_release(R) || R <- Module#module.releases]},
        {tag_list, []}
    ]}.

serializable_release(Release) ->
    {[
        {version, Release#release.version},
        {file, Release#release.file}, 
        {dependencies, Release#release.dependencies}
    ]}.

serializable_releases(Releases) ->
    FullNames = dict:fetch_keys(Releases),
    {[ 
        {FN, [ serializable_release(R) || R <- sets:to_list(dict:fetch(FN, Releases)) ]} || FN <- FullNames 
    ]}.