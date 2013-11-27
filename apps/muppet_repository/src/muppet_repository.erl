-module(muppet_repository).

-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).

-export([find/1, search/1, assets_dir/0]).
-export([start_link/0]).
-define(FULL_NAME(Author, ModuleName), <<Author/binary, <<"/">>/binary, ModuleName/binary>>).
-define(FILE_NAME(Author, ModuleName, Version), << <<"/">>/binary , Author/binary, <<"-">>/binary, ModuleName/binary, <<"-">>/binary, Version/binary, <<".tar.gz">>/binary >>).

-record(release, {
    version, % {1.0.0}
    file, 
    dependencies = [] % {FullName, [constraint_type()]} -> {"Mario/mario", [constraints]}
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


find_release({Author, Name}, VersionConstraints) ->
    gen_server:call(?MODULE, {find_release, Author, Name, VersionConstraints}).
%% priv
find_release([], _Modules, Dict) ->
    Dict;
find_release([{FullName, VersionConstraints}|Others], Modules, Dict) ->
    Dict2 = case dict:is_key(FullName, Dict) of 
        false -> dict:store(FullName, sets:new(), Dict);
        _ -> Dict
    end,
    {true, Module} = find(Modules, FullName),
    ViableReleases = search_matching_versions(Module#module.releases, VersionConstraints),
    NewSet = sets:union(ViableReleases, dict:fetch(FullName, Dict2)),
    Dict3 = dict:store(FullName, NewSet, Dict2),
    NewDependencies = sets:from_list(lists:append([V#release.dependencies || V <- sets:to_list(ViableReleases)])),
    NewQueue = sets:to_list(sets:union(NewDependencies,sets:from_list(Others))),
    find_release(NewQueue, Modules, Dict3).


search_matching_versions(Releases, VersionConstraints) ->
    sets:from_list(lists:filter(fun(R) -> 
        versions:matches(VersionConstraints, R#release.version)
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

store_module(Tarball) ->
    gen_server:call(?MODULE, {store_module, Tarball}).

store_module(Module, Tarball, Modules) ->
    % TODO: split save_tarball from add_metadata (also used in init state loading)
    [Release] = Module#module.releases,
    AbsFileName = assets_dir() ++ binary_to_list(Release#release.file),
    ok = file:write_file(AbsFileName, Tarball),
    merge_into_modules(Module, Modules).

merge_into_modules(Module, Modules) ->
    case lists:keyfind(Module#module.full_name, 2, Modules) of
        false ->
            [Module| Modules];
        MatchedModule -> 
                NewReleases = Module#module.releases,
                PurgedOldModules = lists:foldl(fun(NewRel, Acc) -> 
                    lists:keydelete(NewRel#release.version, 2, Acc) 
                end, MatchedModule#module.releases, NewReleases),
                NewModule = MatchedModule#module{ releases = NewReleases ++ PurgedOldModules},
                lists:keyreplace(Module#module.full_name, 2, Modules, NewModule)
    end.

search(Terms) ->
    gen_server:call(?MODULE, {search_modules, Terms}).
%% priv
search(Modules, Terms) ->
    search(Modules, Terms,[]).
search(Modules, [], _) ->
    Modules;
search([], _Terms, Matching) ->
    Matching;
search([Module|Rest], Terms, Matching) ->
    case binary:match(Module#module.full_name, Terms) of
        nomatch -> search(Rest, Terms, Matching);
        _ -> search(Rest, Terms, [Module|Matching])
    end.

% gen_server callbacks

init([]) ->
    AssetsDir = assets_dir(),
    filelib:ensure_dir(AssetsDir ++ "/anyname"),
    {ok, Files} = file:list_dir(AssetsDir),
    Modules = lists:foldl(fun(F, ModulesIn) ->
        Read = read_metadata(AssetsDir ++ "/" ++ F),
        merge_into_modules(Read, ModulesIn)
    end, [], Files),
    {ok, Modules}.


read_metadata(File) ->
    {ok, FileNamesInTar} = erl_tar:table(File, [compressed]),
    DirName = hd(FileNamesInTar),
    CleanDirName = case hd(lists:reverse(DirName)) of 
        $/ -> DirName;
        _ -> DirName ++ "/"
    end,    
    {ok, [{_, BinaryJson}]} = erl_tar:extract(File, [memory, compressed, {files,[CleanDirName++"metadata.json"]}]),
    {Decoded} = jiffy:decode(BinaryJson),
    Author = proplists:get_value(<<"author">>, Decoded),
    FullName = proplists:get_value(<<"name">>, Decoded),
    [_, Name] = re:split(FullName, <<"[\\-/]">>, [{return, binary}]),
    BinaryVersion = proplists:get_value(<<"version">>, Decoded),
    Version = versions:version(BinaryVersion),

    RawDependencies = proplists:get_value(<<"dependencies">>, Decoded),
    Dependencies = [
        {
            proplists:get_value(<<"name">>, element(1, D)), 
            versions:constraints(proplists:get_value(<<"version_requirement">>, element(1, D)))
        } 
            || D <- RawDependencies
    ],
    FileName = ?FILE_NAME(Author, Name, BinaryVersion),
    #module{
        author = Author,
        name = Name,
        full_name = ?FULL_NAME(Author, Name),
        desc = proplists:get_value(<<"description">>, Decoded),
        project_url = proplists:get_value(<<"project_page">>, Decoded),
        releases = [#release{version=Version, file=FileName, dependencies=Dependencies}],
        tag_list = [] % TODO: not in metadata, should be added to the post request. or /cares.
    }.


handle_cast(_Request, State) ->
    {noreply, State}.

handle_call({add_module, Module}, _From, State) ->
    % TODO: precond(does not exist)
    {reply, ok, [Module|State]};
handle_call({find_module, FullName}, _From, State) ->
    Result = find(State, FullName),
    {reply, Result, State};
handle_call({search_modules, Terms}, _From, State) ->
    Result = search(State, Terms),
    {reply, Result, State};
handle_call({find_release, Author, Name, VersionConstraints}, _From, State) ->
    Result = find_release([{?FULL_NAME(Author, Name), VersionConstraints}], State, dict:new()),
    {reply, Result, State};
handle_call({store_module, Tarball}, _From, State) ->
    Module = read_metadata({binary, Tarball}),
    NewState = store_module(Module, Tarball, State),
    {reply, ok, NewState};
handle_call(status, _From, State) ->
    {reply, State, State}.

handle_info(_Info, State) ->
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

-spec serializable_module(#module{}) -> tuple(list()).
serializable_module(Module) ->
    [First|Others] = Module#module.releases,
    Latest = lists:foldl(fun(R, Max) ->
        versions:max(R#release.version, Max)
    end, First#release.version, Others),
    {[
        {full_name, Module#module.full_name},
        {author, Module#module.author},
        {name, Module#module.name},
        {desc, Module#module.desc},
        {project_url, Module#module.project_url},
        {version, versions:to_binary(Latest) }, 
        {releases, [serializable_release(R) || R <- Module#module.releases]},
        {tag_list, []}
    ]}.

serializable_release(Release) ->
    {[
        {version, versions:to_binary(Release#release.version)},
        {file, Release#release.file}, 
        {dependencies, [[element(1, D), versions:to_binary(element(2, D))] || D <- Release#release.dependencies ]}
    ]}.

serializable_releases(Releases) ->
    FullNames = dict:fetch_keys(Releases),
    {[ 
        {FN, [ serializable_release(R) || R <- sets:to_list(dict:fetch(FN, Releases)) ]} || FN <- FullNames 
    ]}.