-module(muppet_driver).
-export([new/0, new/1, find_release/4, find/2, store/3, delete/3, search/2, serializable/1, author_and_module/1, knows/4, info/1]).

-define(FULL_NAME(Author, ModuleName), <<Author/binary, <<"/">>/binary, ModuleName/binary>>).
-define(FILE_NAME(Author, ModuleName, Version), << <<"/">>/binary , Author/binary, <<"-">>/binary, ModuleName/binary, <<"-">>/binary, Version/binary, <<".tar.gz">>/binary >>).

-record(release, { version, file, dependencies = [] }).

-record(module, { full_name, author, name, desc, project_url, releases = [], tag_list = [] }).


-type module_type() :: #module{}.
-type state() :: [module_type()].
-export_type([module_type/0]).



% -----------------------------------------------------------------------------
-spec new() -> state().
% -----------------------------------------------------------------------------
new() ->
    [].


% -----------------------------------------------------------------------------
-spec new(string()) -> state().
% -----------------------------------------------------------------------------
new(AssetsDir) ->
    filelib:fold_files(AssetsDir, "\\.tar\\.gz", false, fun(FileName, ModulesIn) ->
        merge_into_modules(read_metadata(FileName), ModulesIn)
    end, []).


% -----------------------------------------------------------------------------
-spec knows(state(), Author::binary(), ModuleName::binary(), Version::versions:version_type()) -> boolean().
% -----------------------------------------------------------------------------
knows([Module|Rest], Author, ModuleName, Version) ->
    case Author =:= Module#module.author andalso ModuleName =:= Module#module.name andalso lists:keyfind(Version, 2, Module#module.releases) =/= false of
        true -> true;
        false -> knows(Rest, Author, ModuleName, Version)
    end;
knows([], _Author, _ModuleName, _Version) ->
    false.


% -----------------------------------------------------------------------------
-spec find_release(state(), binary(), binary(), [versions:constraint_type()]) -> {ok, dict()} | {not_found, FullName::binary()}.
% -----------------------------------------------------------------------------
find_release(Modules, Author, Name, Constraints) ->
    FullName = ?FULL_NAME(Author, Name),
    case lists:keyfind(FullName, 2, Modules) of
        false -> {not_found, FullName};
        _ -> {ok, find_release(Modules, [{?FULL_NAME(Author, Name), Constraints}], dict:new())}
    end.

find_release(_Modules, [], Dict) ->
    Dict;
find_release(Modules, [{FullName, VersionConstraints}|Others], Dict) ->
    Dict2 = case dict:is_key(FullName, Dict) of 
        false -> dict:store(FullName, sets:new(), Dict);
        _ -> Dict
    end,
    case find(Modules, FullName) of 
        {false, FullName} -> 
            find_release(Modules, Others, Dict2);
        {true, Module} -> 
            ViableReleases = sets:from_list(lists:filter(fun(R) -> 
                versions:matches(VersionConstraints, R#release.version)
            end, Module#module.releases)),
            NewSet = sets:union(ViableReleases, dict:fetch(FullName, Dict2)),
            Dict3 = dict:store(FullName, NewSet, Dict2),
            NewDependencies = sets:from_list(lists:append([V#release.dependencies || V <- sets:to_list(ViableReleases)])),
            NewQueue = sets:to_list(sets:union(NewDependencies,sets:from_list(Others))),
            find_release(Modules, NewQueue, Dict3)
    end.


placeholder_module(FullName) ->
    {Author, Module} = author_and_module(FullName),
    #module{ author=Author, name=Module, full_name=FullName }.

% -----------------------------------------------------------------------------
-spec find(state(), binary()) -> {true, module_type()} | {false, FullName::binary()}.
% -----------------------------------------------------------------------------
find([Module|T], FullName) ->
    case binary:match(Module#module.full_name, FullName) of
        nomatch -> find(T, FullName);
        _ -> {true, Module}
    end;
find([], FullName) ->
    {false, FullName}.


% -----------------------------------------------------------------------------
-spec delete(state(), string(), binary()) -> {ok, state()} | {error, state()}.
% -----------------------------------------------------------------------------
delete(Modules, AssetsDir, {Author, Name, Version}) ->
    {Matching, Others} = lists:partition(fun(M) -> 
        knows([M], Author, Name, Version)
    end, Modules),
    case Matching of
        [] -> {error, Modules};
        [M] -> 
            V = versions:to_binary(Version),
            FileName = AssetsDir ++ binary_to_list(?FILE_NAME(Author, Name, V)),
            ok = file:delete(FileName),
            NewReleases = lists:keydelete(Version, 2, M#module.releases),
            case NewReleases of 
                [] -> {ok, Others};
                _ -> {ok, [M#module{releases = NewReleases} | Others]}
            end            
    end.

% -----------------------------------------------------------------------------
-spec store(state(), string(), binary()) -> {ok, state()} | {error, any()}.
% -----------------------------------------------------------------------------
store(Modules, AssetsDir, Tarball) ->
    try
        Module = read_metadata({binary, Tarball}), 
        [Release] = Module#module.releases,
        AbsFileName = AssetsDir ++ binary_to_list(Release#release.file),
        ok = file:write_file(AbsFileName, Tarball),
        NewModules = merge_into_modules(Module, Modules),
        {ok, NewModules}
    catch 
        Type:Reason ->  {error, {Type, Reason}}
    end.

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


% -----------------------------------------------------------------------------
-spec search(state(), [binary()]) -> [module_type()].
% -----------------------------------------------------------------------------
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


% -----------------------------------------------------------------------------
-spec author_and_module(binary()) -> {Author::binary(), Module::binary()}.
% -----------------------------------------------------------------------------
author_and_module(FullName) ->
    case re:run(FullName, <<"\\s*(.+)[\\-/](.+?)\\s*$">>, [{capture, all_but_first, binary}]) of
        {match, [Author, Name]} -> {Author, Name};
        _ -> throw({parse_error, FullName})
    end.

read_metadata(File) ->
    {ok, MetadataFileName} = find_metadata_file_in_tarball(File),
    {ok, [{_, BinaryJson}]} = erl_tar:extract(File, [memory, compressed, {files,[MetadataFileName]}]),
    {Decoded} = jiffy:decode(BinaryJson),
    FullName = proplists:get_value(<<"name">>, Decoded),
    {Author, Name} = author_and_module(FullName),
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
        desc = proplists:get_value(<<"summary">>, Decoded),
        project_url = proplists:get_value(<<"project_page">>, Decoded),
        releases = [#release{version=Version, file=FileName, dependencies=Dependencies}],
        tag_list = [] % TODO: not in metadata, should be added to the post request. or /cares.
    }.

find_metadata_file_in_tarball(File) ->
    {ok, FileNamesInTar} = erl_tar:table(File, [compressed]),
    lists:foldl(fun(CurrentFile, Res) -> 
        case re:run(CurrentFile, "^[^/]+/metadata.json$", [{capture, all, list}]) of
            {match, [NewRes]} -> {ok, NewRes};
            _ -> Res
        end
    end, {metadata_not_present, FileNamesInTar}, FileNamesInTar).


% -----------------------------------------------------------------------------
-spec serializable(module_type() | dict()) -> {[{atom(), any()}]}.
% -----------------------------------------------------------------------------
serializable(#module{} = Module) ->
    [First|Others] = Module#module.releases,
    Latest = lists:foldl(fun(R, Max) -> versions:max(R#release.version, Max) end, First#release.version, Others),
    {[
        {full_name, Module#module.full_name},
        {author, Module#module.author},
        {name, Module#module.name},
        {desc, Module#module.desc},
        {project_url, Module#module.project_url},
        {version, versions:to_binary(Latest) }, 
        {releases, [serializable_release(R) || R <- Module#module.releases]},
        {tag_list, []}
    ]};
serializable(Releases) ->
    FullNames = dict:fetch_keys(Releases),
    {[ 
        {FN, [ serializable_release(R) || R <- sets:to_list(dict:fetch(FN, Releases)) ]} || FN <- FullNames 
    ]}.    

serializable_release(Release) ->
    {[
        {version, versions:to_binary(Release#release.version)},
        {file, Release#release.file}, 
        {dependencies, [[element(1, D), versions:to_binary(element(2, D))] || D <- Release#release.dependencies ]}
    ]}.


% -----------------------------------------------------------------------------
-spec info(state()) -> {Modules::integer(), Releases::integer()}.
% -----------------------------------------------------------------------------
info(Modules) ->
    ReleasesOfModules = [ length(M#module.releases) || M <- Modules ],
    lists:foldl(fun(R,{ModulesCount, ReleasesCount}) ->
        {ModulesCount+1, ReleasesCount+R}
    end, {0,0}, ReleasesOfModules).
