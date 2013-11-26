-module(muppet_repository).

-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).

-export([find/1, search/1, assets_dir/0]).
-export([start_link/0]).
-define(FULL_NAME(Author, ModuleName), <<Author/binary, <<"/">>/binary, ModuleName/binary>>).
-define(FILE_NAME(Author, ModuleName, Version), <<Author/binary, <<"-">>/binary, ModuleName/binary, <<"-">>/binary, Version/binary, <<".tar.gz">>/binary >>).

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
    search_modules(Modules, Terms,[]).
search([Module|Rest], Terms, Matching) ->
    case binary:match(Module#module.full_name, Terms) of
        nomatch -> search(Rest, Terms, Matching);
        _ -> search(Rest, Terms, [Module|Matching])
    end;
search([], Terms, Matching) ->
    Matching.

% gen_server callbacks

init([]) ->
    filelib:ensure_dir(assets_dir()),
    {ok, [
        module(<<"Mario">>, <<"mario_module">>, <<"A mario module">>, <<"http://mario.example.com">>, [
            release(<<"0.0.1">>, <<"m/Mario/Mario-mario_module-0.0.1.tar.gz">>, [
                dependency(<<"Luigi">>, <<"0.0.1">>)
            ])
        ])
    ]}.

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
handle_call(status, From, State) ->
    {reply, State, State}.

handle_info(Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


%priv
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
    {[
        {full_name, Module#module.full_name},
        {author, Module#module.author},
        {name, Module#module.name},
        {desc, Module#module.desc},
        {project_url, Module#module.project_url},
        {releases, [serializable_release(R) || R <- Module#module.releases]}
    ]}.

serializable_release(Release) ->
    <<"something">>.


read_metadata(File) ->
    {ok, FileNamesInTar} = erl_tar:table(File, [compressed]),
    DirName = hd(FileNamesInTar),
    {ok, [{_, BinaryJson}]} = erl_tar:extract(File, [memory, compressed, {files,[DirName++"/metadata.json"]}]),
    {Decoded} = jiffy:decode(BinaryJson),
    Author = element(2, lists:keyfind(<<"author">>, 1, Decoded)),
    Name = element(2, lists:keyfind(<<"name">>, 1, Decoded)), % FIXME: name in metadata.json is the dash-separated fullname 
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
