-module(muppet_upstream).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).
-export([info/0]).
-export([store_upstream/1, fetch_upstream/0]).
-export([store_blacklist/1, fetch_blacklist/0, serializable_blacklist/1, proplists_to_blacklist/1]).
-export([reset_errors/0, fetch_errors/0, serializable_errors/1]).

-define(TICK_INTERVAL_MILLIS, 10000).
-define(REFRESH_INTERVAL_MICROS, 60 * 60* 1000* 1000 * 1000).
-record(state, { pids=sets:new(), retards =[], upstream = dict:new(), tbd = dict:new(), errors = dict:new() }).
-type blacklist_entry() :: {binary() | null, binary() | null, binary() | null, binary() | null}.

% -----------------------------------------------------------------------------
-spec start_link() -> {ok,pid()} | ignore | {error, {already_started, pid()} | term()}.
% -----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


% -----------------------------------------------------------------------------
-spec store_upstream([{binary(), boolean()}]) -> ok.
% -----------------------------------------------------------------------------
store_upstream(UpstreamDefinitions) ->
    gen_server:cast(?MODULE, {store_upstream, UpstreamDefinitions}).


% -----------------------------------------------------------------------------
-spec fetch_upstream() -> dict().
% -----------------------------------------------------------------------------
fetch_upstream() ->
    gen_server:call(?MODULE, fetch_upstream).


% -----------------------------------------------------------------------------
-spec store_blacklist([blacklist_entry()]) -> ok.
% -----------------------------------------------------------------------------
store_blacklist(Retards) ->
    gen_server:cast(?MODULE, {store_blacklist, Retards}).


% -----------------------------------------------------------------------------
-spec fetch_blacklist() -> [blacklist_entry()].
% -----------------------------------------------------------------------------
fetch_blacklist() ->
    gen_server:call(?MODULE, fetch_blacklist).


% -----------------------------------------------------------------------------
-spec reset_errors() -> ok.
% -----------------------------------------------------------------------------
reset_errors() ->
    gen_server:cast(?MODULE, reset_errors).


% -----------------------------------------------------------------------------
-spec fetch_errors() -> dict().
% -----------------------------------------------------------------------------
fetch_errors() ->
    gen_server:call(?MODULE, fetch_errors).


% -----------------------------------------------------------------------------
-spec info() -> [{atom(), any()}].
% -----------------------------------------------------------------------------
info() ->
    gen_server:call(?MODULE, info).



init([]) ->
    StorageFileName = filename:join(code:priv_dir(?MODULE), "storage"),
    filelib:ensure_dir(StorageFileName),
    {ok, storage} = dets:open_file(storage, [{file,  StorageFileName}]),
    Retards = dets_value(storage, retards, []),
    UpstreamDefinitions = dets_value(storage, upstream, []),
    NewUpstreams = upstream_from_definitions(UpstreamDefinitions),
    ok = reset_listeners(NewUpstreams),
    process_flag(trap_exit, true),
    self() ! tick,
    {ok, #state{upstream = NewUpstreams, retards = Retards}}.

handle_cast({store_upstream, UpstreamDefinitions}, State) ->
    NewUpstream = upstream_from_definitions(UpstreamDefinitions),
    ok = dets:insert(storage, {upstream, UpstreamDefinitions}),
    ok = dets:sync(storage),
    reset_listeners(NewUpstream),
    {noreply, State#state{ upstream = NewUpstream, tbd = dict:new(), errors = dict:new() }};
handle_cast({store_blacklist, Retards}, State) ->
    ok = dets:insert(storage, {retards, Retards}),
    ok = dets:sync(storage),
    {noreply, State#state{ retards=Retards }};
handle_cast(reset_errors, State) ->
    {noreply, State#state{ errors=dict:new() }};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_call(fetch_blacklist, _From, State) ->
    {reply, State#state.retards, State};
handle_call(fetch_upstream, _From, State) ->
    {reply, State#state.upstream, State};
handle_call(fetch_errors, _From, State) ->
    {reply, State#state.errors, State};
handle_call(info, From, State) ->
    spawn(fun() ->
        gen_server:reply(From, [
            {errors, dict:size(State#state.errors)},
            {retards, length(State#state.retards)},
            {upstream, dict:size(State#state.upstream)},
            {tbd, dict:size(State#state.tbd)}
        ])
    end),
    {noreply, State};
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_info(tick, State) ->
    ThisPid = self(),
    {Now, ExpiredUpstream} = upstream_to_be_refreshed(State),
    Pids = case ExpiredUpstream of
        [{UpstreamBaseUrl, UseFastChangeNotification}| _] -> 
            Pid = spawn_link(fun() -> refresh_upstream(ThisPid, Now, UpstreamBaseUrl, UseFastChangeNotification) end),
            [Pid];
        [] -> 
            case dict:fetch_keys(State#state.tbd) of
                [] ->  
                    timer:send_after(?TICK_INTERVAL_MILLIS, tick),
                    [];
                [{AuthorAndModule, Version} = Coords|_] -> 
                    BaseUrl = dict:fetch(Coords, State#state.tbd),
                    Pid = spawn_link(fun() -> fetch_and_store_tarball(ThisPid, Now, BaseUrl, AuthorAndModule, Version) end),
                    [Pid]
            end
    end,
    {noreply, State#state{ pids = sets:union(sets:from_list(Pids), State#state.pids)}};

handle_info({upstream_metadata, UseFastChangeNotification, At, UpstreamBaseUrl, VersionsFromUpstream}, State) ->
    NewTbd = new_tbd(VersionsFromUpstream, State),
    NewUpstream = dict:store(UpstreamBaseUrl, {UseFastChangeNotification, At}, State#state.upstream),
    {noreply, State#state{ upstream = NewUpstream, tbd=NewTbd }};

handle_info({upstream_failed, UseFastChangeNotification, At, UpstreamBaseUrl, _ErrorType, _Reason}, State) ->
    NewUpstream = dict:store(UpstreamBaseUrl, {UseFastChangeNotification, At}, State#state.upstream),
    {noreply, State#state{ upstream = NewUpstream }};

handle_info({tarball_done, _At, _BaseUrl, AuthorAndModule, Version}, State) ->
    NewTbd = dict:erase({AuthorAndModule, Version}, State#state.tbd),
    {noreply, State#state{ tbd = NewTbd}};

handle_info({tarball_failed, At, BaseUrl, AuthorAndModule, Version, ErrorType, Reason, StackTrace}, State) ->
    NewTbd = dict:erase({AuthorAndModule, Version}, State#state.tbd),
    NewErrors = dict:store({BaseUrl, AuthorAndModule, Version}, {At, {ErrorType, Reason, StackTrace}}, State#state.errors),
    {noreply, State#state{ tbd = NewTbd, errors = NewErrors}};

handle_info({new_release, FromUpstream, Author, Module, Version, _TarballPath}, State) ->
    NewTbd = new_tbd([{FromUpstream, {{Author, Module}, Version}}], State),
    {noreply, State#state{ tbd = NewTbd}};

handle_info({'EXIT', Pid, _Reason}, State) ->
    NewPids = case sets:is_element(Pid, State#state.pids) of
        false -> State#state.pids;
        true ->
            self() ! tick,
            sets:del_element(Pid, State#state.pids)
    end,
    {noreply, State#state{ pids = NewPids }};

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    dets:close(storage),
    ok.

upstream_from_definitions(UpstreamDefinitions) ->
    dict:from_list([{BaseUrl, {UseFastChangeNotification, {0,0,0}}} || {BaseUrl, UseFastChangeNotification} <- UpstreamDefinitions]).

dets_value(Name, Key, Default) ->
    case dets:lookup(Name, Key) of
        [{Key, Value}] -> Value;
        _ -> Default
    end.

allowed([], {_BaseUrl, _Author, _Module, _Version}) ->
    true;    
allowed([Bl|Cdr], El) ->
    case blacklisted(Bl, El) of 
        true -> true;
        false -> allowed(Cdr, El)
    end.
blacklisted({BlBaseUrl, BlAuthor, BlModule, BlVersion}, {BaseUrl, Author, Module, Version}) ->
    bl(BlBaseUrl, BaseUrl) andalso bl(BlAuthor, Author) andalso bl(BlModule, Module) andalso bl(BlVersion, Version).
bl(null, _El) -> true;        
bl(BlEl, El) -> BlEl =:= El.

refresh_upstream(Parent, Now, UpstreamBaseUrl, UseFastChangeNotification) ->
    try
        {ok, {{_, 200, _}, _Headers, Body}} = httpc:request(binary_to_list(UpstreamBaseUrl) ++ "/modules.json"),
        DecodedModules = jiffy:decode(Body),
        VersionsFromUpstream = lists:flatmap(fun({M}) ->
            AuthorAndModule = muppet_driver:author_and_module(proplists:get_value(<<"full_name">>, M)),
            Releases = proplists:get_value(<<"releases">>, M),
            [{UpstreamBaseUrl, {AuthorAndModule, versions:version(proplists:get_value(<<"version">>, R)) }} || {R} <- Releases]
        end, DecodedModules),
        Parent ! {upstream_metadata, UseFastChangeNotification, Now, UpstreamBaseUrl, VersionsFromUpstream }
    catch 
        T:R -> Parent ! {upstream_failed, UseFastChangeNotification, Now, UpstreamBaseUrl, T, R}
    end.

reset_listeners(Upstreams) ->
    UpstreamsWithFastChangeNotification = dict:fold(fun
        (_BaseUrl, {false, _At}, Acc) -> Acc;
        (BaseUrl, {true, _At}, Acc) -> [BaseUrl|Acc]
    end,[] , Upstreams),
    muppet_upstream_fcn:reset_listeners(UpstreamsWithFastChangeNotification).


new_tbd(VersionsFromUpstream, State) ->
    Unknown = lists:filter(fun({_BaseUrl, {{Author, Module}, Version}}) ->
        not muppet_repository:knows(Author, Module, Version)
    end, VersionsFromUpstream),
    NotBlacklisted = lists:filter(fun({BaseUrl, {{Author, Module}, Version}}) ->
        allowed(State#state.retards, {BaseUrl, Author, Module, Version})
    end, Unknown),
    lists:foldl(fun({BaseUrl, Coords}, Accum) ->
        dict:store(Coords, BaseUrl, Accum)
    end, State#state.tbd, NotBlacklisted).

fetch_and_store_tarball(Parent, Now, BaseUrl, AuthorAndModule, Version) ->
    try
        TarballBinary = fetch_tarball_binary(BaseUrl, AuthorAndModule, Version),
        {ok, _ReleaseCoords} = muppet_repository:store(TarballBinary),
        Parent ! {tarball_done, Now, BaseUrl, AuthorAndModule, Version}
    catch
        T:R -> Parent ! {tarball_failed, Now, BaseUrl, AuthorAndModule, Version, T, R, erlang:get_stacktrace() }
    end.

upstream_to_be_refreshed(State) ->
    Now = now(),
    Expired = dict:filter(fun(_UpstreamUrl, {_UseFastChangeNotification, RefreshedAt}) ->
        timer:now_diff(Now, RefreshedAt) > ?REFRESH_INTERVAL_MICROS
    end, State#state.upstream),
    {Now, lists:map(fun({Url, {UseFastChangeNotification, _At}}) -> 
        {Url, UseFastChangeNotification} 
    end, dict:to_list(Expired))}.

fetch_tarball_binary(BaseUrl, {Author, Module}, Version) ->
    FullName = binary_to_list(Author) ++ "/" ++ binary_to_list(Module),
    Url = binary_to_list(BaseUrl) ++ "/api/v1/releases.json?module="++FullName++"&version="++binary_to_list(versions:to_binary(Version)),
    {ok, {{_, 200, _}, _, RelBody}} = httpc:request(Url),
    {DecodedBody} = jiffy:decode(RelBody),
    Release = case proplists:get_value(list_to_binary(FullName), DecodedBody) of
        [{R}] -> R;
        _ -> throw({no_release, BaseUrl, Author, Module, Version})
    end,
    RemoteFileName = proplists:get_value(<<"file">>, Release),
    {ok, {{_, 200, _}, _, TarBody}} = httpc:request(get, {binary_to_list(BaseUrl) ++ binary_to_list(RemoteFileName), []}, [], [{body_format, binary}]),
    TarBody.

proplists_to_blacklist(Pls) ->
    [proplist_to_blacklist_entry(Pl) || {Pl} <- Pls].
proplist_to_blacklist_entry(Pl) ->
    Upstream = proplists:get_value(<<"upstream">>, Pl),
    Author = proplists:get_value(<<"author">>, Pl),
    Module = proplists:get_value(<<"module">>, Pl),
    Version = proplists:get_value(<<"version">>, Pl),
    {Upstream, Author, Module, Version}.

serializable_blacklist(Bl) ->
    [serializable_blacklist_entry(B) || B <- Bl].
    
serializable_blacklist_entry({UpstreamBaseUrl, Author, Module, Version}) ->
    {[
        {upstream, UpstreamBaseUrl},
        {author, Author},
        {module, Module},
        {version, Version}
    ]}.

serializable_errors(Errors) ->
    lists:map(fun serializable_error/1, dict:to_list(Errors)).

serializable_error({{BaseUrl, {Author, Module}, Version}, {Now, {Type, Error, StackTrace}}}) ->
    {[
        {base_url, BaseUrl},
        {author, Author}, 
        {module, Module},
        {version, versions:to_binary(Version)},
        {at, timer:now_diff(Now, {0,0,0}) div 1000},
        {error_type, Type},
        {error, list_to_binary(lists:flatten(io_lib:format("~p", [Error]))) },
        {stack_trace, lists:map(fun stack_item/1, StackTrace) }
    ]}.

stack_item({Module, FunctionName, Args, FileNameAndLine}) when is_list(Args) ->
    stack_item({Module, FunctionName, length(Args), FileNameAndLine});
stack_item({Module, FunctionName, Arity, FileNameAndLine}) ->
    {[
        {module, Module},
        {fn, FunctionName},
        {arity,  Arity},
        {file, list_to_binary(proplists:get_value(file, FileNameAndLine, "builtin"))},
        {line, proplists:get_value(line, FileNameAndLine, 0) }
    ]}.