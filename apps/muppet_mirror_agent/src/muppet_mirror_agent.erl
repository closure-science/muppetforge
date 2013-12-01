-module(muppet_mirror_agent).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).
-export([store_upstream/1, fetch_upstream/0]).
-export([store_blacklist/1, fetch_blacklist/0]).
-export([reset_errors/0, fetch_errors/0, serializable_errors/1]).

-define(TICK_INTERVAL_MILLIS, 10000).
-define(REFRESH_INTERVAL_MICROS, 60 * 60* 1000* 1000 * 1000).
-record(state, { retards = [], upstream = dict:new(), tbd = dict:new(), errors = dict:new() }).

-compile(export_all).
% -----------------------------------------------------------------------------
-spec start_link() -> {ok,pid()} | ignore | {error, {already_started, pid()} | term()}.
% -----------------------------------------------------------------------------
start_link() ->
    process_flag(trap_exit, true),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------
store_upstream(BaseUrls) ->
    gen_server:cast(?MODULE, {store_upstream, BaseUrls}).
% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------
fetch_upstream() ->
    gen_server:call(?MODULE, fetch_upstream).

% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------
store_blacklist(Retards) ->
    gen_server:cast(?MODULE, {store_blacklist, Retards}).
% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------
fetch_blacklist() ->
    gen_server:call(?MODULE, fetch_blacklist).


% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------
reset_errors() ->
    gen_server:cast(?MODULE, reset_errors).
% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------
fetch_errors() ->
    gen_server:call(?MODULE, fetch_errors).


init([]) ->
    process_flag(trap_exit, true),
    self() ! tick,
    {ok, #state{}}.

handle_cast({store_upstream, BaseUrls}, State) ->
    NewUpstream = dict:from_list([{BaseUrl, {0,0,0}} || BaseUrl <- BaseUrls]),
    {noreply, State#state{ upstream = NewUpstream, tbd = dict:new(), errors = dict:new() }};
handle_cast({store_blacklist, Retards}, State) ->
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
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_info(tick, State) ->
    ThisPid = self(),
    {Now, UpstreamBaseUrls} = upstream_to_be_refreshed(State),
    case UpstreamBaseUrls of
        [UpstreamBaseUrl| _] -> 
            spawn_link(fun() -> refresh_upstream(ThisPid, Now, UpstreamBaseUrl) end);
        [] -> 
            case dict:fetch_keys(State#state.tbd) of
                [] ->  timer:send_after(?TICK_INTERVAL_MILLIS, tick);
                [{FullName, Version} = Coords|_] -> 
                    BaseUrl = dict:fetch(Coords, State#state.tbd),
                    spawn_link(fun() -> fetch_and_store_tarball(ThisPid, Now, BaseUrl, FullName, Version) end)
            end
    end,
    {noreply, State};

handle_info({upstream_metadata, At, UpstreamBaseUrl, VersionsFromUpstream}, State) ->
    NewTbd = lists:foldl(fun({BaseUrl, Coords}, Accum) ->
        dict:store(Coords, BaseUrl, Accum)
    end, State#state.tbd, VersionsFromUpstream),
    NewUpstream = dict:store(UpstreamBaseUrl, At, State#state.upstream),
    {noreply, State#state{ upstream = NewUpstream, tbd=NewTbd }};

handle_info({upstream_failed, _At, ErrorType, Reason}, State) ->
    {noreply, State};

handle_info({tarball_done, _At, _BaseUrl, FullName, Version}, State) ->
    NewTbd = dict:erase({FullName, Version}, State#state.tbd),
    {noreply, State#state{ tbd = NewTbd}};

handle_info({tarball_failed, At, BaseUrl, FullName, Version, ErrorType, Reason}, State) ->
    NewTbd = dict:erase({FullName, Version}, State#state.tbd),
    NewErrors = dict:store({BaseUrl, FullName, Version}, {At, {ErrorType, Reason}}, State#state.errors),
    {noreply, State#state{ tbd = NewTbd, errors = NewErrors}};

handle_info({'EXIT', _Pid, _Reason}, State) ->
    self() ! tick,
    {noreply, State};

handle_info(Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

refresh_upstream(Parent, Now, UpstreamBaseUrl) ->
    try
        {ok, {{_, 200, _}, Headers, Body}} = httpc:request(binary_to_list(UpstreamBaseUrl) ++ "/modules.json"),
        DecodedModules = jiffy:decode(Body),
        VersionsFromUpstream = lists:flatmap(fun({M}) ->
            FullName = proplists:get_value(<<"full_name">>, M),
            Releases = proplists:get_value(<<"releases">>, M),
            [{UpstreamBaseUrl, {FullName, proplists:get_value(<<"version">>, R) }} || {R} <- Releases]
        end, DecodedModules),
        Parent ! {upstream_metadata, Now, UpstreamBaseUrl, VersionsFromUpstream }
    catch 
        T:R -> Parent ! {upstream_failed, Now, T, R}
    end.

fetch_and_store_tarball(Parent, Now, BaseUrl, FullName, Version) ->
    try
        TarballBinary = fetch_tarball_binary(BaseUrl, FullName, Version),
        ok = muppet_repository:store(TarballBinary),
        Parent ! {tarball_done, Now, BaseUrl, FullName, Version}
    catch
        T:R -> Parent ! {tarball_failed, Now, BaseUrl, FullName, Version, T, R}
    end.

upstream_to_be_refreshed(State) ->
    Now = now(),
    Expired = dict:filter(fun(_UpstreamUrl, RefreshedAt) ->
        timer:now_diff(Now, RefreshedAt) > ?REFRESH_INTERVAL_MICROS
    end, State#state.upstream),
    {Now, dict:fetch_keys(Expired) }.

fetch_tarball_binary(BaseUrl, FullName, Version) ->
    Url = binary_to_list(BaseUrl) ++ "/api/v1/releases.json?module="++binary_to_list(FullName)++"&version="++binary_to_list(Version),
    {ok, {{_, 200, _}, _, RelBody}} = httpc:request(Url),
    {DecodedBody} = jiffy:decode(RelBody),
    [{Release}] = proplists:get_value(FullName, DecodedBody),
    RemoteFileName = proplists:get_value(<<"file">>, Release),
    {ok, {{_, 200, _}, _, TarBody}} = httpc:request(get, {binary_to_list(BaseUrl) ++ binary_to_list(RemoteFileName), []}, [], [{body_format, binary}]),
    TarBody.

serializable_errors(Errors) ->
    lists:map(fun serializable_error/1, dict:to_list(Errors)).

serializable_error({{BaseUrl, FullName, Version}, {Now, {Type, Error}}}) ->
    {[
        {base_url, BaseUrl},
        {full_name, FullName},
        {version, Version},
        {at, timer:now_diff(Now, {0,0,0}) div 1000},
        {error_type, Type},
        {error, list_to_binary(lists:flatten(io_lib:format("~p", [Error]))) }
    ]}.
