-module(muppet_mirror_agent).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).
-export([store_upstream/1, fetch_upstream/0]).
-export([store_blacklist/1, fetch_blacklist/0]).
-export([reset_errors/0, fetch_errors/0]).

-define(TICK_INTERVAL_MILLIS, 10000).
-define(REFRESH_INTERVAL_MICROS, 60 * 60* 1000* 1000 * 1000).
-record(state, { retards = [], upstream = dict:new(), tbd = dict:new(), errors = dict:new() }).

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
    self() ! tick,
    {ok, #state{}}.

handle_cast({store_upstream, BaseUrls}, State) ->
    NewUpstream = dict:from_list([{BaseUrl, {0,0,0}} || BaseUrl <- BaseUrls]),
    {noreply, State#state{ upstream = NewUpstream, tbd = dict:new(), errors = dict:new() }};
handle_cast({store_blacklist, Retards}, State) ->
    {noreply, State#state{ retards=Retards }};
handle_cast(reset_errors, State) ->
    {noreply, State#state{ errors=dict:new() }};
handle_cast(Req, State) ->
    {noreply, State}.

handle_call(fetch_blacklist, From, State) ->
    {reply, State#state.retards, State};
handle_call(fetch_upstream, From, State) ->
    {reply, State#state.upstream, State};
handle_call(fetch_errors, From, State) ->
    {reply, State#state.errors, State};
handle_call(Req, From, State) ->
    {reply, ok, State}.

handle_info(tick, State) ->
    {Now, UpstreamBaseUrls} = upstream_to_be_refreshed(State),
    NewState = case UpstreamBaseUrls of
        [UpstreamBaseUrl| _] -> 
            self() ! tick,
            refresh_upstream(State, Now, UpstreamBaseUrl);
        [] -> 
            case dict:fetch_keys(State#state.tbd) of
                [] ->  
                    timer:send_after(?TICK_INTERVAL_MILLIS, tick),
                    State;
                [Coords|_] -> 
                    self() ! tick,
                    fetch_and_store_tarball(State, Now, Coords)
            end
    end,
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, State) ->
    ok.

refresh_upstream(State, Now, UpstreamBaseUrl) ->
    {ok, {{_, 200, _}, Headers, Body}} = httpc:request(UpstreamBaseUrl ++ "/modules.json"),
    DecodedModules = jiffy:decode(Body),
    VersionsFromUpstream = lists:flatmap(fun({M}) ->
        FullName = proplists:get_value(<<"full_name">>, M),
        Releases = proplists:get_value(<<"releases">>, M),
        [{UpstreamBaseUrl, {FullName, proplists:get_value(<<"version">>, R) }} || {R} <- Releases]
    end, DecodedModules),
    NewTbd = lists:foldl(fun({BaseUrl, {FullName, Version} = Coords}, Accum) ->
        dict:store(Coords, BaseUrl, Accum)
    end, State#state.tbd, VersionsFromUpstream),
    State#state{ upstream = dict:store(UpstreamBaseUrl, Now, State#state.upstream), tbd=NewTbd }.

fetch_and_store_tarball(State, Now, {FullName, Version} = Coords) ->
    BaseUrl = dict:fetch(Coords, State#state.tbd),
    TarballBinary = fetch_tarball_binary(BaseUrl, FullName, Version),
    {Outcome, Result} = muppet_repository:store(TarballBinary),
    NewErrors = case Outcome of
        ok -> State#state.errors;
        error -> dict:store({BaseUrl, FullName, Version}, {Now, Result}, State#state.errors)
    end,
    NewTbd = dict:erase(Coords, State#state.tbd),
    State#state{ tbd = NewTbd, errors = NewErrors }.

upstream_to_be_refreshed(State) ->
    Now = now(),
    Expired = dict:filter(fun(_UpstreamUrl, RefreshedAt) ->
        timer:now_diff(Now, RefreshedAt) > ?REFRESH_INTERVAL_MICROS
    end, State#state.upstream),
    {Now, dict:fetch_keys(Expired) }.

fetch_tarball_binary(BaseUrl, FullName, Version) ->
    Url = BaseUrl ++ "/api/v1/releases.json?module="++binary_to_list(FullName)++"&version="++binary_to_list(Version),
    io:format("fetching ~p~n", [Url]),
    {ok, {{_, 200, _}, _, RelBody}} = httpc:request(Url),
    {DecodedBody} = jiffy:decode(RelBody),
    [Release] = proplists:get_value(FullName, DecodedBody),
    RemoteFileName = pluck_file_(Release),
    {ok, {{_, 200, _}, _, TarBody}} = httpc:request(get, {BaseUrl ++ RemoteFileName, []}, [], [{body_format, binary}]),
    TarBody.

pluck_file_({Rel}) ->
    binary_to_list(proplists:get_value(<<"file">>, Rel)).
