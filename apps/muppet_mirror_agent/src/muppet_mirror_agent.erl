-module(muppet_mirror_agent).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).
-export([set_upstream/1, set_retards/1, get_upstream/0, get_retards/0]).
-define(TICK_INTERVAL, 100).
-define(REFRESH_INTERVAL_MICROS, 60 * 60* 1000* 1000 * 1000).
-record(state, { retards = [], upstream = dict:new(), tbd = dict:new() }).

% -----------------------------------------------------------------------------
-spec start_link() -> {ok,pid()} | ignore | {error, {already_started, pid()} | term()}.
% -----------------------------------------------------------------------------
start_link() ->
    process_flag(trap_exit, true),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------
set_upstream(BaseUrls) ->
    gen_server:cast(?MODULE, {set_upstream, BaseUrls}).

get_upstream() ->
    gen_server:call(?MODULE, get_upstream).

% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------
set_retards(Retards) ->
    gen_server:cast(?MODULE, {set_retards, Retards}).
get_retards() ->
    gen_server:call(?MODULE, get_retards).


init([]) ->
    self() ! tick,
    {ok, #state{}}.

handle_cast({set_upstream, BaseUrls}, State) ->
    NewUpstream = dict:from_list([{BaseUrl, {0,0,0}} || BaseUrl <- BaseUrls]),
    {noreply, State#state{ upstream = NewUpstream, tbd = [] }};
handle_cast({set_retards, Retards}, State) ->
    {noreply, State#state{ retards=Retards }};
handle_cast(Req, State) ->
    {noreply, State}.

handle_call(get_retards, From, State) ->
    {reply, State#state.retards, State};
handle_call(get_upstream, From, State) ->
    {reply, State#state.upstream, State};
handle_call(Req, From, State) ->
    {reply, ok, State}.

handle_info(tick, State) ->
    {ShouldSleep, NewState} = do_something(State),
    case ShouldSleep of 
        true  -> timer:send_after(?TICK_INTERVAL, tick);
        false -> self() ! tick
    end,
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, State) ->
    ok.


do_something(State) ->
    case upstream_needing_refresh(State) of
        {_, []} ->
            case dict:fetch_keys(State#state.tbd) of
                [] -> 
                    {true, State};
                [{FullName, Version} = Coords |_] ->
                    BaseUrl = dict:fetch(Coords, State#state.tbd),
                    TarballBinary = fetch_tarball_binary(BaseUrl, FullName, Version),
                    {Outcome, Result} = muppet_repository:store(TarballBinary),
                    if 
                        Outcome =/= ok -> io:format("got an error ~p~n", [Result]);
                        true -> ok
                    end,
                    NewTbd = dict:erase(Coords, State#state.tbd),
                    {false, State#state{ tbd = NewTbd }}
            end;
        {Now, [UpstreamUrl| _]} ->
            VersionsFromUpstream = fetch_versions(UpstreamUrl),
            NewTbd = lists:foldl(fun({BaseUrl, {FullName, Version} = Coords}, Accum) ->
                dict:store(Coords, BaseUrl, Accum)
            end, State#state.tbd, VersionsFromUpstream),
            {false, State#state{ upstream = dict:store(UpstreamUrl, Now, State#state.upstream), tbd=NewTbd }}
    end.


upstream_needing_refresh(State) ->
    Now = now(),
    Expired = dict:filter(fun(_UpstreamUrl, RefreshedAt) ->
        timer:now_diff(Now, RefreshedAt) > ?REFRESH_INTERVAL_MICROS
    end, State#state.upstream),
    {Now, dict:fetch_keys(Expired) }.



fetch_versions(BaseUrl) ->
    {ok, {{_, 200, _}, Headers, Body}} = httpc:request(BaseUrl ++ "/modules.json"),
    DecodedModules = jiffy:decode(Body),
    ToBeFetched = lists:flatmap(fun({M}) ->
        FullName = proplists:get_value(<<"full_name">>, M),
        Releases = proplists:get_value(<<"releases">>, M),
        [{BaseUrl, {FullName, proplists:get_value(<<"version">>, R) }} || {R} <- Releases]
    end, DecodedModules).

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
