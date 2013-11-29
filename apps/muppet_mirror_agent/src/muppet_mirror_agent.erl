-module(muppet_mirror_agent).

-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).
-define(TICK_INTERVAL, 100).
-define(TICK_INTERVAL_AFTER_SYNC, 30000).

-spec start_link() -> {ok,pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link() ->
    process_flag(trap_exit, true),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-record(mirror_state, { url, refreshed_at = 0}).
-record(state, { poll_for = [], tbd = [] }).
                                      
init([]) ->
    self() ! trigger,
    {ok, #state{}}.

handle_cast({poll_for, Url}, State) ->
    MirrorState = #mirror_state{url = Url},
    {noreply, State#state{ poll_for = [MirrorState|State#state.poll_for] }};
handle_cast(Req, State) ->
    {noreply, State}.

handle_call(Req, From, State) ->
    {reply, ok, State}.

handle_info(tick, State) ->
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, State) ->
    ok.

% mirror(Modules, AssetsDir, BaseUrl) ->
%     {ok, {{_, 200, _}, Headers, Body}} = httpc:request(BaseUrl ++ "/modules.json"),
%     DecodedModules = jiffy:decode(Body),
%     ToBeFetched = lists:map(fun({M}) ->
%         proplists:get_value(<<"full_name">>, M)
%     end, DecodedModules),
%     lists:foldl(fun(FullName, Acc) -> 
%         io:format("[+] fetching ~p~n", [FullName]),
%         {ok, {{_, 200, _}, _, RelBody}} = httpc:request(BaseUrl ++ "/api/v1/releases.json?module="++ binary_to_list(FullName)),
%         {DecodedBody} = jiffy:decode(RelBody),
%         Releases = proplists:get_value(FullName, DecodedBody),
%         RemoteFileNames = [pluck_file_(R) || R <- Releases],
%         lists:foldl(fun(RemoteFileName, InnerAcc) ->
%             try 
%                 io:format("   [-] fetching tarball: ~p~n", [RemoteFileName]),
%                 {ok, {{_, 200, _}, _, TarBody}} = httpc:request(get, {BaseUrl ++ RemoteFileName, []}, [], [{body_format, binary}]),
%                 store(InnerAcc, AssetsDir, TarBody)
%             catch
%                 Ex:Reason -> 
%                     io:format("skipped ~p, malformed: ~p~p~n", [RemoteFileName, Ex, Reason]),
%                     InnerAcc
%             end
%         end, Acc, RemoteFileNames)
%     end, Modules, ToBeFetched).

% pluck_file_({Rel}) ->
%     binary_to_list(proplists:get_value(<<"file">>, Rel)).
