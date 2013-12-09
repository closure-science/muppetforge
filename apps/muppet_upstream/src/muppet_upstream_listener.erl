-module(muppet_upstream_listener).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).
-export([set_observed/1]).

-record(state, { upstreams = [], mirror_agent_pid} ). %TODO: keep record of borked ones too? 

% -----------------------------------------------------------------------------
-spec start_link() -> {ok,pid()} | ignore | {error, {already_started, pid()} | term()}.
% -----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% -----------------------------------------------------------------------------
-spec set_observed([string()]) -> ok.
% -----------------------------------------------------------------------------
set_observed(Upstreams) ->
    gen_server:cast(?MODULE, {set_observed, Upstreams, self()}).

init([]) ->
    {ok, #state{}}.

handle_cast({set_observed, Upstreams, MirrorAgentPid}, State) ->
    close_all_upstreams(State#state.upstreams),
    NewUpstreams = lists:map(fun(Upstream) ->
        {ok, Pid} = open_ws_connection(Upstream),
        {Upstream, Pid}
    end, Upstreams),
    {noreply, #state{ upstreams=NewUpstreams, mirror_agent_pid=MirrorAgentPid }};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_info({gun_ws, FromPid, {text, Msg}}, State) ->
    io:format("received~n"),
    {ModuleInfo} = jiffy:decode(Msg),
    [FromUpstream] = lists:filter(fun({_Upstream, Pid}) -> Pid =:= FromPid end, State#state.upstreams),
    {author, Author} = proplists:lookup(author, ModuleInfo),
    {module, Module} = proplists:lookup(module, ModuleInfo),
    {version, Version} = proplists:lookup(version, ModuleInfo),
    {path, TarballPath} = proplists:lookup(path, ModuleInfo),
    State#state.mirror_agent_pid ! {new_release, FromUpstream, Author, Module, Version, TarballPath},
    {noreply, State};
handle_info({gun_error, _Pid, Reason}, State) ->
    io:format("gun error~p~n",[Reason]),
    {noreply, State};
handle_info({gun_ws_upgrade, _Pid, ok}, State) ->
    io:format("upgraded~n"),
    {noreply, State};
handle_info({gun_ws_upgrade, _Pid, error, _IsFin, Status, Headers}, State) ->
    io:format("upgrade error~n"),
    {noreply, State};
handle_info(Msg, State) ->
    io:format("unexpected msg~p~n", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, State) ->
    close_all_upstreams(State#state.upstreams),
    ok.

close_all_upstreams(Upstreams) ->
    lists:foreach(fun({_Upstream, GunPid}) -> gun:shutdown(GunPid) end, Upstreams).
    
open_ws_connection(Upstream) when is_binary(Upstream) ->
    open_ws_connection(binary_to_list(Upstream));
open_ws_connection(Upstream) ->
    {ok, {_Scheme, _User, Host, Port, Path, _QueryString}} = http_uri:parse(Upstream),
    {ok, GunPid} = gun:open(Host, Port, [{type, tcp}]),
    ObserveEndpoint = Path ++ "/api/listen",
    io:format("endpoint:~p ~p ~p~n", [Host, Port, ObserveEndpoint]),
    ok = gun:ws_upgrade(GunPid, ObserveEndpoint),
    {ok, GunPid}.
