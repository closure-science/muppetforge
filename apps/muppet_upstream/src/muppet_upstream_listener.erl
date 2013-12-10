-module(muppet_upstream_listener).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).
-export([set_observed/1]).

-record(state, { upstreams = [], upstream_agent_pid} ).

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
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_cast({set_observed, Upstreams, MirrorAgentPid}, State) ->
    close_all_upstreams(State#state.upstreams),
    NewUpstreams = lists:map(fun(Upstream) ->
        {ok, Pid} = muppet_upstream_listener_ws_client:start_link(Upstream),
        {Upstream, Pid}
    end, Upstreams),
    {noreply, #state{ upstreams=NewUpstreams, upstream_agent_pid=MirrorAgentPid }};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_info({new_release, FromUpstream, Author, Module, Version, TarballPath}, State) ->
    State#state.upstream_agent_pid ! {new_release, FromUpstream, Author, Module, versions:version(Version), TarballPath},
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, State) ->
    close_all_upstreams(State#state.upstreams),
    ok.

close_all_upstreams(Upstreams) ->
    lists:foreach(fun({_Upstream, ClientPid}) -> muppet_upstream_listener_ws_client:close(ClientPid) end, Upstreams).
