-module(muppet_upstream_observer).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).
-export([set_observed/1]).
-define(TICK_INTERVAL_MILLIS, 20000).
-record(state, { observers = [], failed = [], timer} ).

% -----------------------------------------------------------------------------
-spec start_link() -> {ok,pid()} | ignore | {error, {already_started, pid()} | term()}.
% -----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% -----------------------------------------------------------------------------
-spec set_observed([string()]) -> ok.
% -----------------------------------------------------------------------------
set_observed(UpstreamBaseUrls) ->
    gen_server:cast(?MODULE, {set_observed, UpstreamBaseUrls}).

init([]) ->
    process_flag(trap_exit, true),
    self() ! tick,
    {ok, #state{}}.

handle_cast({set_observed, UpstreamBaseUrls}, State) ->
    close_all_upstreams(State#state.observers),
    NewState = start_observing(UpstreamBaseUrls),
    {noreply, NewState};

handle_cast(_Req, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, {error, badrequest}, State}.

handle_info(tick, State) ->
    NewState = start_observing(State#state.failed),
    {ok, Timer} = timer:send_after(?TICK_INTERVAL_MILLIS, tick),
    {noreply, #state{ 
        observers = State#state.observers ++ NewState#state.observers, 
        failed = NewState#state.failed,
        timer = Timer
    }};

handle_info({'EXIT', Pid, _Reason}, State) ->
    NewState = case lists:keyfind(Pid, 2, State#state.observers) of
        false -> State;
        {BaseUrl, _} ->
            #state{ 
                observers = lists:keydelete(Pid, 2, State#state.observers), 
                failed = [BaseUrl | State#state.failed] 
            }
    end,
    timer:cancel(State#state.timer),
    self() ! tick,
    {noreply, NewState};

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, State) ->
    close_all_upstreams(State#state.observers).

close_all_upstreams(Observers) ->
    lists:foreach(fun({_UpstreamBaseUrl, ClientPid}) -> muppet_upstream_observer_ws_client:close(ClientPid) end, Observers).

start_observing(UpstreamBaseUrls) ->
    lists:foldl(fun(UpstreamBaseUrl, State) ->
        case muppet_upstream_observer_ws_client:start_link(UpstreamBaseUrl) of 
            {ok, Pid} -> State#state{ observers= [{UpstreamBaseUrl, Pid}|State#state.observers]};
            _ -> State#state{ failed = [UpstreamBaseUrl|State#state.failed]}
        end
    end, #state{}, UpstreamBaseUrls).

