-module(muppet_upstream_fcn).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0, reset_listeners/1]).
-define(TICK_INTERVAL_MILLIS, 20000).
-record(state, { listeners = [], failed = [], timer} ).

% -----------------------------------------------------------------------------
-spec start_link() -> {ok,pid()} | ignore | {error, {already_started, pid()} | term()}.
% -----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% -----------------------------------------------------------------------------
-spec reset_listeners([string()]) -> ok.
% -----------------------------------------------------------------------------
reset_listeners(UpstreamBaseUrls) ->
    gen_server:cast(?MODULE, {reset_listeners, UpstreamBaseUrls}).

init([]) ->
    process_flag(trap_exit, true),
    self() ! tick,
    {ok, #state{}}.

handle_cast({reset_listeners, UpstreamBaseUrls}, State) ->
    close_all_upstreams(State#state.listeners),
    NewState = start_listening(UpstreamBaseUrls),
    {noreply, NewState};

handle_cast(_Req, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, {error, badrequest}, State}.

handle_info(tick, State) ->
    NewState = start_listening(State#state.failed),
    {ok, Timer} = timer:send_after(?TICK_INTERVAL_MILLIS, tick),
    {noreply, #state{ 
        listeners = State#state.listeners ++ NewState#state.listeners, 
        failed = NewState#state.failed,
        timer = Timer
    }};

handle_info({'EXIT', Pid, _Reason}, State) ->
    NewState = case lists:keyfind(Pid, 2, State#state.listeners) of
        false -> State;
        {BaseUrl, _} ->
            #state{ 
                listeners = lists:keydelete(Pid, 2, State#state.listeners), 
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
    close_all_upstreams(State#state.listeners).

close_all_upstreams(Listeners) ->
    lists:foreach(fun({_UpstreamBaseUrl, ClientPid}) -> muppet_upstream_fcn_listener:close(ClientPid) end, Listeners).

start_listening(UpstreamBaseUrls) ->
    lists:foldl(fun(UpstreamBaseUrl, State) ->
        case muppet_upstream_fcn_listener:start_link(UpstreamBaseUrl) of 
            {ok, Pid} -> State#state{ listeners= [{UpstreamBaseUrl, Pid}|State#state.listeners]};
            _ -> State#state{ failed = [UpstreamBaseUrl|State#state.failed]}
        end
    end, #state{}, UpstreamBaseUrls).

