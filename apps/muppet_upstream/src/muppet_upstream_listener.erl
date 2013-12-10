-module(muppet_upstream_listener).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).
-export([set_observed/1]).

-record(state, { observers = [] } ).

% -----------------------------------------------------------------------------
-spec start_link() -> {ok,pid()} | ignore | {error, {already_started, pid()} | term()}.
% -----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% -----------------------------------------------------------------------------
-spec set_observed([string()]) -> ok.
% -----------------------------------------------------------------------------
set_observed(Upstreams) ->
    gen_server:cast(?MODULE, {set_observed, Upstreams}).

init([]) ->
    process_flag(trap_exit, true),    
    {ok, #state{}}.

handle_cast({set_observed, Upstreams}, State) ->
    close_all_upstreams(State#state.observers),
    Observers = lists:flatmap(fun(UpstreamBaseUrl) ->
        case muppet_upstream_listener_ws_client:start_link(UpstreamBaseUrl) of 
            {ok, Pid} -> [{UpstreamBaseUrl, Pid}];
            _ -> []
        end
    end, Upstreams),
    {noreply, #state{ observers=Observers}};

handle_cast(_Req, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, {error, badrequest}, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, State) ->
    close_all_upstreams(State#state.observers).

close_all_upstreams(Observers) ->
    lists:foreach(fun({_UpstreamBaseUrl, ClientPid}) -> muppet_upstream_listener_ws_client:close(ClientPid) end, Observers).
