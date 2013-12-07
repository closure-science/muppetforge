-module(muppet_repository_observable).

-behaviour(gen_server).
-export([start_link/0]).
-export([register/1, deregister/1, register/0, deregister/0, notify_all/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).

% -----------------------------------------------------------------------------
-spec start_link() -> {ok,pid()} | ignore | {error, {already_started, pid()} | term()}.
% -----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% -----------------------------------------------------------------------------
-spec register() -> ok.
% -----------------------------------------------------------------------------
register() ->
    register(self()).

% -----------------------------------------------------------------------------
-spec register(Pid::pid()) -> ok.
% -----------------------------------------------------------------------------
register(Pid) ->
    gen_server:cast(?MODULE, {register, Pid}).

% -----------------------------------------------------------------------------
-spec deregister() -> ok.
% -----------------------------------------------------------------------------
deregister() ->
    deregister(self()).

% -----------------------------------------------------------------------------
-spec deregister(Pid::pid()) -> ok.
% -----------------------------------------------------------------------------
deregister(Pid) ->
    gen_server:cast(?MODULE, {deregister, Pid}).

% -----------------------------------------------------------------------------
-spec notify_all(Msg::any()) -> ok.
% -----------------------------------------------------------------------------
notify_all(Msg) ->
    gen_server:cast(?MODULE, {notify_all, Msg}).

init([]) ->
    {ok, sets:new() }.

handle_cast({register, Pid}, State) ->
    {noreply, sets:add_element(Pid, State)};

handle_cast({deregister, Pid}, State) ->
    {noreply, sets:del_element(Pid, State)};

handle_cast({notify_all, Msg}, State) ->
    sets:fold(fun(Pid, _) -> Pid ! Msg  end, undefined, State),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    {reply, {error, badrequest}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

