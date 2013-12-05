-module(muppet_auth).
-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/1]).
-export([is_authorized/1, challenge/1]).

-callback init(Options::any()) -> State::any().
-callback is_authorized(State::any(), Req::cowboy_req:req()) -> boolean().
-callback can_challenge(State::any(), Req::cowboy_req:req()) -> boolean().
-callback challenge(State::any(), Req::cowboy_req:req()) -> Headers::[{binary(), binary()}].
-callback terminate(State::any()) -> ok.

start_link(AuthModules) ->
    process_flag(trap_exit, true),
    gen_server:start_link({local, ?MODULE}, ?MODULE, AuthModules, []).


% -----------------------------------------------------------------------------
-spec is_authorized(Req::cowboy_req:req()) -> boolean().
% -----------------------------------------------------------------------------
is_authorized(Req) ->
    gen_server:call(?MODULE, {is_authorized, Req}).


is_authorized([], _Req) ->
    false;
is_authorized([{Module, State}|Others], Req) ->
    case Module:is_authorized(State, Req) of
        true -> true;
        false -> is_authorized(Others, Req)
    end.


% -----------------------------------------------------------------------------
-spec challenge(Req::cowboy_req:req()) -> [{binary(), binary()}].
% -----------------------------------------------------------------------------
challenge(Req) ->
    gen_server:call(?MODULE, {challenge, Req}).

challenge([], _Req) ->
    [];
challenge([{Module, State}|Others], Req) ->
    case Module:can_challenge(State, Req) of
        false -> challenge(Others, Req);
        true -> Module:challenge(State, Req)
    end.


% -----------------------------------------------------------------------------
%% gen_server callbacks
% -----------------------------------------------------------------------------

init(AuthModulesAndOptions) ->
    ModulesAndStates = lists:map(fun({Module, Options}) ->
        {Module, Module:init(Options)}        
    end, AuthModulesAndOptions),
    {ok, ModulesAndStates}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({is_authorized, Req}, _From, State) ->
    Response = is_authorized(State, Req),
    {reply, Response, State};
handle_call({challenge, Req}, _From, State) ->
    Response = challenge(State, Req),
    {reply, Response, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, ModulesAndStates) ->
    lists:foreach(fun({Module, State}) -> 
        Module:terminate(State)
    end, ModulesAndStates),
    ok.
