-module(muppet_repository).

-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).

-export([search_module/1, search_modules/1]).
-export([start_link/0]).

-record(state, {

}).

%public interface

start_link() ->
    process_flag(trap_exit, true),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

search_module(TodoReq) ->
    gen_server:call(?MODULE, todo).

search_modules(TodoReq) ->
    gen_server:call(?MODULE, todo).


% gen_server callbacks

init([]) ->
    {ok, #state{}}.

handle_cast(Request, State) ->
    {noreply, State}.

handle_call(status, From, State) ->
    {reply, State, State}.

handle_info(Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
