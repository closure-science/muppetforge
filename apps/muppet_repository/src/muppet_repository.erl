-module(muppet_repository).

-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).

-export([add_module/6, search_module/1, search_modules/1]).
-export([start_link/0]).


%public interface

start_link() ->
    process_flag(trap_exit, true),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_module(Author, Name, Desc, ProjectUrl, Releases, Tags) ->
    Module = create_module(Author, Name, Desc, ProjectUrl, Releases, Tags),
    gen_server:call(?MODULE, {add_module, Module}).

search_module(Terms) ->
    gen_server:call(?MODULE, {search_one, Terms}).

search_modules(Terms) ->
    gen_server:call(?MODULE, {search_all, Terms}).

% gen_server callbacks

init([]) ->
    {ok, [
        create_module(<<"Mario">>, <<"mario_module">>, <<"A mario module">>, <<"http://mario.example.com">>, [<<"0.1.0">>], [<<"luigi">>, <<"luigi2">>])
    ]}.

handle_cast(Request, State) ->
    {noreply, State}.

handle_call({add_module, Module}, From, State) ->
    % TODO: precond(does not exist)
    {reply, ok, [Module|State]};
handle_call({search_one, Terms}, From, State) ->
    Result = search_one(State, Terms),
    {reply, Result, State};
handle_call({search_all, Terms}, From, State) ->
    Result = search_all(State, Terms),
    {reply, Result, State};
handle_call(status, From, State) ->
    {reply, State, State}.

handle_info(Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


%priv

create_module(Author, Name, Desc, ProjectUrl, Releases, Tags) ->
    [
        {full_name, <<Author/binary, <<"/">>/binary, Name/binary>> },
        {author, Author},
        {name, Name},
        {desc, Desc},
        {project_url, ProjectUrl},
        {releases, [{[{version, V}]} || V <- Releases]},
        {version, hd(Releases)},
        {tags, Tags}
    ].

search_one([Module|T], Terms) ->
    case module_matches(Module, Terms) of
        true -> {true, Module};
        false -> search_one(T, Terms)
    end;
search_one([], Terms) ->
    {false, Terms}.


search_all(Modules, Terms) ->
    search_all(Modules, Terms,[]).
search_all([Module|Rest], Terms, Matching) ->
    case module_matches(Module, Terms) of
        true -> search_all(Rest, Terms, [Module|Matching]);
        false -> search_all(Rest, Terms, Matching)
    end;
search_all([], Terms, Matching) ->
    Matching.

module_matches(Module, Terms) ->
    lists:all(fun(Term) -> 
        lists:any(fun(Tuple) -> Term =:= Tuple end, Module)
    end, Terms).


