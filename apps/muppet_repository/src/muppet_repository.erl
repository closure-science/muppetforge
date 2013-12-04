-module(muppet_repository).

-behaviour(gen_server).
-export([find/1, search/1, delete/3, find_release/2, store/1, assets_dir/0, status/0, knows/3, info/0]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).

% -----------------------------------------------------------------------------
-spec start_link() -> {ok,pid()} | ignore | {error, {already_started, pid()} | term()}.
% -----------------------------------------------------------------------------
start_link() ->
    process_flag(trap_exit, true),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% -----------------------------------------------------------------------------
-spec assets_dir() -> [char()].
% -----------------------------------------------------------------------------
assets_dir() -> 
    code:priv_dir(?MODULE) ++ "/assets".

% -----------------------------------------------------------------------------
-spec find_release({binary(), binary()}, [versions:constraint_type()] ) -> {ok, dict()} | {not_found, FullName::binary()}.
% -----------------------------------------------------------------------------
find_release({Author, Name}, VersionConstraints) ->
    gen_server:call(?MODULE, {find_release, Author, Name, VersionConstraints}).

% -----------------------------------------------------------------------------
-spec find(binary()) -> {true, muppet_driver:module_type()} | {false, FullName::binary()}.
% -----------------------------------------------------------------------------
find(FullName) ->
    gen_server:call(?MODULE, {find_module, FullName}).

% -----------------------------------------------------------------------------
-spec delete(binary(), binary(), versions:version_type()) -> ok | error.
% -----------------------------------------------------------------------------
delete(Author, Name, Version) ->
    gen_server:call(?MODULE, {delete_release, Author, Name, Version}).

% -----------------------------------------------------------------------------
-spec store( binary() ) -> ok | {error, {Error::any(), Reason::any()}}.
% -----------------------------------------------------------------------------
store(Tarball) ->
    gen_server:call(?MODULE, {store_module, Tarball}).

% -----------------------------------------------------------------------------
-spec search( [binary()] ) -> [muppet_driver:module_type()].
% -----------------------------------------------------------------------------
search(Terms) ->
    gen_server:call(?MODULE, {search_modules, Terms}).

% -----------------------------------------------------------------------------
-spec knows(binary(), binary(), versions:version_type() ) -> boolean().
% -----------------------------------------------------------------------------
knows(Author, Module, Version) ->
    gen_server:call(?MODULE, {knows, Author, Module, Version}).


% -----------------------------------------------------------------------------
-spec status() -> term().
% -----------------------------------------------------------------------------
status() ->
    sys:get_status(?MODULE).

% -----------------------------------------------------------------------------
-spec info() -> [{atom(), any()}].
% -----------------------------------------------------------------------------
info() ->
    gen_server:call(?MODULE, info).


init([]) ->
    filelib:ensure_dir(filename:join(assets_dir(), "anyname")),
    State = muppet_driver:new(assets_dir()),
    {ok, State}.

handle_cast({store_module, Tarball}, State) ->
    NewState = case muppet_driver:store(State, assets_dir(), Tarball) of
        {ok, S} -> S;
        _ -> State
    end,
    {noreply, NewState}.
handle_call({store_module, Tarball}, _From, State) ->
    {Response, NewState} = case muppet_driver:store(State, assets_dir(), Tarball) of
        {ok, _State} = R -> R;
        E -> {E, State}
    end,
    {reply, Response, NewState};    
handle_call({delete_release, Author, Name, Version}, _From, State) ->
    {Response, NewState} = muppet_driver:delete(State, assets_dir(), {Author, Name, Version}),
    {reply, Response, NewState};    
handle_call({find_module, FullName}, From, State) ->
    async_(From, State, fun() -> muppet_driver:find(State, FullName) end);
handle_call({search_modules, Terms}, From, State) ->
    async_(From, State, fun() -> muppet_driver:search(State, Terms) end);
handle_call({knows, Author, Module, Version}, From, State) ->
    async_(From, State, fun() -> muppet_driver:knows(State, Author, Module, Version) end);
handle_call({find_release, Author, Name, VersionConstraints}, From, State) ->
    async_(From, State, fun() -> muppet_driver:find_release(State, Author, Name, VersionConstraints) end);

handle_call(info, From, State) ->
    async_(From, State, fun() ->
        {Modules, Releases} = muppet_driver:info(State),
        [{modules, Modules}, {releases, Releases}]
    end).

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


async_(To, State, Fun) -> spawn(fun() -> gen_server:reply(To, Fun()) end), {noreply, State}.

