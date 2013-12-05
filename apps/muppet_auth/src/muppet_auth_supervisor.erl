-module(muppet_auth_supervisor).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).

start_link(AuthModulesAndOptions) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, AuthModulesAndOptions).

init(AuthModulesAndOptions) ->
    MaxRestart = 5,
    MaxTime = 100,
    StartFun = {muppet_auth, start_link, [AuthModulesAndOptions]},
    Modules = [muppet_auth],
    {ok, {
        {one_for_one, MaxRestart, MaxTime},
        [
            {muppet_auth, StartFun, permanent, brutal_kill, worker, Modules}
        ]
    }}.

