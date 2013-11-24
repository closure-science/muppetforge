-module(muppet_repository_supervisor).

-behaviour(supervisor).
-export([init/1]).

init([]) ->
    MaxRestart = 5,
    MaxTime = 100,
    StartFun = {muppet_repository, start_link, []},
    Modules = [muppet_repository],
    {ok, {
        {one_for_one, MaxRestart, MaxTime},
        [
            {muppet_repository, StartFun, permanent, brutal_kill, worker, Modules}
        ]
    }}.
