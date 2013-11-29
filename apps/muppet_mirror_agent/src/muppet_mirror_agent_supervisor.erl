-module(muppet_mirror_agent_supervisor).

-behaviour(supervisor).
-export([init/1]).

init([]) ->
    MaxRestart = 5,
    MaxTime = 100,
    StartFun = {muppet_mirror_agent, start_link, []},
    Modules = [muppet_mirror_agent],
    {ok, {
        {one_for_one, MaxRestart, MaxTime},
        [
            {muppet_mirror_agent, StartFun, permanent, brutal_kill, worker, Modules}
        ]
    }}.
