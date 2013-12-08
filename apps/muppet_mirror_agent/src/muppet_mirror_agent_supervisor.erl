-module(muppet_mirror_agent_supervisor).

-behaviour(supervisor).
-export([init/1]).
-define(MAX_RESTART, 5).
-define(MAX_TIME, 100).


init([]) ->
    StartFun = {muppet_mirror_agent, start_link, []},
    Modules = [muppet_mirror_agent],
    {ok, {
        {one_for_one, ?MAX_RESTART, ?MAX_TIME},
        [
            {muppet_mirror_agent, StartFun, permanent, brutal_kill, worker, Modules}
        ]
    }}.
