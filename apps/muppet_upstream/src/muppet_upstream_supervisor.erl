-module(muppet_upstream_supervisor).

-behaviour(supervisor).
-export([init/1]).
-define(MAX_RESTART, 5).
-define(MAX_TIME, 100).


init([]) ->
    {ok, {
        {one_for_one, ?MAX_RESTART, ?MAX_TIME},
        [
            {muppet_upstream_observer, {muppet_upstream_observer, start_link, []}, permanent, brutal_kill, worker, [muppet_upstream_observer]},
            {muppet_upstream, {muppet_upstream, start_link, []}, permanent, brutal_kill, worker, [muppet_upstream]}
        ]
    }}.
