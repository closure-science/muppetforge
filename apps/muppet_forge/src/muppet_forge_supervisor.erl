-module(muppet_forge_supervisor).

-behaviour(supervisor).
-export([init/1]).
-define(MAX_RESTART, 5).
-define(MAX_TIME, 100).

init([]) ->
    {ok, { 
        {one_for_one, ?MAX_RESTART, ?MAX_TIME}, 
        [{muppet_forge, {muppet_forge, start_httpd, []}, permanent, brutal_kill, worker, [muppet_forge]}]
    }}.
