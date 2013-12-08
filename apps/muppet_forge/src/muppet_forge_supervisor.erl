-module(muppet_forge_supervisor).

-behaviour(supervisor).
-export([init/1]).

init([]) ->
    MaxRestart = 5,
    MaxTime = 100,
    StartFun = {muppet_forge, start_httpd, []},
    Modules = [muppet_forge],
    {ok, { 
        {one_for_one, MaxRestart, MaxTime}, 
        [{muppet_forge, StartFun, permanent, brutal_kill, worker, Modules}]
    }}.
