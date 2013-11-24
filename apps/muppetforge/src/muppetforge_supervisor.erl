-module(muppetforge_supervisor).

-behaviour(supervisor).
-export([init/1]).

init([]) ->
    MaxRestart = 5,
    MaxTime = 100,
    StartFun = {muppetforge, start_httpd, []},
    Modules = [muppetforge],
    {ok, { 
        {one_for_one, MaxRestart, MaxTime}, 
        [{muppetforge, StartFun, permanent, brutal_kill, worker, Modules}]
    }}.
