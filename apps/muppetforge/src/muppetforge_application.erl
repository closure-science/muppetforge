-module(muppetforge_application).

-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    register(muppetforge_application, self()),
    supervisor:start_link({local, muppetforge_supervisor}, muppetforge_supervisor, []).

stop(_State) ->
    ok.

