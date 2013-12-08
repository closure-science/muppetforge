-module(muppet_forge_application).

-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    register(muppet_forge_application, self()),
    supervisor:start_link({local, muppet_forge_supervisor}, muppet_forge_supervisor, []).

stop(_State) ->
    ok.

