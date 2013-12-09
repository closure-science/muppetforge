-module(muppet_upstream_application).
-behaviour(application).
-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
   register(muppet_upstream_application, self()),
   supervisor:start_link({local, muppet_upstream_supervisor}, muppet_upstream_supervisor, []).

stop(_State) ->
    ok.