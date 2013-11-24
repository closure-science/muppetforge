-module(muppet_repository_application).
-behaviour(application).
-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
   register(muppet_repository_application, self()),
   supervisor:start_link({local, muppet_repository_supervisor}, muppet_repository_supervisor, []).

stop(_State) ->
    ok.