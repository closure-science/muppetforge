-module(muppet_auth_application).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    register(muppet_auth_application, self()),
    AuthModules = application:get_env(muppet_auth, auth_modules, [{muppet_auth_always,[]}]),
    muppet_auth_supervisor:start_link(AuthModules).


stop(_State) ->
    ok.
