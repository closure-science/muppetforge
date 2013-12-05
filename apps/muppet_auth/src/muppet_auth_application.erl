-module(muppet_auth_application).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    register(muppet_auth_application, self()),
    muppet_auth_supervisor:start_link(auth_modules_and_options()).


auth_modules_and_options() ->
    case proplists:get_value(auth_modules, application:get_all_env()) of
        undefined -> [{muppet_auth_always,[]}];
        AuthModulesAndOptions -> AuthModulesAndOptions
    end.

stop(_State) ->
    ok.
