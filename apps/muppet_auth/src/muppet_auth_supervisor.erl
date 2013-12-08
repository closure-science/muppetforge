-module(muppet_auth_supervisor).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).
-define(MAX_RESTART, 5).
-define(MAX_TIME, 100).


start_link(AuthModulesAndOptions) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, AuthModulesAndOptions).

init(AuthModulesAndOptions) ->
    {ok, {
        {one_for_one, ?MAX_RESTART, ?MAX_TIME},
        [
            {muppet_auth, {muppet_auth, start_link, [AuthModulesAndOptions]}, permanent, brutal_kill, worker, [muppet_auth]}
        ]
    }}.

