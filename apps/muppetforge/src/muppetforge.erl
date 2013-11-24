-module(muppetforge).
-export([start_httpd/0]).
-spec start_httpd() -> {ok, pid()} | {error, any()}.

start_httpd() ->
    Dispatcher = cowboy_router:compile([
        % {HostMatch, list({PathMatch, Handler, Opts})}        
        {'_', [
            {"/", muppetforge_root_handler, []},
            {"/modules.json", muppetforge_modules_handler, [modules]},
            {"/api/v1/releases.json", muppetforge_releases_handler, []},
            {"/:author/:modulename", muppetforge_modules_handler, [module]}
        ]}
    ]),
    cowboy:start_http(http, 100, [{port, 8080}], [ {env, [{dispatch, Dispatcher}]} ]).
