-module(muppet_forge).
-export([start_httpd/0]).

% -----------------------------------------------------------------------------
-spec start_httpd() -> {ok, pid()} | {error, any()}.
% -----------------------------------------------------------------------------
start_httpd() ->
    Dispatcher = cowboy_router:compile([
        {'_', [
            {"/mf/modules.json", muppet_forge_handler, [modules]},
            {"/mf/api/v1/releases.json", muppet_forge_handler, [releases]},
            {"/mf/api/deploy", muppet_forge_handler, [deploy]},
            {"/mf/api/blacklist", muppet_forge_handler, [blacklist]},
            {"/mf/api/upstream", muppet_forge_handler, [upstream]},
            {"/mf/api/listen", muppet_forge_websocket_handler, undefined},
            {"/mf/api/errors", muppet_forge_handler, [errors]},
            {"/mf/api/info", muppet_forge_handler, [info]},
            {"/mf/api/:author/:modulename/:version", muppet_forge_handler, [release]},
            {"/mf/:author/:modulename", [{modulename, function, fun module_name/1}], muppet_forge_handler, [module]},
            {"/mf/[...]", muppet_forge_static_handler, [muppet_repository:assets_dir()]},
            {"/", cowboy_static,  {file, code:priv_dir(muppet_forge) ++ "/static/index.html"}},
            {"/[...]", cowboy_static, {dir, code:priv_dir(muppet_forge) ++ "/static" }}
        ]}
    ]),
    cowboy:start_http(http, 100, [{port, 8080}], [ {env, [{dispatch, Dispatcher}]} ]).


module_name(PossiblySuffixed) ->
    {match, [ModuleName]} = re:run(PossiblySuffixed, "(.*?)(?:\\.json)?$", [{capture, all_but_first, binary}]),
    {true, ModuleName}.
