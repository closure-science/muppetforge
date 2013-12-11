-module(muppet_forge).
-export([start_httpd/0]).

% -----------------------------------------------------------------------------
-spec start_httpd() -> {ok, pid()} | {error, any()}.
% -----------------------------------------------------------------------------
start_httpd() ->
    Dispatcher = cowboy_router:compile([
        {'_', [
            {"/mf/modules.json", muppet_forge_handler_json, [modules]},
            {"/mf/api/v1/releases.json", muppet_forge_handler_json, [releases]},
            {"/mf/api/deploy", muppet_forge_handler_json, [deploy]},
            {"/mf/api/blacklist", muppet_forge_handler_json, [blacklist]},
            {"/mf/api/upstream", muppet_forge_handler_json, [upstream]},
            {"/mf/api/listen", muppet_forge_handler_websocket, undefined},
            {"/mf/api/errors", muppet_forge_handler_json, [errors]},
            {"/mf/api/info", muppet_forge_handler_json, [info]},
            {"/mf/api/:author/:modulename/:version", muppet_forge_handler_json, [release]},
            {"/mf/:author/:modulename", [{modulename, function, fun module_name/1}], muppet_forge_handler_json, [module]},
            {"/mf/[...]", muppet_forge_handler_static, [muppet_repository:assets_dir()]},
            {"/", cowboy_static,  {file, code:priv_dir(muppet_forge) ++ "/static/index.html"}},
            {"/[...]", cowboy_static, {dir, code:priv_dir(muppet_forge) ++ "/static" }}
        ]}
    ]),
    cowboy:start_http(http, 100, [{port, 8080}], [ {env, [{dispatch, Dispatcher}]} ]).


module_name(PossiblySuffixed) ->
    {match, [ModuleName]} = re:run(PossiblySuffixed, "(.*?)(?:\\.json)?$", [{capture, all_but_first, binary}]),
    {true, ModuleName}.
