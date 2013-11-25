-module(muppetforge).
-export([start_httpd/0]).
-spec start_httpd() -> {ok, pid()} | {error, any()}.

start_httpd() ->
    Dispatcher = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static,  {file, code:priv_dir(muppetforge) ++ "/static/index.html"}},
            {"/resources/[...]", cowboy_static, {dir, code:priv_dir(muppetforge) ++ "/static/resources" }},
            {"/modules.json", muppetforge_modules_handler, [modules]},
            {"/api/v1/releases.json", muppetforge_releases_handler, []},
            {"/api/deploy", muppetforge_modules_handler, [deploy]},
            {"/:author/:modulename", [{modulename, function, fun module_name/1}], muppetforge_modules_handler, [module]},
            {"/system/releases/[...]", cowboy_static, {dir, muppet_repository:assets_dir() }}
        ]}
    ]),
    cowboy:start_http(http, 100, [{port, 8080}], [ {env, [{dispatch, Dispatcher}]} ]).


module_name(ModuleNameAndJsonSuffix) ->
    ExtensionSize = byte_size(<<".json">>),
    Size = byte_size(ModuleNameAndJsonSuffix),
    case Size > ExtensionSize of 
        true ->
            Prefix = binary:part(ModuleNameAndJsonSuffix, {0, Size - ExtensionSize}),
            {true, Prefix};
        _ -> false
    end.
