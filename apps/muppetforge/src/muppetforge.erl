-module(muppetforge).
-export([start_httpd/0]).
-spec start_httpd() -> {ok, pid()} | {error, any()}.

start_httpd() ->
    Dispatcher = cowboy_router:compile([
        {'_', [
            {"/mf/modules.json", muppetforge_handler, [modules]},
            {"/mf/api/v1/releases.json", muppetforge_handler, [releases]},
            {"/mf/api/deploy", muppetforge_handler, [deploy]},
            {"/mf/:author/:modulename", [{modulename, function, fun module_name/1}], muppetforge_handler, [module]},
            {"/mf/[...]", cowboy_static, {dir, muppet_repository:assets_dir() }},
            {"/", cowboy_static,  {file, code:priv_dir(muppetforge) ++ "/static/index.html"}},
            {"/[...]", cowboy_static, {dir, code:priv_dir(muppetforge) ++ "/static" }}
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
