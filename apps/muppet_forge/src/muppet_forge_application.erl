-module(muppet_forge_application).

-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    register(muppet_forge_application, self()),
    Protocol = application:get_env(muppet_forge, protocol, https),
    Port = application:get_env(muppet_forge, port, 8080),
    Acceptors = application:get_env(muppet_forge, acceptors, 100),

    Dispatcher = cowboy_router:compile([
        {'_', [
            {"/modules.json", muppet_forge_handler_json, [modules]},
            {"/api/v1/releases.json", muppet_forge_handler_json, [releases]},
            {"/api/mf/deploy", muppet_forge_handler_json, [deploy]},
            {"/api/mf/blacklist", muppet_forge_handler_json, [blacklist]},
            {"/api/mf/upstream", muppet_forge_handler_json, [upstream]},
            {"/api/mf/listen", muppet_forge_handler_websocket, undefined},
            {"/api/mf/errors", muppet_forge_handler_json, [errors]},
            {"/api/mf/info", muppet_forge_handler_json, [info]},
            {"/api/mf/:author/:modulename/:version", muppet_forge_handler_json, [release]},
            {"/static/[...]", cowboy_static, {dir, code:priv_dir(muppet_forge) ++ "/static" }},
            {"/:author/:modulename", [{modulename, function, fun module_name/1}], muppet_forge_handler_json, [module]},
            {"/", cowboy_static,  {file, code:priv_dir(muppet_forge) ++ "/static/index.html"}},
            {"/[...]", muppet_forge_handler_static, [muppet_repository:assets_dir()]}
        ]}
    ]),
    start_cowboy(Protocol, Port, Acceptors, Dispatcher).

stop(_State) ->
    ok.

module_name(PossiblySuffixed) ->
    {match, [ModuleName]} = re:run(PossiblySuffixed, "(.*?)(?:\\.json)?$", [{capture, all_but_first, binary}]),
    {true, ModuleName}.


start_cowboy(http, Port, Acceptors, Dispatcher) ->
    cowboy:start_http(cowboy_ref, Acceptors, [{port, Port}], [ {env, [{dispatch, Dispatcher}]} ]);

start_cowboy(https, Port, Acceptors, Dispatcher) ->
    {ok, CaCertFile} = application:get_env(cacertfile),
    {ok, CertFile} = application:get_env(certfile),
    {ok, KeyFile} = application:get_env(keyfile),
    cowboy:start_https(cowboy_ref, Acceptors, [{port, Port},{cacertfile, CaCertFile},{certfile, CertFile},{keyfile, KeyFile}], [ {env, [{dispatch, Dispatcher}]} ]);

start_cowboy(UnsupportedProtocol, _, _, _) ->
    throw({unsupported_protocol, UnsupportedProtocol}).