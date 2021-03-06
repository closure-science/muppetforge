-module(muppet_auth_always).
-behaviour(muppet_auth).
-export([init/1, is_authorized/2, can_challenge/2, challenge/2, terminate/1]).

init(_Any) ->
    [].
    
is_authorized(_State, _Req) ->
    true.

can_challenge(_State, _Req) ->
    false.

challenge(_State, _Req) ->
    throw(muppet_auth_always_never_challenges).

terminate(_State) ->
    ok.