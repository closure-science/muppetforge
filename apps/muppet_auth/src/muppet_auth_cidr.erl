-module(muppet_auth_cidr).
-behaviour(muppet_auth).
-export([init/1, is_authorized/2, can_challenge/2, challenge/2, terminate/1]).

init(Options) ->
    {allowed_cidrs, Spec} = proplists:lookup(allowed_cidrs, Options),
    lists:map(fun({Ip, Mask}) ->
        {ok, IpAddress} = inet:parse_address(Ip),
        cidr_to_prefix(IpAddress, Mask) 
    end, Spec).

is_authorized(Allowed, Req) ->
    {{Ip, _Port}, _} = cowboy_req:peer(Req),
    is_auth(Allowed, Ip).

can_challenge(_AllowedAddresses, _Req) ->
    false.

challenge(_AllowedAddresses, _Req) ->
    throw(muppet_auth_network_never_challenges).

terminate(_AllowedAddresses) ->
    ok.

%% @private
is_auth([], _Ip) ->
    false;
is_auth([Spec|Rest], Ip) ->
    case in_whitelist(Spec, Ip) of
        true -> true;
        false -> is_auth(Rest, Ip)
    end.

%% @private
in_whitelist({Size, Prefix, Bits}, Peer) when is_tuple(Peer), Size =:= size(Peer) ->
    Prefix =:= cidr_to_prefix(Peer, Bits);
in_whitelist(_, _) ->
    false.

%% @private
cidr_to_prefix({O1, O2, O3, O4}, Bits) ->
    BinIp = <<O1:8, O2:8, O3:8, O4:8>>,
    {4, <<BinIp:Bits/bitstring>> };
cidr_to_prefix({O1, O2, O3, O4, O5, O6, O7, O8}, Bits) ->
    BinIp = <<O1:16, O2:16, O3:16, O4:16, O5:16, O6:16, O7:16, O8:16>>,
    {8, <<BinIp:Bits/bitstring>>}.



