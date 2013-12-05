-module(muppet_basic_auth_ldap).
-behaviour(muppet_auth).
-export([init/1, is_authorized/2, can_challenge/2, challenge/2, terminate/1]).

-record(state, {servers, dn_format, ldap_options}).

init(Options) ->
    {servers, Servers} = proplists:lookup(servers, Options),
    {dn_format, DnFormat} = proplists:lookup(dn_format, Options),
    {ldap_options, LdapOptions} = proplists:lookup(ldap_options, Options),
    #state{servers = Servers, dn_format=DnFormat, ldap_options = LdapOptions}.


is_authorized(State, Req) ->
    {ok, Auth, _} = cowboy_req:parse_header(<<"authorization">>, Req),
    case Auth of
        {<<"basic">>, {User, Pass}} -> 
            {ok, Handle} = eldap:open(State#state.servers, State#state.ldap_options),
            Dn = lists:flatten(io_lib:format(State#state.dn_format, [escape_dn_entry(User)])),
            Response = eldap:simple_bind(Handle, Dn, binary_to_list(Pass)),
            eldap:close(Handle),
            case  Response of
                {error, _Reason} -> false;
                ok -> true
            end;
        _ -> 
            false
    end.

can_challenge(_State, _Req) ->
    true.

challenge(_State, _Req) ->
    [
        {<<"WWW-Authenticate">>,<<"Basic realm=\"muppet_forge\"">>}
    ].

terminate(_State) ->
    ok.



escape_dn_entry(Username) when is_binary(Username)->
    escape_dn_entry(binary_to_list(Username));
escape_dn_entry(Username) ->
    lists:flatmap(fun escape/1, Username).

escape($,) ->  [$\\, $,];
escape($\\) -> [$\\, $\\];
escape($#) -> [$\\, $#];
escape($+) -> [$\\, $+];
escape($<) -> [$\\, $<];
escape($>) -> [$\\, $>];
escape($;) -> [$\\, $;];
escape($") -> [$\\, $"];
escape($=) -> [$\\, $=];
escape($/) -> [$\\, $/];
escape(Ch) -> [Ch].
    