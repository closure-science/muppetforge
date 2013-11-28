-module(versions).
-export([constraints/1, matches/2, max/2, version/1, to_binary/1, to_string/1]).


-type version_type() :: {Major::integer(), Minor::integer(), Patch::integer()}.
-type version_op() :: eq | lt | gt | lte | gte.
-type constraint_type() :: {version_op(), version_type()}.

-export_type([version_type/0, version_op/0, constraint_type/0]).

-spec constraints( string() | binary() ) -> [constraint_type()].
-spec version( binary()) -> version_type().
-spec matches( [constraint_type()], version_type()) -> boolean().
-spec max( version_type(), version_type()) -> version_type().
-spec to_binary( version_type() | [constraint_type()] ) -> binary().
-spec to_string( version_type() | [constraint_type()] ) -> string().

to_binary(Version) when is_tuple(Version)->
    bin(Version);
to_binary(Constraints) ->
    bin_join( lists:map(fun bin/1, Constraints), <<" ">>).

to_string(E) ->
    binary_to_list(to_binary(E)).

bin(eq) -> <<>>;
bin(lt) -> <<"<">>;
bin(lte) -> <<"<=">>;
bin(gt) -> <<">">>;
bin(gte) -> <<">=">>;
bin({Major, Minor, Patch}) ->
    Maj = integer_to_binary(Major),
    Min = integer_to_binary(Minor),
    Pat = integer_to_binary(Patch), 
    << Maj/binary, <<".">>/binary, Min/binary, <<".">>/binary, Pat/binary >>;
bin({Op, Version}) ->
    BinOp =bin(Op), 
    BinVer = bin(Version),
    << BinOp/binary, BinVer/binary >>.
    
bin_join([], _Sep) ->
    <<>>;
bin_join([H|T], Sep) ->
    Suffixed = [<<Sep/binary, X/binary>> || X <- T],
    lists:foldl(fun(B, Acc) -> <<Acc/binary, B/binary>> end, H, Suffixed).


version(StrOrBinary)->
    {_Len, Version} = version_tuple_from_stream(StrOrBinary),
    Version.

max(Lhs, Rhs) ->
    case compare(gte, Lhs, Rhs) of
        true -> Lhs;
        _ -> Rhs
    end.


matches([], _Version) ->
    true;
matches(Constraints, {Major, Minor, Patch}) ->
    matches(Constraints, Major, Minor, Patch).

matches([], _NeedleMajor, _NeedleMinor, _NeedlePatch) ->
    true;
matches([{Op,HayStack}|Constraints], NeedleMajor, NeedleMinor, NeedlePatch) ->
    case compare(Op, {NeedleMajor, NeedleMinor, NeedlePatch}, HayStack) of
        true -> matches(Constraints, NeedleMajor, NeedleMinor, NeedlePatch);
        false -> false
    end.

compare(eq, Version1, Version2) ->
    Version1 =:= Version2;
compare(gt, {LhsMajor, LhsMinor, LhsPatch}, {RhsMajor, RhsMinor, RhsPatch}) ->
    if
        LhsMajor =:= RhsMajor andalso LhsMinor =:= RhsMinor  -> LhsPatch > RhsPatch;
        LhsMajor =:= RhsMajor -> LhsMinor > RhsMinor;
        true -> LhsMajor > RhsMajor
    end;
compare(lt, LHS, RHS) ->
    compare(gt, RHS, LHS);
compare(gte, LHS, RHS) ->
    compare(eq, LHS, RHS) orelse compare(gt, LHS, RHS);
compare(lte, LHS, RHS) ->
    compare(eq, LHS, RHS) orelse compare(lt, LHS, RHS).



constraints(ConstraintsBin) when is_binary(ConstraintsBin) ->
    constraints(binary_to_list(ConstraintsBin));
constraints(ConstraintsStr) ->
    Tokens = tokenize(ConstraintsStr, [], default),
    parse(Tokens, []).

parse([], Accum) ->
    Accum;

parse([Op, {_, _, _} = Version|Rest], Accum) when is_atom(Op) ->
    parse(Rest, Accum ++ [{Op, Version}]).

tokenize([], Accum, _State) ->
    Accum;
tokenize([$\s|Rest], Accum, State) ->
    tokenize(Rest, Accum, State);

tokenize([$<, $= | Rest], Accum, default) ->
    tokenize(Rest, Accum ++ [lte], version);

tokenize([$>, $= | Rest], Accum, default) ->
    tokenize(Rest, Accum ++ [gte], version);

tokenize([$> | Rest], Accum, default) ->
    tokenize(Rest, Accum ++ [gt], version);

tokenize([$< | Rest], Accum, default) ->
    tokenize(Rest, Accum ++ [lt], version);

tokenize(Stream, Accum, default) ->
    case re:run(Stream, "(\\d+)(?:\\.(\\d+))?\\.x", [{capture, all, list}]) of
        {match, [All|Parts]} ->
            Versions = wildcard_to_tokens(Parts),
            tokenize(lists:nthtail(length(All), Stream) , Accum ++ Versions, default);
        nomatch -> 
            tokenize(Stream, Accum ++ [eq], version)
    end;
tokenize(Stream, Accum, version) ->
    {Size, Version} = version_tuple_from_stream(Stream),
    tokenize(lists:nthtail(Size, Stream) , Accum ++ [Version], default).



wildcard_to_tokens([MajorStr, MinorStr]) ->
    Major = list_to_integer(MajorStr),
    Minor = list_to_integer(MinorStr),
    [gte, {Major, Minor, 0}, lt, {Major, Minor+1, 0}];
wildcard_to_tokens([MajorStr]) ->
    Major = list_to_integer(MajorStr),
    [gte, {Major, 0, 0}, lt, {Major+1, 0, 0}].

version_tuple_from_stream(Stream) when is_binary(Stream) ->
    version_tuple_from_stream(binary_to_list(Stream));
version_tuple_from_stream(Stream) ->
    {match, [All, Major, Minor, Patch]} = re:run(Stream, "(\\d+)\\.(\\d+)\\.(\\d+)", [{capture, all, list}]),
    Version = {list_to_integer(Major), list_to_integer(Minor), list_to_integer(Patch)},
    {length(All), Version}.
