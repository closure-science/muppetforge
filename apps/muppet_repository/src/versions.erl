-module(versions).
-export([constraints/1, matches/2, max/2, version/1, to_binary/1, to_string/1]).


-type version_type() :: {Major::integer(), Minor::integer(), Patch::integer(), Special::binary() }.
-type version_op() :: eq | lt | gt | lte | gte.
-type constraint_type() :: {version_op(), version_type()}.

-export_type([version_type/0, version_op/0, constraint_type/0]).



-spec to_binary( version_type() | [constraint_type()] ) -> binary().
to_binary(Version) when is_tuple(Version)->
    bin(Version);
to_binary(Constraints) ->
    bin_join( lists:map(fun bin/1, Constraints), <<" ">>).

-spec to_string( version_type() | [constraint_type()] ) -> string().
to_string(E) ->
    binary_to_list(to_binary(E)).

bin(eq) -> <<>>;
bin(lt) -> <<"<">>;
bin(lte) -> <<"<=">>;
bin(gt) -> <<">">>;
bin(gte) -> <<">=">>;
bin({Major, Minor, Patch, undefined}) ->
    Maj = integer_to_binary(Major),
    Min = integer_to_binary(Minor),
    Pat = integer_to_binary(Patch), 
    << Maj/binary, <<".">>/binary, Min/binary, <<".">>/binary, Pat/binary >>;
bin({Major, Minor, Patch, Special}) ->
    Maj = integer_to_binary(Major),
    Min = integer_to_binary(Minor),
    Pat = integer_to_binary(Patch), 
    << Maj/binary, <<".">>/binary, Min/binary, <<".">>/binary, Pat/binary, <<"-">>/binary, Special/binary >>;
bin({Op, Version}) ->
    BinOp =bin(Op), 
    BinVer = bin(Version),
    << BinOp/binary, BinVer/binary >>.

    
bin_join([], _Sep) ->
    <<>>;
bin_join([H|T], Sep) ->
    Suffixed = [<<Sep/binary, X/binary>> || X <- T],
    lists:foldl(fun(B, Acc) -> <<Acc/binary, B/binary>> end, H, Suffixed).


-spec version( binary()) -> version_type().
version(StrOrBinary)->
    {_Len, Version} = version_tuple_from_stream(StrOrBinary),
    Version.

-spec max( version_type(), version_type()) -> version_type().
max(Lhs, Rhs) ->
    case compare(gte, Lhs, Rhs) of
        true -> Lhs;
        _ -> Rhs
    end.

-spec matches( [constraint_type()], version_type()) -> boolean().
matches([], _Version) ->
    true;
matches(Constraints, {Major, Minor, Patch, Special}) ->
    matches(Constraints, Major, Minor, Patch, Special).

matches([], _NeedleMajor, _NeedleMinor, _NeedlePatch, _NeedleSpecial) ->
    true;
matches([{Op,HayStack}|Constraints], NeedleMajor, NeedleMinor, NeedlePatch, NeedleSpecial) ->
    case compare(Op, {NeedleMajor, NeedleMinor, NeedlePatch, NeedleSpecial}, HayStack) of
        true -> matches(Constraints, NeedleMajor, NeedleMinor, NeedlePatch, NeedleSpecial);
        false -> false
    end.

compare(eq, Version1, Version2) ->
    Version1 =:= Version2;
compare(gt, {LhsMajor, LhsMinor, LhsPatch, LhsSpecial}, {RhsMajor, RhsMinor, RhsPatch, RhsSpecial}) ->
    if
        LhsMajor =:= RhsMajor andalso LhsMinor =:= RhsMinor andalso LhsPatch =:= RhsPatch -> LhsSpecial > RhsSpecial;
        LhsMajor =:= RhsMajor andalso LhsMinor =:= RhsMinor -> LhsPatch > RhsPatch;
        LhsMajor =:= RhsMajor -> LhsMinor > RhsMinor;
        true -> LhsMajor > RhsMajor
    end;
compare(lt, LHS, RHS) ->
    compare(gt, RHS, LHS);
compare(gte, LHS, RHS) ->
    compare(eq, LHS, RHS) orelse compare(gt, LHS, RHS);
compare(lte, LHS, RHS) ->
    compare(eq, LHS, RHS) orelse compare(lt, LHS, RHS).


-spec constraints( undefined | string() | binary() ) -> [constraint_type()].
constraints(undefined) ->
    [];
constraints(ConstraintsBin) when is_binary(ConstraintsBin) ->
    constraints(binary_to_list(ConstraintsBin));
constraints(ConstraintsStr) ->
    Tokens = tokenize(ConstraintsStr, [], default),
    parse(Tokens, []).

parse([], Accum) ->
    Accum;

parse([Op, {_, _, _, _} = Version|Rest], Accum) when is_atom(Op) ->
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

tokenize([$~ | Rest], Accum, default) ->
    % ~3 is an alias for 3.x
    % ~3.2 is an alias for 3.2.x
    tokenize(Rest, Accum, circa);


tokenize(Stream, Accum, circa) ->
    {match, [All|Parts]} = re:run(Stream, "(\\d+)(?:\\.(\\d+))?", [{capture, all, list}]),
    Versions = wildcard_to_tokens(Parts),
    tokenize(lists:nthtail(length(All), Stream) , Accum ++ Versions, default);    
tokenize(Stream, Accum, default) ->
    case re:run(Stream, "(\\d+(?:\\.\\d+)?)\\.x", [{capture, all, list}]) of
        {match, [All, AsCirca]}   -> 
            Accum2 = tokenize(AsCirca, Accum, circa),
            tokenize(lists:nthtail(length(All), Stream) , Accum2, default);
        nomatch -> tokenize(Stream, Accum ++ [eq], version)
    end;
tokenize(Stream, Accum, version) ->
    {Size, Version} = version_tuple_from_stream(Stream),
    tokenize(lists:nthtail(Size, Stream) , Accum ++ [Version], default).



wildcard_to_tokens([MajorStr, MinorStr]) ->
    Major = list_to_integer(MajorStr),
    Minor = list_to_integer(MinorStr),
    [gte, {Major, Minor, 0, undefined}, lt, {Major, Minor+1, 0, undefined}];
wildcard_to_tokens([MajorStr]) ->
    Major = list_to_integer(MajorStr),
    [gte, {Major, 0, 0, undefined}, lt, {Major+1, 0, 0, undefined}].

version_tuple_from_stream(Stream) when is_binary(Stream) ->
    version_tuple_from_stream(binary_to_list(Stream));
version_tuple_from_stream(Stream) ->
    case re:run(Stream, "(\\d+)\\.(\\d+)\\.(\\d+)(?:-(\\S+))?", [{capture, all, list}]) of
        {match, [All, Major, Minor, Patch]} ->  {length(All), {list_to_integer(Major), list_to_integer(Minor), list_to_integer(Patch), undefined }};
        {match, [All, Major, Minor, Patch, Special]} ->  {length(All), {list_to_integer(Major), list_to_integer(Minor), list_to_integer(Patch), list_to_binary(Special) }}
    end.
