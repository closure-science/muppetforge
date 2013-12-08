-module(versions_tests).
-include_lib("eunit/include/eunit.hrl").



can_parse_version_test() -> [{eq, {1,2,3, <<>>}}] = versions:constraints("1.2.3").
can_parse_version_with_lt_operator_test() -> [{lt, {1,2,3, <<>>}}] = versions:constraints("<1.2.3").
can_parse_version_with_gt_operator_test() -> [{gt, {1,2,3, <<>>}}] = versions:constraints(">1.2.3").
can_parse_version_with_lte_operator_test() -> [{lte, {1,2,3, <<>>}}] = versions:constraints("<=1.2.3").
can_parse_version_with_gte_operator_test() -> [{gte, {1,2,3, <<>>}}] = versions:constraints(">=1.2.3").
can_parse_minor_wildcard_test() -> [{gte, {1,0,0, <<>>}}, {lt, {2,0,0, <<>>}}] = versions:constraints("1.x").
can_parse_patch_wildcard_test() -> [{gte, {1,0,0, <<>>}}, {lt, {1,1,0, <<>>}}] = versions:constraints("1.0.x").
can_parse_multiple_versions_test() -> [{gt, {1,2,3, <<>>}}, {gt, {1,2,4, <<>>}}] = versions:constraints(">1.2.3 >1.2.4").
can_parse_spaces_test() -> [{gt, {1,2,3, <<>>}}] = versions:constraints("  >  1.2.3 ").




can_compare_with_lt_test() -> 
    [
        ?_assertMatch(true, versions:compare(lt, {1,0,0, <<>>}, {2,0,0, <<>>})),
        ?_assertMatch(false, versions:compare(lt, {3,0,0, <<>>}, {2,1,0, <<>>})),
        ?_assertMatch(false, versions:compare(lt, {2,0,0, <<>>}, {1,0,0, <<>>})),
        ?_assertMatch(false, versions:compare(lt, {2,0,0, <<>>}, {2,0,0, <<>>})),
        ?_assertMatch(true, versions:compare(lt, {0,1,0, <<>>}, {0,2,0, <<>>})),
        ?_assertMatch(false, versions:compare(lt, {0,2,0, <<>>}, {0,1,0, <<>>})),
        ?_assertMatch(false, versions:compare(lt, {0,2,0, <<>>}, {0,2,0, <<>>})),
        ?_assertMatch(true, versions:compare(lt, {0,0,1, <<>>}, {0,0,2, <<>>})),
        ?_assertMatch(false, versions:compare(lt, {0,0,2, <<>>}, {0,0,1, <<>>})),
        ?_assertMatch(false, versions:compare(lt, {0,0,2, <<>>}, {0,0,2, <<>>}))
    ].

can_compare_with_lte_test() -> 
    [
        ?_assertMatch(true, versions:compare(lte, {1,0,0, <<>>}, {2,0,0, <<>>})),
        ?_assertMatch(false, versions:compare(lte, {2,0,0, <<>>}, {1,0,0, <<>>})),
        ?_assertMatch(true, versions:compare(lte, {2,0,0, <<>>}, {2,0,0, <<>>})),
        ?_assertMatch(true, versions:compare(lte, {0,1,0, <<>>}, {0,2,0, <<>>})),
        ?_assertMatch(false, versions:compare(lte, {0,2,0, <<>>}, {0,1,0, <<>>})),
        ?_assertMatch(true, versions:compare(lte, {0,2,0, <<>>}, {0,2,0, <<>>})),
        ?_assertMatch(true, versions:compare(lte, {0,0,1, <<>>}, {0,0,2, <<>>})),
        ?_assertMatch(false, versions:compare(lte, {0,0,2, <<>>}, {0,0,1, <<>>})),
        ?_assertMatch(true, versions:compare(lte, {0,0,2, <<>>}, {0,0,2, <<>>}))
    ].
    

can_compare_with_gt_test() -> 
    [
        ?_assertMatch(true, versions:compare(gt, {2,0,0, <<>>}, {1,0,0, <<>>})),
        ?_assertMatch(false, versions:compare(gt, {1,0,0, <<>>}, {2,0,0, <<>>})),
        ?_assertMatch(false, versions:compare(gt, {2,0,0, <<>>}, {2,0,0, <<>>})),
        ?_assertMatch(true, versions:compare(gt, {0,2,0, <<>>}, {0,1,0, <<>>})),
        ?_assertMatch(false, versions:compare(gt, {0,1,0, <<>>}, {0,2,0, <<>>})),
        ?_assertMatch(false, versions:compare(gt, {0,2,0, <<>>}, {0,2,0, <<>>})),
        ?_assertMatch(true, versions:compare(gt, {0,0,2, <<>>}, {0,0,1, <<>>})),
        ?_assertMatch(false, versions:compare(gt, {0,0,1, <<>>}, {0,0,2, <<>>})),
        ?_assertMatch(false, versions:compare(gt, {0,0,2, <<>>}, {0,0,2, <<>>}))
    ].
    

can_compare_with_gte_test() -> 
    [
        ?_assertMatch(true, versions:compare(gte, {2,0,0, <<>>}, {1,0,0, <<>>})),
        ?_assertMatch(false, versions:compare(gte, {1,0,0, <<>>}, {2,0,0, <<>>})),
        ?_assertMatch(true, versions:compare(gte, {2,0,0, <<>>}, {2,0,0, <<>>})),
        ?_assertMatch(true, versions:compare(gte, {0,2,0, <<>>}, {0,1,0, <<>>})),
        ?_assertMatch(false, versions:compare(gte, {0,1,0, <<>>}, {0,2,0, <<>>})),
        ?_assertMatch(true, versions:compare(gte, {0,2,0, <<>>}, {0,2,0, <<>>})),
        ?_assertMatch(true, versions:compare(gte, {0,0,2, <<>>}, {0,0,1, <<>>})),
        ?_assertMatch(false, versions:compare(gte, {0,0,1, <<>>}, {0,0,2, <<>>})),
        ?_assertMatch(true, versions:compare(gte, {0,0,2, <<>>}, {0,0,2, <<>>}))
    ].
    


can_match_test() ->
    ?_assertMatch(true, versions:matches([{eq, {2,0,0, <<>>}}], {2,0,0, <<>>})).

% porting of puppet semver specs
should_parse_basic_version_strings_test_() ->
    [
        ?_assertMatch({0,0,0,<<"">>}, versions:version(<<"0.0.0">>)),
        ?_assertMatch({999,999,999,<<"">>}, versions:version(<<"999.999.999">>)),
        ?_assertMatch({0,0,0,<<"">>}, versions:version(<<"v0.0.0">>)),
        ?_assertMatch({999,999,999,<<"">>}, versions:version(<<"v999.999.999">>))
    ].

should_parse_special_version_strings_test_() ->
    [
        ?_assertMatch({0,0,0,<<"foo">>}, versions:version(<<"0.0.0-foo">>)),
        ?_assertMatch({999,999,999,<<"bar">>}, versions:version(<<"999.999.999-bar">>)),
        ?_assertMatch({0,0,0,<<"a">>}, versions:version(<<"v0.0.0-a">>)),
        ?_assertMatch({999,999,999,<<"beta">>}, versions:version(<<"v999.999.999-beta">>))
    ].

should_fail_on_invalid_version_strings_test_() ->
    [
        ?_assertException(error, _, versions:version(<<"nope">>)),
        ?_assertException(error, _, versions:version(<<"0.0foo">>)),
        ?_assertException(error, _, versions:version(<<"999.999">>)),
        ?_assertException(error, _, versions:version(<<"x0.0.0">>)),
        ?_assertException(error, _, versions:version(<<"z.z.z">>)),
        ?_assertException(error, _, versions:version(<<"1.2.3beta">>)),
        ?_assertException(error, _, versions:version(<<"1.x.y">>))
    ].

should_produce_expected_constraints_test_() ->
    [
        ?_assertMatch([{eq, {1,2,3, <<"alpha">>}}], versions:constraints("1.2.3-alpha")),
        ?_assertMatch([{eq, {1,2,3, <<>>}}], versions:constraints("1.2.3")),
        ?_assertMatch([{gt, {1,2,3, <<"alpha">>}}], versions:constraints(">1.2.3-alpha")),
        ?_assertMatch([{gt, {1,2,3, <<>>}}], versions:constraints(">1.2.3")),
        ?_assertMatch([{lt, {1,2,3, <<"alpha">>}}], versions:constraints("<1.2.3-alpha")),
        ?_assertMatch([{lt, {1,2,3, <<>>}}], versions:constraints("<1.2.3")),
        ?_assertMatch([{gte, {1,2,3, <<"alpha">>}}], versions:constraints(">=1.2.3-alpha")),
        ?_assertMatch([{gte, {1,2,3, <<>>}}], versions:constraints(">=1.2.3")),
        ?_assertMatch([{lte, {1,2,3, <<"alpha">>}}], versions:constraints("<=1.2.3-alpha")),
        ?_assertMatch([{lte, {1,2,3, <<>>}}], versions:constraints("<=1.2.3")),
        ?_assertMatch([{gt, {1,2,3, <<"a">>}}, {lt, {1,2,3, <<"b">>}}], versions:constraints(">1.2.3-a <1.2.3-b")),
        ?_assertMatch([{gt, {1,2,3, <<>>}}, {lt, {1,2,5, <<>>}}], versions:constraints(">1.2.3 <1.2.5")),
        ?_assertMatch([{gte, {1,2,3, <<"a">>}}, {lte, {1,2,3, <<"b">>}}], versions:constraints(">=1.2.3-a <=1.2.3-b")),
        ?_assertMatch([{gte, {1,2,3, <<>>}}, {lte, {1,2,5, <<>>}}], versions:constraints(">=1.2.3 <=1.2.5")),
        % ?_assertMatch([{gte, {1,2,3, <<"a">>}}, {lte, {2,3,4, <<"b">>}}], versions:constraints("1.2.3-a - 2.3.4-b")),
        % ?_assertMatch([{gte, {1,2,3, <<>>}}, {lte, {2,3,4, <<>>}}], versions:constraints("1.2.3 - 2.3.4")),
        ?_assertMatch([{gte, {1,2,3, <<>>}}, {lt, {1,3,0, <<>>}}], versions:constraints("~1.2.3")),
        ?_assertMatch([{gte, {1,2,0, <<>>}}, {lt, {2,0,0, <<>>}}], versions:constraints("~1.2")),
        ?_assertMatch([{gte, {1,0,0, <<>>}}, {lt, {2,0,0, <<>>}}], versions:constraints("~1")),
        ?_assertMatch([{gte, {1,2,0, <<>>}}, {lt, {1,3,0, <<>>}}], versions:constraints("1.2.x")),
        ?_assertMatch([{gte, {1,0,0, <<>>}}, {lt, {2,0,0, <<>>}}], versions:constraints("1.x"))
    ].

should_suit_up_test_() ->
    [
        ?_assertNot(versions:matches(versions:constraints("1.2.3"), versions:version(<<"v1.2.2">>))),
        ?_assertNot(versions:matches(versions:constraints(">=1.2.3"), versions:version(<<"v1.2.2">>))),
        ?_assert(versions:matches(versions:constraints("<=1.2.3"), versions:version(<<"v1.2.2">>))),
        ?_assertNot(versions:matches(versions:constraints(">= 1.2.3"), versions:version(<<"v1.2.2">>))),
        ?_assert(versions:matches(versions:constraints("<= 1.2.3"), versions:version(<<"v1.2.2">>))),
        % ?_assertNot(versions:matches(versions:constraints("1.2.3 - 1.2.4"), versions:version(<<"v1.2.2">>))),
        ?_assertNot(versions:matches(versions:constraints("~1.2.3"), versions:version(<<"v1.2.2">>))),
        ?_assert(versions:matches(versions:constraints("~1.2"), versions:version(<<"v1.2.2">>))),
        ?_assert(versions:matches(versions:constraints("~1"), versions:version(<<"v1.2.2">>))),
        ?_assert(versions:matches(versions:constraints("1.2.x"), versions:version(<<"v1.2.2">>))),
        ?_assert(versions:matches(versions:constraints("1.x"), versions:version(<<"v1.2.2">>))),

        ?_assert(versions:matches(versions:constraints("1.2.3"), versions:version(<<"v1.2.3-alpha">>))),
        ?_assert(versions:matches(versions:constraints(">=1.2.3"), versions:version(<<"v1.2.3-alpha">>))),
        ?_assert(versions:matches(versions:constraints("<=1.2.3"), versions:version(<<"v1.2.3-alpha">>))),
        ?_assert(versions:matches(versions:constraints(">= 1.2.3"), versions:version(<<"v1.2.3-alpha">>))),
        ?_assert(versions:matches(versions:constraints("<= 1.2.3"), versions:version(<<"v1.2.3-alpha">>))),
        ?_assertNot(versions:matches(versions:constraints(">1.2.3"), versions:version(<<"v1.2.3-alpha">>))),
        ?_assertNot(versions:matches(versions:constraints("<1.2.3"), versions:version(<<"v1.2.3-alpha">>))),
        ?_assertNot(versions:matches(versions:constraints("> 1.2.3"), versions:version(<<"v1.2.3-alpha">>))),
        ?_assertNot(versions:matches(versions:constraints("< 1.2.3"), versions:version(<<"v1.2.3-alpha">>))),
        % ?_assert(versions:matches(versions:constraints("1.2.3 - 1.2.4"), versions:version(<<"v1.2.3-alpha">>))),
        % ?_assert(versions:matches(versions:constraints("1.2.3 - 1.2.4"), versions:version(<<"v1.2.4-alpha">>))),
        % ?_assertNot(versions:matches(versions:constraints("1.2.3 - 1.2.4"), versions:version(<<"v1.2.5-alpha">>))),
        ?_assert(versions:matches(versions:constraints("~1.2.3"), versions:version(<<"v1.2.3-alpha">>))),
        ?_assertNot(versions:matches(versions:constraints("~1.2.3"), versions:version(<<"v1.3.0-alpha">>))),
        ?_assert(versions:matches(versions:constraints("~1.2"), versions:version(<<"v1.2.3-alpha">>))),
        ?_assertNot(versions:matches(versions:constraints("~1.2"), versions:version(<<"v2.0.0-alpha">>))),
        ?_assert(versions:matches(versions:constraints("~1"), versions:version(<<"v1.2.3-alpha">>))),
        ?_assertNot(versions:matches(versions:constraints("~1"), versions:version(<<"v2.0.0-alpha">>))),
        ?_assert(versions:matches(versions:constraints("1.2.x"), versions:version(<<"v1.2.3-alpha">>))),
        ?_assertNot(versions:matches(versions:constraints("1.2.x"), versions:version(<<"v1.3.0-alpha">>))),
        ?_assert(versions:matches(versions:constraints("1.x"), versions:version(<<"v1.2.3-alpha">>))),
        ?_assertNot(versions:matches(versions:constraints("1.x"), versions:version(<<"v2.0.0-alpha">>))),

        ?_assert(versions:matches(versions:constraints("1.2.3"), versions:version(<<"v1.2.3">>))),
        ?_assert(versions:matches(versions:constraints(">=1.2.3"), versions:version(<<"v1.2.3">>))),
        ?_assert(versions:matches(versions:constraints("<=1.2.3"), versions:version(<<"v1.2.3">>))),
        ?_assert(versions:matches(versions:constraints(">= 1.2.3"), versions:version(<<"v1.2.3">>))),
        ?_assert(versions:matches(versions:constraints("<= 1.2.3"), versions:version(<<"v1.2.3">>))),
        % ?_assert(versions:matches(versions:constraints("1.2.3 - 1.2.4"), versions:version(<<"v1.2.3">>))),
        ?_assert(versions:matches(versions:constraints("~1.2.3"), versions:version(<<"v1.2.3">>))),
        ?_assert(versions:matches(versions:constraints("~1.2"), versions:version(<<"v1.2.3">>))),
        ?_assert(versions:matches(versions:constraints("~1"), versions:version(<<"v1.2.3">>))),
        ?_assert(versions:matches(versions:constraints("1.2.x"), versions:version(<<"v1.2.3">>))),
        ?_assert(versions:matches(versions:constraints("1.x"), versions:version(<<"v1.2.3">>))),

        ?_assertNot(versions:matches(versions:constraints("1.2.3"), versions:version(<<"v1.2.4">>))),
        ?_assert(versions:matches(versions:constraints(">=1.2.3"), versions:version(<<"v1.2.4">>))),
        ?_assertNot(versions:matches(versions:constraints("<=1.2.3"), versions:version(<<"v1.2.4">>))),
        ?_assert(versions:matches(versions:constraints(">= 1.2.3"), versions:version(<<"v1.2.4">>))),
        ?_assertNot(versions:matches(versions:constraints("<= 1.2.3"), versions:version(<<"v1.2.4">>))),
        % ?_assert(versions:matches(versions:constraints("1.2.3 - 1.2.4"), versions:version(<<"v1.2.4">>))),
        ?_assert(versions:matches(versions:constraints("~1.2.3"), versions:version(<<"v1.2.4">>))),
        ?_assert(versions:matches(versions:constraints("~1.2"), versions:version(<<"v1.2.4">>))),
        ?_assert(versions:matches(versions:constraints("~1"), versions:version(<<"v1.2.4">>))),
        ?_assert(versions:matches(versions:constraints("1.2.x"), versions:version(<<"v1.2.4">>))),
        ?_assert(versions:matches(versions:constraints("1.x"), versions:version(<<"v1.2.4">>)))
    ].

comparisons_on_a_basic_version_test_() ->
    [
        ?_assert(versions:compare(eq, versions:version(<<"v1.2.3">>), versions:version(<<"1.2.3">>))),
        ?_assert(versions:compare(gt, versions:version(<<"v1.2.3">>), versions:version(<<"0.2.3">>))),
        ?_assert(versions:compare(lt, versions:version(<<"v1.2.3">>), versions:version(<<"2.2.3">>))),
        ?_assert(versions:compare(gt, versions:version(<<"v1.2.3">>), versions:version(<<"1.1.3">>))),
        ?_assert(versions:compare(lt, versions:version(<<"v1.2.3">>), versions:version(<<"1.3.3">>))),
        ?_assert(versions:compare(gt, versions:version(<<"v1.2.3">>), versions:version(<<"1.2.2">>))),
        ?_assert(versions:compare(lt, versions:version(<<"v1.2.3">>), versions:version(<<"1.2.4">>))),
        ?_assert(versions:compare(gt, versions:version(<<"v1.2.3">>), versions:version(<<"1.2.3-beta">>))),
        ?_assert(versions:compare(lt, versions:version(<<"v1.2.3">>), versions:version(<<"1.2.4-beta">>)))
    ].

comparisons_on_a_special_version_test_() ->
    [
        ?_assert(versions:compare(eq, versions:version(<<"v1.2.3-beta">>), versions:version(<<"1.2.3-beta">>))),
        ?_assert(versions:compare(lt, versions:version(<<"v1.2.3-beta">>), versions:version(<<"1.2.3">>))),
        ?_assert(versions:compare(gt, versions:version(<<"v1.2.3-beta">>), versions:version(<<"0.2.3">>))),
        ?_assert(versions:compare(lt, versions:version(<<"v1.2.3-beta">>), versions:version(<<"2.2.3">>))),
        ?_assert(versions:compare(gt, versions:version(<<"v1.2.3-beta">>), versions:version(<<"1.1.3">>))),
        ?_assert(versions:compare(lt, versions:version(<<"v1.2.3-beta">>), versions:version(<<"1.3.3">>))),
        ?_assert(versions:compare(gt, versions:version(<<"v1.2.3-beta">>), versions:version(<<"1.2.2">>))),
        ?_assert(versions:compare(lt, versions:version(<<"v1.2.3-beta">>), versions:version(<<"1.2.4">>))),
        ?_assert(versions:compare(gt, versions:version(<<"v1.2.3-beta">>), versions:version(<<"1.2.3-alpha">>))),
        ?_assert(versions:compare(lt, versions:version(<<"v1.2.3-beta">>), versions:version(<<"1.2.3-beta2">>)))
    ].
