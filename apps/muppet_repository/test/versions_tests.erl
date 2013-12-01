-module(versions_tests).
-include_lib("eunit/include/eunit.hrl").



can_parse_version_test() -> [{eq, {1,2,3, undefined}}] = versions:constraints("1.2.3").
can_parse_version_with_lt_operator_test() -> [{lt, {1,2,3, undefined}}] = versions:constraints("<1.2.3").
can_parse_version_with_gt_operator_test() -> [{gt, {1,2,3, undefined}}] = versions:constraints(">1.2.3").
can_parse_version_with_lte_operator_test() -> [{lte, {1,2,3, undefined}}] = versions:constraints("<=1.2.3").
can_parse_version_with_gte_operator_test() -> [{gte, {1,2,3, undefined}}] = versions:constraints(">=1.2.3").
can_parse_minor_wildcard_test() -> [{gte, {1,0,0, undefined}}, {lt, {2,0,0, undefined}}] = versions:constraints("1.x").
can_parse_patch_wildcard_test() -> [{gte, {1,0,0, undefined}}, {lt, {1,1,0, undefined}}] = versions:constraints("1.0.x").
can_parse_multiple_versions_test() -> [{gt, {1,2,3, undefined}}, {gt, {1,2,4, undefined}}] = versions:constraints(">1.2.3 >1.2.4").
can_parse_spaces_test() -> [{gt, {1,2,3, undefined}}] = versions:constraints("  >  1.2.3 ").




can_compare_with_lt_test() -> 
    true = versions:compare(lt, {1,0,0, undefined}, {2,0,0, undefined}),
    false = versions:compare(lt, {3,0,0, undefined}, {2,1,0, undefined}),
    false = versions:compare(lt, {2,0,0, undefined}, {1,0,0, undefined}),
    false = versions:compare(lt, {2,0,0, undefined}, {2,0,0, undefined}),
    true = versions:compare(lt, {0,1,0, undefined}, {0,2,0, undefined}),
    false = versions:compare(lt, {0,2,0, undefined}, {0,1,0, undefined}),
    false = versions:compare(lt, {0,2,0, undefined}, {0,2,0, undefined}),
    true = versions:compare(lt, {0,0,1, undefined}, {0,0,2, undefined}),
    false = versions:compare(lt, {0,0,2, undefined}, {0,0,1, undefined}),
    false = versions:compare(lt, {0,0,2, undefined}, {0,0,2, undefined}).

can_compare_with_lte_test() -> 
    true = versions:compare(lte, {1,0,0, undefined}, {2,0,0, undefined}),
    false = versions:compare(lte, {2,0,0, undefined}, {1,0,0, undefined}),
    true = versions:compare(lte, {2,0,0, undefined}, {2,0,0, undefined}),
    true = versions:compare(lte, {0,1,0, undefined}, {0,2,0, undefined}),
    false = versions:compare(lte, {0,2,0, undefined}, {0,1,0, undefined}),
    true = versions:compare(lte, {0,2,0, undefined}, {0,2,0, undefined}),
    true = versions:compare(lte, {0,0,1, undefined}, {0,0,2, undefined}),
    false = versions:compare(lte, {0,0,2, undefined}, {0,0,1, undefined}),
    true = versions:compare(lte, {0,0,2, undefined}, {0,0,2, undefined}).
    

can_compare_with_gt_test() -> 
    true = versions:compare(gt, {2,0,0, undefined}, {1,0,0, undefined}),
    false = versions:compare(gt, {1,0,0, undefined}, {2,0,0, undefined}),
    false = versions:compare(gt, {2,0,0, undefined}, {2,0,0, undefined}),
    true = versions:compare(gt, {0,2,0, undefined}, {0,1,0, undefined}),
    false = versions:compare(gt, {0,1,0, undefined}, {0,2,0, undefined}),
    false = versions:compare(gt, {0,2,0, undefined}, {0,2,0, undefined}),
    true = versions:compare(gt, {0,0,2, undefined}, {0,0,1, undefined}),
    false = versions:compare(gt, {0,0,1, undefined}, {0,0,2, undefined}),
    false = versions:compare(gt, {0,0,2, undefined}, {0,0,2, undefined}).
    

can_compare_with_gte_test() -> 
    true = versions:compare(gte, {2,0,0, undefined}, {1,0,0, undefined}),
    false = versions:compare(gte, {1,0,0, undefined}, {2,0,0, undefined}),
    true = versions:compare(gte, {2,0,0, undefined}, {2,0,0, undefined}),
    true = versions:compare(gte, {0,2,0, undefined}, {0,1,0, undefined}),
    false = versions:compare(gte, {0,1,0, undefined}, {0,2,0, undefined}),
    true = versions:compare(gte, {0,2,0, undefined}, {0,2,0, undefined}),
    true = versions:compare(gte, {0,0,2, undefined}, {0,0,1, undefined}),
    false = versions:compare(gte, {0,0,1, undefined}, {0,0,2, undefined}),
    true = versions:compare(gte, {0,0,2, undefined}, {0,0,2, undefined}).
    


can_match_test() ->
    true = versions:matches([{eq, {2,0,0, undefined}}], {2,0,0, undefined}).

