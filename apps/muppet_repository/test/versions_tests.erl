-module(versions_tests).
-include_lib("eunit/include/eunit.hrl").



can_parse_version_test() -> [{eq, {1,2,3}}] = versions:constraints("1.2.3").
can_parse_version_with_lt_operator_test() -> [{lt, {1,2,3}}] = versions:constraints("<1.2.3").
can_parse_version_with_gt_operator_test() -> [{gt, {1,2,3}}] = versions:constraints(">1.2.3").
can_parse_version_with_lte_operator_test() -> [{lte, {1,2,3}}] = versions:constraints("<=1.2.3").
can_parse_version_with_gte_operator_test() -> [{gte, {1,2,3}}] = versions:constraints(">=1.2.3").
can_parse_minor_wildcard_test() -> [{gte, {1,0,0}}, {lt, {2,0,0}}] = versions:constraints("1.x").
can_parse_patch_wildcard_test() -> [{gte, {1,0,0}}, {lt, {1,1,0}}] = versions:constraints("1.0.x").
can_parse_multiple_versions_test() -> [{gt, {1,2,3}}, {gt, {1,2,4}}] = versions:constraints(">1.2.3 >1.2.4").
can_parse_spaces_test() -> [{gt, {1,2,3}}] = versions:constraints("  >  1.2.3 ").




can_compare_with_lt_test() -> 
    true = versions:compare(lt, {1,0,0}, {2,0,0}),
    false = versions:compare(lt, {3,0,0}, {2,1,0}),
    false = versions:compare(lt, {2,0,0}, {1,0,0}),
    false = versions:compare(lt, {2,0,0}, {2,0,0}),
    true = versions:compare(lt, {0,1,0}, {0,2,0}),
    false = versions:compare(lt, {0,2,0}, {0,1,0}),
    false = versions:compare(lt, {0,2,0}, {0,2,0}),
    true = versions:compare(lt, {0,0,1}, {0,0,2}),
    false = versions:compare(lt, {0,0,2}, {0,0,1}),
    false = versions:compare(lt, {0,0,2}, {0,0,2}).

can_compare_with_lte_test() -> 
    true = versions:compare(lte, {1,0,0}, {2,0,0}),
    false = versions:compare(lte, {2,0,0}, {1,0,0}),
    true = versions:compare(lte, {2,0,0}, {2,0,0}),
    true = versions:compare(lte, {0,1,0}, {0,2,0}),
    false = versions:compare(lte, {0,2,0}, {0,1,0}),
    true = versions:compare(lte, {0,2,0}, {0,2,0}),
    true = versions:compare(lte, {0,0,1}, {0,0,2}),
    false = versions:compare(lte, {0,0,2}, {0,0,1}),
    true = versions:compare(lte, {0,0,2}, {0,0,2}).
    

can_compare_with_gt_test() -> 
    true = versions:compare(gt, {2,0,0}, {1,0,0}),
    false = versions:compare(gt, {1,0,0}, {2,0,0}),
    false = versions:compare(gt, {2,0,0}, {2,0,0}),
    true = versions:compare(gt, {0,2,0}, {0,1,0}),
    false = versions:compare(gt, {0,1,0}, {0,2,0}),
    false = versions:compare(gt, {0,2,0}, {0,2,0}),
    true = versions:compare(gt, {0,0,2}, {0,0,1}),
    false = versions:compare(gt, {0,0,1}, {0,0,2}),
    false = versions:compare(gt, {0,0,2}, {0,0,2}).
    

can_compare_with_gte_test() -> 
    true = versions:compare(gte, {2,0,0}, {1,0,0}),
    false = versions:compare(gte, {1,0,0}, {2,0,0}),
    true = versions:compare(gte, {2,0,0}, {2,0,0}),
    true = versions:compare(gte, {0,2,0}, {0,1,0}),
    false = versions:compare(gte, {0,1,0}, {0,2,0}),
    true = versions:compare(gte, {0,2,0}, {0,2,0}),
    true = versions:compare(gte, {0,0,2}, {0,0,1}),
    false = versions:compare(gte, {0,0,1}, {0,0,2}),
    true = versions:compare(gte, {0,0,2}, {0,0,2}).
    


can_match_test() ->
    true = versions:matches([{eq, {2,0,0}}], {2,0,0}).

