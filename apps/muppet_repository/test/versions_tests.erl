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
    true = versions:compare(lt, {1,0,0, <<>>}, {2,0,0, <<>>}),
    false = versions:compare(lt, {3,0,0, <<>>}, {2,1,0, <<>>}),
    false = versions:compare(lt, {2,0,0, <<>>}, {1,0,0, <<>>}),
    false = versions:compare(lt, {2,0,0, <<>>}, {2,0,0, <<>>}),
    true = versions:compare(lt, {0,1,0, <<>>}, {0,2,0, <<>>}),
    false = versions:compare(lt, {0,2,0, <<>>}, {0,1,0, <<>>}),
    false = versions:compare(lt, {0,2,0, <<>>}, {0,2,0, <<>>}),
    true = versions:compare(lt, {0,0,1, <<>>}, {0,0,2, <<>>}),
    false = versions:compare(lt, {0,0,2, <<>>}, {0,0,1, <<>>}),
    false = versions:compare(lt, {0,0,2, <<>>}, {0,0,2, <<>>}).

can_compare_with_lte_test() -> 
    true = versions:compare(lte, {1,0,0, <<>>}, {2,0,0, <<>>}),
    false = versions:compare(lte, {2,0,0, <<>>}, {1,0,0, <<>>}),
    true = versions:compare(lte, {2,0,0, <<>>}, {2,0,0, <<>>}),
    true = versions:compare(lte, {0,1,0, <<>>}, {0,2,0, <<>>}),
    false = versions:compare(lte, {0,2,0, <<>>}, {0,1,0, <<>>}),
    true = versions:compare(lte, {0,2,0, <<>>}, {0,2,0, <<>>}),
    true = versions:compare(lte, {0,0,1, <<>>}, {0,0,2, <<>>}),
    false = versions:compare(lte, {0,0,2, <<>>}, {0,0,1, <<>>}),
    true = versions:compare(lte, {0,0,2, <<>>}, {0,0,2, <<>>}).
    

can_compare_with_gt_test() -> 
    true = versions:compare(gt, {2,0,0, <<>>}, {1,0,0, <<>>}),
    false = versions:compare(gt, {1,0,0, <<>>}, {2,0,0, <<>>}),
    false = versions:compare(gt, {2,0,0, <<>>}, {2,0,0, <<>>}),
    true = versions:compare(gt, {0,2,0, <<>>}, {0,1,0, <<>>}),
    false = versions:compare(gt, {0,1,0, <<>>}, {0,2,0, <<>>}),
    false = versions:compare(gt, {0,2,0, <<>>}, {0,2,0, <<>>}),
    true = versions:compare(gt, {0,0,2, <<>>}, {0,0,1, <<>>}),
    false = versions:compare(gt, {0,0,1, <<>>}, {0,0,2, <<>>}),
    false = versions:compare(gt, {0,0,2, <<>>}, {0,0,2, <<>>}).
    

can_compare_with_gte_test() -> 
    true = versions:compare(gte, {2,0,0, <<>>}, {1,0,0, <<>>}),
    false = versions:compare(gte, {1,0,0, <<>>}, {2,0,0, <<>>}),
    true = versions:compare(gte, {2,0,0, <<>>}, {2,0,0, <<>>}),
    true = versions:compare(gte, {0,2,0, <<>>}, {0,1,0, <<>>}),
    false = versions:compare(gte, {0,1,0, <<>>}, {0,2,0, <<>>}),
    true = versions:compare(gte, {0,2,0, <<>>}, {0,2,0, <<>>}),
    true = versions:compare(gte, {0,0,2, <<>>}, {0,0,1, <<>>}),
    false = versions:compare(gte, {0,0,1, <<>>}, {0,0,2, <<>>}),
    true = versions:compare(gte, {0,0,2, <<>>}, {0,0,2, <<>>}).
    


can_match_test() ->
    true = versions:matches([{eq, {2,0,0, <<>>}}], {2,0,0, <<>>}).

% porting of puppet semver specs
should_parse_basic_version_strings_test_() ->
    [
        ?_assert({0,0,0,<<"">>} =:=versions:version(<<"0.0.0">>)),
        ?_assert({999,999,999,<<"">>} =:=versions:version(<<"999.999.999">>)),
        ?_assert({0,0,0,<<"">>} =:=versions:version(<<"v0.0.0">>)),
        ?_assert({999,999,999,<<"">>} =:=versions:version(<<"v999.999.999">>))
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
        ?_assertException(_T, _D, versions:version(<<"nope">>)),
        ?_assertException(_T, _D, versions:version(<<"0.0foo">>)),
        ?_assertException(_T, _D, versions:version(<<"999.999">>)),
        ?_assertException(_T, _D, versions:version(<<"x0.0.0">>)),
        ?_assertException(_T, _D, versions:version(<<"z.z.z">>)),
        ?_assertException(_T, _D, versions:version(<<"1.2.3beta">>)),
        ?_assertException(_T, _D, versions:version(<<"1.x.y">>))
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

%   describe 'instantiation' do
%     it 'should raise an exception when passed an invalid version string' do
%       expect { SemVer.new('invalidVersion') }.to raise_exception ArgumentError
%     end

%     it 'should populate the appropriate fields for a basic version string' do
%       version = SemVer.new('1.2.3')
%       version.major.should   == 1
%       version.minor.should   == 2
%       version.tiny.should    == 3
%       version.special.should == ''
%     end

%     it 'should populate the appropriate fields for a special version string' do
%       version = SemVer.new('3.4.5-beta6')
%       version.major.should   == 3
%       version.minor.should   == 4
%       version.tiny.should    == 5
%       version.special.should == '-beta6'
%     end
%   end

%   describe '#matched_by?' do
%     subject { SemVer.new('v1.2.3-beta') }

%     describe 'should match against' do
%       describe 'literal version strings' do
%         it { should be_matched_by('1.2.3-beta') }

%         it { should_not be_matched_by('1.2.3-alpha') }
%         it { should_not be_matched_by('1.2.4-beta') }
%         it { should_not be_matched_by('1.3.3-beta') }
%         it { should_not be_matched_by('2.2.3-beta') }
%       end

%       describe 'partial version strings' do
%         it { should be_matched_by('1.2.3') }
%         it { should be_matched_by('1.2') }
%         it { should be_matched_by('1') }
%       end

%       describe 'version strings with placeholders' do
%         it { should be_matched_by('1.2.x') }
%         it { should be_matched_by('1.x.3') }
%         it { should be_matched_by('1.x.x') }
%         it { should be_matched_by('1.x') }
%       end
%     end
%   end

%   describe 'comparisons' do
%     describe 'against a string' do
%       it 'should just work' do
%         SemVer.new('1.2.3').should == '1.2.3'
%       end
%     end

%     describe 'against a symbol' do
%       it 'should just work' do
%         SemVer.new('1.2.3').should == :'1.2.3'
%       end
%     end

%     describe 'on a basic version (v1.2.3)' do
%       subject { SemVer.new('v1.2.3') }

%       it { should == SemVer.new('1.2.3') }

%       # Different major versions
%       it { should > SemVer.new('0.2.3') }
%       it { should < SemVer.new('2.2.3') }

%       # Different minor versions
%       it { should > SemVer.new('1.1.3') }
%       it { should < SemVer.new('1.3.3') }

%       # Different tiny versions
%       it { should > SemVer.new('1.2.2') }
%       it { should < SemVer.new('1.2.4') }

%       # Against special versions
%       it { should > SemVer.new('1.2.3-beta') }
%       it { should < SemVer.new('1.2.4-beta') }
%     end

%     describe 'on a special version (v1.2.3-beta)' do
%       subject { SemVer.new('v1.2.3-beta') }

%       it { should == SemVer.new('1.2.3-beta') }

%       # Same version, final release
%       it { should < SemVer.new('1.2.3') }

%       # Different major versions
%       it { should > SemVer.new('0.2.3') }
%       it { should < SemVer.new('2.2.3') }

%       # Different minor versions
%       it { should > SemVer.new('1.1.3') }
%       it { should < SemVer.new('1.3.3') }

%       # Different tiny versions
%       it { should > SemVer.new('1.2.2') }
%       it { should < SemVer.new('1.2.4') }

%       # Against special versions
%       it { should > SemVer.new('1.2.3-alpha') }
%       it { should < SemVer.new('1.2.3-beta2') }
%     end
%   end
% end

