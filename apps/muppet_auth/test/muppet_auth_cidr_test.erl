-module(muppet_auth_cidr_test).

can_match_a_cidr_test() ->
    Spec = muppet_auth_cidr:cidr_to_prefix({192,168,0,0}, 16),
    true = muppet_auth_cidr:in_whitelist(Spec, {192, 168, 1, 51}).