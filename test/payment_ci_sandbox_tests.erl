%% @doc CI smoke test: install payment and the selected PSP in Zotonic's test site.
-module(payment_ci_sandbox_tests).
-include_lib("eunit/include/eunit.hrl").

sandbox_test_() ->
    case os:getenv("PAYMENT_CI_TARGET") of
        false -> [];
        Target -> {timeout, 120, fun() -> install(Target) end}
    end.

install(Target) ->
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ?assertEqual(ok, z_module_manager:activate_await(mod_payment, Context)),
    ?assert(z_db:table_exists(payment, Context)),
    ?assert(z_db:table_exists(payment_log, Context)),
    case Target of
        "payment" -> ok;
        _ ->
            Module = psp_module(Target),
            ?assertEqual(ok, z_module_manager:activate_await(Module, Context)),
            ?assert(z_module_manager:active(Module, Context))
    end.

psp_module("mollie") -> mod_payment_mollie;
psp_module("stripe") -> mod_payment_stripe;
psp_module("paypal") -> mod_payment_paypal;
psp_module("buckaroo") -> mod_payment_buckaroo;
psp_module("sovergn") -> mod_payment_sovergn.
