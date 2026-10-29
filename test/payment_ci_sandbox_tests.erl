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
    end,
    z_module_indexer:reindex(Context),
    wait_templates(Target, Context, 100),
    Rendered = z_template:render_block(content, "admin_payment_config.tpl", [], Context),
    {Output, _} = z_render:output(Rendered, Context),
    Html = iolist_to_binary(Output),
    ?assertNotEqual(nomatch, binary:match(Html, <<"Payment configuration">>)),
    ?assertNotEqual(nomatch, binary:match(Html, <<"accept_unknown_payments">>)),
    case Target of
        "payment" -> ok;
        _ ->
            ?assertNotEqual(nomatch, binary:match(Html, atom_to_binary(psp_module(Target)))),
            ?assertNotEqual(nomatch, binary:match(Html, <<"model/password/post/toggle">>)),
            ?assertEqual(nomatch, binary:match(Html, <<"name=\"clear_">>))
    end,
    case Target of
        "mollie" ->
            ?assertMatch({match, _}, re:run(Html, <<"<select[^>]+name=\"recurring_payment_interval\"">>)),
            ?assertNotEqual(nomatch, binary:match(Html, <<"value=\"monthly\"">>)),
            ?assertNotEqual(nomatch, binary:match(Html, <<"value=\"yearly\"">>));
        "sovergn" ->
            ?assertMatch({match, _}, re:run(Html, <<"type=\"radio\" name=\"environment\" value=\"test\" checked">>)),
            ?assertMatch({match, _}, re:run(Html, <<"type=\"radio\" name=\"environment\" value=\"live\"">>));
        _ -> ok
    end.

psp_module("mollie") -> mod_payment_mollie;
psp_module("stripe") -> mod_payment_stripe;
psp_module("paypal") -> mod_payment_paypal;
psp_module("buckaroo") -> mod_payment_buckaroo;
psp_module("sovergn") -> mod_payment_sovergn.


%% Module activation finishes before the asynchronous template scan.
wait_templates(Target, Context, N) when N > 0 ->
    Page = z_module_indexer:find(template, <<"admin_payment_config.tpl">>, Context),
    Psps = z_module_indexer:find_all(template, <<"_admin_payment_config_psp.tpl">>, Context),
    case {Page, Target =:= "payment" orelse Psps =/= []} of
        {{ok, _}, true} -> ok;
        _ -> timer:sleep(100), wait_templates(Target, Context, N - 1)
    end;
wait_templates(_, _, 0) -> error(payment_templates_not_indexed).
