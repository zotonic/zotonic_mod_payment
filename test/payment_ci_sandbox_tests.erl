%% @doc CI smoke test: install payment and the selected PSP in Zotonic's test site.
-module(payment_ci_sandbox_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

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
    wait_templates(Target, Context, 300),
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


%% Activation can overlap an older scan. The indexer ignores reindex requests
%% while scanning, so retry once a second until the selected module is indexed.
wait_templates(Target, Context, N) ->
    Page = z_module_indexer:find(template, <<"admin_payment_config.tpl">>, Context),
    Psps = z_module_indexer:find_all(template, <<"_admin_payment_config_psp.tpl">>, Context),
    Modules = [M || #module_index{module = M} <- Psps],
    HasPsp = Target =:= "payment" orelse lists:member(psp_module(Target), Modules),
    case {Page, HasPsp, N} of
        {{ok, _}, true, _} -> ok;
        {_, _, 0} ->
            error({payment_templates_not_indexed, #{
                target => Target,
                page => Page,
                psp_templates => Modules,
                active_modules => z_module_manager:active(Context)
            }});
        _ ->
            case N rem 10 of
                0 -> z_module_indexer:reindex(Context);
                _ -> ok
            end,
            timer:sleep(100),
            wait_templates(Target, Context, N - 1)
    end.

%% Simulate an older scan finishing with another PSP's template, followed by
%% a reindex request lost during that scan. A later request must recover.
template_reindex_retry_test() ->
    Context = #context{},
    Page = #module_index{
        key = #module_index_key{site = default, type = template, name = <<"admin_payment_config.tpl">>},
        module = mod_payment
    },
    Psp = Page#module_index{
        key = #module_index_key{site = default, type = template, name = <<"_admin_payment_config_psp.tpl">>}
    },
    meck:new(z_module_indexer, [passthrough, no_link]),
    put(reindex_requests, 0),
    try
        payment_test_mock:expect(z_module_indexer, find, psp,
            fun(template, _, _) -> {ok, Page} end),
        payment_test_mock:expect(z_module_indexer, find_all, psp,
            fun(template, _, _) ->
                Module = case get(reindex_requests) >= 2 of
                    true -> mod_payment_stripe;
                    false -> mod_payment_mollie
                end,
                [Psp#module_index{module = Module}]
            end),
        payment_test_mock:expect(z_module_indexer, reindex, psp,
            fun(_) -> put(reindex_requests, get(reindex_requests) + 1), ok end),
        ?assertEqual(ok, wait_templates("stripe", Context, 20)),
        ?assertEqual(2, get(reindex_requests))
    after
        erase(reindex_requests),
        meck:unload(z_module_indexer)
    end.
