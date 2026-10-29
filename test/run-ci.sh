#!/usr/bin/env bash
# Run every selected payment module test in a live Zotonic test sandbox.
set -euo pipefail
cd "$(dirname "$0")/../../.."
target=${1:?Usage: run-ci.sh payment|mollie|stripe|paypal|buckaroo|sovergn}
case "$target" in
    payment)
        app=zotonic_mod_payment
        : "${PAYMENT_TEST_PORT:?Set PAYMENT_TEST_PORT so DB tests cannot silently skip}"
        unset PAYMENT_TEST_PSP
        ;;
    mollie|stripe|paypal|buckaroo|sovergn)
        app="zotonic_mod_payment_$target"
        export PAYMENT_TEST_PSP="$target"
        ;;
    *) echo "Unknown test target: $target" >&2; exit 2;;
esac
build_dir=$(mktemp -d "${TMPDIR:-/tmp}/payment-ci.XXXXXX")
trap 'rm -rf "$build_dir"' EXIT
export ERL_LIBS="$PWD/_build/default/lib"
export PAYMENT_CI_TARGET="$target"
export PAYMENT_CI_APP="$app"
export PAYMENT_CONTRACT_DIR="$PWD/apps_user/zotonic_mod_payment/test/psp_contracts"
export ZOTONIC="$PWD"

# Compile ordinary production code and separate test files. No TEST macro is
# required: the helpers used by tests are explicitly exported by the modules.
apps=(zotonic_mod_payment)
if [[ "$target" != payment ]]; then apps+=("$app"); fi
for module_app in "${apps[@]}"; do
    sources=()
    while IFS= read -r -d '' source; do sources+=("$source"); done < <(
        find "apps_user/$module_app/src" -name '*.erl' -print0
    )
    erlc +debug_info -I apps -I apps_user -I "apps_user/$module_app/include" \
        -o "$build_dir" "${sources[@]}"
    shopt -s nullglob
    tests=("apps_user/$module_app/test/"*.erl)
    if [[ ${#tests[@]} -gt 0 ]]; then
        erlc +debug_info -I apps_user -I "apps_user/$module_app/include" \
            -o "$build_dir" "${tests[@]}"
    fi
done

# Explicit discovery includes every test file; the stock runtests CLI only
# discovers apps/, not apps_user/. PSP contract tests need the selected PSP;
# the independent mod_payment job runs all other mod_payment test files.
erl -pa "$PWD"/_build/default/lib/*/ebin -pa "$build_dir" \
    -sname zotonic001_testsandbox -sasl errlog_type error -s zotonic -noshell \
    -eval 'spawn(fun() -> receive after 300000 -> io:format(standard_error, "Sandbox tests timed out~n", []), halt(1) end end),
        App = os:getenv("PAYMENT_CI_APP"),
        Files = filelib:wildcard("apps_user/" ++ App ++ "/test/*_tests.erl"),
        Tests = [list_to_atom(filename:basename(F, ".erl")) || F <- Files],
        Selected = case os:getenv("PAYMENT_CI_TARGET") of
            "payment" -> Tests -- [payment_ci_sandbox_tests, payment_psp_contract_tests];
            _ -> Tests ++ [payment_psp_contract_tests]
        end,
        HealthCheck = fun() ->
            C = z_context:new(zotonic_site_testsandbox),
            1 = z_db:q1("select 1", C),
            true = z_module_manager:active(mod_payment, C)
        end,
        zotonic:runtests([payment_ci_sandbox_tests | Selected] ++ [HealthCheck]).'
