#!/usr/bin/env bash
# Run in a compiled Zotonic checkout with PostgreSQL available.
set -euo pipefail
cd "$(dirname "$0")/../../.."
target=${1:?Usage: run-ci.sh payment|mollie|stripe|paypal|buckaroo|sovergn}
case "$target" in
    payment) app=zotonic_mod_payment;;
    mollie|stripe|paypal|buckaroo|sovergn) app="zotonic_mod_payment_$target";;
    *) echo "Unknown test target: $target" >&2; exit 2;;
esac
build_dir=$(mktemp -d "${TMPDIR:-/tmp}/payment-ci.XXXXXX")
trap 'rm -rf "$build_dir"' EXIT
export ERL_LIBS="$PWD/_build/default/lib"
export PAYMENT_CI_TARGET="$target"
export PAYMENT_CI_APP="$app"
export ZOTONIC="$PWD"

# Explicitly select tests: the stock runtests CLI only discovers apps/, not apps_user/.
if [[ "$target" == payment ]]; then
    tests=(apps_user/zotonic_mod_payment/test/mod_payment_tests.erl
           apps_user/zotonic_mod_payment/test/payment_receive_tests.erl)
else
    shopt -s nullglob
    tests=("apps_user/$app/test/"*_tests.erl)
fi
if [[ ${#tests[@]} -gt 0 ]]; then
    erlc -I apps_user -o "$build_dir" "${tests[@]}"
fi
erlc -I apps_user -o "$build_dir" apps_user/zotonic_mod_payment/test/payment_ci_sandbox_tests.erl

# Same sandbox node name and entry point as bin/zotonic runtests. The smoke test
# activates the selected module and checks its installed state in a real site.
erl -pa _build/default/lib/*/ebin -pa "$build_dir" \
    -sname zotonic001_testsandbox -sasl errlog_type error -s zotonic -noshell \
    -eval 'spawn(fun() -> receive after 180000 -> io:format(standard_error, "Sandbox tests timed out~n", []), halt(1) end end),
        App = os:getenv("PAYMENT_CI_APP"),
        Files = case os:getenv("PAYMENT_CI_TARGET") of
            "payment" -> ["mod_payment_tests", "payment_receive_tests"];
            _ -> [filename:basename(F, ".erl") || F <- filelib:wildcard("apps_user/" ++ App ++ "/test/*_tests.erl")]
        end,
        zotonic:runtests([payment_ci_sandbox_tests | [list_to_atom(F) || F <- Files]]).'

# Global meck replacements must run in a separate VM, away from live site workers.
if [[ "$target" == payment ]]; then
    : "${PAYMENT_TEST_PORT:?Set PAYMENT_TEST_PORT so DB tests cannot silently skip}"
    erlc -I apps_user -o "$build_dir" apps_user/zotonic_mod_payment/test/payment_receive_db_tests.erl
    erl -pa _build/default/lib/*/ebin -pa "$build_dir" -noshell \
        -eval 'case eunit:test(payment_receive_db_tests, [verbose]) of ok -> halt(0); _ -> halt(1) end.'
else
    apps_user/zotonic_mod_payment/test/run-psp-contracts.sh "$target"
fi
