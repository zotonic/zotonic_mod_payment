#!/usr/bin/env bash
# Run from the Zotonic umbrella root, after ./rebar3 compile.
# Compiled beams stay outside the normal build.
set -euo pipefail
cd "$(dirname "$0")/../../.."
build_dir=$(mktemp -d "${TMPDIR:-/tmp}/payment-contracts.XXXXXX")
trap 'rm -rf "$build_dir"' EXIT
export ERL_LIBS="$PWD/_build/default/lib"
export PAYMENT_CONTRACT_DIR="$PWD/apps_user/zotonic_mod_payment/test/psp_contracts"
providers=(mollie stripe paypal buckaroo sovergn)
if [[ $# -gt 0 ]]; then
    case "$1" in
        mollie|stripe|paypal|buckaroo|sovergn) providers=("$1");;
        *) echo "Unknown PSP: $1" >&2; exit 2;;
    esac
    export PAYMENT_TEST_PSP="$1"
else
    unset PAYMENT_TEST_PSP
fi
sources=()
# Compile the ordinary adapters and their discovery helpers together, so the
# optional offline runner cannot accidentally use stale module observer beams.
for app in zotonic_mod_payment "${providers[@]/#/zotonic_mod_payment_}"; do
    while IFS= read -r -d '' source; do sources+=("$source"); done < <(
        find "apps_user/$app/src" -name '*.erl' -print0
    )
done
erlc +debug_info -I apps -I apps_user -o "$build_dir" "${sources[@]}" \
    apps_user/zotonic_mod_payment/test/payment_psp*.erl \
    apps_user/zotonic_mod_payment/test/payment_test_mock.erl
erl -pa _build/default/lib/*/ebin -pa "$build_dir" -noshell \
    -eval 'case eunit:test(payment_psp_contract_tests, [verbose]) of ok -> halt(0); _ -> halt(1) end.'
