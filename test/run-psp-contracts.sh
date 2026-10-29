#!/usr/bin/env bash
# Run from the Zotonic umbrella root, after ./rebar3 compile.
# Test-only exports and beams stay outside the normal build.
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
for provider in "${providers[@]}"; do
    if [[ "$provider" == sovergn ]]; then
        sources+=(apps_user/zotonic_mod_payment_sovergn/src/support/payment_sovergn_api.erl)
    else
        sources+=("apps_user/zotonic_mod_payment_${provider}/src/models/m_payment_${provider}_api.erl")
    fi
done
erlc -DTEST -I apps_user -o "$build_dir" "${sources[@]}" \
    apps_user/zotonic_mod_payment/test/payment_psp*.erl
erl -pa _build/default/lib/*/ebin -pa "$build_dir" -noshell \
    -eval 'case eunit:test(payment_psp_contract_tests, [verbose]) of ok -> halt(0); _ -> halt(1) end.'
