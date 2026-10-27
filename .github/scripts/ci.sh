#!/usr/bin/env bash
set -euo pipefail

# Run from the repository root. Outputs live outside the checkout by default.
output="${CI_OUTPUT_DIR:-${RUNNER_TEMP:-${TMPDIR:-/tmp}}/streamdeck-ci}"
mkdir -p "$output/logs" "$output/previews"

run_step() {
    local name="$1"
    shift
    printf '::group::%s\n' "$name"
    "$@" 2>&1 | tee "$output/logs/$name.log"
    printf '::endgroup::\n'
}

toolchain() {
    local utility
    ghc --version || return
    cabal --version || return
    pkg-config --modversion libudev x11 xtst zlib || return
    for utility in xvfb-run Xvfb xauth setxkbmap; do
        command -v "$utility" || return
    done
}

check_examples() {
    local example
    for example in examples/*.hs; do
        printf 'Checking %s\n' "$example"
        cabal exec -- ghc -fno-code -Wall -Wcompat \
            -package=haskell-elgato-streamdeck "$example" || return
    done
}

run_step toolchain toolchain
run_step package-check cabal check
run_step package-index cabal update
run_step build cabal build all --enable-tests --jobs=2
run_step examples check_examples

# Both modes run the ordinary suite too. XTEST input is confined to private
# servers; no physical Stream Deck, desktop session, or USB permissions needed.
run_step tests-x11 env STREAMDECK_TEST_X11=1 \
    xvfb-run -a -s '-screen 0 1024x768x24 -nolisten tcp' \
    cabal test all --test-show-details=direct --test-options='+RTS -N2 -RTS'

run_step tests-no-xtest env STREAMDECK_TEST_X11=no-xtest \
    xvfb-run -a -s '-screen 0 1024x768x24 -nolisten tcp -extension XTEST' \
    cabal test all --test-show-details=direct --test-options='+RTS -N2 -RTS'

run_step image-previews cabal exec -- runghc \
    -package=haskell-elgato-streamdeck examples/FontAwesome.hs \
    --preview "$output/previews/fontawesome"
