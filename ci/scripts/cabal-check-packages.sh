#!/usr/bin/env bash

COUNT=0
FAILS=0
declare GITHUB_STEP_SUMMARY
declare -a invalid=()

set -eux

LOG_FILE=$(readlink -f ./cabal-check-log.log)
touch "${LOG_FILE}"

trap 'rm -f "${LOG_FILE}"' EXIT

while read -r CABAL; do
  echo "Checking ${CABAL}"
  COUNT=$((COUNT + 1))
  pushd "$(dirname "${CABAL}")"
  echo "" >> "${LOG_FILE}"
  PKG=$(basename "${CABAL}")
  PKG="${PKG%.cabal}"
  {
    echo "### ${PKG}"
    echo ""
    echo '```'
  } >> "${LOG_FILE}"

  if ! (cabal check >>"${LOG_FILE}" 2>&1) ; then
    invalid+=("${PKG}")
    FAILS=$((FAILS + 1))
  fi

  {
    echo '```'
    echo ""
  } >> "${LOG_FILE}"
  popd
done < <(find . -name '*.cabal' -not -path './dist-newstyle/**')

if [ "${FAILS}" -gt 0 ]; then
  {
    echo ":no_entry_sign: Cabal check failed for the following packages:"
    echo ""
    for PKG in "${invalid[@]}"; do
      echo "- ${PKG}"
    done

    echo ""
    echo "## Messages"
    echo ""
    cat "${LOG_FILE}"
  } >> "${GITHUB_STEP_SUMMARY}"
  exit 1
else
  { 
    echo ":white_check_mark: All Cabal green!" 
    echo ""

    echo "## Messages"
    echo ""
    cat "${LOG_FILE}"
  } >> "${GITHUB_STEP_SUMMARY}"
fi
