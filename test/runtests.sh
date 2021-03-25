#!/usr/bin/env bash

# No `.dream` files? Alright.
shopt -s nullglob

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

for file in "${TEST_DIR}"/*.dream; do
  echo "Checking ${file}"
  opam exec dune -- exec dreamtt -- "${file}" || exit 1
done

echo DONE
