#!/bin/bash

set -e

if [ $# -ne 1 ]; then
    echo "Error: Running the script requires passing the strategy name as an argument."
    echo "Usage: $0 <strategy>"
    exit 1
fi

strategy=$1

# Validate the generator name by looking up `*Generator.v` in `Strategies` directory
if [ ! -f "Strategies/$strategy"".v" ]; then
    echo "Error: The generator name is invalid."
    # Provide a list of valid fuzzers
    echo "Valid generators are:"
    for f in Strategies/*Generator.v; do
        echo "  - $(basename $f .v)"
    done
    exit 1
fi

ocamlfind ocamlopt -linkpkg -package zarith -package unix -package eio_main -thread -rectypes $strategy"_test_runner.mli" $strategy"_test_runner.ml" -o $strategy"_test_runner.native"
