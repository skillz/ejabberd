#!/bin/bash

# Script that runs the tests and coverage tool through mix.
# Not the most elegant solution, but it will work for now.

CURRENT_DIR=`pwd`
BUILD_DIR="$CURRENT_DIR/ebin/dev/lib/*/"

DIRECTORIES=""
for dir in $BUILD_DIR
do
    dir=${dir%*/}      # remove the trailing "/"
    EBIN_DIR="$dir/ebin"
    DIRECTORIES="$DIRECTORIES $EBIN_DIR"
done

CT_BACKENDS=mysql,mnesia elixir --erl "-pa $DIRECTORIES" -S mix coveralls
