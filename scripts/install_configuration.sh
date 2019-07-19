#!/bin/bash

## Script that installs ejabberd's configuration.  The first argument is the location.

./autogen.sh
./configure --prefix=$HOME/my-ejabberd \
            --enable-elixir \
            --enable-odbc \
            --enable-mysql \
            --enable-tools \
            --prefix=$1
make install-configuration
