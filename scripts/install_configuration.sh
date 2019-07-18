#!/bin/bash

./autogen.sh
./configure --prefix=$HOME/my-ejabberd --enable-elixir --enable-odbc --enable-mysql --enable-tools
make install-configuration
