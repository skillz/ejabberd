#!/bin/bash

./autogen.sh
./configure --prefix=$HOME/my-ejabberd --enable-all
make ejabberd_debug=true
