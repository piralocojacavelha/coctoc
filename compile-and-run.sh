#!/usr/bin/bash

set -e

./bin/coc "./${1%.coc}.coc" "./${1%.coc}.c"
./out
rm out.c
rm out
