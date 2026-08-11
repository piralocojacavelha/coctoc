#!/usr/bin/bash

set -xe

clang src/main.c -o bin/coc \
    -fsanitize=address \
    -fsanitize=undefined \
    -ggdb
