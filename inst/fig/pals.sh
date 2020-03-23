#!/usr/bin/env sh

# https://www.imagemagick.org/script/escape.php

convert *.{png,jpg,jpeg} +dither -colors 9 -define histogram:unique-colors=true -format "%f, n=%k\n%c\n
" histogram:info: > hex.txt

