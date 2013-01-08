#!/bin/sh
erl -sname sender -setcookie hallo -boot start_sasl -noshell -s coordinator easy_start
