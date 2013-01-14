#!/bin/bash
for n in `seq 1 20`
do
(java datasource.DataSource $n 99 | erl -sname sender$n -setcookie vsp -boot start_sasl -noshell -s coordinator start 1337 $n 99 225.10.1.2 10.0.2.15) > log/$n.log &
done
