#!/bin/bash
cp /dev/null log/all.log
rm *.beam
echo "re-compiling"
erlc *.erl

for n in `seq $2 $3`
do
  (java datasource.DataSource $n $n | erl +A25 -sname sender$n -setcookie vsp -boot start_sasl -noshell -s coordinator start 1337 $n $n 225.10.1.2 $1) >> log/all.log &
  sleep 0.2
done
