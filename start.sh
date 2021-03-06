#!/bin/bash

if [ $# == '0' ] ; then
  echo 'usage: sh start.sh <adresse des zu verwendenen Interfaces> <start-client-nummer> <end-client-nummer> <Port> <Multicast Adresse>'
  echo 'example: sh start.sh 172.16.1.2 0 9 1337 225.10.1.2'
  exit
fi

cp /dev/null log/all.log
rm *.beam
echo "re-compiling"
erlc *.erl

#$1 adresse des zu verwendenen Interfaces
#$2 start-client-nummer
#$3 end-client-nummer
#$4 Port
#$5 Multicast Adresse
for n in `seq $2 $3`
do
  echo "starting client-$n"
  (java datasource.DataSource $n $n | erl +A25 -sname sender$n -setcookie vsp -boot start_sasl -noshell -s coordinator start $4 $n $n $5 $1) >> log/all.log &
  sleep 0.2
done
