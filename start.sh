#!/bin/sh

if [ "$#" == "5" ] ; then
  erl -sname sender$2 -setcookie hallo -boot start_sasl -noshell -s coordinator start $@
else
  echo "usage: ./start.sh <ReceivingPort> <TeamNumber> <StationNumber> <MulticastIP> <LocalIP>"
  echo "example: ./start.sh 1338 08 99 225.10.1.2 127.0.0.1"
  echo "big example: java -cp ~/Downloads/datasource/ datasource.DataSource 6 99 | ./start.sh 1338 08 99 225.10.1.2 127.0.0.1"
fi
