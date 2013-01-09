#!/bin/bash
for pid_name in `ls -1 | grep [\.]pid`
do
   station=${pid_name%.pid}
   echo "Killing: " $station
   kill `cat $station.pid`
   rm $station.pid
done
killall java > /dev/null 2>&1
