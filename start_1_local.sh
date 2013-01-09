#!/bin/bash
CLASSPATH=`dirname $0`

java -cp $CLASSPATH datasource.DataSource $2 $3 | ./start.sh $@ &
echo $! > $3.pid
