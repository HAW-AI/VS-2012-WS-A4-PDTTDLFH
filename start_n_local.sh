#!/bin/bash
# starts n processes on the machine you are currently on that start communicating
#   usage: ./start_n_local <NUMBER OF PRCESSES TO START> <YOUR ID ADDRESS>
# example: ./start_n_local 5 192.168.0.1
PORT=15050
TEAM_NO=6
MULTICAST_IP="225.10.1.2"

for n in `seq 1 $1`
do
  ./start_1_local.sh $PORT $TEAM_NO $n $MULTICAST_IP $2 &
done
