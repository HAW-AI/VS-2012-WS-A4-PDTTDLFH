# Verteilte Systeme - Wintersemester 2012 - Aufgabe 4

## Compiling
    handled by the start.sh script

## Running

### Usage
    sh start.sh <LocalIP> <start-client-nummer> <end-client-nummer> <ReceivingPort> <MulticastIP>

### Example for 20 clients
    sh start.sh 172.16.1.9 0 19 1338 225.10.1.2

### Stopping
    sh stop.sh

## Logging
The logs are written to the `log/` directory.
