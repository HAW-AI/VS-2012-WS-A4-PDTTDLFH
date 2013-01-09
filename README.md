# Verteilte Systeme - Wintersemester 2012 - Aufgabe 4

## Compiling
    erlc *.erl

## Running

### Usage
    java -cp <PathToDataSource> datasource.DataSource 6 99 | sh start.sh <ReceivingPort> [<SendingPort>] <TeamNumber> <StationNumber> <MulticastIP> <LocalIP>

### Example
    java -cp ~/Downloads/datasource/ datasource.DataSource 6 99 | sh start.sh 1338 08 99 225.10.1.2 127.0.0.1


