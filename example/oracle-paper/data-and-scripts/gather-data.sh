#!/bin/bash

# usage: gather-data.sh <opts> 
# where opts include
# -e        path to benchmark executable
# -s        input size
# -p        number of processors
# -c        cutoff size
# -k        log constants
# -m        param
# -o        optimization mode 
# -t        timeout (seconds)
# -f        output file
# 
# output is of the form
#   <time (in seconds)> 

# TODO: -p -1 should run the sequential version 
# TODO: -p -2 should run the work-stealing version 

usage="See comments in script."

BENCHMARK=""
INPUT_SIZE=""
PROCS=""
CUTOFF=""
OPTIM=0
CSTS=0
TIMEOUT="10"

while getopts ":e:s:p:c:k:o:t:m:f:" options; 
do
    case $options in
	e ) BENCHMARK="$OPTARG";;
	s ) INPUT_SIZE="$OPTARG";;
	p ) PROCS="$OPTARG";;
	c ) CUTOFF="$OPTARG";;
	k ) CSTS="$OPTARG";;
	o ) OPTIM="$OPTARG";;
	m ) PARAM="$OPTARG";;
	f ) OUTPUT="$OPTARG";;
	t ) TIMEOUT="$OPTARG";;
	\? ) echo $usage
            exit 1;;
	* ) echo $usage
            exit 1;;
    esac
done

if [ ! -x $BENCHMARK ]; then
    echo "$BENCHMARK does not exist"
    exit 1
fi

gcc timeout.c -o timeout

 retcode=1
 sleep 0.1
 rm -f $OUTPUT
 touch $OUTPUT
 if [ "$BENCHMARK" == "../examples/bintree-sum.out" ]; then
     ./timeout $TIMEOUT $BENCHMARK -p $PROCS -size $INPUT_SIZE -oracle-kappa $CUTOFF -optim $OPTIM -param $PARAM -csts $CSTS -measure-oracle-cost 0 > $OUTPUT
 else
     ./timeout $TIMEOUT $BENCHMARK -p $PROCS -size $INPUT_SIZE -oracle-kappa $CUTOFF -optim $OPTIM -param $PARAM -csts $CSTS > $OUTPUT
 fi
 retcode=$?
 if [ $retcode == "-1" ]; then
   echo "Benchmark timed out." >&2
   exit 10
 fi
 if [ $retcode != "0" ]; then
   echo "Benchmark crashed." >&2
   exit 11
 fi


      #ERR=$( cat err )
      #rm -f err
#while [ $retcode != "0" ]; do
      #echo "stderr: $ERR" >&2
      # echo "stdout: $RES" >&2
      #echo "Retrying..." >&2

#done
