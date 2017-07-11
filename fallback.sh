#!/bin/bash
cd ../../../../
EXT=$1
FILE=$2
if [ "$EXT" = "erl" ]
then
	case $FILE in
		*/_build/*)
			exit
			;;
		*)
			echo $FILE
			REBAR_COLOR=none rebar3 release
			;;
	esac
fi
