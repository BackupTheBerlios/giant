#!/bin/sh

cd `dirname $0`
system=`uname -s`
if [ "$system" = "Linux" ]; then
	./giant-linux
elif [ "$system" = "SunOS" ]; then
	./giant-sunos
else
	echo "$system is unsupported."
	exit 1
fi
