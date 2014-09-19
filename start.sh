#!/bin/sh
reset() { stty $clropt ; }

case `uname` in
    Darwin) echo "Tested OS" >&2 ;;
    Linux) echo "Untested OS - let's see if stty can do it anyway..." >&2 ;;
    SunOS) echo "Tested OS" >&2 ;;
    *) echo "You are on your own. Trying sty anyway" >&2 ;;
esac

setopt=`stty -a | grep -w icanon | while read line ; do
    for o in $line ; do
        if [ "$o" = "icanon" ] ; then
            echo "-icanon"
        fi
    done
done`

setopt="$setopt "`stty -a | grep -w echo | while read line ; do
    for o in $line ; do
	if [ "$o" = "echo" ] ; then
	    echo "-echo"
	fi
    done
done`
clropt=`echo $setopt | sed 's/-//g'`

echo "setopt:$setopt / clropt:$clropt" >&2

if [ -f "$1" ] ; then
    game="$1"
elif [ -f "$1"'.hs' ] ; then
    game="$1"'.hs'
else
    echo "Not found:$1" >&2
    exit 1
fi

stty $setopt
trap reset 0
runghc "$game"
