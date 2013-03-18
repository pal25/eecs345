#!/usr/bin/env bash

function cleanup {
    rm -f tests/temp.c
}
trap cleanup EXIT

desc=$1
if [ `head -n1 $1 | grep -c '^description:' -` -eq 1 ]; then
    desc=`head -n1 $1 | sed s/description:// -`
    value=`sed -n '2p' $1 | sed -e 's/value://' -e 's/^ *//g'`
else
    value=`sed -n '1p' $1 | sed -e 's/value://' -e 's/^ *//g'`
fi

tail -n +3 "$1" > tests/temp.c
if [[ $value = "ERROR" ]]; then
    result=`racket -Ve '(load "interpreter.scm")(interpret "tests/temp.c")(exit)' 2>&1 1>/dev/null`
else
    result=`racket -e '(load "interpreter.scm")(interpret "tests/temp.c")(exit)'`
fi

result=`echo $result | sed -e 's/^ *//g'`

if [[ $value = $result || ( $value = "ERROR" && $result ) ]] ; then
    echo -e "\e[00;32m[passed] $desc \e[00m"
    exit
else
    echo -e "\e[00;31m[failed] $desc -- Expected \"$value\", got \"$result\" ($1)\e[00m"
    exit
fi
