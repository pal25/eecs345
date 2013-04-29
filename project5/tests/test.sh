#!/usr/bin/env bash
# XXX: This _may_ work for updating the old tests to be valid now...

RED="\033[0;31m"
GREEN="\033[0;32m"
WHITE="\033[1;37m"

function cleanup {
    rm -f tests/temp.c
    rm -f tests/results.c
}
trap cleanup EXIT

desc=$1
if [ `head -n1 $1 | grep -c '^description:' -` -eq 1 ]; then
    desc=`head -n1 $1`
    value=`sed -n '2p' $1 | sed -e 's/value://' -e 's/^ *//g'`
else
    value=`sed -n '1p' $1 | sed -e 's/value://' -e 's/^ *//g'`
fi
echo -en "$WHITE + $1 | $desc"

rm -f tests/temp.c

if [ ! `echo $1 | egrep 'assignment4'` ]; then
  echo "class Main {" >> tests/temp.c
fi
if [ `echo $1 | egrep 'the-a(1|2)'` ]; then
  echo "static main() {" >> tests/temp.c
fi
tail -n +3 "$1" >> tests/temp.c
if [ `echo $1 | egrep 'the-a(1|2)'` ]; then
  echo -n "}" >> tests/temp.c
fi
if [ ! `echo $1 | egrep 'assignment4'` ]; then
  echo -n "}" >> tests/temp.c
fi

#if [ $(echo $1 | egrep 'the-a(1|2|3)') ]; then
#  sed '/^[a-z]*(.*).*{/ s/^/static /' < tests/temp.c > tests/temp2.c
#  mv tests/temp2.c tests/temp.c
#fi

if [[ $value = "ERROR" ]]; then
    result=`racket -Ve '(load "interpreter.scm")(interpret "tests/temp.c" "Main")(exit)' 2>&1 1>/dev/null`
else
    result=`racket -e '(load "interpreter.scm")(interpret "tests/temp.c" "Main")(exit)' 2>&1`
fi

result=`echo $result | sed -e 's/^ *//g'`

if [[ $value = $result || ( $value = "ERROR" && $result ) ]] ; then
    echo -e "$WHITE |--> $GREEN[passed]"
    echo pass >> tests/results.txt
    exit
else
    echo -e "$WHITE |--> $RED[failed] $WHITE\n    Expected \"$value\", got \"$result\""
    echo fail >> tests/results.txt
    exit
fi
