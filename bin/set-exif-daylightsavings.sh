#!/bin/bash

# set -x

# for all raw files and .jpg in cwd do
#   change all date tags to daylight saving time
#
# used when camera was not switched to daylight saving time
#
# option "-n" : swith to normal time

verbose=-v
summer="Yes"
if [[ "$1" = "-n" ]]
then
    summer="No"
    shift
fi

log () {
    if [[ "$verbose" = "-v" ]]
    then
        echo $@ 1>&2
    fi
}

setcdate() {
    if [[ -f "$1" ]]
    then
        dlst=$(exiftool -DaylightSavings "$1" | sed -e 's/Daylight.*: //')
        if [[ "$dlst" != "$summer" && "dlist" != "" ]]
        then
            log "$1: switch to daylight saving time"
            log $(exiftool -AllDates "$1")
            if [[ "$summer" = "Yes" ]]
            then
                exiftool -overwrite_original -q -AllDates+=1 "$1"
            else
                exiftool -overwrite_original -q -AllDates-=1 "$1"
            fi
            exiftool -q -DaylightSavings="$summer" "$1"
            log $(exiftool -AllDates "$1")
            log $(exiftool -DaylightSavings "$1")
        fi
    fi
}

if [[ "$*" != "" ]]
then
    for i in "$@"
    do
        setcdate "$i"
    done
else
    for i in $(find . -maxdepth 1 -name "*.nef" -or -name "*.jpg")
    do
        setcdate "$i"
    done
fi
