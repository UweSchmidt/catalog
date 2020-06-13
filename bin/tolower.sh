#!/bin/bash

# set -x

for i in $(find . -maxdepth 1 -name "*.NEF" -or -name "*.MP4" -or -name "*.MOV" -or -name "*.JPG" -or -name "*.RW2" -or -name "*.xmp" -or -name "*.dop")
do
    if [[ -f "$i" ]]
    then
	chmod 664 "$i"
	newi=$(echo $i | tr A-Z a-z)
	if [[ "$i" != "$newi" ]]
	then
	    mv "$i" "$i.tmp"
	    mv "$i.tmp" "$newi"
	fi
    fi
done

ls -l
