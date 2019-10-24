#!/bin/bash

px="albums/Reisen/Spanien-2019/Slideshow"
dirs="Gegend Blauelster Bienenfresser Stoerche AmGeierfelsen Mitbewohner Flugstudien"

for i in $dirs
do
    c=$px/$i
    getcollection.sh $i
done
