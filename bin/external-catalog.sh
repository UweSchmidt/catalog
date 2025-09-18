#!/bin/bash

# set -x

# no uninitialized variables

set -u

# rsync all media files from "Diakaesten" with a media backup

function die() {
    echo $1 1>&2
    exit 1
}

function trc() {
    echo '#' "$@" 1>&2
}

xOpt=
tOpt=
extVolumeOpt="5TB-Diakasten"

while [[ $# -gt 0 ]]; do
    case "$1" in
        -x)
            xOpt="-x"
            shift
            ;;
        -t|--test)
            tOpt="-t"
            shift
            ;;
        --ext-volume)
            shift
            extVolumeOpt="$1"
            shift
            ;;
        *)
            die "Illegal option(s) / argument(s): $* "
            ;;
    esac
done

# compute physical dir path of this script
res=""

function physDir() {
    local sp="$1"
    local wd=$(pwd -P)
    local ad=$(dirname "$wd/$sp")

    { pushd "$ad"; res=$(pwd -P); popd; } >/dev/null

    # trc "$wd $sp $ad $res"
    trc physDir: result: "$res"
}

orgBas="/Volumes/8TB-SieGeht/home/uwe/Bilder"
orgDia="$orgBas/Diakaesten"
orgCat="$orgBas/catalog"

devCat="/Users/uwe/haskell/catalog"
extCat="/Volumes/$extVolumeOpt/home/uwe/Bilder/catalog"

physDir "$0"
curCat=$(dirname "$res")

function isTest() { [[ "$tOpt"  = "-t" ]]; }
function isExec() { [[ "$xOpt"  = "-x" ]]; }

function isOrg() { [[ "$curCat" = "$orgCat" ]] || isTest; }
function isDev() { [[ "$curCat" = "$devCat" ]] || isTest; }
function isExt() { [[ "$curCat" = "$extCat" ]] || isTest; }

isOrg || isDev || isExt || die "unknown catalog dir: $curCat"

if isTest
then
    catSrc="$HOME/haskell/catalog"
    catDst="$HOME/tmp/catalog"
    srcDir="$HOME/haskell/Diakaesten"
else
    catSrc="$curCat"
    srcDir="$orgDia"
    catDst="$extCat"
fi
dstDir="$catDst/photos"

dry="--dry-run"
DRY="echo DRY-RUN> "

if isExec
then
    dry=
    DRY=
fi

# [[ -d "$srcDir" ]] || die "media source directory not found: $srcDir"
[[ -d "$catDst" ]] || die "catalog destination directory not found: $dstDir"
[[ -d "$dstDir" ]] || $DRY mkdir "$dstDir"

if [[ -d "$srcDir" ]]
then
    trc compute exclude options for media files

    excludes=""
    for i in $(find "$srcDir" -type f | \
                   sed -e 's|^.*[.]|.|'| \
                   sort -u | \
                   grep -i -v -E '.jpg|.jpeg|.gif|.md|.mp4|.tiff?|.png|.pbm|.pgm|.ppm|.md|.txt' | \
                   cat )
    do
        trc "files with extension \"$i\" are excluded"
        excludes="$excludes --exclude=*$i"
    done

    trc copy all media files from $srcDir to $dstDir
    trc rsync $dry -av $excludes "$srcDir/" "$dstDir"
    trc "no .-files and no files not used by catalog server"

    rsync $dry -av \
          --delete \
          --delete-excluded \
          --exclude='.*' \
          $excludes \
          "$srcDir/" "$dstDir"
else
    trc "media source directory not found: $srcDir"
    trc "no media files copied"
fi

if isOrg
then
    trc copy catalog data from "$catSrc/data/" to "$catDst/data"
    trc "git repos and configs (.git, .gitignore) are included"

    rsync $dry -av \
          --exclude='.DS_Store' \
          --exclude='*~' \
          --exclude='catalog-journal*.json' \
          "$catSrc/data/" "$catDst/data"
fi

if isOrg || isDev
then
    trc copy catalog sources and binaries from $catSrc/ to $catDst
    trc "git repos and configs (.git, .gitignore) are included"

    rsync $dry -av \
          --exclude='.DS_Store' \
          --exclude='*~' \
          --exclude="data" \
          --exclude='dist-newstyle' \
          "$catSrc/" "$catDst"
fi
