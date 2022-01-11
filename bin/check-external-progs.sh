#!/bin/bash

# check whether all external programs called by catalog servant-polysemy and client-polysemy
# are installed

ok=0                   # true

notThere () {
    ok=1
    echo $1 >&2
}

which -s pandoc || \
    notThere "pandoc not found, try 'cabal install pandoc'"

which -s exiftool || \
    notThere "exiftool not found, try 'brew install exiftool'"

which -s convert || \
    notThere "convert not found, try 'brew install imagemagick'"

which -s composite || \
    notThere "composite not found, try 'brew install imagemagick'"

which -s git ||
    notThere "git not found, try 'brew install git' or install xcode-tools"

which -s ffmpeg ||
    notThere "ffmpeg not found, try brew install ffmpeg, only needed for .mov -> .mp4"

which -s perl ||
    notThere "perl not found, try install xcode-tools, only needed for versionbump.sh"

exit $ok
