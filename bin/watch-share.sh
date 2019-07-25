#!/bin/bash

DIR="/home/beta/netshares/oeaw-owncloud/east-biblatex"
FILE="${DIR}/bib-formatting.log"

while RES=$(inotifywait -e modify $FILE)
do
    echo RES is $RES at `date`
    /home/beta/webstuff/east-biblio/bin/format-bib-to-html.sh
done
