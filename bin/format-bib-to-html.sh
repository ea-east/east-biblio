#!/bin/bash

TARGETFILE="formatted.html"
TARGETDIR="/home/beta/netshares/oeaw-owncloud/east-biblatex"
SOURCEDIR="/home/beta/webstuff/east-biblio"
LOGFILE="${TARGETDIR}/bib-formatting.log"
STARTDIR=$(pwd)

if [ ! -d ${TARGETDIR} ]; then
   echo "Target directory not found: ${TARGETDIR}"
   exit 1
fi

echo "Formatting bibliography"

cd "${SOURCEDIR}"

pandoc -s \
     --bibliography "${TARGETDIR}/east.bib" \
     --filter pandoc-citeproc \
     --csl "${SOURCEDIR}/styles/chicago-author-date-east.csl" \
     bib-formatting.md \
     --log="${LOGFILE}" \
     -o "/tmp/${TARGETFILE}" \
    || exit 1

echo "%% Empty is good" >> "${LOGFILE}"

echo "Seems it went well, updating version in ${TARGETDIR}"

mv "/tmp/${TARGETFILE}" "${TARGETDIR}/${TARGETFILE}"

cd "${STARTDIR}"
