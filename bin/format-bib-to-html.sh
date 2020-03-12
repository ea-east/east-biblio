#!/bin/sh
echo "Formatting bibliography"

pandoc -s \
       --bibliography "bib/east.bib" \
       --filter pandoc-citeproc \
       --csl "./styles/chicago-author-date-east.csl" \
       bib-formatting.md \
       -o "/tmp/east-formatted.html" && \
    echo "Formatted bib in /tmp/east-formatted.html"
