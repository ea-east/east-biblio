verify:
	file -i bib/east.bib | grep -i "charset=utf-8"
	biber --tool \
	--dieondatamodel \
	--no-default-datamodel \
	--configfile=bib/biber.conf \
	--output-resolve \
	--validate-datamodel \
	bib/east.bib

format-to-html:
	bin/format-bib-to-html.sh

bib-to-biblatexml:
	biber --config=bib/biber.conf --tool --output-format=biblatexml --output-file bib/east_bibertool.bltxml bib/east.bib

normalize-bib:
	biber --config=bib/biber.conf --tool --output-file bib/east_bibertool.bib bib/east.bib
