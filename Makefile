verify:
	biber --tool \
	--dieondatamodel \
	--no-default-datamodel \
	--validate-config \
	--configfile=bib/biber.conf \
	--output-resolve \
	--validate-datamodel \
	bib/east.bib

format-to-html:
	bin/format-bib-to-html.sh

bib-to-biblatexml:
	biber --config=bib/biber.conf --tool --output-format=biblatexml bib/east.bib
