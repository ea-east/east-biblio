verify:
	biber --config=bib/biber.conf --validate-datamodel --tool bib/east.bib

format-to-html:
	bin/format-bib-to-html.sh

bib-to-biblatexml:
	biber --config=bib/biber.conf --tool --output-format=biblatexml bib/east.bib
