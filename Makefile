verify:
	biber --config=bib/biber.conf --validate-datamodel --tool bib/east.bib

format-to-html:
	bin/format-bib-to-html.sh

bib-to-biblatexml:
	biber --config=bib/biber.conf --tool --output-format=biblatexml --output-file bib/east_bibertool.bltxml bib/east.bib

normalize-bib:
	biber --config=bib/biber.conf --tool --output-file bib/east_bibertool.bib bib/east.bib
