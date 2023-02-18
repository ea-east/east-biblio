CITEPROCDIR ?= $(shell realpath ./.haskelly/)
CSLSTYLE ?= styles/chicago-author-date-east.csl
BIBFILE ?= bib/east.bib

verify:
	file -i $(BIBFILE) | grep -i "charset=utf-8"
	biber --tool \
	--dieondatamodel \
	--no-default-datamodel \
	--configfile=bib/biber.conf \
	--output-resolve \
	--validate-datamodel \
	$(BIBFILE)

format-to-html: $(CITEPROCDIR)/citeproc
	@#bin/format-bib-to-html.sh
	pandoc -s \
	--bibliography "$(BIBFILE)" \
	--citeproc \
        --csl "./styles/chicago-author-date-east.csl" \
        bib-formatting.md \
       -o "/tmp/east-formatted.html" && \
	echo "Formatted bib in /tmp/east-formatted.html"

bib-to-biblatexml:
	biber --config=bib/biber.conf \
		--tool \
		--output-format=biblatexml \
		--output-file bib/east_bibertool.bltxml \
		$(BIBFILE)

normalize-bib:
	biber --config=bib/biber.conf --tool --output-file bib/east_bibertool.bib $(BIBFILE)

# citeproc:
# 	test -f $(CITEPROCDIR)/citeproc || \
# 	cabal install --installdir=$(CITEPROCDIR) -f "executable" citeproc

# citeproc-update:
# 	cabal install --overwrite-policy=always \
# 		--installdir=$(CITEPROCDIR) \
# 		-f "executable" citeproc

# pandoc-citeproc --bib2json $(BIBFILE) > bib/east.json
# $(CITEPROCDIR)/pandoc -s \
# 	--style=$(CSLSTYLE) \
# 	--references=bib/east.json \
# 	--format=html \
# 	bib-formatting.md \
# 	> /tmp/test.html
