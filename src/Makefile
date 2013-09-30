LISPSRC := $(wildcard *.lisp)
MDOBJS := $(patsubst %, doc/obj/%.md, $(LISPSRC)) $(wildcard *.md)

book: doc/romreader.pdf doc/romreader.epub

doc/obj/%.lisp.md : %.lisp Makefile
	awk '/^ *[^;]{3}/ {if (last~/^ *;;;/) printf("\n")} {print} {last=$$0}' < $< | sed -E -e 's/^/    /g' -e 's/^    ;;; ?//g' > $@

doc/romreader.md: $(MDOBJS) Makefile doc/genbook.sh
	(cd doc; ./genbook.sh) > $@

doc/romreader.pdf: doc/romreader.md
	pandoc --template=doc/template.latex --latex-engine=lualatex -V fontsize=10pt -V monofont=droidsansmono -V monoscale=.70 -V verbatimspacing=.85 -V mainfont=droidserif -V sansfont=droidsans -V documentclass:book -V geometry:top=1.0in -V geometry:bottom=0.75in -S --toc --chapters  -o $@ $<

doc/romreader.epub: doc/romreader.md
	pandoc -S --toc --chapters -o $@ $<

clean:
	-rm doc/romreader.*
	-rm doc/obj/*.md

clean-md:
	-rm doc/obj/*.md
