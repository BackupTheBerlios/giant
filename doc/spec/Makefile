default: view

include ../Makefile.incl

$(shell ./create_MakefilePics.sh)
include Makefile.pics

ifeq ($(shell test -r ../Makefile.local && echo "true"),true)
  include ../Makefile.local
endif

view: ps
	gv spec.ps

spec.pdf: base_pdf spec-pdf.pdf

spec-pdf.pdf:
	pdflatex $*
	pdflatex $*
	makeindex $*
	pdflatex $*
	thumbpdf $*
	pdflatex $*
	mv spec-pdf.pdf spec.pdf

ps: base_ps spec.ps
pdf: spec.pdf

step1: *.tex
	rm -f spec.dvi spec.ps spec.pdf

base_ps: step1 pics spec.ind
base_pdf: step1

clean:
	rm -f *~ *.aux *.dvi *.eps *.ilg *.ind *.dix *.log *.toc spec.pdf

