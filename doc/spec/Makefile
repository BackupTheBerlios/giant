include ../Makefile.incl

view: full
	xpdf spec.pdf

step1: *.tex
	rm -f spec.ps spec.dvi spec.pdf

full: step1 spec.ind spec.pdf
