default: view

include ../Makefile.incl

$(shell ./create_MakefilePics.sh)
include Makefile.pics

ifeq ($(shell test -r ../Makefile.local && echo "true"),true)
  include ../Makefile.local
endif

view: ps
	gv spec.ps

ps: base spec.ps
pdf: base spec.pdf

step1: *.tex
	rm -f spec.dvi spec.ps spec.pdf

base: step1 pics spec.ind
