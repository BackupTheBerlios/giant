#
# GIANT Makefile
#

# gnatmake is called from this directory
# therefore all pathes need to be relative to this directory
OUT_PATH        = obj/

IMLLIB_PATH     = $(HOME)/GIANT/IML/iml_browser
XMLADA_PATH     = ../../src/xmlada-0.7.1
GIANT_PATH      = ../

IMLLIB_INCLUDES = -I$(IMLLIB_PATH)/IML.generated -I$(IMLLIB_PATH)/IML.src -I$(IMLLIB_PATH)/Reuse.src -I$(IMLLIB_PATH)/usr.local.reuse
GIANT_INCLUDES  = -I$(GIANT_PATH)/src -I$(GIANT_PATH)/src/config -I$(GIANT_PATH)/src/control -I$(GIANT_PATH)/src/graph_lib -I$(GIANT_PATH)/src/gsl -I$(GIANT_PATH)/src/gui -I$(GIANT_PATH)/src/project -I$(GIANT_PATH)/src/utils -I$(GIANT_PATH)/src/vis
XMLADA_INCLUDES = -I$(XMLADA_PATH)/dom -I$(XMLADA_PATH)/input_sources -I$(XMLADA_PATH)/sax -I$(XMLADA_PATH)/unicode

INCLUDES        = $(GIANT_INCLUDES) $(IMLLIB_INCLUDES) $(XMLADA_INCLUDES)

LIBS            = $(shell gtkada-config)

CFLAGS          = -g -O2 -funroll-loops
GNAT_FLAGS      = -j2 -gnatX -gnata -gnatf -gnatwl -gnatwe -gnato 
                  #-gnaty3abcefhiklmprt #-gnatys #-gnatwu 
ALL_CFLAGS      = $(CFLAGS) $(TARGET_CFLAGS) $(GNAT_FLAGS) $(INCLUDES) 
ALL_LFLAGS      = $(LFLAGS) $(TARGET_LFLAGS) $(LIBS)

MAIN = src/giant-main.adb
EXEC = giant

ifeq ($(shell test -r Makefile.local && echo "true"),true)
  include Makefile.local
endif

.PHONY : all static style

all: 
	test -x $(OUT_PATH) || mkdir $(OUT_PATH)
	cd $(OUT_PATH) && \
	gnatmake -m $(ALL_CFLAGS) $(GIANT_PATH)/$(MAIN) -o $(GIANT_PATH)/$(EXEC) $(GNATMAKE_FLAGS) -bargs -static -E -largs $(ALL_LFLAGS)

static :
	make LIBS="$(shell gtkada --static)" all

style :
	make CFLAGS="-gnaty3abcefhiklmprt $(CFLAGS)" all

clean :
	rm -rf $(OUT_PATH)/*

tags :
	find -name "*.ad[bs]" | xargs etags
