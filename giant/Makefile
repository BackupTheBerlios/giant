#
# GIANT Makefile
#

# gnatmake is called from this directory
# therefore all pathes need to be relative to this directory
OUT_PATH        = obj/

IMLLIB_PATH     = $(HOME)/GIANT/IML/iml_browser
XMLADA_PATH     = ../../src/xmlada-0.7.1
GIANT_PATH      = ../src

IMLLIB_INCLUDES = -I$(IMLLIB_PATH)/IML.generated -I$(IMLLIB_PATH)/IML.src -I$(IMLLIB_PATH)/Reuse.src -I$(IMLLIB_PATH)/usr.local.reuse
GIANT_INCLUDES  = -I$(GIANT_PATH) -I$(GIANT_PATH)/config -I$(GIANT_PATH)/control -I$(GIANT_PATH)/graph_lib -I$(GIANT_PATH)/gsl -I$(GIANT_PATH)/gui -I$(GIANT_PATH)/project -I$(GIANT_PATH)/utils -I$(GIANT_PATH)/vis
XMLADA_INCLUDES = -I$(XMLADA_PATH)/dom -I$(XMLADA_PATH)/input_sources -I$(XMLADA_PATH)/sax -I$(XMLADA_PATH)/unicode

INCLUDES        = $(GIANT_INCLUDES) $(IMLLIB_INCLUDES) $(XMLADA_INCLUDES)

LIBS            = $(shell gtkada-config)

GNAT_FLAGS      = -g -j2 -gnatX -gnata -gnatf -gnatwl -gnatwe -gnato 
                  #-gnaty3abcefhiklmprt #-gnatys #-gnatwu 
ALL_CFLAGS      = $(CFLAGS) $(TARGET_CFLAGS) $(GNAT_FLAGS) $(INCLUDES) 
ALL_LFLAGS      = $(LFLAGS) $(TARGET_LFLAGS) $(LIBS)

MAIN = ../src/giant-main.adb
EXEC = ../giant

ifeq ($(shell test -r Makefile.local && echo "true"),true)
  include Makefile.local
endif

.PHONY : all static style

all: 
	test -x $(OUT_PATH) || mkdir $(OUT_PATH)
	cd $(OUT_PATH) && \
	gnatmake -m $(ALL_CFLAGS) $(MAIN) -o $(EXEC) $(GNATMAKE_FLAGS) \
	  -bargs -static -E -largs $(ALL_LFLAGS)

static:
	make LIBS="$(shell gtkada --static)" all

style:
	make CFLAGS="-gnaty3abcefhiklmprt $(CFLAGS)" all

clean:
	rm -rf $(OUT_PATH)
