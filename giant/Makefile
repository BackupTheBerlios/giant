#
# GIANT Makefile
#

TMP_PATH		= /tmp/$(shell id -un)

# gnatmake is called from this directory
# therefore all pathes need to be relative to this directory
OUT_PATH        = obj/

# IML - if available, take StuPro's IML-lib
#GLOBAL_IMLLIB_PATH = /home/stsopra/giant/IML/iml_browser
#ifeq ($(shell test -d $(GLOBAL_IMLLIB_PATH) && echo "true"),true)
#  IMLLIB_PATH = $(GLOBAL_IMLLIB_PATH)
#else
  IMLLIB_PATH     = $(HOME)/GIANT/IML/iml_browser
#endif

# relative to $(OUT_PATH)
GIANT_PATH      = ../

AUNIT_PATH      = $(GIANT_PATH)/../src/aunit-1.01
XMLADA_PATH     = $(GIANT_PATH)/../src/xmlada-0.7.1

AUNIT_INCLUDES  = -I$(AUNIT_PATH)/aunit/framework -I$(AUNIT_PATH)/aunit/text_reporter
IMLLIB_INCLUDES = -I$(IMLLIB_PATH)/IML.generated -I$(IMLLIB_PATH)/IML.src -I$(IMLLIB_PATH)/Reuse.src -I$(IMLLIB_PATH)/usr.local.reuse
GIANT_INCLUDES  = -I$(GIANT_PATH)/src -I$(GIANT_PATH)/src/config -I$(GIANT_PATH)/src/control -I$(GIANT_PATH)/src/graph_lib -I$(GIANT_PATH)/src/gsl -I$(GIANT_PATH)/src/gsl/generated -I$(GIANT_PATH)/src/gui -I$(GIANT_PATH)/src/project -I$(GIANT_PATH)/src/reuse -I$(GIANT_PATH)/src/utils -I$(GIANT_PATH)/src/vis
XMLADA_INCLUDES = -I$(XMLADA_PATH)/dom -I$(XMLADA_PATH)/input_sources -I$(XMLADA_PATH)/sax -I$(XMLADA_PATH)/unicode

INCLUDES        = $(GIANT_INCLUDES) $(IMLLIB_INCLUDES) $(AUNIT_INCLUDES) $(XMLADA_INCLUDES)

LIBS            = $(shell gtkada-config)

CFLAGS          = -g 
#-O2 -funroll-loops
GNAT_FLAGS      = -j2 -gnatX -gnata -gnatf -gnatwl -gnato 
                  #-gnaty3abcefhiklmprt #-gnatys 
ALL_CFLAGS      = $(CFLAGS) $(TARGET_CFLAGS) $(GNAT_FLAGS) $(INCLUDES) $(CUSTOM_CFLAGS)
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
	gnatmake -m $(ALL_CFLAGS) $(GIANT_PATH)/$(MAIN) -o $(GIANT_PATH)/$(EXEC) $(GNATMAKE_FLAGS) $(ALL_LFLAGS) -bargs -static -E -largs 

static :
	make LIBS="$(shell gtkada --static)" all

style :
	make CFLAGS="-gnaty3abcefhiklmprt $(CFLAGS)" all

clean :
	rm -rf $(OUT_PATH)/*

tags :
	find -name "*.ad[bs]" | xargs etags

fetch-iml :
	mkdir -p $(TMP_PATH)
	scp droste.informatik.uni-stuttgart.de:/home/bauhaus/imbphase/iml_browser_030606.tar.gz $(TMP_PATH)
	tar -C $(TMP_PATH) -xzvf $(TMP_PATH)/iml_browser_030606.tar.gz

sloc :
	PATH=/home/stsopra/giant/sloccount-2.22:$(PATH) sloccount src/

marvin :
	mkdir -p /tmp/$(USER)/GIANT/cvs
	cd /tmp/$(USER)/GIANT/cvs;\
        cvs -d droste.informatik.uni-stuttgart.de:/home/squig/cvsroot co .
	echo "Please change to /tmp/$(USER)/GIANT/cvs"
