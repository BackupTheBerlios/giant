set IML_PATH=../iml_browser_030825
set GIANT_PATH=../giant-1.0.1
set GTKADA_PATH=../GtkAda-1.3.12
set XMLADA_PATH=../xmlada-0.7.1

mkdir obj
cd obj
gnatmake -m -g -gnatX -gnata -gnatf -gnatwl -gnato -fstack-check -I%GIANT_PATH%/src -I%GIANT_PATH%/src/config -I%GIANT_PATH%/src/control -I%GIANT_PATH%/src/graph_lib -I%GIANT_PATH%/src/gsl -I%GIANT_PATH%/src/gsl/generated -I%GIANT_PATH%/src/gui -I%GIANT_PATH%/src/project -I%GIANT_PATH%/src/reuse -I%GIANT_PATH%/src/utils -I%GIANT_PATH%/src/vis -I%IML_PATH%/IML.generated -I%IML_PATH%/IML.src -I%IML_PATH%/Reuse.src -I%IML_PATH%/usr.local.reuse -I../aunit-1.01/aunit/framework -I../aunit-1.01/aunit/text_reporter -I%XMLADA_PATH%/dom -I%XMLADA_PATH%/input_sources -I%XMLADA_PATH%/sax -I%XMLADA_PATH%/unicode -I%GTKADA_PATH%/lib %GIANT_PATH%/src/giant-main.adb -o %GIANT_PATH%.exe -bargs -static -E
cd ..