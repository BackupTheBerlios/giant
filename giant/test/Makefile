framework_test :
	make -C ../ MAIN=test/framework_test EXEC=test/framework_test

mini_map :
	make -C ../ MAIN=test/giant_test-mini_map EXEC=test/mini_map all

file_management :
	make -C ../ MAIN=test/giant_test-file_management EXEC=test/file_management all

testxml_val :
	make -C ../ MAIN=test/testxml_val EXEC=test/testxml_val all

evolution :
	make -C ../ MAIN=test/giant_test-evolution EXEC=test/evolution all

graph_lib : giant_test-graph_lib.adb
	make -C ../ MAIN=test/giant_test-graph_lib EXEC=test/graph_lib all

all:
	make -C ../ all

.PHONY : framework_test
