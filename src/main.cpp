#include <cassert>
#include <iostream>
#include <cstring>

#include "manager.h"

void compile(const char *src_file) {
	Manager manager;
	manager.init();

	manager.run(src_file);
}

int main() {

    const char *src_file = "examples/test.qwr";
    compile(src_file);

	return 0;
}
