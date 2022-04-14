#include <cassert>
#include <iostream>
#include <cstring>

#include "manager.h"

void compile(const char *src_file, u8 flags) {
	Manager manager;
	manager.init();

	manager.run(src_file, flags);
}

int main(int argc, char *argv[]) {
    if (argc <= 1) {
        std::cout << "usage: qwr <FILE>\n";
        std::cout << "\t -c\t\tCompile only\n";
        std::cout << "\t--release\tRelease build\n";
        return 0;
    }
    const char *src_file = "";
    u8 flags;

    argc--; argv++;
    while (argc--) {
        char *arg = *argv++;
        
        if (strcmp(arg, "--release") == 0) {
            flags |= OPTIMIZE;
        } else if (strcmp(arg, "-c") == 0) {
            flags |= COMPILE_ONLY;
        } else {
            src_file = arg;
        }
    }

    compile(src_file, flags);

	return 0;
}
