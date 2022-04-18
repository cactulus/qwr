#include <cassert>
#include <iostream>
#include <cstring>

#include "manager.h"

void compile(const char *src_file, Options options) {
	Manager manager;
	manager.init();

    try {
        manager.run(src_file, options);
    } catch (const char *e) {
        std::cout << e << "\n";
    }
}

int main(int argc, char *argv[]) {
    if (argc <= 1) {
        std::cout << "usage: qwr <FILE>\n";
        std::cout << "\t -c\t\tCompile only\n";
        std::cout << "\t--release\tRelease build\n";
        return 0;
    }
    const char *src_file = "";
    Options options;

    argc--; argv++;
    while (argc--) {
        char *arg = *argv++;
        
        if (strcmp(arg, "--release") == 0) {
            options.flags |= OPTIMIZE;
        } else if (strcmp(arg, "-c") == 0) {
            options.flags |= COMPILE_ONLY;
        } else if (strcmp(arg, "-l") == 0) {
            const char *lib = *argv++;
            argc--;
            options.libs.push_back(lib);
        } else {
            src_file = arg;
        }
    }

    compile(src_file, options);

	return 0;
}
