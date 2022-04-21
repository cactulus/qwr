#include <cassert>
#include <iostream>
#include <cstring>

#include "manager.h"

char *change_file_extension(const char *filename, const char *extension);

void compile(Options *options) {
	manager_init(options);

    manager_run();
}

int main(int argc, char *argv[]) {
    if (argc <= 1) {
        std::cout << "usage: qwr <FILE>\n";
        std::cout << "\t -c\t\tCompile only\n";
        std::cout << "\t--release\tRelease build\n";
        return 0;
    }

    Options options;
    options.src_file = 0;

    argc--; argv++;
    while (argc--) {
        char *arg = *argv++;
        
        if (strcmp(arg, "-release") == 0) {
            options.flags |= OPTIMIZE;
        } else if (strcmp(arg, "-debug") == 0) {
            options.flags |= DEBUG;
        } else if (strcmp(arg, "-print-ir") == 0) {
            options.flags |= PRINT_LLVM;
        } else if (strcmp(arg, "-c") == 0) {
            options.flags |= COMPILE_ONLY;
        } else if (strcmp(arg, "-l") == 0) {
            const char *lib = *argv++;
            argc--;
            options.libs.push_back(lib);
        } else {
            const char *obj_ext;
            const char *exe_ext;

#ifdef _WIN32
            obj_ext = ".obj";
            exe_ext = ".exe";
#else
            obj_ext = ".o";
            exe_ext = "";
#endif
            options.src_file = arg;
            options.ll_file = change_file_extension(options.src_file, ".ll");
            options.obj_file = change_file_extension(options.src_file, obj_ext);
            options.exe_file = change_file_extension(options.src_file, exe_ext);
        }
    }

    if (!options.src_file) {
        std::cout << "No input file\n";
        exit(1);
    }

    if ((options.flags & DEBUG) && (options.flags & OPTIMIZE)) {
        std::cout << "-debug and -release cannot be specified at the same time\n";
        exit(1);
    }

    compile(&options);

	return 0;
}

char *change_file_extension(const char *filename, const char *extension) {
	char *new_filename = new char[strlen(filename) + strlen(extension) + 1];
	strcpy(new_filename, filename);
	char *dot = strrchr(new_filename, '.');
	if (dot) {
		*dot = '\0';
	}
	strcat(new_filename, extension);
	return new_filename;
}
