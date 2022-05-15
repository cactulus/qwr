#include <cassert>
#include <iostream>
#include <cstring>

#include "manager.h"

char *file_change_extension(const char *filename, const char *extension);
char *file_base_path(const char *filename);

void compile(Options *options) {
	manager_init(options);

    manager_run();
}

int main(int argc, char *argv[]) {
    if (argc <= 1) {
        std::cout << "usage: qwr <FILE>\n";
        std::cout << "\t -c\t\tCompile only\n";
        std::cout << "\t-release\tRelease build\n";
		std::cout << "\t-debug\tDebug build\n";
		std::cout << "\t-l\tLinker flags\n";
		std::cout << "\t-print-ir\tPrint LLVM IR\n";
		std::cout << "\t-x64\tUse x64 backend\n";
		std::cout << "\t-print-tokens\tPrint tokens\n";
		std::cout << "\t-print-ast\tPrint AST\n";
		std::cout << "\t-callgraph\tOutput callgraph\n";
        return 0;
    }

    Options options = {};
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
		} else if (strcmp(arg, "-x64") == 0) {
			options.flags |= X64_BACKEND;
		} else if (strcmp(arg, "-print-ast") == 0) {
			options.flags |= PRINT_AST;
		} else if (strcmp(arg, "-callgraph") == 0) {
			options.flags |= CALL_GRAPH;
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
            options.ll_file = file_change_extension(options.src_file, ".ll");
            options.obj_file = file_change_extension(options.src_file, obj_ext);
            options.exe_file = file_change_extension(options.src_file, exe_ext);
            options.cgraph_file = file_change_extension(options.src_file, ".callgraph.dot");
            options.base_path = file_base_path(options.src_file);
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

char *file_change_extension(const char *filename, const char *extension) {
	char *new_filename = new char[strlen(filename) + strlen(extension) + 1];
	strcpy(new_filename, filename);
	char *dot = strrchr(new_filename, '.');
	if (dot) {
		*dot = '\0';
	}
	strcat(new_filename, extension);
	return new_filename;
}

char *file_base_path(const char *filename) {
    int len = strlen(filename);
    int i;
    for (i = len - 1; i >= 0; --i) {
        if (filename[i] == '/') {
            break;
        }
    }

    if (i == 0)
        return "";

    char *base_path = new char[i + 2];
    strncpy(base_path, filename, i+1);
    base_path[i+1] = '\0';

    return base_path;
}
