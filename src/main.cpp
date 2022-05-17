#include <cassert>
#include <iostream>
#include <cstring>
#include <fstream>
#include <string>

#include "manager.h"

static void init_linker(Options *options);

char *file_change_extension(const char *filename, const char *extension);
char *file_base_path(const char *filename);
static std::string string_replace_all(std::string str, const std::string& from, const std::string& to);

const char *get_win_conf_path();

void compile(Options *options) {
	manager_init(options);

    manager_run();
}

int main(int argc, char *argv[]) {
    if (argc <= 1) {
        std::cout << "usage: qwr <FILE>\n";
        std::cout << "\t -c\t\tCompile only\n";
        std::cout << "\t-release\tRelease build\n";
		std::cout << "\t-debug\t\tDebug build\n";
		std::cout << "\t-l\t\tLinker flags\n";
		std::cout << "\t-print-ir\tPrint LLVM IR\n";
		std::cout << "\t-x64\t\tUse x64 backend\n";
		std::cout << "\t-print-ast\tPrint AST\n";
		std::cout << "\t-v\t\tVerbose\n";
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
		} else if (strcmp(arg, "-v") == 0) {
			options.flags |= VERBOSE;
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

	init_linker(&options);

    compile(&options);

	return 0;
}

void init_linker(Options *options) {
#ifdef _WIN32
	std::ifstream conf_file(get_win_conf_path());
	if (!conf_file) {
		std::cerr << "Not configured.\n" << "Please run install.cmd first!\n";
		std::exit(0);
	}

	std::string linker;
	getline(conf_file, linker);
	linker = "\"" + string_replace_all(linker, "\\", "\\\\") + "\"";

	std::string stdlib_path;
	getline(conf_file, stdlib_path);
	stdlib_path = "\"-libpath:" + string_replace_all(stdlib_path, "\\", "\\\\") + "\"";

	std::string um_path;
	getline(conf_file, um_path);
	um_path = string_replace_all(um_path, "\\", "\\\\");

	std::string ucrt_path;
	getline(conf_file, ucrt_path);
	ucrt_path = string_replace_all(ucrt_path, "\\", "\\\\");

	options->obj_file = strdup(string_replace_all(options->obj_file, "/", "\\\\").c_str());

	options->linker_flags.push_back("-defaultlib:libcmt");
	options->linker_flags.push_back("-defaultlib:oldnames");
	options->linker_flags.push_back(strdup(stdlib_path.c_str()));
	options->linker_flags.push_back(strdup(um_path.c_str()));
	options->linker_flags.push_back(strdup(ucrt_path.c_str()));
	
	options->linker = strdup(linker.c_str());
#elif defined(__MACH__)
	options->linker_flags.push_back("-syslibroot");
	options->linker_flags.push_back("`xcrun --show-sdk-path`");
	options->linker_flags.push_back("-lSystem");

	options->linker = "ld";

#else

	options->linker = "ld";

#endif
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
        return (char *) "";

    char *base_path = new char[i + 2];
    strncpy(base_path, filename, i+1);
    base_path[i+1] = '\0';

    return base_path;
}

std::string string_replace_all(std::string str, const std::string& from, const std::string& to) {
	size_t start_pos = 0;
	while ((start_pos = str.find(from, start_pos)) != std::string::npos) {
		str.replace(start_pos, from.length(), to);
		start_pos += to.length();
	}
	return str;
}

const char *get_win_conf_path() {
	char *local_appdata;
	_dupenv_s(&local_appdata, 0, "LOCALAPPDATA");

	const char *win_conf_path = "\\qwr\\win_conf.txt";

	char *path = new char[strlen(local_appdata) + strlen(win_conf_path)];
	strcpy(path, local_appdata);
	strcat(path, win_conf_path);

	return path;
}