#include <chrono>
#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>

#include "manager.h"
#include "parser.h"
#include "llvmgen.h"
#include "x64.h"
#include "builtin.h"

#define TIMER_NOW std::chrono::steady_clock::now()
#define TIMER_DIFF(a, b) std::chrono::duration_cast<std::chrono::microseconds>(b - a).count()

typedef void(*code_gen_func)(Stmt *stmt);

static void manager_add_additional_file(const char *file_name);
static void llvm_gen_stmt(Stmt *stmt);
static size_t read_entire_file(const char *file_name, const char **contents);
const char *get_lib_path();

const char *LIB_PATH;

static Options *options;
static CodeGenerator llvm_code_gen;
static Messenger messenger;
static Parser parser;
static Typer typer;

static std::vector<const char *> parsed_files = {};

static long long parse_time = 0;
static long long gen_time = 0;
static long long link_time = 0;
static long long loc = 0;

static code_gen_func gen_func;

void manager_init(Options *_options) {
    options = _options;

	typer.init(&llvm_code_gen.llvm_context, &messenger);
	init_builtins(&typer);
	parser.init(&typer, &messenger);

	if (options->flags & X64_BACKEND) {
		gen_func = x64_gen;
		x64_init();
	} else {
		gen_func = llvm_gen_stmt;
		llvm_code_gen.init(&typer, options);
	}

	LIB_PATH = get_lib_path();
}

void manager_run() {
	auto total_start = TIMER_NOW;

    const char *code;
    auto code_len = read_entire_file(options->src_file, &code);

	if (!(options->flags & X64_BACKEND) && options->flags & QWR_DEBUG) {
		llvm_code_gen.init_debug(options->src_file);
	}

    messenger.open_file(options->src_file, code);
    parser.lexer.init(code, code_len);

	if (!(options->flags & X64_BACKEND)) {
		manager_add_library("qwr");
	}

    Stmt *stmt;

    while (true) {
        auto start = TIMER_NOW;

        stmt = parser.parse_top_level_stmt();

        auto end = TIMER_NOW;
        auto diff = TIMER_DIFF(start, end);
        parse_time += diff;

        if (parser.has_reached_end) {
            break;
        }

        if (!stmt) {
            continue;
        }

        start = TIMER_NOW;

		gen_func(stmt);

        end = TIMER_NOW;
        diff = TIMER_DIFF(start, end);

		gen_time += diff;
	}

	auto start = TIMER_NOW;

	if (options->flags & X64_BACKEND) {
		x64_dump(options);
	} else {
		llvm_code_gen.output(options);

		auto end = TIMER_NOW;
		auto diff = TIMER_DIFF(start, end);

		gen_time += diff;

		start = TIMER_NOW;

		if ((options->flags & COMPILE_ONLY) == 0) {
			llvm_code_gen.link(options);
		}

		if (options->flags & PRINT_LLVM) {
			llvm_code_gen.dump(options);
		}

		end = TIMER_NOW;
		diff = TIMER_DIFF(start, end);
		link_time = diff;
	}

    auto total_end = std::chrono::steady_clock::now();
    auto total_time = TIMER_DIFF(total_start, total_end);

    if (options->flags & VERBOSE) {
        std::cout << "Program LOC: " << loc << "\n";
        std::cout << "Compilation: " << (total_time / 1000) << " ms\n";
        std::cout << "Parsing: " << (parse_time / 1000) << " ms\n";

        if (options->flags & X64_BACKEND) {
            std::cout << "x64 backend: " << (gen_time / 1000) << " ms\n";
        } else {
            std::cout << "LLVM backend: " << (gen_time / 1000) << " ms\n";
        }
        std::cout << "Linking: " << (link_time / 1000) << " ms\n";
    }
}

void manager_add_library(const char *lib_name) {
    // + 5 = ".qwr" + \0
    char *full_lib_path = new char[strlen(LIB_PATH) + strlen(lib_name) + 5];
    strcpy(full_lib_path, LIB_PATH);
    strcat(full_lib_path, lib_name);
    strcat(full_lib_path, ".qwr");

    manager_add_additional_file(full_lib_path);
}

void manager_add_src_file(const char *file_name) {
    // + 5 = ".qwr" + \0
    char *base_path = options->base_path;
    char *full_file_path = new char[strlen(base_path) + strlen(file_name) + 5];
    strcpy(full_file_path, base_path);
    strcat(full_file_path, file_name);
    strcat(full_file_path, ".qwr");

    manager_add_additional_file(full_file_path);
}

void manager_add_flags(const char *flags) {
    options->linker_flags.push_back(flags);
}

void manager_add_additional_file(const char *file_name) {
	for (auto parsed_file : parsed_files)
		if (strcmp(parsed_file, file_name) == 0)
			return;

    parsed_files.push_back(file_name);

    const char *code;
    auto code_len = read_entire_file(file_name, &code);

    auto old_file = messenger.current_file;
	messenger.open_file(file_name, code);
    parser.lexer.backup();
    parser.lexer.init(code, code_len);

    Stmt *stmt;

    while (true) {
        auto start = TIMER_NOW;

        stmt = parser.parse_top_level_stmt();
        if (parser.has_reached_end) {
            break;
        }

        auto end = TIMER_NOW;
        auto diff = TIMER_DIFF(start, end);
        parse_time += diff;

        start = TIMER_NOW;

		gen_func(stmt);

        end = TIMER_NOW;
        diff = TIMER_DIFF(start, end);

        gen_time += diff;
	}

    messenger.open_file(old_file, "");
    parser.has_reached_end = false;
	parser.lexer.restore();
}

void llvm_gen_stmt(Stmt *stmt) {
	llvm_code_gen.gen(stmt);
}

size_t read_entire_file(const char *file_name, const char **contents) {
    FILE *f = fopen(file_name, "rb");
    if (!f) {
        std::cout << "Failed to open file " << file_name << "\n";
        std::exit(1);
    }

	fseek(f, 0, SEEK_END);
	size_t len = ftell(f);
	fseek(f, 0, SEEK_SET);

	char *buffer = new char[len + 1];
	if (!fread(buffer, 1, len, f)) {
		fclose(f);
		return 0;
	}

    fclose(f);

	buffer[len] = '\0';
	*contents = buffer;

    std::ifstream file_stream(file_name); 
    loc += std::count(std::istreambuf_iterator<char>(file_stream), 
             std::istreambuf_iterator<char>(), '\n');

	return len;
}

const char *get_lib_path() {
#ifdef _WIN32
	char *local_appdata;
	_dupenv_s(&local_appdata, 0, "LOCALAPPDATA");

	const char *lib_path = "\\qwr\\";

	char *path = new char[strlen(local_appdata) + strlen(lib_path)];
	strcpy(path, local_appdata);
	strcat(path, lib_path);

	return path;
#else
	return "/usr/local/bin/qwrstd/";
#endif
}
