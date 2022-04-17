#include <iostream>
#include <sstream>

#ifdef DIAGNOSTICS
#include <chrono>
#include <fstream>
#define TIMER_NOW std::chrono::steady_clock::now()
#define TIMER_DIFF(a, b) std::chrono::duration_cast<std::chrono::microseconds>(b - a).count()
#endif

#include "manager.h"
#include "arena.h"

size_t read_entire_file(FILE *f, char **contents);
char *change_file_extension(const char *filename, const char *extension);

void Manager::init() {
	parser.init(&typer, &messenger);
	code_gen.init(&typer);
	typer.init(&code_gen.llvm_context, &messenger);

	arena_init();
}

void Manager::run(const char *src_file, Options options) {
#ifdef DIAGNOSTICS
	long long parse_time = 0;
	long long llvm_time = 0;
	long long link_time = 0;

	auto total_start = TIMER_NOW;
#endif 

	FILE *input_file = fopen(src_file, "r");
	char *code;
	size_t code_len = read_entire_file(input_file, &code);
	fclose(input_file);

	messenger.init(code);
	parser.lexer.init(code, code_len);

	Stmt *stmt;

	while (true) {
#ifdef DIAGNOSTICS
		auto start = TIMER_NOW;
#endif

		stmt = parser.parse_top_level_stmt();
		if (parser.has_reached_end) {
			break;
		}

#ifdef DIAGNOSTICS
		auto end = TIMER_NOW;
		auto diff = TIMER_DIFF(start, end);
		parse_time += diff;

		start = TIMER_NOW;
#endif

		code_gen.gen_stmt(stmt);

#ifdef DIAGNOSTICS
		end = TIMER_NOW;
		diff = TIMER_DIFF(start, end);

		llvm_time += diff;
#endif 
	}
	const char *obj_ext;
	const char *exe_ext;
#ifdef _WIN32
        obj_ext = ".obj";
        exe_ext = ".exe";
#else
        obj_ext = ".o";
        exe_ext = "";
#endif

#ifdef DIAGNOSTICS
	auto start = TIMER_NOW;
#endif
	char *obj_file = change_file_extension(src_file, obj_ext);
	char *exe_file = change_file_extension(src_file, exe_ext);
	code_gen.output(obj_file, options);
    code_gen.dump();
#ifdef DIAGNOSTICS
	auto end = TIMER_NOW;
	auto diff = TIMER_DIFF(start, end);

	llvm_time += diff;
#endif

#ifdef DIAGNOSTICS
	start = TIMER_NOW;
#endif

    if ((options.flags & COMPILE_ONLY) == 0) {
	    code_gen.link(obj_file, exe_file, options);
    }

#ifdef DIAGNOSTICS
	end = TIMER_NOW;
	diff = TIMER_DIFF(start, end);
	link_time = diff;

    auto total_end = std::chrono::steady_clock::now();
    auto total_time = TIMER_DIFF(total_start, total_end);

    std::ifstream file_stream(src_file); 
    auto loc = std::count(std::istreambuf_iterator<char>(file_stream), 
             std::istreambuf_iterator<char>(), '\n');

    std::cout << "Program LOC: " << loc << "\n";
    std::cout << "Compilation time: " << (total_time / 1000) << " ms\n";
	std::cout << "Parse time: " << (parse_time / 1000) << " ms\n";
	std::cout << "LLVM time: " << (llvm_time / 1000) << " ms\n";
	std::cout << "Link time: " << (link_time / 1000) << " ms\n";
#endif
}

size_t read_entire_file(FILE *f, char **contents) {
	fseek(f, 0, SEEK_END);
	size_t len = ftell(f);
	fseek(f, 0, SEEK_SET);

	char *buffer = new char[len + 1];
	if (!fread(buffer, 1, len, f)) {
		fclose(f);
		return 0;
	}

	buffer[len] = '\0';
	*contents = buffer;

	return len;
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
