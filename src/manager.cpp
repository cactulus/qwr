#include <iostream>
#include <sstream>

#ifdef DIAGNOSTICS
#include <chrono>
#define TIMER_NOW std::chrono::steady_clock::now()
#define TIMER_DIFF std::chrono::duration_cast<std::chrono::microseconds>(end - start).count()
#endif

#include "manager.h"
#include "arena.h"

void Manager::init(char *code, int code_len) {
	messenger.init(code);
	parser.init(&typer, &messenger);
	code_gen.init(&typer);
	typer.init(&code_gen.llvm_context, &messenger);
	parser.lexer.init(code, code_len);

	arena_init();
}

void Manager::run() {
    #ifdef DIAGNOSTICS
	long long parse_time = 0;
	long long llvm_time = 0;

    auto start = TIMER_NOW;
    #endif 

	Stmt *stmt = parser.parse_top_level_stmt();

    #ifdef DIAGNOSTICS
    auto end = TIMER_NOW;
    auto diff = TIMER_DIFF;

	parse_time += diff;
	#endif

	while (!parser.has_reached_end) {
		#ifdef DIAGNOSTICS
		start = TIMER_NOW;
		#endif

		code_gen.gen_stmt(stmt);

		#ifdef DIAGNOSTICS
		end = TIMER_NOW;
		diff = TIMER_DIFF;

		llvm_time += diff;
		start = TIMER_NOW;
		#endif 

		stmt = parser.parse_top_level_stmt();

		#ifdef DIAGNOSTICS
		auto end = TIMER_NOW;
		auto diff = TIMER_DIFF;

		parse_time += diff;
		#endif
	}
	code_gen.dump();

	#ifdef DIAGNOSTICS
	std::cout << "Parse time: " << (parse_time / 1000) << " ms\n";
	std::cout << "LLVM time: " << (llvm_time / 1000) << " ms\n";
	#endif
}
