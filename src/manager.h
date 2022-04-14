#ifndef MANAGER_H_
#define MANAGER_H_

#include <vector>

#include "gen.h"
#include "parser.h"

const u8 COMPILE_ONLY = 0x1;
const u8 OPTIMIZE = 0x2;

struct Manager {
	CodeGenerator code_gen;
	Messenger messenger;
	Parser parser;
	Typer typer;

	void init();

	void run(const char *src_file, u8 flags);
};

#endif
