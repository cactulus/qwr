#ifndef MANAGER_H_
#define MANAGER_H_

#include <vector>

#include "gen.h"
#include "options.h"
#include "parser.h"

struct Manager {
	CodeGenerator code_gen;
	Messenger messenger;
	Parser parser;
	Typer typer;

	void init();

	void run(const char *src_file, Options options);
};

#endif
