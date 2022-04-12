#ifndef MANAGER_H_
#define MANAGER_H_

#include <vector>

#include "gen.h"
#include "parser.h"

struct Manager {
	CodeGenerator code_gen;
	Messenger messenger;
	Parser parser;
	Typer typer;

	void init(char *code, int code_len);

	void run();
};

#endif
