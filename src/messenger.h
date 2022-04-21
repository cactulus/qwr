#ifndef MESSENGER_H_
#define MESSENGER_H_

#include "lexer.h"

struct Messenger {
	std::vector<std::string> code_lines;

	void init(const char *code);

	void error(Token *token);

	void report(Token *token, const char *msg);
	void report_print_token(Token *token, const char *msg);
};


#endif
