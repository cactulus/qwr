#ifndef MESSENGER_H_
#define MESSENGER_H_

#include <unordered_map>

#include "lexer.h"

struct Messenger {
	std::unordered_map<std::string, std::vector<std::string>> file_contents;
	std::string current_file;

	void open_file(const std::string &file, const char *code);

	void error(Token *token);

	void report(Token *token, const char *msg);
	void report_print_token(Token *token, const char *msg);
};


#endif
