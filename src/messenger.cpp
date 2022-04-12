#include <iostream>
#include <sstream>

#include "messenger.h"

void Messenger::init(char *code) {
	std::stringstream code_stream(code);
	std::string line;
	
	while(std::getline(code_stream, line)) {
		code_lines.push_back(line);
	}
}

void Messenger::error(Token *token) {
	std::string line = code_lines[token->line];
	
	std::cout << "Error found in line " << (token->line + 1) << "\n";
	std::cout << line;
	std::cout << "\n";
	for (int i = 0; i < line.length(); ++i) {
		if (i >= token->col_from && i < token->col_to) {
			std::cout << "*";
		} else if (line[i] == '\t') {
			std::cout << "\t";
		} else {
			std::cout << " ";
		}
	}
	std::cout << "\n";
}

void Messenger::report(Token *token, const char *msg) {
	error(token);
	std::cout << msg << "\n";
	std::exit(0);
}

void Messenger::report_print_token(Token *token, const char *msg) {
	error(token);
	std::cout << msg << " ";
	token->print();
	std::exit(0);
}
