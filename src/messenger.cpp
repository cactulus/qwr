#include <iostream>
#include <sstream>

#include "messenger.h"

void Messenger::open_file(const std::string &file, const char *code) {
	current_file = file;

	auto file_it = file_contents.find(file);
	if (file_it != file_contents.end()) {
		return;
	}

	std::stringstream code_stream(code);
	std::vector<std::string> code_lines;
	std::string line;

	while (std::getline(code_stream, line)) {
		code_lines.push_back(line);
	}

	file_contents.insert({ file, code_lines });
}

void Messenger::error(Token *token) {
	std::vector<std::string> &code_lines = file_contents[current_file];
	auto loc = token->location;
	std::string line = code_lines[loc.line];
	
	std::cout << "Error found in line " << (loc.line + 1) << "\n";
	std::cout << line;
	std::cout << "\n";
	for (int i = 0; i < line.length(); ++i) {
		if (i >= loc.col_from && i < loc.col_to) {
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
