#include <cassert>
#include <cctype>
#include <cstring>
#include <iostream>
#include <unordered_map>
#include <string>

#include "lexer.h"
#include "arena.h"

static int read_xchars(Lexer *l, char **output, bool (*check) (char c));
static void read_atom_or_keyword(Lexer *l, Token *t);
static void read_int_lit(Lexer *l, Token *t);

static bool is_ident_char(char c);
static bool is_digit(char c);

const char *operator_chars = "+-=*/();,.{}&|:";

const std::unordered_map<std::string, TokenType> keyword_tokens = {
	{"return", TOKEN_RETURN},
	{"extern", TOKEN_EXTERN},
	{"as", TOKEN_AS},
};

const std::unordered_map<std::string, TokenType> two_char_tokens = {
	{"::", TOKEN_COLON_COLON},
	{"..", TOKEN_DOT_DOT},
	{"+=", TOKEN_ADD_EQ},
	{"-=", TOKEN_SUB_EQ},
	{"*=", TOKEN_MUL_EQ},
	{"/=", TOKEN_DIV_EQ},
	{"%=", TOKEN_MOD_EQ},
}; 

void Lexer::init(char *code, int code_len) {
	input = code;
	input_len = code_len;
	col = 0;
	line = 0;
	pos = 0;
	token_index = 0;
}

Token *Lexer::peek_token(int ahead) {
	assert(input);

	int token_pos = token_index + ahead;
	if (token_pos < tokens.size()) {
		return tokens[token_pos];
	}

	int i = 0;
	while (i <= ahead) {
		tokens.push_back(read_token());
		i++;
	}
	 
	return tokens[token_pos];
}

Token *Lexer::last_token(int amount) {
	int idx = token_index - amount;
	assert(idx >= 0 && idx < tokens.size());
	return tokens[idx];
}

void Lexer::eat_token() {
	token_index++;
}

Token *Lexer::read_token() {
	auto c = peek_char();
	while (isspace(c)) {
		eat_char();
		c = peek_char();
	}

	auto t = create_token();
	set_token_start(t);

	if (c == '\0') {
		t->type = TOKEN_EOF;
	} else if (c == '_' || isalpha(c)) {
		read_atom_or_keyword(this, t);
	} else if (is_digit(c)) {
		read_int_lit(this, t);
	} else {
		std::string possible_two_char_token = std::string(1, c) + peek_char(1);
		auto two_char_token_it = two_char_tokens.find(possible_two_char_token);

		if (two_char_token_it != two_char_tokens.end()) {
			t->type = two_char_token_it->second;
			eat_char();
			eat_char();
		} else {
			bool found = false;
			for (int i = 0; i < strlen(operator_chars); ++i) {
				if (operator_chars[i] == c) {
					t->type = (TokenType) c;
					found = true;
				}
			}
			eat_char();

			if (!found) {
				std::cout << "Encountered unexpected char '" << c << "'\n";
				std::cout << "in line " << (line + 1) << ", col " << col << "\n";
				exit(0);
			}
		}
	}

	set_token_end(t);
	return t;
}

void Lexer::set_token_start(Token *t) {
	t->col_from = col;
	t->line = line;
}

void Lexer::set_token_end(Token *t) {
	t->col_to = col;
}

char Lexer::peek_char(int ahead) {
	auto p = pos + ahead;
	if (p < input_len) 
		return input[p];
	return '\0';
}

void Lexer::eat_char() {
	auto c = input[pos];
	if (c == '\n') {
		line++;
		col = 0;
		pos++;
		return;
	}
	col++;
	pos++;
}

int read_xchars(Lexer *l, char **output, bool (*check) (char c)) {
	auto start_pos = l->pos;
	auto c = l->peek_char();

	while (check(c)) {
		l->eat_char();
		c = l->peek_char();
	}

	auto end_pos = l->pos;

	auto len = end_pos - start_pos;
	auto name = new char[len + 1];
	strncpy(name, &l->input[start_pos], len);
	name[len] = '\0';
	*output = name;
	return len;
}

void read_atom_or_keyword(Lexer *l, Token *t) {
	char *lexeme;
	int lexeme_len = read_xchars(l, &lexeme, is_ident_char);

	auto it = keyword_tokens.find(std::string(lexeme));
	if (it != keyword_tokens.end()) {
		t->type = it->second;	
	} else {
		t->type = TOKEN_ATOM;
		t->atom.name = lexeme;
		t->atom.len = lexeme_len;
	}
}

void read_int_lit(Lexer *l, Token *t) {
	char *num;
	read_xchars(l, &num, is_digit);

	t->type = TOKEN_INT_LIT;
	t->int_value = atol(num);
}

bool is_ident_char(char c) {
	return c == '_' || isalnum(c);
}

bool is_digit(char c) {
	return '0' <= c && c <= '9';
}