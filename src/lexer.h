
#ifndef LEXER_H_
#define LEXER_H_

#include <queue>

enum TokenType {
	TOKEN_ERROR = 0,
	TOKEN_EOF = 1,
	TOKEN_ATOM = 2,
	TOKEN_INT_LIT,

	TOKEN_RETURN,
	TOKEN_EXTERN,
	TOKEN_AS,

	TOKEN_COLON_COLON,
	TOKEN_DOT_DOT,

	TOKEN_ADD_EQ,
	TOKEN_SUB_EQ,
	TOKEN_MUL_EQ,
	TOKEN_DIV_EQ,
	TOKEN_MOD_EQ,
};

struct Token {
	TokenType type;

	int col_from;
	int col_to;

	int line;

	union {
		struct {
			char *name;
			int len;
		} atom;
		long int_value;
	};

	void print();
};

struct Lexer {
	std::vector<Token *> tokens;
	char *input;
	int input_len;

	int line;
	int col;
	int pos;
	int token_index;

	void init(char *code, int code_len);

	Token *peek_token(int ahead=0);
	Token *last_token(int amount=1);
	void eat_token();

	Token *read_token();

	void set_token_start(Token *t);
	void set_token_end(Token *t);

	char peek_char(int ahead=0);
	void eat_char();
};

#endif
