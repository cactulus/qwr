#ifndef LEXER_H_
#define LEXER_H_

#include <queue>

enum TokenType {
	TOKEN_ERROR = 0,
	TOKEN_EOF = 1,

	TOKEN_COLON_COLON,
	TOKEN_DOT_DOT,

	TOKEN_PLUS_PLUS,
	TOKEN_MINUS_MINUS,

	TOKEN_ADD_EQ,
	TOKEN_SUB_EQ,
	TOKEN_MUL_EQ,
	TOKEN_DIV_EQ,
	TOKEN_MOD_EQ,

	TOKEN_EQ_EQ,
	TOKEN_NOT_EQ,
	TOKEN_LT_EQ,
	TOKEN_GT_EQ,

	TOKEN_AND_AND,
	TOKEN_BAR_BAR,
	
	TOKEN_ATOM = 128,
	TOKEN_INT_LIT,
	TOKEN_FLOAT_LIT,
    TOKEN_STRING_LIT,
    TOKEN_CHAR_LIT,

	TOKEN_RETURN,
	TOKEN_EXTERN,
	TOKEN_BUILTIN,
	TOKEN_AS,
	TOKEN_TRUE,
	TOKEN_FALSE,
	TOKEN_NIL,
	TOKEN_IF,
	TOKEN_ELSE,
	TOKEN_WHILE,
	TOKEN_FOR,
	TOKEN_NEW,
	TOKEN_DELETE,
	TOKEN_ENUM,
	TOKEN_STRUCT,
	TOKEN_USE,
	TOKEN_TYPEDEF,

	TOKEN_QWR,
};

struct Token {
	TokenType type;

	int col_from;
	int col_to;

	int line;

	union {
		const char *lexeme;
		long int_value;
		char char_value;
		double float_value;
	};

	void print();
};

struct Lexer {
	std::vector<Token *> tokens;
	const char *input;
	int input_len;

	int line;
	int col;
	int pos;
	int token_index;

	void init(const char *code, int code_len);

	Token *peek_token(int ahead=0);
	Token *last_token(int amount=1);
	void eat_token();

	Token *read_token();

	void set_token_start(Token *t);
	void set_token_end(Token *t);

	char peek_char(int ahead=0);
	void eat_char();

	void backup();
	void restore();
};

bool ttype_is_binary(int type);
bool ttype_is_binary(TokenType type);
bool ttype_is_conditional(int type);
bool ttype_is_conditional(TokenType type);
bool ttype_is_assign(int type);
bool ttype_is_assign(TokenType type);
bool ttype_is_logical(int type);
bool ttype_is_logical(TokenType type);

#endif
