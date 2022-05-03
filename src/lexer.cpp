#include <cassert>
#include <cctype>
#include <cstring>
#include <iostream>
#include <unordered_map>
#include <string>
#include <stack>

#include "lexer.h"
#include "alloc.h"

struct LexerInfo {
    const char *input;
    int input_len;
    int line;
    int col;
    int pos;
};

static int read_xchars(Lexer *l, const char **output, bool (*check) (char c));
static void read_atom_or_keyword(Lexer *l, Token *t);
static void read_int_or_float(Lexer *l, Token *t);
static void read_string_lit(Lexer *l, Token *t);
static void read_char_lit(Lexer *l, Token *t);
static void read_hex_number(Lexer *l, Token *t);

static bool is_ident_char(char c);
static bool is_digit(char c);
static bool is_hex_digit(char c);
static bool is_not_quote(char c);

const char *escape_str_lit(const char *text);
static char make_escape(char c);

const char *operator_chars = "+-=*/();,.{}&|:<>![]@%";

static std::stack<LexerInfo> backups;

const std::unordered_map<std::string, TokenType> keyword_tokens = {
	{"return", TOKEN_RETURN},
	{"extern", TOKEN_EXTERN},
	{"builtin", TOKEN_BUILTIN},
	{"as", TOKEN_AS},
	{"true", TOKEN_TRUE},
	{"false", TOKEN_FALSE},
	{"nil", TOKEN_NIL},
	{"if", TOKEN_IF},
	{"else", TOKEN_ELSE},
	{"while", TOKEN_WHILE},
	{"for", TOKEN_FOR},
	{"new", TOKEN_NEW},
	{"delete", TOKEN_DELETE},
	{"enum", TOKEN_ENUM},
	{"struct", TOKEN_STRUCT},
	{"use", TOKEN_USE},
	{"qwr", TOKEN_QWR},
};

const std::unordered_map<std::string, TokenType> two_char_tokens = {
	{"::", TOKEN_COLON_COLON},
	{"..", TOKEN_DOT_DOT},
	{"+=", TOKEN_ADD_EQ},
	{"-=", TOKEN_SUB_EQ},
	{"*=", TOKEN_MUL_EQ},
	{"/=", TOKEN_DIV_EQ},
	{"%=", TOKEN_MOD_EQ},
	{"==", TOKEN_EQ_EQ},
	{"!=", TOKEN_NOT_EQ},
	{"<=", TOKEN_LT_EQ},
	{">=", TOKEN_GT_EQ},
	{"&&", TOKEN_AND_AND},
	{"||", TOKEN_BAR_BAR},
	{"++", TOKEN_PLUS_PLUS},
	{"--", TOKEN_MINUS_MINUS},
}; 

void Lexer::init(const char *code, int code_len) {
	tokens.clear();
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

    if (c == '/' && peek_char(1) == '/') {
        while (c != '\n') {
            eat_char();
            c = peek_char();
        }
        eat_char();
        return read_token();
    }

    if (c == '/' && peek_char(1) == '*') {
        while (c != '*' || peek_char(1) != '/') {
            eat_char();
            c = peek_char();
        }
        eat_char();
        eat_char();
        return read_token();
    }

	auto t = create_token();
	set_token_start(t);

	if (c == '\0') {
		t->type = TOKEN_EOF;
	} else if (c == '_' || isalpha(c)) {
		read_atom_or_keyword(this, t);
	} else if(c == '0' && peek_char(1) == 'x') {
		read_hex_number(this, t);
	} else if (is_digit(c)) {
		read_int_or_float(this, t);
    } else if (c == '"') {
        read_string_lit(this, t);
    } else if (c == '\'') {
        read_char_lit(this, t);
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

int read_xchars(Lexer *l, const char **output, bool (*check) (char c)) {
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
	const char *lexeme;
	read_xchars(l, &lexeme, is_ident_char);

	auto it = keyword_tokens.find(std::string(lexeme));
	if (it != keyword_tokens.end()) {
		t->type = it->second;	
	} else {
		t->type = TOKEN_ATOM;
		t->lexeme = lexeme;
	}
}

void read_int_or_float(Lexer *l, Token *t) {
	const char *num;
	auto num1_len = read_xchars(l, &num, is_digit);
	
	if (l->peek_char() == '.' && l->peek_char(1) != '.') {
		l->eat_char();

		const char *num2;
		auto num2_len = read_xchars(l, &num2, is_digit);

		auto len = num1_len + num2_len;
		char *float_num = new char[len + 2];

		strcpy(float_num, num);
		strcat(float_num, ".");
		strcat(float_num, num2);

		float_num[len + 1] = '\0';
			
		t->type = TOKEN_FLOAT_LIT;
		t->float_value = atof(float_num);
	} else {
		t->type = TOKEN_INT_LIT;
		t->int_value = atol(num);
	}
}

void read_string_lit(Lexer *l, Token *t) {
    l->eat_char();
    const char *lit;
    int lit_len = read_xchars(l, &lit, is_not_quote);
    l->eat_char();

    t->type = TOKEN_STRING_LIT;
    t->lexeme = escape_str_lit(lit);
}

void read_char_lit(Lexer *l, Token *t) {
    l->eat_char();
    char c = l->peek_char();

    l->eat_char();
    char end = l->peek_char();

    if (c == '\\') {
        c = make_escape(end);

        l->eat_char();
        end = l->peek_char();
    }
        
    l->eat_char();

    if (end != '\'') {
        std::cout << "Expected ' but got " << c << "\n";
        std::cout << "in line " << (l->line + 1) << ", col " << l->col << "\n";
        exit(0);
    }

    t->type = TOKEN_CHAR_LIT;
    t->char_value = c;
}

void read_hex_number(Lexer *l, Token *t) {
	l->eat_char();
	l->eat_char();

	const char *num;
	int num_len = read_xchars(l, &num, is_hex_digit);

	t->type = TOKEN_INT_LIT;
	t->int_value = strtol(num, 0, 16);
}

bool is_ident_char(char c) {
	return c == '_' || isalnum(c);
}

bool is_digit(char c) {
	return '0' <= c && c <= '9';
}

bool is_hex_digit(char c) {
	return ('0' <= c && c <= '9') || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F');
}

bool is_not_quote(char c) {
    return c != '"';
}

const char *escape_str_lit(const char *text) {
    int sl = strlen(text), nl = sl, i, j;
    char *etext;

    for (i = 0; i < sl; ++i) {
        if (text[i] != '\\') continue;

        switch (text[i+1]) {
            case 'n':
            case 'r':
            case 't':
            case '0':
                nl--;
                break;
            default:
                break;
        }
    }

    etext = new char [nl + 1];
    for (i = 0, j = 0; i < sl; ++i, ++j) {
        if (text[i] != '\\') {
            etext[i] = text[i]; 
            continue;
        }

        char esc = make_escape(text[i+1]);
        etext[j] = esc;
		i++;
    }

    etext[nl] = '\0';
    return etext;
}

char make_escape(char c) {
    switch (c) {
        case 'n':
            return '\n';
        case 'r':
            return '\t';
        case 't':
            return '\r';
        case '0':
            return '\0';
        default:
            return c;
    }
}

void Lexer::backup() {
    LexerInfo info;

    info.input = input;
    info.input_len = input_len;
    info.line = line;
    info.col = col;
    info.pos = pos;

    backups.push(info);
}

void Lexer::restore() {
    LexerInfo info = backups.top();

    tokens.clear();
    input = info.input;
    input_len = info.input_len;
    line = info.line;
    col = info.col;
    pos = info.pos;
    token_index = 0;

    backups.pop();
}

bool ttype_is_binary(int type) {
    switch (type) {
        case '+':
        case '-':
        case '*':
        case '/':
        case '%':
            return true;
        default:
            return false;
    }
}

bool ttype_is_binary(TokenType type) {
    return ttype_is_binary((int) type);
}

bool ttype_is_conditional(int type) {
    if (type >= TOKEN_EQ_EQ && type <= TOKEN_GT_EQ) {
        return true;
    }

    if (type == '<' || type == '>') {
        return true;
    }

    return false;
}

bool ttype_is_conditional(TokenType type) {
    return ttype_is_conditional((int) type);
}

bool ttype_is_assign(int type) {
    return type == '=' || (type >= TOKEN_ADD_EQ && type <= TOKEN_MOD_EQ);
}

bool ttype_is_assign(TokenType type) {
    return ttype_is_assign((int) type);
}

bool ttype_is_logical(int type) {
    return type == TOKEN_AND_AND || type == TOKEN_BAR_BAR;
}

bool ttype_is_logical(TokenType type) {
    return ttype_is_logical((int) type);
}
