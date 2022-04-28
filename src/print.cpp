#include <iostream>
#include <unordered_map>
#include <string>

#include "lexer.h"

const std::unordered_map<TokenType, std::string> keyword_tokens_names = {
	{TOKEN_RETURN, "return"},
	{TOKEN_EXTERN, "extern"},
	{TOKEN_BUILTIN, "builtin"},
	{TOKEN_AS, "as"},
	{TOKEN_TRUE, "true"},
	{TOKEN_FALSE, "false"},
	{TOKEN_NIL, "nil"},
	{TOKEN_IF, "if"},
	{TOKEN_ELSE, "else"},
	{TOKEN_WHILE, "while"},
	{TOKEN_NEW, "new"},
	{TOKEN_DELETE, "delete"},
	{TOKEN_ENUM, "enum"},
	{TOKEN_STRUCT, "struct"},
	{TOKEN_USE, "use"},
	{TOKEN_QWR, "qwr"},
};

const std::unordered_map<TokenType, std::string> two_char_tokens_names = {
	{TOKEN_COLON_COLON, "'::'"},
	{TOKEN_DOT_DOT, "'..'"},
	{TOKEN_ADD_EQ, "'+='"},
	{TOKEN_SUB_EQ, "'-='"},
	{TOKEN_MUL_EQ, "'*='"},
	{TOKEN_DIV_EQ, "'/='"},
	{TOKEN_MOD_EQ, "'%='"},
	{TOKEN_EQ_EQ, "'=='"},
	{TOKEN_NOT_EQ, "'!='"},
	{TOKEN_LT_EQ, "'<='"},
	{TOKEN_GT_EQ, "'>='"},
	{TOKEN_AND_AND, "'&&'"},
	{TOKEN_BAR_BAR, "'||'"},
	{TOKEN_PLUS_PLUS, "'++'"},
	{TOKEN_MINUS_MINUS, "'--'"},
};

void Token::print() {
	switch (type) {
		case TOKEN_EOF: {
			std::cout << "EOF\n";
		} break;
		case TOKEN_ATOM: {
			std::cout << "Atom '"
					  << lexeme
					  << "'\n";
		} break;
		case TOKEN_INT_LIT: {
			std::cout << "Int " << int_value << "\n";
		} break;
		default: {
			auto it = keyword_tokens_names.find(type);
			if (it != keyword_tokens_names.end()) {
				std::cout << "Keyword '" << it->second << "'\n";
			} else {
				it = two_char_tokens_names.find(type);
				if (it != two_char_tokens_names.end()) {
					std::cout << "Operator " << it->second << "\n";
				} else {
					std::cout << "Operator '" << (char)type << "'\n";
				}
			}
		} break;
	}
}
