#include <cassert>
#include <cstdint>
#include <iostream>

#include "lexer.h"
#include "parser.h"
#include "alloc.h"

#define ARENA_ALLOC
#ifdef ARENA_ALLOC

static void *arena_block = 0;
static size_t arena_pos = 0;
static size_t arena_size = 256 * 4096;

void arena_init() {
    arena_block = malloc(arena_size);
    arena_pos = 0;
}

void *arena_alloc(size_t size) {
    if (arena_pos + size > arena_size) {
        std::cout << "Out of memory!\n";
        std::exit(-1);
    }

	auto p = &(((char *) arena_block)[arena_pos]);
    arena_pos += size;
    return p; 
}

Token *create_token() {
	return (Token *) arena_alloc(sizeof(Token));
}

Stmt *create_stmt() {
	return (Stmt *) arena_alloc(sizeof(Stmt));
}

Expr *create_expr() {
	return (Expr *) arena_alloc(sizeof(Expr));
}

Variable *create_variable() {
	return (Variable *) arena_alloc(sizeof(Variable));
}

QType *create_type() {
	return (QType *) arena_alloc(sizeof(QType));
}

#else
Token *create_token() {
	return new Token();
}

Stmt *create_stmt() {
	return new Stmt();
}

Expr *create_expr() {
	return new Expr();
}

Variable *create_variable() {
	return new Variable();
}

QType *create_type() {
	return new QType();
}
#endif
