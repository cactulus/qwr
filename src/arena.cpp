#include <cassert>
#include <iostream>

#include "lexer.h"
#include "parser.h"
#include "arena.h"

static std::vector<Token> token_arena;
static size_t token_arena_index = 0;

static std::vector<Stmt> stmt_arena;
static size_t stmt_arena_index = 0;

static std::vector<Expr> expr_arena;
static size_t expr_arena_index = 0;

static std::vector<Variable> variable_arena;
static size_t variable_arena_index = 0;

void arena_init() {
	token_arena.resize(1024 * 256);
	token_arena_index = 0;

	stmt_arena.resize(1024 * 128);
	stmt_arena_index = 0;

	expr_arena.resize(1024 * 128);
	expr_arena_index = 0;

	variable_arena.resize(1024 * 128);
	variable_arena_index = 0;
}

Token *create_token() {
	assert(token_arena_index < token_arena.size());
	return &token_arena[token_arena_index++];
}

Stmt *create_stmt() {
	assert(stmt_arena_index < stmt_arena.size());
	return &stmt_arena[stmt_arena_index++];
}

Expr *create_expr() {
	assert(expr_arena_index < expr_arena.size());
	return &expr_arena[expr_arena_index++];
}

Variable *create_variable() {
	assert(variable_arena_index < variable_arena.size());
	return &variable_arena[variable_arena_index++];
}