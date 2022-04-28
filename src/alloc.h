#ifndef ALLOC_H_
#define ALLOC_H_

struct Stmt;
struct Expr;
struct Variable;

void arena_init();

Token *create_token();
Stmt *create_stmt();
Expr *create_expr();
Variable *create_variable();

#endif