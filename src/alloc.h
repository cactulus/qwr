#ifndef ALLOC_H_
#define ALLOC_H_

struct Token;
struct Stmt;
struct Expr;
struct Variable;
struct QType;

void arena_init();
void *arena_alloc(size_t size);

Token *create_token();
Stmt *create_stmt();
Expr *create_expr();
Variable *create_variable();
QType *create_type();

#endif