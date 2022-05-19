#ifndef PARSER_H_
#define PARSER_H_

#include <string>
#include <vector>

#include "lexer.h"
#include "messenger.h"
#include "typer.h"
#include "ast.h"

typedef uint8_t u8;

struct Scope {
	Messenger *messenger;
	Scope *parent;
	std::unordered_map<std::string, Variable *> variables;

	Scope(Messenger *_messenger, Scope *_parent=NULL);

	void add(Token *token, Variable *var);
	void add_proxy(Token *token, Variable *var);
	void add_replace(Variable *var);
	Variable *find(Token *token);
	Variable *find_null(const char *name);
};

struct ParameterInfo {
    std::vector<Variable *> *parameters;
    bool isvararg;
};

struct Parser {
	Messenger *messenger;
	Typer *typer;
	Lexer lexer;
	Scope *scope;
	bool has_reached_end;

    std::unordered_map<std::string, FunctionDefinition *> functions;

	void init(Typer *_typer, Messenger *_messenger);

	Stmt *parse_top_level_stmt();
	Stmt *parse_stmt();				

    Stmt *parse_preproc(Token *op_token, bool top_level);
    void parse_enum(Token *name);
    void parse_struct(Token *name);
	void parse_using();
	Stmt *parse_func_def(Token *name, u8 flags);
	Stmt *parse_extern_func_def(Token *name);

	Stmt *parse_variable_definition(Token *name_token, u8 flags);
	Stmt *parse_variable_definition_type(Token *name_token, u8 flags);
    QType *parse_variable_definition_base(Token *name_token, u8 flags, VariableDefinition *stmt);

    Stmt *parse_multiple_variable_definition(Token *name_token);

    Stmt *parse_block();
    Stmt *parse_if();
    Stmt *parse_while();
	Stmt *parse_for();

	Stmt *parse_return();
	Stmt *parse_delete();
	Stmt *try_parse_atom();

	void parse_function_parameters(FunctionDefinition *stmt, bool add_to_scope);

	Expr *parse_expr(int prec=1);
	Expr *parse_binary(int prec);
	Expr *parse_cast();
	Expr *parse_postfix();
	Expr *parse_access();
	Expr *parse_unary();
	Expr *parse_primary();
    FunctionCall *parse_function_call();

    Expr *cast(Expr *target, QType *to);

    Expr *make_compare_zero(Expr *expr);
    Expr *make_compare_strings(Expr *lhs, Expr *rhs, TokenType op);

    bool expr_is_targatable(Expr *expr);
    
	QType *parse_type();

	bool token_is_op(char op, int off=0);
	bool eat_token_if(TokenType type);
	bool eat_token_if(char type);
	void eat_semicolon();

	void scope_push();
	void scope_pop();

    void insert_func(Token *name_token, const char *mangled_name, FunctionDefinition *func_decl, bool is_extern=false);
    FunctionDefinition *get_func(Token *name_token, std::vector<Expr *> *arguments);
    FunctionDefinition *get_func(const char *name);

	const char *mangle_func(FunctionDefinition *stmt);

	Variable *add_or_get_variable(Token *token, QType *type);

	void check_builtin_func(Token *token, Builtin *expr);
};

#endif
