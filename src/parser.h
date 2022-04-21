#ifndef PARSER_H_
#define PARSER_H_

#include <string>
#include <vector>

#include "lexer.h"
#include "messenger.h"
#include "typer.h"

typedef uint8_t u8;

namespace llvm {
	class Value;
	class Function;
};

struct Variable {
	const char *name;
	QType *type;
	llvm::Value *llvm_ref;
	bool is_const;
};

struct Scope {
	Messenger *messenger;
	Scope *parent;
	std::unordered_map<std::string, Variable *> variables;

	Scope(Messenger *_messenger, Scope *_parent=NULL);

	void add(Token *token, Variable *var);
	Variable *find(Token *token);
	Variable *find_null(const char *name);
};

enum ExprKind {
	BINARY,
	VARIABLE,
	INT_LIT,
    STRING_LIT,
    FUNCTION_CALL,
    CAST,
    UNARY,
    DEREF,
    COMPARE_ZERO,
    NIL,
    NEW,
    MEMBER,
};

struct Expr {
	ExprKind kind;
	QType *type;

	union {
		Expr *target;
		QType *alloc_type;
		Variable *var;
		long int_value;	
        const char *string_lit;

		struct {
			Expr *lhs;
			Expr *rhs;
			char op;
		} bin;

        struct {
            Stmt *target_func_decl;
            std::vector<Expr *> *arguments;
        } func_call;

        struct {
            Expr *target;
            QType *from;
            QType *to;
        } cast;

        struct {
            Expr *target;
            unsigned char op;
        } unary;

        struct {
            Expr *target;
            std::vector<int> *indices;
            std::vector<bool> *dereferences;
        } member;
	};
};

enum StmtKind {
	EXTERN_FUNCTION,
	FUNCTION_DEFINITION,
	VARIABLE_DEFINITION,
	RETURN,
    IF,
    WHILE,
    BLOCK,
    EXPR_STMT,
    DELETE,
};

const u8 VAR_CONST = 0x1;
const u8 VAR_GLOBAL = 0x2;
const u8 VAR_MULTIPLE = 0x4;

struct Stmt {
	StmtKind kind;

	union {
        std::vector<Stmt *> *stmts;
        std::vector<Expr *> *return_values;
        Expr *target_expr;

		struct {
			union {
			    Variable *var;
                std::vector<Variable *> *vars;
            };
			Expr *value;
			u8 flags;
		} var_def;

		struct {
            llvm::Function *llvm_ref;
			const char *mangled_name;
            const char *unmangled_name;
            std::vector<QType *> *return_types;
			std::vector<Variable *> *parameters;
			std::vector<Stmt *> *body;
			bool isvararg;
		} func_def;

		struct {
		    Expr *cond;
		    Stmt *then;
		    Stmt *otherwise;
        } if_;

        struct {
            Expr *cond;
            Stmt *body;
        } while_;
	};
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

    std::unordered_map<std::string, Stmt *> functions;

	void init(Typer *_typer, Messenger *_messenger);

	Stmt *parse_top_level_stmt();
	Stmt *parse_stmt();				

    void parse_enum(Token *name);
    void parse_struct(Token *name);
	Stmt *parse_func_def(Token *name);
	Stmt *parse_extern_func_def(Token *name);

	Stmt *parse_variable_definition(Token *name_token, u8 flags);
	Stmt *parse_variable_definition_type(Token *name_token, u8 flags);
    QType *parse_variable_definition_base(Token *name_token, u8 flags, Stmt *stmt);

    Stmt *parse_multiple_variable_definition(Token *name_token);

    Stmt *parse_block();
    Stmt *parse_if();
    Stmt *parse_while();

	Stmt *parse_return();
	Stmt *parse_delete();
	Stmt *try_parse_atom();

	void parse_function_parameters(Stmt *stmt, bool add_to_scope);

	Expr *parse_expr(int prec=1);
	Expr *parse_binary(int prec);
	Expr *parse_access();
	Expr *parse_unary();
	Expr *parse_postfix();
	Expr *parse_primary();

    Expr *cast(Expr *target, QType *to);
    Expr *make_compare_zero(Expr *expr);
	QType *parse_type();

    bool expr_is_targatable(Expr *expr);

	bool token_is_op(char op, int off=0);
	bool eat_token_if(TokenType type);
	bool eat_token_if(char type);
	void eat_semicolon();

	void scope_push();
	void scope_pop();

    void insert_func(Token *name_token, const char *mangled_name, Stmt *func_decl);
    Stmt *get_func(Token *name_token, std::vector<Expr *> *arguments);

	const char *mangle_func(Stmt *stmt);
	std::string mangle_type(QType *type);

	Stmt *make_stmt(StmtKind kind);
	Expr *make_expr(ExprKind kind, QType *type);
	Variable *make_variable(const char *name, QType *type);
	Variable *add_or_get_variable(Token *token, QType *type);
};

#endif
