#ifndef AST_H_
#define AST_H_

#include <vector>

#include "lexer.h"

typedef uint8_t u8;

namespace llvm {
	class Value;
	class Function;
};

enum ExprKind {
    BINARY,
	VARIABLE,
	INT_LIT,
	FLOAT_LIT,
    STRING_LIT,
	COMPOUND_LIT,
    FUNCTION_CALL,
	BUILTIN_FUNCTION,
    CAST,
    UNARY,
    DEREF,
    COMPARE_ZERO,
    NIL,
    NEW,
    MEMBER,
	INDEXED,
	SIZEOF,
};

struct Expr {
    QType *type;
    SourceLocation location;

    Expr(QType *_type, SourceLocation _location) {
        type = _type;
        location = _location;
    }

    Expr(QType *_type, Token *_location) {
        type = _type;
        location = _location->location;
    }

    virtual ExprKind kind() = 0;
};

struct Binary : Expr {
    using Expr::Expr;

    Expr *lhs;
    Expr *rhs;
    char op;

    virtual ExprKind kind() override {
        return BINARY;
    }
};

struct Member;
struct Variable : Expr {
    using Expr::Expr;

	const char *name;
	llvm::Value *llvm_ref;
	u8 flags = 0;

    unsigned int proxy_index;
    Member *proxy_member;

    virtual ExprKind kind() override {
        return VARIABLE;
    }
};

struct FunctionDefinition;
struct FunctionCall : Expr {
    using Expr::Expr;

	union {
		FunctionDefinition *target_func_decl;
		Variable *function_pointer;
	};
    std::vector<Expr *> arguments;
	bool from_function_pointer;

    virtual ExprKind kind() override {
        return FUNCTION_CALL;
    }
};

struct IntegerLiteral : Expr {
    using Expr::Expr;

    long int int_value;

    virtual ExprKind kind() override {
        return INT_LIT;
    }
};

struct FloatLiteral : Expr {
    using Expr::Expr;

    double float_value;

    virtual ExprKind kind() override {
        return FLOAT_LIT;
    }
};

struct QStringLiteral : Expr {
    using Expr::Expr;

    const char *string_lit;

    virtual ExprKind kind() override {
        return STRING_LIT;
    }
};

struct New : Expr {
    using Expr::Expr;

	QType *alloc_type;

    virtual ExprKind kind() override {
        return NEW;
    }
};

struct Cast : Expr {
    using Expr::Expr;

    Expr *target;
    QType *from;
    QType *to;

    virtual ExprKind kind() override {
        return CAST;
    }
};

struct Unary : Expr {
    using Expr::Expr;

	union {
		Expr *target;
		FunctionDefinition *target_fn; /* for function pointer referencing*/
	};
    unsigned char op;
    bool ispost;

    virtual ExprKind kind() override {
        return UNARY;
    }
};

struct Member : Expr {
    using Expr::Expr;

    Expr *target;
    std::vector<int> indices;
    std::vector<bool> dereferences;

    virtual ExprKind kind() override {
        return MEMBER;
    }
};

struct CompoundLiteral : Expr {
    using Expr::Expr;

    std::vector<Expr *> values;
    bool lit_is_constant;

    virtual ExprKind kind() override {
        return COMPOUND_LIT;
    }
};

struct Indexed : Expr {
    using Expr::Expr;

    Expr *target;
    Expr *index;

    virtual ExprKind kind() override {
        return INDEXED;
    }
};

struct Builtin : Expr {
    using Expr::Expr;

    const char *name;
    std::vector<Expr *> arguments;

    virtual ExprKind kind() override {
        return BUILTIN_FUNCTION;
    }
};

struct Deref : Expr {
    using Expr::Expr;

    Expr *target;

    virtual ExprKind kind() override {
        return DEREF;
    }
};

struct CompareZero : Expr {
    using Expr::Expr;

    Expr *target;

    virtual ExprKind kind() override {
        return COMPARE_ZERO;
    }
};

struct Nil : Expr {
    using Expr::Expr;

    virtual ExprKind kind() override {
        return NIL;
    }
};

struct SizeOf : Expr {
    using Expr::Expr;

    QType *target_type;

    virtual ExprKind kind() override {
        return SIZEOF;
    }
};

const u8 VAR_CONST = 0x1;
const u8 VAR_GLOBAL = 0x2;
const u8 VAR_MULTIPLE = 0x4;

const u8 VAR_PROXY_ENUM = 0x2;
const u8 VAR_PROXY_STRUCT = 0x4;

const u8 FUNCTION_EXTERN = 0x1;
const u8 FUNCTION_BUILTIN = 0x2;
const u8 FUNCTION_VARARG = 0x4;

enum StmtKind {
	FUNCTION_DEFINITION,
	VARIABLE_DEFINITION,
	RETURN,
    IF,
    WHILE,
	FOR,
    COMPOUND,
    EXPR_STMT,
    DELETE,
};

struct Stmt {
    SourceLocation location;

    Stmt(Token *_location) {
        location = _location->location;
    }

    virtual StmtKind kind() = 0;
};

struct VariableDefinition : Stmt {
    using Stmt::Stmt;

    Variable *var;
    std::vector<Variable *> vars;
    Expr *value;
    u8 flags;
    
    virtual StmtKind kind() override {
        return VARIABLE_DEFINITION;
    }
};

struct FunctionDefinition : Stmt{
    using Stmt::Stmt;

	QType *type;
    llvm::Function *llvm_ref;
    const char *mangled_name;
    const char *unmangled_name;
    std::vector<Stmt *> body;
    u8 flags;
    
    virtual StmtKind kind() override {
        return FUNCTION_DEFINITION;
    }
};

struct If : Stmt {
    using Stmt::Stmt;

	Expr *cond;
    Stmt *then;
    Stmt *otherwise;
    
    virtual StmtKind kind() override {
        return IF;
    }
};

struct While : Stmt {
    using Stmt::Stmt;

    Expr *cond;
    Stmt *body;
    
    virtual StmtKind kind() override {
        return WHILE;
    }
};

struct For : Stmt {
    using Stmt::Stmt;

    Variable *var;
    Stmt *body;

    Expr *iterator;
    Expr *range_from;
    Expr *range_to;

    bool is_range;
    
    virtual StmtKind kind() override {
        return FOR;
    }
};

struct ExprStmt : Stmt {
    using Stmt::Stmt;

    Expr *target_expr;
    
    virtual StmtKind kind() override {
        return EXPR_STMT;
    }
};

struct Return : Stmt {
    using Stmt::Stmt;

    std::vector<Expr *> return_values;
    
    virtual StmtKind kind() override {
        return RETURN;
    }
};

struct Delete : Stmt {
    using Stmt::Stmt;

    Expr *target_expr;
    
    virtual StmtKind kind() override {
        return DELETE;
    }
};

struct CompoundStmt : Stmt {
    using Stmt::Stmt;

    std::vector<Stmt *> stmts;
    
    virtual StmtKind kind() override {
        return COMPOUND;
    }
};

#endif
