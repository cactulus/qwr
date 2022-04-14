#ifndef GEN_H_
#define GEN_H_

#include <llvm/IR/Module.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "parser.h"

struct Manager;

struct CodeGenerator {
	Typer *typer;
    llvm::TargetMachine *target_machine;
	llvm::Module *llvm_module;
	llvm::LLVMContext llvm_context;
	llvm::IRBuilder<> *builder;

	void init(Typer *_typer);

	void gen_stmt(Stmt *stmt);
	void gen_func_def(Stmt *stmt);
	void gen_extern_func_def(Stmt *stmt);
	void gen_var_def(Stmt *stmt);
	void gen_return(Stmt *stmt);
	void gen_expr_stmt(Stmt *stmt);

	llvm::Value *gen_expr(Expr *expr);
	llvm::Value *gen_assign(Expr *expr);
	llvm::Value *gen_binary(Expr *expr);
	llvm::Value *gen_cast(Expr *expr);
	llvm::Value *gen_unary(Expr *expr);
	llvm::Value *gen_deref(Expr *expr);
	llvm::Value *gen_variable(Expr *expr);
	llvm::Value *gen_int_lit(Expr *expr);
	llvm::Value *gen_string_lit(Expr *expr);
	llvm::Value *gen_func_call(Expr *expr);

	llvm::Value *gen_expr_target(Expr *expr);

    void init_module();
	void output(char *obj_file, u8 flags);
	void link(char *obj_file, char *exe_file);
	void optimize();
	void dump();
};

#endif
