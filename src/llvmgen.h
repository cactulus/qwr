#ifndef GEN_H_
#define GEN_H_

#include <llvm/Analysis/CallGraph.h>
#include <llvm/IR/Module.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Type.h>

#include "options.h"
#include "parser.h"

struct CodeGenerator {
	Typer *typer;
    llvm::TargetMachine *target_machine;
	llvm::Module *llvm_module;
	llvm::LLVMContext llvm_context;
	llvm::IRBuilder<> *builder;
	
	llvm::DIBuilder *dbg_builder;
	llvm::DICompileUnit *dbg_cu;
	llvm::DIFile *dbg_file;
	std::vector<llvm::DIScope *> dbg_scopes;
	bool debug;

	llvm::CallGraph *callgraph;
	bool gen_callgraph;

	void init(Typer *_typer, Options *_options);
	void init_debug(const char *src_file);

	void gen_stmt(Stmt *stmt);
	void gen_func_def(Stmt *stmt);
	void gen_var_def(Stmt *stmt);
	void gen_return(Stmt *stmt);
	void gen_if(Stmt *stmt);
	void gen_while(Stmt *stmt);
	void gen_for(Stmt *stmt);
	void gen_block(Stmt *stmt);
	void gen_expr_stmt(Stmt *stmt);
	void gen_delete(Stmt *stmt);

	llvm::Value *gen_expr(Expr *expr);
	llvm::Value *gen_binary(Expr *expr);
	llvm::Value *gen_cast(Expr *expr);
	llvm::Value *gen_unary(Expr *expr);
	llvm::Value *gen_deref(Expr *expr);
	llvm::Value *gen_variable(Expr *expr);
	llvm::Value *gen_int_lit(Expr *expr);
	llvm::Value *gen_float_lit(Expr *expr);
	llvm::Value *gen_string_lit(Expr *expr);
	llvm::Value *gen_func_call(Expr *expr);
	llvm::Value *gen_compare_zero(Expr *expr);
	llvm::Value *gen_nil(Expr *expr);
	llvm::Value *gen_new(Expr *expr);
	llvm::Value *gen_member(Expr *expr);
	llvm::Value *gen_indexed(Expr *expr);

	llvm::Value *gen_expr_target(Expr *expr);

	llvm::Value *gen_array_indexed(llvm::Value *arr, llvm::Type *arr_type, llvm::Value *index);
	llvm::Value *gen_string_indexed(llvm::Value *str, llvm::Value *index);
		
    llvm::Type *gen_return_type(std::vector<QType *> *types);
    llvm::Type *gen_return_type(std::vector<Expr *> *types);

	llvm::Value *gen_builtin(Expr *expr);
	llvm::Function *get_builtin(const char *name);
	llvm::Function *gen_append_func(std::string name, QType *value_type);

	void emit_location_dbg(Stmt *stmt);
	void emit_location_dbg(Expr *expr);

	llvm::DIType *convert_type_dbg(QType *type);
    int llvm_size_of(llvm::Type *type);

    void init_module();
	void init_types();
	void output(Options *options);
	void link(Options *options);
	void optimize();
	void dump(Options *options);
	void output_call_graph(Options *options);
};

#endif
