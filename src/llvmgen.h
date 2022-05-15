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

    void gen(Stmt *stmt);
	void gen_func_def(FunctionDefinition *stmt);
	void gen_var_def(VariableDefinition *stmt);
	void gen_return(Return *stmt);
	void gen_if(If *stmt);
	void gen_while(While *stmt);
	void gen_for(For *stmt);
	void gen_compound(CompoundStmt *stmt);
	void gen_expr_stmt(ExprStmt *stmt);
	void gen_delete(Delete *stmt);

    llvm::Value *gen(Expr *stmt);
	llvm::Value *gen_binary(Binary *expr);
	llvm::Value *gen_unary(Unary *expr);
	llvm::Value *gen_deref(Deref *expr);
	llvm::Value *gen_variable(Variable *expr);
	llvm::Value *gen_int_lit(IntegerLiteral *expr);
	llvm::Value *gen_float_lit(FloatLiteral *expr);
	llvm::Value *gen_string_lit(QStringLiteral *expr);
	llvm::Value *gen_func_call(FunctionCall *expr);
	llvm::Value *gen_compare_zero(CompareZero *expr);
	llvm::Value *gen_nil(Nil *expr);
	llvm::Value *gen_cast(Cast *expr);
	llvm::Value *gen_new(New *expr);
	llvm::Value *gen_member(Member *expr);
    llvm::Value *gen_indexed(Indexed *expr);
	llvm::Value *gen_builtin(Builtin *expr);

	llvm::Value *gen_expr_target(Expr *expr);

	llvm::Value *gen_array_indexed(llvm::Value *arr, llvm::Type *arr_type, llvm::Value *index);
	llvm::Value *gen_string_indexed(llvm::Value *str, llvm::Value *index);
		
    llvm::Type *gen_return_type(std::vector<QType *> types);
    llvm::Type *gen_return_type(std::vector<Expr *> types);

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
