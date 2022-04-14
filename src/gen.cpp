#include <iostream>
#include <unordered_map>
#include <string>
#include <sstream>

#include <llvm/CodeGen/CommandFlags.h>
#include <llvm/MC/SubtargetFeature.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/ToolOutputFile.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

#include "gen.h"
#include "manager.h"

using namespace llvm;

static llvm::codegen::RegisterCodeGenFlags CGF;

typedef void (CodeGenerator::*gen_stmt_fun) (Stmt *stmt);
typedef Value *(CodeGenerator::*gen_expr_fun) (Expr *expr);

const std::unordered_map<StmtKind, gen_stmt_fun> stmt_gen_funs = {
	{FUNCTION_DEFINITION, (gen_stmt_fun) &CodeGenerator::gen_func_def},
	{EXTERN_FUNCTION, (gen_stmt_fun) &CodeGenerator::gen_extern_func_def},
	{VARIABLE_DEFINITION, (gen_stmt_fun) &CodeGenerator::gen_var_def},
	{RETURN, (gen_stmt_fun) &CodeGenerator::gen_return},
	{EXPR_STMT, (gen_stmt_fun) &CodeGenerator::gen_expr_stmt},
};

const std::unordered_map<ExprKind, gen_expr_fun> expr_gen_funs = {
	{BINARY, (gen_expr_fun) &CodeGenerator::gen_binary},
	{ASSIGN, (gen_expr_fun) &CodeGenerator::gen_assign},
	{CAST, (gen_expr_fun) &CodeGenerator::gen_cast},
	{UNARY, (gen_expr_fun) &CodeGenerator::gen_unary},
	{DEREF, (gen_expr_fun) &CodeGenerator::gen_deref},
	{VARIABLE, (gen_expr_fun) &CodeGenerator::gen_variable},
	{INT_LIT, (gen_expr_fun) &CodeGenerator::gen_int_lit},
	{STRING_LIT, (gen_expr_fun) &CodeGenerator::gen_string_lit},
	{FUNCTION_CALL, (gen_expr_fun) &CodeGenerator::gen_func_call},
};

std::unordered_map<std::string, Value *> constant_string_literals = {};

void CodeGenerator::init(Typer *_typer) {
	typer = _typer;

	llvm_module = new Module("test.ll", llvm_context);
	builder = new IRBuilder<>(llvm_context);

    init_module();
}

void CodeGenerator::gen_stmt(Stmt *stmt) {
	auto it = stmt_gen_funs.find(stmt->kind);
	gen_stmt_fun fn = it->second;
	(*this.*fn)(stmt);
}

void CodeGenerator::gen_func_def(Stmt *stmt) {
	std::vector<Type *> parameter_types(stmt->func_def.parameters->size());
	for (int i = 0; i < stmt->func_def.parameters->size(); i++) {
		parameter_types[i] = (*stmt->func_def.parameters)[i]->type->llvm_type;
	}
	auto fun_type = FunctionType::get(stmt->func_def.return_type->llvm_type, parameter_types, stmt->func_def.isvararg);
	auto linkage = Function::ExternalLinkage;

 	auto fn = Function::Create(fun_type, linkage,linkage, stmt->func_def.mangled_name, llvm_module);

    stmt->func_def.llvm_ref = fn;

	auto entry = BasicBlock::Create(llvm_context, "", fn);
	builder->SetInsertPoint(entry);

	int i = 0;
	for (auto arg_it = fn->arg_begin(); arg_it != fn->arg_end(); arg_it++) {
		auto par = (*stmt->func_def.parameters)[i];
		auto var = builder->CreateAlloca(par->type->llvm_type);
		par->llvm_ref = var;
		builder->CreateStore(&*arg_it, var);
		i++;
	}

	for (auto s : *stmt->func_def.body) {
		gen_stmt(s);
	}

    if (builder->GetInsertBlock()->getTerminator() == 0) {
        builder->CreateRetVoid();
    }
}

void CodeGenerator::gen_extern_func_def(Stmt *stmt) {
	std::vector<Type *> parameter_types(stmt->func_def.parameters->size());
	for (int i = 0; i < stmt->func_def.parameters->size(); i++) {
		parameter_types[i] = (*stmt->func_def.parameters)[i]->type->llvm_type;
	}
	auto fun_type = FunctionType::get(stmt->func_def.return_type->llvm_type, parameter_types, stmt->func_def.isvararg);
	auto linkage = Function::ExternalLinkage;

 	auto fn = Function::Create(fun_type, linkage,linkage, stmt->func_def.unmangled_name, llvm_module);
    stmt->func_def.llvm_ref = fn;
}

void CodeGenerator::gen_var_def(Stmt *stmt) {
	if (stmt->var_def.flags & VAR_GLOBAL) {
		auto var_name = stmt->var_def.var->name;

		llvm_module->getOrInsertGlobal(var_name, stmt->var_def.var->type->llvm_type);
		auto var = llvm_module->getGlobalVariable(var_name);

		var->setConstant(stmt->var_def.flags & VAR_CONST);
		
		auto init = dyn_cast<Constant>(gen_expr(stmt->var_def.value));
		var->setInitializer(init);

		stmt->var_def.var->llvm_ref = var;
	} else {
		auto var_ptr = builder->CreateAlloca(stmt->var_def.var->type->llvm_type);
		auto val = gen_expr(stmt->var_def.value);

		builder->CreateStore(val, var_ptr);

		stmt->var_def.var->llvm_ref = var_ptr;
	}
}

void CodeGenerator::gen_return(Stmt *stmt) {
    if (stmt->return_value) {
	    builder->CreateRet(gen_expr(stmt->return_value));
    } else {
        builder->CreateRetVoid();
    }
}

void CodeGenerator::gen_expr_stmt(Stmt *stmt) {
    gen_expr(stmt->target_expr);
}

Value *CodeGenerator::gen_expr(Expr *expr) {
	auto it = expr_gen_funs.find(expr->kind);
	gen_expr_fun fn = it->second;
	return (*this.*fn)(expr);
}

Value *CodeGenerator::gen_binary(Expr *expr) {
	auto lhs = gen_expr(expr->bin.lhs);
	auto rhs = gen_expr(expr->bin.rhs);
	Instruction::BinaryOps op;

	switch (expr->bin.op) {
		case '+': op = Instruction::BinaryOps::Add; break;
		case '-': op = Instruction::BinaryOps::Sub; break;
		case '*': op = Instruction::BinaryOps::Mul; break;
		case '/': op = Instruction::BinaryOps::SDiv; break;
		case '%': op = Instruction::BinaryOps::SRem; break;
	}

	return builder->CreateBinOp(op, lhs, rhs);
}

Value *CodeGenerator::gen_assign(Expr *expr) {
	auto target = gen_expr_target(expr->assign.target);
	auto value = gen_expr(expr->assign.value);

	Value *new_value;

	if (expr->assign.op == '=') {
		new_value = value;
	} else {
		auto target_value = gen_expr(expr->assign.target);
		Instruction::BinaryOps op;

		switch (expr->assign.op) {
			case TOKEN_ADD_EQ: op = Instruction::BinaryOps::Add; break;
			case TOKEN_SUB_EQ: op = Instruction::BinaryOps::Sub; break;
			case TOKEN_MUL_EQ: op = Instruction::BinaryOps::Mul; break;
			case TOKEN_DIV_EQ: op = Instruction::BinaryOps::SDiv; break;
			case TOKEN_MOD_EQ: op = Instruction::BinaryOps::SRem; break;
		}

		new_value = builder->CreateBinOp(op, target_value, value);
	}

	builder->CreateStore(new_value, target);
	return value;
}

Value *CodeGenerator::gen_cast(Expr *expr) {
    QBaseType bf = expr->cast.from->base;
    QBaseType bt = expr->cast.to->base;
    auto target = gen_expr(expr->cast.target);
    auto to = expr->cast.to->llvm_type;

    if (bf == TYPE_POINTER && bt == TYPE_POINTER) {
        return builder->CreatePointerCast(target, to);
    }

    if (bf == TYPE_BOOL && expr->cast.to->isint()) {
        return builder->CreateZExt(target, to);
    }

    if (expr->cast.from->isint() && bt == TYPE_BOOL) {
        return builder->CreateIntCast(target, to, expr->cast.from->isuint());
    }
    
    if (expr->cast.from->isuint() && bt > bf) {
        return builder->CreateZExt(target, to);
    }
    return builder->CreateIntCast(target, to, expr->cast.from->isuint());
}

Value *CodeGenerator::gen_unary(Expr *expr) {
    switch (expr->unary.op) {
        case '&': {
            return expr->unary.target->var->llvm_ref;
        } break;
        case '!': {
            auto target = gen_expr(expr->unary.target);
            return builder->CreateNot(target);
        } break;
        case '-': {
            auto target = gen_expr(expr->unary.target);
            return builder->CreateNeg(target);
        } break;
    }

    return 0;
}

Value *CodeGenerator::gen_deref(Expr *expr) {
    auto target = gen_expr(expr->deref_target);
    return builder->CreateLoad(expr->type->llvm_type, target);
}

Value *CodeGenerator::gen_variable(Expr *expr) {
	return builder->CreateLoad(expr->var->type->llvm_type, expr->var->llvm_ref);
}

Value *CodeGenerator::gen_int_lit(Expr *expr) {
	return ConstantInt::get(expr->type->llvm_type, expr->int_value);
}

Value *CodeGenerator::gen_string_lit(Expr *expr) {
    auto it = constant_string_literals.find(expr->string_lit);

    if (it != constant_string_literals.end()) {
        return it->second;
    } 

    auto ptr = builder->CreateGlobalStringPtr(expr->string_lit);
    constant_string_literals.insert(std::make_pair(expr->string_lit, ptr));
    return ptr;
}

Value *CodeGenerator::gen_func_call(Expr *expr) {
    std::vector<Value *> arg_values{};
    auto target_fn = expr->func_call.target_func_decl->func_def.llvm_ref;

    for (auto arg : *expr->func_call.arguments)
        arg_values.push_back(gen_expr(arg));

    return builder->CreateCall(target_fn, arg_values);
}

llvm::Value *CodeGenerator::gen_expr_target(Expr *expr) {
    if (expr->kind == VARIABLE) {
        return expr->var->llvm_ref;
    }

    if (expr->kind == DEREF) {
        auto target = gen_expr_target(expr->deref_target); 
        auto type = expr->deref_target->type->llvm_type;

        return builder->CreateLoad(type, target);
    }

    /* unreachable */
    return 0;
}

void CodeGenerator::init_module() {
	InitializeAllTargetInfos();
	InitializeAllTargets();
	InitializeAllTargetMCs();
	InitializeAllAsmParsers();
	InitializeAllAsmPrinters();

    Triple triple(sys::getDefaultTargetTriple());
	triple.setArch(Triple::x86_64);

    llvm_module->setTargetTriple(triple.getTriple());

    std::string error;
    const auto target = TargetRegistry::lookupTarget(triple.getTriple(), error);

    if (!target) {
        std::cerr << "Error, could not find target: " << error << "\n";
        return;
    }

    std::string mcpu = codegen::getMCPU();
    if (mcpu == "native") {
	    mcpu = sys::getHostCPUName().str();
    }

    std::string features_str;
    std::vector<std::string> mattrs = codegen::getMAttrs();
    if (mattrs.size()) {
        SubtargetFeatures features;
        for (unsigned int i = 0; i != mattrs.size(); ++i) {
            features.AddFeature(mattrs[i]);
        }
        features_str = features.getString();
    }

    TargetOptions options;
    target_machine = target->createTargetMachine(triple.getTriple(), mcpu, features_str, options, llvm::Reloc::PIC_);

    if (!target_machine) {
        std::cerr << "Error: Could not create target machine.\n";
        return;
    }

    const auto dl = target_machine->createDataLayout();
    llvm_module->setDataLayout(dl);
}

void CodeGenerator::output(char *obj_file, u8 flags) {
    if (flags & OPTIMIZE) {
        optimize();
    }

    legacy::PassManager pm;

    std::error_code std_error;
    auto out = new ToolOutputFile(obj_file, std_error, sys::fs::OF_None);
    if (!out) {
        std::cerr << "Could not open file " << obj_file << "\n";
        return;
    }

    raw_pwrite_stream *os = &out->os();

    if (target_machine->addPassesToEmitFile(pm, *os, nullptr, CodeGenFileType::CGFT_ObjectFile, false)) {
        std::cerr << obj_file << ": target does not support generation of this file type!\n";
        return;
    }

    pm.run(*llvm_module);
    out->keep();
}

void CodeGenerator::link(char *obj_file, char *exe_file) {
    std::stringstream cmd;

#ifdef _WIN32
	cmd << "link.exe /out:";
	cmd << exe_file << " ";
	cmd << obj_file << " msvcrt.lib";
#else
	cmd << "gcc -o ";
	cmd << exe_file << " ";
    cmd << obj_file;
#endif
    
    std::system("gcc -o examples/test examples/test.o");
    std::remove(obj_file);
}

void CodeGenerator::optimize() {
    legacy::PassManager *pm = new legacy::PassManager();
    PassManagerBuilder pmb;
    pmb.OptLevel = 3;
    pmb.DisableUnrollLoops = false;
    pmb.LoopVectorize = true;
    pmb.SLPVectorize = true;
    pmb.populateModulePassManager(*pm);
    pm->run(*llvm_module);
}

void CodeGenerator::dump() {
	llvm_module->print(errs(), 0);
}
