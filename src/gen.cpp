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
#include <llvm/IR/DerivedTypes.h>
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

#include "lexer.h"
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
    {IF, (gen_stmt_fun) &CodeGenerator::gen_if},
    {WHILE, (gen_stmt_fun) &CodeGenerator::gen_while},
    {BLOCK, (gen_stmt_fun) &CodeGenerator::gen_block},
	{EXPR_STMT, (gen_stmt_fun) &CodeGenerator::gen_expr_stmt},
	{DELETE, (gen_stmt_fun) &CodeGenerator::gen_delete},
};

const std::unordered_map<ExprKind, gen_expr_fun> expr_gen_funs = {
	{BINARY, (gen_expr_fun) &CodeGenerator::gen_binary},
	{CAST, (gen_expr_fun) &CodeGenerator::gen_cast},
	{UNARY, (gen_expr_fun) &CodeGenerator::gen_unary},
	{DEREF, (gen_expr_fun) &CodeGenerator::gen_deref},
	{VARIABLE, (gen_expr_fun) &CodeGenerator::gen_variable},
	{INT_LIT, (gen_expr_fun) &CodeGenerator::gen_int_lit},
	{STRING_LIT, (gen_expr_fun) &CodeGenerator::gen_string_lit},
	{FUNCTION_CALL, (gen_expr_fun) &CodeGenerator::gen_func_call},
	{COMPARE_ZERO, (gen_expr_fun) &CodeGenerator::gen_compare_zero},
	{NIL, (gen_expr_fun) &CodeGenerator::gen_nil},
	{NEW, (gen_expr_fun) &CodeGenerator::gen_new},
	{MEMBER, (gen_expr_fun) &CodeGenerator::gen_member},
};

std::unordered_map<std::string, Value *> constant_string_literals = {};

static Function *current_function;

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
	auto ret_type = gen_return_type(stmt->func_def.return_types);
	auto fun_type = FunctionType::get(ret_type, parameter_types, stmt->func_def.isvararg);
	auto linkage = Function::ExternalLinkage;

 	auto fn = Function::Create(fun_type, linkage, stmt->func_def.mangled_name, llvm_module);

    stmt->func_def.llvm_ref = fn;
    current_function = fn;

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
	auto ret_type = gen_return_type(stmt->func_def.return_types);
	auto fun_type = FunctionType::get(ret_type, parameter_types, stmt->func_def.isvararg);
	auto linkage = Function::ExternalLinkage;

 	auto fn = Function::Create(fun_type, linkage, stmt->func_def.unmangled_name, llvm_module);
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
    } else if (stmt->var_def.flags & VAR_MULTIPLE) {
	    auto val = gen_expr(stmt->var_def.value);
	    auto vars = stmt->var_def.vars;

	    for (int i = 0; i < vars->size(); ++i) {
	        auto var = (*vars)[i];
	        auto var_ptr = builder->CreateAlloca(var->type->llvm_type);
            auto var_val = builder->CreateExtractValue(val, i);

            builder->CreateStore(var_val, var_ptr);

            var->llvm_ref = var_ptr;
        }
	} else {
		auto var_ptr = builder->CreateAlloca(stmt->var_def.var->type->llvm_type);
		if (stmt->var_def.value) {
            auto val = gen_expr(stmt->var_def.value);

            if (val->getType()->isStructTy()) {
                val = builder->CreateExtractValue(val, 0);
            }

            builder->CreateStore(val, var_ptr);
        }

		stmt->var_def.var->llvm_ref = var_ptr;
	} 
}

void CodeGenerator::gen_return(Stmt *stmt) {
    auto return_values = stmt->return_values;

    if (return_values) {
        if (return_values->size() == 1) {
            auto val = (*return_values)[0];
	        builder->CreateRet(gen_expr(val));
        } else {
            auto ty = gen_return_type(return_values);
            auto val = builder->CreateAlloca(ty);
            Value *ret_val = builder->CreateLoad(ty, val);

            for (int i = 0; i < return_values->size(); ++i) {
                ret_val = builder->CreateInsertValue(ret_val, gen_expr((*return_values)[i]), i);
            }

            builder->CreateRet(ret_val);
        }
    } else {
        builder->CreateRetVoid();
    }
}

void CodeGenerator::gen_if(Stmt *stmt) {
    BasicBlock *true_block = BasicBlock::Create(llvm_context, "", current_function);
    BasicBlock *false_block = NULL;
    BasicBlock *after_block = NULL;
    if (stmt->if_.otherwise) {
        false_block = BasicBlock::Create(llvm_context, "", current_function);
        after_block = BasicBlock::Create(llvm_context, "", current_function);
    } else {
        false_block = BasicBlock::Create(llvm_context, "", current_function);
        after_block = false_block;
    }
    Value *cmp = gen_expr(stmt->if_.cond);
    builder->CreateCondBr(cmp, true_block, false_block);
    builder->SetInsertPoint(true_block);
    gen_stmt(stmt->if_.then);
    builder->CreateBr(stmt->if_.otherwise ? after_block : false_block);
    builder->SetInsertPoint(false_block);
    if (stmt->if_.otherwise) {
        gen_stmt(stmt->if_.otherwise);
        builder->CreateBr(after_block);
        builder->SetInsertPoint(after_block);
    }
}

void CodeGenerator::gen_while(Stmt *stmt) {
    BasicBlock *cond_block = BasicBlock::Create(llvm_context, "", current_function);
    BasicBlock *body_block = BasicBlock::Create(llvm_context, "", current_function);
    BasicBlock *after_block = BasicBlock::Create(llvm_context, "", current_function);

    builder->CreateBr(cond_block);
    builder->SetInsertPoint(cond_block);
    Value *cmp = gen_expr(stmt->while_.cond);

    builder->CreateCondBr(cmp, body_block, after_block);
    builder->SetInsertPoint(body_block);

    gen_stmt(stmt->while_.body);

    builder->CreateBr(cond_block);

    builder->SetInsertPoint(after_block); 
}

void CodeGenerator::gen_block(Stmt *stmt) {
    for (auto s : *stmt->stmts)
        gen_stmt(s);
}

void CodeGenerator::gen_expr_stmt(Stmt *stmt) {
    gen_expr(stmt->target_expr);
}

void CodeGenerator::gen_delete(Stmt *stmt) {
    Function *free_fn = llvm_module->getFunction("free");
    auto ptr_ty = typer->make_pointer(typer->get("u8"))->llvm_type;
    if (!free_fn) {
        auto free_fn_type = FunctionType::get(typer->get("void")->llvm_type, {ptr_ty}, false);
	    auto linkage = Function::ExternalLinkage;
        free_fn = Function::Create(free_fn_type, linkage, "free", llvm_module);
    }

    Value *target = gen_expr(stmt->target_expr);

    Value *to_free = builder->CreatePointerCast(target, ptr_ty);
    builder->CreateCall(free_fn, {to_free});
}

Value *CodeGenerator::gen_expr(Expr *expr) {
	auto it = expr_gen_funs.find(expr->kind);
	gen_expr_fun fn = it->second;
	return (*this.*fn)(expr);
}

Value *CodeGenerator::gen_binary(Expr *expr) {
	auto lhs = gen_expr(expr->bin.lhs);
	auto rhs = gen_expr(expr->bin.rhs);
	auto top = expr->bin.op;
	Instruction::BinaryOps op;
	CmpInst::Predicate cmpop;
	Value *new_value = 0;
	bool is_ptr = expr->type->ispointer();

    if (expr->type->isuint() || is_ptr) {
        switch (top) {
            case '+':
            case TOKEN_ADD_EQ: 
                op = Instruction::BinaryOps::Add;
                break;
            case '-':
            case TOKEN_SUB_EQ:
                op = Instruction::BinaryOps::Sub;
                break;
            case '*':
            case TOKEN_MUL_EQ:
                op = Instruction::BinaryOps::Mul;
                break;
            case '/':
            case TOKEN_DIV_EQ:
                op = Instruction::BinaryOps::UDiv;
                break;
            case '%':
            case TOKEN_MOD_EQ:
                op = Instruction::BinaryOps::URem;
                break;
            case TOKEN_EQ_EQ: cmpop = CmpInst::Predicate::ICMP_EQ; break;
            case TOKEN_NOT_EQ: cmpop = CmpInst::Predicate::ICMP_NE; break;
            case TOKEN_LT_EQ: cmpop = CmpInst::Predicate::ICMP_ULE; break;
            case TOKEN_GT_EQ: cmpop = CmpInst::Predicate::ICMP_UGE; break;
            case '<': cmpop = CmpInst::Predicate::ICMP_ULT; break;
            case '>': cmpop = CmpInst::Predicate::ICMP_UGT; break;
	    }
    } else {
        switch (top) {
            case '+':
            case TOKEN_ADD_EQ: 
                op = Instruction::BinaryOps::Add;
                break;
            case '-':
            case TOKEN_SUB_EQ:
                op = Instruction::BinaryOps::Sub;
                break;
            case '*':
            case TOKEN_MUL_EQ:
                op = Instruction::BinaryOps::Mul;
                break;
            case '/':
            case TOKEN_DIV_EQ:
                op = Instruction::BinaryOps::SDiv;
                break;
            case '%':
            case TOKEN_MOD_EQ:
                op = Instruction::BinaryOps::SRem;
                break;
            case TOKEN_EQ_EQ: cmpop = CmpInst::Predicate::ICMP_EQ; break;
            case TOKEN_NOT_EQ: cmpop = CmpInst::Predicate::ICMP_NE; break;
            case TOKEN_LT_EQ: cmpop = CmpInst::Predicate::ICMP_SLE; break;
            case TOKEN_GT_EQ: cmpop = CmpInst::Predicate::ICMP_SGE; break;
            case '<': cmpop = CmpInst::Predicate::ICMP_SLT; break;
            case '>': cmpop = CmpInst::Predicate::ICMP_SGT; break;
	    }
    }

	if (top == '=') {
		new_value = rhs;
	    auto target = gen_expr_target(expr->bin.lhs);
	    builder->CreateStore(new_value, target);
	} else if (ttype_is_binary(top) || top >= TOKEN_ADD_EQ && top <= TOKEN_MOD_EQ) {
	    if (is_ptr) {
	        new_value = builder->CreateInBoundsGEP(expr->type->llvm_type, lhs, rhs);
        } else {
		    new_value = builder->CreateBinOp(op, lhs, rhs);
        }
        if (top >= TOKEN_ADD_EQ && top <= TOKEN_MOD_EQ) {
	        auto target = gen_expr_target(expr->bin.lhs);
	        builder->CreateStore(new_value, target);
        }
	}  else if (ttype_is_conditional(top)) {
        new_value = builder->CreateICmp(cmpop, lhs, rhs);
    } else if (ttype_is_logical(top)) {
        BasicBlock *rhs_block = BasicBlock::Create(llvm_context, "", current_function);
        BasicBlock *merge_block = BasicBlock::Create(llvm_context, "", current_function);

        lhs = builder->CreateIsNotNull(lhs);

        if (top == TOKEN_AND_AND) {
            builder->CreateCondBr(lhs, rhs_block, merge_block);
        } else {
            builder->CreateCondBr(lhs, merge_block, rhs_block);
        }

        BasicBlock *lhs_block = builder->GetInsertBlock();

    	builder->SetInsertPoint(rhs_block);
        rhs = builder->CreateIsNotNull(rhs);

        builder->CreateBr(merge_block);
        builder->SetInsertPoint(merge_block);

        PHINode *cmp = builder->CreatePHI(expr->type->llvm_type, 2);
        cmp->addIncoming(lhs, lhs_block);
        cmp->addIncoming(rhs, rhs_block);

        return cmp;
    }

    return new_value;
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
    auto target = gen_expr(expr->target);
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

Value *CodeGenerator::gen_compare_zero(Expr *expr) {
    Value *target = gen_expr(expr->target);
	Value *zero;
	if (expr->target->type->ispointer()) {
	    zero = ConstantPointerNull::get((PointerType *) expr->target->type->llvm_type);
    } else {
	    zero = ConstantInt::get(typer->get("s32")->llvm_type, 0);
    }

    return builder->CreateICmp(ICmpInst::Predicate::ICMP_NE, target, zero);
}

Value *CodeGenerator::gen_nil(Expr *expr) {
    return ConstantPointerNull::get((PointerType *) expr->type->llvm_type);
}

Value *CodeGenerator::gen_new(Expr *expr) {
    auto i64_ty = typer->get("u64")->llvm_type;
    Function *malloc_fn = llvm_module->getFunction("malloc");
    if (!malloc_fn) {
        auto ptr_ty = typer->make_pointer(typer->get("u8"))->llvm_type;
        auto malloc_fn_type = FunctionType::get(ptr_ty, {i64_ty}, false);
	    auto linkage = Function::ExternalLinkage;
        malloc_fn = Function::Create(malloc_fn_type, linkage, "malloc", llvm_module);
    }

    Type *target_type = expr->alloc_type->llvm_type;
    Value *type_size = ConstantInt::get(i64_ty, llvm_size_of(target_type));

    Value *mallocd = builder->CreateCall(malloc_fn, {type_size});
    return builder->CreatePointerCast(mallocd, expr->type->llvm_type);
}

Value *CodeGenerator::gen_member(Expr *expr) {
    return builder->CreateLoad(expr->type->llvm_type, gen_expr_target(expr));
}

Value *CodeGenerator::gen_expr_target(Expr *expr) {
    if (expr->kind == VARIABLE) {
        return expr->var->llvm_ref;
    }

    if (expr->kind == DEREF) {
        auto target = gen_expr_target(expr->target); 
        auto type = expr->target->type->llvm_type;

        return builder->CreateLoad(type, target);
    }

    if (expr->kind == MEMBER) {
        auto target = gen_expr_target(expr->member.target);
		auto i32_ty = typer->get("s32")->llvm_type;
		auto indices = *expr->member.indices;

		for (int i = 0; i < indices.size(); ++i) {
			auto index = indices[i];
			auto llvm_zero = ConstantInt::get(i32_ty, 0);
			auto llvm_index = ConstantInt::get(i32_ty, index);

			if ((*expr->member.dereferences)[i]) {
				target = builder->CreateLoad(target);
			}
			target = builder->CreateInBoundsGEP(target, {llvm_zero, llvm_index});
		}

		return target;
    }

    /* unreachable */
    return 0;
}

Type *CodeGenerator::gen_return_type(std::vector<QType *> *types) {
    if (types->size() == 1)
        return (*types)[0]->llvm_type;

	std::vector<Type *> return_types(types->size());
	for (int i = 0; i < types->size(); i++) {
		return_types[i] = (*types)[i]->llvm_type;
	}

    return StructType::get(llvm_context, return_types);
}

Type *CodeGenerator::gen_return_type(std::vector<Expr *> *types) {
	std::vector<Type *> return_types(types->size());
	for (int i = 0; i < types->size(); i++) {
		return_types[i] = (*types)[i]->type->llvm_type;
	}

    return StructType::get(llvm_context, return_types);
}

int CodeGenerator::llvm_size_of(Type *type) {
    int size = llvm_module->getDataLayout().getTypeSizeInBits(type);    
    return size / 8;
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

void CodeGenerator::output(Options *options) {
    if (options->flags & OPTIMIZE) {
        optimize();
    }

#ifdef _WIN32
    std::error_code std_error;
    auto out = new ToolOutputFile(options->ll_file, std_error, sys::fs::OF_None);
    if (!out) {
        std::cerr << "Could not open file " << options->ll_file << "\n";
        return;
    }

    raw_pwrite_stream *os = &out->os();

    llvm_module->print(*os, nullptr);
#else
    std::error_code std_error;
    auto out = new ToolOutputFile(options->obj_file, std_error, sys::fs::OF_None);
    if (!out) {
        std::cerr << "Could not open file " << options->obj_file << "\n";
        return;
    }

    raw_pwrite_stream *os = &out->os();

    legacy::PassManager pm;

    if (target_machine->addPassesToEmitFile(pm, *os, nullptr, CodeGenFileType::CGFT_ObjectFile, false)) {
        std::cerr << options->obj_file << ": target does not support generation of this file type!\n";
        return;
    }

    pm.run(*llvm_module);
#endif

    out->keep();
}

void CodeGenerator::link(Options *options) {
    std::stringstream cmd;

#ifdef _WIN32
	cmd << "clang -o";
    cmd << options->exe_file << " ";
    cmd << options->ll_file << " ";

	for (auto lib : options->libs) {
	    cmd << lib << " ";
    }

	for (auto linker_flags : options->linker_flags) {
	    cmd << linker_flags << " ";
    }

    std::system(cmd.str().c_str());
    std::remove(options->ll_file);
#else
	cmd << "gcc -o ";
	cmd << options->exe_file << " ";
    cmd << options->obj_file;

	for (auto lib : options->libs) {
	    cmd << " -l" << lib;
    }

	for (auto linker_flags : options->linker_flags) {
	    cmd << " " << linker_flags;
    }

    std::cout << cmd.str() << "\n";
    std::system(cmd.str().c_str());
    std::remove(options->obj_file);
#endif
}

void CodeGenerator::optimize() {
    legacy::PassManager *pm = new legacy::PassManager();
    PassManagerBuilder pmb;
    pmb.Inliner = createFunctionInliningPass(3, 3, false);
    pmb.OptLevel = 3;
    pmb.SizeLevel = 3;
    pmb.DisableUnrollLoops = false;
    pmb.LoopVectorize = true;
    pmb.SLPVectorize = true;
    pmb.populateModulePassManager(*pm);
    pm->run(*llvm_module);
}

void CodeGenerator::dump(Options *options) {
    if (options->flags & PRINT_LLVM) {
        std::error_code std_error;
        auto out = new ToolOutputFile(options->ll_file, std_error, sys::fs::OF_None);
        if (!out) {
            std::cerr << "Could not open file " << options->ll_file << "\n";
            return;
        }

        raw_pwrite_stream *os = &out->os();
        llvm_module->print(*os, nullptr);
        out->keep();
    }
   // llvm_module->print(errs(), 0);
}
