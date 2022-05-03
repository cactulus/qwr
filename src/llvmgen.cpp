#include <iostream>
#include <unordered_map>
#include <string>
#include <sstream>

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
#include "llvmgen.h"
#include "manager.h"

using namespace llvm;

typedef void (CodeGenerator::*gen_stmt_fun) (Stmt *stmt);
typedef Value *(CodeGenerator::*gen_expr_fun) (Expr *expr);

const std::unordered_map<StmtKind, gen_stmt_fun> stmt_gen_funs = {
	{FUNCTION_DEFINITION, (gen_stmt_fun) &CodeGenerator::gen_func_def},
	{VARIABLE_DEFINITION, (gen_stmt_fun) &CodeGenerator::gen_var_def},
	{RETURN, (gen_stmt_fun) &CodeGenerator::gen_return},
    {IF, (gen_stmt_fun) &CodeGenerator::gen_if},
    {WHILE, (gen_stmt_fun) &CodeGenerator::gen_while},
	{FOR, (gen_stmt_fun)&CodeGenerator::gen_for},
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
	{FLOAT_LIT, (gen_expr_fun)&CodeGenerator::gen_float_lit},
	{STRING_LIT, (gen_expr_fun) &CodeGenerator::gen_string_lit},
	{FUNCTION_CALL, (gen_expr_fun) &CodeGenerator::gen_func_call},
	{BUILTIN_FUNCTION, (gen_expr_fun)&CodeGenerator::gen_builtin},
	{COMPARE_ZERO, (gen_expr_fun) &CodeGenerator::gen_compare_zero},
	{NIL, (gen_expr_fun) &CodeGenerator::gen_nil},
	{NEW, (gen_expr_fun) &CodeGenerator::gen_new},
	{MEMBER, (gen_expr_fun) &CodeGenerator::gen_member},
	{INDEXED, (gen_expr_fun) &CodeGenerator::gen_indexed},
};

std::unordered_map<std::string, Value *> constant_string_literals = {};

static Function *current_function;
static Type *s32_ty;
static Type *u8_ty;
static Type *u64_ty;

void CodeGenerator::init(Typer *_typer) {
	typer = _typer;

	llvm_module = new Module("test.ll", llvm_context);
	builder = new IRBuilder<>(llvm_context);

    init_module();
	init_types();
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
	auto fun_type = FunctionType::get(ret_type, parameter_types, stmt->func_def.flags & FUNCTION_VARARG);
	auto linkage = Function::ExternalLinkage;

	auto name = (stmt->func_def.flags & FUNCTION_EXTERN) ?
				stmt->func_def.unmangled_name :
				stmt->func_def.mangled_name;

	auto fn = Function::Create(fun_type, linkage, name, llvm_module);

    stmt->func_def.llvm_ref = fn;

	if (stmt->func_def.flags & FUNCTION_EXTERN) {
		return;
	}

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
		auto var = stmt->var_def.var;
		auto var_type = var->type;

		Value *var_ptr = 0;
		Value *type_size_llvm = 0;

		if (var_type->isarray()) {
			var_ptr = builder->CreateAlloca(var_type->llvm_type);

			auto create_fn = get_builtin("qwr_array_create");
			auto type_size = llvm_size_of(var_type->element_type->llvm_type);
			type_size_llvm = ConstantInt::get(u64_ty, type_size);
			auto array_ptr = builder->CreateCall(create_fn, { type_size_llvm });
			builder->CreateStore(array_ptr, var_ptr);
		} else {
			var_ptr = builder->CreateAlloca(var_type->llvm_type);
		}

		if (stmt->var_def.value) {
			if (stmt->var_def.value->kind == COMPOUND_LIT) {
				auto init_expr = stmt->var_def.value;

				auto values = *init_expr->init.values;

				auto target = var_ptr;
				if (var_type->isarray()) {
					auto init_fn = get_builtin("qwr_array_init");
					auto data_fn = get_builtin("qwr_array_data");
					auto loaded_var_ptr = builder->CreateLoad(var_ptr);
					auto llvm_values_count = ConstantInt::get(u64_ty, values.size());

					builder->CreateCall(init_fn, { loaded_var_ptr, llvm_values_count, type_size_llvm });
					target = builder->CreateCall(data_fn, { loaded_var_ptr });
					target = builder->CreatePointerCast(target, var_type->data_type->llvm_type);

					for (int i = 0; i < values.size(); ++i) {
						auto llvm_index = ConstantInt::get(s32_ty, i);
						auto val = gen_expr(values[i]);

						auto tar = builder->CreateInBoundsGEP(target, { llvm_index });
						builder->CreateStore(val, tar);
					}
				} else {
					for (int i = 0; i < values.size(); ++i) {
						auto llvm_zero = ConstantInt::get(s32_ty, 0);
						auto llvm_index = ConstantInt::get(s32_ty, i);
						auto val = gen_expr(values[i]);

						auto tar = builder->CreateInBoundsGEP(target, { llvm_zero, llvm_index });
						builder->CreateStore(val, tar);
					}
				}
			} else {
				auto val = gen_expr(stmt->var_def.value);
				if (val->getType()->isStructTy()) {
					val = builder->CreateExtractValue(val, 0);
				}
				builder->CreateStore(val, var_ptr);
			}
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
            Value *ret_val = builder->CreateLoad(val);

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
	if (!true_block->getTerminator()) {
		builder->CreateBr(stmt->if_.otherwise ? after_block : false_block);
	}

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

void CodeGenerator::gen_for(Stmt *stmt) {
	auto size_t = typer->get("u64")->llvm_type;
	Value *iterator_val;
	Value *from;
	Value *to;
	Type *var_type;

	if (stmt->for_.is_range) {
		from = gen_expr(stmt->for_.range_from);
		to = gen_expr(stmt->for_.range_to);
		var_type = size_t;
	} else {
		from = ConstantInt::get(size_t, 0);

		iterator_val = gen_expr_target(stmt->for_.iterator);
		auto ty = stmt->for_.iterator->type;
		var_type = stmt->for_.var->type->llvm_type;
		Function *f;
		if (ty->isarray()) {
			f = get_builtin("qwr_array_len");
		} else {
			f = get_builtin("strlen");
		}
		to = builder->CreateCall(f, builder->CreateLoad(iterator_val));
	}

	auto cond_block = BasicBlock::Create(llvm_context, "", current_function);
	auto body_block = BasicBlock::Create(llvm_context, "", current_function);
	auto inc_block = BasicBlock::Create(llvm_context, "", current_function);
	auto after_block = BasicBlock::Create(llvm_context, "", current_function);

	auto inc_var = builder->CreateAlloca(size_t);
	auto var = stmt->for_.is_range ? inc_var : builder->CreateAlloca(var_type);
	stmt->for_.var->llvm_ref = var;

	builder->CreateStore(from, inc_var);

	builder->CreateBr(cond_block);
	builder->SetInsertPoint(cond_block);

	auto loaded_index = builder->CreateLoad(inc_var);
	auto cmp = builder->CreateICmpSLT(loaded_index, to);
	builder->CreateCondBr(cmp, body_block, after_block);

	builder->SetInsertPoint(body_block);

	loaded_index = builder->CreateLoad(inc_var);
	if (!stmt->for_.is_range) {
		auto it_ty = stmt->for_.iterator->type;
		if (it_ty->isarray()) {
			auto data_type = stmt->for_.iterator->type->data_type->llvm_type;
			auto var_val = gen_array_indexed(iterator_val, data_type, loaded_index);

			auto loaded = builder->CreateLoad(var_val);
			builder->CreateStore(loaded, var);
		} else {
			auto var_val = gen_string_indexed(iterator_val, loaded_index);

			auto loaded = builder->CreateLoad(var_val);
			builder->CreateStore(loaded, var);
		}
	}

	gen_stmt(stmt->for_.body);
	builder->CreateBr(inc_block);

	builder->SetInsertPoint(inc_block);
	loaded_index = builder->CreateLoad(inc_var);
	auto added = builder->CreateNSWAdd(
		loaded_index,
		ConstantInt::get(size_t, 1)
	);
	builder->CreateStore(added, inc_var);
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
	auto target_ty = stmt->target_expr->type;

	if (target_ty->isarray()) {
		Function *free_fn = get_builtin("qwr_array_free");
		Value *target = builder->CreateLoad(gen_expr_target(stmt->target_expr));
		builder->CreateCall(free_fn, { target });
	} else {
		Function *free_fn = get_builtin("free");
		auto ptr_ty = typer->make_pointer(typer->get("u8"))->llvm_type;

		Value *target = gen_expr(stmt->target_expr);

		Value *to_free = builder->CreatePointerCast(target, ptr_ty);
		builder->CreateCall(free_fn, { to_free });
	}
}

Value *CodeGenerator::gen_expr(Expr *expr) {
	auto it = expr_gen_funs.find(expr->kind);
	gen_expr_fun fn = it->second;
	return (*this.*fn)(expr);
}

Value *CodeGenerator::gen_binary(Expr *expr) {
	auto rhs = gen_expr(expr->bin.rhs);
	auto top = expr->bin.op;
	Instruction::BinaryOps op;
	CmpInst::Predicate cmpop;
	Value *new_value = 0;
	auto ty = expr->type;
	bool is_ptr = ty->ispointer();

    if (ty->isuint() || is_ptr) {
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
    } else if (ty->is_int_in_llvm()) {
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
	} else if (ty->isfloat()) {
		switch (top) {
		case '+':
		case TOKEN_ADD_EQ:
			op = Instruction::BinaryOps::FAdd;
			break;
		case '-':
		case TOKEN_SUB_EQ:
			op = Instruction::BinaryOps::FSub;
			break;
		case '*':
		case TOKEN_MUL_EQ:
			op = Instruction::BinaryOps::FMul;
			break;
		case '/':
		case TOKEN_DIV_EQ:
			op = Instruction::BinaryOps::FDiv;
			break;
		case '%':
		case TOKEN_MOD_EQ:
			op = Instruction::BinaryOps::FRem;
			break;
		case TOKEN_EQ_EQ: cmpop = CmpInst::Predicate::FCMP_UEQ; break;
		case TOKEN_NOT_EQ: cmpop = CmpInst::Predicate::FCMP_UNE; break;
		case TOKEN_LT_EQ: cmpop = CmpInst::Predicate::FCMP_ULE; break;
		case TOKEN_GT_EQ: cmpop = CmpInst::Predicate::FCMP_UGE; break;
		case '<': cmpop = CmpInst::Predicate::FCMP_ULT; break;
		case '>': cmpop = CmpInst::Predicate::FCMP_UGT; break;
		}
	} else if (top != '=') {
		std::cout << ty->base << "\n";
		llvm_unreachable("Not implemented binary expression for this type");
	}

	if (top == '=') {
		new_value = rhs;
	    auto target = gen_expr_target(expr->bin.lhs);
	    builder->CreateStore(new_value, target);
	} else if (ttype_is_binary(top) || top >= TOKEN_ADD_EQ && top <= TOKEN_MOD_EQ) {
		auto lhs = gen_expr(expr->bin.lhs);
	    if (is_ptr) {
	        new_value = builder->CreateInBoundsGEP(expr->type->llvm_type, lhs, rhs);
        } else {
			if (op == BinaryOperator::Add) {
				new_value = builder->CreateNSWAdd(lhs, rhs);
			} else {
				new_value = builder->CreateBinOp(op, lhs, rhs);
			}
        }
        if (top >= TOKEN_ADD_EQ && top <= TOKEN_MOD_EQ) {
	        auto target = gen_expr_target(expr->bin.lhs);
	        builder->CreateStore(new_value, target);
        }
	}  else if (ttype_is_conditional(top)) {
		auto lhs = gen_expr(expr->bin.lhs);
		if (ty->isfloat()) {
			new_value = builder->CreateFCmp(cmpop, lhs, rhs);
		} else {
			new_value = builder->CreateICmp(cmpop, lhs, rhs);
		}
    } else if (ttype_is_logical(top)) {
		auto lhs = gen_expr(expr->bin.lhs);
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
	QType *ft = expr->cast.from;
	QType *tt = expr->cast.to;
    QBaseType bf = ft->base;
    QBaseType bt = tt->base;
    auto target = gen_expr(expr->cast.target);
    auto to = tt->llvm_type;

    if (bf == TYPE_POINTER && bt == TYPE_POINTER) {
        return builder->CreatePointerCast(target, to);
    }

	if (ft->is_int_in_llvm() && tt->is_int_in_llvm()) {
		return builder->CreateIntCast(target, to, ft->isuint());
	}

	if (ft->isfloat() && tt->isfloat()) {
		return builder->CreateFPCast(target, to);
	}

	if (ft->is_int_in_llvm() && tt->isfloat()) {
		if (ft->isuint()) {
			return builder->CreateUIToFP(target, to);
		}
		return builder->CreateSIToFP(target, to);
	}

	if (ft->isfloat() && tt->is_int_in_llvm()) {
		if (tt->isuint()) {
			return builder->CreateFPToUI(target, to);
		}
		return builder->CreateFPToSI(target, to);
	}

	llvm_unreachable("Cast not implemented");
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
			if (expr->type->isfloat()) {
				return builder->CreateFNeg(target);
			} else {
				return builder->CreateNeg(target);
			}
            
        } break;
		case TOKEN_PLUS_PLUS:
		case TOKEN_MINUS_MINUS: {
			auto target = gen_expr_target(expr->unary.target);
			auto loaded = builder->CreateLoad(target);
			auto type = expr->type->llvm_type;
			Value *one;

			if (expr->unary.op == TOKEN_PLUS_PLUS) {
				one = ConstantInt::get(type, 1);
			} else {
				one = ConstantInt::get(type, -1);
			}

			auto val = builder->CreateNSWAdd(loaded, one);
			builder->CreateStore(val, target);

			if (expr->unary.ispost) {
				return loaded;
			} else {
				return val;
			}
		}
    }

    return 0;
}

Value *CodeGenerator::gen_deref(Expr *expr) {
    auto target = gen_expr(expr->target);
    return builder->CreateLoad(target);
}

Value *CodeGenerator::gen_variable(Expr *expr) {
	return builder->CreateLoad(expr->var->llvm_ref);
}

Value *CodeGenerator::gen_int_lit(Expr *expr) {
	return ConstantInt::get(expr->type->llvm_type, expr->int_value);
}

Value *CodeGenerator::gen_float_lit(Expr *expr) {
	return ConstantFP::get(expr->type->llvm_type, expr->float_value);
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
	QType *ty = expr->target->type;
	Value *zero;

	if (ty->ispointer()) {
		zero = ConstantPointerNull::get((PointerType *)ty->llvm_type);
		return builder->CreateICmpNE(target, zero);
	} else if (ty->isfloat()) {
		zero = ConstantFP::get(ty->llvm_type, 0.0);
		return builder->CreateFCmpUNE(target, zero);
    } else {
		zero = ConstantInt::get(ty->llvm_type, 0);
		return builder->CreateICmpNE(target, zero);
    }
}

Value *CodeGenerator::gen_nil(Expr *expr) {
    return ConstantPointerNull::get((PointerType *) expr->type->llvm_type);
}

Value *CodeGenerator::gen_new(Expr *expr) {
    Function *malloc_fn = get_builtin("malloc");

    Type *target_type = expr->alloc_type->llvm_type;
    Value *type_size = ConstantInt::get(u64_ty, llvm_size_of(target_type));

    Value *mallocd = builder->CreateCall(malloc_fn, {type_size});
    return builder->CreatePointerCast(mallocd, expr->type->llvm_type);
}

Value *CodeGenerator::gen_member(Expr *expr) {
    return builder->CreateLoad(gen_expr_target(expr));
}

Value *CodeGenerator::gen_indexed(Expr *expr) {
	return builder->CreateLoad(gen_expr_target(expr));
}

Value *CodeGenerator::gen_expr_target(Expr *expr) {
    if (expr->kind == VARIABLE) {
        return expr->var->llvm_ref;
    }

    if (expr->kind == DEREF) {
        auto target = gen_expr_target(expr->target); 
        auto type = expr->target->type->llvm_type;

        return builder->CreateLoad(target);
    }

	if (expr->kind == INDEXED) {
		auto index = expr->indexed.index;
		auto target_ty = expr->indexed.target->type;
		if (target_ty->isarray()) {
			return gen_array_indexed(gen_expr_target(expr->indexed.target), target_ty->data_type->llvm_type, gen_expr(index));
		} else if (target_ty->isstring()) {
			return gen_string_indexed(gen_expr_target(expr->indexed.target), gen_expr(index));
		} else {
			auto target = gen_expr(expr->indexed.target);

			return builder->CreateInBoundsGEP(target, { gen_expr(index) });
		}
	}

    if (expr->kind == MEMBER) {
        auto target = gen_expr_target(expr->member.target);
		auto indices = *expr->member.indices;

		for (int i = 0; i < indices.size(); ++i) {
			auto index = indices[i];
			auto llvm_zero = ConstantInt::get(s32_ty, 0);
			auto llvm_index = ConstantInt::get(s32_ty, index);

			if ((*expr->member.dereferences)[i]) {
				target = builder->CreateLoad(target);
			}
			target = builder->CreateInBoundsGEP(target, {llvm_zero, llvm_index});
		}

		return target;
    }

	if (expr->kind == CAST) {
		return gen_expr_target(expr->target);
	}

    /* unreachable */
    return 0;
}

Value *CodeGenerator::gen_array_indexed(Value *arr, Type *arr_ty, Value *index) {
	auto data_fn = get_builtin("qwr_array_data");

	auto var_ptr = builder->CreateLoad(arr);
	Value *target = builder->CreateCall(data_fn, { var_ptr });
	target = builder->CreatePointerCast(target, arr_ty);

	return builder->CreateInBoundsGEP(target, { index });
}

Value *CodeGenerator::gen_string_indexed(Value *str, Value *index) {
	auto str_ptr = builder->CreateLoad(str);
	return builder->CreateInBoundsGEP(str_ptr, { index });
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

    std::string mcpu = sys::getHostCPUName().str();

    TargetOptions options;
    target_machine = target->createTargetMachine(triple.getTriple(), mcpu, "", options, llvm::Reloc::PIC_);

    if (!target_machine) {
        std::cerr << "Error: Could not create target machine.\n";
        return;
    }

    const auto dl = target_machine->createDataLayout();
    llvm_module->setDataLayout(dl);
}

void CodeGenerator::init_types() {
	s32_ty = typer->get("s32")->llvm_type;
	u8_ty = typer->get("u8")->llvm_type;
	u64_ty = typer->get("u64")->llvm_type;
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
    pmb.Inliner = createFunctionInliningPass(3, 0, false);
    pmb.OptLevel = 3;
    pmb.SizeLevel = 0;
    pmb.DisableUnrollLoops = false;
    pmb.LoopVectorize = true;
    pmb.SLPVectorize = true;
    pmb.populateModulePassManager(*pm);
    pm->run(*llvm_module);
}

void CodeGenerator::dump(Options *options) {
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
