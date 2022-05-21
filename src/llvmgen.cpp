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
#include "llvm/Transforms/Scalar.h"

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
    {COMPOUND, (gen_stmt_fun) &CodeGenerator::gen_compound},
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
	{SIZEOF, (gen_expr_fun) &CodeGenerator::gen_sizeof},
	{COMPOUND_LIT, (gen_expr_fun) &CodeGenerator::gen_compound_lit},
};

std::unordered_map<std::string, Value *> constant_string_literals = {};
static int global_constants_count = 0;

static Function *current_function;
static Type *s32_ty;
static Type *u8_ty;
static Type *u8ptr_ty;
static Type *u64_ty;

void CodeGenerator::init(Typer *_typer, Options * _options) {
	typer = _typer;
	debug = _options->flags & QWR_DEBUG;

	llvm_module = new Module("test.ll", llvm_context);
	builder = new IRBuilder<>(llvm_context);
    
    init_module();
	init_types();
}

void CodeGenerator::init_debug(const char *src_file) {
	dbg_builder = new DIBuilder(*llvm_module);
	dbg_cu = dbg_builder->createCompileUnit(
		dwarf::DW_LANG_C, dbg_builder->createFile(src_file, ""),
		"QWR Compiler", 0, "", 0
	);
	dbg_file = dbg_builder->createFile(src_file, "");
	dbg_scopes.push_back(dbg_file);
}

void CodeGenerator::gen(Stmt *stmt) {
    if (!stmt)
        return;

	if (debug)
		emit_location_dbg(stmt);

	auto it = stmt_gen_funs.find(stmt->kind());
	gen_stmt_fun fn = it->second;
	(*this.*fn)(stmt);
}

void CodeGenerator::gen_func_def(FunctionDefinition *stmt) {
	std::vector<Type *> parameter_types(stmt->parameters.size());
	for (int i = 0; i < stmt->parameters.size(); i++) {
		parameter_types[i] = stmt->parameters[i]->type->llvm_type;
	}
	auto ret_type = gen_return_type(stmt->return_types);
	auto fun_type = FunctionType::get(ret_type, parameter_types, stmt->flags & FUNCTION_VARARG);
	auto linkage = Function::ExternalLinkage;

	auto name = (stmt->flags & FUNCTION_EXTERN) ?
				stmt->unmangled_name :
				stmt->mangled_name;

	auto fn = Function::Create(fun_type, linkage, name, llvm_module);
    stmt->llvm_ref = fn;

	if (stmt->flags & FUNCTION_EXTERN) {
		return;
	}

	current_function = fn;

	if (debug) {
		std::vector<Metadata *> dbg_params;
		dbg_params.push_back(convert_type_dbg(stmt->return_types[0]));
		for (int i = 0; i < parameter_types.size(); i++) {
			dbg_params.push_back(convert_type_dbg(stmt->parameters[i]->type));
		}

		auto dbg_fun_type = dbg_builder->createSubroutineType(
			dbg_builder->getOrCreateTypeArray(dbg_params)
		);

		unsigned line_number = stmt->location.line;
		unsigned scope_line = line_number;
		auto subprogram = dbg_builder->createFunction(
			dbg_scopes[0], name, "", dbg_file, line_number,
			dbg_fun_type, scope_line,
			DINode::FlagPrototyped,
			DISubprogram::DISPFlags::SPFlagLocalToUnit | DISubprogram::DISPFlags::SPFlagDefinition);

		fn->setSubprogram(subprogram);
		dbg_scopes.push_back(subprogram);

		builder->SetCurrentDebugLocation(
			DebugLoc()
		);
	}

	auto entry = BasicBlock::Create(llvm_context, "", fn);
	builder->SetInsertPoint(entry);

	int i = 0;
	for (auto arg_it = fn->arg_begin(); arg_it != fn->arg_end(); arg_it++) {
		auto par = stmt->parameters[i];
		auto var = builder->CreateAlloca(par->type->llvm_type);
		par->llvm_ref = var;
		builder->CreateStore(&*arg_it, var);

		if (debug) {
			auto ln = stmt->location.line;
			auto dbg_scope = dbg_scopes.back();
			auto dbg_par = dbg_builder->createParameterVariable(
				dbg_scope, par->name, i, dbg_file, ln, convert_type_dbg(par->type),
				true);

			dbg_builder->insertDeclare(var, dbg_par, dbg_builder->createExpression(),
				DILocation::get(dbg_scope->getContext(), ln, 0, dbg_scope), builder->GetInsertBlock());
		}

		i++;
	}

	for (auto s : stmt->body) {
		gen(s);
	}

    if (builder->GetInsertBlock()->getTerminator() == 0) {
        builder->CreateRetVoid();
    }

	if (debug) {
		dbg_scopes.pop_back();
	}
}

void CodeGenerator::gen_var_def(VariableDefinition *stmt) {
	if (stmt->flags & VAR_GLOBAL) {
		auto var_name = stmt->var->name;

		llvm_module->getOrInsertGlobal(var_name, stmt->var->type->llvm_type);
		auto var = llvm_module->getGlobalVariable(var_name);

		var->setConstant(stmt->flags & VAR_CONST);
		
        auto init = dyn_cast<Constant>(gen(stmt->value));
        var->setInitializer(init);

		stmt->var->llvm_ref = var;
    } else if (stmt->flags & VAR_MULTIPLE) {
	    auto val = gen(stmt->value);
	    auto vars = stmt->vars;

	    for (int i = 0; i < vars.size(); ++i) {
	        auto var = vars[i];
	        auto var_ptr = builder->CreateAlloca(var->type->llvm_type);
            auto var_val = builder->CreateExtractValue(val, i);

			if (debug) {
				auto ln = stmt->location.line;
				auto dbg_scope = dbg_scopes.back();
				auto dbg_var = dbg_builder->createAutoVariable(dbg_scope, var->name, dbg_file, ln, convert_type_dbg(var->type));

				dbg_builder->insertDeclare(var_ptr, dbg_var, dbg_builder->createExpression(),
					DILocation::get(dbg_scope->getContext(), ln, 0, dbg_scope), builder->GetInsertBlock());
			}

            builder->CreateStore(var_val, var_ptr);

            var->llvm_ref = var_ptr;
        }
	} else {
		auto var = stmt->var;
		auto var_type = var->type;

		Value *var_ptr = 0;
		Value *type_size_llvm = 0;

		if (var_type->isarray()) {
			var_ptr = builder->CreateAlloca(var_type->llvm_type);

            if (!var_type->array_is_static) {
                auto create_fn = get_builtin("qwr_array_create");
                auto type_size = llvm_size_of(var_type->element_type->llvm_type);
                type_size_llvm = ConstantInt::get(u64_ty, type_size);
                auto array_ptr = builder->CreateCall(create_fn, { type_size_llvm });
                builder->CreateStore(array_ptr, var_ptr);
            }
		} else {
			var_ptr = builder->CreateAlloca(var_type->llvm_type);
		}

        if (debug) {
            auto ln = stmt->location.line;
            auto dbg_scope = dbg_scopes.back();
            auto dbg_var = dbg_builder->createAutoVariable(dbg_scope, var->name, dbg_file, ln, convert_type_dbg(var->type));

            dbg_builder->insertDeclare(var_ptr, dbg_var, dbg_builder->createExpression(),
                DILocation::get(dbg_scope->getContext(), ln, 0, dbg_scope), builder->GetInsertBlock());
        }

        stmt->var->llvm_ref = var_ptr;

		if (!stmt->value) {
		    return;
        }

        if (stmt->value->type->isarray() || stmt->value->type->isstruct()) {
            auto init_expr = (CompoundLiteral *) stmt->value;
            auto values = init_expr->values;
            auto target = var_ptr;

            bool is_dynamic_array = var_type->isarray() && !var_type->array_is_static;
            if (is_dynamic_array) {
                auto init_fn = get_builtin("qwr_array_init");
                auto data_fn = get_builtin("qwr_array_data");
                auto loaded_var_ptr = builder->CreateLoad(var_ptr);
                auto llvm_values_count = ConstantInt::get(u64_ty, values.size());

                builder->CreateCall(init_fn, { loaded_var_ptr, llvm_values_count, type_size_llvm });
                target = builder->CreateCall(data_fn, { loaded_var_ptr });
            }
            if (init_expr->lit_is_constant) {
                target = builder->CreateBitCast(target, u8ptr_ty);

                QType *lit_type = var_type;
                if (is_dynamic_array) {
                    lit_type = typer->make_array(var_type->element_type, true, values.size());
                }

                auto constant_value = builder->CreateBitCast(gen_constant_compound_lit_var(init_expr, lit_type), u8ptr_ty);

                MaybeAlign align(llvm_align_of(lit_type->llvm_type));
                auto mem_cpy_size = llvm_size_of(lit_type->llvm_type);
                builder->CreateMemCpy(target, align, constant_value, align, mem_cpy_size);
            } else {
                if (is_dynamic_array) {
                    target = builder->CreatePointerCast(target, var_type->data_type->llvm_type);
                }
                
                for (int i = 0; i < values.size(); ++i) {
                    auto llvm_zero = ConstantInt::get(s32_ty, 0);
                    auto llvm_index = ConstantInt::get(s32_ty, i);
                    auto val = gen(values[i]);
                    Value *tar;
                    if (is_dynamic_array) {
                        tar = builder->CreateInBoundsGEP(target, { llvm_index });
                    } else {
                        tar = builder->CreateInBoundsGEP(target, { llvm_zero, llvm_index });
                    }
                    builder->CreateStore(val, tar);
                }
            }

        } else {
            auto val = gen(stmt->value);
            if (val->getType()->isStructTy()) {
                val = builder->CreateExtractValue(val, 0);
            }
            builder->CreateStore(val, var_ptr);
        }
    }
}

void CodeGenerator::gen_return(Return *stmt) {
    auto return_values = stmt->return_values;

    if (return_values.size() > 0) {
        if (return_values.size() == 1) {
            auto val = return_values[0];
	        builder->CreateRet(gen(val));
        } else {
            auto ty = gen_return_type(return_values);
            auto val = builder->CreateAlloca(ty);
            Value *ret_val = builder->CreateLoad(val);

            for (int i = 0; i < return_values.size(); ++i) {
                ret_val = builder->CreateInsertValue(ret_val, gen(return_values[i]), i);
            }

            builder->CreateRet(ret_val);
        }
    } else {
        builder->CreateRetVoid();
    }
}

void CodeGenerator::gen_if(If *stmt) {
    BasicBlock *true_block = BasicBlock::Create(llvm_context, "", current_function);
    BasicBlock *false_block = NULL;
    BasicBlock *after_block = NULL;
    if (stmt->otherwise) {
        false_block = BasicBlock::Create(llvm_context, "", current_function);
        after_block = BasicBlock::Create(llvm_context, "", current_function);
    } else {
        false_block = BasicBlock::Create(llvm_context, "", current_function);
        after_block = false_block;
    }
    Value *cmp = gen(stmt->cond);
    builder->CreateCondBr(cmp, true_block, false_block);
    builder->SetInsertPoint(true_block);
    gen(stmt->then);
	if (!true_block->getTerminator()) {
		builder->CreateBr(stmt->otherwise ? after_block : false_block);
	}

    builder->SetInsertPoint(false_block);
    if (stmt->otherwise) {
        gen(stmt->otherwise);
        builder->CreateBr(after_block);
        builder->SetInsertPoint(after_block);
    }
}

void CodeGenerator::gen_while(While *stmt) {
    BasicBlock *cond_block = BasicBlock::Create(llvm_context, "", current_function);
    BasicBlock *body_block = BasicBlock::Create(llvm_context, "", current_function);
    BasicBlock *after_block = BasicBlock::Create(llvm_context, "", current_function);

    builder->CreateBr(cond_block);
    builder->SetInsertPoint(cond_block);
    Value *cmp = gen(stmt->cond);

    builder->CreateCondBr(cmp, body_block, after_block);
    builder->SetInsertPoint(body_block);

    gen(stmt->body);

    builder->CreateBr(cond_block);

    builder->SetInsertPoint(after_block); 
}

void CodeGenerator::gen_for(For *stmt) {
	auto size_t = typer->get("u64")->llvm_type;
	Value *iterator_val;
    Value *loaded_iterator_val;
	Value *from;
	Value *to;
	Type *var_type;

	if (stmt->is_range) {
		from = gen(stmt->range_from);
		to = gen(stmt->range_to);
		var_type = size_t;
	} else {
		from = ConstantInt::get(size_t, 0);

		iterator_val = gen_expr_target(stmt->iterator);
		auto ty = stmt->iterator->type;
		var_type = stmt->var->type->llvm_type;
		Function *f;
		if (ty->isarray()) {
            if (ty->array_is_static) {
                to = ConstantInt::get(size_t, ty->array_size);
            } else {
                f = get_builtin("qwr_array_len");
                to = builder->CreateCall(f, builder->CreateLoad(iterator_val));
            }
		} else {
			f = get_builtin("strlen");
            to = builder->CreateCall(f, builder->CreateLoad(iterator_val));
		}
	}

	auto cond_block = BasicBlock::Create(llvm_context, "", current_function);
	auto body_block = BasicBlock::Create(llvm_context, "", current_function);
	auto inc_block = BasicBlock::Create(llvm_context, "", current_function);
	auto after_block = BasicBlock::Create(llvm_context, "", current_function);

	auto inc_var = builder->CreateAlloca(size_t);
	auto var = stmt->is_range ? inc_var : builder->CreateAlloca(var_type);
	stmt->var->llvm_ref = var;

	builder->CreateStore(from, inc_var);

	builder->CreateBr(cond_block);
	builder->SetInsertPoint(cond_block);

	auto loaded_index = builder->CreateLoad(inc_var);
	auto cmp = builder->CreateICmpSLT(loaded_index, to);
	builder->CreateCondBr(cmp, body_block, after_block);

	builder->SetInsertPoint(body_block);

	if (!stmt->is_range) {
	    loaded_index = builder->CreateLoad(inc_var);
		auto it_ty = stmt->iterator->type;
		if (it_ty->isarray()) {
			auto data_type = stmt->iterator->type->data_type->llvm_type;
			auto var_val = gen_array_indexed(iterator_val, it_ty, loaded_index);

			auto loaded = builder->CreateLoad(var_val);
			builder->CreateStore(loaded, var);
		} else {
			auto var_val = gen_string_indexed(builder->CreateLoad(iterator_val), loaded_index);

			auto loaded = builder->CreateLoad(var_val);
			builder->CreateStore(loaded, var);
		}
	}

	gen(stmt->body);
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

void CodeGenerator::gen_compound(CompoundStmt *stmt) {
    for (auto s : stmt->stmts)
        gen(s);
}

void CodeGenerator::gen_expr_stmt(ExprStmt *stmt) {
    gen(stmt->target_expr);
}

void CodeGenerator::gen_delete(Delete *stmt) {
	auto target_ty = stmt->target_expr->type;

	if (target_ty->isarray()) {
		Function *free_fn = get_builtin("qwr_array_free");
		Value *target = builder->CreateLoad(gen_expr_target(stmt->target_expr));
		builder->CreateCall(free_fn, { target });
	} else {
		Function *free_fn = get_builtin("free");
		auto ptr_ty = typer->make_pointer(typer->get("u8"))->llvm_type;

		Value *target = gen(stmt->target_expr);

		Value *to_free = builder->CreatePointerCast(target, ptr_ty);
		builder->CreateCall(free_fn, { to_free });
	}
}

Value *CodeGenerator::gen(Expr *expr) {
	if (debug)
		emit_location_dbg(expr);

    auto it = expr_gen_funs.find(expr->kind());
	gen_expr_fun fn = it->second;
	return (*this.*fn)(expr);
}

Value *CodeGenerator::gen_binary(Binary *expr) {
	auto rhs = gen(expr->rhs);
	auto top = expr->op;
	Instruction::BinaryOps op;
	CmpInst::Predicate cmpop;
	Value *new_value = 0;
	auto ty = expr->type;
	bool is_ptr = ty->ispointer();

    if (ty->isuint() || is_ptr) {
        switch (top) {
            case '+':
            case TOKEN_ADD_EQ: op = Instruction::BinaryOps::Add; break;
            case '-':
            case TOKEN_SUB_EQ: op = Instruction::BinaryOps::Sub; break;
            case '*':
            case TOKEN_MUL_EQ: op = Instruction::BinaryOps::Mul; break;
            case '/':
            case TOKEN_DIV_EQ: op = Instruction::BinaryOps::UDiv; break;
            case '%':
            case TOKEN_MOD_EQ: op = Instruction::BinaryOps::URem; break;
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
            case TOKEN_ADD_EQ:  op = Instruction::BinaryOps::Add; break;
            case '-':
            case TOKEN_SUB_EQ: op = Instruction::BinaryOps::Sub; break;
            case '*':
            case TOKEN_MUL_EQ: op = Instruction::BinaryOps::Mul; break;
            case '/':
            case TOKEN_DIV_EQ: op = Instruction::BinaryOps::SDiv; break;
            case '%':
            case TOKEN_MOD_EQ: op = Instruction::BinaryOps::SRem; break;
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
		case TOKEN_ADD_EQ: op = Instruction::BinaryOps::FAdd; break;
		case '-':
		case TOKEN_SUB_EQ: op = Instruction::BinaryOps::FSub; break;
		case '*':
		case TOKEN_MUL_EQ: op = Instruction::BinaryOps::FMul; break;
		case '/':
		case TOKEN_DIV_EQ: op = Instruction::BinaryOps::FDiv; break;
		case '%':
		case TOKEN_MOD_EQ: op = Instruction::BinaryOps::FRem; break;
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
	    auto target = gen_expr_target(expr->lhs);
	    builder->CreateStore(new_value, target);
	} else if (ttype_is_binary(top) || top >= TOKEN_ADD_EQ && top <= TOKEN_MOD_EQ) {
		auto lhs = gen(expr->lhs);
	    if (is_ptr) {
	        new_value = builder->CreateInBoundsGEP(expr->type->llvm_type, lhs, rhs);
        } else {
            switch (top) {
                case '|':
                    new_value = builder->CreateOr(lhs, rhs);
                    break;
                case '&':
                    new_value = builder->CreateAnd(lhs, rhs);
                    break;
                case '^':
                    new_value = builder->CreateXor(lhs, rhs);
                    break;
                case TOKEN_SHR:
                    new_value = builder->CreateLShr(lhs, rhs);
                    break;
                case TOKEN_SHL:
                    new_value = builder->CreateShl(lhs, rhs);
                    break;
                default:
                    if (op == BinaryOperator::Add) {
                        new_value = builder->CreateNSWAdd(lhs, rhs);
                    }
                    new_value = builder->CreateBinOp(op, lhs, rhs);
                    break;
            }
        }
        if (top >= TOKEN_ADD_EQ && top <= TOKEN_MOD_EQ) {
	        auto target = gen_expr_target(expr->lhs);
	        builder->CreateStore(new_value, target);
        }
	}  else if (ttype_is_conditional(top)) {
		auto lhs = gen(expr->lhs);
		if (ty->isfloat()) {
			new_value = builder->CreateFCmp(cmpop, lhs, rhs);
		} else {
			new_value = builder->CreateICmp(cmpop, lhs, rhs);
		}
    } else if (ttype_is_logical(top)) {
		auto lhs = gen(expr->lhs);
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

Value *CodeGenerator::gen_cast(Cast *expr) {
	QType *ft = expr->from;
	QType *tt = expr->to;
    QBaseType bf = ft->base;
    QBaseType bt = tt->base;
    auto target = gen(expr->target);
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

	if ((ft->isstring() && tt->ispointer()) || (ft->ispointer() && tt->isstring())) {
		return builder->CreatePointerCast(target, to);
	}
	llvm_unreachable("Cast not implemented");
}

Value *CodeGenerator::gen_unary(Unary *expr) {
    switch (expr->op) {
        case '&': {
            return gen_expr_target(expr->target);
        } break;
        case '!': {
            auto target = gen(expr->target);
            return builder->CreateNot(target);
        } break;
        case '-': {
            auto target = gen(expr->target);
			if (expr->type->isfloat()) {
				return builder->CreateFNeg(target);
			} else {
				return builder->CreateNeg(target);
			}
            
        } break;
		case TOKEN_PLUS_PLUS:
		case TOKEN_MINUS_MINUS: {
			auto target = gen_expr_target(expr->target);
			auto loaded = builder->CreateLoad(target);
			auto type = expr->type->llvm_type;
			Value *one;

			if (expr->op == TOKEN_PLUS_PLUS) {
				one = ConstantInt::get(type, 1);
			} else {
				one = ConstantInt::get(type, -1);
			}

			auto val = builder->CreateNSWAdd(loaded, one);
			builder->CreateStore(val, target);

			if (expr->ispost) {
				return loaded;
			} else {
				return val;
			}
		}
    }

    return 0;
}

Value *CodeGenerator::gen_deref(Deref *expr) {
    auto target = gen(expr->target);
    return builder->CreateLoad(target);
}

Value *CodeGenerator::gen_variable(Variable *expr) {
    if (expr->flags & VAR_PROXY_ENUM) {
        return ConstantInt::get(expr->type->llvm_type, expr->proxy_index);
    }
    if (expr->flags & VAR_PROXY_STRUCT) {
        return gen_member(expr->proxy_member);
    }
	return builder->CreateLoad(expr->llvm_ref);
}

Value *CodeGenerator::gen_int_lit(IntegerLiteral *expr) {
	return ConstantInt::get(expr->type->llvm_type, expr->int_value);
}

Value *CodeGenerator::gen_float_lit(FloatLiteral *expr) {
	return ConstantFP::get(expr->type->llvm_type, expr->float_value);
}

Value *CodeGenerator::gen_string_lit(QStringLiteral *expr) {
    auto it = constant_string_literals.find(expr->string_lit);

    if (it != constant_string_literals.end()) {
        return it->second;
    } 

    auto ptr = builder->CreateGlobalStringPtr(expr->string_lit);
    constant_string_literals.insert(std::make_pair(expr->string_lit, ptr));
    return ptr;
}

Value *CodeGenerator::gen_func_call(FunctionCall *expr) {
    std::vector<Value *> arg_values{};
    auto target_fn = expr->target_func_decl->llvm_ref;

    for (auto arg : expr->arguments)
        arg_values.push_back(gen(arg));

	CallInst *call_inst = builder->CreateCall(target_fn, arg_values);

    return call_inst;
}

Value *CodeGenerator::gen_compare_zero(CompareZero *expr) {
    Value *target = gen(expr->target);
	QType *ty = expr->target->type;
	Value *zero;

	if (ty->ispointer() || ty->isstring()) {
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

Value *CodeGenerator::gen_nil(Nil *expr) {
    return ConstantPointerNull::get((PointerType *) expr->type->llvm_type);
}

Value *CodeGenerator::gen_new(New *expr) {
    Function *malloc_fn = get_builtin("malloc");

    Type *target_type = expr->alloc_type->llvm_type;
    Value *type_size = ConstantInt::get(u64_ty, llvm_size_of(target_type));

    Value *mallocd = builder->CreateCall(malloc_fn, {type_size});
    return builder->CreatePointerCast(mallocd, expr->type->llvm_type);
}

Value *CodeGenerator::gen_member(Member *expr) {
    return builder->CreateLoad(gen_expr_target(expr));
}

Value *CodeGenerator::gen_indexed(Indexed *expr) {
	return builder->CreateLoad(gen_expr_target(expr));
}

Value *CodeGenerator::gen_sizeof(SizeOf *expr) {
    auto ty = expr->type->llvm_type;
    auto size = llvm_size_of(expr->target_type->llvm_type);
    return ConstantInt::get(ty, size);
}

Value *CodeGenerator::gen_compound_lit(CompoundLiteral *expr) {
    return gen_constant_compound_lit(expr, expr->type);
}

Value *CodeGenerator::gen_expr_target(Expr *expr) {
    ExprKind kind = expr->kind();
    if (kind == VARIABLE) {
        auto var = (Variable *) expr;
        if (var->flags & VAR_PROXY_STRUCT) {
            return gen_expr_target(var->proxy_member);
        }
        return var->llvm_ref;
    }

    if (kind == DEREF) {
        auto deref = (Deref *) expr;
        auto target = gen_expr_target(deref->target); 
        auto type = deref->target->type->llvm_type;

        return builder->CreateLoad(target);
    }

	if (kind == INDEXED) {
		auto ind_expr = (Indexed *) expr;
		auto index = ind_expr->index;
		auto target_ty = ind_expr->target->type;
		if (target_ty->isarray()) {
			return gen_array_indexed(gen_expr_target(ind_expr->target), target_ty, gen(index));
		} else if (target_ty->isstring()) {
			return gen_string_indexed(gen(ind_expr->target), gen(index));
		} else {
			auto target = gen(ind_expr->target);

			return builder->CreateInBoundsGEP(target, { gen(index) });
		}
	}

    if (kind == MEMBER) {
	    auto mem_expr = (Member *) expr;
        auto target = gen_expr_target(mem_expr->target);
		auto indices = mem_expr->indices;

		for (int i = 0; i < indices.size(); ++i) {
			auto index = indices[i];
			auto llvm_zero = ConstantInt::get(s32_ty, 0);
			auto llvm_index = ConstantInt::get(s32_ty, index);

			if (mem_expr->dereferences[i]) {
				target = builder->CreateLoad(target);
			}
			target = builder->CreateInBoundsGEP(target, {llvm_zero, llvm_index});
		}

		return target;
    }

	if (kind == CAST) {
	    auto cast_expr = (Cast *) expr;
		return gen_expr_target(cast_expr->target);
	}

    /* unreachable */
    return 0;
}

Value *CodeGenerator::gen_array_indexed(Value *arr, QType *arr_ty, Value *index) {
    if (arr_ty->array_is_static) {
        auto llvm_zero = ConstantInt::get(index->getType(), 0);
        return builder->CreateInBoundsGEP(arr, { llvm_zero, index });
    } else {
        auto data_fn = get_builtin("qwr_array_data");

        auto data_type = arr_ty->data_type->llvm_type;
        Value *target = builder->CreateCall(data_fn, { builder->CreateLoad(arr) });
        target = builder->CreatePointerCast(target, data_type);

        return builder->CreateInBoundsGEP(target, { index });
    }
}

Value *CodeGenerator::gen_string_indexed(Value *str, Value *index) {
	return builder->CreateInBoundsGEP(str, { index });
}

Type *CodeGenerator::gen_return_type(std::vector<QType *> types) {
    if (types.size() == 1)
        return types[0]->llvm_type;

	std::vector<Type *> return_types(types.size());
	for (int i = 0; i < types.size(); i++) {
		return_types[i] = types[i]->llvm_type;
	}

    return StructType::get(llvm_context, return_types);
}

Type *CodeGenerator::gen_return_type(std::vector<Expr *> types) {
	std::vector<Type *> return_types(types.size());
	for (int i = 0; i < types.size(); i++) {
		return_types[i] = types[i]->type->llvm_type;
	}

    return StructType::get(llvm_context, return_types);
}

Value *CodeGenerator::gen_constant_compound_lit_var(CompoundLiteral *expr, QType *var_type) {
    auto constant_val_name = "const_data" + std::to_string(global_constants_count++);

    llvm_module->getOrInsertGlobal(constant_val_name, var_type->llvm_type);
    auto var = llvm_module->getGlobalVariable(constant_val_name);

    var->setConstant(true);
    
    var->setInitializer(gen_constant_compound_lit(expr, var_type));
    
    return var;
}

Constant *CodeGenerator::gen_constant_compound_lit(CompoundLiteral *expr, QType *var_type) {
    auto values = expr->values;
    std::vector<Constant *> constants;
    for (int i = 0; i < values.size(); ++i) {
        constants.push_back((Constant *) gen(values[i]));
    }
    Constant *constant_value;
    if (var_type->isarray()) {
        constant_value = ConstantArray::get((ArrayType *) var_type->llvm_type, constants);
    } else {
        constant_value = ConstantStruct::get((StructType *) var_type->llvm_type, constants);
    }

    return constant_value;
}

void CodeGenerator::emit_location_dbg(Stmt *stmt) {
	auto scope = dbg_scopes.back();
	auto loc = stmt->location;
	builder->SetCurrentDebugLocation(
		DILocation::get(scope->getContext(), loc.line, loc.col_from, scope)
	);
}

void CodeGenerator::emit_location_dbg(Expr *expr) {
	auto scope = dbg_scopes.back();
	auto loc = expr->location;
	builder->SetCurrentDebugLocation(
		DILocation::get(scope->getContext(), loc.line, loc.col_from, scope)
	);
}

DIType *CodeGenerator::convert_type_dbg(QType *type) {
	if (type->base == TYPE_VOID)
		return dbg_builder->createNullPtrType();

	int bit_size = llvm_module->getDataLayout().getTypeSizeInBits(type->llvm_type);

	if (type->ispointer()) {
		return dbg_builder->createPointerType(convert_type_dbg(type->element_type), bit_size, bit_size);
	}

	unsigned encoding = 0;
	if (type->isfloat()) {
		encoding = dwarf::DW_ATE_float;
	} else if (type->ischar()) {
		encoding = dwarf::DW_ATE_unsigned_char;
	} else if (type->isuint()) {
		encoding = dwarf::DW_ATE_unsigned;
	} else if (type->is_int_in_llvm()) {
		encoding = dwarf::DW_ATE_signed;
	}
	return dbg_builder->createBasicType(type->id, bit_size, encoding);
}

int CodeGenerator::llvm_size_of(Type *type) {
    int size = llvm_module->getDataLayout().getTypeSizeInBits(type);    
    return size / 8;
}

int CodeGenerator::llvm_align_of(Type *type) {
    return llvm_module->getDataLayout().getPrefTypeAlignment(type);
}

void CodeGenerator::init_module() {
    InitializeNativeTarget();
    InitializeNativeTargetAsmParser();
    InitializeNativeTargetAsmPrinter();

    Triple triple(sys::getDefaultTargetTriple());

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
	u8ptr_ty = typer->make_pointer(typer->get("u8"))->llvm_type;
	u64_ty = typer->get("u64")->llvm_type;
}

void CodeGenerator::output(Options *options) {
	if (debug) {
		dbg_builder->finalize();
	}

    if (options->flags & OPTIMIZE) {
        optimize();
    }

    std::error_code std_error;
    auto out = new ToolOutputFile(options->obj_file, std_error, sys::fs::OF_None);
    if (!out) {
        std::cerr << "Could not open file " << options->obj_file << "\n";
        return;
    }

    raw_pwrite_stream *os = &out->os();

    legacy::PassManager pm;


#ifdef _WIN32
    auto file_type = TargetMachine::CodeGenFileType::CGFT_ObjectFile;
#else
    auto file_type = CodeGenFileType::CGFT_ObjectFile;
#endif
    if (target_machine->addPassesToEmitFile(pm, *os, nullptr, file_type, false)) {
        std::cerr << options->obj_file << ": target does not support generation of this file type!\n";
        return;
    }

    pm.run(*llvm_module);

    os->flush();
    out->keep();
	delete out;
}

void CodeGenerator::link(Options *options) {
#ifdef _WIN32
	link_windows(options);
#else
	link_unix(options);
#endif
}

void CodeGenerator::link_windows(Options *options) {
	std::stringstream cmd;

	cmd << options->linker;
	cmd << " -out:" << options->exe_file;
	cmd << " \"" << options->obj_file << "\"";

	for (auto lib : options->libs) {
		cmd << " " << lib;
	}

	for (auto linker_flags : options->linker_flags) {
		cmd << " " << linker_flags;
	}

	std::string command = "cmd /C \"" + cmd.str() + "\"";
	std::system(command.c_str());
	std::remove(options->obj_file);

	if (options->flags & VERBOSE) {
		std::cout << command << "\n";
	}
}

void CodeGenerator::link_unix(Options *options) {
    std::stringstream cmd;

	cmd << options->linker;

	cmd << " -o ";
	cmd << options->exe_file << " ";
	cmd << options->obj_file;

	for (auto lib : options->libs) {
		cmd << " -l" << lib;
	}

	for (auto linker_flags : options->linker_flags) {
		cmd << " " << linker_flags;
	}

	std::string command = cmd.str();
	std::system(command.c_str());
	std::remove(options->obj_file);

	if (options->flags & VERBOSE) {
	    std::cout << command << "\n";
    }
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
