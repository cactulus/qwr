#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Module.h>

#include "llvmgen.h"
#include "parser.h"
#include "builtin.h"

#define BUILTIN(e, x) (strcmp(expr->builtin.name, x) == 0)

using namespace llvm;

static std::unordered_map<std::string, QType *> builtin_functions = {};

void init_builtins(Typer *typer) {
	builtin_functions.insert({ "append", typer->get("void") });
	builtin_functions.insert({ "len", typer->get("u64") });
}

bool is_builtin(const char *name) {
	return builtin_functions.find(std::string(name)) != builtin_functions.end();
}

QType *get_builtin_return_type(const char *name) {
	return builtin_functions[std::string(name)];
}

void Parser::check_builtin_func(Token *token, Expr *expr) {
	auto args = *expr->builtin.arguments;
	if (BUILTIN(expr, "len")) {
		auto ty = args[0]->type;
		if (args.size() > 1 || args.size() < 1) {
			messenger->report(token, "Expected one argument of type string or array");
		}
		if (!ty->isarray() && !ty->isstring()) {
			messenger->report(token, "Expected one argument of type string or array");
		}
		return;
	}

	if (BUILTIN(expr, "append")) {
		if (args.size() > 2 || args.size() < 2) {
			messenger->report(token, "Expected two arguments. Array and value");
		}
		if (!args[0]->type->isarray()) {
			messenger->report(token, "Expected first argument to be of type array");
		}

		auto arr_ty = args[0]->type->element_type;
		auto val_ty = args[1]->type;
		if (!typer->compare(arr_ty, val_ty)) {
			if (typer->can_convert_implicit(val_ty, arr_ty)) {
				(*expr->builtin.arguments)[1] = cast(args[1], arr_ty);
			} else {
				messenger->report(token, "Provided value does not match element type of array");
			}
		}
		return;
	}

	assert(0 && "Not implemented builtin function");
}

Value *CodeGenerator::gen_builtin(Expr *expr) {
	auto args = *expr->builtin.arguments;

	if (BUILTIN(expr, "len")) {
		auto ty = args[0]->type;
		if (ty->isarray()) {
			auto target = builder->CreateLoad(gen_expr_target(args[0]));
			auto fn = get_builtin("qwr_array_len");
			return builder->CreateCall(fn, { target });
		} else {
			auto target = builder->CreateLoad(gen_expr_target(args[0]));
			auto fn = get_builtin("strlen");
			return builder->CreateCall(fn, { target });
		}
	}

	if (BUILTIN(expr, "append")) {
		auto arr = args[0];
		auto arr_ty = arr->type->element_type;

		auto mangled_type = typer->mangle_type(arr_ty);
		auto fn_name = "qwr_array_append_" + mangled_type;
		auto fn = llvm_module->getFunction(fn_name);

		if (!fn) {
			fn = gen_append_func(fn_name, arr_ty);
		}

		auto loaded_arr = gen_expr(arr);
		auto value = gen_expr(args[1]);

		builder->CreateCall(fn, { loaded_arr, value });
	}

	return 0;
}

Function *CodeGenerator::get_builtin(const char *name) {
	return llvm_module->getFunction(name);
}

Function *CodeGenerator::gen_append_func(std::string name, QType *value_type) {
	auto ret_type = typer->get("void")->llvm_type;
	auto array_type = typer->get_array(value_type)->llvm_type;
	auto value_ptr_type = typer->make_pointer(value_type)->llvm_type;
	auto u64_ty = typer->get("u64")->llvm_type;
	auto s32_ty = typer->get("s32")->llvm_type;

	auto fun_type = FunctionType::get(ret_type, { array_type, value_type->llvm_type }, false);
	auto linkage = Function::ExternalLinkage;

	auto fn = Function::Create(fun_type, linkage, name, llvm_module);

	auto entry = BasicBlock::Create(llvm_context, "", fn);
	auto old_insert_point = builder->GetInsertBlock();
	builder->SetInsertPoint(entry);

	auto resize_fn = get_builtin("qwr_array_resize");
	auto type_size_llvm = ConstantInt::get(u64_ty, llvm_size_of(value_type->llvm_type));

	auto arr_ref = fn->arg_begin();
	auto value = fn->arg_begin() + 1;

	builder->CreateCall(resize_fn, { arr_ref, type_size_llvm });

	auto llvm_zero = ConstantInt::get(s32_ty, 0);
	auto llvm_one = ConstantInt::get(s32_ty, 1);
	auto llvm_one_u64 = ConstantInt::get(u64_ty, 1);
	
	auto data_ptr = builder->CreateInBoundsGEP(arr_ref, { llvm_zero, llvm_zero });
	data_ptr = builder->CreatePointerCast(builder->CreateLoad(data_ptr), value_ptr_type);
	auto len_ptr = builder->CreateInBoundsGEP(arr_ref, { llvm_zero, llvm_one });
	auto len = builder->CreateLoad(len_ptr);

	auto value_loc = builder->CreateInBoundsGEP(data_ptr, len);
	builder->CreateStore(value, value_loc);
	builder->CreateStore(builder->CreateAdd(len, llvm_one_u64), len_ptr);

	builder->CreateRetVoid();
	builder->SetInsertPoint(old_insert_point);
	return fn;
}
