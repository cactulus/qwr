#include <string>

#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>

#include "typer.h"

using namespace llvm;

bool QType::isint() {
	return base >= TYPE_INT8 && base <= TYPE_UINT64;
}

bool QType::isuint() {
    return base >= TYPE_UINT8 && base <= TYPE_UINT64;
}

bool QType::ispointer() {
	return base == TYPE_POINTER;
}

bool QType::isbool() {
    return base == TYPE_BOOL;
}

bool QType::isenum() {
    return base == TYPE_ENUM;
}

void Typer::init(LLVMContext *_llvm_context, Messenger *_messenger) {
	llvm_context = _llvm_context;
	messenger = _messenger;

	insert_builtin("void", make_type(TYPE_VOID, (Type *) Type::getVoidTy(*llvm_context)));
	insert_builtin("s8", make_type(TYPE_INT8, (Type *) Type::getInt8Ty(*llvm_context)));
	insert_builtin("s16", make_type(TYPE_INT16, (Type *) Type::getInt16Ty(*llvm_context)));
	insert_builtin("s32", make_type(TYPE_INT32, (Type *) Type::getInt32Ty(*llvm_context)));
	insert_builtin("s64", make_type(TYPE_INT64, (Type *) Type::getInt64Ty(*llvm_context)));
	insert_builtin("int", make_type(TYPE_INT32, (Type *) Type::getInt32Ty(*llvm_context)));
	insert_builtin("u8", make_type(TYPE_UINT8, (Type *) Type::getIntNTy(*llvm_context, 8)));
	insert_builtin("u16", make_type(TYPE_UINT16, (Type *) Type::getIntNTy(*llvm_context, 16)));
	insert_builtin("u32", make_type(TYPE_UINT32, (Type *) Type::getIntNTy(*llvm_context, 32)));
	insert_builtin("u64", make_type(TYPE_UINT64, (Type *) Type::getIntNTy(*llvm_context, 64)));
	insert_builtin("uint", make_type(TYPE_UINT32, (Type *) Type::getIntNTy(*llvm_context, 32)));
	insert_builtin("bool", make_type(TYPE_BOOL, (Type *) Type::getInt1Ty(*llvm_context)));
}

void Typer::insert_builtin(const char *type_str, QType *type) {
	types.insert({std::string(type_str), type});
}

void Typer::insert_custom(Token *token, QType *type) {
    auto sname = std::string(token->lexeme);
    if (types.find(sname) != types.end()) {
        messenger->report(token, "Type with this name does already exist");
    }
	types.insert({sname, type});
}

QType *Typer::make_pointer(QType *type) {
	QType *ptty = make_type(TYPE_POINTER, (Type *) PointerType::get(type->llvm_type, 0));
	ptty->element_type = type;
	return ptty;
}

QType *Typer::get(Token *type_token) {
	auto it = types.find(std::string(type_token->lexeme));
	if (it != types.end()) {
		return it->second;
	}

	messenger->report(type_token, "Unknown type");
	return NULL;
}

QType *Typer::get(const char *type_str) {
	return types[std::string(type_str)];
}

bool Typer::has(const char *type_str) {
	auto it = types.find(std::string(type_str));
    return it != types.end();
}

bool Typer::can_convert(QType *from, QType *to) {
    if (compare(from, to))
        return true;

    if (from->isint() && to->isint())
        return true;

    if (from->isint() && to->isbool()) {
        return true;
    }

    if (from->isbool() && to->isint()) {
        return true;
    }

    return false;
}

bool Typer::compare(QType *type1, QType *type2) {
    int b1 = type1->base;
    int b2 = type2->base;

    if (b1 == TYPE_POINTER && b2 == TYPE_POINTER)
        return true;

    return b1 == b2;
}

QType *Typer::make_type(QBaseType base, Type *llvm_type) {
	auto ty = new QType();
	ty->base = base;
	ty->llvm_type = llvm_type;
	return ty;
}
