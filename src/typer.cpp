#include <string>

#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>

#include "typer.h"

using namespace llvm;

static QType *make_type(QBaseType base, Type *llvm_type);

bool QType::isint() {
	return base >= TYPE_INT8 && base <= TYPE_INT64;
}

bool QType::ispointer() {
	return base == TYPE_POINTER;
}

void Typer::init(LLVMContext *_llvm_context, Messenger *_messenger) {
	llvm_context = _llvm_context;
	messenger = _messenger;

	insert("void", make_type(TYPE_VOID, (Type *) Type::getVoidTy(*llvm_context)));
	insert("i8", make_type(TYPE_INT8, (Type *) Type::getInt8Ty(*llvm_context)));
	insert("i16", make_type(TYPE_INT16, (Type *) Type::getInt16Ty(*llvm_context)));
	insert("i32", make_type(TYPE_INT32, (Type *) Type::getInt32Ty(*llvm_context)));
	insert("i64", make_type(TYPE_INT64, (Type *) Type::getInt64Ty(*llvm_context)));
	insert("int", make_type(TYPE_INT64, (Type *) Type::getInt32Ty(*llvm_context)));
}

void Typer::insert(const char *type_str, QType *type) {
	types.insert({std::string(type_str), type});
}

QType *Typer::make_pointer(QType *type) {
	QType *ptty = make_type(TYPE_POINTER, (Type *) PointerType::get(type->llvm_type, 0));
	ptty->element_type = type;
	return ptty;
}

QType *Typer::get(Token *type_token) {
	auto it = types.find(std::string(type_token->atom.name));
	if (it != types.end()) {
		return it->second;
	}

	messenger->report(type_token, "Unknown type");
	return NULL;
}

QType *Typer::get(const char *type_str) {
	return types[std::string(type_str)];
}

bool Typer::can_convert(QType *from, QType *to) {
    if (compare(from, to))
        return true;

    if (from->isint() && to->isint())
        return true;

    return false;
}

bool Typer::compare(QType *type1, QType *type2) {
    int b1 = type1->base;
    int b2 = type2->base;

    if (b1 == TYPE_POINTER && b2 == TYPE_POINTER)
        return true;

    return b1 == b2;
}

QType *make_type(QBaseType base, Type *llvm_type) {
	auto ty = new QType();
	ty->base = base;
	ty->llvm_type = llvm_type;
	return ty;
}