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

bool QType::isfloat() {
	return base >= TYPE_F16 && base <= TYPE_F64;
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

bool QType::isstruct() {
    return base == TYPE_STRUCT;
}

bool QType::ischar() {
    return base == TYPE_CHAR;
}

bool QType::isstring() {
    return base == TYPE_STRING;
}

bool QType::isarray() {
	return base == TYPE_ARRAY;
}

bool QType::is_int_in_llvm() {
	return isint() || ischar() || isbool();
}

void Typer::init(LLVMContext *_llvm_context, Messenger *_messenger) {
	llvm_context = _llvm_context;
	messenger = _messenger;

	insert_builtin("void", make_type(TYPE_VOID, (Type *) Type::getVoidTy(*llvm_context)));
	insert_builtin("s8", make_type(TYPE_INT8, (Type *) Type::getInt8Ty(*llvm_context)));
	insert_builtin("s16", make_type(TYPE_INT16, (Type *) Type::getInt16Ty(*llvm_context)));
	insert_builtin("s32", make_type(TYPE_INT32, (Type *) Type::getInt32Ty(*llvm_context)));
	insert_builtin("s64", make_type(TYPE_INT64, (Type *) Type::getInt64Ty(*llvm_context)));
	insert_builtin("u8", make_type(TYPE_UINT8, (Type *) Type::getIntNTy(*llvm_context, 8)));
	insert_builtin("u16", make_type(TYPE_UINT16, (Type *) Type::getIntNTy(*llvm_context, 16)));
	insert_builtin("u32", make_type(TYPE_UINT32, (Type *) Type::getIntNTy(*llvm_context, 32)));
	insert_builtin("u64", make_type(TYPE_UINT64, (Type *) Type::getIntNTy(*llvm_context, 64)));
	insert_builtin("f16", make_type(TYPE_F16, (Type *)Type::getHalfTy(*llvm_context)));
	insert_builtin("f32", make_type(TYPE_F32, (Type *)Type::getFloatTy(*llvm_context)));
	insert_builtin("f64", make_type(TYPE_F64, (Type *)Type::getDoubleTy(*llvm_context)));
	insert_builtin("int", make_type(TYPE_INT32, (Type *)Type::getInt32Ty(*llvm_context)));
	insert_builtin("uint", make_type(TYPE_UINT32, (Type *) Type::getIntNTy(*llvm_context, 32)));
	insert_builtin("float", make_type(TYPE_F32, (Type *)Type::getFloatTy(*llvm_context)));
	insert_builtin("bool", make_type(TYPE_BOOL, (Type *) Type::getInt1Ty(*llvm_context)));
	insert_builtin("char", make_type(TYPE_CHAR, (Type *) Type::getIntNTy(*llvm_context, 8)));

	auto char_ty = get("char");
	auto char_ptr = make_pointer(char_ty);
	QType *str_ty = make_type(TYPE_STRING, char_ptr->llvm_type);
	str_ty->element_type = char_ty;
	types.insert({ "str", str_ty});

	auto u8_ty = get("u8");
	auto u8ptr_ty = make_pointer(u8_ty);
	types.insert({ "ptr", u8ptr_ty });
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

QType *Typer::make_array(QType *type) {
	QType *ptty = make_type(TYPE_ARRAY, make_pointer(get("Array"))->llvm_type);
	ptty->element_type = type;
	ptty->data_type = make_pointer(type);
	return ptty;
}

QType *Typer::make_struct(const char *name, struct_fields_type *fields) {
    std::vector<Type *> llvm_fields;

    for (auto field : *fields) {
        llvm_fields.push_back(field.second->llvm_type);
    }

    auto sty = StructType::create(*llvm_context, name);
    sty->setBody(llvm_fields);
    
    auto ty = make_type(TYPE_STRUCT, sty);
	ty->struct_name = name;
    ty->fields = fields;

    return ty;
}

QType *Typer::make_type(QBaseType base, Type *llvm_type) {
	auto ty = new QType();
	ty->base = base;
	ty->llvm_type = llvm_type;
	return ty;
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

bool Typer::can_convert_implicit(QType *from, QType *to) {
    if (compare(from, to))
        return true;

	if (from->isfloat() && to->isfloat())
		return true;

	if (from->isint() && to->isint())
		return true;

	if (from->isint() && to->isbool())
		return true;

	if (from->isbool() && to->isint())
		return true;

    return false;
}

bool Typer::can_convert_explicit(QType *from, QType *to) {
	if (can_convert_implicit(from, to))
		return true;

	bool from_is_primitive = (from->base >= TYPE_INT8) && (from->base <= TYPE_CHAR);
	bool to_is_primitive = (to->base >= TYPE_INT8) && (to->base <= TYPE_CHAR);

	return from_is_primitive && to_is_primitive;
}

bool Typer::compare(QType *type1, QType *type2) {
    int b1 = type1->base;
    int b2 = type2->base;

    if (b1 == TYPE_POINTER && b2 == TYPE_POINTER)
        return true;

    return b1 == b2;
}

std::string Typer::mangle_type(QType *type) {
	switch (type->base) {
		case TYPE_INT8: return "s8";
		case TYPE_INT16: return "s16";
		case TYPE_INT32: return "s32";
		case TYPE_INT64: return "s64";
		case TYPE_UINT8: return "u8";
		case TYPE_UINT16: return "u16";
		case TYPE_UINT32: return "u32";
		case TYPE_UINT64: return "u64";
		case TYPE_F16: return "f16";
		case TYPE_F32: return "f32";
		case TYPE_F64: return "f64";
		case TYPE_BOOL: return "b";
		case TYPE_CHAR: return "c";
		case TYPE_STRING: return "s";
	}

	if (type->isstruct()) {
		return type->struct_name;
	}
	if (type->isarray()) {
		return "a" + mangle_type(type->element_type);
	}
	if (type->ispointer()) {
		return "p" + mangle_type(type->element_type);
	}

	assert(0 && "Tried to call mangle_type on type that is not implemented");
	return 0;
}