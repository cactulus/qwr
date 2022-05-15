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

	make_type_intern("void", TYPE_VOID, (Type *) Type::getVoidTy(*llvm_context));
	make_type_intern("i8", TYPE_INT8, (Type *) Type::getInt8Ty(*llvm_context));
	make_type_intern("s16", TYPE_INT16, (Type *) Type::getInt16Ty(*llvm_context));
	make_type_intern("s32", TYPE_INT32, (Type *) Type::getInt32Ty(*llvm_context));
	make_type_intern("s64", TYPE_INT64, (Type *) Type::getInt64Ty(*llvm_context));
	make_type_intern("u8", TYPE_UINT8, (Type *) Type::getIntNTy(*llvm_context, 8));
	make_type_intern("u16", TYPE_UINT16, (Type *) Type::getIntNTy(*llvm_context, 16));
	make_type_intern("u32", TYPE_UINT32, (Type *) Type::getIntNTy(*llvm_context, 32));
	make_type_intern("u64", TYPE_UINT64, (Type *) Type::getIntNTy(*llvm_context, 64));
	make_type_intern("f16", TYPE_F16, (Type *)Type::getHalfTy(*llvm_context));
	make_type_intern("f32", TYPE_F32, (Type *)Type::getFloatTy(*llvm_context));
	make_type_intern("f64", TYPE_F64, (Type *)Type::getDoubleTy(*llvm_context));
	make_type_intern("int", TYPE_INT32, (Type *)Type::getInt32Ty(*llvm_context));
	make_type_intern("uint", TYPE_UINT32, (Type *) Type::getIntNTy(*llvm_context, 32));
	make_type_intern("float", TYPE_F32, (Type *)Type::getFloatTy(*llvm_context));
	make_type_intern("bool", TYPE_BOOL, (Type *) Type::getInt1Ty(*llvm_context));
	make_type_intern("char", TYPE_CHAR, (Type *) Type::getIntNTy(*llvm_context, 8));

	auto char_ty = get("char");
	auto char_ptr = make_pointer(char_ty);
	QType *str_ty = make_type_intern("str", TYPE_STRING, char_ptr->llvm_type);
	str_ty->element_type = char_ty;

	auto u8_ty = get("u8");
	auto u8ptr_ty = make_pointer(u8_ty);
	auto ptr_ty = make_type_intern("ptr", TYPE_POINTER, u8ptr_ty->llvm_type);
	ptr_ty->element_type = u8_ty;
}

QType *Typer::make_pointer(QType *type) {
	auto id = "*" + type->id;
	if (has(id)) {
		return get(id);
	}

	QType *ptty = make_type_intern(id, TYPE_POINTER, (Type *) PointerType::get(type->llvm_type, 0));
	ptty->element_type = type;
	return ptty;
}

QType *Typer::make_array(QType *type) {
	auto id = "Array_" + type->id;
	if (has(id)) {
		return get(id);
	}

	QType *ptty = make_type_intern(id, TYPE_ARRAY, make_pointer(get("Array"))->llvm_type);
	ptty->element_type = type;
	ptty->data_type = make_pointer(type);
	return ptty;
}

QType *Typer::make_struct(const char *name, struct_fields_type *fields) {
	auto id = std::string(name);
	if (has(id)) {
		return get(id);
	}

    std::vector<Type *> llvm_fields;

    for (auto field : *fields) {
        llvm_fields.push_back(field.second->llvm_type);
    }

    auto sty = StructType::create(*llvm_context, name);
    sty->setBody(llvm_fields);
    
    auto ty = make_type_intern(id, TYPE_STRUCT, sty);
	ty->struct_name = name;
    ty->fields = fields;

    return ty;
}

QType *Typer::make_type(Token *token, QBaseType base, Type *llvm_type) {
	auto sname = std::string(token->lexeme);
	if (types.find(sname) != types.end()) {
		messenger->report(token, "Type with this name does already exist");
	}

	return make_type_intern(token->lexeme, base, llvm_type);
}

QType *Typer::make_type_intern(const std::string &id, QBaseType base, llvm::Type *llvm_type) {
	auto ty = new QType();
	ty->base = base;
	ty->id = id;
	ty->llvm_type = llvm_type;

	types.insert({ id, ty });

	return ty;
}

void Typer::make_ref_type(Token *token, QType *ref) {
	auto sname = std::string(token->lexeme);
	if (types.find(sname) != types.end()) {
		messenger->report(token, "Type with this name does already exist");
	}

	types.insert({ sname, ref });
}

QType *Typer::get(Token *type_token) {
	auto it = types.find(std::string(type_token->lexeme));
	if (it != types.end()) {
		return it->second;
	}

	messenger->report(type_token, "Unknown type");
	return NULL;
}

QType *Typer::get(const std::string &id) {
	return types[id];
}

QType *Typer::get_array(QType *element_type) {
	return get("Array_" + element_type->id);
}

bool Typer::has(const std::string &id) {
	auto it = types.find(id);
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
