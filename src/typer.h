#ifndef TYPE_H_
#define TYPE_H_

#include <unordered_map>

#include "lexer.h"
#include "messenger.h"

struct Stmt;

namespace llvm {
	class Type;
	class PointerType;
	class LLVMContext;
};

enum QBaseType {
	TYPE_VOID,
	TYPE_POINTER,
	TYPE_INT8,
	TYPE_INT16,
	TYPE_INT32,
	TYPE_INT64,
	TYPE_UINT8,
	TYPE_UINT16,
	TYPE_UINT32,
	TYPE_UINT64,
	TYPE_BOOL,
};

struct QType {
	QBaseType base;
	QType *element_type;
	llvm::Type *llvm_type;

	bool isint();
	bool isuint();
	bool ispointer();
    bool isbool();
};

struct Typer {
    /* map from type name to type object */
	std::unordered_map<std::string, QType *> types;
	llvm::LLVMContext *llvm_context;
	Messenger *messenger;

	void init(llvm::LLVMContext *_llvm_context, Messenger *_messenger);
	
	void insert(const char *type_str, QType *type);
	QType *make_pointer(QType *type);

	QType *get(Token *type_token);
	QType *get(const char *type_str);

	bool can_convert(QType *from, QType *to);
	bool compare(QType *type1, QType *type2);
};

#endif