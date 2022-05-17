#ifndef TYPE_H_
#define TYPE_H_

#include <tuple>
#include <string>
#include <unordered_map>

#include "lexer.h"
#include "messenger.h"

struct Stmt;

namespace llvm {
	class Type;
	class PointerType;
	class LLVMContext;
};

enum QBaseType { /* order is very important */
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
	TYPE_F16,
	TYPE_F32,
	TYPE_F64,
	TYPE_BOOL,
	TYPE_CHAR,
	TYPE_ENUM,
	TYPE_STRUCT,
	TYPE_STRING,
	TYPE_ARRAY,
	TYPE_NIL,
};

struct QType;
typedef std::vector<std::pair<const char *, QType *>> struct_fields_type;

struct QType {
	QBaseType base;
	llvm::Type *llvm_type;
	std::string id;

	union {
		struct {
        	std::vector<const char *> *categories;
			std::vector<unsigned int> *indices;
		};

		struct {
			QType *element_type;
			QType *data_type;
		};

		struct {
			const char *struct_name;
			struct_fields_type *fields;
		};
    };

	bool isint();
	bool isuint();
	bool isfloat();
	bool ispointer();
    bool isbool();
    bool isenum();
    bool isstruct();
    bool ischar();
    bool isstring();
	bool isarray();

	/* returns true if in the end, it is an int type in llvm */
	bool is_int_in_llvm();
};

struct Typer {
    /* map from type name to type object */
	std::unordered_map<std::string, QType *> types;
	llvm::LLVMContext *llvm_context;
	Messenger *messenger;

	void init(llvm::LLVMContext *_llvm_context, Messenger *_messenger);
	
	QType *make_pointer(QType *type);
	QType *make_array(QType *type);
	QType *make_struct(const char *name, struct_fields_type *fields);
	QType *make_nil();
	QType *make_type(Token *token, QBaseType base, llvm::Type *llvm_type);
	QType *make_type_intern(const std::string &id, QBaseType base, llvm::Type *llvm_type);
	void make_ref_type(Token *token, QType *ref);

	QType *get(Token *type_token);
	QType *get(const std::string &id);
	QType *get_array(QType *element_type);

	bool has(const std::string &id);

	bool can_convert_implicit(QType *from, QType *to);
	bool can_convert_explicit(QType *from, QType *to);
	bool compare(QType *type1, QType *type2);

	std::string mangle_type(QType *type);
};

#endif
