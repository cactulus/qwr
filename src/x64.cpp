/*
Inspired by the Bitwise "Programming an x64 compiler from scratch" live streams
*/
#include <cassert>
#include <iostream>
#include <cstdint>
#include <unordered_map>

#include "options.h"
#include "parser.h"
#include "x64.h"

#if _WIN32

#define MAX_CODE_LEN 1024

typedef uint8_t u8;
typedef uint32_t u32;

enum Reg {
	RAX = 0x0,
	RCX = 0x1,
	RDX = 0x2,
	RBX = 0x3,
	RSP = 0x4,
	RBP = 0x5,
	RSI = 0x6,
	RDI = 0x7,
	R8 = 0x8,
	R9 = 0x9,
	R10 = 0x10,
	R11 = 0x11,
	R12 = 0x12,
	R13 = 0x13,
	R14 = 0x14,
	R15 = 0x15,
};

enum Op {
	ADD = 0x03,
	MOV_REG_RM = 0x8b,
	MOV_REG_IMM = 0xb8,
};

enum Mode {
	INDIRECT = 0x0,
	INDIRECT_BYTE_DISPLACED = 0x1,
	INDIRECT_DISPLACED = 0x2,
	DIRECT = 0x3
};

enum Scale {
	X1 = 0x0,
	X2 = 0x1,
	X4 = 0x2,
	X8 = 0x3,
};

enum OperandType {
	OPERAND_NULL,
	OPERAND_FRAME_OFFSET,
	OPERAND_REGISTER,
	OPERAND_IMMEDIATE,
	OPERAND_ADDRESS
};

struct Operand {
	OperandType type;
	union {
		u8 reg;
		u32 val;
		u32 offset;
		u32 address;
	};
};

static void gen_expr(Operand *operand, Expr *expr);
static void emit_add(Operand *op1, Operand *op2);
static void emit_sub(Operand *op1, Operand *op2);

static u32 free_register_mask;
static u8 code[MAX_CODE_LEN];
static u8 *code_ptr;

static std::unordered_map<std::string, u32> variable_offset_map = {};
static u32 local_var_count = 0;

void emit(u8 byte) {
	assert(code_ptr - code <= MAX_CODE_LEN);
	*code_ptr = byte;
	code_ptr++;
}

void emit_word(u32 word) {
	emit(word & 0xFF);
	emit((word >> 8) & 0xFF);
	emit((word >> 16) & 0xFF);
	emit((word >> 24) & 0xFF);
}

void emit_modrm(u8 mod, u8 rx, u8 rm) {
	emit((rm &= 7) | ((rx &= 7) << 3) | (mod << 6));
}

void emit_direct(u8 rx, u8 rm) {
	emit_modrm(DIRECT, rx, rm);
}

void emit_indirect(u8 rx, u8 rm) {
	emit_modrm(INDIRECT, rx, rm);
}

void emit_byte_displaced(u8 rx, u8 rm, u8 displacement) {
	emit_modrm(INDIRECT_BYTE_DISPLACED, rx, rm);
	emit(displacement);
}

void emit_displaced(u8 rx, u8 rm, u32 displacement) {
	emit_modrm(INDIRECT_DISPLACED, rx, rm);
	emit_word(displacement);
}

void emit_indirect_indexed(u8 rx, u8 rm, u8 index, u8 scale) {
	emit_modrm(INDIRECT, rx, RSP);
	emit_modrm(scale, index, rm);
}

void emit_byte_displaced_indexed(u8 rx, u8 rm, u8 index, u8 scale, u8 displacement) {
	emit_modrm(INDIRECT_DISPLACED, rx, 4);
	emit_modrm(scale, rm, index);
	emit(displacement);
}

void emit_displaced_indexed(u8 rx, u8 rm, u8 index, u8 scale, u32 displacement) {
	emit_modrm(INDIRECT_DISPLACED, rx, 4);
	emit_modrm(scale, rm, index);
	emit_word(displacement);
}

void emit_rex(u8 rx, u8 rm, u8 index) {
	emit(0x48 | (rm >> 3) | ((index >> 3) << 1) | ((rx >> 3) << 2));
}

void emit_rex2(u8 rx, u8 rm) {
	emit(0x48 | (rm >> 3) | ((rx >> 3) << 2));
}

#define EMIT_OP(operation) \
    emit_##operation##(); \

#define EMIT_R(operation, reg) \
    emit_##operation##(reg); \

#define EMIT_R_R(operation, dest, source) \
    emit_rex2(dest, source); \
    emit_##operation##_r(); \
    emit_direct(dest, source)

#define EMIT_R_M(operation, destination, source) \
    emit_rex2(destination, source); \
    emit_##operation##_r(); \
    emit_indirect(destination, source)

#define EMIT_R_D(operation, destination, source) \
    emit_rex2(dest, 0x0); \
    emit_##operation##_r(); \
    emit_displaced(dest, source)

#define EMIT_D_R(operation, destination, source) \
    emit_rex2(source, 0x0); \
    emit_##operation##_m(); \
    emit_displaced(source, dest)

#define EMIT_D_I(operation, destination, source_immediate) \
    emit_rex2(0x0, 0x0); \
    emit_##operation##_i(); \
    emit_displaced(extension_##operation##_I, destination); \
    emit_word(source_immediate)

#define EMIT_R_MD1(operation, dest, source, disp) \
    emit_rex2(dest, source); \
    emit_##operation##_r(); \
    emit_byte_displaced(dest, source, disp)

#define EMIT_R_MD(operation, dest, source, disp) \
    emit_rex2(dest, source); \
    emit_##operation##_r(); \
    emit_displaced(dest, source, disp)

#define EMIT_R_SIB(operation, destination, source_base, source_scale, source_index) \
    emit_rex(destination, source_base, source_index); \
    emit_##operation##_r(); \
    emit_indirect_indexed(destination, source_base, source_index, source_scale)

#define EMIT_R_SIBD1(operation, destination, source_base, source_scale, source_index, displacement) \
    emit_rex(destination, source_base, source_index); \
    emit_##operation##_r(); \
    emit_byte_displaced_indexed(destination, source_base, source_index, source_scale, displacement)

#define EMIT_R_SIBD(operation, destination, source_base, source_scale, source_index, displacement) \
    emit_rex(destination, source_base, source_index); \
    emit_##operation##_r(); \
    emit_displaced_indexed(destination, source_base, source_index, source_scale, displacement)

#define EMIT_M_R(operation, destination, source) \
    emit_rex2(source, destination); \
    emit_##operation##_m(); \
    emit_indirect(source, destination);

#define EMIT_MD1_R(operation, destination, displacement, source) \
    emit_rex2(source, destination); \
    emit_##operation##_m(); \
    emit_byte_displaced(source, destination, displacement);

#define EMIT_MD_R(operation, destination, displacement, source) \
    emit_rex2(source, destination); \
    emit_##operation##_m(); \
    emit_displaced(source, destination, displacement);

#define EMIT_SIB_R(operation, destination_base, destination_scale, destination_index, source) \
    emit_rex(source, destination_base, destination_index); \
    emit_##opeartion##_m(); \
    emit_indirect_indexed(source, destination_base, destination_index, destination_scale)

#define EMIT_SIBD1_R(operation, destination_base, destination_scale, destination_index, destination_displacement, source) \
    emit_rex(source, destination_base, destination_index); \
    emit_##opeartion##_m(); \
    emit_byte_displaced_indexed(source, destination_base, destination_index, destination_scale, destination_displacement)

#define EMIT_SIBD_R(operation, destination_base, destination_scale, destination_index, destination_displacement, source) \
    emit_rex(source, destination_base, destination_index); \
    emit_##opeartion##_m(); \
    emit_displaced_indexed(source, destination_base, destination_index, destination_scale, destination_displacement)

#define EMIT_R_I(operation, destination, source_immediate) \
    emit_rex2(0x0, destination); \
    emit_##operation##_i(); \
    emit_direct(extension_##operation##_i, destination); \
    emit_word(source_immediate)

#define EMIT_R_I1(operation, destination, source_immediate) \
    emit_rex2(0x0, destination); \
    emit_##operation##_i1(); \
    emit_direct(extension_##operation##_i1, destination); \
    emit_word(source_immediate)

#define EMIT_M_I(operation, destination, source_immediate) \
    emit_rex2(0x0, destination); \
    emit_##operation##_i(); \
    emit_indirect(extension_##operation##_i, destination); \
    emit_word(source_immediate)

#define EMIT_MD1_I(operation, destination, destination_displacement, source_immediate) \
    emit_rex2(0x0, destination); \
    emit_##operation##_i(); \
    emit_byte_displaced(extension_##operation##_i, destination, destination_displacement); \
    emit_word(source_immediate)

#define EMIT_MD_I(operation, destination, destination_displacement, source_immediate) \
    emit_rex2(0x0, destination); \
    emit_##operation##_i(); \
    emit_displaced(extension_##operation##_i, destination, destination_displacement); \
    emit_word(source_immediate)

#define EMIT_SIB_I(operation, destination_base, destination_scale, destination_index, source_immediate) \
    emit_rex(0x0, destination_base, destination_index); \
    emit_##operation##_i(); \
    emit_indirect_indexed(extension_##operation##_i, destination_base, destination_index, destination_scale); \
    emit_word(source_immediate)

#define EMIT_SIBD1_I(operation, destination_base, destination_scale, destination_index, destination_displacement, source_immediate) \
    emit_rex(0x0, destination_base, destination_index); \
    emit_##operation##_i(); \
    emit_byte_displaced_indexed(extension_##operation##_i, destination_base, destination_index, destination_scale, destination_displacement); \
    emit_word(source_immediate)

#define EMIT_SIBD_I(operation, destination_base, destination_scale, destination_index, destination_displacement, source_immediate) \
    emit_rex(0x0, destination_base, destination_index); \
    emit_##operation##_i(); \
    emit_displaced_indexed(extension_##operation##_i, destination_base, destination_index, destination_scale, destination_displacement); \
    emit_word(source_immediate)

#define EMIT_X_R(operation, source) \
    emit_rex2(0x0, source); \
    emit_##operation##_x(); \
    emit_direct(extension_##operation##_x, source)

#define EMIT_X_M(operation, source) \
    emit_rex2(0x0, source); \
    emit_##operation##_x(); \
    emit_indirect(extension_##operation##_x, source)

#define EMIT_X_D(operation, source) \
    emit_rex2(0x0, 0x0); \
    emit_##operation##_x(); \
    emit_displaced(extension_##opeartion##_x, source)

#define EMIT_X_MD1(operation, source, disp) \
    emit_rex2(0x0, source); \
    emit_##operation##_x(); \
    emit_byte_displaced(extension_##operation##_x, source, disp)

#define EMIT_X_MD(operation, source, disp) \
    emit_rex2(0x0, source); \
    emit_##operation##_x(); \
    emit_displaced(extension_##operation##_x, source, disp)

#define EMIT_X_SIB(operation, source_base, source_scale, source_index) \
    emit_rex(0x0, source_base, source_index); \
    emit_##operation##_x(); \
    emit_indirect_indexed(extension_##operation##_x, source_base, source_index, source_scale)

#define EMIT_X_SIBD1(operation, source_base, source_scale, source_index, displacement) \
    emit_rex(0x0, source_base, source_index); \
    emit_##operation##_x(); \
    emit_byte_displaced_indexed(extension_##operation##_x, source_base, source_index, source_scale, displacement)

#define EMIT_X_SIBD(operation, source_base, source_scale, source_index, displacement) \
    emit_rex(0x0, source_base, source_index); \
    emit_##operation##_x(); \
    emit_displaced_indexed(extension_##operation##_x, source_base, source_index, source_scale, displacement)

#define EMIT_I(operation, source_immediate) \
    emit_rex2(0x0, 0x0); \
    emit_##operation##_i(); \
    emit_word(source_immediate);

#define EMIT_C_I(operation, condition_code, source_immediate) \
    emit_##operation##_c_i(condition_code); \
    emit_word(source_immediate);

#define OP(operation, opcode) \
    void emit_##operation##() { \
        emit(opcode); \
    }

#define OP1R(operation, opcode) \
    void emit_##operation##_r() { \
        emit(opcode); \
    }

#define OP1M(operation, opcode) \
    void emit_##operation##_m() { \
        emit(opcode); \
    }

#define OP1I(operation, opcode, extension) \
    void emit_##operation##_i() { \
        emit(opcode); \
    } \
    enum { extension_##operation##_i = extension };

#define OP1I1(operation, opcode, extension) \
    void emit_##operation##_i1() { \
        emit(opcode); \
    } \
    enum { extension_##operation##_i1 = extension };

#define OP1X(operation, opcode, extension) \
    void emit_##operation##_x() { \
        emit(opcode); \
    } \
    enum { extension_##operation##_x = extension };

#define OP1CI(operation, opcode, extension) \
    void emit_##operation##_c_i(u32 code) { \
        emit(opcode + code); \
    } 

#define OP2CI(operation, opcode) \
    void emit_##operation##_c_i(u32 code) { \
        emit(0x0F); \
        emit(opcode + code); \
    } 

#define OP1R1(operation, opcode) \
    void emit_##operation##(u32 code) { \
        emit(opcode + code); \
    } 

OP1M(mov, 0x8B)
OP1R(mov, 0x89)
OP1I(movsx, 0xC7, 0x00)

OP1R(add, 0x03)
OP1M(add, 0x01)
OP1I(add, 0x81, 0x00)

OP1R(lea, 0x8D);
OP1R(xor, 0x33)

OP(ret, 0xC3);

OP1R1(push, 0x50)
OP1R1(pop, 0x58)

OP1R(sub, 0x2B)
OP1M(sub, 0x29)
OP1I(sub, 0x81, 0x05)

OP1X(mul, 0xF7, 0x04)
OP1X(div, 0xF7, 0x06)

OP1I(cmp, 0x81, 0x07)
OP1I(jmp, 0xE9, 0x00)
OP1I(je, 0x84, 0x00)
OP2CI(J, 0x80)

u8 alloc_reg() {
	u8 free_register = 0;

	while (!(free_register_mask & (1 << free_register))) {
		free_register++;
	}

	free_register_mask &= ~(1 << free_register);
	return free_register;
}

void operand_alloc_reg(Operand *op) {
	if (op->type != OPERAND_REGISTER) {
		op->type = OPERAND_REGISTER;
		op->reg = alloc_reg();
	}
}

void free_reg(u8 reg) {
	free_register_mask |= 1 << reg;
}

void operand_free_reg(Operand *op) {
	if (op->type == OPERAND_REGISTER) {
		free_reg(op->reg);
	}
	op->type = OPERAND_NULL;
}

void move_operand_reg(Operand* op, u8 reg) {
	if (op->type == OPERAND_IMMEDIATE) {
		EMIT_R_I(movsx, reg, op->val);
	} else if (op->type == OPERAND_FRAME_OFFSET) {
		EMIT_MD1_R(mov, RBP, -op->offset, reg);
	} else if (op->type == OPERAND_ADDRESS) {
		EMIT_R_M(mov, reg, op->address);
	} else if (op->type == OPERAND_REGISTER) {
		if (op->reg != reg) {
			EMIT_R_R(mov, reg, op->reg);
		}
	}
	op->type = OPERAND_REGISTER;
	op->reg = reg;
}

void emit_as_register(Operand* op) {
	if (op->type != OPERAND_REGISTER) {
		u8 reg = alloc_reg();
		move_operand_reg(op, reg);
		op->type = OPERAND_REGISTER;
		op->reg = reg;
	}
}

void x64_init() {
	code_ptr = code;

	free_register_mask = 0b1111111111111111;
}

void x64_gen(Stmt *stmt) {
	switch (stmt->kind()) {
	case COMPOUND: {
	    auto com = (CompoundStmt *) stmt;
		for (auto s : com->stmts) {
			x64_gen(s);
		}
	} break;
	case FUNCTION_DEFINITION: {
	    auto fn = (FunctionDefinition *) stmt;
		EMIT_R(push, RBP);
		EMIT_R_R(mov, RBP, RSP);

		for (auto s : fn->body)
			x64_gen(s);
		break;
	}
	case RETURN: {
	    auto retur = (Return *) stmt;
		Operand operand;
		gen_expr(&operand, retur->return_values[0]);

		move_operand_reg(&operand, RAX);
		operand_free_reg(&operand);

		EMIT_R(pop, RBP);
		EMIT_OP(ret);
	} break;
	case VARIABLE_DEFINITION: {
	    auto var_def = (VariableDefinition *) stmt;
		u32 off = 0x8 + local_var_count++ * 8;
		Operand operand;
		gen_expr(&operand, var_def->value);
		if (operand.type == OPERAND_IMMEDIATE) {
			EMIT_MD1_I(movsx, RBP, -off, operand.val);
		} else {
			emit_as_register(&operand);
			EMIT_R_MD1(mov, operand.reg, RBP, -off);
		}
		operand_free_reg(&operand);

		variable_offset_map.insert({ std::string(var_def->var->name), off });

	} break;
	case WHILE: {
	    auto wh = (While *) stmt;
		uint8_t *while_statement = code_ptr;
		Operand cond;
		gen_expr(&cond, wh->cond);

		u8 *after_addr = code_ptr + 2;

		if (wh->cond->kind() == BINARY) {
			auto bin = (Binary *) wh->cond;
			auto op = bin->op;
		}

		EMIT_C_I(J, 0x4, 0x0);
		
		x64_gen(wh->body);

		EMIT_I(jmp, while_statement - code_ptr - 0x4);
		*after_addr = code_ptr - after_addr - 0x4;
	
		operand_free_reg(&cond);
	} break;
	case EXPR_STMT: {
	    auto expr_stmt = (ExprStmt *) stmt;
		Operand operand;
		gen_expr(&operand, expr_stmt->target_expr);
	} break;
	default:
		assert(0 && "Stmt not implemented yet");
	}
}

void gen_expr(Operand *operand, Expr *expr) {
	switch (expr->kind()) {
		case INT_LIT: {
			auto int_lit = (IntegerLiteral *) expr;
			operand->type = OPERAND_IMMEDIATE;
			operand->val = int_lit->int_value;
		} break;
		case VARIABLE: {
		    auto var = (Variable *) expr;
			operand->type = OPERAND_FRAME_OFFSET;
			operand->offset = variable_offset_map[std::string(var->name)];
		} break;
		case BINARY: {
		    auto bin = (Binary *) expr;
			Operand rhs;

			gen_expr(operand, bin->lhs);
			gen_expr(&rhs, bin->rhs);

			if (bin->op == '+') {
				emit_add(operand, &rhs);
			} else if (bin->op == '-') {
				emit_sub(operand, &rhs);
			} else if (bin->op == TOKEN_ADD_EQ) {
				u32 off = operand->offset;

				u32 reg = alloc_reg();
				operand->type = OPERAND_REGISTER; 
				operand->reg = reg;

				EMIT_MD1_R(mov, RBP, -off, reg);
				emit_add(operand, &rhs);
				EMIT_R_MD1(mov, reg, RBP, -off);

				free_reg(reg);
			} else if (bin->op == TOKEN_SUB_EQ) {
				u32 off = operand->offset;

				u32 reg = alloc_reg();
				operand->type = OPERAND_REGISTER;
				operand->reg = reg;

				EMIT_MD1_R(mov, RBP, -off, reg);
				emit_sub(operand, &rhs);
				EMIT_R_MD1(mov, reg, RBP, -off);

				free_reg(reg);
			} else if (bin->op == '=') {
				auto off = operand->offset;

				if (rhs.type == OPERAND_IMMEDIATE) {
					EMIT_MD1_I(movsx, RBP, -off, rhs.val);
				} else {
					emit_as_register(&rhs);
					EMIT_R_MD1(mov, rhs.reg, RBP, -off);
				}
			} else {
				assert(0 && "Binary operation not implemented yet");
			}
		} break;
		case COMPARE_ZERO: {
		    auto cmp = (CompareZero *) expr;
			gen_expr(operand, cmp->target);
			if (operand->type == OPERAND_FRAME_OFFSET) {
				EMIT_MD1_I(cmp, RBP, -operand->offset, 0x0);
			} else {
				emit_as_register(operand);
				EMIT_R_I(cmp, operand->reg, 0x0);
			}
		} break;
		case UNARY: {
		    auto unary = (Unary *) expr;
			switch (unary->op) {
				case '&': {
					gen_expr(operand, unary->target);

					Operand result;
					result.type = OPERAND_REGISTER;
					result.reg = alloc_reg();

					EMIT_R_MD1(lea, result.reg, RBP, -operand->offset);

					operand_free_reg(operand);
					operand->type = result.type;
					operand->reg = result.reg;
				} break;
				case TOKEN_PLUS_PLUS: {
					auto reg = alloc_reg();

					gen_expr(operand, unary->target);
					auto off = operand->offset;
					move_operand_reg(operand, reg);
					EMIT_R_I(add, reg, 0x1);
					EMIT_R_MD1(mov, reg, RBP, -off);

					free_reg(reg);
				} break;
				case TOKEN_MINUS_MINUS: {
					auto reg = alloc_reg();

					gen_expr(operand, unary->target);
					auto off = operand->offset;
					move_operand_reg(operand, reg);
					EMIT_R_I(sub, reg, 0x1);
					EMIT_R_MD1(mov, reg, RBP, -off);

					free_reg(reg);
				} break;
				case '-': {
					gen_expr(operand, unary->target);

					Operand reg_op;
					operand_alloc_reg(&reg_op);

					EMIT_R_R(xor, reg_op.reg, reg_op.reg);
					emit_sub(&reg_op, operand);

					operand->type = OPERAND_REGISTER;
					operand->reg = reg_op.reg;
				} break;
			}
		} break;
		default:
			assert(0 && "Expr not implemented yet");
	}
}

void emit_add(Operand *op1, Operand *op2) {
	if (op1->type == OPERAND_IMMEDIATE && op2->type == OPERAND_IMMEDIATE) {
		op1->val += op2->val;
	} else if (op1->type == OPERAND_IMMEDIATE) {
		emit_as_register(op2);
		EMIT_R_I(add, op2->reg, op1->val);
		op1->type = OPERAND_REGISTER;
		op1->reg = op2->reg;
	} else {
		emit_as_register(op1);
		if (op2->type == OPERAND_IMMEDIATE) {
			EMIT_R_I(add, op1->reg, op2->val);
		} else if (op2->type == OPERAND_FRAME_OFFSET) {
			EMIT_R_MD1(add, op1->reg, RBP, -op2->offset);
		} else {
			assert(op2->type == OPERAND_REGISTER);
			EMIT_R_R(add, op1->reg, op2->reg);
		}
	}
}

void emit_sub(Operand *op1, Operand *op2) {
	if (op1->type == OPERAND_IMMEDIATE && op2->type == OPERAND_IMMEDIATE) {
		op1->val -= op2->val;
	} else if (op1->type == OPERAND_IMMEDIATE) {
		emit_as_register(op2);
		EMIT_R_I(sub, op2->reg, op1->val);
		op1->type = OPERAND_REGISTER;
		op1->reg = op2->reg;
	} else {
		emit_as_register(op1);
		if (op2->type == OPERAND_IMMEDIATE) {
			EMIT_R_I(sub, op1->reg, op2->val);
		} else if (op2->type == OPERAND_FRAME_OFFSET) {
			EMIT_R_MD1(sub, op1->reg, RBP, -op2->offset);
		} else {
			assert(op2->type == OPERAND_REGISTER);
			EMIT_R_R(sub, op1->reg, op2->reg);
		}
	}
}

void x64_dump(Options *options) {
	FILE *f = fopen(options->obj_file, "wb");
	fwrite(code, 1, code_ptr - code, f);
	fclose(f);
}

#else

void x64_init() {}
void x64_gen(Stmt *stmt) {}
void x64_dump(Options *options) {}

#endif
