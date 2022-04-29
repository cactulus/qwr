/*
Inspired by the Bitwise "Programming an x64 compiler from scratch" live streams
*/
#include <cassert>
#include <iostream>
#include <cstdint>

#include "options.h"
#include "parser.h"
#include "x64.h"

#ifdef _WIN32
namespace win {
	#include <Windows.h>
}

#define MAX_CODE_LEN 1024

typedef uint8_t u8;
typedef uint32_t u32;

enum Reg {
	RAX = 0x0,
	RCX = 0x1,
	RDX = 0x2,
	RBX = 0x3,
	RSI = 0x4,
	RDI = 0x5,
	RBP = 0x6,
	RSP = 0x7,
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

enum JumpCodes {

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
		uint32_t val;
		uint32_t offset;
		uint32_t address;
	};
};

static void gen_expr(Operand *operand, Expr *expr);

static u32 free_register_mask;
static u8 code[MAX_CODE_LEN];
static u8 *code_ptr;

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

#define EMIT_R_R(operation, dest, source) \
    emit_rex2(dest, source); \
    emit_##operation##_r(); \
    emit_direct(dest, source)

#define EMIT_R_M(operation, destination, source) \
    emit_rex2(destination, source); \
    emit_##operation##_r(); \
    emit_indirect(destination, source)

#define EMIT_R_MD1(operation, dest, source, disp) \
    emit_rex2(dest, source); \
    emit_##operation##_r(); \
    emit_byte_displaced(dest, source, disp)

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

#define EMIT_MD1_R(operation, destination, displacement, source) \
    emit_rex2(source, destination); \
    emit_##operation##_m(); \
    emit_byte_displaced(source, destination, displacement);

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
    void emit_##operation##_c_i(u8 code) { \
        emit(opcode + code); \
    } 

#define OP2CI(operation, opcode) \
    void emit_##operation##_c_i(u8 code) { \
        emit(0x0F); \
        emit(opcode + code); \
    } 

OP1M(MOV, 0x8B)
OP1R(MOV, 0x89)
OP1I(MOVSX, 0xC7, 0x00)

OP1R(ADD, 0x03)
OP1M(ADD, 0x01)
OP1I(ADD, 0x81, 0x00)

OP1R(SUB, 0x2B)
OP1M(SUB, 0x29)
OP1I(SUB, 0x81, 0x05)

OP1X(MUL, 0xF7, 0x04)
OP1X(DIV, 0xF7, 0x06)

OP1I(CMP, 0x81, 0x07)

OP1I(JMP, 0xE9, 0x00)

u8 alloc_reg() {
	win::DWORD free_register;
	win::_BitScanForward(&free_register, free_register_mask);
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
		EMIT_R_I(MOVSX, reg, op->val);
	} else if (op->type == OPERAND_FRAME_OFFSET) {
		EMIT_R_MD1(MOV, reg, RBP, op->offset);
	} else if (op->type == OPERAND_ADDRESS) {
		EMIT_R_M(MOV, reg, op->address);
	} else if (op->type == OPERAND_REGISTER) {
		if (op->reg != reg) {
			EMIT_R_R(MOV, reg, op->reg);
		}
	}
	op->type = OPERAND_REGISTER;
	op->reg = reg;
}

void EmitAsRegister(Operand* op) {
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
	switch (stmt->kind) {
	case FUNCTION_DEFINITION:
		for (auto s : *stmt->func_def.body)
			x64_gen(s);
		break;
	case VARIABLE_DEFINITION: {
		u32 off = local_var_count++ * 8;
		Operand operand;
		gen_expr(&operand, stmt->var_def.value);
		if (operand.type == OPERAND_IMMEDIATE) {
			EMIT_MD1_I(MOVSX, RBP, off, operand.val);
		} else {
			EmitAsRegister(&operand);
			EMIT_MD1_R(MOV, RBP, off, operand.reg);
		}
		operand_free_reg(&operand);
		} break;
	default:
		assert(0 && "Stmt not implemented yet");
	}
}

void gen_expr(Operand *operand, Expr *expr) {
	switch (expr->kind) {
	case INT_LIT:
		operand->type = OPERAND_IMMEDIATE;
		operand->val = expr->int_value;
		break;
	default:
		assert(0 && "Expr not implemented yet");
	}
}

void x64_dump(Options *options) {
	FILE *f = fopen(options->obj_file, "wb");
	fwrite(code, 1, code_ptr - code, f);
	fclose(f);
}

#else
void x64_init() {
	assert(0 && "x64 backend is only available on Windows!");
}

void x64_gen(Stmt *stmt) {
	assert(0 && "x64 backend is only available on Windows!");
}

void x64_dump(Options *options) {
	assert(0 && "x64 backend is only available on Windows!");
}
#endif