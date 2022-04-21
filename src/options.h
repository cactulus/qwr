#ifndef OPTIONS_H_
#define OPTIONS_H_

#include <cstdint>
#include <vector>

typedef uint8_t u8;

const u8 COMPILE_ONLY = 0x1;
const u8 OPTIMIZE = 0x2;
const u8 DEBUG = 0x4;
const u8 PRINT_LLVM = 0x8;

struct Options {
    std::vector<const char *> linker_flags;
    std::vector<const char *> libs;
    const char *src_file;
    const char *ll_file;
    const char *obj_file;
    const char *exe_file;
    u8 flags;
};

#endif