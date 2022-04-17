#ifndef OPTIONS_H_
#define OPTIONS_H_

#include <cstdint>
#include <vector>

typedef uint8_t u8;

const u8 COMPILE_ONLY = 0x1;
const u8 OPTIMIZE = 0x2;

struct Options {
    std::vector<const char *> libs;
    u8 flags;
};

#endif
