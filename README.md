# QWR
A Compiler for my programming language

## Installation
### Requirements
LLVM \
gcc

### Step by Step Installation
> git clone https://github.com/cactulus/qwr.git \
> cd qwr \
> sudo make clean install

## Basic Documentation
Semicolons are optional in qwr. \
Single line comments denoted by a double slash //
Multi line comments denoted by /* */

### Builtin-Types
s8, s16, s32, s64 -> integer types \
u8, u16, u32, u64 -> unsigned integer types \
bool -> boolean type \
string, pointers \
int -> s32 \
uint -> u32

### Function Declaration / Definition
Every qwr program has to have an main function without parameters and returning an s32.
```Rust
test :: (a: i32) s32;

extern printf :: (fmt: *u8, ..) // extern function. '..' indicates varargs

sub :: (a: int, b: int) int, int { // multiple return values
    return a - b, b - a;
}

main :: () s32 {
    return 0;
}
```

### Variable Declaration / Definition
```Rust
x := 20; // infer type
y: i32 = 10; // type specified
c, d := sub(5, 3); // multiple return values from function
```

## If Statement
```Rust
if x == 10 {
    // ...
}
```
