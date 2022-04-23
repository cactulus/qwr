# QWR
A Compiler for my programming language

## Installation
### Requirements
LLVM \
gcc \
clang on windows (for now)

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
string, pointers, char \
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

PI :@ 3.14; // :@ for constant

```

## If Statement
```Rust
if x == 10 {
    // ...
}
```

## While Statement
```Rust
while x < 10 {
    // ...
}
```

## Structs end Enums
```Rust
Weekdays :: enum {
	MONDAY,
	TUESDAY,
	WEDNESDAY
}

Person :: struct {
	name: string,
	age: int,
	fav_day: int
}
```

## Memory management
```Cpp
num := new int; // allocation with 'new'

delete num; // freeing with 'delete'
```

## Standard Library
Files in /std
```Rust
use "io";
use "glfw";
```

## Specify Linker Options (Linux only)
```Rust
qwr "-lglfw"; // used for example in glfw standard library
```

## Examples
Examples can be found in /examples
