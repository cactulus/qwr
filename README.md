# QWR
A Compiler for my programming language

## Installation
### Requirements
LLVM \

### Step by Step Installation (Linux)
> git clone https://github.com/cactulus/qwr.git \
> cd qwr \
> sudo make clean install

## Basic Documentation
Semicolons are mostly optional in qwr (recommended to use them). \
Single line comments denoted by a double slash //
Multi line comments denoted by /* */

### Builtin-Types
s8, s16, s32, s64 -> integer types \
u8, u16, u32, u64 -> unsigned integer types \
f16, f32, f64 -> floating pointer types \
bool -> boolean type \
string, pointers, char \
int -> s32 \
uint -> u32 \
enums, structs \
arrays

### Function Declaration / Definition
Every qwr program must have a main function without any parameters and returning an s32.
```Rust
test :: (a i32) s32;

extern printf :: (fmt *u8, ..) // extern function. '..' indicates varargs

sub :: (a int, b int) int, int { // multiple return values
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

## For Statement
```Rust
for i := 0..10 {
    print(i);
}

for 0..10 {
	print(it); // it <-- standard variable
}

str := "Hello World!";
for str {
	print(it); // prints all characters
}

arr := {2, 8, 5, 1, 6} []int;";
for v := arr {
	print(v); // prints all values
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
	name string,
	age int,
	fav_day int
}
```

## Memory management
```Cpp
num := new int; // allocation with 'new'

delete num; // freeing with 'delete'
```

## Arrays
```Cpp
print_arr :: (a []int) {
	i := 0;
	while i < len(a) {
		print(a[i]);
		i++;
	}
}

arr := {2, 4, 1} []int;

print_arr(arr);

append(arr, 4);
append(arr, 6);
print("\n\n");

print_arr(arr);

delete arr;
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
