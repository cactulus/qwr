#include <cassert>
#include <iostream>
#include <cstring>

#ifdef DIAGNOSTICS
#include <fstream>
#include <chrono>
#endif

#include "manager.h"


size_t read_entire_file(FILE *f, char **contents);

void compile(char *code, size_t code_len) {
	Manager manager;
	manager.init(code, code_len);

	manager.run();
}

int main() {

    const char *src_file = "examples/test.qwr";
	FILE *input_file = fopen(src_file, "r");
	char *code;
	size_t code_len = read_entire_file(input_file, &code);
	fclose(input_file);

    #ifdef DIAGNOSTICS
    auto start = std::chrono::steady_clock::now();
    #endif 
    compile(code, code_len);

    #ifdef DIAGNOSTICS
    auto end = std::chrono::steady_clock::now();
    auto timems = std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count();

    std::cout << "Compilation time: " << timems << " ms\n";

    std::ifstream file_stream(src_file); 
    auto loc = std::count(std::istreambuf_iterator<char>(file_stream), 
             std::istreambuf_iterator<char>(), '\n');

    std::cout << "LOC: " << loc << "\n";
    #endif

	return 0;
}

size_t read_entire_file(FILE *f, char **contents) {
	fseek(f, 0, SEEK_END);
	size_t len = ftell(f);
	fseek(f, 0, SEEK_SET);

	char *buffer = new char[len + 1];
	if (!fread(buffer, 1, len, f)) {
		fclose(f);
		return 0;
	}

	buffer[len] = '\0';
	*contents = buffer;

	return len;
}
