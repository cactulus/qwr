#ifndef BUILTIN_H_
#define BUILTIN_H_

struct CodeGenerator;

void init_builtins(Typer *typer);
bool is_builtin(const char *name);
QType *get_builtin_return_type(const char *name);

#endif
