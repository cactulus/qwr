#ifndef MANAGER_H_
#define MANAGER_H_

#include "options.h"

void manager_init(Options *_options);
void manager_run();
void manager_add_library(const char *lib_name);
void manager_add_flags(const char *flags);

#endif
