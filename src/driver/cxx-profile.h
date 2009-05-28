#ifndef CXX_PROFILE_H
#define CXX_PROFILE_H

#include "cxx-driver-decls.h"

compilation_configuration_t* get_compilation_configuration(const char* name);
compilation_configuration_t* new_compilation_configuration(const char* name, compilation_configuration_t* base);

void initialize_with_base(compilation_configuration_t* config);

#endif // CXX_PROFILE_H
