#ifndef CXX_TYPEENVIRON_H
#define CXX_TYPEENVIRON_H

#include "cxx-typeenviron-decls.h"

#define DEBUG_SIZEOF_CODE() if (CURRENT_CONFIGURATION(debug_options.debug_sizeof))

// Environments are to be defined in cxx-typeenviron.c
// and declared here

// IA32 environment (still here for compatibility reasons)
extern type_environment_t* type_environment_linux_ia32;

// A NULL ended list of those above
extern type_environment_t* type_environment_list[];

#endif // CXX_TYPEENVIRON_H
