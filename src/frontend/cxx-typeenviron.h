#ifndef CXX_TYPEENVIRON_H
#define CXX_TYPEENVIRON_H

#ifdef HAVE_CONFIG_H
  // Required for DEFAULT_TYPE_ENVIRONMENT
  #include "config.h"
#endif

#include "libmcxx-common.h"
#include "cxx-typeenviron-decls.h"

#define DEBUG_SIZEOF_CODE() if (CURRENT_CONFIGURATION(debug_options.debug_sizeof))

// Environments are to be defined in cxx-typeenviron.c
// and declared here

// A NULL ended list of those above
LIBMCXX_EXTERN type_environment_t* type_environment_list[];
LIBMCXX_EXTERN type_environment_t* default_environment;

// Fallback for faulty configuration
#ifndef DEFAULT_TYPE_ENVIRONMENT
  #error Missing default type environment
#endif

#endif // CXX_TYPEENVIRON_H
