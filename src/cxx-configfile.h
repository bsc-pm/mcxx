#ifndef CXX_CONFIGFILE
#define CXX_CONFIGFILE

#include "cxx-driver.h"
#include "cxx-macros.h"

MCXX_BEGIN_DECLS

typedef int (option_function_t)(char* value);

option_function_t config_set_language;
option_function_t config_set_options;
option_function_t config_set_preprocessor_name;
option_function_t config_set_preprocessor_options;
option_function_t config_set_compiler_name;
option_function_t config_set_compiler_options;
option_function_t config_set_linker_name;
option_function_t config_set_linker_options;

MCXX_END_DECLS

#endif // CXX_CONFIGFILE
