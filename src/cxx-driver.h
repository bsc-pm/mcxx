#ifndef CXX_DRIVER_H
#define CXX_DRIVER_H

#include "cxx-ast.h"
#include "cxx-scope.h"
#include "cxx-scopelink.h"
#include "cxx-macros.h"

MCXX_BEGIN_DECLS

extern compilation_options_t compilation_options;

extern int mcxx_flex_debug;
extern int mc99_flex_debug;

extern int yyparse(AST* parsed_tree);

struct extensions_table_t*
fileextensions_lookup (register const char *str, 
        register unsigned int len);

struct configuration_directive_t*
configoptions_lookup (register const char *str, 
        register unsigned int len);

void parse_arguments(int argc, char* argv[], char from_command_line);

extern int num_seen_file_names;
extern char** seen_file_names;

extern int mcxxdebug;
extern int mcxxparse(AST* a);

extern int mc99debug;
extern int mc99parse(AST* a);

struct debug_flags_list_t** list_of_debug_flags(void);

struct debug_flags_list_t *
debugflags_lookup (register const char *str, register unsigned int len);

MCXX_END_DECLS

#endif // CXX_DRIVER_H
