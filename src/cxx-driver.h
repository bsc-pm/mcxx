#ifndef CXX_DRIVER_H
#define CXX_DRIVER_H

#include <cxx-ast.h>

struct compilation_options_tag
{
	int debug;

	AST parsed_tree;
};

typedef struct compilation_options_tag compilation_options_t;

extern compilation_options_t compilation_options;

extern int yydebug;
extern int mcxx_flex_debug;

extern int yyparse(AST* parsed_tree);

#endif // CXX_DRIVER_H
