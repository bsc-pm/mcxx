#ifdef HAVE_CONFIG_H
 #include <config.h>
#endif

#include <stdio.h>

#include "cxx-driver.h"
#include "cxx-graphviz.h"

// Compilation options
compilation_options_t compilation_options;

int main(int argc, char* argv[])
{
	fprintf(stderr, PACKAGE " - " VERSION " (experimental)\n");
	mcxx_flex_debug = yydebug = 0;

	yyparse(&compilation_options.parsed_tree);

	ast_dump_graphviz(compilation_options.parsed_tree, stdout);

	return 0;
}
