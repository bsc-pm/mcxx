#ifdef HAVE_CONFIG_H
 #include <config.h>
#endif

#include <stdio.h>

#include "cxx-driver.h"
#include "cxx-ast.h"
#include "cxx-graphviz.h"

// Compilation options
compilation_options_t compilation_options;

void print_ambiguities(AST a);

int main(int argc, char* argv[])
{
	fprintf(stderr, PACKAGE " - " VERSION " (experimental)\n");
	mcxx_flex_debug = yydebug = 0;
	// yydebug = 1;

	yyparse(&compilation_options.parsed_tree);

	// ast_dump_graphviz(compilation_options.parsed_tree, stdout);
	// print_ambiguities(compilation_options.parsed_tree);

	return 0;
}

void print_ambiguities(AST a)
{
	if (a == NULL) 
		return;

	switch (ASTType(a))
	{
		case AST_NODE_LIST :
			{
				int i;
				for (i = 0; i < a->num_list; i++)
				{
					print_ambiguities(a->list[i]);
				}
				break;
			}
		case AST_AMBIGUITY :
			{
				int i;
				fprintf(stdout, "%d ", a->num_ambig);
				for (i = 0; i < a->num_ambig; i++)
				{
					fprintf(stdout, "%s ", ast_print_node_type(ASTType(a->ambig[i])));
				}
				break;
			}
		default :
			{
				int i;
				for (i = 0; i < ASTNumChildren(a); i++)
				{
					print_ambiguities(ASTChild(a, i));
				}
				break;
			}
	}
}
