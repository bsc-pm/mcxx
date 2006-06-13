#ifdef HAVE_CONFIG_H
 #include <config.h>
#endif

#include <string.h>
#include <stdio.h>

#include "cxx-driver.h"
#include "cxx-ast.h"
#include "cxx-graphviz.h"
#include "cxx-prettyprint.h"
#include "cxx-buildscope.h"

// Compilation options
compilation_options_t compilation_options;

void print_ambiguities(AST a, char lines);
char check_for_ambiguities(AST a);

int main(int argc, char* argv[])
{
	fprintf(stderr, PACKAGE " - " VERSION " (experimental)\n");
	mcxx_flex_debug = yydebug = 0;
	// yydebug = 1;

	yyparse(&compilation_options.parsed_tree);

	if (argc > 1)
	{
		if (strcmp(argv[1], "-a") == 0)
		{
			char lines = 0;
			if (argc > 2 && (strcmp(argv[2], "-l") == 0))
			{
				lines = 1;
			}
			print_ambiguities(compilation_options.parsed_tree, lines);
		}
		else if (strcmp(argv[1], "-g") == 0)
		{
			ast_dump_graphviz(compilation_options.parsed_tree, stdout);
		}
		else if (strcmp(argv[1], "-y") == 0)
		{
			build_scope_translation_unit(compilation_options.parsed_tree);

			if (argc > 2 && (strcmp(argv[2], "-g") == 0))
			{
				ast_dump_graphviz(compilation_options.parsed_tree, stdout);
			}

			if (!check_for_ambiguities(compilation_options.parsed_tree))
			{
				fprintf(stderr, "***** THERE ARE AMBIGUITIES UNRESOLVED !!! *****\n");
				print_ambiguities(compilation_options.parsed_tree, 1);
				return 1;
			}
		}
	}
	else
	{
		prettyprint(stdout, compilation_options.parsed_tree);
	}

	return 0;
}

char check_for_ambiguities(AST a)
{
	if (a == NULL)
		return 1;

	if (ASTType(a) == AST_AMBIGUITY)
	{
		return 0;
	}
	else
	{
		return check_for_ambiguities(ASTSon0(a))
			&& check_for_ambiguities(ASTSon1(a))
			&& check_for_ambiguities(ASTSon2(a))
			&& check_for_ambiguities(ASTSon3(a));
	}
}

void print_ambiguities(AST a, char lines)
{
	if (a == NULL) 
		return;

	// fprintf(stderr, "%p ", a);
	// fprintf(stderr, "[%d] %s\n", ASTNumChildren(a), ast_print_node_type(ASTType(a)));
	switch (ASTType(a))
	{
		case AST_AMBIGUITY :
			{
				int i;

				for (i = 0; i < a->num_ambig; i++)
				{
					print_ambiguities(a->ambig[i], lines);
				}
				
				fprintf(stderr, "%d ", a->num_ambig);
				for (i = 0; i < a->num_ambig; i++)
				{
					if (!lines)
					{
						fprintf(stderr, "%s | ", ast_print_node_type(ASTType(a->ambig[i])));
					}
					else
					{
						fprintf(stderr, "%s (%d) | ", ast_print_node_type(ASTType(a->ambig[i])), ASTLine(a->ambig[i]));
					}
				}
				fprintf(stderr, "\n");
				break;
			}
		default :
			{
				int i;
				for (i = 0; i < ASTNumChildren(a); i++)
				{
					print_ambiguities(ASTChild(a, i), lines);
				}
				break;
			}
	}
}
