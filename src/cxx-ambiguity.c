#include "cxx-ambiguity.h"
#include "cxx-utils.h"

/*
 * This file performs disambiguation. If a symbol table is passed along the
 * tree the disambiguation is context-sensitive otherwise it is entirely
 * context-free (i.e. a flaw in our grammar or the standard grammar)
 */

static void choose_option(AST a, int n);
static int select_node_type(AST a, node_t type);

#define EXPECT_OPTIONS(a, n) \
do \
{ \
	if (((a)->num_ambig) != (n)) \
	{ \
	   internal_error("We expected %d ambiguities but %d found", (n), (a)->num_ambig); \
	} \
} while (0);


/*
 * Ambiguity between parameter-declaration and type-parameter in a template
 * parameter list
 *
 * Example:
 *
 * template <class T>
 *
 * Here 'class T' can be understood as a parameter declaration with no
 * declarator or as a type parameter.
 *
 * Options:
 *
 * AST_TYPE_PARAMETER_CLASS
 * AST_PARAMETER_DECL
 *
 * Solution:
 *
 * Always favour type parameters (AST_TYPE_PARAMETER_CLASS)
 */
void solve_parameter_declaration_vs_type_parameter_class(AST a)
{
	EXPECT_OPTIONS(a, 2);

	int k = select_node_type(a, AST_TYPE_PARAMETER_CLASS);

	choose_option(a, k);
}

/*
 * This function discards all but the n-option of this ambiguity. The node is
 * converted to one of its options.
 */
static void choose_option(AST a, int n)
{
	if (n >= a->num_ambig)
	{
		internal_error("There is no such option (%d) in this ambiguous node (options = %d)", n, a->num_ambig);
	}
	else if (n < 0)
	{
		internal_error("Invalid node number (%d)", n);
	}

	// Free discarded options
	int i;
	for (i = 0; i < a->num_ambig; i++)
	{
		if (i != n)
		{
			ASTFree(a->ambig[i]);
		}
	}

	// This will work
	*a = *(a->ambig[n]);
}

static int select_node_type(AST a, node_t type)
{
	int i;

	for (i = 0; i < a->num_ambig; i++)
	{
		if (ASTType(a->ambig[i]) == type)
		{
			return i;
		}
	}

	return -1;
}
