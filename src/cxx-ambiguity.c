#include <stdio.h>
#include "cxx-ambiguity.h"
#include "cxx-utils.h"

/*
 * This file performs disambiguation. If a symbol table is passed along the
 * tree the disambiguation is context-sensitive otherwise it is entirely
 * context-free (i.e. a flaw in our grammar or the standard grammar)
 */

static void choose_option(AST a, int n);
static int select_node_type(AST a, node_t type);
static AST recursive_search(AST a, node_t type);
static AST look_for_node_type_within_ambig(AST a, node_t type, int n);
static void solve_integral_specification_ambig(AST a);

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
 * Ambiguity in a high order declaration
 */
void solve_ambiguous_declaration(AST a, scope_t* st)
{
	char valid;
	int i;
	int j;
	
	// Determine the ambiguity
	// 
	// a) signed/unsigned/short/long

	valid = 1;
	for (i = 0; (i < a->num_ambig) && valid; i++)
	{
		AST option = a->ambig[i];

		if (ASTType(option) != AST_SIMPLE_DECLARATION)
		{
			// It is not this case
			valid = 0;
		}
		else
		{
			AST decl_specifier = ASTSon0(option);

			// Check that the type_spec only is null or holds a SUSL type
			if (ASTType(decl_specifier) == AST_AMBIGUITY)
			{
				// It can be ambiguous
				for (j = 0; (j < decl_specifier->num_ambig) && valid; j++)
				{
					AST true_decl_specifier = decl_specifier->ambig[j];

					AST type_specifier = ASTSon1(true_decl_specifier);
					valid = (type_specifier == NULL)
							|| (ASTType(type_specifier) == AST_LONG_TYPE)
							|| (ASTType(type_specifier) == AST_SHORT_TYPE)
							|| (ASTType(type_specifier) == AST_UNSIGNED_TYPE)
							|| (ASTType(type_specifier) == AST_SIGNED_TYPE);

					if (!valid)
						fprintf(stderr, "(1) Found not valid!\n");
				}
			}
			else
			{
				// If it is not ambiguous, then it cannot have declarators at all
				AST init_declarator_list = ASTSon1(option);
				valid = (init_declarator_list == NULL);
				if (!valid)
						fprintf(stderr, "(2) Found not valid!\n");
			}
		}
	}
	if (valid)
	{
		solve_integral_specification_ambig(a);
		return;
	}

	internal_error("Don't know how to handle this ambiguity", 0);
}

// Solves this case
//
//    unsigned long A;
//
// We will choose the first one that has a non null type spec
static void solve_integral_specification_ambig(AST a)
{
	// First solve the decl_specifier_ambiguity
	int i;
	AST candidate = NULL;

	for (i = 0; (i < a->num_ambig) && (candidate == NULL); i++)
	{
		// Choose the one with nonempty init_declarator
		candidate = a->ambig[i];

		if (ASTSon1(candidate) != NULL)
		{
			candidate = a->ambig[i];
		}
	}

	int j;

	AST ambig_decl_specifier_seq = ASTSon0(candidate);

	if (ASTType(ambig_decl_specifier_seq) != AST_AMBIGUITY)
	{
		internal_error("I expected an ambiguity here, what has happened to it ?", 0);
	}

	char found = 0;
	for (j = 0; (j < ambig_decl_specifier_seq->num_ambig) && !found; j++)
	{
		AST decl_specifier_seq = ambig_decl_specifier_seq->ambig[j];

		if (ASTSon1(decl_specifier_seq) != NULL)
		{
			found = 1;
		}
	}

	// Chose one of the several decl_specifier_seq
	choose_option(ambig_decl_specifier_seq, j-1);

	// Choose the one with init_declarator
	choose_option(a, i-1);
}

/*
 * Ambiguity within a declarator.
 */
void solve_ambiguous_declarator(AST a, scope_t* st)
{
	int num_ambig = a->num_ambig;

	switch (num_ambig)
	{
		case 2 :
			{
				// Case for declarator of
				// "(operator new[])" vs "(operator new)[]"
				int n, m;
				if ((n = select_node_type(a, AST_DECLARATOR_ARRAY)) != -1
						&& (m = select_node_type(a, AST_DECLARATOR_ID_EXPR)) != -1)
				{
					AST operator_function_id1 = look_for_node_type_within_ambig(a, AST_OPERATOR_FUNCTION_ID, n);
					AST operator_function_id2 = look_for_node_type_within_ambig(a, AST_OPERATOR_FUNCTION_ID, m);

					fprintf(stderr, "Lala -> 1=%p 2=%p\n", operator_function_id1, operator_function_id2);

					if ((operator_function_id1 != NULL)
							&& (operator_function_id2 != NULL))
					{
						// We want the declarator_id_expr
						choose_option(a, m);
						return;
					}
				}
				break;
			}
	}

	internal_error("Don't know how to handle this ambiguity", 0);
}

/*
 * Auxiliar functions
 */
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

	// This will work, trust on me :)
	*a = *(a->ambig[n]);
}

// Returns the index of the first node of type "type"
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

static AST recursive_search(AST a, node_t type)
{
	if (a == NULL)
		return NULL;

	AST result = NULL;

	if (ASTType(a) == type)
	{
		result = a;
	}

	int i;
	for (i = 0; (result == NULL) && (i < ASTNumChildren(a)); i++)
	{
		result = recursive_search(ASTChild(a, i), type);
	}

	return result;
}

static AST look_for_node_type_within_ambig(AST a, node_t type, int n)
{
	if (n >= a->num_ambig)
	{
		internal_error("There is no such option (%d) in this ambiguous node (options = %d)", n, a->num_ambig);
	}
	else if (n < 0)
	{
		internal_error("Invalid node number (%d)", n);
	}

	AST result = recursive_search(a->ambig[n], type);

	return result;
}
