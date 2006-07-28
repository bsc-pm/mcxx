#include <stdio.h>
#include <string.h>
#include "cxx-ambiguity.h"
#include "cxx-typeutils.h"
#include "cxx-utils.h"
#include "cxx-prettyprint.h"
#include "cxx-buildscope.h"
#include "cxx-graphviz.h"

/*
 * This file performs disambiguation. If a symbol table is passed along the
 * tree the disambiguation is context-sensitive otherwise it is entirely
 * context-free (i.e. a flaw in our grammar or the standard grammar)
 *
 */

static void choose_option(AST a, int n);
static int select_node_type(AST a, node_t type);
static AST recursive_search(AST a, node_t type);
static AST look_for_node_type_within_ambig(AST a, node_t type, int n);
static void solve_integral_specification_ambig(AST a);
static void solve_nested_name_with_no_type(AST a);
static void solve_ambiguous_simple_declaration(AST a, scope_t* st);

static char check_for_declaration_statement(AST a, scope_t* st);
static char check_for_expression_statement(AST a, scope_t* st);
static char check_for_qualified_id(AST expr, scope_t* st);
static char check_for_symbol(AST expr, scope_t* st);
#if 0
static char check_for_destructor_id(AST expr, scope_t* st);
#endif
static char check_for_function_call(AST expr, scope_t* st);
static char check_for_explicit_type_conversion(AST expr, scope_t* st);
static char check_for_explicit_typename_type_conversion(AST expr, scope_t* st);
static char check_for_typeid(AST expr, scope_t* st);
static char check_for_typeid_expr(AST expr, scope_t* st);
static char check_for_sizeof_expr(AST expr, scope_t* st);
static char check_for_sizeof_typeid(AST expr, scope_t* st);
static char check_for_cast(AST expr, scope_t* st);

static char check_for_simple_type_spec(AST type_spec, scope_t* st);
static char check_for_type_id_tree(AST type_id, scope_t* st);
static char check_for_type_specifier(AST type_id, scope_t* st);

static char check_for_typeless_declarator(AST declarator, scope_t* st);

static char check_for_init_declarator(AST init_declarator, scope_t* st);
static char check_for_declarator(AST declarator, scope_t* st);
static char check_for_declarator_rec(AST declarator, scope_t* st);
static char check_for_function_declarator_parameters(AST parameter_declaration_clause, scope_t* st);

static char check_for_simple_declaration(AST a, scope_t* st);
static char check_for_initializer_clause(AST initializer_clause, scope_t* st);

static char check_for_new_expression(AST new_expr, scope_t* st);
static char check_for_new_type_id_expr(AST new_expr, scope_t* st);

#define EXPECT_OPTIONS(a, n) \
do \
{ \
	if (((a)->num_ambig) != (n)) \
	{ \
	   internal_error("We expected %d ambiguities but %d found", (n), (a)->num_ambig); \
	} \
} while (0);


// Returns 1 if ASTType(t1) == n1 && ASTType(t2) == n2
// Returns -1 if ASTType(t1) == n2 && ASTType(t2) == n1
// Returns 0 otherwise
static char either_type(AST t1, AST t2, node_t n1, node_t n2)
{
	if ((ASTType(t1) == n1) 
			&& (ASTType(t2) == n2)) 
		return 1;

	if ((ASTType(t1) == n2) 
			&& (ASTType(t2) == n1)) 
		return -1;

	return 0;
}

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
// #warning TODO - Refactorize this with the code that makes the same with a single decl_specifier_seq/type_specifier_seq
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

		if (ASTType(option) != AST_SIMPLE_DECLARATION
				&& ASTType(option) != AST_MEMBER_DECLARATION)
		{
			// It is not this case
			valid = 0;
		}
		else
		{
			AST decl_specifier = ASTSon0(option);

			// Check that the type_spec only is null or holds a SUSL type
			if (decl_specifier != NULL)
			{
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
					}
				}
				else
				{
					// ??
					// If it is not ambiguous, then it cannot have declarators at all
					AST init_declarator_list = ASTSon1(option);
					valid = (init_declarator_list == NULL);
				}
			}
		}
	}
	if (valid)
	{
		solve_integral_specification_ambig(a);
		return;
	}

	/*
	 * Ambiguous declaration for a nested name
	 * referring to a constructor
	 */
	char there_is_empty_type_spec = 0;
	valid = 1;
	for (i = 0; (i < a->num_ambig) && valid; i++)
	{
		AST option = a->ambig[i];

		// This should only happen with declarators referring to constructors
		// and since they cannot appear as just plain declarations this must be
		// a function definition
		if (ASTType(option) != AST_FUNCTION_DEFINITION)
		{
			valid = 0;
			break;
		}

		AST decl_specifier_seq = ASTSon0(option);

		if (decl_specifier_seq == NULL
				|| ASTSon1(decl_specifier_seq) == NULL)
		{
			there_is_empty_type_spec = 1;
		}
	}
	
	if (!there_is_empty_type_spec)
	{
		valid = 0;
	}

	if (valid)
	{
	    solve_nested_name_with_no_type(a);
		return;
	}

	/*
	 * running nested name ambiguity
	 *
	 *  class A::B;
	 *
	 * does not have to be understood as class A  ::B;
	 */
	valid = 1;
	for (i = 0; (i < a->num_ambig) && valid; i++)
	{
		AST option = a->ambig[i];

		valid &= (ASTType(option) == AST_SIMPLE_DECLARATION
				|| ASTType(option) == AST_MEMBER_DECLARATION);
	}

	if (valid)
	{
		solve_ambiguous_simple_declaration(a, st);
		return;
	}

	ASSERT_MESSAGE(1, "Don't know how to handle this ambiguity. %s", node_information(a));
}

static void solve_ambiguous_simple_declaration(AST a, scope_t* st)
{
	int correct_option = -1;
	int i;
	for (i = 0; i < a->num_ambig; i++)
	{
		AST option = a->ambig[i];

		if (check_for_simple_declaration(option, st))
		{
			if (correct_option < 0)
			{
				correct_option = i;
			}
			else
			{
				AST previous_option = a->ambig[correct_option];
				AST current_option = option;
				internal_error("More than one valid alternative! %s vs %s\n", 
						ast_print_node_type(ASTType(previous_option)),
						ast_print_node_type(ASTType(current_option)));
			}
		}
	}

	if (correct_option < 0)
	{
		internal_error("Ambiguity not solved\n", 0);
	}
	else
	{
		choose_option(a, correct_option);
	}
}


static void solve_nested_name_with_no_type(AST a)
{
	int i;
	for (i = 0; i < a->num_ambig; i++)
	{
		AST option = a->ambig[i];

		AST decl_specifier_seq = ASTSon0(option);

		if (decl_specifier_seq == NULL || 
				ASTSon1(decl_specifier_seq) == NULL)
		{
			choose_option(a, i);
			return;
		}
	}
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

void solve_ambiguous_statement(AST a, scope_t* st)
{
	// The strategy used here is to check every ambiguity and select
	// the valid one
	int correct_choice = -1;
	int i;

	for (i = 0; i < a->num_ambig; i++)
	{
		char current_check = 0;

		switch (ASTType(a->ambig[i]))
		{
			case AST_DECLARATION_STATEMENT :
				{
					current_check = check_for_declaration_statement(a->ambig[i], st);
					break;
				}
			case AST_EXPRESSION_STATEMENT :
				{
					current_check = check_for_expression_statement(a->ambig[i], st);
					break;
				}
			default :
				{
					internal_error("Unexpected node '%s'\n", ast_print_node_type(ASTType(a)));
					break;
				}
		}

		if (current_check)
		{
			if (correct_choice < 0)
			{
				correct_choice = i;
			}
			else
			{
				// Consider the case described in the standard
				//
				//      T(a);
				//
				// where "T" is a typename, this is a declaration instead of an
				// expression.
				AST first_option = ASTSon0(a->ambig[correct_choice]);
				AST second_option = ASTSon0(a->ambig[i]);

				char either;
				if ((either = either_type(first_option, second_option, 
							AST_SIMPLE_DECLARATION, AST_EXPLICIT_TYPE_CONVERSION)))
				{
					if (either < 0)
					{
						correct_choice = i;
					}
				}
				else
				{
					internal_error("More than one valid choice! '%s' vs '%s' %s", 
							ast_print_node_type(ASTType(first_option)),
								ast_print_node_type(ASTType(second_option)),
								node_information(second_option));
				}
			}
		}
	}

	if (correct_choice < 0)
	{
		ast_dump_graphviz(a, stderr);
		internal_error("Ambiguity not solved\n", 0);
	}
	else
	{
		choose_option(a, correct_choice);
	}
}


static char check_for_init_declarator_list(AST init_declarator_list, scope_t* st)
{
	AST list = init_declarator_list;
	AST iter;

	for_each_element(list, iter)
	{
		AST init_declarator = ASTSon1(iter);

		if (ASTType(init_declarator) == AST_AMBIGUITY)
		{
			int current_choice = -1;
			int i;
			for (i = 0; i < init_declarator->num_ambig; i++)
			{
				AST one_init_decl = init_declarator->ambig[i];

				if (check_for_init_declarator(one_init_decl, st))
				{
					if (current_choice < 0)
					{
						current_choice = i;
					}
					else
					{
						AST current_option = init_declarator->ambig[i];
						AST previous_option = init_declarator->ambig[current_choice];

						internal_error("More than one valid option '%s' vs '%s'", 
								ast_print_node_type(ASTType(current_option)),
								ast_print_node_type(ASTType(previous_option)));
					}
				}
			}

			if (current_choice < 0)
			{
				return 0;
			}
			else
			{
				choose_option(init_declarator, current_choice);
			}
		}
		else
		{
			if (!check_for_init_declarator(init_declarator, st))
			{
				return 0;
			}
		}
	}

	return 1;
}

static char check_for_decl_spec_seq_followed_by_declarator(AST decl_specifier_seq, AST declarator)
{
	// A::f(c) has to be interpreted as A::f(c) and never as A   ::f(c)
	// (if you want the latest you must use A(::f(c))
	AST type_spec = ASTSon1(decl_specifier_seq);
	if (type_spec != NULL
			&& ASTSon2(decl_specifier_seq) == NULL
			&& (ASTType(type_spec) == AST_SIMPLE_TYPE_SPECIFIER
				|| ASTType(type_spec) == AST_ELABORATED_TYPE_CLASS
				|| ASTType(type_spec) == AST_ELABORATED_TYPE_ENUM
				|| ASTType(type_spec) == AST_ELABORATED_TYPENAME
				|| ASTType(type_spec) == AST_ELABORATED_TYPENAME_TEMPLATE
				|| ASTType(type_spec) == AST_ELABORATED_TYPE_TEMPLATE_CLASS
				|| ASTType(type_spec) == AST_ELABORATED_TYPE_TEMPLATE_TEMPLATE_CLASS))
	{
		AST declarator_name = get_leftmost_declarator_name(declarator);

		if (declarator_name != NULL)
		{
			if (ASTType(declarator_name) == AST_QUALIFIED_ID
					|| ASTType(declarator_name) == AST_QUALIFIED_OPERATOR_FUNCTION_ID)
			{
				AST global_op = ASTSon0(declarator_name);
				if (global_op != NULL)
				{
					if (ASTType(global_op) != AST_GLOBAL_SCOPE)
					{
						internal_error("Expecting a global scope operator\n", 0);
					}

					// At this point, this sequence of decl_spec and declarator looks like one of the following
					//
					//  [AST_SIMPLE_TYPE_SPECIFIER]
					//   A::B ::C;
					//   ::A::B ::C;
					//   A::B<int> ::C;
					//   ::A::B<int> ::C;
					//   A::template B<int> ::C;
					//   ::A::template B<int> ::C;
					//  [AST_ELABORATED_TYPE_CLASS]
					//   class A::B ::C;
					//   class ::A::B ::C;
					//  [AST_ELABORATED_TYPE_ENUM]
					//   enum A::B ::C;
					//   enum ::A::B ::C;
					//  [AST_ELABORATED_TYPENAME]
					//   typename A::B ::C;
					//   typename ::A::B ::C;
					//  [AST_ELABORATED_TYPENAME_TEMPLATE]
					//   typename A::B<int> ::C;
					//   typename ::A::B<int> ::C;
					//  [AST_ELABORATED_TYPE_TEMPLATE]
					//   class A::B<int> ::C;
					//   class ::A::B<int> ::C;
					//  [AST_ELABORATED_TYPE_TEMPLATE_TEMPLATE]
					//   class template B<int> ::C;
					//   class A::template B<int> ::C;
					//
					// which it is not intended to be a correct
					// declaration because the nested name spec has to
					// be as long as possible
					return 0;
				}
			}
		}
	}

	return 1;
}

static char check_for_simple_declaration(AST a, scope_t* st)
{
	AST decl_specifier_seq = ASTSon0(a);

	if (decl_specifier_seq != NULL)
	{
		// We should check that this type specifier is really a type specifier
		//
		//    A(t);
		//
		// is a declaration if "A" names a type. Otherwise this is not a valid
		// simple declaration
		
		// Additional check. Ensure we are using the longest possible nested name seq
		AST first_init_declarator = NULL;
		AST list = ASTSon1(a);
		AST iter;

		if (list != NULL)
		{

			for_each_element(list, iter)
			{
				first_init_declarator = ASTSon1(iter);
				break;
			}

			AST first_declarator;
			if (ASTType(first_init_declarator) == AST_AMBIGUITY)
			{
				first_declarator = ASTSon0(first_init_declarator->ambig[0]);
			}
			else
			{
				first_declarator = ASTSon0(first_init_declarator);
			}

			if (!check_for_decl_spec_seq_followed_by_declarator(decl_specifier_seq, first_declarator))
			{
				return 0;
			}
		}

		AST type_spec = ASTSon1(decl_specifier_seq);

		if (type_spec != NULL)
		{
			if (!check_for_type_specifier(type_spec, st))
			{
				return 0;
			}
		}

		AST init_declarator_list = ASTSon1(a);

		if (init_declarator_list != NULL)
		{
			if (!check_for_init_declarator_list(init_declarator_list, st))
			{
				return 0;
			}
		}


		// Additional check for this special case
		// typedef int T;
		// struct A
		// {
		//    A(T);     <-- This is a constructor not "A T;"
		// };
		//
		// This is not a field declarator if all of these happen
		//
		//    * 'A' must be a SK_CLASS (or SK_TEMPLATE_PRIMARY_CLASS or SK_TEMPLATE_SPECIALIZED_CLASS)
		//    * 'A' related-scope is the same of current scope
		//    * 'T' is just a declarator_id_expr
		//    * 'T' names a type

		if (first_init_declarator != NULL 
				&& type_spec != NULL)
		{
			AST first_declarator = ASTSon0(first_init_declarator);

			AST parenthesized_declarator;
			AST inner_declarator;
			AST declarator_id_expression;
			// T is just a parenthesized declarator_id_expr
			if (ASTType(first_declarator) == AST_DECLARATOR
					&& (parenthesized_declarator = ASTSon0(first_declarator)) != NULL
					&& ASTType(parenthesized_declarator) == AST_PARENTHESIZED_DECLARATOR
					&& (inner_declarator = ASTSon0(parenthesized_declarator)) != NULL
					&& ASTType(inner_declarator) == AST_DECLARATOR
					&& (declarator_id_expression = ASTSon0(inner_declarator)) != NULL
					&& ASTType(declarator_id_expression) == AST_DECLARATOR_ID_EXPR)
			{
				AST id_expression = ASTSon0(declarator_id_expression);
				scope_entry_list_t* entry_list = query_id_expression(st, id_expression, FULL_UNQUALIFIED_LOOKUP);

				// T names a type
				if (entry_list != NULL 
						&& (entry_list->entry->kind == SK_TYPEDEF
							|| entry_list->entry->kind == SK_ENUM
							|| entry_list->entry->kind == SK_CLASS
							|| entry_list->entry->kind == SK_TEMPLATE_PRIMARY_CLASS
							|| entry_list->entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS
							|| entry_list->entry->kind == SK_TEMPLATE_TYPE_PARAMETER))
				{
					// A is a simple type specifier
					if (ASTType(type_spec) == AST_SIMPLE_TYPE_SPECIFIER)
					{
						AST global_op = ASTSon0(type_spec);
						AST nested_name_spec = ASTSon1(type_spec);
						AST type_name = ASTSon2(type_spec);

						entry_list = query_nested_name(st, global_op, nested_name_spec, type_name, FULL_UNQUALIFIED_LOOKUP);

						// A is of class nature
						if (entry_list != NULL
								&& (entry_list->entry->kind == SK_CLASS
									|| entry_list->entry->kind == SK_TEMPLATE_PRIMARY_CLASS
									|| entry_list->entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS))
						{
							scope_entry_t* entry = entry_list->entry;
							
							// The related scope of A is the same as the
							// current scope
							if (entry->related_scope == st)
							{
								// In this case, and only in this case, this is
								// not a data member declaration
								return 0;
							}
						}
					}
				}
			}
		}
	}
	else
	{
		// Ok, check these are conversion functions, constructors or destructors
		//
		// Note that something like the following is perfectly valid
		//
		//  struct A {
		//      (A)(), (A)(const A& a), ~A(), operator int();
		//  };
		AST init_declarator_list = ASTSon1(a);
		AST iter;
		for_each_element(init_declarator_list, iter)
		{
			AST init_declarator = ASTSon1(iter);
			AST declarator = ASTSon0(init_declarator);

			if (ASTType(init_declarator) == AST_AMBIGUITY)
			{
				int correct_choice = -1;
				int i;
				for (i = 0; i < init_declarator->num_ambig; i++)
				{
					AST opt_declarator = ASTSon0(init_declarator->ambig[i]);

					if (check_for_typeless_declarator(opt_declarator, st))
					{
						if (correct_choice < 0)
						{
							correct_choice = i;
						}
						else
						{
							internal_error("More than one valid choice", 0);
						}
					}
				}

				// No choice was possible
				if (correct_choice < 0)
				{
					return 0;
				}
				else
				{
					choose_option(init_declarator, correct_choice);
				}
			}
			else
			{
				if (!check_for_typeless_declarator(declarator, st))
				{
					return 0;
				}
			}
		}
	}

	return 1;
}

static char check_for_declaration_statement(AST declaration_statement, scope_t* st)
{
	AST a = ASTSon0(declaration_statement);

	// In general only AST_SIMPLE_DECLARATION gets ambiguous here
	if (ASTType(a) == AST_SIMPLE_DECLARATION)
	{
		return check_for_simple_declaration(a, st);
	}
	else if (ASTType(a) == AST_AMBIGUITY)
	{
		// internal_error("Unknown node type '%s' (line=%d)\n", ast_print_node_type(ASTType(a)),
		// 		node_information(a));
		//
		// In general only AST_SIMPLE_DECLARATION gets ambiguous here

		int correct_choice = -1;
		int i;
		for (i = 0; i < a->num_ambig; i++)
		{
			if (check_for_simple_declaration(a->ambig[i], st))
			{
				if (correct_choice < 0)
				{
					correct_choice = i;
				}
				else
				{
					internal_error("More than one valid alternative", 0);
				}
			}
		}

		if (correct_choice < 0)
		{
			return 0;
		}
		else
		{
			choose_option(a, correct_choice);
		}
	}

	return 1;
}

static char check_for_typeless_declarator_rec(AST declarator, scope_t* st, int nfuncs)
{
	switch (ASTType(declarator))
	{
		case AST_PARENTHESIZED_EXPRESSION :
		case AST_DECLARATOR :
			{
				return check_for_typeless_declarator_rec(ASTSon0(declarator), st, nfuncs);
				break;
			}
		case AST_POINTER_DECL :
		case AST_DECLARATOR_ARRAY : 
			{
				// struct A
				// {
				//    *A();  <-- invalid
				//    A()[]; <-- invalid
				// };
				return 0;
			}
		case AST_DECLARATOR_FUNC :
			{
				return check_for_typeless_declarator_rec(ASTSon0(declarator), st, nfuncs+1);
				break;
			}
		case AST_DECLARATOR_ID_EXPR :
			{
				// Do nothing
				// will continue below
				break;
			}
		default :
			{
			}
	}

	// We are in a AST_DECLARATOR_ID_EXPR
	if (nfuncs != 1)
	{
		// struct A
		// {
		//   A;     <-- invalid (nfuncs == 0)
		//   A()(); <-- invalid (nfuncs == 2)
		//  };
		return 0;
	}

	AST id_expression = ASTSon0(declarator);

	switch (ASTType(id_expression))
	{
		case AST_QUALIFIED_ID :
			{
				AST global_scope = ASTSon0(id_expression);
				AST nested_name_spec = ASTSon1(id_expression);
				AST symbol = ASTSon2(id_expression);

				char is_dependent = 0;
				scope_t* nested_scope = query_nested_name_spec(st, global_scope, nested_name_spec, NULL, &is_dependent);

				if (nested_scope == NULL)
				{
					// Unknown scope
					return 0;
				}

				if (nested_scope->kind != CLASS_SCOPE)
				{
					return 0;
				}
				
				// These always have type
				if (ASTType(symbol) == AST_OPERATOR_FUNCTION_ID
						|| ASTType(symbol) == AST_TEMPLATE_ID
						|| ASTType(symbol) == AST_OPERATOR_FUNCTION_ID_TEMPLATE)
				{
					return 0;
				}
				
				char* class_name = ASTText(symbol);

				if (ASTType(symbol) == AST_DESTRUCTOR_ID
						|| ASTType(symbol) == AST_DESTRUCTOR_TEMPLATE_ID)
				{
					// Spring '~'
					class_name++;
				}
				
				
				// Now look for the class symbol
				nested_scope = nested_scope->contained_in;

				if (nested_scope == NULL)
				{
					// This should not happen
					return 0;
				}

				// This is somewhat strict
				scope_entry_list_t* result_list = query_in_symbols_of_scope(nested_scope, class_name);
				enum cxx_symbol_kind filter_classes[3] = {
					SK_CLASS, 
					SK_TEMPLATE_PRIMARY_CLASS, 
					SK_TEMPLATE_SPECIALIZED_CLASS
				};
				scope_entry_list_t* classes_list = filter_symbol_kind_set(result_list, 3, filter_classes);

				if (classes_list == NULL)
				{
					// This is not a class name
					return 0;
				}

				// It looks sane here
				return 1;
				break;
			}
		case AST_DESTRUCTOR_ID :
		case AST_DESTRUCTOR_TEMPLATE_ID :
		case AST_SYMBOL :
			{
				char* class_name = ASTText(id_expression);

				// We want a class scope
				if (st->kind != CLASS_SCOPE)
				{
					return 0;
				}

				if (ASTType(id_expression) == AST_DESTRUCTOR_ID ||
						ASTType(id_expression) == AST_DESTRUCTOR_TEMPLATE_ID)
				{
					// Spring '~'
					class_name++;
				}

				if (st->contained_in == NULL)
				{
					// This unqualified name cannot be a class
					//
					// A(); <-- invalid;
					//
					return 0;
				}
				else
				{
					// Now look for the class symbol in the enclosing scope
					//
					//   class A {
					//      A();  <-- valid
					//      ~A(); <-- valid
					//   };
					//
					scope_t* enclosing_scope = st->contained_in;
					scope_entry_list_t* result = query_in_symbols_of_scope(enclosing_scope, class_name);

					if (result == NULL
							|| (result->entry->kind != SK_CLASS
								&& result->entry->kind != SK_TEMPLATE_PRIMARY_CLASS
								&& result->entry->kind != SK_TEMPLATE_SPECIALIZED_CLASS))
					{
						// This is not a class name
						return 0;
					}
					// It looks sane here
					return 1;
				}
				break;
			}
		case AST_CONVERSION_FUNCTION_ID :
			// That's fine
			return 1;
		default :
			// Do nothing for any other things
			break;
	}

	return 0;
}

static char check_for_typeless_declarator(AST declarator, scope_t* st)
{
	return check_for_typeless_declarator_rec(declarator, st, 0);
}


// This function will check for soundness of an expression.
// It does not typechecking but checks that the expression
// is semantically feasible.
//
// e.g.
//
//   template <class A>
//   struct B { };
//
//   void f()
//   {
//      int C, D;
//      B < C > D;
//   }
//
// "(B < C) > D" intepretation is not feasible because B names
// a type and thus does not yield a value
//
char check_for_expression(AST expression, scope_t* st)
{
	switch (ASTType(expression))
	{
		case AST_EXPRESSION : 
		case AST_CONSTANT_EXPRESSION : 
			{
				return check_for_expression(ASTSon0(expression), st);
			}
		case AST_AMBIGUITY :
			{
				int correct_choice = -1;
				int i;
				for (i = 0; i < expression->num_ambig; i++)
				{
					if (check_for_expression(expression->ambig[i], st))
					{
						if (correct_choice < 0)
						{
							correct_choice = i;
						}
						else
						{
							// Favor known ambiguities
							AST previous_choice = expression->ambig[correct_choice];
							AST current_choice = expression->ambig[i];

							char either;
							if ((either = either_type(previous_choice, current_choice,
										AST_SIZEOF_TYPEID, AST_SIZEOF)))
							{
								if (either < 0)
								{
									correct_choice = i;
								}
							}
							// This case must be considered because the
							// AST_FUNCTION_CALL might return a valid dependent
							// expression instead of saying that this is not a
							// valid function call
							else if ((either = either_type(previous_choice, current_choice, 
										AST_EXPLICIT_TYPE_CONVERSION, AST_FUNCTION_CALL)))
							{
								if (either < 0)
								{
									correct_choice = i;
								}
							}
							else if ((either = either_type(previous_choice, current_choice,
										AST_FUNCTION_CALL, AST_GREATER_THAN)))
							{
								if (either < 0)
								{
									correct_choice = i;
								}
							}
							else
							{
								internal_error("More than one valid choice for expression (%s)\n'%s' vs '%s'\n", 
										node_information(expression), ast_print_node_type(ASTType(expression->ambig[i])), 
										ast_print_node_type(ASTType(expression->ambig[correct_choice])));
							}
						}
					}
				}

				if (correct_choice < 0)
				{
					// No ambiguity is valid
					return 0;
				}
				else
				{
					// Choose the option and state that this can be valid
					choose_option(expression, correct_choice);
					return 1;
				}
				break;
			}
			// Primaries
		case AST_DECIMAL_LITERAL :
		case AST_OCTAL_LITERAL :
		case AST_HEXADECIMAL_LITERAL :
		case AST_FLOATING_LITERAL :
		case AST_BOOLEAN_LITERAL :
		case AST_CHARACTER_LITERAL :
		case AST_STRING_LITERAL :
		case AST_THIS_VARIABLE :
			{
				return 1;
			}
		case AST_QUALIFIED_ID :
			{
				return check_for_qualified_id(expression, st);
			}
		case AST_QUALIFIED_TEMPLATE :
			{
				// This always yields a value because it must be used in a
				// template context where everything is regarded as an
				// expression unless the user "typenames" things

				AST global_scope = ASTSon0(expression);
				AST nested_name = ASTSon1(expression);
				AST template_id = ASTSon2(expression);

				query_nested_name_flags(st, global_scope, nested_name, template_id, FULL_UNQUALIFIED_LOOKUP, LF_NO_FAIL);

				return 1;
			}
		case AST_QUALIFIED_OPERATOR_FUNCTION_ID :
			{
				// This always yields a value, does not it?
				return 1;
			}
		case AST_PARENTHESIZED_EXPRESSION :
			{
				return check_for_expression(ASTSon0(expression), st);
			}
		case AST_SYMBOL :
			{
				return check_for_symbol(expression, st);
			}
		case AST_DESTRUCTOR_ID :
		case AST_DESTRUCTOR_TEMPLATE_ID :
			{
				// This is never a value since it must be invoked via pseudo destructor call
				return 0;
				// return check_for_destructor_id(expression, st);
			}
		case AST_OPERATOR_FUNCTION_ID :
			{
				// This should yield a value, should not ?
				return 1;
			}
        case AST_OPERATOR_FUNCTION_ID_TEMPLATE :
            {
				// This should yield a value, should not ?
                solve_possibly_ambiguous_template_id(expression, st);
                return 1;
            }
		case AST_CONVERSION_FUNCTION_ID :
			{
				// This should yield a value, should not ?
				return 1;
			}
		case AST_TEMPLATE_ID :
			{
				// This is never a value
				solve_possibly_ambiguous_template_id(expression, st);
				return 0;
			}
			// Postfix expressions
		case AST_ARRAY_SUBSCRIPT :
			{
				return check_for_expression(ASTSon0(expression), st)
					&& check_for_expression(ASTSon1(expression), st);
			}
		case AST_FUNCTION_CALL :
			{
				return check_for_function_call(expression, st);
			}
		case AST_EXPLICIT_TYPE_CONVERSION :
			{
				return check_for_explicit_type_conversion(expression, st);
			}
		case AST_TYPENAME_EXPLICIT_TYPE_CONVERSION :
			{
				return check_for_explicit_typename_type_conversion(expression, st);
			}
		case AST_TYPENAME_TEMPLATE :
		case AST_TYPENAME_TEMPLATE_TEMPLATE :
			{
				// This is never a value
				return 0;
			}
		case AST_CLASS_MEMBER_ACCESS :
		case AST_POINTER_CLASS_MEMBER_ACCESS :
			{
				int result = 0;
				result = check_for_expression(ASTSon0(expression), st);

				// This should always yield a value unless the right hand is shit
				if (result 
						&& (ASTType(ASTSon1(expression)) == AST_DESTRUCTOR_ID
							|| ASTType(ASTSon1(expression)) == AST_DESTRUCTOR_TEMPLATE_ID))
				{
					result = 0;
				}

				check_for_expression(ASTSon1(expression), st);
				
				return result;
			}
		case AST_CLASS_TEMPLATE_MEMBER_ACCESS :
		case AST_POINTER_CLASS_TEMPLATE_MEMBER_ACCESS :
			{
				// Think on this. Can this yield something that is not a value ?
				if (ASTType(ASTSon1(expression)) == AST_DESTRUCTOR_ID
						|| ASTType(ASTSon1(expression)) == AST_DESTRUCTOR_TEMPLATE_ID)
				{
					return 0;
				}

				check_for_expression(ASTSon0(expression), st);
				check_for_expression(ASTSon1(expression), st);

				return 1;
			}
		case AST_POSTINCREMENT :
		case AST_POSTDECREMENT :
			{
				// This cannot yield a type
				check_for_expression(ASTSon0(expression), st);
				return 1;
			}
		case AST_DYNAMIC_CAST :
		case AST_STATIC_CAST :
		case AST_REINTERPRET_CAST :
		case AST_CONST_CAST :
			{
				AST type_id = ASTSon0(expression);
				AST type_specifier = ASTSon0(type_id);
				AST abstract_declarator = ASTSon1(type_id);

				gather_decl_spec_t gather_info;
				memset(&gather_info, 0, sizeof(gather_info));

				type_t* simple_type_info = NULL;
				build_scope_decl_specifier_seq(type_specifier, st, &gather_info, &simple_type_info, 
						default_decl_context);

				if (abstract_declarator != NULL)
				{
					type_t* declarator_type = NULL;
					build_scope_declarator(abstract_declarator, st, &gather_info, simple_type_info, 
							&declarator_type, default_decl_context);
				}

				AST casted_expression = ASTSon1(expression);

				solve_possibly_ambiguous_expression(casted_expression, st);
				// This should not yield a type
				return 1;
			}
		case AST_TYPEID_TYPE :
			{
				return check_for_typeid(expression, st);
			}
		case AST_TYPEID_EXPR :
			{
				return check_for_typeid_expr(expression, st);
			}
		// Unary expressions
		case AST_PREINCREMENT :
		case AST_PREDECREMENT :
			{
				check_for_expression(ASTSon0(expression), st);
				return 1;
			}
		case AST_SIZEOF :
			{
				return check_for_sizeof_expr(expression, st);
			}
		case AST_SIZEOF_TYPEID :
			{
				return check_for_sizeof_typeid(expression, st);
			}
		case AST_DERREFERENCE :
		case AST_REFERENCE :
		case AST_PLUS_OP :
		case AST_NEG_OP :
		case AST_NOT_OP :
		case AST_COMPLEMENT_OP :
			{
				return check_for_expression(ASTSon0(expression), st);
			}
			// Cast expression
		case AST_CAST_EXPRESSION :
			{
				return check_for_cast(expression, st);
			}
		// Pointer to method expressions 
		case AST_POINTER_TO_POINTER_MEMBER :
		case AST_POINTER_TO_MEMBER :
			{
				// This should always yield a value
				return 1;
			}
		case AST_MULT_OP :
		case AST_DIV_OP :
		case AST_MOD_OP :
		case AST_ADD_OP :
		case AST_MINUS_OP :
		case AST_SHL_OP :
		case AST_SHR_OP :
		case AST_LOWER_THAN :
		case AST_GREATER_THAN :
		case AST_GREATER_OR_EQUAL_THAN :
		case AST_LOWER_OR_EQUAL_THAN :
		case AST_EQUAL_OP :
		case AST_DIFFERENT_OP :
		case AST_BITWISE_AND :
		case AST_BITWISE_XOR :
		case AST_BITWISE_OR :
		case AST_LOGICAL_AND :
		case AST_LOGICAL_OR :
			{
				return check_for_expression(ASTSon0(expression), st)
					&& check_for_expression(ASTSon1(expression), st);
			}
		case AST_CONDITIONAL_EXPRESSION :
			{
				return check_for_expression(ASTSon0(expression), st)
					&& check_for_expression(ASTSon1(expression), st)
					&& check_for_expression(ASTSon2(expression), st);
			}
		case AST_ASSIGNMENT :
		case AST_MUL_ASSIGNMENT :
		case AST_DIV_ASSIGNMENT :
		case AST_ADD_ASSIGNMENT :
		case AST_SUB_ASSIGNMENT :
		case AST_SHL_ASSIGNMENT :
		case AST_SHR_ASSIGNMENT :
		case AST_AND_ASSIGNMENT :
		case AST_OR_ASSIGNMENT :
		case AST_XOR_ASSIGNMENT :
		case AST_MOD_ASSIGNMENT :
			{
				return check_for_expression(ASTSon0(expression), st)
					&& check_for_expression(ASTSon1(expression), st);
			}
		case AST_THROW_EXPRESSION :
			{
				if (ASTSon0(expression) != NULL)
				{
					return check_for_expression(ASTSon0(expression), st);
				}
				else 
					return 1;
			}
		case AST_COMMA_OP :
			{
				return check_for_expression(ASTSon0(expression), st)
					&& check_for_expression(ASTSon1(expression), st);
			}
			// GCC Extension
		case AST_GCC_LABEL_ADDR :
			{
				// Let's assume this is correct
				return 1;
			}
		case AST_GCC_REAL_PART :
		case AST_GCC_IMAG_PART :
		case AST_GCC_EXTENSION_EXPR :
			{
				return check_for_expression(ASTSon0(expression), st);
			}
		case AST_GCC_ALIGNOF :
			{
				// Reuse the sizeof code
				return check_for_sizeof_expr(expression, st);
				break;
			}
		case AST_GCC_ALIGNOF_TYPE :
			{
				// Reuse the sizeof code
				return check_for_sizeof_typeid(expression, st);
				break;
			}
		case AST_NEW_EXPRESSION :
			{
				// This is always a value, never a type
				return check_for_new_expression(expression, st);
				break;
			}
		case AST_NEW_TYPE_ID_EXPR :
			{
				return check_for_new_type_id_expr(expression, st);
			}
		case AST_DELETE_EXPR :
		case AST_DELETE_ARRAY_EXPR :
			{
				// This is always a value, never a type
				check_for_expression(ASTSon1(expression), st);
				return 1;
				break;
			}
		case AST_PSEUDO_DESTRUCTOR_CALL :
		case AST_POINTER_PSEUDO_DESTRUCTOR_CALL :
			{
				return 1;
				break;
			}
		default :
			{
				internal_error("Unexpected node '%s' %s", ast_print_node_type(ASTType(expression)), 
				node_information(expression));
				break;
			}
	}
}

static char check_for_new_expression(AST new_expr, scope_t* st)
{
	// AST global_op = ASTSon0(new_expr);
	AST new_placement = ASTSon1(new_expr);
	AST new_type_id = ASTSon2(new_expr);
	AST new_initializer = ASTSon3(new_expr);

	if (new_placement != NULL)
	{
		AST expression_list = ASTSon0(new_placement);
		AST iter;

		for_each_element(expression_list, iter)
		{
			AST expression = ASTSon1(iter);

			solve_possibly_ambiguous_expression(expression, st);
		}
	}

	AST type_specifier_seq = ASTSon0(new_type_id);
	AST new_declarator = ASTSon1(new_type_id);

	type_t* dummy_type;
	gather_decl_spec_t gather_info;
	memset(&gather_info, 0, sizeof(gather_info));

	build_scope_decl_specifier_seq(type_specifier_seq, st, &gather_info, &dummy_type, default_decl_context);

	if (new_declarator != NULL)
	{
		type_t* declarator_type = NULL;
		build_scope_declarator(new_declarator, st, &gather_info, dummy_type, &declarator_type, default_decl_context);
	}

	if (new_initializer != NULL)
	{
		AST expression_list = ASTSon0(new_initializer);
		
		if (expression_list != NULL)
		{
			AST iter;

			for_each_element(expression_list, iter)
			{
				AST expression = ASTSon1(iter);

				solve_possibly_ambiguous_expression(expression, st);
			}
		}
	}

	// This is a bit bogus
	return 1;
}

static char check_for_new_type_id_expr(AST new_expr, scope_t* st)
{
	return check_for_new_expression(new_expr, st);
}

void solve_possibly_ambiguous_expression(AST a, scope_t* st)
{
	check_for_expression(a, st);
}

static char check_for_expression_statement(AST a, scope_t* st)
{
	AST expression = ASTSon0(a);
	return check_for_expression(expression, st);
}

#define ENSURE_TYPE(expr, type) \
do { \
	if (ASTType(expr) != type) \
	{ \
		internal_error("Expecting node of type '"#type"' but got '%s'\n", ast_print_node_type(ASTType(expr))); \
	} \
} \
while (0);

static char check_for_qualified_id(AST expr, scope_t* st)
{
	AST global_scope = ASTSon0(expr);
	AST nested_name_spec = ASTSon1(expr);
	AST unqualified_object = ASTSon2(expr);

	scope_entry_list_t* result_list = query_nested_name_flags(st, global_scope, nested_name_spec, 
			unqualified_object, FULL_UNQUALIFIED_LOOKUP, LF_EXPRESSION | LF_NO_FAIL);

	if (ASTType(unqualified_object) == AST_TEMPLATE_ID)
	{
		return (result_list != NULL
				&& (result_list->entry->kind == SK_TEMPLATE_FUNCTION));
	}
	else
	{
		return (result_list != NULL
				&& (result_list->entry->kind == SK_VARIABLE
					|| result_list->entry->kind == SK_ENUMERATOR
					|| result_list->entry->kind == SK_FUNCTION
					|| result_list->entry->kind == SK_TEMPLATE_FUNCTION
					|| result_list->entry->kind == SK_DEPENDENT_ENTITY));
	}
}

static char check_for_symbol(AST expr, scope_t* st)
{
	ENSURE_TYPE(expr, AST_SYMBOL);
	scope_entry_list_t* result = query_unqualified_name(st, ASTText(expr)); 

	return (result != NULL 
			&& (result->entry->kind == SK_VARIABLE
				|| result->entry->kind == SK_ENUMERATOR
				|| result->entry->kind == SK_FUNCTION
				|| result->entry->kind == SK_TEMPLATE_PARAMETER));
}

#if 0
static char check_for_destructor_id(AST expr, scope_t* st)
{
	// ENSURE_TYPE(expr, AST_DESTRUCTOR_ID);
	if (ASTType(expr) != AST_DESTRUCTOR_ID
			&& ASTType(expr) != AST_DESTRUCTOR_TEMPLATE_ID)
	{
		internal_error("Expecting node of AST_DESTRUCTOR_{ID|TEMPLATE_ID} but got '%s'\n", ast_print_node_type(ASTType(expr)));
	}

	char* class_name = ASTText(expr);
	// Spring ~
	class_name++;
	scope_entry_list_t* result_list = query_unqualified_name(st, class_name);

	if (result_list == NULL)
	{
		return 0;
	}

	type_t* type_result = result_list->entry->type_information;

	if (type_result->kind != TK_DIRECT)
	{
		return 0;
	}

	// Advance over typedefs
	type_result = advance_over_typedefs(type_result);

	if (type_result->type->kind == STK_USER_DEFINED)
	{
		type_result = type_result->type->user_defined_type->type_information;
	}

	return (type_result->kind == TK_DIRECT
			&& (type_result->type->kind == STK_CLASS
				// Dubious without exact instantiation
				|| type_result->type->kind == STK_TYPE_TEMPLATE_PARAMETER));
}
#endif

static char check_for_functional_expression(AST expr, AST arguments, scope_t* st)
{
	char result = 0;
	switch(ASTType(expr))
	{
		case AST_SYMBOL :
		case AST_TEMPLATE_ID :
			{
// #warning Koenig lookup should be performed here
				// The technique here relies on finding something that is a
				// function.  If nothing is found, assume also that is a
				// function that would need full Koenig lookup for correct
				// lookup
				scope_entry_list_t* function_lookup = NULL;
				char* name;
				if (ASTType(expr) == AST_SYMBOL)
				{
					name = ASTText(expr);
				}
				else
				{
					solve_possibly_ambiguous_template_id(expr, st);
					name = ASTText(ASTSon0(expr));
				}

				function_lookup = query_unqualified_name(st, name);

				if (function_lookup == NULL)
				{
					// Assume this will name a function
					// WARNING_MESSAGE("Assuming symbol '%s' not found to be a function\n", name)
					result = 1;
				}

				enum cxx_symbol_kind filter_funct[3] =
				{ 
					SK_VARIABLE, // for "operator()" on objects
					SK_FUNCTION,
					SK_TEMPLATE_FUNCTION
				};
				function_lookup = filter_symbol_kind_set(function_lookup, 3, filter_funct);

				// We have found some function
				if (function_lookup != NULL)
				{
					result = 1;
				}

#if 0
				// Koenig lookup here!
				scope_entry_list_t* function_lookup = NULL;
				function_lookup = lookup_unqualified_function(st, ASTText(expr), arguments);

				enum cxx_symbol_kind filter_funct[2] =
				{ 
					SK_FUNCTION,
					SK_TEMPLATE_FUNCTION
				};
				function_lookup = filter_symbol_kind_set(function_lookup, 2, filter_funct);

				if (function_lookup != NULL)
				{
					result = 1;
				}
#endif
				break;
			}
		case AST_PARENTHESIZED_EXPRESSION :
			{
				result = check_for_functional_expression(ASTSon0(expr), arguments, st);
				break;
			}
		default :
			{
				result = check_for_expression(expr, st);
			}
	}

	if (result)
	{
		if (arguments != NULL)
		{
			if (ASTType(arguments) == AST_AMBIGUITY)
			{
				solve_ambiguous_expression_list(arguments, st);
			}

			AST list = arguments;
			AST iter;

			for_each_element(list, iter)
			{
				AST parameter_expr = ASTSon1(iter);
				solve_possibly_ambiguous_expression(parameter_expr, st);
			}
		}
	}

	return result;
}

static char check_for_function_call(AST expr, scope_t* st)
{
	ENSURE_TYPE(expr, AST_FUNCTION_CALL);
	
	// A function call is of the form
	//   f ( e );
	//
	// f has to yield a valid value or functional
	return check_for_functional_expression(ASTSon0(expr), ASTSon1(expr), st);
}

static char check_for_explicit_typename_type_conversion(AST expr, scope_t* st)
{
	AST global_op = ASTSon0(expr);
	AST nested_name_spec = ASTSon1(expr);
	AST symbol = ASTSon2(expr);

	scope_entry_list_t* entry_list = query_nested_name_flags(st, global_op, nested_name_spec, symbol, 
			FULL_UNQUALIFIED_LOOKUP, LF_NONE);

	if (entry_list == NULL)
	{
		return 0;
	}

	AST list = ASTSon3(expr);
	if (list != NULL)
	{
		AST iter;

		if (ASTType(list) == AST_AMBIGUITY)
		{
			solve_ambiguous_expression_list(list, st);
		}

		for_each_element(list, iter)
		{
			AST expression = ASTSon1(iter);

			solve_possibly_ambiguous_expression(expression, st);
		}
	}

	return 1;
}

static char check_for_explicit_type_conversion(AST expr, scope_t* st)
{
	// An explicit type conversion is of the form
	//
	//   T ( e );
	//
	// T has to be a valid typename
	char result = 0;
	AST simple_type_spec = ASTSon0(expr);

	result = check_for_simple_type_spec(simple_type_spec, st);

	if (result)
	{
		AST expression_list = ASTSon1(expr);

		if (expression_list != NULL)
		{
			AST iter;
			for_each_element(expression_list, iter)
			{
				AST current_expression = ASTSon1(iter);

				solve_possibly_ambiguous_expression(current_expression, st);
			}
		}
	}

	return result;
}

void solve_ambiguous_template_argument(AST ambig_template_argument, scope_t* st)
{
	int i;

	int selected_option = -1;
	for (i = 0; i < ambig_template_argument->num_ambig; i++)
	{
		char current_option = 0;
		AST current_template_argument = ambig_template_argument->ambig[i];

		switch (ASTType(current_template_argument))
		{
			case AST_TEMPLATE_TYPE_ARGUMENT :
				{
					AST type_id = ASTSon0(current_template_argument);

					// Recursively fix if needed
					AST type_id_type_spec = ASTSon0(type_id);
					AST type_id_simple_type_spec = ASTSon1(type_id_type_spec);
					
					if (ASTSon2(type_id_simple_type_spec) != NULL
							&& ASTType(ASTSon2(type_id_simple_type_spec)) == AST_TEMPLATE_ID)
					{
						solve_possibly_ambiguous_template_id(ASTSon2(type_id_simple_type_spec), st);
					}

					current_option = check_for_type_id_tree(type_id, st);
					break;
				}
			case AST_TEMPLATE_EXPRESSION_ARGUMENT :
				{
					AST expression_arg = ASTSon0(current_template_argument);
					current_option = check_for_expression(expression_arg, st);
					break;
				}
			default :
				internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(current_template_argument)));
		}
		
		if (current_option)
		{
			if (selected_option < 0)
			{
				selected_option = i;
			}
			else
			{
				AST previous_template_argument = ambig_template_argument->ambig[selected_option];

				char either;
				if ((either = either_type(previous_template_argument, current_template_argument, 
								AST_TEMPLATE_TYPE_ARGUMENT, AST_TEMPLATE_EXPRESSION_ARGUMENT)))
				{
					if (either < 0)
					{
						selected_option = i;
					}
				}
				else
				{
					internal_error("Two valid ambiguities", 0);
				}
			}
		}
	}

	if (selected_option < 0)
	{
		DEBUG_CODE()
		{
			fprintf(stderr, "Template argument '");
			prettyprint(stderr, ambig_template_argument);
			fprintf(stderr, "'\n");
		}
		internal_error("No valid choice found! %s", node_information(ambig_template_argument));
	}
	else
	{
		choose_option(ambig_template_argument, selected_option);
	}

}

void solve_possibly_ambiguous_template_id(AST type_name, scope_t* st)
{
	if (ASTType(type_name) != AST_TEMPLATE_ID
            && ASTType(type_name) != AST_OPERATOR_FUNCTION_ID_TEMPLATE)
	{
		internal_error("Unexpected node '%s' only AST_TEMPLATE_ID or AST_OPERATOR_FUNCTION_ID_TEMPLATE allowed", 0);
	}

	// For every argument solve its possible ambiguities
	AST argument_list = ASTSon1(type_name);
	if (argument_list != NULL)
	{
		AST iter;
		for_each_element(argument_list, iter)
		{
			AST template_argument = ASTSon1(iter);

			if (ASTType(template_argument) == AST_AMBIGUITY)
			{
				solve_ambiguous_template_argument(template_argument, st);
			}

			if (ASTType(template_argument) == AST_TEMPLATE_EXPRESSION_ARGUMENT)
			{
				solve_possibly_ambiguous_expression(ASTSon0(template_argument), st);
			}
			else if (ASTType(template_argument) == AST_TEMPLATE_TYPE_ARGUMENT)
			{
				AST type_id = ASTSon0(template_argument);

				AST type_specifier = ASTSon0(type_id);
				AST abstract_declarator = ASTSon1(type_id);

				gather_decl_spec_t gather_info;
				memset(&gather_info, 0, sizeof(gather_info));

				type_t* simple_type_info = NULL;
				build_scope_decl_specifier_seq(type_specifier, st, &gather_info, &simple_type_info, 
						default_decl_context);

				if (abstract_declarator != NULL)
				{
					type_t* declarator_type = NULL;
					build_scope_declarator(abstract_declarator, st, &gather_info, simple_type_info, 
							&declarator_type, default_decl_context);
				}
			}
		}
	}
}

static char check_for_simple_type_spec(AST type_spec, scope_t* st)
{
	if (ASTType(type_spec) != AST_SIMPLE_TYPE_SPECIFIER)
	{
		switch (ASTType(type_spec))
		{
			case AST_CHAR_TYPE :
			case AST_INT_TYPE:
			case AST_FLOAT_TYPE :
			case AST_DOUBLE_TYPE :
			case AST_LONG_TYPE :
			case AST_SHORT_TYPE :
			case AST_SIGNED_TYPE :
			case AST_UNSIGNED_TYPE :
			case AST_WCHAR_TYPE :
			case AST_VOID_TYPE :
			case AST_BOOL_TYPE :
				return 1;
				break;
			default :
				internal_error("Unexpected node '%s'\n", ast_print_node_type(ASTType(type_spec)));
		}
	}

	AST global_op = ASTSon0(type_spec);
	AST nested_name_spec = ASTSon1(type_spec);
	AST type_name = ASTSon2(type_spec) != NULL ? ASTSon2(type_spec) : ASTSon3(type_spec);

	if (ASTType(type_name) == AST_TEMPLATE_ID)
	{
		solve_possibly_ambiguous_template_id(type_name, st);
	}

	scope_entry_list_t* entry_list = query_nested_name_flags(st, global_op, nested_name_spec, 
            type_name, FULL_UNQUALIFIED_LOOKUP, LF_NO_FAIL);

	if (entry_list == NULL)
	{
		return 0;
	}

	return (entry_list->entry->kind == SK_TYPEDEF
			|| entry_list->entry->kind == SK_ENUM
			|| entry_list->entry->kind == SK_CLASS
			|| entry_list->entry->kind == SK_TEMPLATE_PRIMARY_CLASS
			|| entry_list->entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS
			|| entry_list->entry->kind == SK_TEMPLATE_TYPE_PARAMETER);
}

static char check_for_typeid(AST expr, scope_t* st)
{
	return check_for_type_id_tree(ASTSon0(expr), st);
}

static char check_for_type_id_tree(AST type_id, scope_t* st)
{
	AST type_specifier_seq = ASTSon0(type_id);
	// AST abstract_declarator = ASTSon1(type_id);
	
	if (ASTType(type_specifier_seq) == AST_AMBIGUITY)
	{
		solve_ambiguous_decl_specifier_seq(type_specifier_seq, st);
	}
	
	// This is never NULL
	AST type_specifier = ASTSon1(type_specifier_seq);

	return check_for_type_specifier(type_specifier, st);
}

static char check_for_typeid_expr(AST expr, scope_t* st)
{
	AST expression = ASTSon0(expr);
	return check_for_expression(expression, st);
}

static char check_for_type_specifier(AST type_id, scope_t* st)
{
	switch (ASTType(type_id))
	{
		case AST_SIMPLE_TYPE_SPECIFIER :
			return check_for_simple_type_spec(type_id, st);
			break;
		case AST_CLASS_SPECIFIER :
		case AST_ENUM_SPECIFIER :
        case AST_ELABORATED_TYPENAME :
        case AST_ELABORATED_TYPENAME_TEMPLATE :
		case AST_ELABORATED_TYPE_ENUM :
		case AST_ELABORATED_TYPE_CLASS :
		case AST_ELABORATED_TYPE_TEMPLATE_CLASS :
		case AST_ELABORATED_TYPE_TEMPLATE_TEMPLATE_CLASS :
		case AST_CHAR_TYPE :
		case AST_WCHAR_TYPE :
		case AST_BOOL_TYPE :
		case AST_SHORT_TYPE :
		case AST_LONG_TYPE :
		case AST_SIGNED_TYPE :
		case AST_UNSIGNED_TYPE :
		case AST_INT_TYPE :
		case AST_DOUBLE_TYPE :
		case AST_FLOAT_TYPE :
		case AST_VOID_TYPE :
			{
				return 1;
			}
		default :
			{
				internal_error("Unexpected node '%s'\n", ast_print_node_type(ASTType(type_id)));
			}
	}
}

static char check_for_sizeof_expr(AST expr, scope_t* st)
{
	AST sizeof_expression = ASTSon0(expr);
	return check_for_expression(sizeof_expression, st);
}

static char check_for_sizeof_typeid(AST expr, scope_t* st)
{
	AST type_id = ASTSon0(expr);
	return check_for_type_id_tree(type_id, st);
}

static char check_for_cast(AST expr, scope_t* st)
{
	AST type_id = ASTSon0(expr);
	if (check_for_type_id_tree(type_id, st))
	{
		AST type_specifier = ASTSon0(type_id);
		AST abstract_declarator = ASTSon1(type_id);

		gather_decl_spec_t gather_info;
		memset(&gather_info, 0, sizeof(gather_info));

		type_t* simple_type_info = NULL;
		build_scope_decl_specifier_seq(type_specifier, st, &gather_info, &simple_type_info, 
				default_decl_context);

		if (abstract_declarator != NULL)
		{
			type_t* declarator_type = NULL;
			build_scope_declarator(abstract_declarator, st, &gather_info, simple_type_info, 
					&declarator_type, default_decl_context);
		}

		solve_possibly_ambiguous_expression(ASTSon1(expr), st);

		return 1;
	}
	else
	{
		return 0;
	}
}

void solve_ambiguous_init_declarator(AST a, scope_t* st)
{
	int correct_choice = -1;
	int i;

	for (i = 0; i < a->num_ambig; i++)
	{
		AST init_declarator = a->ambig[i];

		if (check_for_init_declarator(init_declarator, st))
		{
			if (correct_choice < 0)
			{
				correct_choice = i;
			}
			else
			{
				// Ambiguity: T t(Q()); where T and Q are type-names always solves to 
				// function declaration

				AST previous_choice = a->ambig[correct_choice];
				AST previous_choice_declarator = ASTSon0(previous_choice);

				AST current_choice_declarator = ASTSon0(init_declarator);

				char either;
				if ((either = either_type(ASTSon0(previous_choice_declarator), ASTSon0(current_choice_declarator), 
							AST_DECLARATOR_FUNC, AST_DECLARATOR_ID_EXPR)))
				{
					// Always favor function declarations
					if (either < 0)
					{
						correct_choice = i;
					}
				}
				else
				{
					internal_error("More than one valid choice!\n", 0);
				}
			}
		}
	}

	if (correct_choice < 0)
	{
		internal_error("Unsolved ambiguity\n", 0);
	}
	else
	{
		choose_option(a, correct_choice);
	}
}

static char check_for_init_declarator(AST init_declarator, scope_t* st)
{
	AST declarator = ASTSon0(init_declarator);
	AST initializer = ASTSon1(init_declarator);

	if (!check_for_declarator(declarator, st))
		return 0;

	if (initializer != NULL)
	{
		if (!check_for_initialization(initializer, st))
			return 0;
	}

	return 1;
}

static char check_for_initializer_list(AST initializer_list, scope_t* st)
{
	if (ASTType(initializer_list) == AST_AMBIGUITY)
	{
		int current_choice = -1;
		int i;

		for (i = 0; i < initializer_list->num_ambig; i++)
		{
			if (check_for_initializer_list(initializer_list->ambig[i], st))
			{
				if (current_choice < 0)
				{
					current_choice = i;
				}
				else
				{
					internal_error("More than one valid alternative", 0);
				}
			}
		}

		if (current_choice < 0)
		{
			return 0;
		}
		else
		{
			choose_option(initializer_list, current_choice);
			return 1;
		}
	}
	else if (ASTType(initializer_list) == AST_NODE_LIST)
	{
		AST expression = ASTSon1(initializer_list);

		if (!check_for_expression(expression, st))
		{
			return 0;
		}

		// Recurse, because there may be additional ambiguities lying around here
		if (ASTSon0(initializer_list) != NULL)
		{
			return check_for_initializer_list(ASTSon0(initializer_list), st);
		}
		else
		{
			return 1;
		}
	}
	else
	{
		internal_error("Unknown node '%s' %s", 
				ast_print_node_type(ASTType(initializer_list)), node_information(initializer_list));

		return 0;
	}
}

char check_for_initialization(AST initializer, scope_t* st)
{
	switch (ASTType(initializer))
	{
		case AST_CONSTANT_INITIALIZER :
			{
				AST expression = ASTSon0(initializer);
				return check_for_expression(expression, st);
				break;
			}
		case AST_INITIALIZER :
			{
				AST initializer_clause = ASTSon0(initializer);
				return check_for_initializer_clause(initializer_clause, st);
				break;
			}
		case AST_PARENTHESIZED_INITIALIZER :
			{
				return check_for_initializer_list(ASTSon0(initializer), st);
				break;
			}
		default :
			{
				internal_error("Unexpected node type '%s'\n", ast_print_node_type(ASTType(initializer)));
			}
	}
	return 1;
}

static char check_for_initializer_clause(AST initializer, scope_t* st)
{
	switch (ASTType(initializer))
	{
		case AST_INITIALIZER_BRACES :
			{
				// This is never ambiguous
				AST expression_list = ASTSon0(initializer);
				AST iter;
				for_each_element(expression_list, iter)
				{
					AST initializer_clause = ASTSon1(iter);

					check_for_initializer_clause(initializer_clause, st);
				}
				return 1;
			}
		case AST_INITIALIZER_EXPR :
			{
				AST expression = ASTSon0(initializer);
				return check_for_expression(expression, st);
				break;
			}
		default :
			{
				internal_error("Unexpected node type '%s'\n", ast_print_node_type(ASTType(initializer)));
			}
	}
}

static char check_for_declarator(AST declarator, scope_t* st)
{
	return check_for_declarator_rec(declarator, st);
}

static char check_for_declarator_rec(AST declarator, scope_t* st)
{
	switch (ASTType(declarator))
	{
		case AST_ABSTRACT_DECLARATOR :
			{
				return check_for_declarator_rec(ASTSon1(declarator), st);
				break;
			}
		case AST_DECLARATOR_ARRAY :
		case AST_ABSTRACT_ARRAY :
			{
				if (ASTSon0(declarator) != NULL)
				{
					solve_possibly_ambiguous_expression(ASTSon0(declarator), st);
				}
				return check_for_declarator_rec(ASTSon0(declarator), st);
			}
		case AST_PARENTHESIZED_ABSTRACT_DECLARATOR :
		case AST_PARENTHESIZED_DECLARATOR :
		case AST_DECLARATOR :
			{
				return check_for_declarator_rec(ASTSon0(declarator), st);
				break;
			}
		case AST_POINTER_DECL :
			{
				return check_for_declarator_rec(ASTSon1(declarator), st);
				break;
			}
		case AST_ABSTRACT_DECLARATOR_FUNC :
		case AST_DECLARATOR_FUNC :
			{
				// Check for parameters here
				AST parameter_declaration_clause = ASTSon1(declarator);
				if (parameter_declaration_clause != NULL)
				{
					if (!check_for_function_declarator_parameters(parameter_declaration_clause, st))
					{
						return 0;
					}
				}
				if (ASTSon0(declarator) != NULL)
				{
					return check_for_declarator_rec(ASTSon0(declarator), st);
				}
				break;
			}
		case AST_DECLARATOR_ID_EXPR :
			{
				// Is this already correct or we have to check something else ?
				return 1;
				break;
			}
		default :
			{
				internal_error("Unexpected node type '%s'\n", ast_print_node_type(ASTType(declarator)));
				break;
			}
	}

	return 0;
}

static char check_for_function_declarator_parameters(AST parameter_declaration_clause, scope_t* st)
{
	AST list = parameter_declaration_clause;
	AST iter;

	if (ASTType(parameter_declaration_clause) == AST_EMPTY_PARAMETER_DECLARATION_CLAUSE)
	{
		return 1;
	}

	for_each_element(list, iter)
	{
		AST parameter = ASTSon1(iter);

		if (ASTType(parameter) == AST_VARIADIC_ARG)
		{
			continue;
		}

		if (ASTType(parameter) == AST_AMBIGUITY)
		{
			int correct_choice = -1;
			int i;
			for (i = 0; i < parameter->num_ambig; i++)
			{
				AST parameter_decl = parameter->ambig[i];

				AST decl_specifier_seq = ASTSon0(parameter_decl);
				AST type_specifier = ASTSon1(decl_specifier_seq);
				AST declarator = ASTSon1(parameter_decl);

				char seems_ok = 1;

				seems_ok &= check_for_type_specifier(type_specifier, st);

				if (seems_ok && declarator != NULL)
				{
					seems_ok &= check_for_declarator(declarator, st);
					seems_ok &= check_for_decl_spec_seq_followed_by_declarator(decl_specifier_seq, declarator);
				}

				if (seems_ok)
				{
					if (correct_choice < 0)
					{
						correct_choice = i;
					}
					else
					{
						AST current_choice = parameter_decl;
						AST previous_choice = parameter->ambig[correct_choice];
						ast_dump_graphviz(previous_choice, stderr);
						ast_dump_graphviz(current_choice, stderr);
						internal_error("More than one valid alternative '%s' vs '%s' %s", 
								ast_print_node_type(ASTType(previous_choice)),
								ast_print_node_type(ASTType(current_choice)),
								node_information(previous_choice));
					}
				}
			}

			if (correct_choice < 0)
			{
				return 0;
			}
			else
			{
				choose_option(parameter, correct_choice);
			}
		}

		if (ASTType(parameter) != AST_PARAMETER_DECL
                && ASTType(parameter) != AST_GCC_PARAMETER_DECL)
		{
			internal_error("Unexpected node '%s'\n", ast_print_node_type(ASTType(parameter)));
		}

		AST decl_specifier_seq = ASTSon0(parameter);
		AST abstract_declarator = ASTSon1(parameter);

		if (ASTType(decl_specifier_seq) == AST_AMBIGUITY)
		{
			// Ensure that this is the unique ambiguity than can appear here
			solve_ambiguous_decl_specifier_seq(decl_specifier_seq, st);
		}

		AST type_specifier = ASTSon1(decl_specifier_seq);

		if (!check_for_type_specifier(type_specifier, st))
		{
			return 0;
		}

		if (abstract_declarator != NULL)
		{
			if (!check_for_declarator(abstract_declarator, st))
			{
				return 0;
			}
		}

		AST default_arg = ASTSon2(parameter);

		if (default_arg != NULL)
		{
			check_for_expression(default_arg, st);
		}
	}

	return 1;
}

void solve_ambiguous_parameter_decl(AST parameter_declaration, scope_t* st)
{
	int current_choice = -1;
	int i;
	for (i = 0; i < parameter_declaration->num_ambig; i++)
	{
		char current_valid = 1;
		AST parameter_decl = parameter_declaration->ambig[i];

		AST decl_specifier_seq = ASTSon0(parameter_decl);

		if (ASTType(decl_specifier_seq) == AST_AMBIGUITY)
		{
			solve_ambiguous_decl_specifier_seq(decl_specifier_seq, st);
		}

		AST type_specifier = ASTSon1(decl_specifier_seq);

		if (type_specifier != NULL)
		{
			current_valid &= check_for_type_specifier(type_specifier, st);
		}
		else
		{
			// There must be type_spec in a parameter_decl
			current_valid = 0;
		}

		AST declarator = ASTSon1(parameter_decl);

		if (declarator != NULL)
		{
			current_valid &= check_for_decl_spec_seq_followed_by_declarator(decl_specifier_seq, declarator);
		}

		if (current_valid)
		{
			if (current_choice < 0)
			{
				current_choice = i;
			}
			else
			{
				internal_error("More than one option is possible in %s", node_information(parameter_declaration));
			}
		}
	}

	if (current_choice < 0)
	{
		internal_error("Ambiguity not solved", 0);
	}
	else
	{
		choose_option(parameter_declaration, current_choice);
	}
}

// Convencience function name
void solve_ambiguous_type_specifier_seq(AST type_spec_seq, scope_t* st)
{
	solve_ambiguous_decl_specifier_seq(type_spec_seq, st);
}

void solve_ambiguous_decl_specifier_seq(AST type_spec_seq, scope_t* st)
{
	// What makes different a type_specifier_seq from a decl_specifier_seq
	// is the fact that a type_specifier always has a type_spec while
	// a decl_specifier_seq might not.
	//
	// The unique ambiguity that can appear in a type_spec_seq is the 
	//
	//     unsigned long a; 
	//
	// ambiguity

	int i;
	char integral_ambiguity = 1;

	for (i = 0; i < type_spec_seq->num_ambig; i++)
	{
		AST type_specifier_seq = type_spec_seq->ambig[i];

		if (ASTType(type_specifier_seq) != AST_TYPE_SPECIFIER_SEQ
				&& ASTType(type_specifier_seq) != AST_DECL_SPECIFIER_SEQ)
		{
			internal_error("Invalid node type '%s'\n", ast_print_node_type(ASTType(type_specifier_seq)));
		}

		AST type_specifier = ASTSon1(type_specifier_seq);

		if (type_specifier != NULL)
		{
			if (ASTType(type_specifier) != AST_SIGNED_TYPE
					&& ASTType(type_specifier) != AST_UNSIGNED_TYPE
					&& ASTType(type_specifier) != AST_LONG_TYPE
					&& ASTType(type_specifier) != AST_SHORT_TYPE)
			{
				integral_ambiguity = 0;
				break;
			}
		}
	}

	if (!integral_ambiguity)
	{
		internal_error("Unknown ambiguity\n", 0);
	}
	else
	{
		// This is a bit different from solve_integral_specification_ambig
		// Choose the first one that has type_specifier
		for (i = 0; i < type_spec_seq->num_ambig; i++)
		{
			AST type_specifier_seq = type_spec_seq->ambig[i];
			
			if (ASTSon1(type_specifier_seq) != NULL)
			{
				choose_option(type_spec_seq, i);
				break;
			}
		}
	}
}

void solve_ambiguous_for_init_statement(AST a, scope_t* st)
{
	int correct_choice = -1;
	int i;
	for (i = 0; i < a->num_ambig; i++)
	{
		int current = 0;
		AST for_init_statement = a->ambig[i];

		switch (ASTType(for_init_statement))
		{
			case AST_SIMPLE_DECLARATION :
				if (check_for_simple_declaration(for_init_statement, st))
				{
					current = 1;
				}
				break;
			case AST_EXPRESSION_STATEMENT :
				if (check_for_expression(ASTSon0(for_init_statement), st))
				{
					current = 1;
				}
				break;
			default :
				internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(for_init_statement)));
		}

		if (current)
		{
			if (correct_choice < 0)
			{
				correct_choice = i;
			}
			else
			{
				internal_error("More than one valid choice! %s vs %s\n", ast_print_node_type(ASTType(a->ambig[i])),
						ast_print_node_type(ASTType(a->ambig[correct_choice])));
			}
		}
	}

	if (correct_choice < 0)
	{
		internal_error("Ambiguity not solved !", 0);
	}
	else
	{
		choose_option(a, correct_choice);
	}
}

void solve_ambiguous_type_specifier(AST ambig_type, scope_t* st)
{
	// The unique ambiguity that should happen here is the one below
	//
	//   __typeof(foo) bar;
	//
	// We don't know if foo is a type or an expression
	
	char is_typeof_ambiguity = 1;
	int i;
	for (i = 0; (i < ambig_type->num_ambig) && is_typeof_ambiguity; i++)
	{
		AST type_specifier = ambig_type->ambig[i];

		is_typeof_ambiguity = ((ASTType(type_specifier) == AST_GCC_TYPEOF)
				|| (ASTType(type_specifier) == AST_GCC_TYPEOF_EXPR));
	}

	if (!is_typeof_ambiguity)
	{
		internal_error("Unknown ambiguity!\n", 0);
	}

	// Solve typeof ambiguity
	int typeof_choice = -1;
	for (i = 0; i < ambig_type->num_ambig; i++)
	{
		char current_typeof = 0;
		AST type_specifier = ambig_type->ambig[i];
		AST typeof_argument = ASTSon0(type_specifier);

		if (ASTType(type_specifier) == AST_GCC_TYPEOF)
		{
			current_typeof = check_for_type_id_tree(typeof_argument, st);
		}
		else if (ASTType(type_specifier) == AST_GCC_TYPEOF_EXPR)
		{
			current_typeof = check_for_expression(typeof_argument, st);
		}
		else
		{
			internal_error("Unexpected node type %s\n", ast_print_node_type(ASTType(type_specifier)));
		}

		if (current_typeof)
		{
			if (typeof_choice < 0)
			{
				typeof_choice = i;
			}
			else
			{
				internal_error("More than one possibility", 0);
			}
		}
	}

	if (typeof_choice < 0)
	{
		internal_error("Ambiguity not solved", 0);
	}
	else
	{
		choose_option(ambig_type, typeof_choice);
	}
}

void solve_ambiguous_expression_list(AST expression_list, scope_t* st)
{
	int correct_choice = -1;
	int i;
	char result = 1;
	for (i = 0; i < expression_list->num_ambig; i++)
	{
		AST current_expression_list = expression_list->ambig[i];
		AST iter;

		for_each_element(current_expression_list, iter)
		{
			AST current_expression = ASTSon1(iter);

			result &= check_for_expression(current_expression, st);
		}

		if (result)
		{
			if (correct_choice < 0)
			{
				correct_choice = i;
			}
			else
			{
				AST previous_choice = expression_list->ambig[correct_choice];
				AST current_choice = expression_list->ambig[i];
				internal_error("More than one valid alternative '%s' vs '%s'", 
						ast_print_node_type(ASTType(previous_choice)),
						ast_print_node_type(ASTType(current_choice)));
			}
		}
	}

	if (correct_choice < 0)
	{
		internal_error("Ambiguity not solved", 0);
	}
	else
	{
		choose_option(expression_list, correct_choice);
	}
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

	AST parent = ASTParent(a);
	// int num_children = get_children_num(parent, a);

	// if (num_children < 0)
	// {
	// 	internal_error("Children not found in the parent!\n", 0);
	// }

	DEBUG_CODE()
	{
		fprintf(stderr, "*** Choosing '%s' in the ambiguity tree %p (%s) using %p\n", 
				ast_print_node_type(ASTType(a->ambig[n])), a, node_information(a->ambig[n]), 
				a->ambig[n]);
	}

	if (!ASTCheck(a->ambig[n]))
	{
		internal_error("*** INCONSISTENT TREE DETECTED *** %p\n", a->ambig[n]);
	}
	
	// This will work, trust on me :)
	*a = *(duplicate_ast(a->ambig[n]));

	// Correctly relink to the parent
	ASTParent(a) = parent;

	int i;
	for (i = 0; i < ASTNumChildren(a); i++)
	{
		if (ASTChild(a, i) != NULL)
		{
			AST child = ASTChild(a, i);

			ASTParent(child) = a;
		}
	}

	if (!ASTCheck(a))
	{
		internal_error("*** INCONSISTENT TREE DETECTED ***\n", a);
	}
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
