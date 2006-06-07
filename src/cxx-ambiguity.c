#include <stdio.h>
#include "cxx-ambiguity.h"
#include "cxx-utils.h"
#include "cxx-koenig.h"

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

static char check_for_declaration_statement(AST a, scope_t* st);
static char check_for_expression(AST expression, scope_t* st);
static char check_for_expression_statement(AST a, scope_t* st);
static char check_for_qualified_id(AST expr, scope_t* st);
static char check_for_symbol(AST expr, scope_t* st);
static char check_for_destructor_id(AST expr, scope_t* st);
static char check_for_function_call(AST expr, scope_t* st);
static char check_for_explicit_type_conversion(AST expr, scope_t* st);
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
static char check_for_initialization(AST initializer, scope_t* st);

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
#warning TODO - Refactorize this with the code that makes the same with a single decl_specifier_seq/type_specifier_seq
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
				}
			}
			else
			{
				// If it is not ambiguous, then it cannot have declarators at all
				AST init_declarator_list = ASTSon1(option);
				valid = (init_declarator_list == NULL);
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

				if ((ASTType(first_option) == AST_SIMPLE_DECLARATION
						&& ASTType(second_option) == AST_EXPLICIT_TYPE_CONVERSION)
						|| (ASTType(first_option) == AST_EXPLICIT_TYPE_CONVERSION
							&& ASTType(second_option) == AST_SIMPLE_DECLARATION))
				{
					if (ASTType(first_option) != AST_SIMPLE_DECLARATION)
					{
						correct_choice = i;
					}
				}
				else
				{
					internal_error("More than one valid choice! '%s' vs '%s'", 
							ast_print_node_type(ASTType(first_option)),
								ast_print_node_type(ASTType(second_option)));
				}
			}
		}
	}

	if (correct_choice < 0)
	{
		internal_error("Ambiguity not solved\n", 0);
	}
	else
	{
		choose_option(a, correct_choice);
	}
}

static char check_for_simple_declaration(AST a, scope_t* st)
{
	// And it will be invalid if it does not have type and it is not a
	// conversion function id nor a class constructor/destructor
	AST decl_specifier_seq = ASTSon0(a);

	if (decl_specifier_seq != NULL)
	{
		// We should check that this type specifier is really a type specifier
		//
		//    A(t);
		//
		// is a declaration if "A" names a type. Otherwise this is not a valid
		// simple declaration

		AST type_spec = ASTSon1(decl_specifier_seq);

		return check_for_type_specifier(type_spec, st);
	}

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

	return 0;
}

static char check_for_declaration_statement(AST declaration_statement, scope_t* st)
{
	AST a = ASTSon0(declaration_statement);

	// In general only AST_SIMPLE_DECLARATION gets ambiguous here
	if (ASTType(a) == AST_SIMPLE_DECLARATION)
	{
		return check_for_simple_declaration(a, st);
	}
	else
	{
		internal_error("Unknown node type '%s'\n", ast_print_node_type(ASTType(a)));
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

				scope_t* nested_scope = query_nested_name_spec(st, global_scope, nested_name_spec, NULL);

				if (nested_scope == NULL)
				{
					// Unknown scope
					return 0;
				}

				if (nested_scope->kind != CLASS_SCOPE)
				{
					return 0;
				}

				char* class_name = ASTText(symbol);

				if (ASTType(symbol) == AST_DESTRUCTOR_ID)
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
				scope_entry_list_t* result = query_in_symbols_of_scope(nested_scope, class_name);

				if (result == NULL
						|| result->entry->kind != SK_CLASS)
				{
					// This is not a class name
					return 0;
				}

				// It looks sane here
				return 1;
				break;
			}
		case AST_DESTRUCTOR_ID :
		case AST_SYMBOL :
			{
				char* class_name = ASTText(id_expression);

				// We want a class scope
				if (st->kind != CLASS_SCOPE)
				{
					return 0;
				}

				if (ASTType(id_expression) == AST_DESTRUCTOR_ID)
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
							|| result->entry->kind != SK_CLASS)
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
static char check_for_expression(AST expression, scope_t* st)
{
	switch (ASTType(expression))
	{
		case AST_AMBIGUITY :
			{
				char correct_choice = -1;
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
							internal_error("More than one valid choice for expression statement\n", 0);
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
				// TODO - This can yield a value ?
				return 0;
			}
		case AST_QUALIFIED_OPERATOR_FUNCTION_ID :
			{
				// This always yields a value, does not it?
				return 1;
			}
		case AST_QUALIFIED_TEMPLATE_ID :
			{
				// This is never a value
				return 0;
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
			{
				return check_for_destructor_id(expression, st);
			}
		case AST_OPERATOR_FUNCTION_ID :
			{
				// This should yield a value, should not ?
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
				return 0;
			}
			// Postfix expressions
		case AST_ARRAY_SUBSCRIPT :
			{
				// It depends only on the postfix expression
				return check_for_expression(ASTSon0(expression), st);
			}
		case AST_FUNCTION_CALL :
			{
				return check_for_function_call(expression, st);
			}
		case AST_EXPLICIT_TYPE_CONVERSION :
			{
				return check_for_explicit_type_conversion(expression, st);
			}
		case AST_TYPENAME_FUNCTION_CALL :
			{
				// This yields a type
				return 1;
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
				// This should always yield a value
				return 1;
			}
		case AST_CLASS_TEMPLATE_MEMBER_ACCESS :
		case AST_POINTER_CLASS_TEMPLATE_MEMBER_ACCESS :
			{
				// Think on this. Can this yield something that is not a value ?
				return 1;
			}
		case AST_POSTINCREMENT :
		case AST_POSTDECREMENT :
			{
				// This cannot yield a type
				return 1;
			}
		case AST_DYNAMIC_CAST :
		case AST_STATIC_CAST :
		case AST_REINTERPRET_CAST :
		case AST_CONST_CAST :
			{
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
		default :
			{
				internal_error("Unexpected node '%s'\n", ast_print_node_type(ASTType(expression)));
				break;
			}
	}
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

	scope_entry_list_t* result_list = query_nested_name(st, global_scope, nested_name_spec, unqualified_object, FULL_UNQUALIFIED_LOOKUP);

	return (result_list != NULL
			&& (result_list->entry->kind == SK_VARIABLE
				|| result_list->entry->kind == SK_ENUMERATOR
				|| result_list->entry->kind == SK_FUNCTION));
}

static char check_for_symbol(AST expr, scope_t* st)
{
	ENSURE_TYPE(expr, AST_SYMBOL);
	scope_entry_list_t* result = query_unqualified_name(st, ASTText(expr)); 

	return (result != NULL 
			&& (result->entry->kind == SK_VARIABLE
				|| result->entry->kind == SK_ENUMERATOR
				|| result->entry->kind == SK_FUNCTION));
}

static char check_for_destructor_id(AST expr, scope_t* st)
{
	ENSURE_TYPE(expr, AST_DESTRUCTOR_ID);

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
	while (type_result->type->kind == STK_TYPEDEF)
	{
		type_result = type_result->type->aliased_type;
	}

	if (type_result->type->kind == STK_USER_DEFINED)
	{
		type_result = type_result->type->user_defined_type->type_information;
	}

	return (type_result->kind == TK_DIRECT
			&& (type_result->type->kind == STK_CLASS
				// Dubious without exact instantiation
				|| type_result->type->kind == STK_TYPE_TEMPLATE_PARAMETER));
}

static char check_for_functional_expression(AST expr, AST arguments, scope_t* st)
{
	switch(ASTType(expr))
	{
		case AST_SYMBOL :
			{
				// Koenig lookup here!
				scope_entry_list_t* function_lookup = NULL;
				function_lookup = lookup_unqualified_function(st, ASTText(expr), arguments);

				enum cxx_symbol_kind filter_funct[] =
				{ 
					SK_FUNCTION,
					SK_TEMPLATE_FUNCTION
				};
				function_lookup = filter_symbol_kind_set(function_lookup, 2, filter_funct);

				if (function_lookup != NULL)
				{
					return 1;
				}
				break;
			}
		case AST_PARENTHESIZED_EXPRESSION :
			{
				return check_for_function_call(ASTSon0(expr), st);
				break;
			}
		default :
			{
				return check_for_expression(expr, st);
			}
	}
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

static char check_for_explicit_type_conversion(AST expr, scope_t* st)
{
	// An explicit type conversion is of the form
	//
	//   T ( e );
	//
	// T has to be a valid typename
	AST simple_type_spec = ASTSon0(expr);

	return check_for_simple_type_spec(simple_type_spec, st);
}

static char check_for_simple_type_spec(AST type_spec, scope_t* st)
{
	AST global_op = ASTSon0(type_spec);
	AST nested_name_spec = ASTSon1(type_spec);
	AST type_name = ASTSon2(type_spec) != NULL ? ASTSon2(type_spec) : ASTSon3(type_spec);

	scope_entry_list_t* entry_list = query_nested_name(st, global_op, nested_name_spec, 
            type_name, FULL_UNQUALIFIED_LOOKUP);

	if (entry_list == NULL)
	{
		return 0;
	}

	return (entry_list->entry->kind == SK_TYPEDEF
			|| entry_list->entry->kind == SK_ENUM
			|| entry_list->entry->kind == SK_CLASS
			|| entry_list->entry->kind == SK_TEMPLATE_PRIMARY_CLASS
			|| entry_list->entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS);
}

static char check_for_typeid(AST expr, scope_t* st)
{
	return check_for_type_id_tree(ASTSon0(expr), st);
}

static char check_for_type_id_tree(AST type_id, scope_t* st)
{
	AST type_specifier_seq = ASTSon0(type_id);
	// AST abstract_declarator = ASTSon1(type_id);
	
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
		case AST_ELABORATED_TYPE_ENUM :
		case AST_ELABORATED_TYPE_CLASS :
		case AST_ELABORATED_TYPE_TEMPLATE :
		case AST_ELABORATED_TYPE_TEMPLATE_TEMPLATE :
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
			// These are never ambiguous but are always types!
			return 1;
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
	return check_for_type_id_tree(type_id, st);
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
				internal_error("More than one valid choice!\n", 0);
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

	// Is this needed ?
	if (!check_for_initialization(initializer, st))
		return 0;

	return 1;
}

static char check_for_initialization(AST initializer, scope_t* st)
{
	switch (ASTType(initializer))
	{
		case AST_INITIALIZER :
			{
				// Why are we checking this ? This branch should not be ambiguous
				AST initializer_clause = ASTSon0(initializer);
				switch (ASTType(initializer_clause))
				{
					case AST_INITIALIZER_BRACES :
						{
							// This is never ambiguous
							return 1;
						}
					case AST_INITIALIZER_EXPR :
						{
							AST expression = ASTSon0(initializer_clause);
							return check_for_expression(expression, st);
							break;
						}
					default :
						{
							internal_error("Unexpected node type '%s'\n", ast_print_node_type(ASTType(initializer)));
						}
				}
				break;
			}
		case AST_PARENTHESIZED_INITIALIZER :
			{
				AST list = ASTSon0(initializer);
				AST iter;

				for_each_element(list, iter)
				{
					AST expression = ASTSon1(iter);

					if (!check_for_expression(expression, st))
					{
						return 0;
					}
				}
				return 1;
				break;
			}
		default :
			{
				internal_error("Unexpected node type '%s'\n", ast_print_node_type(ASTType(initializer)));
			}
	}
	return 1;
}

static char check_for_declarator(AST declarator, scope_t* st)
{
	return check_for_declarator_rec(declarator, st);
}

static char check_for_declarator_rec(AST declarator, scope_t* st)
{
	switch (ASTType(declarator))
	{
		case AST_PARENTHESIZED_EXPRESSION :
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
		case AST_DECLARATOR_ARRAY : 
			{
				return check_for_declarator_rec(ASTSon0(declarator), st);
				break;
			}
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
				return check_for_declarator_rec(ASTSon0(declarator), st);
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

	for_each_element(list, iter)
	{
		AST parameter = ASTSon1(iter);

		if (ASTType(parameter) == AST_VARIADIC_ARG
				|| ASTType(parameter) == AST_EMPTY_PARAMETER_DECLARATION_CLAUSE)
		{
			continue;
		}

		if (ASTType(parameter) != AST_PARAMETER_DECL)
		{
			internal_error("Unexpected node '%s'\n", ast_print_node_type(ASTType(parameter)));
		}

		AST decl_specifier_seq = ASTSon0(parameter);

		if (ASTType(decl_specifier_seq) == AST_AMBIGUITY)
		{
			// Ensure that this is the unique ambiguity than can appear here
			solve_ambiguous_type_spec_seq(decl_specifier_seq, st);
		}

		AST type_specifier = ASTSon1(decl_specifier_seq);

		if (!check_for_type_specifier(type_specifier, st))
		{
			return 0;
		}
	}

	return 1;
}

void solve_ambiguous_type_spec_seq(AST type_spec_seq, scope_t* st)
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
				if (check_for_expression(for_init_statement, st))
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
