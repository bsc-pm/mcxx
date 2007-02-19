#include <stdio.h>
#include <string.h>
#include "extstruct.h"
#include "cxx-attrnames.h"
#include "cxx-ambiguity.h"
#include "cxx-typeutils.h"
#include "cxx-utils.h"
#include "cxx-prettyprint.h"
#include "cxx-buildscope.h"
#include "cxx-graphviz.h"
#include "cxx-tltype.h"

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

static void solve_ambiguous_simple_declaration(AST a, scope_t* st, decl_context_t decl_context);

static char check_for_declaration_statement(AST a, scope_t* st, decl_context_t decl_context);
static char check_for_expression_statement(AST a, scope_t* st, decl_context_t decl_context);
static char check_for_qualified_id(AST expr, scope_t* st, decl_context_t decl_context, scope_t** symbol_scope);
static char check_for_symbol(AST expr, scope_t* st, decl_context_t decl_context, scope_t** symbol_scope);
#if 0
static char check_for_destructor_id(AST expr, scope_t* st, decl_context_t decl_context);
#endif
static char check_for_function_call(AST expr, scope_t* st, decl_context_t decl_context);
static char check_for_explicit_type_conversion(AST expr, scope_t* st, decl_context_t decl_context);
static char check_for_explicit_typename_type_conversion(AST expr, scope_t* st, decl_context_t decl_context);
static char check_for_typeid(AST expr, scope_t* st, decl_context_t decl_context);
static char check_for_typeid_expr(AST expr, scope_t* st, decl_context_t decl_context);
static char check_for_sizeof_expr(AST expr, scope_t* st, decl_context_t decl_context);
static char check_for_sizeof_typeid(AST expr, scope_t* st, decl_context_t decl_context);
static char check_for_cast(AST expr, scope_t* st, decl_context_t decl_context);

static char check_for_simple_type_spec(AST type_spec, scope_t* st, decl_context_t decl_context);
static char check_for_type_id_tree(AST type_id, scope_t* st, decl_context_t decl_context);
static char check_for_type_specifier(AST type_id, scope_t* st, decl_context_t decl_context);

static char check_for_typeless_declarator(AST declarator, scope_t* st, decl_context_t decl_context);

static char check_for_init_declarator(AST init_declarator, scope_t* st, decl_context_t decl_context);
static char check_for_declarator(AST declarator, scope_t* st, decl_context_t decl_context);
static char check_for_declarator_rec(AST declarator, scope_t* st, decl_context_t decl_context);
static char check_for_function_declarator_parameters(AST parameter_declaration_clause, scope_t* st, decl_context_t decl_context);

static char check_for_simple_declaration(AST a, scope_t* st, decl_context_t decl_context);
static char check_for_initializer_clause(AST initializer_clause, scope_t* st, decl_context_t decl_context);
static char check_for_parenthesized_initializer(AST parenthesized_initializer, scope_t* st, decl_context_t decl_context);
static char check_for_initializer_list(AST initializer_list, scope_t* st, decl_context_t decl_context);

static char check_for_new_expression(AST new_expr, scope_t* st, decl_context_t decl_context);
static char check_for_new_type_id_expr(AST new_expr, scope_t* st, decl_context_t decl_context);


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
void solve_ambiguous_declaration(AST a, scope_t* st, decl_context_t decl_context)
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
        solve_ambiguous_simple_declaration(a, st, decl_context);
        return;
    }

    internal_error("Don't know how to handle this ambiguity. %s", node_information(a));
}

static void solve_ambiguous_simple_declaration(AST a, scope_t* st, decl_context_t decl_context)
{
    int correct_option = -1;
    int i;
    for (i = 0; i < a->num_ambig; i++)
    {
        AST option = a->ambig[i];

        if (check_for_simple_declaration(option, st, decl_context))
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

// Checks for old-styled functions
static char check_for_kr_parameter_list(AST parameters_kr, scope_t* st, decl_context_t decl_context)
{
    CXX_LANGUAGE()
    {
        internal_error("This function is only for C", 0);
    }

    AST identifier_list = ASTSon0(parameters_kr);
    AST iter;

    for_each_element(identifier_list, iter)
    {
        AST identifier = ASTSon1(iter);

        scope_entry_list_t* entry_list = query_unqualified_name(st, ASTText(identifier));

        if (entry_list != NULL)
        {
            scope_entry_t* entry = entry_list->entry;
            if (entry->kind == SK_TYPEDEF)
            {
                return 0;
            }
        }
    }

    return 1;
}

/*
 * Ambiguity within a declarator.
 */
void solve_ambiguous_declarator(AST a, scope_t* st, decl_context_t decl_context)
{
    int num_ambig = a->num_ambig;

    switch (num_ambig)
    {
        case 2 :
            {
                CXX_LANGUAGE()
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
                }

                C_LANGUAGE()
                {
                    // Case for
                    //
                    //   void f(a, b, c);
                    //
                    // we are unsure if this is a K&R-style function
                    // declaration or a proper prototype with all being
                    // abstract declarators
                    
                    AST first_option = a->ambig[0];
                    AST second_option = a->ambig[1];

                    if (ASTType(first_option) == AST_DECLARATOR_FUNC
                            && ASTType(second_option) == AST_DECLARATOR_FUNC)
                    {
                        AST parameters = ASTSon1(first_option);

                        if (ASTType(parameters) == AST_KR_PARAMETER_LIST)
                        {
                            if (check_for_kr_parameter_list(parameters, st, decl_context))
                            {
                                choose_option(a, 0);
                                return;
                            }
                            else
                            {
                                choose_option(a, 1);
                                return;
                            }
                        }

                        parameters = ASTSon1(second_option);
                        if (ASTType(parameters) == AST_KR_PARAMETER_LIST)
                        {
                            if (check_for_kr_parameter_list(parameters, st, decl_context))
                            {
                                choose_option(a, 1);
                                return;
                            }
                            else
                            {
                                choose_option(a, 0);
                                return;
                            }
                        }
                    }
                }
                break;
            }
    }

    internal_error("Don't know how to handle this ambiguity", 0);
}

void solve_ambiguous_statement(AST a, scope_t* st, decl_context_t decl_context)
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
                    current_check = check_for_declaration_statement(a->ambig[i], st, decl_context);
                    break;
                }
            case AST_EXPRESSION_STATEMENT :
                {
                    current_check = check_for_expression_statement(a->ambig[i], st, decl_context);
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
                            AST_SIMPLE_DECLARATION, AST_EXPRESSION)))
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

    // In case of ignorance, favor expressions
    if (correct_choice < 0)
    {
        for (i = 0; i < a->num_ambig; i++)
        {
            if (ASTType(a->ambig[i]) == AST_EXPRESSION_STATEMENT)
            {
                correct_choice = i;
                break;
            }
        }
    }

    if (correct_choice < 0)
    {
        fprintf(stderr, "This statement cannot be disambiguated: %s\n", node_information(a));
        prettyprint(stderr, a);
        internal_error("Ambiguity not solved\n", 0);
    }
    else
    {
        choose_option(a, correct_choice);
    }
}


static char check_for_init_declarator_list(AST init_declarator_list, scope_t* st, decl_context_t decl_context)
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

                if (check_for_init_declarator(one_init_decl, st, decl_context))
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
            if (!check_for_init_declarator(init_declarator, st, decl_context))
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
        // Fix this
        AST declarator_name = get_leftmost_declarator_name(declarator, default_decl_context);

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

static char check_for_simple_declaration(AST a, scope_t* st, decl_context_t decl_context)
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
            if (!check_for_type_specifier(type_spec, st, decl_context))
            {
                return 0;
            }
        }

        AST init_declarator_list = ASTSon1(a);

        if (init_declarator_list != NULL)
        {
            if (!check_for_init_declarator_list(init_declarator_list, st, decl_context))
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
                scope_entry_list_t* entry_list = query_id_expression(st, id_expression, 
                        FULL_UNQUALIFIED_LOOKUP, decl_context);

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

                        entry_list = query_nested_name(st, global_op, nested_name_spec, 
                                type_name, FULL_UNQUALIFIED_LOOKUP, decl_context);

                        // A is of class nature
                        if (entry_list != NULL
                                && (entry_list->entry->kind == SK_CLASS
                                    || entry_list->entry->kind == SK_TEMPLATE_PRIMARY_CLASS
                                    || entry_list->entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS))
                        {
                            scope_entry_t* entry = entry_list->entry;
                            
                            // The related scope of A is the same as the
                            // current scope
                            if (same_scope(entry->related_scope, st))
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

                    if (check_for_typeless_declarator(opt_declarator, st, decl_context))
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
                if (!check_for_typeless_declarator(declarator, st, decl_context))
                {
                    return 0;
                }
            }
        }
    }

    return 1;
}

static char check_for_declaration_statement(AST declaration_statement, scope_t* st, decl_context_t decl_context)
{
    AST a = ASTSon0(declaration_statement);

    // In general only AST_SIMPLE_DECLARATION gets ambiguous here
    if (ASTType(a) == AST_SIMPLE_DECLARATION)
    {
        return check_for_simple_declaration(a, st, decl_context);
    }
    else if (ASTType(a) == AST_AMBIGUITY)
    {
        // internal_error("Unknown node type '%s' (line=%d)\n", ast_print_node_type(ASTType(a)),
        //      node_information(a));
        //
        // In general only AST_SIMPLE_DECLARATION gets ambiguous here

        int correct_choice = -1;
        int i;
        for (i = 0; i < a->num_ambig; i++)
        {
            if (check_for_simple_declaration(a->ambig[i], st, decl_context))
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

static char check_for_typeless_declarator_rec(AST declarator, scope_t* st, decl_context_t decl_context, int nfuncs)
{
    switch (ASTType(declarator))
    {
        case AST_PARENTHESIZED_EXPRESSION :
        case AST_DECLARATOR :
            {
                return check_for_typeless_declarator_rec(ASTSon0(declarator), 
                        st, decl_context, nfuncs);
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
                return check_for_typeless_declarator_rec(ASTSon0(declarator), st, decl_context, nfuncs+1);
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
                scope_t* nested_scope = query_nested_name_spec(st, global_scope, nested_name_spec, 
                        NULL, &is_dependent, decl_context);

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

static char check_for_typeless_declarator(AST declarator, scope_t* st, decl_context_t decl_context)
{
    return check_for_typeless_declarator_rec(declarator, st, decl_context, 0);
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
char check_for_expression(AST expression, scope_t* st, decl_context_t decl_context)
{
    switch (ASTType(expression))
    {
        case AST_EXPRESSION : 
        case AST_CONSTANT_EXPRESSION : 
        case AST_PARENTHESIZED_EXPRESSION :
            {
                char result;
                result = check_for_expression(ASTSon0(expression), st, decl_context);
                if (result)
                {
                    ASTAttrSetValueType(expression, LANG_IS_EXPRESSION_NEST, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_EXPRESSION_NESTED, tl_type_t, tl_ast(ASTSon0(expression)));
                }
                return result;
            }
        case AST_AMBIGUITY :
            {
                int correct_choice = -1;
                int i;
                for (i = 0; i < expression->num_ambig; i++)
                {
                    if (check_for_expression(expression->ambig[i], st, decl_context))
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
                    // Now recalculate everything to update data properly
                    check_for_expression(expression, st, decl_context);
                    return 1;
                }
                break;
            }
            // Primaries
        case AST_DECIMAL_LITERAL :
        case AST_OCTAL_LITERAL :
        case AST_HEXADECIMAL_LITERAL :
            {
                ASTAttrSetValueType(expression, LANG_IS_INTEGER_LITERAL, tl_type_t, tl_bool(1));
                return 1;
                break;
            }
        case AST_FLOATING_LITERAL :
            {
                ASTAttrSetValueType(expression, LANG_IS_FLOATING_LITERAL, tl_type_t, tl_bool(1));
                return 1;
                break;
            }
        case AST_BOOLEAN_LITERAL :
            {
                ASTAttrSetValueType(expression, LANG_IS_BOOLEAN_LITERAL, tl_type_t, tl_bool(1));
                return 1;
                break;
            }
        case AST_CHARACTER_LITERAL :
            {
                ASTAttrSetValueType(expression, LANG_IS_CHARACTER_LITERAL, tl_type_t, tl_bool(1));
                return 1;
                break;
            }
        case AST_STRING_LITERAL :
            {
                ASTAttrSetValueType(expression, LANG_IS_STRING_LITERAL, tl_type_t, tl_bool(1));
                return 1;
                break;
            }
        case AST_THIS_VARIABLE :
            {
                return 1;
            }
        case AST_QUALIFIED_ID :
            {
                scope_t* symbol_scope = NULL;
                char c = check_for_qualified_id(expression, st, decl_context, &symbol_scope);

                if (c)
                {
                    if (symbol_scope != NULL && compilation_options.scope_link != NULL)
                    {
                        scope_link_set(compilation_options.scope_link, expression, copy_scope(symbol_scope));
                    }

                    AST global_qualif = ASTSon0(expression);
                    AST nested_name_spec = ASTSon1(expression);
                    AST unqualified_id = ASTSon2(expression);

                    ASTAttrSetValueType(expression, LANG_IS_ID_EXPRESSION, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_IS_QUALIFIED_ID, tl_type_t, tl_bool(1));

                    if (global_qualif != NULL)
                    {
                        ASTAttrSetValueType(expression, LANG_IS_GLOBAL_QUALIFIED, tl_type_t, tl_bool(1));
                    }

                    if (nested_name_spec != NULL)
                    {
                        ASTAttrSetValueType(expression, LANG_NESTED_NAME_SPECIFIER, tl_type_t, tl_ast(nested_name_spec));
                    }

                    ASTAttrSetValueType(expression, LANG_UNQUALIFIED_ID, tl_type_t, tl_ast(unqualified_id));
                }

                return c;
            }
        case AST_QUALIFIED_TEMPLATE :
            {
                // This always yields a value because it must be used in a
                // template context where everything is regarded as an
                // expression unless the user "typenames" things

                AST global_scope = ASTSon0(expression);
                AST nested_name = ASTSon1(expression);
                AST template_id = ASTSon2(expression);

                query_nested_name_flags(st, global_scope, nested_name, template_id, 
                        FULL_UNQUALIFIED_LOOKUP, LF_NO_FAIL, decl_context);

                return 1;
            }
        case AST_QUALIFIED_OPERATOR_FUNCTION_ID :
            {
                // This always yields a value, does not it?
                return 1;
            }
        case AST_SYMBOL :
            {
                scope_t* symbol_scope = NULL;
                char c = check_for_symbol(expression, st, decl_context, &symbol_scope);

                if (c)
                {
                    // Should be always non null
                    if (symbol_scope != NULL && compilation_options.scope_link != NULL)
                    {
                        scope_link_set(compilation_options.scope_link, expression, copy_scope(symbol_scope));
                    }
                }

                ASTAttrSetValueType(expression, LANG_IS_ID_EXPRESSION, tl_type_t, tl_bool(1));
                ASTAttrSetValueType(expression, LANG_IS_UNQUALIFIED_ID, tl_type_t, tl_bool(1));
                ASTAttrSetValueType(expression, LANG_UNQUALIFIED_ID, tl_type_t, tl_ast(expression));

                return c;
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
                solve_possibly_ambiguous_template_id(expression, st, decl_context);
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
                solve_possibly_ambiguous_template_id(expression, st, decl_context);

                ASTAttrSetValueType(expression, LANG_IS_TEMPLATE_ID, tl_type_t, tl_bool(1));
                ASTAttrSetValueType(expression, LANG_TEMPLATE_NAME, tl_type_t, tl_ast(ASTSon0(expression)));
                ASTAttrSetValueType(expression, LANG_TEMPLATE_ARGS, tl_type_t, tl_ast(ASTSon1(expression)));
                return 0;
            }
            // Postfix expressions
        case AST_ARRAY_SUBSCRIPT :
            {
                char result = check_for_expression(ASTSon0(expression), st, decl_context )
                    && check_for_expression(ASTSon1(expression), st, decl_context );

                if (result)
                {
                    ASTAttrSetValueType(expression, LANG_IS_ARRAY_SUBSCRIPT, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_SUBSCRIPTED_EXPRESSION, tl_type_t, tl_ast(ASTSon0(expression)));
                    ASTAttrSetValueType(expression, LANG_SUBSCRIPT_EXPRESSION, tl_type_t, tl_ast(ASTSon1(expression)));
                }
                return result;
            }
        case AST_FUNCTION_CALL :
            {
                char result = check_for_function_call(expression, st, decl_context );

                if (result)
                {
                    ASTAttrSetValueType(expression, LANG_IS_FUNCTION_CALL, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_CALLED_EXPRESSION, tl_type_t, tl_ast(ASTSon0(expression)));
                    ASTAttrSetValueType(expression, LANG_FUNCTION_ARGUMENTS, tl_type_t, tl_ast(ASTSon1(expression)));
                }

                return result;
            }
        case AST_EXPLICIT_TYPE_CONVERSION :
            {
                return check_for_explicit_type_conversion(expression, st, decl_context );
            }
        case AST_TYPENAME_EXPLICIT_TYPE_CONVERSION :
            {
                return check_for_explicit_typename_type_conversion(expression, st, decl_context );
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
                result = check_for_expression(ASTSon0(expression), st, decl_context );

                // This should always yield a value unless the right hand is rubbish
                if (result 
                        && (ASTType(ASTSon1(expression)) == AST_DESTRUCTOR_ID
                            || ASTType(ASTSon1(expression)) == AST_DESTRUCTOR_TEMPLATE_ID))
                {
                    result = 0;
                }

                check_for_expression(ASTSon1(expression), st, decl_context );

                ASTAttrSetValueType(ASTSon1(expression), LANG_IS_ACCESSED_MEMBER, tl_type_t, tl_bool(1));

                ASTAttrSetValueType(expression, LANG_ACCESSED_ENTITY, tl_type_t, tl_ast(ASTSon0(expression)));
                ASTAttrSetValueType(expression, LANG_ACCESSED_MEMBER, tl_type_t, tl_ast(ASTSon1(expression)));

                if (ASTType(expression) == AST_POINTER_CLASS_MEMBER_ACCESS)
                {
                    ASTAttrSetValueType(expression, LANG_IS_POINTER_MEMBER_ACCESS, tl_type_t, tl_bool(1));
                }
                else
                {
                    ASTAttrSetValueType(expression, LANG_IS_MEMBER_ACCESS, tl_type_t, tl_bool(1));
                }

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

                check_for_expression(ASTSon0(expression), st, decl_context );
                check_for_expression(ASTSon1(expression), st, decl_context );

                return 1;
            }
        case AST_POSTINCREMENT :
            {
                check_for_expression(ASTSon0(expression), st, decl_context );
                ASTAttrSetValueType(expression, LANG_IS_UNARY_OPERATION, tl_type_t, tl_bool(1));
                ASTAttrSetValueType(expression, LANG_IS_POSTINCREMENT, tl_type_t, tl_bool(1));
                ASTAttrSetValueType(expression, LANG_EXPRESSION_INCREMENTED, tl_type_t, tl_ast(ASTSon0(expression)));
                return 1;
            }
        case AST_POSTDECREMENT :
            {
                check_for_expression(ASTSon0(expression), st, decl_context );
                ASTAttrSetValueType(expression, LANG_IS_UNARY_OPERATION, tl_type_t, tl_bool(1));
                ASTAttrSetValueType(expression, LANG_IS_POSTDECREMENT, tl_type_t, tl_bool(1));
                ASTAttrSetValueType(expression, LANG_EXPRESSION_DECREMENTED, tl_type_t, tl_ast(ASTSon0(expression)));
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

                decl_context.decl_flags |= DF_NO_FAIL;

                type_t* simple_type_info = NULL;
                build_scope_decl_specifier_seq(type_specifier, st, &gather_info, &simple_type_info, 
                        decl_context);

                if (abstract_declarator != NULL)
                {
                    decl_context_t nofail_context = decl_context;
                    nofail_context.decl_flags |= DF_NO_FAIL;
                    type_t* declarator_type = NULL;
                    build_scope_declarator(abstract_declarator, st, &gather_info, simple_type_info, 
                            &declarator_type, nofail_context);
                }

                AST casted_expression = ASTSon1(expression);

                solve_possibly_ambiguous_expression(casted_expression, st, decl_context );
                // This should not yield a type
                return 1;
            }
        case AST_TYPEID_TYPE :
            {
                return check_for_typeid(expression, st, decl_context);
            }
        case AST_TYPEID_EXPR :
            {
                return check_for_typeid_expr(expression, st, decl_context);
            }
        // Unary expressions
        case AST_PREINCREMENT :
            {
                check_for_expression(ASTSon0(expression), st, decl_context);
                ASTAttrSetValueType(expression, LANG_IS_UNARY_OPERATION, tl_type_t, tl_bool(1));
                ASTAttrSetValueType(expression, LANG_IS_PREINCREMENT, tl_type_t, tl_bool(1));
                ASTAttrSetValueType(expression, LANG_EXPRESSION_INCREMENTED, tl_type_t, tl_ast(ASTSon0(expression)));
                return 1;
            }
        case AST_PREDECREMENT :
            {
                check_for_expression(ASTSon0(expression), st, decl_context);
                ASTAttrSetValueType(expression, LANG_IS_UNARY_OPERATION, tl_type_t, tl_bool(1));
                ASTAttrSetValueType(expression, LANG_IS_PREDECREMENT, tl_type_t, tl_bool(1));
                ASTAttrSetValueType(expression, LANG_EXPRESSION_DECREMENTED, tl_type_t, tl_ast(ASTSon0(expression)));
                return 1;
            }
        case AST_SIZEOF :
            {
                return check_for_sizeof_expr(expression, st, decl_context);
            }
        case AST_SIZEOF_TYPEID :
            {
                return check_for_sizeof_typeid(expression, st, decl_context);
            }
        case AST_DERREFERENCE :
        case AST_REFERENCE :
        case AST_PLUS_OP :
        case AST_NEG_OP :
        case AST_NOT_OP :
        case AST_COMPLEMENT_OP :
            {
                char result = check_for_expression(ASTSon0(expression), st, decl_context);

                char* unary_expression_attr[] =
                {
                    [AST_DERREFERENCE]  = LANG_IS_DERREFERENCE_OP,
                    [AST_REFERENCE]     = LANG_IS_REFERENCE_OP,
                    [AST_PLUS_OP]       = LANG_IS_PLUS_OP,
                    [AST_NEG_OP]        = LANG_IS_NEGATE_OP,
                    [AST_NOT_OP]        = LANG_IS_NOT_OP,
                    [AST_COMPLEMENT_OP] = LANG_IS_COMPLEMENT_OP
                };

                if (result)
                {
                    ASTAttrSetValueType(expression, 
                            unary_expression_attr[ASTType(expression)], tl_type_t, tl_bool(1));

                    ASTAttrSetValueType(expression, LANG_UNARY_OPERAND, tl_type_t, tl_ast(ASTSon0(expression)));

                    ASTAttrSetValueType(expression, LANG_IS_UNARY_OPERATION, tl_type_t, tl_bool(1));
                }

                return result;
            }
            // Cast expression
        case AST_CAST_EXPRESSION :
            {
                char result = check_for_cast(expression, st, decl_context );

                if (result)
                {
                    ASTAttrSetValueType(expression, LANG_IS_CAST, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_CAST_TYPE, tl_type_t, tl_ast(ASTSon0(expression)));
                    ASTAttrSetValueType(expression, LANG_CASTED_EXPRESSION, tl_type_t, tl_ast(ASTSon1(expression)));
                }

                return result;
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
                char* binary_expression_attr[] =
                {
                    [AST_MULT_OP] = LANG_IS_MULT_OP,
                    [AST_DIV_OP] = LANG_IS_DIVISION_OP,
                    [AST_MOD_OP] = LANG_IS_MODULUS_OP,
                    [AST_ADD_OP] = LANG_IS_ADDITION_OP,
                    [AST_MINUS_OP] = LANG_IS_SUBSTRACTION_OP,
                    [AST_SHL_OP] = LANG_IS_SHIFT_LEFT_OP,
                    [AST_SHR_OP] = LANG_IS_SHIFT_RIGHT_OP,
                    [AST_LOWER_THAN] = LANG_IS_LOWER_THAN_OP,
                    [AST_GREATER_THAN] = LANG_IS_GREATER_THAN_OP,
                    [AST_GREATER_OR_EQUAL_THAN] = LANG_IS_GREATER_OR_EQUAL_THAN_OP,
                    [AST_LOWER_OR_EQUAL_THAN] = LANG_IS_LOWER_OR_EQUAL_THAN_OP,
                    [AST_EQUAL_OP] = LANG_IS_EQUAL_OP,
                    [AST_DIFFERENT_OP] = LANG_IS_DIFFERENT_OP,
                    [AST_BITWISE_AND] = LANG_IS_BITWISE_AND_OP,
                    [AST_BITWISE_XOR] = LANG_IS_BITWISE_XOR_OP,
                    [AST_BITWISE_OR] = LANG_IS_BITWISE_OR_OP,
                    [AST_LOGICAL_AND] = LANG_IS_LOGICAL_AND_OP,
                    [AST_LOGICAL_OR] = LANG_IS_LOGICAL_OR_OP
                };

                char result = check_for_expression(ASTSon0(expression), st, decl_context )
                    && check_for_expression(ASTSon1(expression), st, decl_context );

                if (result)
                {
                    ASTAttrSetValueType(expression, 
                            binary_expression_attr[ASTType(expression)], tl_type_t, tl_bool(1));

                    ASTAttrSetValueType(expression, LANG_LHS_OPERAND, tl_type_t, tl_ast(ASTSon0(expression)));
                    ASTAttrSetValueType(expression, LANG_RHS_OPERAND, tl_type_t, tl_ast(ASTSon1(expression)));
                    ASTAttrSetValueType(expression, LANG_IS_BINARY_OPERATION, tl_type_t, tl_bool(1));
                }

                return result;
            }
        case AST_CONDITIONAL_EXPRESSION :
            {
                char result = check_for_expression(ASTSon0(expression), st, decl_context )
                    && check_for_expression(ASTSon1(expression), st, decl_context )
                    && check_for_expression(ASTSon2(expression), st, decl_context );

                if (result)
                {
                    ASTAttrSetValueType(expression, LANG_IS_CONDITIONAL_EXPRESSION,
                            tl_type_t, tl_bool(1));

                    ASTAttrSetValueType(expression, LANG_CONDITIONAL_EXPRESSION, tl_type_t, tl_ast(ASTSon0(expression)));
                    ASTAttrSetValueType(expression, LANG_CONDITIONAL_TRUE_EXPRESSION, tl_type_t, tl_ast(ASTSon1(expression)));
                    ASTAttrSetValueType(expression, LANG_CONDITIONAL_FALSE_EXPRESSION, tl_type_t, tl_ast(ASTSon2(expression)));
                }

                return result;
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
                char* assig_op_attr[] =
                {
                    [AST_ASSIGNMENT]     = LANG_IS_ASSIGNMENT,
                    [AST_MUL_ASSIGNMENT] = LANG_IS_MUL_ASSIGNMENT,
                    [AST_DIV_ASSIGNMENT] = LANG_IS_DIV_ASSIGNMENT,
                    [AST_ADD_ASSIGNMENT] = LANG_IS_ADD_ASSIGNMENT,
                    [AST_SUB_ASSIGNMENT] = LANG_IS_SUB_ASSIGNMENT,
                    [AST_SHL_ASSIGNMENT] = LANG_IS_SHL_ASSIGNMENT,
                    [AST_SHR_ASSIGNMENT] = LANG_IS_SHR_ASSIGNMENT,
                    [AST_AND_ASSIGNMENT] = LANG_IS_AND_ASSIGNMENT,
                    [AST_OR_ASSIGNMENT ] = LANG_IS_OR_ASSIGNMENT, 
                    [AST_XOR_ASSIGNMENT] = LANG_IS_XOR_ASSIGNMENT,
                    [AST_MOD_ASSIGNMENT] = LANG_IS_MOD_ASSIGNMENT,
                };
                char result = check_for_expression(ASTSon0(expression), st, decl_context )
                    && check_for_expression(ASTSon1(expression), st, decl_context );

                if (result)
                {
                    ASTAttrSetValueType(expression, assig_op_attr[ASTType(expression)], tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_LHS_ASSIGNMENT, tl_type_t, tl_ast(ASTSon0(expression)));
                    ASTAttrSetValueType(expression, LANG_RHS_ASSIGNMENT, tl_type_t, tl_ast(ASTSon1(expression)));
                }

                return result;
            }
        case AST_THROW_EXPRESSION :
            {
                if (ASTSon0(expression) != NULL)
                {
                    return check_for_expression(ASTSon0(expression), st, decl_context );
                }
                else 
                    return 1;
            }
        case AST_COMMA_OP :
            {
                char result = check_for_expression(ASTSon0(expression), st, decl_context )
                    && check_for_expression(ASTSon1(expression), st, decl_context );

                if (result)
                {
                    ASTAttrSetValueType(expression, LANG_IS_COMMA_OP, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_LHS_OPERAND, tl_type_t, tl_ast(ASTSon0(expression)));
                    ASTAttrSetValueType(expression, LANG_LHS_OPERAND, tl_type_t, tl_ast(ASTSon1(expression)));
                }

                return result;
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
                return check_for_expression(ASTSon0(expression), st, decl_context );
            }
        case AST_GCC_ALIGNOF :
            {
                // Reuse the sizeof code
                return check_for_sizeof_expr(expression, st, decl_context );
                break;
            }
        case AST_GCC_ALIGNOF_TYPE :
            {
                // Reuse the sizeof code
                return check_for_sizeof_typeid(expression, st, decl_context );
                break;
            }
        case AST_NEW_EXPRESSION :
            {
                // This is always a value, never a type
                return check_for_new_expression(expression, st, decl_context );
                break;
            }
        case AST_NEW_TYPE_ID_EXPR :
            {
                return check_for_new_type_id_expr(expression, st, decl_context );
            }
        case AST_DELETE_EXPR :
        case AST_DELETE_ARRAY_EXPR :
            {
                // This is always a value, never a type
                check_for_expression(ASTSon1(expression), st, decl_context );
                return 1;
                break;
            }
        case AST_PSEUDO_DESTRUCTOR_CALL :
        case AST_POINTER_PSEUDO_DESTRUCTOR_CALL :
            {
                return 1;
                break;
            }
        case AST_GCC_POSTFIX_EXPRESSION :
            {
                return (check_for_type_id_tree(ASTSon0(expression), st, decl_context ) &&
                        check_for_initializer_list(ASTSon1(expression), st, decl_context ));
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

static char check_for_new_expression(AST new_expr, scope_t* st, decl_context_t decl_context)
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

            solve_possibly_ambiguous_expression(expression, st, decl_context );
        }
    }

    AST type_specifier_seq = ASTSon0(new_type_id);
    AST new_declarator = ASTSon1(new_type_id);

    type_t* dummy_type;
    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));

    decl_context.decl_flags |= DF_NO_FAIL;

    build_scope_decl_specifier_seq(type_specifier_seq, st, &gather_info, &dummy_type, decl_context);

    if (new_declarator != NULL)
    {
        type_t* declarator_type = NULL;
        build_scope_declarator(new_declarator, st, &gather_info, dummy_type, &declarator_type, decl_context);
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

                solve_possibly_ambiguous_expression(expression, st, decl_context );
            }
        }
    }

    // This is a bit bogus
    return 1;
}

static char check_for_new_type_id_expr(AST new_expr, scope_t* st, decl_context_t decl_context)
{
    return check_for_new_expression(new_expr, st, decl_context );
}

void solve_possibly_ambiguous_expression(AST a, scope_t* st, decl_context_t decl_context)
{
    check_for_expression(a, st, decl_context );
}

static char check_for_expression_statement(AST a, scope_t* st, decl_context_t decl_context)
{
    AST expression = ASTSon0(a);
    return check_for_expression(expression, st, decl_context );
}

#define ENSURE_TYPE(expr, type) \
do { \
    if (ASTType(expr) != type) \
    { \
        internal_error("Expecting node of type '"#type"' but got '%s'\n", ast_print_node_type(ASTType(expr))); \
    } \
} \
while (0);

static char check_for_qualified_id(AST expr, scope_t* st, decl_context_t decl_context, scope_t** symbol_scope)
{
    *symbol_scope = NULL;
    AST global_scope = ASTSon0(expr);
    AST nested_name_spec = ASTSon1(expr);
    AST unqualified_object = ASTSon2(expr);

    scope_entry_list_t* result_list = query_nested_name_flags(st, global_scope, nested_name_spec, 
            unqualified_object, FULL_UNQUALIFIED_LOOKUP, LF_EXPRESSION | LF_NO_FAIL, decl_context );

    if (ASTType(unqualified_object) == AST_TEMPLATE_ID)
    {
        if (result_list != NULL
                && (result_list->entry->kind == SK_TEMPLATE_FUNCTION))
        {
            *symbol_scope = result_list->entry->scope;
            return 1;
        }
        else
        {
            return 0;
        }
    }
    else
    {
        if (result_list != NULL
                && (result_list->entry->kind == SK_VARIABLE
                    || result_list->entry->kind == SK_ENUMERATOR
                    || result_list->entry->kind == SK_FUNCTION
                    || result_list->entry->kind == SK_TEMPLATE_FUNCTION
                    || result_list->entry->kind == SK_DEPENDENT_ENTITY))
        {
            *symbol_scope = result_list->entry->scope;
            return 1;
        }
        else
        {
            return 0;
        }
    }
}

static char check_for_symbol(AST expr, scope_t* st, decl_context_t decl_context, scope_t** symbol_scope)
{
    ENSURE_TYPE(expr, AST_SYMBOL);
    scope_entry_list_t* result = query_unqualified_name(st, ASTText(expr)); 

    *symbol_scope = NULL;

    if (result != NULL 
            && (result->entry->kind == SK_VARIABLE
                || result->entry->kind == SK_ENUMERATOR
                || result->entry->kind == SK_FUNCTION
                || result->entry->kind == SK_TEMPLATE_PARAMETER))
    {
        *symbol_scope = result->entry->scope;
        return 1;
    }
    else
    {
        return 0;
    }
}

static char check_for_functional_expression(AST expr, AST arguments, scope_t* st, decl_context_t decl_context)
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
                    solve_possibly_ambiguous_template_id(expr, st, decl_context );
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

                check_for_expression(expr, st, decl_context);
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
                result = check_for_functional_expression(ASTSon0(expr), arguments, st, decl_context );
                break;
            }
        default :
            {
                result = check_for_expression(expr, st, decl_context );
            }
    }

    if (result)
    {
        if (arguments != NULL)
        {
            if (ASTType(arguments) == AST_AMBIGUITY)
            {
                solve_ambiguous_expression_list(arguments, st, decl_context);
            }

            AST list = arguments;
            AST iter;

            for_each_element(list, iter)
            {
                AST parameter_expr = ASTSon1(iter);
                solve_possibly_ambiguous_expression(parameter_expr, st, decl_context );
            }
        }
    }

    return result;
}

static char check_for_function_call(AST expr, scope_t* st, decl_context_t decl_context)
{
    ENSURE_TYPE(expr, AST_FUNCTION_CALL);
    
    // A function call is of the form
    //   f ( e );
    //
    // f has to yield a valid value or functional
    return check_for_functional_expression(ASTSon0(expr), ASTSon1(expr), st, decl_context );
}

static char check_for_explicit_typename_type_conversion(AST expr, scope_t* st, decl_context_t decl_context)
{
    AST global_op = ASTSon0(expr);
    AST nested_name_spec = ASTSon1(expr);
    AST symbol = ASTSon2(expr);

    scope_entry_list_t* entry_list = query_nested_name_flags(st, global_op, nested_name_spec, symbol, 
            FULL_UNQUALIFIED_LOOKUP, LF_NONE, decl_context );

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
            solve_ambiguous_expression_list(list, st, decl_context );
        }

        for_each_element(list, iter)
        {
            AST expression = ASTSon1(iter);

            solve_possibly_ambiguous_expression(expression, st, decl_context );
        }
    }

    return 1;
}

static char check_for_explicit_type_conversion(AST expr, scope_t* st, decl_context_t decl_context)
{
    // An explicit type conversion is of the form
    //
    //   T ( e );
    //
    // T has to be a valid typename
    char result = 0;
    AST simple_type_spec = ASTSon0(expr);

    result = check_for_simple_type_spec(simple_type_spec, st, decl_context);

    if (result)
    {
        AST expression_list = ASTSon1(expr);

        if (expression_list != NULL)
        {
            AST iter;
            for_each_element(expression_list, iter)
            {
                AST current_expression = ASTSon1(iter);

                solve_possibly_ambiguous_expression(current_expression, st, decl_context);
            }
        }
    }

    return result;
}

void solve_ambiguous_template_argument(AST ambig_template_argument, scope_t* st, decl_context_t decl_context)
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
                        solve_possibly_ambiguous_template_id(ASTSon2(type_id_simple_type_spec), st, decl_context);
                    }

                    current_option = check_for_type_id_tree(type_id, st, decl_context);
                    break;
                }
            case AST_TEMPLATE_EXPRESSION_ARGUMENT :
                {
                    AST expression_arg = ASTSon0(current_template_argument);
                    current_option = check_for_expression(expression_arg, st, decl_context);
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
        internal_error("No valid choice found in %s for '%s' ", node_information(ambig_template_argument),
                prettyprint_in_buffer(ambig_template_argument));
    }
    else
    {
        choose_option(ambig_template_argument, selected_option);
    }

}

void solve_possibly_ambiguous_template_id(AST type_name, scope_t* st, decl_context_t decl_context)
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
                solve_ambiguous_template_argument(template_argument, st, decl_context);
            }

            if (ASTType(template_argument) == AST_TEMPLATE_EXPRESSION_ARGUMENT)
            {
                solve_possibly_ambiguous_expression(ASTSon0(template_argument), st, decl_context);
            }
            else if (ASTType(template_argument) == AST_TEMPLATE_TYPE_ARGUMENT)
            {
                AST type_id = ASTSon0(template_argument);

                AST type_specifier = ASTSon0(type_id);
                AST abstract_declarator = ASTSon1(type_id);

                gather_decl_spec_t gather_info;
                memset(&gather_info, 0, sizeof(gather_info));

                decl_context.decl_flags |= DF_NO_FAIL;

                type_t* simple_type_info = NULL;
                build_scope_decl_specifier_seq(type_specifier, st, &gather_info, &simple_type_info, 
                        decl_context);

                if (abstract_declarator != NULL)
                {
                    type_t* declarator_type = NULL;
                    build_scope_declarator(abstract_declarator, st, &gather_info, simple_type_info, 
                            &declarator_type, decl_context);
                }
            }
        }
    }
}

static char check_for_simple_type_spec(AST type_spec, scope_t* st, decl_context_t decl_context)
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
        solve_possibly_ambiguous_template_id(type_name, st, decl_context);
    }

    scope_entry_list_t* entry_list = query_nested_name_flags(st, global_op, nested_name_spec, 
            type_name, FULL_UNQUALIFIED_LOOKUP, LF_NO_FAIL, decl_context);

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

static char check_for_typeid(AST expr, scope_t* st, decl_context_t decl_context)
{
    return check_for_type_id_tree(ASTSon0(expr), st, decl_context);
}

static char check_for_type_id_tree(AST type_id, scope_t* st, decl_context_t decl_context)
{
    AST type_specifier_seq = ASTSon0(type_id);
    // AST abstract_declarator = ASTSon1(type_id);
    
    if (ASTType(type_specifier_seq) == AST_AMBIGUITY)
    {
        solve_ambiguous_decl_specifier_seq(type_specifier_seq, st, decl_context);
    }
    
    // This is never NULL
    AST type_specifier = ASTSon1(type_specifier_seq);

    return check_for_type_specifier(type_specifier, st, decl_context);
}

static char check_for_typeid_expr(AST expr, scope_t* st, decl_context_t decl_context)
{
    AST expression = ASTSon0(expr);
    return check_for_expression(expression, st, decl_context);
}

static char check_for_type_specifier(AST type_id, scope_t* st, decl_context_t decl_context)
{
    switch (ASTType(type_id))
    {
        case AST_SIMPLE_TYPE_SPECIFIER :
            return check_for_simple_type_spec(type_id, st, decl_context);
            break;
        case AST_CLASS_SPECIFIER :
        case AST_ENUM_SPECIFIER :
            {
                type_t* simple_type_info;
                simple_type_info = calloc(1, sizeof(*simple_type_info));
                simple_type_info->type = calloc(1, sizeof(*(simple_type_info->type)));

                decl_context.decl_flags |= DF_NO_FAIL;

                gather_type_spec_information(type_id, st, simple_type_info, decl_context);
                return 1;
            }
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
            // GCC Extension
        case AST_GCC_TYPEOF_EXPR :
            {
                return 1;
            }
        default :
            {
                internal_error("Unexpected node '%s'\n", ast_print_node_type(ASTType(type_id)));
            }
    }
}

static char check_for_sizeof_expr(AST expr, scope_t* st, decl_context_t decl_context)
{
    AST sizeof_expression = ASTSon0(expr);
    return check_for_expression(sizeof_expression, st, decl_context);
}

static char check_for_sizeof_typeid(AST expr, scope_t* st, decl_context_t decl_context)
{
    AST type_id = ASTSon0(expr);
    return check_for_type_id_tree(type_id, st, decl_context);
}

static char check_for_cast(AST expr, scope_t* st, decl_context_t decl_context)
{
    AST type_id = ASTSon0(expr);
    if (check_for_type_id_tree(type_id, st, decl_context))
    {
        AST type_specifier = ASTSon0(type_id);
        AST abstract_declarator = ASTSon1(type_id);

        gather_decl_spec_t gather_info;
        memset(&gather_info, 0, sizeof(gather_info));

        decl_context.decl_flags |= DF_NO_FAIL;

        type_t* simple_type_info = NULL;
        build_scope_decl_specifier_seq(type_specifier, st, &gather_info, &simple_type_info, 
                decl_context);

        if (abstract_declarator != NULL)
        {
            type_t* declarator_type = NULL;
            build_scope_declarator(abstract_declarator, st, &gather_info, simple_type_info, 
                    &declarator_type, decl_context);
        }

        solve_possibly_ambiguous_expression(ASTSon1(expr), st, decl_context);

        return 1;
    }
    else
    {
        return 0;
    }
}

void solve_ambiguous_init_declarator(AST a, scope_t* st, decl_context_t decl_context)
{
    int correct_choice = -1;
    int i;

    for (i = 0; i < a->num_ambig; i++)
    {
        AST init_declarator = a->ambig[i];

        if (check_for_init_declarator(init_declarator, st, decl_context))
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

static char check_for_init_declarator(AST init_declarator, scope_t* st, decl_context_t decl_context)
{
    AST declarator = ASTSon0(init_declarator);
    AST initializer = ASTSon1(init_declarator);

    if (!check_for_declarator(declarator, st, decl_context ))
        return 0;

    if (initializer != NULL)
    {
        if (!check_for_initialization(initializer, st, decl_context))
            return 0;
    }

    return 1;
}

static char check_for_parenthesized_initializer(AST initializer_list, scope_t* st, decl_context_t decl_context)
{
    if (ASTType(initializer_list) == AST_AMBIGUITY)
    {
        int current_choice = -1;
        int i;

        for (i = 0; i < initializer_list->num_ambig; i++)
        {
            if (check_for_parenthesized_initializer(initializer_list->ambig[i], st, decl_context))
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

        if (!check_for_expression(expression, st, decl_context))
        {
            return 0;
        }

        // Recurse, because there may be additional ambiguities lying around here
        if (ASTSon0(initializer_list) != NULL)
        {
            return check_for_parenthesized_initializer(ASTSon0(initializer_list), st, decl_context);
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

char check_for_initialization(AST initializer, scope_t* st, decl_context_t decl_context)
{
    switch (ASTType(initializer))
    {
        case AST_CONSTANT_INITIALIZER :
            {
                AST expression = ASTSon0(initializer);
                char result = check_for_expression(expression, st, decl_context);
                return result;
                break;
            }
        case AST_INITIALIZER :
            {
                AST initializer_clause = ASTSon0(initializer);
                return check_for_initializer_clause(initializer_clause, st, decl_context);
                break;
            }
        case AST_PARENTHESIZED_INITIALIZER :
            {
                return check_for_parenthesized_initializer(ASTSon0(initializer), st, decl_context);
                break;
            }
        default :
            {
                internal_error("Unexpected node type '%s'\n", ast_print_node_type(ASTType(initializer)));
            }
    }
    return 1;
}

static char check_for_initializer_list(AST initializer_list, scope_t* st, decl_context_t decl_context)
{
    AST iter;
    for_each_element(initializer_list, iter)
    {
        AST initializer_clause = ASTSon1(iter);

        check_for_initializer_clause(initializer_clause, st, decl_context);
    }

    return 1;
}

static char check_for_designation(AST designation, scope_t* st, decl_context_t decl_context)
{
    AST designator_list = ASTSon0(designation);
    AST iter;

    for_each_element(designator_list, iter)
    {
        AST designator = ASTSon1(iter);

        if (ASTType(designator) == AST_INDEX_DESIGNATOR)
        {
            AST index_designator = designator;
            AST constant_expression = ASTSon0(index_designator);
            solve_possibly_ambiguous_expression(constant_expression, st, decl_context);
        }
    }

    return 1;
}

static char check_for_initializer_clause(AST initializer, scope_t* st, decl_context_t decl_context)
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

                    check_for_initializer_clause(initializer_clause, st, decl_context);
                }
                return 1;
            }
        case AST_INITIALIZER_EXPR :
            {
                AST expression = ASTSon0(initializer);
                char result = check_for_expression(expression, st, decl_context);

                if (result)
                {
                    ASTAttrSetValueType(initializer, LANG_IS_EXPRESSION_NEST, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(initializer, LANG_EXPRESSION_NESTED, tl_type_t, tl_ast(expression));
                }

                return result;
                break;
            }
        case AST_DESIGNATED_INITIALIZER :
            {
                AST designation = ASTSon0(initializer);

                check_for_designation(designation, st, decl_context);

                AST initializer_clause = ASTSon1(initializer);

                return check_for_initializer_clause(initializer_clause, st, decl_context);
                break;
            }
        case AST_GCC_INITIALIZER_CLAUSE :
            {
                AST initializer_clause = ASTSon1(initializer);
                return check_for_initializer_clause(initializer_clause, st, decl_context);
                break;
            }
        default :
            {
                internal_error("Unexpected node type '%s'\n", ast_print_node_type(ASTType(initializer)));
            }
    }

    internal_error("Code unreachable", 0);
    return 0;
}

static char check_for_declarator(AST declarator, scope_t* st, decl_context_t decl_context)
{
    return check_for_declarator_rec(declarator, st, decl_context);
}

static char check_for_declarator_rec(AST declarator, scope_t* st, decl_context_t decl_context)
{
    switch (ASTType(declarator))
    {
        case AST_ABSTRACT_DECLARATOR :
            {
                return check_for_declarator_rec(ASTSon1(declarator), st, decl_context);
                break;
            }
        case AST_DECLARATOR_ARRAY :
        case AST_ABSTRACT_ARRAY :
            {
                if (ASTSon1(declarator) != NULL)
                {
                    solve_possibly_ambiguous_expression(ASTSon1(declarator), st, decl_context);
                }
                if (ASTSon0(declarator) != NULL)
                {
                    return check_for_declarator_rec(ASTSon0(declarator), st, decl_context);
                }
                return 1;
            }
        case AST_PARENTHESIZED_ABSTRACT_DECLARATOR :
        case AST_PARENTHESIZED_DECLARATOR :
        case AST_DECLARATOR :
            {
                return check_for_declarator_rec(ASTSon0(declarator), st, decl_context);
                break;
            }
        case AST_POINTER_DECL :
            {
                return check_for_declarator_rec(ASTSon1(declarator), st, decl_context);
                break;
            }
        case AST_ABSTRACT_DECLARATOR_FUNC :
        case AST_DECLARATOR_FUNC :
            {
                // Check for parameters here
                AST parameter_declaration_clause = ASTSon1(declarator);
                if (parameter_declaration_clause != NULL)
                {
                    if (!check_for_function_declarator_parameters(parameter_declaration_clause, st, decl_context))
                    {
                        return 0;
                    }
                }
                if (ASTSon0(declarator) != NULL)
                {
                    return check_for_declarator_rec(ASTSon0(declarator), st, decl_context);
                }
                return 1;
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

static char check_for_function_declarator_parameters(AST parameter_declaration_clause, scope_t* st, decl_context_t decl_context)
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

                seems_ok &= check_for_type_specifier(type_specifier, st, decl_context);

                if (seems_ok && declarator != NULL)
                {
                    seems_ok &= check_for_declarator(declarator, st, decl_context);
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
                        fprintf(stderr, "Previous choice\n");
                        prettyprint(stderr, previous_choice);
                        fprintf(stderr, "\n");
                        fprintf(stderr, "Current choice\n");
                        prettyprint(stderr, current_choice);
                        fprintf(stderr, "\n");
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
            solve_ambiguous_decl_specifier_seq(decl_specifier_seq, st, decl_context);
        }

        AST type_specifier = ASTSon1(decl_specifier_seq);

        if (!check_for_type_specifier(type_specifier, st, decl_context))
        {
            return 0;
        }

        if (abstract_declarator != NULL)
        {
            if (!check_for_declarator(abstract_declarator, st, decl_context))
            {
                return 0;
            }
        }

        AST default_arg = ASTSon2(parameter);

        if (default_arg != NULL)
        {
            check_for_expression(default_arg, st, decl_context);
        }
    }

    return 1;
}

static char is_abstract_declarator(AST a)
{
    return (ASTType(a) == AST_ABSTRACT_DECLARATOR
            || ASTType(a) == AST_ABSTRACT_DECLARATOR_FUNC
            || ASTType(a) == AST_ABSTRACT_ARRAY);
}

static char is_non_abstract_declarator(AST a)
{
    return (ASTType(a) == AST_DECLARATOR
            || ASTType(a) == AST_POINTER_DECL);
}

void solve_ambiguous_parameter_decl(AST parameter_declaration, scope_t* st, decl_context_t decl_context)
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
            solve_ambiguous_decl_specifier_seq(decl_specifier_seq, st, decl_context);
        }

        AST type_specifier = ASTSon1(decl_specifier_seq);

        if (type_specifier != NULL)
        {
            current_valid &= check_for_type_specifier(type_specifier, st, decl_context);
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

            if (current_valid)
            {
                current_valid &= check_for_declarator(declarator, st, decl_context);
            }
        }

        if (current_valid)
        {
            if (current_choice < 0)
            {
                current_choice = i;
            }
            else
            {
                AST previous_parameter_decl = parameter_declaration->ambig[current_choice];
                AST current_parameter_decl = parameter_decl;

                AST previous_declarator = ASTSon1(previous_parameter_decl);
                AST current_declarator = ASTSon1(current_parameter_decl);

                // If an abstract declarator is possible, then it must be an abstract declarator
                char solved_ambiguity = 0;
                if (previous_declarator != NULL
                        && current_declarator != NULL)
                {
                    if (is_abstract_declarator(previous_declarator)
                            && is_non_abstract_declarator(current_declarator))
                    {
                        solved_ambiguity = 1;
                    }
                    else if (is_non_abstract_declarator(previous_declarator)
                            && is_abstract_declarator(current_declarator))
                    {
                        current_choice = i;
                        solved_ambiguity = 1;
                    }
                }

                if (!solved_ambiguity)
                {
                    internal_error("More than one option is possible in %s", 
                            node_information(parameter_declaration));
                }
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
void solve_ambiguous_type_specifier_seq(AST type_spec_seq, scope_t* st, decl_context_t decl_context)
{
    solve_ambiguous_decl_specifier_seq(type_spec_seq, st, decl_context);
}

void solve_ambiguous_decl_specifier_seq(AST type_spec_seq, scope_t* st, decl_context_t decl_context)
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

void solve_ambiguous_for_init_statement(AST a, scope_t* st, decl_context_t decl_context)
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
                if (check_for_simple_declaration(for_init_statement, st, decl_context))
                {
                    current = 1;
                }
                break;
            case AST_EXPRESSION_STATEMENT :
                if (check_for_expression(ASTSon0(for_init_statement), st, decl_context))
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
        fprintf(stderr, "This init statement cannot be disambiguated:\n");
        prettyprint(stderr, a);
        internal_error("Ambiguity not solved !", 0);
    }
    else
    {
        choose_option(a, correct_choice);
    }
}

void solve_ambiguous_type_specifier(AST ambig_type, scope_t* st, decl_context_t decl_context)
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
            current_typeof = check_for_type_id_tree(typeof_argument, st, decl_context);
        }
        else if (ASTType(type_specifier) == AST_GCC_TYPEOF_EXPR)
        {
            current_typeof = check_for_expression(typeof_argument, st, decl_context);
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

void solve_ambiguous_expression_list(AST expression_list, scope_t* st, decl_context_t decl_context)
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

            result &= check_for_expression(current_expression, st, decl_context);
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
    //  internal_error("Children not found in the parent!\n", 0);
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
