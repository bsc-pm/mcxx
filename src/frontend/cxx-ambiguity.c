/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include <stdio.h>
#include <string.h>
#include "extstruct.h"
#include "cxx-ast.h"
#include "cxx-attrnames.h"
#include "cxx-ambiguity.h"
#include "cxx-typeutils.h"
#include "cxx-utils.h"
#include "cxx-prettyprint.h"
#include "cxx-buildscope.h"
#include "cxx-graphviz.h"
#include "cxx-tltype.h"
#include "cxx-exprtype.h"
#include "cxx-cexpr.h"
#include "cxx-overload.h"

/*
 * This file performs disambiguation. If a symbol table is passed along the
 * tree the disambiguation is context-sensitive otherwise it is entirely
 * context-free (i.e. a flaw in our grammar or the standard grammar)
 *
 */

// This function removes types so we force a recomputation, used only in
// ambiguities not resolved, to avoid hidden messages because of some
// implicit types
static void remove_computed_types(AST t)
{
    if (t == NULL)
        return;

    // Remove the computed type
    ast_set_expression_type(t, NULL);

    int i;
    for (i = 0; i < ASTNumChildren(t); i++)
    {
        remove_computed_types(ASTChild(t, i));
    }
}

static char _ambiguity_testing = 0;
void enter_test_expression(void) 
{ 
    _ambiguity_testing++; 
}
void leave_test_expression(void) 
{ 
    ERROR_CONDITION(_ambiguity_testing <= 0, "This cannot be lower than 1", 0); 
    _ambiguity_testing--; 
}

static void choose_option(AST a, int n);
static int select_node_type(AST a, node_t type);
static AST recursive_search(AST a, node_t type);
static AST look_for_node_type_within_ambig(AST a, node_t type, int n);
static void solve_integral_specification_ambig(AST a);
static void solve_nested_name_with_no_type(AST a);

static void solve_ambiguous_simple_declaration(AST a, decl_context_t decl_context);

static char check_for_declaration_statement(AST a, decl_context_t decl_context);
static char check_for_expression_statement(AST a, decl_context_t decl_context);
// static char check_for_qualified_id(AST expr, decl_context_t decl_context, decl_context_t* symbol_scope);
// static char check_for_symbol(AST expr, decl_context_t decl_context, decl_context_t* symbol_scope);
#if 0
static char check_for_destructor_id(AST expr, decl_context_t decl_context);
#endif
// static char check_for_function_call(AST expr, decl_context_t decl_context);
// static char check_for_explicit_type_conversion(AST expr, decl_context_t decl_context);
// static char check_for_explicit_typename_type_conversion(AST expr, decl_context_t decl_context);
// static char check_for_typeid(AST expr, decl_context_t decl_context);
// static char check_for_typeid_expr(AST expr, decl_context_t decl_context);
// static char check_for_sizeof_expr(AST expr, decl_context_t decl_context);
// static char check_for_sizeof_typeid(AST expr, decl_context_t decl_context);
// static char check_for_cast(AST expr, decl_context_t decl_context);

static char check_for_type_specifier(AST type_id, decl_context_t decl_context);

static char check_for_typeless_declarator(AST declarator, decl_context_t decl_context);

static char check_for_init_declarator(AST init_declarator, decl_context_t decl_context);
static char check_for_declarator(AST declarator, decl_context_t decl_context);
static char check_for_declarator_rec(AST declarator, decl_context_t decl_context);
static char check_for_function_declarator_parameters(AST parameter_declaration_clause, decl_context_t decl_context);

static char check_for_simple_declaration(AST a, decl_context_t decl_context);

// static char check_for_new_expression(AST new_expr, decl_context_t decl_context);
// static char check_for_new_type_id_expr(AST new_expr, decl_context_t decl_context);

// static char check_for_array_subscript_expr(AST expr, decl_context_t decl_context);

#define EXPECT_OPTIONS(a, n) \
do \
{ \
    if (ast_get_num_ambiguities(a) != (n)) \
    { \
       internal_error("We expected %d ambiguities but %d found", (n), ast_get_num_ambiguities(a)); \
    } \
} while (0);


// Returns 1 if ASTType(t1) == n1 && ASTType(t2) == n2
// Returns -1 if ASTType(t1) == n2 && ASTType(t2) == n1
// Returns 0 otherwise
char either_type(AST t1, AST t2, node_t n1, node_t n2)
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
 *
 * There is another ambiguity possible concerning the "unsigned ambiguity"
 */
void solve_parameter_declaration_vs_type_parameter_class(AST a, decl_context_t decl_context)
{
    EXPECT_OPTIONS(a, 2);

    int k = select_node_type(a, AST_TYPE_PARAMETER_CLASS);

    if (k != -1)
    {
        choose_option(a, k);
    }
    // Try the unsigned ambiguity
    else if (select_node_type(a, AST_PARAMETER_DECL) >= 0)
    {
        solve_ambiguous_parameter_decl(a, decl_context);
        ERROR_CONDITION((ASTType(a) == AST_AMBIGUITY), "Ambiguity not solved %s", 
                ast_location(a));
    }
}


/*
 * Ambiguity in a high order declaration
 */
void solve_ambiguous_declaration(AST a, decl_context_t decl_context)
{
// #warning TODO - Refactorize this with the code that makes the same with a single decl_specifier_seq/type_specifier_seq
    char valid;
    int i;
    int j;
    
    // Determine the ambiguity
    // 
    // a) signed/unsigned/short/long

    valid = 1;
    for (i = 0; (i < ast_get_num_ambiguities(a)) && valid; i++)
    {
        AST option = ast_get_ambiguity(a, i);

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
                    for (j = 0; (j < ast_get_num_ambiguities(decl_specifier)) && valid; j++)
                    {
                        AST true_decl_specifier = ast_get_ambiguity(decl_specifier, j);

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
    for (i = 0; (i < ast_get_num_ambiguities(a)) && valid; i++)
    {
        AST option = ast_get_ambiguity(a, i);

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
    for (i = 0; (i < ast_get_num_ambiguities(a)) && valid; i++)
    {
        AST option = ast_get_ambiguity(a, i);

        valid &= (ASTType(option) == AST_SIMPLE_DECLARATION
                || ASTType(option) == AST_MEMBER_DECLARATION);
    }

    if (valid)
    {
        solve_ambiguous_simple_declaration(a, decl_context);
        return;
    }

    internal_error("Don't know how to handle this ambiguity. %s", ast_location(a));
}

static void solve_ambiguous_simple_declaration(AST a, decl_context_t decl_context)
{
    int correct_option = -1;
    int i;
    for (i = 0; i < ast_get_num_ambiguities(a); i++)
    {
        AST option = ast_get_ambiguity(a, i);

        if (check_for_simple_declaration(option, decl_context))
        {
            if (correct_option < 0)
            {
                correct_option = i;
            }
            else
            {
                AST previous_option = ast_get_ambiguity(a, correct_option);
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
    for (i = 0; i < ast_get_num_ambiguities(a); i++)
    {
        AST option = ast_get_ambiguity(a, i);

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

    for (i = 0; (i < ast_get_num_ambiguities(a)) && (candidate == NULL); i++)
    {
        // Choose the one with nonempty init_declarator
        candidate = ast_get_ambiguity(a, i);

        if (ASTSon1(candidate) != NULL)
        {
            candidate = ast_get_ambiguity(a, i);
        }
    }

    int j;

    AST ambig_decl_specifier_seq = ASTSon0(candidate);

    if (ASTType(ambig_decl_specifier_seq) != AST_AMBIGUITY)
    {
        internal_error("I expected an ambiguity here, what has happened to it ?", 0);
    }

    char found = 0;
    for (j = 0; (j < ast_get_num_ambiguities(ambig_decl_specifier_seq)) && !found; j++)
    {
        AST decl_specifier_seq = ast_get_ambiguity(ambig_decl_specifier_seq, j);

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
static char check_for_kr_parameter_list(AST parameters_kr, decl_context_t decl_context)
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

        scope_entry_list_t* entry_list = query_unqualified_name_str(decl_context, ASTText(identifier));

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
void solve_ambiguous_declarator(AST a, decl_context_t decl_context)
{
    int num_ambig = ast_get_num_ambiguities(a);

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
                    
                    AST first_option = ast_get_ambiguity(a, 0);
                    AST second_option = ast_get_ambiguity(a, 1);

                    if (ASTType(first_option) == AST_DECLARATOR_FUNC
                            && ASTType(second_option) == AST_DECLARATOR_FUNC)
                    {
                        AST parameters = ASTSon1(first_option);

                        if (ASTType(parameters) == AST_KR_PARAMETER_LIST)
                        {
                            if (check_for_kr_parameter_list(parameters, decl_context))
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
                            if (check_for_kr_parameter_list(parameters, decl_context))
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

void solve_ambiguous_statement(AST a, decl_context_t decl_context)
{
    // The strategy used here is to check every ambiguity and select
    // the valid one
    int correct_choice = -1;
    int i;

    for (i = 0; i < ast_get_num_ambiguities(a); i++)
    {
        char current_check = 0;

        switch (ASTType(ast_get_ambiguity(a, i)))
        {
            case AST_DECLARATION_STATEMENT :
                {
                    current_check = check_for_declaration_statement(ast_get_ambiguity(a, i), decl_context);
                    break;
                }
            case AST_EXPRESSION_STATEMENT :
                {
                    enter_test_expression();
                    current_check = check_for_expression_statement(ast_get_ambiguity(a, i), decl_context);
                    leave_test_expression();
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
                AST first_option = ASTSon0(ast_get_ambiguity(a, correct_choice));
                AST second_option = ASTSon0(ast_get_ambiguity(a, i));

                int either;
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
                                ast_location(second_option));
                }
            }
        }
    }

    if (correct_choice < 0)
    {
        // Recheck the expression again
        for (i = 0; i < ast_get_num_ambiguities(a); i++)
        {
            switch (ASTType(ast_get_ambiguity(a, i)))
            {
                case AST_EXPRESSION_STATEMENT :
                    {
                        AST ambiguous_tree_as_expr = ast_get_ambiguity(a, i);
                        // This will output some informational messages that might
                        // help solving this ambiguity
                        remove_computed_types(ambiguous_tree_as_expr);
                        check_for_expression_statement(ambiguous_tree_as_expr, decl_context);
                        break;
                    }
                default:
                    {
                        break;
                    }
            }
        }

        running_error("%s: error: cannot continue due to serious semantic problems in '%s'",
                ast_location(a), prettyprint_in_buffer(a));
    }
    else
    {
        choose_option(a, correct_choice);
    }
}


static char check_for_init_declarator_list(AST init_declarator_list, decl_context_t decl_context)
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
            for (i = 0; i < ast_get_num_ambiguities(init_declarator); i++)
            {
                AST one_init_decl = ast_get_ambiguity(init_declarator, i);

                if (check_for_init_declarator(one_init_decl, decl_context))
                {
                    if (current_choice < 0)
                    {
                        current_choice = i;
                    }
                    else
                    {
                        AST current_option = ast_get_ambiguity(init_declarator, i);
                        AST previous_option = ast_get_ambiguity(init_declarator, current_choice);

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
            if (!check_for_init_declarator(init_declarator, decl_context))
            {
                return 0;
            }
        }
    }

    return 1;
}

static char check_for_decl_spec_seq_followed_by_declarator(AST decl_specifier_seq, AST declarator, decl_context_t decl_context)
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
        AST declarator_name = get_leftmost_declarator_name(declarator, decl_context);

        if (declarator_name != NULL)
        {
            if (ASTType(declarator_name) == AST_QUALIFIED_ID)
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

static char check_for_simple_declaration(AST a, decl_context_t decl_context)
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
                first_declarator = ASTSon0(ast_get_ambiguity(first_init_declarator, 0));
            }
            else
            {
                first_declarator = ASTSon0(first_init_declarator);
            }

            if (ASTType(first_init_declarator) != AST_BITFIELD_DECLARATOR
                    || first_declarator != NULL)
            {
                if (!check_for_decl_spec_seq_followed_by_declarator(decl_specifier_seq, first_declarator, decl_context))
                {
                    return 0;
                }
            }
        }

        AST type_spec = ASTSon1(decl_specifier_seq);

        if (type_spec != NULL)
        {
            if (!check_for_type_specifier(type_spec, decl_context))
            {
                return 0;
            }
        }

        if (first_init_declarator != NULL
                && ASTType(first_init_declarator) == AST_AMBIGUITY)
        {
            solve_ambiguous_init_declarator(first_init_declarator, decl_context);
        }

        // AST init_declarator_list = ASTSon1(a);
        // if (init_declarator_list != NULL)
        // {
        //     if (!check_for_init_declarator_list(init_declarator_list, decl_context))
        //     {
        //         return 0;
        //     }
        // }
        //
        

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
                scope_entry_list_t* entry_list = query_id_expression(decl_context, id_expression);

                // T names a type
                if (entry_list != NULL 
                        && (entry_list->entry->kind == SK_TYPEDEF
                            || entry_list->entry->kind == SK_ENUM
                            || entry_list->entry->kind == SK_CLASS
                            || entry_list->entry->kind == SK_TEMPLATE_TYPE_PARAMETER))
                {
                    // A is a simple type specifier
                    if (ASTType(type_spec) == AST_SIMPLE_TYPE_SPECIFIER)
                    {
                        AST global_op = ASTSon0(type_spec);
                        AST nested_name_spec = ASTSon1(type_spec);
                        AST type_name = ASTSon2(type_spec);

                        entry_list = query_nested_name(decl_context, global_op, nested_name_spec, 
                                type_name);

                        // A is of class nature
                        if (entry_list != NULL
                                && (entry_list->entry->kind == SK_CLASS))
                        {
                            scope_entry_t* entry = entry_list->entry;
                            
                            // The related scope of A is the same as the
                            // current scope
                            // Should check this is the injected class name
                            if (entry->entity_specs.is_injected_class_name)
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
                for (i = 0; i < ast_get_num_ambiguities(init_declarator); i++)
                {
                    AST opt_declarator = ASTSon0(ast_get_ambiguity(init_declarator, i));

                    if (check_for_typeless_declarator(opt_declarator, decl_context))
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
                if (!check_for_typeless_declarator(declarator, decl_context))
                {
                    return 0;
                }
            }
        }
    }

    return 1;
}

static char check_for_declaration_statement(AST declaration_statement, decl_context_t decl_context)
{
    AST a = ASTSon0(declaration_statement);

    // In general only AST_SIMPLE_DECLARATION gets ambiguous here
    if (ASTType(a) == AST_SIMPLE_DECLARATION)
    {
        return check_for_simple_declaration(a, decl_context);
    }
    else if (ASTType(a) == AST_AMBIGUITY)
    {
        // internal_error("Unknown node type '%s' (line=%d)\n", ast_print_node_type(ASTType(a)),
        //      ast_location(a));
        //
        // In general only AST_SIMPLE_DECLARATION gets ambiguous here

        int correct_choice = -1;
        int i;
        for (i = 0; i < ast_get_num_ambiguities(a); i++)
        {
            if (check_for_simple_declaration(ast_get_ambiguity(a, i), decl_context))
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

static char check_for_typeless_declarator_rec(AST declarator, decl_context_t decl_context, int nfuncs)
{
    switch (ASTType(declarator))
    {
        case AST_PARENTHESIZED_EXPRESSION :
        case AST_DECLARATOR :
            {
                return check_for_typeless_declarator_rec(ASTSon0(declarator), 
                        decl_context, nfuncs);
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
                return check_for_typeless_declarator_rec(ASTSon0(declarator), decl_context, nfuncs+1);
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

                // These always have type
                if (ASTType(symbol) == AST_OPERATOR_FUNCTION_ID
                        || ASTType(symbol) == AST_TEMPLATE_ID
                        || ASTType(symbol) == AST_OPERATOR_FUNCTION_ID_TEMPLATE)
                {
                    return 0;
                }
                
                scope_entry_list_t* result_list = query_nested_name(decl_context, 
                        global_scope, nested_name_spec, symbol);

                enum cxx_symbol_kind filter_classes[] = {
                    SK_CLASS, 
                };

                scope_entry_list_t* classes_list = filter_symbol_kind_set(result_list, STATIC_ARRAY_LENGTH(filter_classes), filter_classes);

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
                const char* class_name = ASTText(id_expression);

                // We want a class scope
                if (decl_context.current_scope->kind != CLASS_SCOPE)
                {
                    return 0;
                }

                if (ASTType(id_expression) == AST_DESTRUCTOR_ID ||
                        ASTType(id_expression) == AST_DESTRUCTOR_TEMPLATE_ID)
                {
                    // Spring '~'
                    class_name++;
                }

                // Now look for the class symbol in the enclosing scope
                //
                //   class A {
                //      A();  <-- valid
                //      ~A(); <-- valid
                //   };
                //
                scope_entry_list_t* result = query_in_scope_str(decl_context, class_name);

                if (result == NULL
                        || (result->entry->kind != SK_CLASS))
                {
                    // This is not a class name
                    return 0;
                }

                // It looks sane here
                return 1;
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

static char check_for_typeless_declarator(AST declarator, decl_context_t decl_context)
{
    return check_for_typeless_declarator_rec(declarator, decl_context, 0);
}

static char check_for_expression_statement(AST a, decl_context_t decl_context)
{
    AST expression = ASTSon0(a);

    char result = 
        check_for_expression(expression, decl_context );

    return result;
}

#define ENSURE_TYPE(expr, type) \
do { \
    if (ASTType(expr) != type) \
    { \
        internal_error("Expecting node of type '"#type"' but got '%s'\n", ast_print_node_type(ASTType(expr))); \
    } \
} \
while (0);





// Returns if the template_argument could be disambiguated.
// If it can be disambiguated, it is disambiguated here
char solve_ambiguous_template_argument(AST ambig_template_argument, decl_context_t decl_context)
{
    int i;

    int selected_option = -1;
    for (i = 0; i < ast_get_num_ambiguities(ambig_template_argument); i++)
    {
        char current_option = 0;
        AST current_template_argument = ast_get_ambiguity(ambig_template_argument, i);

        switch (ASTType(current_template_argument))
        {
            case AST_TEMPLATE_TYPE_ARGUMENT :
                {
                    AST type_id = ASTSon0(current_template_argument);

                    current_option = check_for_type_id_tree(type_id, decl_context);
                    break;
                }
            case AST_TEMPLATE_EXPRESSION_ARGUMENT :
                {
                    AST expression_arg = ASTSon0(current_template_argument);

                    enter_test_expression();
                    current_option = check_for_expression(expression_arg, decl_context);
                    leave_test_expression();

                    break;
                }
            default :
                internal_error("Unknown node '%s' at '%s'\n", ast_print_node_type(ASTType(current_template_argument)), 
                        ast_location(current_template_argument));
                break;
        }
        
        if (current_option)
        {
            if (selected_option < 0)
            {
                selected_option = i;
            }
            else
            {
                AST previous_template_argument = ast_get_ambiguity(ambig_template_argument, selected_option);

                int either;
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
        // DEBUG_CODE()
        // {
        //     fprintf(stderr, "Template argument '");
        //     prettyprint(stderr, ambig_template_argument);
        //     fprintf(stderr, "'\n");
        // }
        // internal_error("No valid choice found in %s for '%s' ", ast_location(ambig_template_argument),
        //         prettyprint_in_buffer(ambig_template_argument));

        // Could not be disambiguated
        return 0;
    }
    else
    {
        // Can be disambiguated, so we do it
        choose_option(ambig_template_argument, selected_option);
        return 1;
    }
}

static char check_for_template_argument_list(AST argument_list, decl_context_t decl_context);

// Returns false if expression arguments do not pass the check_for_expression test,
// otherwise returns true.
char solve_possibly_ambiguous_template_id(AST type_name, decl_context_t decl_context)
{
    char result = 1;
    if (ASTType(type_name) != AST_TEMPLATE_ID
            && ASTType(type_name) != AST_OPERATOR_FUNCTION_ID_TEMPLATE)
    {
        internal_error("Unexpected node '%s' only AST_TEMPLATE_ID or AST_OPERATOR_FUNCTION_ID_TEMPLATE allowed", 
                ast_print_node_type(ASTType(type_name)));
    }

    // For every argument solve its possible ambiguities
    AST argument_list = ASTSon1(type_name);

    if (argument_list != NULL)
    {
        if (ASTType(argument_list) == AST_AMBIGUITY)
        {
            // If this is ambiguous, check for the argument_list that is feasible, and choose it,
            // if no feasible found, this is not a valid template_id
            int i;
            int feasible_list = -1;
            for (i = 0; i < ast_get_num_ambiguities(argument_list); i++)
            {
                if (check_for_template_argument_list(ast_get_ambiguity(argument_list, i), decl_context))
                {
                    if (feasible_list < 0)
                    {
                        internal_error("Two feasible_list template argument lists!\n", 0);
                    }
                    else
                    {
                        feasible_list = i;
                    }
                }
            }
            if (feasible_list < 0)
            {
                result = 0;
            }
            else
            {
                choose_option(argument_list, feasible_list);
            }
        }
        else
        {
            result = check_for_template_argument_list(argument_list, decl_context);
        }
    }
    
    return result;
}

static char check_for_template_argument_list(AST argument_list, decl_context_t decl_context)
{
    ENSURE_TYPE(argument_list, AST_NODE_LIST);

    if (argument_list != NULL)
    {
        AST iter;
        for_each_element(argument_list, iter)
        {
            AST template_argument = ASTSon1(iter);

            if (ASTType(template_argument) == AST_AMBIGUITY)
            {
                char valid_template_argument = solve_ambiguous_template_argument(template_argument, decl_context);
                if (!valid_template_argument)
                    return 0;
            }

            if (ASTType(template_argument) == AST_TEMPLATE_EXPRESSION_ARGUMENT)
            {
                enter_test_expression();
                char valid_template_argument = check_for_expression(ASTSon0(template_argument), decl_context);
                leave_test_expression();
                if (!valid_template_argument)
                    return 0;
            }
            else if (ASTType(template_argument) == AST_TEMPLATE_TYPE_ARGUMENT)
            {
                AST type_id = ASTSon0(template_argument);

                AST type_specifier = ASTSon0(type_id);
                AST abstract_declarator = ASTSon1(type_id);

                gather_decl_spec_t gather_info;
                memset(&gather_info, 0, sizeof(gather_info));

                type_t* simple_type_info = NULL;
                build_scope_decl_specifier_seq(type_specifier, &gather_info, &simple_type_info, 
                        decl_context);

                type_t* declarator_type = NULL;
                compute_declarator_type(abstract_declarator, &gather_info, simple_type_info, 
                        &declarator_type, decl_context);
            }
        }
    }
    return 1;
}

char check_for_simple_type_spec(AST type_spec, decl_context_t decl_context, type_t** computed_type)
{
    if (computed_type != NULL)
    {
        *computed_type = NULL;
    }

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
                {
                    if (computed_type != NULL)
                    {
                        gather_decl_spec_t gather_info;
                        memset(&gather_info, 0, sizeof(gather_info));

                        gather_type_spec_information(type_spec, computed_type, &gather_info, decl_context);
                    }
                    return 1;
                }
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
        if (!solve_possibly_ambiguous_template_id(type_name, decl_context))
        {
            return 0;
        }
    }

    scope_entry_list_t* entry_list = query_nested_name(decl_context, global_op, nested_name_spec, 
            type_name);

    if (entry_list == NULL)
    {
        return 0;
    }

    scope_entry_t* entry = entry_list->entry;
    if (entry->kind == SK_TYPEDEF
            || entry->kind == SK_ENUM
            || entry->kind == SK_CLASS
            // We allow this because templates are like types
            || entry->kind == SK_TEMPLATE
            || entry->kind == SK_TEMPLATE_TYPE_PARAMETER
            || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
    {
        if (entry->kind == SK_TEMPLATE)
        {
            // Check that the template-name is actually a template class name
            // and not a template function name
            type_t* primary = template_type_get_primary_type(entry->type_information);
            if (!is_named_class_type(primary))
            {
                return 0;
            }
        }

        if (computed_type != NULL)
        {
            *computed_type = get_user_defined_type(entry);
        }
        return 1;
    }
    else
    {
        return 0;
    }
}



static char check_for_type_specifier(AST type_id, decl_context_t decl_context)
{
    switch (ASTType(type_id))
    {
        case AST_SIMPLE_TYPE_SPECIFIER :
            return check_for_simple_type_spec(type_id, decl_context, /* computed_type = */ NULL);
            break;
        case AST_CLASS_SPECIFIER :
        case AST_ENUM_SPECIFIER :
            {
                type_t* type_info = NULL;

                gather_decl_spec_t gather_info;
                memset(&gather_info, 0, sizeof(gather_info));

                gather_type_spec_information(type_id, &type_info, &gather_info, decl_context);
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
        case AST_GCC_COMPLEX_TYPE: 
        case AST_GCC_IMAGINARY_TYPE: 
            {
                return 1;
            }
            // GCC Extension
        case AST_GCC_TYPEOF_EXPR :
            {
                enter_test_expression();
                char result = check_for_expression(ASTSon0(type_id), decl_context);
                leave_test_expression();
                return result;
            }
        case AST_GCC_TYPEOF :
            {
                return check_for_type_id_tree(ASTSon0(type_id), decl_context);
            }
            // There is an ambiguity between AST_GCC_TYPEOF_EXPR and AST_GCC_TYPEOF
        case AST_AMBIGUITY :
            {
                int valid = -1;
                int i;
                for (i = 0; i < ast_get_num_ambiguities(type_id); i++)
                {
                    if (check_for_type_specifier(ast_get_ambiguity(type_id, i), decl_context))
                    {
                        if (valid < 0)
                        {
                            valid = i;
                        }
                        else
                        {
                            internal_error("Two or more valid type-id trees '%s' in %s\n", 
                                    prettyprint_in_buffer(type_id),
                                    ast_location(type_id));
                        }
                    }
                }

                if (valid < 0)
                {
                    internal_error("Cannot solve ambiguity of type-id '%s' in '%s'\n", 
                            prettyprint_in_buffer(type_id),
                            ast_location(type_id));
                }

                choose_option(type_id, valid);

                // This is always a valid type
                return 1;
                break;
            }
        default :
            {
                internal_error("Unexpected node '%s'\n", ast_print_node_type(ASTType(type_id)));
            }
    }
}

void solve_ambiguous_init_declarator(AST a, decl_context_t decl_context)
{
    int correct_choice = -1;
    int i;

    for (i = 0; i < ast_get_num_ambiguities(a); i++)
    {
        AST init_declarator = ast_get_ambiguity(a, i);

        if (check_for_init_declarator(init_declarator, decl_context))
        {
            if (correct_choice < 0)
            {
                correct_choice = i;
            }
            else
            {
                // Ambiguity: T t(Q()); where T and Q are type-names always solves to 
                // function declaration

                AST previous_choice = ast_get_ambiguity(a, correct_choice);
                AST previous_choice_declarator = ASTSon0(previous_choice);

                AST current_choice_declarator = ASTSon0(init_declarator);

                int either;
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

static char check_for_init_declarator(AST init_declarator, decl_context_t decl_context)
{
    AST declarator = ASTSon0(init_declarator);
    AST initializer = ASTSon1(init_declarator);

    if (!check_for_declarator(declarator, decl_context ))
        return 0;

    if (initializer != NULL)
    {
        if (!check_for_initialization(initializer, decl_context))
            return 0;
    }

    return 1;
}

static char check_for_declarator(AST declarator, decl_context_t decl_context)
{
    return check_for_declarator_rec(declarator, decl_context);
}

static char check_for_declarator_rec(AST declarator, decl_context_t decl_context)
{
    switch (ASTType(declarator))
    {
        case AST_ABSTRACT_DECLARATOR :
            {
                return check_for_declarator_rec(ASTSon1(declarator), decl_context);
                break;
            }
        case AST_DECLARATOR_ARRAY :
        case AST_ABSTRACT_ARRAY :
            {
                if (ASTSon1(declarator) != NULL)
                {
                    enter_test_expression();
                    char result = check_for_expression(ASTSon1(declarator), decl_context);
                    leave_test_expression();

                    if (!result)
                    {
                        return 0;
                    }
                }
                if (ASTSon0(declarator) != NULL)
                {
                    return check_for_declarator_rec(ASTSon0(declarator), decl_context);
                }
                return 1;
            }
        case AST_PARENTHESIZED_ABSTRACT_DECLARATOR :
        case AST_PARENTHESIZED_DECLARATOR :
        case AST_DECLARATOR :
            {
                return check_for_declarator_rec(ASTSon0(declarator), decl_context);
                break;
            }
        case AST_POINTER_DECL :
            {
                return check_for_declarator_rec(ASTSon1(declarator), decl_context);
                break;
            }
        case AST_ABSTRACT_DECLARATOR_FUNC :
        case AST_DECLARATOR_FUNC :
            {
                // Check for parameters here
                AST parameter_declaration_clause = ASTSon1(declarator);
                if (parameter_declaration_clause != NULL)
                {
                    if (!check_for_function_declarator_parameters(parameter_declaration_clause, decl_context))
                    {
                        return 0;
                    }
                }
                if (ASTSon0(declarator) != NULL)
                {
                    return check_for_declarator_rec(ASTSon0(declarator), decl_context);
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

static char check_for_function_declarator_parameters(AST parameter_declaration_clause, decl_context_t decl_context)
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
            for (i = 0; i < ast_get_num_ambiguities(parameter); i++)
            {
                AST parameter_decl = ast_get_ambiguity(parameter, i);

                AST decl_specifier_seq = ASTSon0(parameter_decl);
                AST type_specifier = ASTSon1(decl_specifier_seq);
                AST declarator = ASTSon1(parameter_decl);

                char seems_ok = 1;

                seems_ok &= check_for_type_specifier(type_specifier, decl_context);

                if (seems_ok && declarator != NULL)
                {
                    seems_ok &= check_for_declarator(declarator, decl_context);
                    seems_ok &= check_for_decl_spec_seq_followed_by_declarator(decl_specifier_seq, declarator, decl_context);
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
                        AST previous_choice = ast_get_ambiguity(parameter, correct_choice);
                        fprintf(stderr, "Previous choice\n");
                        prettyprint(stderr, previous_choice);
                        fprintf(stderr, "\n");
                        fprintf(stderr, "Current choice\n");
                        prettyprint(stderr, current_choice);
                        fprintf(stderr, "\n");
                        internal_error("More than one valid alternative '%s' vs '%s' %s", 
                                ast_print_node_type(ASTType(previous_choice)),
                                ast_print_node_type(ASTType(current_choice)),
                                ast_location(previous_choice));
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
            solve_ambiguous_decl_specifier_seq(decl_specifier_seq, decl_context);
        }

        AST type_specifier = ASTSon1(decl_specifier_seq);

        if (!check_for_type_specifier(type_specifier, decl_context))
        {
            return 0;
        }

        if (abstract_declarator != NULL)
        {
            if (!check_for_declarator(abstract_declarator, decl_context))
            {
                return 0;
            }
        }

        AST default_arg = ASTSon2(parameter);

        if (default_arg != NULL)
        {
            check_for_expression(default_arg, decl_context);
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

void solve_ambiguous_parameter_decl(AST parameter_declaration, decl_context_t decl_context)
{
    int current_choice = -1;
    int i;
    for (i = 0; i < ast_get_num_ambiguities(parameter_declaration); i++)
    {
        char current_valid = 1;
        AST parameter_decl = ast_get_ambiguity(parameter_declaration, i);

        AST decl_specifier_seq = ASTSon0(parameter_decl);

        if (ASTType(decl_specifier_seq) == AST_AMBIGUITY)
        {
            solve_ambiguous_decl_specifier_seq(decl_specifier_seq, decl_context);
        }

        AST type_specifier = ASTSon1(decl_specifier_seq);

        if (type_specifier != NULL)
        {
            current_valid &= check_for_type_specifier(type_specifier, decl_context);
        }
        else
        {
            // There must be type_spec in a parameter_decl
            current_valid = 0;
        }

        AST declarator = ASTSon1(parameter_decl);

        if (declarator != NULL)
        {
            current_valid &= check_for_decl_spec_seq_followed_by_declarator(decl_specifier_seq, declarator, decl_context);

            if (current_valid)
            {
                current_valid &= check_for_declarator(declarator, decl_context);
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
                AST previous_parameter_decl = ast_get_ambiguity(parameter_declaration, current_choice);
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
                            ast_location(parameter_declaration));
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
void solve_ambiguous_type_specifier_seq(AST type_spec_seq, decl_context_t decl_context)
{
    solve_ambiguous_decl_specifier_seq(type_spec_seq, decl_context);
}

void solve_ambiguous_decl_specifier_seq(AST type_spec_seq, 
        decl_context_t decl_context UNUSED_PARAMETER)
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
    char complex_ambiguity = 1;

    for (i = 0; i < ast_get_num_ambiguities(type_spec_seq); i++)
    {
        AST type_specifier_seq = ast_get_ambiguity(type_spec_seq, i);

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
            if (ASTType(type_specifier) != AST_GCC_COMPLEX_TYPE
                    && ASTType(type_specifier) != AST_GCC_IMAGINARY_TYPE)
            {
                complex_ambiguity = 0;
                break;
            }
        }
    }

    if (!integral_ambiguity
            && !complex_ambiguity)
    {
        internal_error("Unknown ambiguity at '%s'\n", ast_location(type_spec_seq));
    }
    else
    {
        // This is a bit different from solve_integral_specification_ambig
        // Choose the first one that has type_specifier
        for (i = 0; i < ast_get_num_ambiguities(type_spec_seq); i++)
        {
            AST type_specifier_seq = ast_get_ambiguity(type_spec_seq, i);
            
            if (ASTSon1(type_specifier_seq) != NULL)
            {
                choose_option(type_spec_seq, i);
                break;
            }
        }
    }
}

void solve_ambiguous_for_init_statement(AST a, decl_context_t decl_context)
{
    int correct_choice = -1;
    int i;
    for (i = 0; i < ast_get_num_ambiguities(a); i++)
    {
        int current = 0;
        AST for_init_statement = ast_get_ambiguity(a, i);

        switch (ASTType(for_init_statement))
        {
            case AST_SIMPLE_DECLARATION :
                if (check_for_simple_declaration(for_init_statement, decl_context))
                {
                    current = 1;
                }
                break;
            case AST_EXPRESSION_STATEMENT :
                {
                    enter_test_expression();
                    if (check_for_expression(ASTSon0(for_init_statement), decl_context))
                    {
                        current = 1;
                    }
                    leave_test_expression();
                }
                break;
            default :
                internal_error("Unknown node '%s' at '%s'\n", 
                        ast_print_node_type(ASTType(for_init_statement)),
                        ast_location(for_init_statement));
        }

        if (current)
        {
            if (correct_choice < 0)
            {
                correct_choice = i;
            }
            else
            {
                internal_error("More than one valid choice! %s vs %s\n", ast_print_node_type(ASTType(ast_get_ambiguity(a, i))),
                        ast_print_node_type(ASTType(ast_get_ambiguity(a, correct_choice))));
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

void solve_ambiguous_type_specifier(AST ambig_type, decl_context_t decl_context)
{
    // The unique ambiguity that should happen here is the one below
    //
    //   __typeof(foo) bar;
    //
    // We don't know if foo is a type or an expression
    
    char is_typeof_ambiguity = 1;
    int i;
    for (i = 0; (i < ast_get_num_ambiguities(ambig_type)) && is_typeof_ambiguity; i++)
    {
        AST type_specifier = ast_get_ambiguity(ambig_type, i);

        is_typeof_ambiguity = ((ASTType(type_specifier) == AST_GCC_TYPEOF)
                || (ASTType(type_specifier) == AST_GCC_TYPEOF_EXPR));
    }

    if (!is_typeof_ambiguity)
    {
        internal_error("Unknown ambiguity at '%s'!\n", 
                ast_location(ambig_type));
    }

    // Solve typeof ambiguity
    int typeof_choice = -1;
    for (i = 0; i < ast_get_num_ambiguities(ambig_type); i++)
    {
        char current_typeof = 0;
        AST type_specifier = ast_get_ambiguity(ambig_type, i);
        AST typeof_argument = ASTSon0(type_specifier);

        if (ASTType(type_specifier) == AST_GCC_TYPEOF)
        {
            current_typeof = check_for_type_id_tree(typeof_argument, decl_context);
        }
        else if (ASTType(type_specifier) == AST_GCC_TYPEOF_EXPR)
        {
            enter_test_expression();
            current_typeof = check_for_expression(typeof_argument, decl_context);
            leave_test_expression();
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

void solve_ambiguous_expression_list(AST expression_list, decl_context_t decl_context)
{
    int correct_choice = -1;
    int i;
    char result = 1;
    for (i = 0; i < ast_get_num_ambiguities(expression_list); i++)
    {
        AST current_expression_list = ast_get_ambiguity(expression_list, i);
        AST iter;

        for_each_element(current_expression_list, iter)
        {
            AST current_expression = ASTSon1(iter);

            enter_test_expression();
            result &= check_for_expression(current_expression, decl_context);
            leave_test_expression();
        }

        if (result)
        {
            if (correct_choice < 0)
            {
                correct_choice = i;
            }
            else
            {
                AST previous_choice = ast_get_ambiguity(expression_list, correct_choice);
                AST current_choice = ast_get_ambiguity(expression_list, i);
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
    ast_replace_with_ambiguity(a, n);
}

// Returns the index of the first node of type "type"
static int select_node_type(AST a, node_t type)
{
    int i;

    for (i = 0; i < ast_get_num_ambiguities(a); i++)
    {
        if (ASTType(ast_get_ambiguity(a, i)) == type)
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
    if (n >= ast_get_num_ambiguities(a))
    {
        internal_error("There is no such option (%d) in this ambiguous node (options = %d)", n, ast_get_num_ambiguities(a));
    }
    else if (n < 0)
    {
        internal_error("Invalid node number (%d)", n);
    }

    AST result = recursive_search(ast_get_ambiguity(a, n), type);

    return result;
}



void solve_ambiguous_exception_decl(AST exception_decl, decl_context_t decl_context)
{
    // They share the same layout
    solve_ambiguous_parameter_decl(exception_decl, decl_context);
}

char check_nested_name_spec(AST nested_name_spec, decl_context_t decl_context)
{
    char result = 1;
    while (nested_name_spec != NULL)
    {
        AST current_name = ASTSon0(nested_name_spec);

        if (ASTType(current_name) == AST_TEMPLATE_ID)
        {
            result &= solve_possibly_ambiguous_template_id(current_name, decl_context);
        }

        nested_name_spec = ASTSon1(nested_name_spec);
    }

    return result;
}


char check_for_type_id_tree(AST type_id, decl_context_t decl_context)
{
    AST type_specifier_seq = ASTSon0(type_id);
    // AST abstract_declarator = ASTSon1(type_id);
    
    if (ASTType(type_specifier_seq) == AST_AMBIGUITY)
    {
        solve_ambiguous_decl_specifier_seq(type_specifier_seq, decl_context);
    }
    
    // This is never NULL
    AST type_specifier = ASTSon1(type_specifier_seq);

    return check_for_type_specifier(type_specifier, decl_context);
}


// States if we are checking ambiguities
char checking_ambiguity(void)
{
    return (_ambiguity_testing != 0);
}

char solve_ambiguous_expression(AST ambig_expression, decl_context_t decl_context)
{
    ERROR_CONDITION(ASTType(ambig_expression) != AST_AMBIGUITY,
            "Must be ambiguous node", 0);

    int correct_choice = -1;
    int i;
    for (i = 0; i < ast_get_num_ambiguities(ambig_expression); i++)
    {
        enter_test_expression();
        char current_check = 
            check_for_expression(ast_get_ambiguity(ambig_expression, i), decl_context);
        leave_test_expression();

        if (current_check)
        {
            if (correct_choice < 0)
            {
                correct_choice = i;
            }
            else
            {
                // Favor known ambiguities
                AST previous_choice = ast_get_ambiguity(ambig_expression, correct_choice);
                AST current_choice = ast_get_ambiguity(ambig_expression, i);

                int either;
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
                // ambig_expression instead of saying that this is not a
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
                    internal_error("More than one valid choice for ambig_expression (%s)\n'%s' vs '%s'\n%s\n", 
                            ast_location(ambig_expression), ast_print_node_type(ASTType(ast_get_ambiguity(ambig_expression, i))), 
                            ast_print_node_type(ASTType(ast_get_ambiguity(ambig_expression, correct_choice))),
                            prettyprint_in_buffer(ambig_expression));
                }
            }
        }
    }

    char result = 0;

    if (correct_choice < 0)
    {
        if (!checking_ambiguity())
        {
            // No ambiguity is valid
            // Print some messages for the function calls being the first cause
            // of problems
            for (i = 0; i < ast_get_num_ambiguities(ambig_expression); i++)
            {
                if (ASTType(ast_get_ambiguity(ambig_expression, i)) == AST_FUNCTION_CALL)
                {
                    check_for_expression(ast_get_ambiguity(ambig_expression, i), decl_context);
                }

                // Choose this one just to avoid spurious errors later
                choose_option(ambig_expression, i);
                break;
            }
        }
        result = 0;
    }
    else
    {
        // Choose the option and state that this can be valid
        choose_option(ambig_expression, correct_choice);
        result = 1;
    }
    
    return result;
}

char check_for_parenthesized_initializer(AST initializer_list, decl_context_t decl_context)
{
    if (ASTType(initializer_list) == AST_AMBIGUITY)
    {
        int current_choice = -1;
        int i;

        for (i = 0; i < ast_get_num_ambiguities(initializer_list); i++)
        {
            if (check_for_parenthesized_initializer(ast_get_ambiguity(initializer_list, i), decl_context))
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

        enter_test_expression();
        char result = check_for_expression(expression, decl_context);
        leave_test_expression();

        if (!result)
        {
            return 0;
        }

        // Recurse, because there may be additional ambiguities lying around here
        if (ASTSon0(initializer_list) != NULL)
        {
            return check_for_parenthesized_initializer(ASTSon0(initializer_list), decl_context);
        }
        else
        {
            return 1;
        }
    }
    else
    {
        internal_error("Unknown node '%s' at '%s'", 
                ast_print_node_type(ASTType(initializer_list)), ast_location(initializer_list));

        return 0;
    }
    return 0;
}
