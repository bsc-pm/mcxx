/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information
  regarding developers and contributors.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/




#include <stdio.h>
#include <string.h>
#include "cxx-ast.h"
#include "cxx-ambiguity.h"
#include "cxx-typeutils.h"
#include "cxx-utils.h"
#include "cxx-prettyprint.h"
#include "cxx-buildscope.h"
#include "cxx-graphviz.h"
#include "cxx-tltype.h"
#include "cxx-exprtype.h"
#include "cxx-cexpr.h"
#include "cxx-entrylist.h"
#include "cxx-overload.h"
#include "cxx-diagnostic.h"

/*
 * This file performs disambiguation. If a symbol table is passed along the
 * tree the disambiguation is context-sensitive otherwise it is entirely
 * context-DELETE (i.e. a flaw in our grammar or the standard grammar)
 *
 */

// Generic routines
void solve_ambiguity_generic(AST a, const decl_context_t* decl_context, void *info,
        ambiguity_check_intepretation_fun_t* ambiguity_check_intepretation,
        ambiguity_choose_interpretation_fun_t* ambiguity_choose_interpretation,
        ambiguity_fallback_interpretation_fun_t* ambiguity_fallback_interpretation
        )
{
    ERROR_CONDITION(ASTKind(a) != AST_AMBIGUITY, "Tree is not an ambiguity", 0);

    int valid_option = -1;

    int i, n = ast_get_num_ambiguities(a);

    diagnostic_context_t* ambig_diag[n + 1];

    for (i = 0; i < n; i++)
    {
        AST current_interpretation = ast_get_ambiguity(a, i);

        ast_fix_parents_inside_intepretation(current_interpretation);

        ambig_diag[i] = diagnostic_context_push_buffered();
        char c = ambiguity_check_intepretation(current_interpretation, decl_context, i, info);
        diagnostic_context_pop();

        if (c)
        {
            if (valid_option < 0)
            {
                valid_option = i;
            }
            else
            {
                AST previous_interpretation = ast_get_ambiguity(a, valid_option);
                int chosen_result = 0;

                if (ambiguity_choose_interpretation != NULL)
                {
                    // WARNING: previous_interpretation may have wrong parents
                    // since we cannot fix two trees that may be sharing nodes
                    chosen_result = ambiguity_choose_interpretation(
                            current_interpretation,
                            previous_interpretation,
                            i, valid_option,
                            decl_context,
                            info);
                }

                if (chosen_result < 0)
                {
                    valid_option = i;
                }
                else if (chosen_result > 0)
                {
                    // valid_option = valid_option;
                }
                else
                {
                    // Error
                    internal_error("More than one valid intepretation of ambiguity '%s'\nat '%s'\n", 
                            prettyprint_in_buffer(a),
                            ast_location(a));
                }
            }
        }
    }

    // Fallback, the first one chosen wins
    if (valid_option < 0
            && ambiguity_fallback_interpretation != NULL)
    {
        for (i = 0; i < n; i++)
        {
            AST current_interpretation = ast_get_ambiguity(a, i);
            ast_fix_parents_inside_intepretation(current_interpretation);

            if (ambiguity_fallback_interpretation(current_interpretation, decl_context, i, info))
            {
                valid_option = i;
                break;
            }
        }
    }

    if (valid_option < 0)
    {
        // There is not any valid option.
        // We choose the first and diagnose all cases
        valid_option = 0;

        DEBUG_CODE()
        {
            fprintf(stderr, "AMBIGUITY: Could not solve ambiguity '%s' at '%s'\n",
                    prettyprint_in_buffer(a),
                    ast_location(a));
        }

        // Commit everything
        diagnostic_context_t* combine_diagnostics = diagnostic_context_push_buffered();
        for (i = 0; i < n; i++)
        {
            diagnostic_context_commit(ambig_diag[i]);
        }
        diagnostic_context_pop();
        diagnostic_context_commit(combine_diagnostics);
    }
    else
    {
        // Commit the chosen interpretation and discard all others
        for (i = 0; i < n; i++)
        {
            if (i == valid_option)
            {
                diagnostic_context_commit(ambig_diag[i]);
            }
            else
            {
                diagnostic_context_discard(ambig_diag[i]);
            }
        }
    }

    ast_replace_with_ambiguity(a, valid_option);
}

static char try_to_solve_ambiguity_generic(AST a, const decl_context_t* decl_context, void *info,
        ambiguity_check_intepretation_fun_t* ambiguity_check_intepretation,
        ambiguity_choose_interpretation_fun_t* ambiguity_choose_interpretation
        )
{
    ERROR_CONDITION(ASTKind(a) != AST_AMBIGUITY, "Tree is not an ambiguity", 0);

    int valid_option = -1;

    int i, n = ast_get_num_ambiguities(a);
    for (i = 0; i < n; i++)
    {
        AST current_interpretation = ast_get_ambiguity(a, i);
        ast_fix_parents_inside_intepretation(current_interpretation);

        char c = ambiguity_check_intepretation(current_interpretation, decl_context, i, info);

        if (c)
        {
            if (valid_option < 0)
            {
                valid_option = i;
            }
            else
            {
                AST previous_interpretation = ast_get_ambiguity(a, valid_option);
                int chosen_result = 0;

                if (ambiguity_choose_interpretation != NULL)
                {
                    // WARNING: previous_interpretation may have wrong parents
                    // since we cannot fix two trees that may be sharing nodes
                    chosen_result = ambiguity_choose_interpretation(
                            current_interpretation,
                            previous_interpretation,
                            i, valid_option,
                            decl_context,
                            info);
                }

                if (chosen_result < 0)
                {
                    valid_option = i;
                }
                else if (chosen_result > 0)
                {
                    // valid_option = valid_option;
                }
                else
                {
                    return 0;
                }
            }
        }
    }

    if (valid_option < 0)
    {
        return 0;
    }

    ast_replace_with_ambiguity(a, valid_option);
    return 1;
}

static int select_node_type(AST a, node_t type);
static AST recursive_search(AST a, node_t type);
static AST look_for_node_type_within_ambig(AST a, node_t type, int n);

static char check_declaration_statement(AST a, const decl_context_t* decl_context, gather_decl_spec_t* gather_info);
static char check_expression_statement(AST a, const decl_context_t* decl_context);

static char check_typeless_declarator(AST declarator, const decl_context_t* decl_context);

static char check_init_declarator(AST init_declarator, const decl_context_t* decl_context, gather_decl_spec_t* gather_info);

static char check_function_definition_declarator(AST declarator, const decl_context_t* decl_context);

static char check_declarator(AST declarator, const decl_context_t* decl_context);
static char check_function_declarator_parameters(AST parameter_declaration_clause, const decl_context_t* decl_context);

static char check_simple_or_member_declaration(AST a, const decl_context_t* decl_context, gather_decl_spec_t* gather_info);

#define EXPECT_OPTIONS(a, n) \
do \
{ \
    if (ast_get_num_ambiguities(a) != (n)) \
    { \
       internal_error("We expected %d ambiguities but %d found", (n), ast_get_num_ambiguities(a)); \
    } \
} while (0);


// Returns 1 if ASTKind(t1) == n1 && ASTKind(t2) == n2
// Returns -1 if ASTKind(t1) == n2 && ASTKind(t2) == n1
// Returns 0 otherwise
int either_type(AST t1, AST t2, node_t n1, node_t n2)
{
    if ((ASTKind(t1) == n1) 
            && (ASTKind(t2) == n2)) 
        return 1;

    if ((ASTKind(t1) == n2) 
            && (ASTKind(t2) == n1)) 
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
void solve_parameter_declaration_vs_type_parameter_class(AST a, const decl_context_t* decl_context)
{
    EXPECT_OPTIONS(a, 2);

    int k = select_node_type(a, AST_TYPE_PARAMETER_CLASS);

    if (k != -1)
    {
        ast_replace_with_ambiguity(a, k);
    }
    else
    {
        solve_ambiguous_parameter_decl(a, decl_context);
    }
}

static char check_function_header(AST a, const decl_context_t* decl_context,
        int option UNUSED_PARAMETER,
        void* p UNUSED_PARAMETER)
{
    AST declarator = ASTSon1(a);
    return check_declarator(declarator, decl_context);
}

void solve_ambiguous_function_header(AST a, const decl_context_t* decl_context)
{
    solve_ambiguity_generic(a, decl_context, NULL,
            check_function_header,
            NULL,
            NULL);
}

static char solve_ambiguous_declaration_check_interpretation(AST declaration, const decl_context_t* decl_context,
        int option UNUSED_PARAMETER,
        void* p)
{
    char current_valid = 0;
    if (ASTKind(declaration) == AST_FUNCTION_DEFINITION)
    {
        AST function_header = ASTSon0(declaration);
        AST declarator = ASTSon1(function_header);
        current_valid = check_function_definition_declarator(declarator, decl_context);
    }
    else if (ASTKind(declaration) == AST_SIMPLE_DECLARATION
            || ASTKind(declaration) == AST_MEMBER_DECLARATION)
    {
        current_valid = check_simple_or_member_declaration(declaration, decl_context, (gather_decl_spec_t*)p);
    }
    else if (ASTKind(declaration) == AST_MEMBER_DECLARATION_QUALIF)
    {
        AST id_expr = ASTSon0(declaration);
        scope_entry_list_t* entry_list = query_id_expression_flags(decl_context, id_expr, NULL, DF_DEPENDENT_TYPENAME);
        current_valid = (entry_list != NULL);
        entry_list_free(entry_list);
    }
    else
    {
        internal_error("Unexpected ambiguous node '%s'\n", ast_print_node_type(ASTKind(declaration)));
    }

    return current_valid;
}

static char simple_declaration_is_elaborated_class_specifier_and_one_declarator_named_class_virtspec(
        AST a, const decl_context_t* decl_context)
{
    ERROR_CONDITION(ASTKind(a) != AST_SIMPLE_DECLARATION, "Invalid node", 0);
    AST decl_specifier_seq = ASTSon0(a);
    if (decl_specifier_seq == NULL)
        return 0;
    AST type_spec = ASTSon1(decl_specifier_seq);

    if (type_spec == NULL)
        return 0;

    if (ASTKind(type_spec) != AST_ELABORATED_TYPE_CLASS_SPEC)
        return 0;

    AST init_declarator_list = ASTSon1(a);
    if (init_declarator_list == NULL)
        return 0;

    // Not a single item list
    if (ASTSon0(init_declarator_list) != NULL)
        return 0;

    AST init_declarator_first = ASTSon1(init_declarator_list);
    AST declarator = ASTSon0(init_declarator_first);

    AST declarator_name = get_declarator_name(declarator, decl_context);
    if (declarator_name == NULL)
        return 0;

    const char* name = ASTText(declarator_name);
    if (name == NULL)
        return 0;

    if (strcmp(name, "final") != 0)
        return 0;

    return 1;
}

static char simple_declaration_is_class_specifier_without_declarators(AST a)
{
    ERROR_CONDITION(ASTKind(a) != AST_SIMPLE_DECLARATION, "Invalid node", 0);
    AST decl_specifier_seq = ASTSon0(a);
    if (decl_specifier_seq == NULL)
        return 0;
    AST type_spec = ASTSon1(decl_specifier_seq);

    if (type_spec == NULL)
        return 0;

    if (ASTKind(type_spec) != AST_CLASS_SPECIFIER)
        return 0;

    AST init_declarator_list = ASTSon1(a);
    if (init_declarator_list != NULL)
        return 0;

    return 1;
}

static int solve_ambiguous_declaration_choose_interpretation(
        AST current,
        AST previous,
        int current_idx UNUSED_PARAMETER,
        int previous_idx UNUSED_PARAMETER,
        const decl_context_t* decl_context UNUSED_PARAMETER,
        void* info UNUSED_PARAMETER)
{
    if (simple_declaration_is_class_specifier_without_declarators(current)
            && simple_declaration_is_elaborated_class_specifier_and_one_declarator_named_class_virtspec(
                previous, decl_context))
    {
        return -1;
    }
    else if (simple_declaration_is_class_specifier_without_declarators(previous)
            && simple_declaration_is_elaborated_class_specifier_and_one_declarator_named_class_virtspec(
                current, decl_context))
    {
        return 1;
    }
    return 0;
}

void solve_ambiguous_declaration(AST a, const decl_context_t* decl_context)
{
    solve_ambiguity_generic(a, decl_context,
            NULL,
            solve_ambiguous_declaration_check_interpretation,
            solve_ambiguous_declaration_choose_interpretation,
            NULL);
}

static char solve_ambiguous_member_declaration_check_interpretation(
        AST declaration,
        const decl_context_t* decl_context,
        int option,
        void* p)
{
    // solve_ambiguous_declaration_check_interpretation already supports member_declarations
    return solve_ambiguous_declaration_check_interpretation(declaration,
            decl_context,
            option,
            p);
}

static int solve_ambiguous_member_declaration_choose_interpretation(
        AST current,
        AST previous,
        int current_idx  UNUSED_PARAMETER,
        int previous_idx UNUSED_PARAMETER,
        const decl_context_t* decl_context UNUSED_PARAMETER,
        void* info UNUSED_PARAMETER)
{
    if (ASTKind(current) == AST_MEMBER_DECLARATION
            && ASTKind(previous) == AST_MEMBER_DECLARATION)
    {
        AST current_decl_spec = ASTSon0(current);
        AST prev_decl_spec = ASTSon0(previous);

        if (current_decl_spec == NULL
                && prev_decl_spec != NULL)
        {
            return -1;
        }
        else if (current_decl_spec == NULL
                && prev_decl_spec != NULL)
        {
            return 1;
        }
    }

    return 0;
}

void solve_ambiguous_member_declaration(AST a, const decl_context_t* decl_context)
{
    solve_ambiguity_generic(a, decl_context,
            NULL,
            solve_ambiguous_member_declaration_check_interpretation,
            solve_ambiguous_member_declaration_choose_interpretation,
            NULL);
}

// Checks for old-styled functions
static char check_kr_parameter_list(AST parameters_kr, const decl_context_t* decl_context)
{
    if (!IS_C_LANGUAGE)
    {
        internal_error("This function is only for C", 0);
    }

    AST identifier_list = ASTSon0(parameters_kr);
    AST iter;

    char ok = 1;

    for_each_element(identifier_list, iter)
    {
        AST identifier = ASTSon1(iter);

        scope_entry_list_t* entry_list = query_name_str(decl_context, ASTText(identifier), NULL);

        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(entry_list);
                !entry_list_iterator_end(it) && ok;
                entry_list_iterator_next(it))
        {
            scope_entry_t* entry = entry_list_iterator_current(it);
            if (entry->kind == SK_TYPEDEF)
            {
                ok = 0;
            }
        }
        entry_list_iterator_free(it);
        entry_list_free(entry_list);

        if (!ok)
            break;
    }

    return ok;
}

/*
 * Ambiguity within a declarator.
 */
void solve_ambiguous_declarator(AST a, const decl_context_t* decl_context UNUSED_PARAMETER)
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
                ast_replace_with_ambiguity(a, m);
                return;
            }
        }
    }

    internal_error("Don't know how to handle this ambiguity", 0);
}

static char solve_ambiguous_statement_check_interpretation(AST a, const decl_context_t* decl_context,
        int position UNUSED_PARAMETER, void *p UNUSED_PARAMETER)
{
    char current_check = 0;
    switch (ASTKind(a))
    {
        case AST_DECLARATION_STATEMENT :
            {
                current_check = check_declaration_statement(a, decl_context, (gather_decl_spec_t*)p);
                break;
            }
        case AST_EXPRESSION_STATEMENT :
            {
                current_check = check_expression_statement(a, decl_context);
                break;
            }
        case AST_IF_ELSE_STATEMENT:
            {
                /* 
                   Normally the if-else ambiguity is solved in the parser but sometimes it may slip in
                   because of the nature of C

                   if (0)
                   for (;;) // This for is not caught in the grammar
                   if (0)
                   if (0)
                   {
                   }
                   else
                   {
                   }
                   */
                // If this 'if' has an else it is the wrong interpretation
                // because we are 'raising' the else too much
                current_check = (ASTSon2(a) == NULL);
                break;
            }
        default :
            {
                internal_error("Unexpected node '%s'\n", ast_print_node_type(ASTKind(a)));
                break;
            }
    }
    return current_check;
}

static int solve_ambiguous_statement_choose_interpretation(AST current_interpretation, AST previous_interpretation, 
        int current_idx UNUSED_PARAMETER, int previous_idx UNUSED_PARAMETER,
        const decl_context_t* decl_context UNUSED_PARAMETER,
        void * p UNUSED_PARAMETER)
{
    // Prioritize the AST_DECLARATION_STATEMENT
    return either_type(
            current_interpretation,
            previous_interpretation,
            AST_EXPRESSION_STATEMENT,
            AST_DECLARATION_STATEMENT);
}

#if 0
static char solve_ambiguous_statement_fallback(AST a, const decl_context_t* decl_context UNUSED_PARAMETER,
        int position UNUSED_PARAMETER, void *p UNUSED_PARAMETER)
{
    return (ASTKind(a) == AST_EXPRESSION_STATEMENT);
}
#endif

void solve_ambiguous_statement(AST a, const decl_context_t* decl_context)
{
    solve_ambiguity_generic(a, decl_context, NULL,
            solve_ambiguous_statement_check_interpretation, 
            solve_ambiguous_statement_choose_interpretation,
            /* solve_ambiguous_statement_fallback */ NULL);
}

static char check_simple_type_spec(AST type_spec, 
        const decl_context_t* decl_context, 
        type_t** computed_type,
        char allow_class_templates)
{
    if (computed_type != NULL)
    {
        *computed_type = NULL;
    }

    AST type_id_expr = ASTSon0(type_spec);

    scope_entry_list_t* entry_list = query_id_expression(decl_context, type_id_expr, NULL);

    if (entry_list == NULL)
    {
        return 0;
    }

    scope_entry_list_iterator_t* it = NULL;

    char ok = 1;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it) && ok;
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);
        entry = entry_advance_aliases(entry);
        if (entry->kind != SK_TYPEDEF
                && entry->kind != SK_ENUM
                && entry->kind != SK_CLASS
                // We allow this because templates are like types
                && entry->kind != SK_TEMPLATE
                && entry->kind != SK_TEMPLATE_ALIAS
                && entry->kind != SK_TEMPLATE_TYPE_PARAMETER
                && entry->kind != SK_TEMPLATE_TEMPLATE_PARAMETER
                && entry->kind != SK_TEMPLATE_TYPE_PARAMETER_PACK
                && entry->kind != SK_TEMPLATE_TEMPLATE_PARAMETER_PACK)
        {
            ok = 0;
        }
        if (entry->kind == SK_TEMPLATE)
        {
            // Check that the template-name is actually a template class name
            // and not a template function name
            type_t* primary = template_type_get_primary_type(entry->type_information);
            if (!is_named_class_type(primary))
            {
                ok = 0;
            }
        }
        if (!allow_class_templates
                && ASTKind(type_id_expr) == AST_SYMBOL
                && (entry->kind == SK_TEMPLATE
                    || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                    || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK))
        {
            ok = 0;
        }
    }
    entry_list_iterator_free(it);

    scope_entry_t* entry = entry_advance_aliases(entry_list_head(entry_list));
    entry_list_free(entry_list);

    if (ok && computed_type != NULL)
    {
        *computed_type = get_user_defined_type(entry);
    }

    return ok;
}

static char check_type_specifier_aux(AST type_id, const decl_context_t* decl_context, char allow_class_templates);

static char solve_ambiguity_type_specifier_check_interpretation(AST a, const decl_context_t* decl_context,
        int option UNUSED_PARAMETER,
        void *p)
{
    char *allow_class_templates = (char*)p;
    return check_type_specifier_aux(a, decl_context, *allow_class_templates);
}

static char check_type_specifier_aux(AST type_id, const decl_context_t* decl_context, char allow_class_templates)
{
    C_LANGUAGE()
    {
        if (type_id == NULL)
            return 1;
    }
    CXX_LANGUAGE()
    {
        ERROR_CONDITION(type_id == NULL,
                "type-id cannot be null", 0);
    }

    switch (ASTKind(type_id))
    {
        case AST_SIMPLE_TYPE_SPEC :
            return check_simple_type_spec(type_id, decl_context, /* computed_type = */ NULL, allow_class_templates);
            break;
        case AST_CLASS_SPECIFIER :
        case AST_ENUM_SPECIFIER :
        case AST_ELABORATED_TYPENAME_SPEC :
        case AST_ELABORATED_TYPE_ENUM_SPEC :
        case AST_ELABORATED_TYPE_CLASS_SPEC :
        case AST_CHAR_TYPE :
        case AST_WCHAR_TYPE :
        case AST_BOOL_TYPE :
        case AST_INT_TYPE :
        case AST_SHORT_TYPE :
        case AST_LONG_TYPE :
        case AST_SIGNED_TYPE :
        case AST_UNSIGNED_TYPE :
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
                nodecl_t nodecl_dummy = nodecl_null();
                char result = check_expression_non_executable(ASTSon0(type_id), decl_context, &nodecl_dummy);
                nodecl_free(nodecl_dummy);
                return result;
            }
        case AST_GCC_TYPEOF :
            {
                return check_type_id_tree(ASTSon0(type_id), decl_context);
            }
            // There is an ambiguity between AST_GCC_TYPEOF_EXPR and AST_GCC_TYPEOF
        case AST_AMBIGUITY :
            {
                return try_to_solve_ambiguity_generic(
                        type_id, decl_context, &allow_class_templates,
                        solve_ambiguity_type_specifier_check_interpretation,
                        NULL);
                break;
            }
        default :
            {
                internal_error("Unexpected node '%s'\n", ast_print_node_type(ASTKind(type_id)));
            }
    }
}

static char check_type_specifier(AST type_id, const decl_context_t* decl_context)
{
    return check_type_specifier_aux(type_id, decl_context, /* allow_class_templates */ 0);
}

static char check_type_specifier_or_class_template_name(AST type_id, const decl_context_t* decl_context)
{
    return check_type_specifier_aux(type_id, decl_context, /* allow_class_templates */ 1);
}

static char try_to_solve_ambiguous_init_declarator(AST a, const decl_context_t* decl_context, gather_decl_spec_t* gather_info);

static char solve_typeless_init_declarator_check_interpretation(AST opt_declarator, const decl_context_t* decl_context,
        int option UNUSED_PARAMETER,
        void* p UNUSED_PARAMETER)
{
    AST declarator = ASTSon0(opt_declarator);

    return check_typeless_declarator(declarator, decl_context);
}

static char check_simple_or_member_declaration(AST a, const decl_context_t* decl_context, gather_decl_spec_t* gather_info)
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

        // Check that this type specifier is really a type specifier
        AST type_spec = ASTSon1(decl_specifier_seq);
        if (type_spec != NULL)
        {
            if (!check_type_specifier(type_spec, decl_context))
            {
                return 0;
            }
        }

        // A typedef declaration without a type specifier should be an error
        AST decl_specifier_seq_list = ASTSon0(decl_specifier_seq);
        if (type_spec == NULL
                && decl_specifier_seq_list != NULL)
        {
            AST iter;
            for_each_element(decl_specifier_seq_list, iter)
            {
                AST spec = ASTSon1(iter);
                if (ASTKind(spec) == AST_TYPEDEF_SPEC)
                {
                    return 0;
                }
            }
        }

        AST first_init_declarator = NULL;
        AST declarator_list = ASTSon1(a);
        AST declarator_iter;

        if (declarator_list != NULL)
        {
            for_each_element(declarator_list, declarator_iter)
            {
                first_init_declarator = ASTSon1(declarator_iter);
                break;
            }
        }


        AST first_declarator = NULL;
        if (first_init_declarator != NULL)
        {
            if (ASTKind(first_init_declarator) == AST_AMBIGUITY)
            {
                if (!try_to_solve_ambiguous_init_declarator(first_init_declarator, decl_context, gather_info))
                    return 0;
            }
            else if (ASTKind(first_init_declarator) == AST_BITFIELD_DECLARATOR)
            {
                // This ambiguity brought to you by C++11
                // A bit-field shall have integral or enumeration type. This
                // check is done to avoid the following ambiguity:
                //
                // struct A {};
                //
                // struct B
                // {
                //      struct C : A {};
                //      // Obviously, It is a member declaration of a nested
                //      // struct C  which inherit from A.
                //
                //      int x : int {1};
                //      // It is a bit-field member declaration
                // };

                type_t* bitfield_type = NULL;
                gather_decl_spec_t dummy_gather_info;
                memset(&dummy_gather_info, 0, sizeof(dummy_gather_info));

                nodecl_t dummy_nodecl_output = nodecl_null();
                build_scope_decl_specifier_seq(
                        decl_specifier_seq,
                        &dummy_gather_info,
                        &bitfield_type,
                        decl_context,
                        &dummy_nodecl_output);

                if (!(is_enum_type(bitfield_type)
                            || is_integral_type(bitfield_type)))
                    return 0;
            }

            first_declarator = ASTSon0(first_init_declarator);
        }

        if (first_declarator != NULL)
        {
            // Make sure this declarator is fine at this point
            // This will avoid problems like
            //
            // typedef T(F)(S) where the intepretation with an empty type-specifier is not allowed
            if (!check_declarator(first_declarator, decl_context))
                return 0;
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

        if (first_declarator != NULL
                && type_spec != NULL)
        {
            // This ambiguity brought to you by C++11
            // struct X :   T { };
            // enum E : class { };
            if (ASTKind(first_init_declarator) == AST_BITFIELD_DECLARATOR
                    && (ASTKind(type_spec) == AST_ELABORATED_TYPE_CLASS_SPEC
                        || ASTKind(type_spec) == AST_ELABORATED_TYPE_ENUM_SPEC))
            {
                return 0;
            }

            AST parenthesized_declarator;
            AST inner_declarator;
            AST declarator_id_expression;
            // T is just a parenthesized declarator_id_expr
            if (first_declarator != NULL
                    && ASTKind(first_declarator) == AST_DECLARATOR
                    && (parenthesized_declarator = ASTSon0(first_declarator)) != NULL
                    && ASTKind(parenthesized_declarator) == AST_PARENTHESIZED_DECLARATOR
                    && (inner_declarator = ASTSon0(parenthesized_declarator)) != NULL
                    && ASTKind(inner_declarator) == AST_DECLARATOR
                    && (declarator_id_expression = ASTSon0(inner_declarator)) != NULL
                    && ASTKind(declarator_id_expression) == AST_DECLARATOR_ID_EXPR)
            {
                AST id_expression = ASTSon0(declarator_id_expression);
                scope_entry_list_t* entry_list = query_id_expression(decl_context, id_expression, NULL);

                // T names a type
                if (entry_list != NULL)
                {
                    scope_entry_t* entry = entry_list_head(entry_list);
                    entry_list_free(entry_list);
                    entry = entry_advance_aliases(entry);

                    if (entry->kind == SK_TYPEDEF
                            || entry->kind == SK_ENUM
                            || entry->kind == SK_CLASS
                            || entry->kind == SK_TEMPLATE_TYPE_PARAMETER)
                    {
                        // A is a simple type specifier
                        if (ASTKind(type_spec) == AST_SIMPLE_TYPE_SPEC)
                        {
                            AST type_id_expr = ASTSon0(type_spec);

                            scope_entry_list_t* type_id_list = query_id_expression(decl_context, type_id_expr, NULL);

                            if (type_id_list != NULL)
                            {
                                scope_entry_t* type_sym = entry_list_head(type_id_list);
                                entry_list_free(type_id_list);

                                // A is of class nature
                                // The related scope of A is the same as the
                                // current scope
                                if (type_sym->kind == SK_CLASS
                                        && symbol_entity_specs_get_is_injected_class_name(type_sym))
                                {
                                    // In this case, and only in this case, this is
                                    // not a data member declaration
                                    return 0;
                                }
                            }
                        }
                    }
                }
                // Something is wrong here
                if (entry_list == NULL
                        && ASTKind(id_expression) == AST_QUALIFIED_ID)
                {
                    return 0;
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

            if (ASTKind(init_declarator) == AST_AMBIGUITY)
            {
                if (!try_to_solve_ambiguity_generic(init_declarator, decl_context, NULL,
                            solve_typeless_init_declarator_check_interpretation, NULL))
                    return 0;
            }
            else
            {
                if (!check_typeless_declarator(declarator, decl_context))
                {
                    return 0;
                }
            }
        }
    }

    return 1;
}

static char solve_ambiguous_declaration_statement_check_interpretation(AST a, const decl_context_t* decl_context,
        int option UNUSED_PARAMETER, void *p UNUSED_PARAMETER)
{
    switch (ASTKind(a))
    {
        case AST_SIMPLE_DECLARATION:
            {
                return check_simple_or_member_declaration(a, decl_context, (gather_decl_spec_t*)p);
            }
        default:
            internal_error("Unexpected ambiguity '%s'\n", ast_print_node_type(ASTKind(a)));
    }

    return 0;
}

static char check_declaration_statement(AST declaration_statement,
        const decl_context_t* decl_context,
        gather_decl_spec_t* gather_info)
{
    AST a = ASTSon0(declaration_statement);

    // In general only AST_SIMPLE_DECLARATION gets ambiguous here
    if (ASTKind(a) == AST_SIMPLE_DECLARATION)
    {
        return check_simple_or_member_declaration(a, decl_context, gather_info);
    }
    else if (ASTKind(a) == AST_AMBIGUITY)
    {
        return try_to_solve_ambiguity_generic(
                a, decl_context, gather_info,
                solve_ambiguous_declaration_statement_check_interpretation,
                NULL);
    }

    return 1;
}

static char check_typeless_declarator_rec(AST declarator, const decl_context_t* decl_context, int nfuncs)
{
    switch (ASTKind(declarator))
    {
        case AST_PARENTHESIZED_EXPRESSION :
        case AST_DECLARATOR :
            {
                return check_typeless_declarator_rec(ASTSon0(declarator), 
                        decl_context, nfuncs);
                break;
            }
        case AST_POINTER_DECLARATOR :
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
                return check_typeless_declarator_rec(ASTSon0(declarator), decl_context, nfuncs+1);
                break;
            }
        case AST_DECLARATOR_ID_EXPR :
            {
                // Do nothing
                // will continue below
                break;
            }
        case AST_AMBIGUITY:
            {
                solve_ambiguous_declarator(declarator, decl_context);
                return check_typeless_declarator_rec(declarator, decl_context, nfuncs);
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

    switch (ASTKind(id_expression))
    {
        case AST_QUALIFIED_ID :
            {
                AST global_scope = ASTSon0(id_expression);
                AST nested_name_spec = ASTSon1(id_expression);
                AST symbol = ASTSon2(id_expression);

                // These always have type
                if (ASTKind(symbol) == AST_OPERATOR_FUNCTION_ID
                        || ASTKind(symbol) == AST_TEMPLATE_ID
                        || ASTKind(symbol) == AST_OPERATOR_FUNCTION_ID_TEMPLATE)
                {
                    return 0;
                }
                
                scope_entry_list_t* result_list = query_nested_name(decl_context, 
                        global_scope, nested_name_spec, symbol, NULL);

                enum cxx_symbol_kind filter_classes[] = {
                    SK_CLASS, 
                };

                scope_entry_list_t* classes_list = filter_symbol_kind_set(result_list, STATIC_ARRAY_LENGTH(filter_classes), filter_classes);
                entry_list_free(result_list);

                if (classes_list == NULL)
                {
                    // This is not a class name
                    return 0;
                }

                entry_list_free(classes_list);

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
                if (decl_context->current_scope->kind != CLASS_SCOPE)
                {
                    return 0;
                }

                if (ASTKind(id_expression) == AST_DESTRUCTOR_ID ||
                        ASTKind(id_expression) == AST_DESTRUCTOR_TEMPLATE_ID)
                {
                    // Jump '~'
                    class_name++;
                }

                // Now look for the class symbol in the enclosing scope
                //
                //   class A {
                //      A();  <-- valid
                //      ~A(); <-- valid
                //   };
                //
                scope_entry_list_t* result = query_in_scope_str(decl_context, class_name, NULL);

                if (result == NULL)
                    return 0;

                if (entry_list_head(result)->kind != SK_CLASS)
                {
                    scope_entry_t* entry = entry_list_head(result);
                    entry_list_free(result);

                    if (entry->kind != SK_CLASS)
                    {
                        // This is not a class name
                        return 0;
                    }
                }

                // It looks sane here
                return 1;
                break;
            }
        case AST_CONVERSION_FUNCTION_ID :
            // That's fine only at class-scope
            return decl_context->current_scope->kind == CLASS_SCOPE;
        default :
            // Do nothing for any other things
            break;
    }

    return 0;
}

static char check_typeless_declarator(AST declarator, const decl_context_t* decl_context)
{
    return check_typeless_declarator_rec(declarator, decl_context, 0);
}

static char check_expression_statement(AST a, const decl_context_t* decl_context)
{
    AST expression = ASTSon0(a);

    nodecl_t nodecl_expr = nodecl_null();
    char result = check_expression(expression, decl_context, &nodecl_expr);
    nodecl_free(nodecl_expr);

    return result;
}

char solve_ambiguous_list_of_expressions(AST ambiguous_list, const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    ERROR_CONDITION(ASTKind(ambiguous_list) != AST_AMBIGUITY, "invalid kind", 0);

    int i;
    int correct_choice = -1;
    int n = ast_get_num_ambiguities(ambiguous_list);

    diagnostic_context_t* ambig_diag[n + 1];

    for (i = 0; i < n; i++)
    {
        AST current_expression_list = ast_get_ambiguity(ambiguous_list, i);
        ast_fix_parents_inside_intepretation(current_expression_list);

        nodecl_t nodecl_expr = nodecl_null();
        ambig_diag[i] = diagnostic_context_push_buffered();
        check_list_of_expressions(current_expression_list, decl_context, &nodecl_expr);
        diagnostic_context_pop();

        if (nodecl_is_null(nodecl_expr)
                || !nodecl_is_err_expr(nodecl_expr))
        {
            if (correct_choice < 0)
            {
                correct_choice = i;
                if (nodecl_output != NULL)
                    *nodecl_output = nodecl_expr;
            }
            else
            {
                AST previous_choice = ast_get_ambiguity(ambiguous_list, correct_choice);
                AST current_choice = ast_get_ambiguity(ambiguous_list, i);
                internal_error("More than one valid alternative '%s' vs '%s'",
                        ast_print_node_type(ASTKind(previous_choice)),
                        ast_print_node_type(ASTKind(current_choice)));
            }
        }
    }

    if (correct_choice < 0)
    {
        // Combine all diagnostics and commit
        diagnostic_context_t* combine_diagnostics = diagnostic_context_push_buffered();
        for (i = 0; i < n; i++)
        {
            diagnostic_context_commit(ambig_diag[i]);
        }
        diagnostic_context_pop();
        diagnostic_context_commit(combine_diagnostics);

        if (nodecl_output != NULL)
            *nodecl_output = nodecl_make_err_expr(ast_get_locus(ambiguous_list));
        return 0;
    }
    else
    {
        for (i = 0; i < n; i++)
        {
            if (i == correct_choice)
            {
                diagnostic_context_commit(ambig_diag[i]);
            }
            else
            {
                diagnostic_context_discard(ambig_diag[i]);
            }
        }

        ast_replace_with_ambiguity(ambiguous_list, correct_choice);
        return 1;
    }
}

char solve_ambiguous_list_of_initializer_clauses(AST ambiguous_list, const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    ERROR_CONDITION(ASTKind(ambiguous_list) != AST_AMBIGUITY, "invalid kind", 0);

    int i;
    int correct_choice = -1;
    int n = ast_get_num_ambiguities(ambiguous_list);

    diagnostic_context_t* ambig_diag[n + 1];

    for (i = 0; i < n; i++)
    {
        AST current_expression_list = ast_get_ambiguity(ambiguous_list, i);
        ast_fix_parents_inside_intepretation(current_expression_list);

        ambig_diag[i] = diagnostic_context_push_buffered();
        nodecl_t nodecl_expr = nodecl_null();
        check_list_of_initializer_clauses(current_expression_list, decl_context, &nodecl_expr);
        diagnostic_context_pop();

        if (nodecl_is_null(nodecl_expr)
                || !nodecl_is_err_expr(nodecl_expr))
        {
            if (correct_choice < 0)
            {
                correct_choice = i;
                if (nodecl_output != NULL)
                    *nodecl_output = nodecl_expr;
            }
            else
            {
                AST previous_choice = ast_get_ambiguity(ambiguous_list, correct_choice);
                AST current_choice = ast_get_ambiguity(ambiguous_list, i);
                internal_error("More than one valid alternative '%s' vs '%s'",
                        ast_print_node_type(ASTKind(previous_choice)),
                        ast_print_node_type(ASTKind(current_choice)));
            }
        }
    }

    if (correct_choice < 0)
    {
        // Combine all diagnostics and commit
        diagnostic_context_t* combine_diagnostics = diagnostic_context_push_buffered();
        for (i = 0; i < n; i++)
        {
            diagnostic_context_commit(ambig_diag[i]);
        }
        diagnostic_context_pop();
        diagnostic_context_commit(combine_diagnostics);

        if (nodecl_output != NULL)
            *nodecl_output = nodecl_make_err_expr(ast_get_locus(ambiguous_list));
        return 0;
    }
    else
    {
        for (i = 0; i < n; i++)
        {
            if (i == correct_choice)
            {
                diagnostic_context_commit(ambig_diag[i]);
            }
            else
            {
                diagnostic_context_discard(ambig_diag[i]);
            }
        }

        ast_replace_with_ambiguity(ambiguous_list, correct_choice);
        return 1;
    }
}

static char solve_ambiguous_nested_part_check_interpretation(AST a, const decl_context_t* decl_context,
        int position UNUSED_PARAMETER, void* info UNUSED_PARAMETER)
{
    ERROR_CONDITION(ASTKind(a) != AST_NODE_LIST, "invalid kind\n", 0);
    nodecl_t nodecl_nested_part;
    compute_nodecl_name_from_nested_part(a, decl_context, &nodecl_nested_part);
    return !nodecl_is_err_expr(nodecl_nested_part);
}

void solve_ambiguous_nested_part(AST a, const decl_context_t* decl_context)
{
    solve_ambiguity_generic(a, decl_context, /* info */ NULL,
            solve_ambiguous_nested_part_check_interpretation,
            /* choose_interpretation */ NULL,
            /* fallback */ NULL);
}

static char solve_ambiguous_init_declarator_check_interpretation(AST a,
        const decl_context_t* decl_context,
        int option UNUSED_PARAMETER,
        void* info)
{
    return check_init_declarator(a, decl_context, (gather_decl_spec_t*)info);
}

static int solve_ambiguous_init_declarator_choose_interpretation(
        AST current_interpretation,
        AST previous_interpretation,
        int current_idx UNUSED_PARAMETER,
        int previous_idx UNUSED_PARAMETER,
        const decl_context_t* decl_context UNUSED_PARAMETER,
        void *p UNUSED_PARAMETER)
{
    AST previous_choice_declarator = ASTSon0(previous_interpretation);
    AST current_choice_declarator = ASTSon0(current_interpretation);

    // Favor the AST_DECLARATOR_ID_EXPR
    return either_type(
            ASTSon0(previous_choice_declarator),
            ASTSon0(current_choice_declarator),
            AST_DECLARATOR_FUNC,
            AST_DECLARATOR_ID_EXPR);
}

void solve_ambiguous_init_declarator(AST a, const decl_context_t* decl_context,
        gather_decl_spec_t *gather_info)
{
    solve_ambiguity_generic(a, decl_context, /* info */ gather_info,
        solve_ambiguous_init_declarator_check_interpretation,
        solve_ambiguous_init_declarator_choose_interpretation,
        /* fallback */ NULL);
}

// Like solve_ambiguous_init_declarator but does not fail
static char try_to_solve_ambiguous_init_declarator(AST a, const decl_context_t* decl_context, gather_decl_spec_t* gather_info)
{
    int correct_choice = -1;
    int i;

    int n = ast_get_num_ambiguities(a);

    diagnostic_context_t* ambig_diag[n + 1];

    for (i = 0; i < n; i++)
    {
        AST init_declarator = ast_get_ambiguity(a, i);
        ast_fix_parents_inside_intepretation(init_declarator);

        ambig_diag[i] = diagnostic_context_push_buffered();
        char c = check_init_declarator(init_declarator, decl_context, gather_info);
        diagnostic_context_pop();

        if (c)
        {
            if (correct_choice < 0)
            {
                correct_choice = i;
            }
            else
            {
                // Ambiguity: T t(Q()); where T and Q are type-names always solves to 
                // function declaration

                // WARNING: previous choice may share nodes with init_declarator
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
        // Combine all diagnostics and commit
        diagnostic_context_t* combine_diagnostics = diagnostic_context_push_buffered();
        for (i = 0; i < n; i++)
        {
            diagnostic_context_commit(ambig_diag[i]);
        }
        diagnostic_context_pop();
        diagnostic_context_commit(combine_diagnostics);

        return 0;
    }
    else
    {
        for (i = 0; i < n; i++)
        {
            if (i == correct_choice)
            {
                diagnostic_context_commit(ambig_diag[i]);
            }
            else
            {
                diagnostic_context_discard(ambig_diag[i]);
            }
        }
        ast_replace_with_ambiguity(a, correct_choice);
        return 1;
    }
}

static char check_init_declarator(AST init_declarator,
        const decl_context_t* decl_context,
        gather_decl_spec_t* gather_info)
{
    AST declarator = ASTSon0(init_declarator);
    AST initializer = ASTSon1(init_declarator);

    if (!check_declarator(declarator, decl_context))
        return 0;

    char result = 1;

    if (initializer != NULL)
    {
        const decl_context_t* initializer_context = decl_context;

        AST declarator_name = get_declarator_name(declarator, decl_context);

        if (declarator_name != NULL
                && ASTKind(declarator_name) == AST_QUALIFIED_ID
                && (gather_info == NULL
                    || !gather_info->is_friend))
        {
            AST global_op = ASTSon0(declarator_name);
            AST nested_name = ASTSon1(declarator_name);
            AST name = ASTSon2(declarator_name);

            decl_flags_t decl_flags = DF_NONE;

            if (BITMAP_TEST(decl_context->decl_flags, DF_CONSTRUCTOR))
            {
                decl_flags |= DF_CONSTRUCTOR;
            }

            scope_entry_list_t* symbols = query_nested_name_flags(decl_context,
                    global_op, nested_name, name, NULL, decl_flags);

            if (symbols == NULL)
            {
                return 0;
            }

            initializer_context = entry_list_head(symbols)->decl_context;
            entry_list_free(symbols);
        }

        // This code is similar to 'check_initialization' in cxx-exprtype.c but
        // here types are not used
        //
        // Ambiguous cases are '= e' and '(e1, e2, .., e3)'
        switch (ASTKind(initializer))
        {
            // Plain expression
            default:
                {
                    nodecl_t nodecl_dummy = nodecl_null();
                    result = check_expression(initializer, initializer_context, &nodecl_dummy);
                    nodecl_free(nodecl_dummy);
                    break;
                }
            case AST_PARENTHESIZED_INITIALIZER:
                {
                    // '(e1, e2, .., eN)'
                    AST initializer_list = ASTSon0(initializer);

                    nodecl_t nodecl_dummy = nodecl_null();
                    result = check_list_of_expressions(initializer_list, initializer_context, &nodecl_dummy);
                    nodecl_free(nodecl_dummy);
                    break;
                }
        }
    }

    return result;
}

static char check_declarator_rec(AST declarator, const decl_context_t* decl_context, char enclosing_is_array, char enclosing_is_function)
{
    if (declarator == NULL)
        return 1;

    switch (ASTKind(declarator))
    {
        case AST_DECLARATOR_ARRAY :
            {
                if (ASTSon1(declarator) != NULL)
                {
                    nodecl_t nodecl_dummy = nodecl_null();
                    char result = check_expression(ASTSon1(declarator), decl_context, &nodecl_dummy);
                    nodecl_free(nodecl_dummy);

                    if (!result)
                    {
                        return 0;
                    }
                }
                return check_declarator_rec(ASTSon0(declarator), decl_context, 1, 0);
                return 1;
            }
        case AST_PARENTHESIZED_DECLARATOR :
        case AST_DECLARATOR :
            {
                return check_declarator_rec(ASTSon0(declarator), decl_context, enclosing_is_array, enclosing_is_function);
                break;
            }
        case AST_POINTER_DECLARATOR :
            {
                return check_declarator_rec(ASTSon1(declarator), decl_context, 0, 0);
                break;
            }
        case AST_DECLARATOR_FUNC :
            {
                if (enclosing_is_function || enclosing_is_array)
                    return 0;

                // Check for parameters here
                AST parameter_declaration_clause = ASTSon0(ASTSon1(declarator));
                if (parameter_declaration_clause != NULL)
                {
                    if (!check_function_declarator_parameters(parameter_declaration_clause, decl_context))
                    {
                        return 0;
                    }
                }
                return check_declarator_rec(ASTSon0(declarator), decl_context, 0, 1);
                break;
            }
        case AST_DECLARATOR_ID_EXPR :
        case AST_DECLARATOR_ID_PACK :
            {
                // Is this already correct or we have to check something else ?
                return 1;
                break;
            }
        case AST_AMBIGUITY:
            {
                solve_ambiguous_declarator(declarator, decl_context);
                return check_declarator_rec(declarator, decl_context, enclosing_is_array, enclosing_is_function);
                break;
            }
        default :
            {
                internal_error("Unexpected node type '%s'\n", ast_print_node_type(ASTKind(declarator)));
                break;
            }
    }

    return 0;
}

static char check_declarator(AST declarator, const decl_context_t* decl_context)
{
    return check_declarator_rec(declarator, decl_context, /* enclosing_is_array */ 0, /* enclosing_is_function */ 0);
}

static char is_abstract_declarator(AST a, const decl_context_t* decl_context)
{
    return get_declarator_id_expression(a, decl_context) == NULL;
}

static char is_non_abstract_declarator(AST a, const decl_context_t* decl_context)
{
    return !is_abstract_declarator(a, decl_context);
}

static char solve_ambiguous_function_declarator_parameter_check_intepretation(AST a, const decl_context_t* decl_context,
        int option UNUSED_PARAMETER, void* p UNUSED_PARAMETER)
{
    AST decl_specifier_seq = ASTSon0(a);
    AST type_specifier = ASTSon1(decl_specifier_seq);
    AST declarator = ASTSon1(a);

    char seems_ok = 1;

    seems_ok = seems_ok && check_type_specifier(type_specifier, decl_context);

    if (seems_ok && declarator != NULL)
    {
        seems_ok = seems_ok && check_declarator(declarator, decl_context);
    }

    return seems_ok;
}

static int solve_ambiguous_function_declarator_parameter_choose_intepretation(
        AST current_interpretation, 
        AST previous_interpretation,
        int current_idx UNUSED_PARAMETER,
        int previous_idx UNUSED_PARAMETER,
        const decl_context_t* decl_context, void* p UNUSED_PARAMETER)
{
    if (ASTSon1(current_interpretation) != NULL
            && is_abstract_declarator(ASTSon1(current_interpretation), decl_context)
            &&  ASTSon1(previous_interpretation) != NULL
            && is_non_abstract_declarator(ASTSon1(previous_interpretation), decl_context))
    {
        // The current is the good one
        return -1;
    }
    else if (ASTSon1(previous_interpretation) != NULL
            && is_abstract_declarator(ASTSon1(previous_interpretation), decl_context)
            &&  ASTSon1(current_interpretation) != NULL
            && is_non_abstract_declarator(ASTSon1(current_interpretation), decl_context))
    {
        // The previous was the good one
        return 1;
    }
    else
    {
        return 0;
        // fprintf(stderr, "Previous interpretation\n");
        // prettyprint(stderr, previous_interpretation);
        // fprintf(stderr, "\n");
        // fprintf(stderr, "Current interpretation\n");
        // prettyprint(stderr, current_interpretation);
        // fprintf(stderr, "\n");
        // internal_error("More than one valid alternative '%s' vs '%s' %s", 
        //         ast_print_node_type(ASTKind(previous_interpretation)),
        //         ast_print_node_type(ASTKind(current_interpretation)),
        //         ast_location(previous_interpretation));
    }
}

static char check_function_declarator_parameters(AST parameter_declaration_clause, const decl_context_t* decl_context)
{
    AST list = parameter_declaration_clause;
    AST iter;

    if (ASTKind(parameter_declaration_clause) == AST_AMBIGUITY)
    {
        solve_ambiguous_parameter_clause(parameter_declaration_clause, decl_context);
    }

    if (ASTKind(parameter_declaration_clause) == AST_EMPTY_PARAMETER_DECLARATION_CLAUSE)
    {
        return 1;
    }

    for_each_element(list, iter)
    {
        AST parameter = ASTSon1(iter);

        if (ASTKind(parameter) == AST_VARIADIC_ARG)
        {
            continue;
        }

        if (ASTKind(parameter) == AST_AMBIGUITY)
        {
            if (!try_to_solve_ambiguity_generic(parameter, decl_context, NULL,
                    solve_ambiguous_function_declarator_parameter_check_intepretation,
                    solve_ambiguous_function_declarator_parameter_choose_intepretation))
                return 0;
        }

        if (ASTKind(parameter) != AST_PARAMETER_DECL)
        {
            internal_error("Unexpected node '%s'\n", ast_print_node_type(ASTKind(parameter)));
        }

        AST decl_specifier_seq = ASTSon0(parameter);
        AST abstract_declarator = ASTSon1(parameter);

        AST type_specifier = ASTSon1(decl_specifier_seq);

        if (!check_type_specifier(type_specifier, decl_context))
        {
            return 0;
        }

        if (abstract_declarator != NULL)
        {
            if (!check_declarator(abstract_declarator, decl_context))
            {
                return 0;
            }
        }

        AST default_arg = ASTSon2(parameter);

        if (default_arg != NULL)
        {
            nodecl_t nodecl_dummy = nodecl_null();
            check_expression(default_arg, decl_context, &nodecl_dummy);
            nodecl_free(nodecl_dummy);
        }
    }

    return 1;
}

static char solve_ambiguous_parameter_declaration_check_interpretation(AST parameter_decl, 
        const decl_context_t* decl_context, 
        int position UNUSED_PARAMETER,
        void* p UNUSED_PARAMETER)
{
    char current_valid = 1;

    AST decl_specifier_seq = ASTSon0(parameter_decl);

    AST type_specifier = ASTSon1(decl_specifier_seq);

    if (type_specifier != NULL)
    {
        current_valid = current_valid && check_type_specifier(type_specifier, decl_context);
    }
    else
    {
        // There must be type_spec in a parameter_decl
        // but it might be a stranded 'long/short/signed/unsigned' 
        // because we have not yet called build_scope_decl_specifier_seq
        gather_decl_spec_t gather_info;
        memset(&gather_info, 0, sizeof(gather_info));
        type_t* t = NULL;

        nodecl_t dummy_nodecl_output = nodecl_null();
        build_scope_decl_specifier_seq(decl_specifier_seq,
                &gather_info, &t, decl_context, &dummy_nodecl_output);

        current_valid = current_valid && (t != NULL);
    }

    AST declarator = ASTSon1(parameter_decl);

    if (declarator != NULL)
    {
        current_valid = current_valid && check_declarator(declarator, decl_context);
    }

    return current_valid;
}

static int solve_ambiguous_parameter_declaration_choose_interpretation(
        AST current_interpretation,
        AST previous_interpretation, 
        int current_idx UNUSED_PARAMETER,
        int previous_idx UNUSED_PARAMETER,
        const decl_context_t* decl_context, void *p UNUSED_PARAMETER)
{
    AST previous_parameter_decl = previous_interpretation;
    AST current_parameter_decl = current_interpretation;

    AST previous_decl_speq_seq = ASTSon0(previous_parameter_decl);
    AST current_decl_speq_seq = ASTSon0(current_parameter_decl);

    AST previous_type_spec = NULL;
    AST current_type_spec = NULL;
    if (previous_decl_speq_seq != NULL)
    {
        previous_type_spec = ASTSon1(previous_decl_speq_seq);

        // Ignore any implicit int here
        if (previous_type_spec != NULL
                && ASTKind(previous_type_spec) == AST_IMPLICIT_INT_TYPE)
            previous_type_spec = NULL;
    }
    if (current_decl_speq_seq != NULL)
    {
        current_type_spec = ASTSon1(current_decl_speq_seq);

        // Ignore any implicit int here
        if (current_type_spec != NULL
                && ASTKind(current_type_spec) == AST_IMPLICIT_INT_TYPE)
            current_type_spec = NULL;
    }

    AST previous_declarator = ASTSon1(previous_parameter_decl);
    AST current_declarator = ASTSon1(current_parameter_decl);

    // If an abstract declarator is possible, then it must be an abstract declarator
    if (previous_declarator != NULL
            && current_declarator != NULL)
    {
        if (is_abstract_declarator(previous_declarator, decl_context)
                && is_non_abstract_declarator(current_declarator, decl_context))
        {
            return 1;
        }
        else if (is_non_abstract_declarator(previous_declarator, decl_context)
                && is_abstract_declarator(current_declarator, decl_context))
        {
            return -1;
        }
    }
    // If one interpretation has type and the other does not, then it must have type
    else if (previous_type_spec != current_type_spec)
    {
        if (previous_type_spec != NULL)
        {
            return 1;
        }
        else
        {
            return -1;
        }
    }

    return 0;
}

void solve_ambiguous_parameter_decl(AST parameter_declaration, const decl_context_t* decl_context)
{
    solve_ambiguity_generic(parameter_declaration, decl_context, NULL,
            solve_ambiguous_parameter_declaration_check_interpretation,
            solve_ambiguous_parameter_declaration_choose_interpretation, 
            NULL);
}

static char solve_ambiguous_for_init_statement_check_interpretation(AST for_init_statement, const decl_context_t* decl_context,
        int option UNUSED_PARAMETER, void *p UNUSED_PARAMETER)
{
    char current = 0;
    switch (ASTKind(for_init_statement))
    {
        case AST_SIMPLE_DECLARATION :
            if (check_simple_or_member_declaration(for_init_statement, decl_context, (gather_decl_spec_t*)p))
            {
                current = 1;
            }
            break;
        case AST_EXPRESSION_STATEMENT :
            {
                nodecl_t nodecl_dummy = nodecl_null();
                if (check_expression(ASTSon0(for_init_statement), decl_context, &nodecl_dummy))
                {
                    current = 1;
                }
                nodecl_free(nodecl_dummy);
            }
            break;
        default :
            internal_error("Unknown node '%s' at '%s'\n", 
                    ast_print_node_type(ASTKind(for_init_statement)),
                    ast_location(for_init_statement));
    }

    return current;
}

#if 0
static char solve_ambiguous_for_init_statement_fallback(AST a, const decl_context_t* decl_context,
        int position, void *p)
{
    return solve_ambiguous_statement_fallback(a, decl_context, position, p);
}
#endif

void solve_ambiguous_for_init_statement(AST a, const decl_context_t* decl_context)
{
    solve_ambiguity_generic(a, decl_context, NULL,
            solve_ambiguous_for_init_statement_check_interpretation,
            NULL,
            /* solve_ambiguous_for_init_statement_fallback */ NULL);
}

static char solve_ambiguous_type_specifier_check_interpretation(AST type_specifier, const decl_context_t* decl_context,
        int position UNUSED_PARAMETER, void* p UNUSED_PARAMETER)
{
    char current_typeof = 0;
    AST typeof_argument = ASTSon0(type_specifier);
    if (ASTKind(type_specifier) == AST_GCC_TYPEOF)
    {
        current_typeof = check_type_id_tree(typeof_argument, decl_context);
    }
    else if (ASTKind(type_specifier) == AST_GCC_TYPEOF_EXPR)
    {
        nodecl_t nodecl_dummy = nodecl_null();
        current_typeof = check_expression_non_executable(typeof_argument, decl_context, &nodecl_dummy);
        nodecl_free(nodecl_dummy);
    }
    else
    {
        internal_error("Unexpected node type %s\n", ast_print_node_type(ASTKind(type_specifier)));
    }
    return current_typeof;
}

void solve_ambiguous_type_specifier(AST ambig_type, const decl_context_t* decl_context)
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

        is_typeof_ambiguity = ((ASTKind(type_specifier) == AST_GCC_TYPEOF)
                || (ASTKind(type_specifier) == AST_GCC_TYPEOF_EXPR));
    }

    if (!is_typeof_ambiguity)
    {
        internal_error("Unknown ambiguity at '%s'!\n", 
                ast_location(ambig_type));
    }

    solve_ambiguity_generic(ambig_type, decl_context, NULL, 
            solve_ambiguous_type_specifier_check_interpretation,
            NULL,
            NULL);
}

/*
 * Auxiliar functions
 */

// Returns the index of the first node of type "type"
static int select_node_type(AST a, node_t type)
{
    int i;

    for (i = 0; i < ast_get_num_ambiguities(a); i++)
    {
        if (ASTKind(ast_get_ambiguity(a, i)) == type)
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

    if (ASTKind(a) == type)
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

void solve_ambiguous_exception_decl(AST exception_decl, const decl_context_t* decl_context)
{
    // They share the same layout
    solve_ambiguous_parameter_decl(exception_decl, decl_context);
}


char check_type_id_tree(AST type_id, const decl_context_t* decl_context)
{
    AST type_specifier_seq = ASTSon0(type_id);
    AST abstract_declarator = ASTSon1(type_id);
    
    // This is never NULL
    AST type_specifier = ASTSon1(type_specifier_seq);

    return check_type_specifier(type_specifier, decl_context)
        && ((abstract_declarator == NULL)
                || (check_declarator(abstract_declarator, decl_context)));
}

char check_type_id_tree_or_class_template_name(AST type_id, const decl_context_t* decl_context)
{
    AST type_specifier_seq = ASTSon0(type_id);
    AST abstract_declarator = ASTSon1(type_id);
    
    // This is never NULL
    AST type_specifier = ASTSon1(type_specifier_seq);

    return check_type_specifier_or_class_template_name(type_specifier, decl_context)
        && ((abstract_declarator == NULL)
                || (check_declarator(abstract_declarator, decl_context)));
}

struct nodecl_expr_ambiguities_tag
{
    int chosen;
    nodecl_t* nodecls;
};

static char solve_ambiguous_expression_check_intepretation(AST ambig_expression, const decl_context_t* decl_context,
        int position, void* p)
{
    struct nodecl_expr_ambiguities_tag* data = (struct nodecl_expr_ambiguities_tag*)p;

    char current_check = check_expression(ambig_expression, decl_context, &(data->nodecls[position]));

    if (current_check)
    {
        data->chosen = position;
    }

    return current_check;
}

static int solve_ambiguous_expression_choose_interpretation(
        AST current_choice, 
        AST previous_choice, 
        int current_idx,
        int previous_idx,
        const decl_context_t* decl_context UNUSED_PARAMETER, 
        void* p)
{
    struct nodecl_expr_ambiguities_tag* data = (struct nodecl_expr_ambiguities_tag*)p;
    // How to read this checks
    //
    //  either_type(a, b, T1, T2) 
    //     will return  1 if a == T1 and b == T2
    //     will return -1 if a == T2 and b == T1 
    //     will return  0 otherwise
    //
    //  So if 
    //
    //     either_type(previous_choice, current_choice, A, B)
    //
    //  returns -1 it means that the previous choice is a B and the
    //  current_choice is an A. If it returns 1 it means that the
    //  previous_choice is an A and current_choice is a B
    //
    int either;
    if ((either = either_type(current_choice, previous_choice, 
                    AST_FUNCTION_CALL, AST_EXPLICIT_TYPE_CONVERSION)))
    {
        // This one covers cases like this one
        //
        // template <typename _T>
        // void f(_T *t)
        // {
        //    _T::f(t);
        // }
        //
        // here '_T::f' must be a function call and not an explicit type
        // conversion. If you meant an explicit type conversion '_T::f'
        // must be seen as a type, so 'typename' is mandatory
        //
        // template <typename _T>
        // void f(_T *t)
        // {
        //    typename _T::f(t);
        // }
        //
        // But this last case is not ambiguous at the expression level so it will
        // never go through this desambiguation code
        if (either > 0)
        {
            if (data != NULL)
            {
                data->chosen = current_idx;
            }
            return -1;
        }
        else
        {
            if (data != NULL)
            {
                data->chosen = previous_idx;
            }
            return 1;
        }
    }
    else if ((either = either_type(current_choice, previous_choice,
                    AST_GREATER_THAN, AST_FUNCTION_CALL)))
    {
        // This one covers cases like this one
        //
        // template <int N>
        // void f(int c)
        // {
        //    a.b<N  >  (c);
        // }
        //
        // Must always be interpreted as AST_GREATER_THAN rather than a AST_FUNCTION_CALL
        // (for the later the right syntax is "a.template b<N>(c)")
        if (either > 0)
        {
            if (data != NULL)
            {
                data->chosen = current_idx;
            }
            return -1;
        }
        else
        {
            if (data != NULL)
            {
                data->chosen = previous_idx;
            }
            return 1;
        }
    }
    else
    {
        return 0;
    }
}

#if 0
static char solve_ambiguous_expression_fallback(AST current_interpretation,
        const decl_context_t* decl_context UNUSED_PARAMETER, int position, void *p)
{
    struct nodecl_expr_ambiguities_tag* data = (struct nodecl_expr_ambiguities_tag*)p;
    // Prioritize function calls
    if (ASTKind(current_interpretation) == AST_FUNCTION_CALL)
    {
        data->chosen = position;
        return 1;
    }
    return 0;
}
#endif

void solve_ambiguous_expression(AST ambig_expression, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    int n = ast_get_num_ambiguities(ambig_expression);

    nodecl_t nodecl_local_array[n + 1];

    struct nodecl_expr_ambiguities_tag nodecl_expr_ambiguities;
    nodecl_expr_ambiguities.chosen = 0;
    nodecl_expr_ambiguities.nodecls = nodecl_local_array;

    solve_ambiguity_generic(ambig_expression, decl_context, &nodecl_expr_ambiguities,
            solve_ambiguous_expression_check_intepretation,
            solve_ambiguous_expression_choose_interpretation,
            /* solve_ambiguous_expression_fallback */ NULL);

    int i;
    for (i = 0; i < n; i++)
    {
        if (i != nodecl_expr_ambiguities.chosen)
        {
            nodecl_free(nodecl_expr_ambiguities.nodecls[i]);
        }
    }

    *nodecl_output = nodecl_expr_ambiguities.nodecls[nodecl_expr_ambiguities.chosen];
}

static char check_function_definition_declarator(AST declarator, const decl_context_t* decl_context)
{
    return check_declarator(declarator, decl_context);
}

static AST get_expression_of_condition(AST current_condition)
{
    if (ASTSon0(current_condition) == NULL) // Expression
    {
        return ASTSon2(current_condition);
    }
    else
    {
        AST equal_initializer = ASTSon2(current_condition);
        return ASTSon0(equal_initializer);
    }
}

static char solve_ambiguous_condition_interpretation(AST current_condition, const decl_context_t* decl_context,
        int position UNUSED_PARAMETER, void *p UNUSED_PARAMETER)
{
    char current_check = 0;
    AST current_expression;
    nodecl_t current_nodecl = nodecl_null();

    if (ASTSon0(current_condition) == NULL) // Expression
    {
        current_expression = ASTSon2(current_condition);
        current_check = check_expression(current_expression, decl_context, &current_nodecl);
    }
    else
    {
        // Like a declaration
        // type_specifier_seq declarator '=' assignment_expr
        AST type_specifier_seq = ASTSon0(current_condition);
        AST declarator = ASTSon1(current_condition);
        AST equal_initializer = ASTSon2(current_condition);

        AST type_specifier = ASTSon1(type_specifier_seq);

        current_check = check_type_specifier(type_specifier, decl_context)
            && check_declarator(declarator, decl_context);

        current_expression = ASTSon0(equal_initializer);
        current_check = current_check && check_expression(current_expression, decl_context, &current_nodecl);
    }

    return current_check;
}

static int solve_ambiguous_condition_choose_interpretation(AST current_condition,
        AST previous_condition,
        int current_idx,
        int previous_idx,
        const decl_context_t* decl_context,
        void *p UNUSED_PARAMETER)
{
    return solve_ambiguous_expression_choose_interpretation(
            get_expression_of_condition(current_condition),
            get_expression_of_condition(previous_condition),
            current_idx,
            previous_idx,
            decl_context, NULL);
}

void solve_ambiguous_condition(AST a, const decl_context_t* decl_context)
{
    solve_ambiguity_generic(a, decl_context, NULL,
                solve_ambiguous_condition_interpretation,
                solve_ambiguous_condition_choose_interpretation,
                NULL);
}

// Look for a template parameter pack
static char contains_template_parameter_pack(AST a, const decl_context_t* decl_context)
{
    if (a == NULL)
        return 0;

    if (ASTKind(a) == AST_SYMBOL)
    {
        scope_entry_list_t* entry_list = query_name_str(decl_context, ASTText(a), NULL);
        if (entry_list != NULL)
        {
            scope_entry_t* entry = entry_list_head(entry_list);

            entry_list_free(entry_list);

            if (entry->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK
                || entry->kind == SK_TEMPLATE_NONTYPE_PARAMETER_PACK
                || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK)
                return 1;
        }
    }
    else if (ASTKind(a) == AST_AMBIGUITY)
    {
        int i, n = ast_get_num_ambiguities(a);
        for (i = 0; i < n; i++)
        {
            AST current_interpretation = ast_get_ambiguity(a, i);
            if (contains_template_parameter_pack(current_interpretation, decl_context))
                return 1;
        }
    }
    else if (ASTKind(a) == AST_DECLARATOR_ID_PACK
            || ASTKind(a) == AST_INITIALIZER_CLAUSE_PACK_EXPANSION)
    {
        // Stop here as these start a new expansion
        return 0;
    }
    else
    {
        int i;
        for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
        {
            if (contains_template_parameter_pack(ast_get_child(a, i), decl_context))
                return 1;
        }
    }

    return 0;
}

static char solve_ambiguous_parameter_clause_check_interpretation(
        AST parameter_clause UNUSED_PARAMETER,
        const decl_context_t* decl_context UNUSED_PARAMETER,
        int position UNUSED_PARAMETER,
        void *info UNUSED_PARAMETER)
{
    char result = 0;
    C_LANGUAGE()
    {
        // K&R parameter lists
        if (ASTKind(parameter_clause) == AST_KR_PARAMETER_LIST)
        {
            return check_kr_parameter_list(parameter_clause, decl_context);
        }
        else
        {
            return check_function_declarator_parameters(parameter_clause, decl_context);
        }
    }

    CXX_LANGUAGE()
    {
        ERROR_CONDITION(ASTKind(parameter_clause) != AST_NODE_LIST, "Invalid node", 0);

        AST last = ASTSon1(parameter_clause);

        if (ASTKind(last) == AST_VARIADIC_ARG)
        {
            // void f(T...); where T is NOT a parameter pack
            ERROR_CONDITION(ASTSon0(parameter_clause) == NULL, "Invalid tree", 0);
            AST before_last = ASTSon1(ASTSon0(parameter_clause));
            ERROR_CONDITION(before_last == NULL, "Invalid tree", 0);

            result = !contains_template_parameter_pack(before_last, decl_context);
        }
        else if (ASTKind(last) == AST_PARAMETER_DECL)
        {
            // void f(T...); where T is a parameter pack
            result = contains_template_parameter_pack(last, decl_context);
        }
        else
        {
            internal_error("Invalid node %s", ast_print_node_type(ASTKind(last)));
        }
    }

    return result;
}

void solve_ambiguous_parameter_clause(AST parameter_clause, const decl_context_t* decl_context)
{
    // Ambiguity at this level arises in C++ because of this
    //
    // void f(int x, T...)
    //
    // We do not know if T... is a parameter-pack or a T abstract-declarator
    // followed by an ellipsis (int x, T, ...)
    //
    // In C99 it also may be caused by KR-identifier lists
    solve_ambiguity_generic(
            parameter_clause,
            decl_context, NULL,
            solve_ambiguous_parameter_clause_check_interpretation,
            NULL,
            NULL);
}

static char solve_ambiguous_decl_specifier_check_intepretation(
        AST decl_specifier,
        const decl_context_t* decl_context UNUSED_PARAMETER,
        int position UNUSED_PARAMETER,
        void *info UNUSED_PARAMETER)
{
    if (ASTKind(decl_specifier) == AST_ALIGNAS_TYPE)
    {
        AST type_id = ASTSon0(decl_specifier);
        return check_type_id_tree(type_id, decl_context);
    }
    else if (ASTKind(decl_specifier) == AST_ALIGNAS)
    {
        AST expr = ASTSon0(decl_specifier);
        nodecl_t nodecl_dummy = nodecl_null();
        char result = check_expression_non_executable_must_be_constant(expr, decl_context, &nodecl_dummy);
        nodecl_free(nodecl_dummy);
        return result;
    }
    else
    {
        internal_error("Invalid node %s", ast_print_node_type(ASTKind(decl_specifier)));
    }

}

void solve_ambiguous_decl_specifier(AST decl_spec, const decl_context_t* decl_context)
{
    /* Ambiguity at this level only involves alignas(X) where X can be an expression or a type-id */
    solve_ambiguity_generic(
            decl_spec,
            decl_context, NULL,
            solve_ambiguous_decl_specifier_check_intepretation,
            NULL,
            NULL);
}

AST find_ambiguity(AST a)
{
    if (a == NULL)
    {
        return NULL;
    }
    else if (ASTKind(a) == AST_AMBIGUITY)
    {
        return a;
    }
    else if (ASTKind(a) == AST_NODE_LIST)
    {
        AST iter;
        for_each_element(a, iter)
        {
            AST result = find_ambiguity(ASTSon1(iter));
            if (result != NULL)
                return result;
        }
    }
    else
    {
        int i;
        for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
        {
            AST result = find_ambiguity(ast_get_child(a, i));
            if (result != NULL)
                return result;
        }
    }

    return NULL;
}
