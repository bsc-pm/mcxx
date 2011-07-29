/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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



#include <string.h>
#include <stdio.h>
#include <signal.h>
#include "extstruct.h"
#include "cxx-driver.h"
#include "cxx-buildscope.h"
#include "cxx-scope.h"
#include "cxx-prettyprint.h"
#include "cxx-typeutils.h"
#include "cxx-typeorder.h"
#include "cxx-utils.h"
#include "cxx-cexpr.h"
#include "cxx-exprtype.h"
#include "cxx-ambiguity.h"
#include "cxx-printscope.h"
#include "cxx-solvetemplate.h"
#include "cxx-instantiation.h"
#include "cxx-tltype.h"
#include "cxx-scopelink.h"
#include "cxx-attrnames.h"
#include "cxx-gccsupport.h"
#include "cxx-gccbuiltins.h"
#include "cxx-gccspubuiltins.h"
#include "cxx-nodecl-output.h"
#include "cxx-overload.h"
#include "cxx-upc.h"
#include "cxx-cuda.h"
#include "cxx-entrylist.h"
#include "cxx-lexer.h"
#include "cxx-parser.h"
#include "c99-parser.h"
#include "cxx-limits.h"
#include "cxx-diagnostic.h"
#include "cxx-pragma.h"

/*
 * This file builds symbol table. If ambiguous nodes are found disambiguating
 * routines will be called prior to filling symbolic inormation. Note that
 * disambiguating routines will use the currently built symbol table.
 *
 * Note that some "semantic checks" performed here are intended only to verify
 * that lookup and symbol registration are performed correctly. By no means
 * this is a full type checking phase
 */

static void build_scope_declaration(AST a, decl_context_t decl_context, 
        nodecl_t* output, scope_entry_list_t** declared_symbols);
static void build_scope_declaration_sequence(AST a, 
        decl_context_t decl_context, nodecl_t* nodecl_output);
static void build_scope_simple_declaration(AST a, decl_context_t decl_context, 
        char is_template,
        char is_explicit_instantiation,
        nodecl_t* nodecl_output, 
        scope_entry_list_t** declared_symbols);

static void build_scope_namespace_alias(AST a, decl_context_t decl_context);
static void build_scope_namespace_definition(AST a, decl_context_t decl_context, nodecl_t* nodecl_output);
static void build_scope_declarator_with_parameter_context(AST a, 
        gather_decl_spec_t* gather_info, type_t* simple_type_info, type_t** declarator_type,
        decl_context_t decl_context, decl_context_t *prototype_context,
        nodecl_t* nodecl_output);

static void build_scope_member_specification(decl_context_t inner_decl_context, AST member_specification_tree, 
        access_specifier_t default_current_access, type_t* type_info, nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols);

static void build_scope_member_declaration(decl_context_t inner_decl_context,
        AST a, access_specifier_t current_access, type_t* class_info,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols);
static scope_entry_t* build_scope_member_function_definition(decl_context_t decl_context,
        AST a, access_specifier_t current_access, type_t* class_info,
        char is_template,
        char is_explicit_instantiation,
        nodecl_t* nodecl_output);
static void build_scope_default_or_delete_member_function_definition(decl_context_t decl_context, 
        AST a, access_specifier_t current_access, type_t* class_info,
        nodecl_t* nodecl_output);
static void build_scope_member_simple_declaration(decl_context_t decl_context, AST a, 
        access_specifier_t current_access, type_t* class_info, 
        char is_template,
        char is_explicit_instantiation,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols);

static void gather_type_spec_from_simple_type_specifier(AST a, type_t** type_info,
        decl_context_t decl_context);
static void gather_type_spec_from_enum_specifier(AST a, type_t** type_info, 
        gather_decl_spec_t* gather_info,
        decl_context_t decl_context);
static void gather_type_spec_from_class_specifier(AST a, type_t** type_info,
        gather_decl_spec_t* gather_info,
        decl_context_t decl_context,
        nodecl_t* nodecl_output);
static void gather_type_spec_from_dependent_typename(AST a, type_t** simple_type_info,
        decl_context_t decl_context);

static void gather_type_spec_from_elaborated_class_specifier(AST a, 
        type_t** type_info,
        gather_decl_spec_t *gather_info,
        decl_context_t decl_context);
static void gather_type_spec_from_elaborated_enum_specifier(AST a, 
        type_t** type_info, 
        gather_decl_spec_t* gather_info, 
        decl_context_t decl_context);

static void gather_gcc_attributes_spread(AST a, gather_decl_spec_t* gather_info, 
        decl_context_t declarator_context);
static void build_scope_declarator_rec(
        AST a, type_t** declarator_type, 
        gather_decl_spec_t* gather_info,
        decl_context_t declarator_context,
        decl_context_t entity_context,
        decl_context_t *prototype_context,
        nodecl_t* nodecl_output);

static scope_entry_t* build_scope_declarator_name(AST declarator, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, decl_context_t decl_context,
        nodecl_t* nodecl_output);
static scope_entry_t* build_scope_declarator_id_expr(AST declarator_name, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, decl_context_t decl_context,
        nodecl_t* nodecl_output);

static void build_scope_linkage_specifier(AST a, decl_context_t decl_context, nodecl_t* nodecl_output);
static void build_scope_linkage_specifier_declaration(AST a, 
        AST top_linkage_decl, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols);

static void build_scope_template_declaration(AST a, 
        AST top_template_decl, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output);
static void build_scope_explicit_template_specialization(AST a, decl_context_t decl_context, nodecl_t* nodecl_output);

static void build_scope_statement_(AST a, decl_context_t decl_context, nodecl_t* nodecl_output);
static void build_scope_statement_seq(AST a, decl_context_t decl_context, nodecl_t* nodecl_output);

static void build_scope_template_parameter_list(AST a, 
        template_parameter_list_t* template_parameters,
        int nesting,
        decl_context_t template_context,
        nodecl_t* nodecl_output);
static void build_scope_template_parameter(AST a, 
        template_parameter_list_t* template_parameter_list, 
        int nesting,
        decl_context_t template_context,
        nodecl_t* nodecl_output);
static void build_scope_nontype_template_parameter(AST a,
        template_parameter_list_t* template_parameter_list,
        int nesting,
        decl_context_t decl_context,
        nodecl_t* nodecl_output);
static void build_scope_type_template_parameter(AST a,
        template_parameter_list_t* template_parameter_list,
        int nesting,
        decl_context_t decl_context,
        nodecl_t* nodecl_output);
static void build_scope_template_template_parameter(AST a,
        template_parameter_list_t* template_parameter_list,
        int nesting,
        decl_context_t decl_context,
        nodecl_t* nodecl_output);

static void build_scope_member_template_declaration(decl_context_t decl_context, AST a, 
        access_specifier_t current_access, type_t* class_info,
        char is_explicit_instantiation,
        nodecl_t* nodecl_output);
static void build_scope_member_template_function_definition(decl_context_t decl_context,
        AST a, access_specifier_t current_access, type_t* class_info,
        char is_explicit_instantiation,
        nodecl_t* nodecl_output);
        
static void build_scope_member_template_simple_declaration(decl_context_t decl_context, AST a,
        access_specifier_t current_access, type_t* class_info,
        char is_explicit_instantiation,
        nodecl_t* nodecl_output);

static void build_scope_static_assert(AST a, decl_context_t decl_context);

static void build_scope_defaulted_function_definition(AST a, decl_context_t decl_context, nodecl_t* nodecl_output);
static void build_scope_deleted_function_definition(AST a, decl_context_t decl_context, nodecl_t* nodecl_output);

static void build_scope_using_directive(AST a, decl_context_t decl_context, nodecl_t* nodecl_output);
static void build_scope_using_declaration(AST a, decl_context_t decl_context, access_specifier_t, nodecl_t* nodecl_output);

static void build_scope_member_declaration_qualified(AST a, decl_context_t decl_context, access_specifier_t, nodecl_t* nodecl_output);

static void build_scope_template_function_definition(AST a, decl_context_t decl_context, 
        char is_explicit_instantiation, nodecl_t* nodecl_output);

static void build_scope_explicit_instantiation(AST a, decl_context_t decl_context, nodecl_t* nodecl_output);

static scope_entry_t* register_new_typedef_name(AST declarator_id, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, decl_context_t decl_context);
static scope_entry_t* register_new_variable_name(AST declarator_id, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, decl_context_t decl_context);
static scope_entry_t* register_function(AST declarator_id, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, decl_context_t decl_context);

static void build_scope_template_simple_declaration(AST a, decl_context_t template_context, 
        char is_explicit_instantiation, nodecl_t* nodecl_output);

static void build_scope_gcc_asm_definition(AST a, decl_context_t decl_context, nodecl_t* nodecl_output);

static cv_qualifier_t compute_cv_qualifier(AST a);

static void build_exception_spec(type_t* function_type, AST a, gather_decl_spec_t *gather_info, decl_context_t decl_context, 
        nodecl_t* nodecl_output);

static char is_constructor_declarator(AST a);

static scope_entry_t* find_function_declaration(AST declarator_id, 
        type_t* declarator_type, 
        gather_decl_spec_t* gather_info,
        decl_context_t decl_context);

static AST get_enclosing_declaration(AST point_of_declarator);

static void build_scope_pragma_custom_directive(AST a, decl_context_t decl_context, 
        char* _dummy, 
        nodecl_t* nodecl_output);
static void build_scope_pragma_custom_construct_declaration(AST a, 
        decl_context_t decl_context, 
        char* attr_name,
        nodecl_t* nodecl_output);
static void build_scope_pragma_custom_construct_member_declaration(AST a, 
        decl_context_t decl_context, 
        access_specifier_t current_access,
        type_t* class_info,
        nodecl_t* nodecl_output);

static void call_destructors_of_classes(decl_context_t block_context, 
        nodecl_t* nodecl_output);

// Current linkage, by default C++
static const char* current_linkage = "\"C++\"";

static void initialize_builtin_symbols(decl_context_t decl_context);


static void gather_decl_spec_information(AST a, 
        gather_decl_spec_t* gather_info, decl_context_t decl_context);

static unsigned long long _bytes_used_buildscope = 0;

unsigned long long int buildscope_used_memory(void)
{
    return _bytes_used_buildscope;
}

void initialize_translation_unit_scope(translation_unit_t* translation_unit, decl_context_t* decl_context)
{
    *decl_context = new_global_context();

    // The global scope is created here
    translation_unit->global_decl_context = *decl_context;
    translation_unit->scope_link = scope_link_new(*decl_context);

    // Link the AST root node with the global scope
    AST a = translation_unit->parsed_tree;
    scope_link_set(translation_unit->scope_link, a, *decl_context);
}

void c_initialize_translation_unit_scope(translation_unit_t* translation_unit)
{
    decl_context_t decl_context;
    initialize_translation_unit_scope(translation_unit, &decl_context);
    initialize_builtin_symbols(decl_context);
}

// Builds scope for the translation unit
void build_scope_translation_unit(translation_unit_t* translation_unit)
{
    AST a = translation_unit->parsed_tree;
    decl_context_t decl_context 
        = scope_link_get_global_decl_context(translation_unit->scope_link);

    nodecl_t nodecl = nodecl_null();

    CXX_LANGUAGE()
    {
        instantiation_init();
    }

    AST list = ASTSon0(a);
    if (list != NULL)
    {
        // Refactor this and "build_scope_translation_unit_tree_with_global_scope" one day
        build_scope_declaration_sequence(list, decl_context, &nodecl);
    }

    CXX_LANGUAGE()
    {
        nodecl_t instantiated_units = instantiation_get_instantiated_functions();
        nodecl = nodecl_concat_lists(nodecl, instantiated_units);
    }

    translation_unit->nodecl = nodecl_make_top_level(nodecl, ASTFileName(a), ASTLine(a));
}

void build_scope_translation_unit_tree_with_global_scope(AST tree, 
        scope_link_t* scope_link UNUSED_PARAMETER, 
        decl_context_t decl_context)
{
    if (ASTType(tree) != AST_TRANSLATION_UNIT)
    {
        internal_error("This function expects a translation unit tree but '%s'", 
                ast_print_node_type(ASTType(tree)));
    }

    nodecl_t nodecl_output = nodecl_null();

    CXX_LANGUAGE()
    {
        instantiation_init();
    }

    AST list = ASTSon0(tree);
    if (list != NULL)
    {
        // The scope will have been already populated with basic things
        build_scope_declaration_sequence(list, decl_context, &nodecl_output);
    }

    CXX_LANGUAGE()
    {
        nodecl_t instantiated_units = instantiation_get_instantiated_functions();
        nodecl_output = nodecl_concat_lists(nodecl_output, instantiated_units);
    }
}

static default_argument_info_t** empty_default_argument_info(int num_parameters)
{
    return counted_calloc(sizeof(default_argument_info_t*), num_parameters, &_bytes_used_buildscope);
}

// This function initialize global symbols that exist in every translation unit
// prior to its translation
static void initialize_builtin_symbols(decl_context_t decl_context)
{
    // __builtin_va_list is a very special type in GCC
    scope_entry_t* builtin_va_list;

    builtin_va_list = new_symbol(decl_context, decl_context.global_scope, "__builtin_va_list");
    builtin_va_list->kind = SK_TYPEDEF;
    builtin_va_list->defined = 1;
    builtin_va_list->type_information = get_gcc_builtin_va_list_type();
    builtin_va_list->do_not_print = 1;
    builtin_va_list->file = "(global scope)";
    builtin_va_list->entity_specs.is_builtin = 1;

    CXX_LANGUAGE()
    {
        {
            // __null is a magic NULL in g++
            scope_entry_t* null_keyword;

            null_keyword = new_symbol(decl_context, decl_context.global_scope, "__null");
            null_keyword->kind = SK_VARIABLE;
            null_keyword->type_information = get_null_type();
            const_value_t* val = const_value_get_signed_int(0);
            null_keyword->language_dependent_value = const_value_to_tree(val);
            null_keyword->value = const_value_to_nodecl(val);
            null_keyword->defined = 1;
            null_keyword->do_not_print = 1;
            null_keyword->file = "(global scope)";
            // This should be renamed one day into 'builtin_symbol'
            null_keyword->entity_specs.is_builtin = 1;
        }

        // There are two 'operator new' and two 'operator delete' at global scope
        {
            scope_entry_t* global_operator_new;
            global_operator_new = new_symbol(decl_context, decl_context.global_scope, "operator new");
            global_operator_new->kind = SK_FUNCTION;
            global_operator_new->do_not_print = 1;

            type_t* return_type = get_pointer_type(get_void_type());

            parameter_info_t parameter_info[1] = { 
                { .is_ellipsis = 0, .type_info = get_size_t_type() } 
            };
            
            global_operator_new->type_information = get_new_function_type(return_type, parameter_info, 1);
            global_operator_new->entity_specs.num_parameters = 1;
            global_operator_new->entity_specs.default_argument_info 
                = empty_default_argument_info( /* num_parameters */ 1);

            global_operator_new->file = "(global scope)";
        }
        // Version for arrays
        {
            scope_entry_t* global_operator_new;
            global_operator_new = new_symbol(decl_context, decl_context.global_scope, "operator new[]");
            global_operator_new->kind = SK_FUNCTION;
            global_operator_new->do_not_print = 1;

            type_t* return_type = get_pointer_type(get_void_type());

            parameter_info_t parameter_info[1] = { 
                { .is_ellipsis = 0, .type_info = get_size_t_type() } 
            };
            
            global_operator_new->type_information = get_new_function_type(return_type, parameter_info, 1);
            global_operator_new->entity_specs.num_parameters = 1;
            global_operator_new->entity_specs.default_argument_info 
                = empty_default_argument_info(/* num_parameters */ 1);

            global_operator_new->file = "(global scope)";
        }

        {
            scope_entry_t* global_operator_delete;
            global_operator_delete = new_symbol(decl_context, decl_context.global_scope, "operator delete");
            global_operator_delete->kind = SK_FUNCTION;
            global_operator_delete->do_not_print = 1;

            type_t* return_type = get_void_type();

            parameter_info_t parameter_info[1] = { 
                { .is_ellipsis = 0, .type_info = get_pointer_type(get_void_type()) } 
            };
            
            global_operator_delete->type_information = get_new_function_type(return_type, parameter_info, 1);
            global_operator_delete->entity_specs.num_parameters = 1;
            global_operator_delete->entity_specs.default_argument_info
                = empty_default_argument_info(/* num_parameters */ 1);

            global_operator_delete->file = "(global scope)";
        }
        {
            scope_entry_t* global_operator_delete;
            global_operator_delete = new_symbol(decl_context, decl_context.global_scope, "operator delete[]");
            global_operator_delete->kind = SK_FUNCTION;
            global_operator_delete->do_not_print = 1;

            type_t* return_type = get_void_type();

            parameter_info_t parameter_info[1] = { 
                { .is_ellipsis = 0, .type_info = get_pointer_type(get_void_type()) } 
            };
            
            global_operator_delete->type_information = get_new_function_type(return_type, parameter_info, 1);
            global_operator_delete->entity_specs.num_parameters = 1;
            global_operator_delete->entity_specs.default_argument_info
                = empty_default_argument_info(/* num_parameters */ 1);

            global_operator_delete->file = "(global scope)";
        }
    }

    gcc_sign_in_builtins(decl_context);

    C_LANGUAGE()
    {
        // This is reserved for C only
        gcc_sign_in_spu_builtins(decl_context);
    }

    C_LANGUAGE()
    {
        if (CURRENT_CONFIGURATION->enable_upc)
        {
            upc_sign_in_builtins(decl_context);
        }
    }

    if (CURRENT_CONFIGURATION->enable_cuda)
    {
        init_cuda_builtins(decl_context);
    }
}

static void build_scope_declaration_sequence(AST list, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output_list)
{
    AST iter;
    for_each_element(list, iter)
    {
        nodecl_t current_nodecl_output_list = nodecl_null();
        build_scope_declaration(ASTSon1(iter), decl_context, &current_nodecl_output_list, /* declared_symbols */ NULL);

        *nodecl_output_list = nodecl_concat_lists(*nodecl_output_list, current_nodecl_output_list);
    }
}

// Build scope for a declaration
static void build_scope_declaration(AST a, decl_context_t decl_context, 
        nodecl_t* nodecl_output, scope_entry_list_t** declared_symbols)
{
    // NOTE: if nodecl_output is not nodecl_null it should return a list
    DEBUG_CODE()
    {
        fprintf(stderr, "==== Declaration line [%s] ====\n", ast_location(a));
    }

    switch (ASTType(a))
    {
        case AST_SIMPLE_DECLARATION :
            {
                // Simple declarations are of the form.
                //
                //   int a;
                //   class A { ... } [a];
                //   struct C { ... } [c];
                //   enum E { ... } [e];
                //   int f(int [k]);
                //
                // [thing] means that thing is optional
                build_scope_simple_declaration(a, decl_context, 
                        /* is_template */ 0,
                        /* is_explicit_instantiation */ 0,
                        nodecl_output, declared_symbols);
                break;
            }
        case AST_NAMESPACE_DEFINITION :
            {
                // Namespace definitions are of the form
                //   namespace [name]
                //   {
                //      ...
                //   }
                build_scope_namespace_definition(a, decl_context, nodecl_output);
                break;
            }
        case AST_NAMESPACE_ALIAS :
            {
                build_scope_namespace_alias(a, decl_context);
                break;
            }
        case AST_FUNCTION_DEFINITION :
            {
                // A function definition is of the form
                //   [T] f(T1 [t1], T2 [t2], T3 [t3])
                //   {
                //     ...
                //   }
                build_scope_function_definition(a, /* previous_symbol */ NULL, decl_context, 
                        /* is_template */ 0, /* is_explicit_instantiation */ 0, nodecl_output);
                break;
            }
        case AST_DELETED_FUNCTION_DEFINITION :
            {
                build_scope_deleted_function_definition(a, decl_context, nodecl_output);
                break;
            }
        case AST_DEFAULTED_FUNCTION_DEFINITION :
            {
                build_scope_defaulted_function_definition(a, decl_context, nodecl_output);
                break;
            }
        case AST_LINKAGE_SPEC :
            {
                // extern "C" { ... }
                build_scope_linkage_specifier(a, decl_context, nodecl_output);
                break;
            }
        case AST_LINKAGE_SPEC_DECL :
            {
                // extern "C" int a;
                build_scope_linkage_specifier_declaration(a, a, decl_context, nodecl_output, declared_symbols);
                break;
            }
        case AST_EXPORT_TEMPLATE_DECLARATION :
        case AST_TEMPLATE_DECLARATION :
            {
                // [export] template<typename _T> struct A;
                // [export] template<typename _T> struct A { };
                // [export] template<typename _T> void f(_T t);
                build_scope_template_declaration(a, a, decl_context, nodecl_output);
                break;
            }
        case AST_EXPLICIT_INSTANTIATION :
            {
                // template A<int>;
                build_scope_explicit_instantiation(a, decl_context, nodecl_output);
                break;
            }
        case AST_EXPLICIT_SPECIALIZATION :
            {
                // template<> struct A<int> { };
                build_scope_explicit_template_specialization(a, decl_context, nodecl_output);
                break;
            }
        case AST_USING_NAMESPACE_DIRECTIVE:
            {
                // using namespace std;
                build_scope_using_directive(a, decl_context, nodecl_output);
                break;
            }
        case AST_USING_DECLARATION :
        case AST_USING_DECLARATION_TYPENAME :
            {
                // using A::b;
                build_scope_using_declaration(a, decl_context, AS_UNKNOWN, nodecl_output);
                break;
            }
        case AST_STATIC_ASSERT:
            {
                build_scope_static_assert(a, decl_context);
                break;
            }
        case AST_AMBIGUITY :
            {
                solve_ambiguous_declaration(a, decl_context);
                // Restart function
                if (ASTType(a) == AST_AMBIGUITY)
                {
                    internal_error("Ambiguity not handled\n", 0);
                }
                build_scope_declaration(a, decl_context, nodecl_output, declared_symbols);
                break;
            }
        case AST_EMPTY_DECL :
            {
                // Do nothing
                break;
            }
        case AST_ASM_DEFINITION :
            {
                internal_error("Not yet implemented in nodecl", 0);
                break;
            }
        case AST_UNKNOWN_PRAGMA :
            {
                *nodecl_output = 
                    nodecl_make_list_1(
                            nodecl_make_builtin_decl(
                                nodecl_make_any_list(
                                    nodecl_make_list_1(
                                        nodecl_make_text(ASTText(a), ASTFileName(a), ASTLine(a))), 
                                    ASTFileName(a), ASTLine(a)),
                                "unknown-pragma", ASTFileName(a), ASTLine(a)));
                break;
            }
        case AST_PRAGMA_CUSTOM_DIRECTIVE :
            {
                build_scope_pragma_custom_directive(a, decl_context, NULL, nodecl_output);
                break;
            }
        case AST_PRAGMA_CUSTOM_CONSTRUCT: 
            {
                build_scope_pragma_custom_construct_declaration(a, decl_context, NULL, nodecl_output);
                break;
            }
            // GCC Extensions
        case AST_GCC_EXTENSION : // __extension__
            {
                build_scope_declaration(ASTSon0(a), decl_context, nodecl_output, declared_symbols);
                // FIXME Enable a gcc_extension bit for these symbols
                break;
            }
        case AST_GCC_ASM_DEFINITION :
            {
                build_scope_gcc_asm_definition(a, decl_context, nodecl_output);
                break;
            }
        case AST_GCC_USING_NAMESPACE_DIRECTIVE:
            {
                build_scope_using_directive(a, decl_context, nodecl_output);
                break;
            }
        case AST_GCC_NAMESPACE_DEFINITION :
            {
                build_scope_namespace_definition(a, decl_context, nodecl_output);
                break;
            }
        case AST_PP_COMMENT :
            {
                *nodecl_output = 
                    nodecl_make_list_1(
                            nodecl_make_builtin_decl(
                                nodecl_make_text(ASTText(a), ASTFileName(a), ASTLine(a)),
                                "pp-comment", ASTFileName(a), ASTLine(a)));
                break;
            }
        case AST_PP_TOKEN :
            {
                *nodecl_output = 
                    nodecl_make_list_1(
                            nodecl_make_builtin_decl(
                                nodecl_make_text(ASTText(a), ASTFileName(a), ASTLine(a)),
                                "pp-token", ASTFileName(a), ASTLine(a)));
                break;
            }
        case AST_VERBATIM :
            {
                *nodecl_output = 
                    nodecl_make_list_1(
                            nodecl_make_builtin_decl(
                                nodecl_make_text(ASTText(a), ASTFileName(a), ASTLine(a)),
                                "verbatim", ASTFileName(a), ASTLine(a)));
                break;
            }
        default :
            {
                internal_error("A declaration of kind '%s' is still unsupported (%s)\n", 
                        ast_print_node_type(ASTType(a)), ast_location(a));
                break;
            }
    }
}

// It simply disambiguates
static void build_scope_gcc_asm_definition(AST a, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    AST asm_parms = ASTSon1(a);

    nodecl_t nodecl_asm_params[3] = { nodecl_null(), nodecl_null(), nodecl_null() };

    int i;
    // first one is always an AST_STRING_LITERAL
    for (i = 1; i < 4; i++)
    {
        AST asm_operand_list = ASTChild(asm_parms, i);

        if (asm_operand_list != NULL)
        {
            AST iter;
            for_each_element(asm_operand_list, iter)
            {
                AST asm_operand = ASTSon1(iter);

                if (ASTType(asm_operand) == AST_GCC_ASM_OPERAND)
                {
                    AST identifier = ASTSon0(asm_operand);
                    AST constraint = ASTSon1(asm_operand);
                    AST expression = ASTSon2(asm_operand);
                    if (!check_expression(expression, decl_context))
                    {
                        error_printf("%s: error: assembler operand '%s' could not be checked\n",
                                ast_location(expression),
                                prettyprint_in_buffer(expression));
                    }

                    nodecl_t nodecl_identifier = nodecl_null();
                    if (identifier != NULL)
                    {
                        nodecl_identifier = nodecl_make_text(ASTText(identifier), ASTFileName(identifier), ASTLine(identifier));
                    }

                    nodecl_t nodecl_constraint = nodecl_null();
                    if (constraint != NULL)
                    {
                        nodecl_constraint = nodecl_make_text(ASTText(constraint), ASTFileName(constraint), ASTLine(constraint));
                    }

                    nodecl_t nodecl_asm_param = nodecl_make_builtin_decl(
                            nodecl_make_any_list(
                                nodecl_make_list_3(
                                    nodecl_constraint,
                                    expression_get_nodecl(expression),
                                    nodecl_identifier), 
                                ASTFileName(asm_operand), ASTLine(asm_operand)),
                            "gcc-asm-operand", 
                            ASTFileName(asm_operand), ASTLine(asm_operand));

                    nodecl_asm_params[i-1] = nodecl_append_to_list(nodecl_asm_params[i-1], 
                            nodecl_asm_param);
                }
                else
                {
                    internal_error("Unexpected tree '%s'\n", ast_print_node_type(ASTType(asm_operand)));
                }

            }
        }
    }

    nodecl_t nodecl_gcc_asm = nodecl_make_builtin_decl(
            nodecl_make_any_list(
                nodecl_make_list_4(
                    nodecl_make_text(
                        ASTText(ASTSon0(asm_parms)), 
                        ASTFileName(ASTSon0(asm_parms)), 
                        ASTLine(ASTSon0(asm_parms))),
                    nodecl_make_any_list(nodecl_asm_params[0], ASTFileName(a), ASTLine(a)),
                    nodecl_make_any_list(nodecl_asm_params[1], ASTFileName(a), ASTLine(a)),
                    nodecl_make_any_list(nodecl_asm_params[2], ASTFileName(a), ASTLine(a))),
                ASTFileName(a), ASTLine(a)),
            "gcc-asm-definition", ASTFileName(a), ASTLine(a));

    *nodecl_output = nodecl_make_list_1(nodecl_gcc_asm);
}

static void build_scope_explicit_instantiation(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    AST decl_specifier_seq = ASTSon1(a);
    AST declarator = ASTSon2(a);

    type_t* simple_type_info = NULL;
    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));
    gather_info.is_template = 1;
    gather_info.is_explicit_instantiation = 1;
    gather_info.no_declarators = (declarator == NULL);

    if (decl_specifier_seq != NULL)
    {
        build_scope_decl_specifier_seq(decl_specifier_seq, 
                &gather_info, 
                &simple_type_info, 
                decl_context,
                nodecl_output);
    }

    type_t* declarator_type = NULL;
    compute_declarator_type(declarator, &gather_info, simple_type_info, &declarator_type, decl_context, nodecl_output);
    // FIXME - We should instantiate here if no 'extern' is given
    if (declarator != NULL)
    {
        scope_entry_t *entry = build_scope_declarator_name(declarator, declarator_type, &gather_info, decl_context, nodecl_output);

        if (entry == NULL)
        {
            running_error("%s: invalid explicit instantiation\n", ast_location(a));
        }
    }
}

static void build_scope_using_directive(AST a, decl_context_t decl_context, nodecl_t* nodecl_output UNUSED_PARAMETER)
{
    // First get the involved namespace
    AST id_expression = ASTSon0(a);

    char turn_into_inline = 0;

    if (ASTType(a) == AST_GCC_USING_NAMESPACE_DIRECTIVE)
    {
        AST attr_list = ASTSon1(a);

        if (attr_list != NULL)
        {
            gather_decl_spec_t gather_info;
            memset(&gather_info, 0, sizeof(gather_info));

            gather_gcc_attribute_list(attr_list, &gather_info, decl_context);

            if (gather_info.is_inline)
            {
                turn_into_inline = 1;
            }
        }
    }

    scope_entry_list_t* result_list = query_id_expression(decl_context, 
            id_expression);
        
    if (result_list == NULL)
    {
        running_error("%s: error: unknown namespace '%s'\n",
                ast_location(a), 
                prettyprint_in_buffer(id_expression));
    }

    if (entry_list_size(result_list) > 1
            || entry_list_head(result_list)->kind != SK_NAMESPACE)
    {
        running_error("%s: error: '%s' does not name a namespace\n",
                ast_location(a), 
                prettyprint_in_buffer(id_expression));
    }

    scope_entry_t* entry = entry_list_head(result_list);

    entry_list_free(result_list);

    if (turn_into_inline)
    {
        entry->entity_specs.is_inline = 1;
    }

    // Now add this namespace to the used namespaces of this scope
    scope_t* namespace_scope = decl_context.current_scope;

    ERROR_CONDITION(entry->related_decl_context.current_scope->kind != NAMESPACE_SCOPE,
            "Error, related scope is not namespace scope", 0);

    P_LIST_ADD_ONCE(namespace_scope->use_namespace, 
            namespace_scope->num_used_namespaces, 
            entry);
}

static void introduce_using_entity(AST id_expression, decl_context_t decl_context, access_specifier_t current_access)
{
    scope_entry_list_t* used_entities = query_id_expression_flags(decl_context, 
            id_expression, 
            // Do not examine uninstantiated templates
            DF_DEPENDENT_TYPENAME);

    if (used_entities == NULL)
    {
        running_error("%s: error: entity '%s' in using-declaration is unknown",
                ast_location(id_expression),
                prettyprint_in_buffer(id_expression));
    }

    scope_entry_t* current_class = NULL;
    char is_class_scope = 0;
    if (decl_context.current_scope->kind == CLASS_SCOPE)
    {
        current_class = decl_context.current_scope->related_entry;
        is_class_scope = 1;
    }

    // Now add all the used entities to the current scope
    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(used_entities);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        char is_hidden = 0;

        if (is_class_scope)
        {
            if (entry->kind != SK_DEPENDENT_ENTITY)
            {
                if (!entry->entity_specs.is_member
                        || !class_type_is_base(entry->entity_specs.class_type, current_class->type_information))
                {
                    running_error("%s: error: '%s' is not a member of a base class\n",
                            ast_location(id_expression),
                            get_qualified_symbol_name(entry, 
                                decl_context));
                }
            }
            else
            {
                // Dependent entity
                continue;
            }

            // If this entity is being hidden by another member of this class, do not add it
            scope_entry_list_t* member_functions = class_type_get_member_functions(current_class->type_information);
            scope_entry_list_iterator_t* it2 = NULL;
            for (it2 = entry_list_iterator_begin(member_functions);
                    !entry_list_iterator_end(it2);
                    entry_list_iterator_next(it2))
            {
                scope_entry_t* current_member = entry_list_iterator_current(it2);

                if ((strcmp(current_member->symbol_name, entry->symbol_name) == 0)
                        && function_type_same_parameter_types(current_member->type_information, entry->type_information))
                {
                    // This one is being hidden by a member function of the current class
                    is_hidden = 1;
                    break;
                }
            }

            entry_list_iterator_free(it2);
            entry_list_free(member_functions);
        }

        if (is_hidden)
            continue;

        scope_entry_t* used_name = new_symbol(decl_context, decl_context.current_scope, entry->symbol_name);
        used_name->kind = SK_USING;
        used_name->file = ASTFileName(id_expression);
        used_name->line = ASTLine(id_expression);
        used_name->entity_specs.alias_to = entry;
        if (current_class != NULL)
        {
            used_name->entity_specs.is_member = 1;
            used_name->entity_specs.class_type = get_user_defined_type(current_class);
            used_name->entity_specs.access = current_access;
        }

        insert_entry(decl_context.current_scope, used_name);
    }

    entry_list_iterator_free(it);

    if (current_class != NULL)
    {
        scope_entry_t* used_hub_symbol = counted_calloc(1, sizeof(*used_hub_symbol), &_bytes_used_buildscope);
        used_hub_symbol->kind = SK_USING;
        used_hub_symbol->type_information = get_unresolved_overloaded_type(used_entities, NULL);
        used_hub_symbol->entity_specs.access = current_access;

        class_type_add_member(current_class->type_information, used_hub_symbol);
    }
}

static void build_scope_using_declaration(AST a, decl_context_t decl_context, access_specifier_t current_access,
        nodecl_t* nodecl_output UNUSED_PARAMETER)
{
    AST id_expression = ASTSon0(a);

    if (decl_context.current_scope->kind != CLASS_SCOPE
            && decl_context.current_scope->kind != NAMESPACE_SCOPE
            && decl_context.current_scope->kind != BLOCK_SCOPE)
    {
        running_error("%s: error: using-declaration not in a class, namespace or block scope",
                ast_location(a));
    }

    introduce_using_entity(id_expression, decl_context, current_access);
}

static void build_scope_member_declaration_qualified(AST a, decl_context_t decl_context, 
        access_specifier_t current_access,
        nodecl_t* nodecl_output UNUSED_PARAMETER)
{
    AST id_expression = ASTSon0(a);
    introduce_using_entity(id_expression, decl_context, current_access);
}

static void build_scope_static_assert(AST a, decl_context_t decl_context)
{
    AST constant_expr = ASTSon0(a);
    AST message = ASTSon1(a);

    if (!check_expression(constant_expr, decl_context))
    {
        error_printf("%s: error: static_assert expression is invalid\n",
                ast_location(a));
    }

    if (!expression_is_value_dependent(constant_expr))
    {
        if (!expression_is_constant(constant_expr))
        {
            error_printf("%s: error: static_assert expression is not constant\n",
                    ast_location(a));
        }
        else
        {
            const_value_t * val = expression_get_constant(constant_expr);

            if (const_value_is_zero(val))
            {
                running_error("%s: error: static_assert failed: %s\n",
                        ast_location(a),
                        prettyprint_in_buffer(message));
            }
        }
    }

    // FIXME - static_assert is not properly implemented for classes where they
    // should be signed in as if they were members
}

// Builds scope for a simple declaration
static void build_scope_simple_declaration(AST a, decl_context_t decl_context, 
        char is_template, char is_explicit_instantiation,
        nodecl_t *nodecl_output, scope_entry_list_t** declared_symbols)
{
    // Empty declarations are meaningless for the symbol table
    // They are of the form
    //    ;
    if (ASTType(a) == AST_EMPTY_DECL)
        return;

    type_t* simple_type_info = NULL;
    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));

    gather_info.is_template = is_template;
    gather_info.is_explicit_instantiation = is_explicit_instantiation;

    /* A simple declaration has two parts 
     *
     *    decl_specifier_seq declarator_list ';'
     *
     * Both are optional. decl_specifier_seq is ommited for constructors and
     * may be ommited for conversion functions and destructors.
     *
     * The declarator_list can be ommited only when the decl_specifier_seq
     * includes a class specifier, enum specifier or an elaborated type name
     * and declares something.
     */

    // If there are decl_specifiers gather information about them.
    //   gather_info will have everything not related to the type.
    //   simple_type_info will have the "base" type of every declarator 
    //
    // For instance 'int *f' will have "int" as a base type, but "f" will be
    // a pointer to int.
    if (ASTSon0(a) != NULL)
    {
        // This declaration can define a type if it is a class specifier or enum specifier
        // and just declare it if the declaration contains an elaborated-type-spec
        gather_info.no_declarators = (ASTSon1(a) == NULL);

        nodecl_t nodecl_decl_specifier = nodecl_null();
        build_scope_decl_specifier_seq(ASTSon0(a), &gather_info, &simple_type_info,
                decl_context, &nodecl_decl_specifier);
        *nodecl_output = nodecl_concat_lists(*nodecl_output, nodecl_decl_specifier);
    }
    else
    {
        C_LANGUAGE()
        {
            fprintf(stderr, "%s: warning: declaration does not have decl-specifier, assuming 'int'\n",
                    ast_location(a));

            simple_type_info = get_signed_int_type();
        }
    }

    if (gather_info.is_friend)
    {
        running_error("%s: error: friend specifier is not allowed here\n");
    }

    // If the type specifier defined a type, add it to the declared symbols
    if (gather_info.defined_type != NULL
            && declared_symbols != NULL)
    {
        *declared_symbols = entry_list_add(*declared_symbols, gather_info.defined_type);
    }

    ASTAttrSetValueType(a, LANG_IS_DECLARATION, tl_type_t, tl_bool(1));
    ast_set_link_to_child(a, LANG_DECLARATION_SPECIFIERS, ASTSon0(a));
    ast_set_link_to_child(a, LANG_DECLARATION_DECLARATORS, ASTSon1(a));

    // A type has been specified and there are declarators ahead
    if (simple_type_info != NULL 
            && ASTSon1(a) != NULL)
    {
        AST list, iter;
        list = ASTSon1(a);

        // Copy because of attributes that might modify this info
        gather_decl_spec_t current_gather_info = gather_info;

        // For every declarator create its full type based on the type
        // specified in the decl_specifier_seq
        for_each_element(list, iter)
        {
            AST init_declarator = ASTSon1(iter);

            if (ASTType(init_declarator) == AST_AMBIGUITY)
            {
                solve_ambiguous_init_declarator(init_declarator, decl_context);
            }

            ERROR_CONDITION(ASTType(init_declarator) != AST_INIT_DECLARATOR
                    && ASTType(init_declarator) != AST_GCC_INIT_DECLARATOR,
                    "Invalid node", 0);

            AST asm_specification = NULL;
            if (ASTType(init_declarator) == AST_GCC_INIT_DECLARATOR)
            {
                if (ASTSon3(init_declarator) != NULL)
                {
                    AST attribute_list = ASTSon3(init_declarator);
                    gather_gcc_attribute_list(attribute_list, &current_gather_info, decl_context);
                }
                asm_specification = ASTSon2(init_declarator);
            }

            AST declarator = ASTSon0(init_declarator);
            AST initializer = ASTSon1(init_declarator);

            type_t* declarator_type = NULL;

            // This will create the symbol if it is unqualified
            nodecl_t nodecl_declarator = nodecl_null();
            compute_declarator_type(declarator, &current_gather_info, 
                    simple_type_info, &declarator_type, decl_context, &nodecl_declarator);
            nodecl_t nodecl_declarator_name = nodecl_null();
            scope_entry_t *entry = build_scope_declarator_name(declarator, declarator_type, 
                    &current_gather_info, decl_context, &nodecl_declarator_name);

            ERROR_CONDITION(entry == NULL, "Declaration '%s' in '%s' did not declare anything!", 
                    prettyprint_in_buffer(a),
                    ast_location(a));

            if (initializer != NULL)
            {
                if (current_gather_info.is_extern)
                    running_error("%s: error: cannot initialize an 'extern' declaration\n", ast_location(a));

                if (entry->kind == SK_TYPEDEF)
                    running_error("%s: error: cannot initialize an typedef\n", ast_location(a));
            }

            // Only variables can be initialized
            if (entry->kind == SK_VARIABLE)
            {
                if (entry->defined
                        && !BITMAP_TEST(decl_context.decl_flags, DF_ALLOW_REDEFINITION))
                {
                    error_printf("%s: error: redefined entity '%s', first declared in '%s:%d'\n",
                            ast_location(declarator),
                            get_qualified_symbol_name(entry, decl_context),
                            entry->file,
                            entry->line);
                }
                if (initializer != NULL)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Initializer: '%s'\n", ast_print_node_type(ASTType(initializer)));
                    }

                    // This will yield a warning if needed but do not make it an error
                    char init_check = check_initialization(initializer, entry->decl_context, 
                            get_unqualified_type(declarator_type));

                    // Update unbounded arrays, bounded by their initialization
                    if (init_check)
                    {
                        type_t* initializer_type = expression_get_type(initializer);

                        if (is_array_type(declarator_type)
                                && nodecl_get_ast(array_type_get_array_size_expr(declarator_type)) == NULL
                                && is_array_type(initializer_type)
                                && nodecl_get_ast(array_type_get_array_size_expr(initializer_type)) != NULL)
                        {
                            entry->type_information = initializer_type;
                        }
                    }

                    entry->language_dependent_value = initializer;
                    entry->value = expression_get_nodecl(initializer);

                    {
                        ast_set_link_to_child(init_declarator, LANG_INITIALIZER, initializer);
                        ast_set_link_to_child(init_declarator, LANG_DECLARATOR, declarator);
                    }
                }
                // If it does not have initializer and it is not an extern entity
                // check a zero args constructor
                else if (!current_gather_info.is_extern)
                {
                    CXX_LANGUAGE()
                    {
                        if (!is_dependent_type(declarator_type))
                        {
                            check_default_initialization_and_destruction_declarator(entry, decl_context, 
                                    ASTFileName(declarator), ASTLine(declarator));
                        }
                    }
                }
                if (!current_gather_info.is_extern
                        || current_gather_info.emit_always)
                {
                    if (!entry->entity_specs.is_member // Not a member
                            || ( // Or a static member definition (outside of the class)
                                entry->entity_specs.is_static  
                                && decl_context.current_scope->kind != CLASS_SCOPE))
                    {
                        // Define the symbol
                        entry->defined = 1;
                        entry->point_of_definition = get_enclosing_declaration(init_declarator);

                        if (decl_context.current_scope->kind == NAMESPACE_SCOPE
                                || (IS_CXX_LANGUAGE
                                    && decl_context.current_scope->kind == BLOCK_SCOPE
                                    && is_class_type(entry->type_information)))
                        {
                            // Add an explicit initialization of this entity at namespace scope
                            *nodecl_output = nodecl_concat_lists(
                                    *nodecl_output,
                                    nodecl_make_list_1(nodecl_make_object_init(
                                            nodecl_null(),
                                            entry, 
                                            ASTFileName(init_declarator), ASTLine(init_declarator)))); 
                        }
                    }
                }
            }

            // GCC weird stuff
            if (asm_specification != NULL)
            {
                nodecl_t asm_spec = nodecl_make_builtin_decl(
                        nodecl_make_any_list(
                            nodecl_make_list_1(
                                nodecl_make_text(
                                    ASTText(ASTSon0(asm_specification)), 
                                    ASTFileName(ASTSon0(asm_specification)), 
                                    ASTLine(ASTSon0(asm_specification)))),
                            ASTFileName(asm_specification), 
                            ASTLine(asm_specification)),
                        "gcc-asm-spec",
                        ASTFileName(asm_specification), 
                        ASTLine(asm_specification));
                entry->entity_specs.asm_specification = nodecl_get_ast(asm_spec);
            }

            if (declared_symbols != NULL)
            {
                *declared_symbols = entry_list_add(*declared_symbols, entry);
            }
        }
    }
}


/* 
 * This function fills gather_info and simple_type_info with proper information
 *
 * gather_info contains every decl_specifier that is not type related. However
 * it can also include qualifiers like const, volatile, restrict, signed,
 * unsigned and long.
 *
 * unsigned int a;  // "unsigned" will be in gather_info and "int" in simple_type_info
 * unsigned b;      // "unsigned" will be considered directly simple_type_info
 * const A b;       // "const" will be in gather_info "A" in simple_type_info
 * unsigned long b; // There is an ambiguity in this case that should be solved favouring
 *                  // the option where there is a type_spec (either unsigned or long)
 *
 * Recall our grammar defines a decl_specifier_seq as 
 *    decl_specifier_seq -> nontype_decl_specifier_seq[opt] type_spec[opt] nontype_decl_specifier_seq[opt]
 *
 * Note: type_spec can be optional due to some corner cases like the following
 *
 *    struct A
 *    {
 *       // None of the following has type_spec but a nontype_decl_specifier_seq
 *       inline operator int(); 
 *       virtual ~A();
 *    };
 */
void build_scope_decl_specifier_seq(AST a, 
        gather_decl_spec_t* gather_info, 
        type_t **type_info, 
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    AST iter, list;

    if (ASTType(a) == AST_AMBIGUITY)
    {
        solve_ambiguous_decl_specifier_seq(a, decl_context);
        ERROR_CONDITION((ASTType(a) == AST_AMBIGUITY), "Ambiguity not solved", 0);
    }

    // Gather decl specifier sequence information previous to type_spec
    list = ASTSon0(a);
    if (list != NULL)
    {
        for_each_element(list, iter)
        {
            AST spec = ASTSon1(iter);
            gather_decl_spec_information(spec, gather_info, decl_context);
        }
    }

    // Gather decl specifier sequence information after type_spec
    list = ASTSon2(a);
    if (list != NULL)
    {
        for_each_element(list, iter)
        {
            AST spec = ASTSon1(iter);
            gather_decl_spec_information(spec, gather_info, decl_context);
        }
    }

    // Now gather information of the type_spec
    if (ASTSon1(a) != NULL) 
    {
        ast_set_link_to_child(a, LANG_TYPE_SPECIFIER, ASTSon1(a));

        gather_type_spec_information(ASTSon1(a), type_info, gather_info, decl_context, nodecl_output);
        
        // Now update the type_spec with type information that was caught in the decl_specifier_seq
        // First "long"/"short"
        if (gather_info->is_short)
        {
            if (*type_info == get_signed_int_type())
            {
                *type_info = get_signed_short_int_type();
            }
        }
        else if (gather_info->is_long == 1)
        {
            if (*type_info == get_signed_int_type())
            {
                *type_info = get_signed_long_int_type();
            }
            else if (*type_info == get_double_type())
            {
                *type_info = get_long_double_type();
            }
        }
        else if (gather_info->is_long >= 2)
        {
            if (*type_info == get_signed_int_type())
            {
                *type_info = get_signed_long_long_int_type();
            }
        }
        // Second signed/usigned
        if (gather_info->is_unsigned)
        {
            if (*type_info == get_char_type())
            {
                *type_info = get_unsigned_char_type();
            }
            else if (*type_info == get_signed_short_int_type())
            {
                *type_info = get_unsigned_short_int_type();
            }
            else if (*type_info == get_signed_int_type())
            {
                *type_info = get_unsigned_int_type();
            }
            else if (*type_info == get_signed_long_int_type())
            {
                *type_info = get_unsigned_long_int_type();
            }
            else if (*type_info == get_signed_long_long_int_type())
            {
                *type_info = get_unsigned_long_long_int_type();
            }
        }
        else if (gather_info->is_signed)
        {
            // Only for characters
            if (*type_info == get_char_type())
            {
                *type_info = get_signed_char_type();
            }
        }

        // GCC extension/C99
        if (gather_info->is_complex)
        {
            *type_info = get_complex_type(*type_info);
        }
        
        // cv-qualification
        if (gather_info->is_const)
        {
            // Add const
            *type_info = get_const_qualified_type(*type_info);
        }

        if (gather_info->is_volatile)
        {
            // Add volatile
            *type_info = get_volatile_qualified_type(*type_info);
        }

        if (gather_info->is_restrict)
        {
            // Add restrict
            *type_info = get_restrict_qualified_type(*type_info);
        }

    }
    else
    {
        C_LANGUAGE()
        {
            fprintf(stderr, "%s: warning: declaration does not have a type-specifier, assuming 'int'\n",
                    ast_location(a));

            *type_info = get_signed_int_type();
        }
    }
}


/*
 * This function gathers everything that is in a decl_spec and fills gather_info
 */
static void gather_decl_spec_information(AST a, gather_decl_spec_t* gather_info, decl_context_t decl_context)
{
    switch (ASTType(a))
    {
        // Storage specs
        case AST_AUTO_SPEC :
            gather_info->is_auto = 1;
            break;
        case AST_REGISTER_SPEC :
            gather_info->is_register = 1;
            break;
        case AST_STATIC_SPEC :
            gather_info->is_static = 1;
            break;
        case AST_EXTERN_SPEC :
            gather_info->is_extern = 1;
            break;
        case AST_MUTABLE_SPEC :
            gather_info->is_mutable = 1;
            break;
        case AST_THREAD_SPEC :
            gather_info->is_thread = 1;
            break;
        // Friend
        case AST_FRIEND_SPEC :
            gather_info->is_friend = 1;
            break;
        // Typedef
        case AST_TYPEDEF_SPEC :
            gather_info->is_typedef = 1;
            break;
        // Type modifiers
        case AST_SIGNED_TYPE :
            gather_info->is_signed = 1;
            break;
        case AST_UNSIGNED_TYPE :
            gather_info->is_unsigned = 1;
            break;
        case AST_LONG_TYPE :
            gather_info->is_long++;
            break;
        case AST_SHORT_TYPE :
            gather_info->is_short = 1;
            break;
        // CV qualifiers
        case AST_CONST_SPEC :
            gather_info->is_const = 1;
            break;
        case AST_VOLATILE_SPEC :
            gather_info->is_volatile = 1;
            break;
        // Function specifiers
        case AST_INLINE_SPEC :
            gather_info->is_inline = 1;
            break;
        case AST_VIRTUAL_SPEC :
            gather_info->is_virtual = 1;
            break;
        case AST_EXPLICIT_SPEC :
            gather_info->is_explicit = 1;
            break;
            // GCC Extensions
        case AST_GCC_RESTRICT_SPEC :
            gather_info->is_restrict = 1;
            break;
        case AST_GCC_ATTRIBUTE :
            gather_gcc_attribute(a, gather_info, decl_context);
            break;
        case AST_GCC_COMPLEX_TYPE :
            gather_info->is_complex = 1;
            break;
            // UPC extensions
        case AST_UPC_SHARED :
            {
                gather_info->upc.is_shared = 1;
                gather_info->upc.shared_layout = ASTSon0(a);
                if (gather_info->upc.shared_layout != NULL)
                {
                    AST list = gather_info->upc.shared_layout;
                    AST iter;

                    for_each_element(list, iter)
                    {
                        AST layout = ASTSon1(iter);
                        AST layout_qualif_kind = ASTSon0(layout);

                        if (layout_qualif_kind != NULL
                                && ASTType(layout_qualif_kind) != AST_UPC_LAYOUT_UNDEF)
                        {
                            check_expression(layout_qualif_kind, decl_context);
                        }
                    }
                }
                break;
            }
        case AST_UPC_RELAXED :
            gather_info->upc.is_relaxed = 1;
            break;
        case AST_UPC_STRICT :
            gather_info->upc.is_strict = 1;
            break;
            // CUDA stuff
        case AST_CUDA_GLOBAL:
            gather_info->cuda.is_global = 1;
            break;
        case AST_CUDA_DEVICE:
            gather_info->cuda.is_device = 1;
            break;
        case AST_CUDA_SHARED:
            gather_info->cuda.is_shared = 1;
            break;
        case AST_CUDA_CONSTANT:
            gather_info->cuda.is_constant = 1;
            break;
        case AST_XL_BUILTIN_SPEC :
            // Do nothing at the moment
            break;
            // Unknown node
        default:
            internal_error("Unknown node '%s' (%s)", ast_print_node_type(ASTType(a)), ast_location(a));
            break;
    }
}


/*
 * This function fills simple_type_info with type information.
 *
 * scope_t* sc is unused here
 */
void gather_type_spec_information(AST a, type_t** simple_type_info,
        gather_decl_spec_t* gather_info,
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    switch (ASTType(a))
    {
        case AST_SIMPLE_TYPE_SPEC :
            gather_type_spec_from_simple_type_specifier(a, simple_type_info, decl_context);
            break;
        case AST_ELABORATED_TYPENAME_SPEC : 
            gather_type_spec_from_dependent_typename(a, simple_type_info, decl_context);
            break;
        case AST_ENUM_SPECIFIER :
        case AST_GCC_ENUM_SPECIFIER :
            gather_type_spec_from_enum_specifier(a, simple_type_info, gather_info, decl_context);
            break;
        case AST_CLASS_SPECIFIER :
            gather_type_spec_from_class_specifier(a, simple_type_info, gather_info, decl_context, nodecl_output);
            break;
        case AST_ELABORATED_TYPE_ENUM_SPEC :
        case AST_GCC_ELABORATED_TYPE_ENUM_SPEC :
            gather_type_spec_from_elaborated_enum_specifier(a, simple_type_info, gather_info, decl_context);
            break;
        case AST_ELABORATED_TYPE_CLASS_SPEC :
        case AST_GCC_ELABORATED_TYPE_CLASS_SPEC :
            gather_type_spec_from_elaborated_class_specifier(a, simple_type_info, gather_info, decl_context);
            break;
        case AST_CHAR_TYPE :
            // It can be either signed or unsigned, so do not assume it is
            // signed like we do for int
            *simple_type_info = get_char_type();
            break;
        case AST_WCHAR_TYPE :
            *simple_type_info = get_wchar_t_type();
            break;
        case AST_BOOL_TYPE :
            *simple_type_info = get_bool_type();
            break;
        case AST_SHORT_TYPE :
            *simple_type_info = get_signed_int_type();
            gather_info->is_short = 1;
            break;
        case AST_INT_TYPE :
            *simple_type_info = get_signed_int_type();
            break;
        case AST_LONG_TYPE :
            *simple_type_info = get_signed_int_type();
            gather_info->is_long++;
            break;
        case AST_SIGNED_TYPE :
            *simple_type_info = get_signed_int_type();
            gather_info->is_signed = 1;
            break;
        case AST_UNSIGNED_TYPE :
            *simple_type_info = get_signed_int_type();
            gather_info->is_unsigned = 1;
            break;
        case AST_FLOAT_TYPE :
            *simple_type_info = get_float_type();
            break;
        case AST_DOUBLE_TYPE :
            *simple_type_info = get_double_type();
            break;
        case AST_VOID_TYPE :
            *simple_type_info = get_void_type();
            break;
        case AST_GCC_COMPLEX_TYPE :
            *simple_type_info = get_signed_int_type();
            gather_info->is_complex = 1;
            break;
        case AST_AMBIGUITY :
            solve_ambiguous_type_specifier(a, decl_context);
            // Restart function
            gather_type_spec_information(a, simple_type_info, gather_info, decl_context, nodecl_output);
            break;
            // C++0x
        case AST_DECLTYPE :
            {
                // Advance just before parentheses 
                // (a normal call to 'advance_expression_nest' would advance after them)
                AST expression = advance_expression_nest_flags(ASTSon0(a), /* advance_parentheses */ 0);

                // Compute the expression type and use it for the whole type
                if (check_expression(expression, decl_context)
                        && (expression_get_type(expression) != NULL))
                {
                    // Do not remove the reference here, we will do this later
                    // if mandated
                    type_t* computed_type = expression_get_type(expression);

                    if (is_unresolved_overloaded_type(computed_type))
                    {
                        scope_entry_list_t* entry_list = 
                            unresolved_overloaded_type_get_overload_set(computed_type);

                        if (entry_list_size(entry_list) > 1)
                        { 
                            running_error("%s: error: '%s' yields an unresolved overload type",
                                    ast_location(a), 
                                    prettyprint_in_buffer(a));
                        }

                        scope_entry_t* entry = entry_list_head(entry_list);
                        entry_list_free(entry_list);

                        if (!entry->entity_specs.is_member
                                || entry->entity_specs.is_static)
                        {
                            computed_type = entry->type_information;
                        }
                        else
                        {
                            computed_type = get_pointer_to_member_type(
                                    entry->type_information,
                                    entry);
                        }
                    }

                    if (is_dependent_expr_type(computed_type))
                    {
                        // The expression type is dependent, wrap it in a typeof
                        computed_type = get_gcc_typeof_expr_type(expression, decl_context);
                    }

                    switch (ASTType(expression))
                    {
                        // id-expressions
                        case AST_SYMBOL :
                        case AST_TEMPLATE_ID :
                        case AST_DESTRUCTOR_ID :
                        case AST_DESTRUCTOR_TEMPLATE_ID :
                        case AST_CONVERSION_FUNCTION_ID :
                        case AST_OPERATOR_FUNCTION_ID :
                        case AST_OPERATOR_FUNCTION_ID_TEMPLATE :
                        case AST_QUALIFIED_ID :
                        case AST_QUALIFIED_TEMPLATE :
                        // class member accesses
                        case AST_CLASS_MEMBER_ACCESS :
                        case AST_CLASS_TEMPLATE_MEMBER_ACCESS :
                        case AST_POINTER_CLASS_MEMBER_ACCESS :
                        case AST_POINTER_CLASS_TEMPLATE_MEMBER_ACCESS :
                            {
                                // If the 'e' expression is an id-expression or class member
                                // access, 'decltype(e)' is defined as the type of the entity
                                // named by 'e'. We remove the reference type.
                                *simple_type_info = no_ref(computed_type);
                                break;
                            }
                        default:
                            {
                                // Function calls or other expressions will
                                // return 'lvalues' with form of 'reference to
                                // type'. So, we do not need to update the type
                                *simple_type_info = computed_type;
                                break;
                            }
                    }
                }
                else
                {
                    running_error("%s: error: could not solve type '%s'\n",
                            ast_location(a),
                            prettyprint_in_buffer(a));
                }
                break;
            }
            // GCC Extensions
        case AST_GCC_TYPEOF :
            {
                // This one can be computed here, in fact, i don't understand
                // why one would want to use this
                //
                // typeof(int) <-- is it not obvious that this is an int ?

                // Compute here
                AST type_id = ASTSon0(a);
                AST type_specifier_seq = ASTSon0(type_id);
                AST abstract_decl = ASTSon1(type_id);

                type_t *type_info = NULL;

                gather_decl_spec_t typeof_gather_info;
                memset(&gather_info, 0, sizeof(gather_info));

                build_scope_decl_specifier_seq(type_specifier_seq, &typeof_gather_info, &type_info, decl_context, nodecl_output);

                if (is_named_type(type_info)
                        && named_type_get_symbol(type_info)->kind == SK_TEMPLATE)
                {
                    running_error("%s: error: invalid '%s' type-name, it names a template-name\n",
                            ast_location(type_specifier_seq),
                            prettyprint_in_buffer(type_specifier_seq));
                }

                type_t* declarator_type = type_info;
                compute_declarator_type(abstract_decl, 
                        &typeof_gather_info, type_info, &declarator_type,
                        decl_context, nodecl_output);

                *simple_type_info = declarator_type;
                break;
            }
        case AST_GCC_TYPEOF_EXPR :
            {
                // Compute the expression type and use it for the whole type
                if (check_expression(ASTSon0(a), decl_context)
                        && (expression_get_type(ASTSon0(a)) != NULL))
                {
                    type_t* computed_type = expression_get_type(ASTSon0(a));

                    CXX_LANGUAGE()
                    {
                        // Ignore top level references like g++ does
                        computed_type = no_ref(computed_type);

                        if (is_unresolved_overloaded_type(computed_type))
                        {
                            scope_entry_list_t* entry_list = 
                                unresolved_overloaded_type_get_overload_set(computed_type);

                            if (entry_list_size(entry_list) > 1)
                            {
                                running_error("%s: error: '%s' yields an unresolved overload type",
                                        ast_location(a), 
                                        prettyprint_in_buffer(a));
                            }

                            scope_entry_t* entry = entry_list_head(entry_list);
                            entry_list_free(entry_list);

                            if (!entry->entity_specs.is_member
                                    || entry->entity_specs.is_static)
                            {
                                computed_type = entry->type_information;
                            }
                            else
                            {
                                computed_type = get_pointer_to_member_type(
                                        entry->type_information, 
                                        entry);
                            }
                        }
                        else if (is_dependent_expr_type(computed_type))
                        {
                            // The expression type is dependent, so we will wrap in an typeof expression
                            computed_type = get_gcc_typeof_expr_type(ASTSon0(a), decl_context);
                        }
                    }

                    *simple_type_info = computed_type;
                }
                else
                {
                    running_error("%s: error: could not solve type '%s'\n",
                            ast_location(a),
                            prettyprint_in_buffer(a));
                }
                break;
            }
        default:
            internal_error("Unknown node '%s'", ast_print_node_type(ASTType(a)));
    }
    ASTAttrSetValueType(a, LANG_IS_TYPE_SPECIFIER, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_TYPE_SPECIFIER_TYPE, tl_type_t, tl_type(*simple_type_info));
}

static void gather_type_spec_from_elaborated_class_specifier(AST a, 
        type_t** type_info,
        gather_decl_spec_t *gather_info,
        decl_context_t decl_context)
{
    /*
     * This function should maintain strictly these two variables.
     *
     * class_entry will hold the symbol associated to a class specifier with name (or template-id)
     *
     * class_type will hold the class_type (NEVER a user defined type) to the class being declared
     *
     * *type_info must be computed as follows:
     *
     * *type_info = get_user_defined_type(class_entry);
     */
    scope_entry_t* class_entry = NULL;
    type_t* class_type = NULL;
    /* --- */

    AST class_key = ASTSon0(a);

    enum class_kind_t class_kind = CK_INVALID;
    const char *class_kind_name = NULL;

    switch (ASTType(class_key))
    {
        case AST_CLASS_KEY_CLASS:
            {
                class_kind = CK_CLASS;
                class_kind_name = "class";
                break;
            }
        case AST_CLASS_KEY_STRUCT:
            {
                class_kind = CK_STRUCT;
                class_kind_name = "struct";
                break;
            }
        case AST_CLASS_KEY_UNION:
            {
                class_kind = CK_UNION;
                class_kind_name = "union";
                break;
            }
        default:
            internal_error("Code unreachable", 0);
    }

    AST id_expression = ASTSon1(a);

    scope_entry_list_t* result_list = NULL;

    decl_flags_t decl_flags = DF_NONE;

    if (is_unqualified_id_expression(id_expression))
    {
        decl_flags |= DF_ELABORATED_NAME;
    }

    CXX_LANGUAGE()
    {
        if (gather_info->no_declarators
                && !gather_info->parameter_declaration)
        {
            if (is_unqualified_id_expression(id_expression))
            {
                result_list = query_in_scope_flags(decl_context, id_expression, decl_flags);
            }
            else
            {
                result_list = query_id_expression_flags(decl_context, id_expression, 
                        decl_flags);
            }
        }
        else
        {
            result_list = query_id_expression_flags(decl_context, 
                    id_expression, decl_flags);
        }
    }

    C_LANGUAGE()
    {
        const char* class_name = ASTText(id_expression);
        class_name = strappend(class_kind_name, strappend(" ", class_name));

        result_list = query_unqualified_name_str(decl_context, class_name);
    }

    // Now look for a type
    enum cxx_symbol_kind filter_classes[] = 
    {
        SK_CLASS,
        SK_TEMPLATE, // For the primary template
    };

    scope_entry_list_t* entry_list = filter_symbol_kind_set(result_list, 
            STATIC_ARRAY_LENGTH(filter_classes), filter_classes);

    entry_list_free(result_list);

    scope_entry_t* entry = (entry_list != NULL) ? entry_list_head(entry_list) : NULL;

    entry_list_free(entry_list);

    // We want the primary template in this particular case
    if (entry != NULL
            && entry->kind == SK_TEMPLATE)
    {
        if (decl_context.template_parameters->num_parameters
                != template_type_get_template_parameters(entry->type_information)->num_parameters)
        {
            running_error("%s: error: redeclaration with %d template parameters while previous declaration used %d\n",
                    ast_location(id_expression),
                    decl_context.template_parameters->num_parameters,
                    template_type_get_template_parameters(entry->type_information)->num_parameters);
        }

        template_type_update_template_parameters(entry->type_information,
                decl_context.template_parameters);

        // This is the class name
        type_t* primary_template_type = template_type_get_primary_type(entry->type_information);
        entry = named_type_get_symbol(primary_template_type);
    }

    char is_friend_class_declaration = 
        (gather_info->no_declarators && gather_info->is_friend);

    decl_context_t orig_decl_context = decl_context;

    if (entry == NULL)
    {
        // The name was not found.
        // We will have to create a symbol (if unqualified, otherwise this is an error)
        if (is_unqualified_id_expression(id_expression))
        {
            if (is_friend_class_declaration)
            {
                decl_context.current_scope = decl_context.namespace_scope;
            }
            else if (!gather_info->no_declarators
                    || gather_info->parameter_declaration)
            {
                // Look for the smallest enclosing non-function-prototype scope
                while (decl_context.current_scope->kind == CLASS_SCOPE
                        || decl_context.current_scope->kind == PROTOTYPE_SCOPE)
                {
                    decl_context.current_scope = decl_context.current_scope->contained_in;
                }
            }

            const char* class_name = NULL;
            if (ASTType(id_expression) == AST_SYMBOL)
            {
                class_name = ASTText(id_expression);
            }
            else if (ASTType(id_expression) == AST_TEMPLATE_ID)
            {
                class_name = ASTText(ASTSon0(id_expression));
            }
            else
            {
                running_error("%s: invalid class specifier '%s'\n",
                        ast_location(id_expression),
                        prettyprint_in_buffer(id_expression));
            }

            C_LANGUAGE()
            {
                class_name = strappend(class_kind_name, strappend(" ", class_name));
            }

            scope_entry_t* new_class = NULL;
            new_class = new_symbol(decl_context, decl_context.current_scope, class_name);

            new_class->line = ASTLine(id_expression);
            new_class->file = ASTFileName(id_expression);
            new_class->point_of_declaration = get_enclosing_declaration(id_expression);

            if ((!gather_info->is_template
                        || !gather_info->no_declarators)
                    && ASTType(id_expression) != AST_TEMPLATE_ID)
            {

                DEBUG_CODE()
                {
                    fprintf(stderr, "Type not found, creating a stub in scope %p for '%s' %p\n", 
                            decl_context.current_scope,
                            class_name,
                            new_class);
                }

                new_class->type_information = get_new_class_type(decl_context, class_kind);
                new_class->kind = SK_CLASS;

                new_class->entity_specs.is_friend_declared = is_friend_class_declaration;

                class_entry = new_class;
                class_type = class_entry->type_information;
            }
            else 
            {
                if (ASTType(id_expression) != AST_TEMPLATE_ID)
                {
                    new_class->kind = SK_TEMPLATE;
                    new_class->type_information = get_new_template_type(decl_context.template_parameters, 
                            get_new_class_type(decl_context, class_kind),
                            ASTText(id_expression), decl_context,
                            ASTLine(id_expression),
                            ASTFileName(id_expression));
                    template_type_set_related_symbol(new_class->type_information, new_class);

                    new_class->line = ASTLine(a);
                    new_class->file = ASTFileName(a);
                    new_class->point_of_declaration = get_enclosing_declaration(id_expression);

                    new_class->entity_specs.is_friend_declared = is_friend_class_declaration;

                    if (decl_context.current_scope->kind == CLASS_SCOPE)
                    {
                        new_class->entity_specs.is_member = 1;
                        // FIXME!
                        // new_class->entity_specs.access = current_access;
                        new_class->entity_specs.class_type = 
                            get_user_defined_type(decl_context.current_scope->related_entry);
                    }

                    // Get the primary class
                    class_entry = named_type_get_symbol(
                            template_type_get_primary_type(new_class->type_information)
                            );
                    // Update some fields
                    class_entry->line = ASTLine(a);
                    class_entry->file = ASTFileName(a);
                    class_entry->point_of_declaration = get_enclosing_declaration(id_expression);

                    class_type = class_entry->type_information;
                }
                else 
                {
                    // This is invalid because it is "class A<int>" but we
                    // didn't find any symbol related to it
                    running_error("%s: error: invalid template-name '%s'\n", 
                            ast_location(id_expression),
                            prettyprint_in_buffer(id_expression));
                }
            }
        }
        else
        {
            running_error("%s: error: class name '%s' not found", ast_location(id_expression), prettyprint_in_buffer(id_expression));
        }

        // If the class is being declared in class-scope it means
        // it is a nested class
        if (decl_context.current_scope->kind == CLASS_SCOPE)
        {
            // If the enclosing class is dependent, so is this one
            char c = is_dependent_type(class_entry->type_information);
            type_t* enclosing_class_type = decl_context.current_scope->related_entry->type_information;
            c = c || is_dependent_type(enclosing_class_type);
            set_is_dependent_type(class_entry->type_information, c);

            class_type_set_enclosing_class_type(class_entry->type_information, enclosing_class_type);
        }
        else if (decl_context.current_scope->kind == BLOCK_SCOPE)
        {
            // This is a local class
            scope_entry_t* enclosing_function = decl_context.current_scope->related_entry;
            if (enclosing_function != NULL
                    && is_dependent_type(enclosing_function->type_information))
            {
                set_is_dependent_type(class_entry->type_information, 1);
            }
        }
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Class type found already declared in %s:%d, using it\n", entry->file, entry->line);
        }
        ERROR_CONDITION(entry->kind != SK_CLASS, "This must be a class", 0);

        class_entry = entry;
        class_type = class_entry->type_information;

        if (!gather_info->is_friend
                && entry->entity_specs.is_friend_declared)
        {
            entry->entity_specs.is_friend_declared = 0;
        }

        // Check the enclosing namespace scope
        // This is only valid if the scope of the entry is an inlined namespace of the current one
        if (!gather_info->is_explicit_instantiation
                && is_template_specialized_type(class_entry->type_information)
                && (class_entry->decl_context.namespace_scope != decl_context.namespace_scope)
                && !is_inline_namespace_of(class_entry->decl_context, decl_context))
        {

            running_error("%s: specialization of '%s' in different namespace from definition\n",
                    ast_location(id_expression),
                    prettyprint_in_buffer(id_expression));
        }

        // Update template parameters only if not defined and they are not
        // empty (this happens for explicit instantiations)
        if (is_template_specialized_type(class_entry->type_information)
                && decl_context.template_parameters != NULL
                && !class_entry->defined)
        {
            template_specialized_type_update_template_parameters(class_entry->type_information,
                    decl_context.template_parameters);
        }
    }

    if (is_friend_class_declaration)
    {
        ERROR_CONDITION(orig_decl_context.current_scope->kind != CLASS_SCOPE, "Invalid class scope\n", 0);

        scope_entry_t* class_symbol = orig_decl_context.current_scope->related_entry;
        ERROR_CONDITION(class_symbol->kind != SK_CLASS, "Invalid symbol", 0);

        class_type_add_friend_symbol(class_symbol->type_information, class_entry);
    }

    ERROR_CONDITION(class_entry == NULL,
            "Invalid class entry", 0);

    if (is_template_specialized_type(class_type) 
            && !class_entry->defined)
    {
        // State this symbol has been created by the code and not by the type system
        class_entry->entity_specs.is_user_declared = 1;
    }

    *type_info = get_user_defined_type(class_entry);
}

static void gather_type_spec_from_elaborated_enum_specifier(AST a, 
        type_t** type_info, 
        gather_decl_spec_t* gather_info, 
        decl_context_t decl_context)
{
    AST id_expression = ASTSon0(a);

    scope_entry_list_t* result_list = NULL;

    decl_flags_t decl_flags = DF_NONE;

    if (is_unqualified_id_expression(id_expression))
    {
        decl_flags |= DF_ELABORATED_NAME;
    }

    CXX_LANGUAGE()
    {
        if (gather_info->no_declarators
                && !gather_info->parameter_declaration)
        {
            if (is_unqualified_id_expression(id_expression))
            {
                result_list = query_in_scope_flags(decl_context, id_expression, decl_flags);
            }
            else
            {
                result_list = query_id_expression_flags(decl_context, id_expression, decl_flags);
            };
        }
        else
        {
            result_list = query_id_expression_flags(decl_context, id_expression, decl_flags);
        }
    }

    C_LANGUAGE()
    {
        const char* enum_name = ASTText(id_expression);

        enum_name = strappend("enum ", enum_name);
        result_list = query_unqualified_name_str(decl_context, enum_name);
    }

    // Look for an enum name
    scope_entry_t* entry = NULL;

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(result_list);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t *current_entry = entry_list_iterator_current(it);
        if (current_entry->kind != SK_ENUM)
        {
            running_error("%s:%d: error: '%s' is not an enum-name\n", current_entry->file, current_entry->line, current_entry->symbol_name);
        }
        else
        {
            entry = current_entry;
        }
    }
    entry_list_iterator_free(it);
    entry_list_free(result_list);

    if (entry == NULL)
    {
        // Create a stub but only if it is unqualified, otherwise it should exist elsewhere
        if (is_unqualified_id_expression(id_expression)
                // If does not exist and there are no declarators
                && gather_info->no_declarators
                && !gather_info->parameter_declaration)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Enum type not found, creating a stub for this scope\n");
            }

            if (ASTType(id_expression) != AST_SYMBOL)
            {
                running_error("%s: invalid enum-name '%s'\n", 
                        ast_location(id_expression),
                        prettyprint_in_buffer(id_expression));
            }

            const char* enum_name = ASTText(id_expression);

            C_LANGUAGE()
            {
                enum_name = strappend("enum ", enum_name);
            }

            decl_context_t new_decl_context = decl_context;

            if (!gather_info->no_declarators)
            {
                // If no declarators declare it in the first non-class enclosing namespace
                new_decl_context.current_scope = decl_context.namespace_scope;
            }

            scope_entry_t* new_enum = new_symbol(new_decl_context, new_decl_context.current_scope, enum_name);
            new_enum->line = ASTLine(id_expression);
            new_enum->file = ASTFileName(id_expression);
            new_enum->point_of_declaration = get_enclosing_declaration(id_expression);
            new_enum->kind = SK_ENUM;
            new_enum->type_information = get_new_enum_type(decl_context);

            *type_info = get_user_defined_type(new_enum);

            if (decl_context.current_scope->kind == CLASS_SCOPE
                    && is_dependent_type(decl_context.current_scope->related_entry->type_information))
            {
                set_is_dependent_type(new_enum->type_information, 1);
            }
        }
        else
        {
            running_error("%s: error: enum type '%s' not found\n", ast_location(a), prettyprint_in_buffer(a));
        }
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Enum type found in %s:%d, using it\n", entry->file, entry->line);
        }

        *type_info = get_user_defined_type(entry);
    }
}

static void gather_type_spec_from_dependent_typename(AST a, type_t** type_info,
        decl_context_t decl_context)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "BUILDSCOPE: Have to solve dependent typename '%s'\n",
                prettyprint_in_buffer(a));
    }
    AST id_expression = ASTSon0(a);


    DEBUG_CODE()
    {
        fprintf(stderr, "BUILDSCOPE: Trying to look up a dependent typename '%s'\n",
                prettyprint_in_buffer(a));
    }

    scope_entry_list_t* result = NULL;
    result = query_id_expression_flags(decl_context, 
            id_expression,
            // We do not want to use uninstantiated 
            // templates when looking up
            DF_DEPENDENT_TYPENAME);

    if (result == NULL)
    {
        running_error("%s: typename '%s' not found\n", 
                ast_location(id_expression),
                prettyprint_in_buffer(id_expression));
    }

    scope_entry_t* entry = entry_list_head(result);

    entry_list_free(result);

    if (entry->kind != SK_DEPENDENT_ENTITY)
    {
        if (entry->kind != SK_TYPEDEF)
        {
            *type_info = get_user_defined_type(entry);
        }
        else
        {
            *type_info = advance_over_typedefs(entry->type_information);
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: Dependent typename refers to an existing type '%s'\n", print_declarator(*type_info));
        }

        return;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "BUILDSCOPE: Typename is a dependent entity -> returning a dependent type\n");
    }

    if (get_template_nesting_of_context(decl_context) == 0)
    {
        internal_error("Dependent typename '%s' not resolved outside of template scope (%s)\n", 
                prettyprint_in_buffer(a), ast_location(a));
    }

    *type_info = entry->type_information;
}

/*
 * This routine is called in gather_type_spec_information and its purpose is to
 * fill the simple_type with the proper reference of the user defined type.
 */
void gather_type_spec_from_simple_type_specifier(AST a, type_t** type_info,
        decl_context_t decl_context)
{
    AST id_expression = ASTSon0(a);

    decl_flags_t flags = DF_NONE;
    scope_entry_list_t* entry_list = query_id_expression_flags(decl_context, 
            id_expression, flags);

    scope_entry_list_t* old_entry_list = entry_list;
    entry_list = filter_friend_declared(old_entry_list);
    entry_list_free(old_entry_list);

    if (entry_list == NULL)
    {
        running_error("%s: error: type name '%s' has not been found in the current scope\n",
                ast_location(a), prettyprint_in_buffer(a));
    }

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_advance_aliases(entry_list_iterator_current(it));
        if (entry->kind != SK_ENUM 
                && entry->kind != SK_CLASS 
                && entry->kind != SK_TYPEDEF 
                && entry->kind != SK_TEMPLATE
                && entry->kind != SK_TEMPLATE_TYPE_PARAMETER
                && entry->kind != SK_TEMPLATE_TEMPLATE_PARAMETER
                && entry->kind != SK_GCC_BUILTIN_TYPE)
        {
            running_error("%s: error: identifier '%s' does not name a type", 
                    ast_location(a),
                    prettyprint_in_buffer(a));
        }
    }
    entry_list_iterator_free(it);

    scope_entry_t* entry = entry_advance_aliases(entry_list_head(entry_list));

    entry_list_free(entry_list);

    if (is_dependent_type(entry->type_information))
    {
        (*type_info) = get_dependent_typename_type_from_parts(entry, NULL);
    }
    else
    {
        (*type_info) = get_user_defined_type(entry);
    }
}

static type_t* compute_underlying_type_enum(const_value_t* min_value, 
        const_value_t* max_value, 
        type_t* underlying_type,
        char short_enums)
{
    if (is_dependent_expr_type(underlying_type))
        return underlying_type;


    type_t* signed_types[] =
    {
        get_signed_char_type(),
        get_signed_short_int_type(),
        get_signed_int_type(),
        get_signed_long_int_type(),
        get_signed_long_long_int_type(),
        NULL,
    };

    type_t* unsigned_types[] =
    {
        get_unsigned_char_type(),
        get_unsigned_short_int_type(),
        get_unsigned_int_type(),
        get_unsigned_long_int_type(),
        get_unsigned_long_long_int_type(),
        NULL,
    };

    char there_are_negatives = 0;
#define B_(x) const_value_is_nonzero(x)
    there_are_negatives = B_(const_value_lt(min_value, const_value_get_unsigned_int(0)));

    type_t** result = NULL;
    if (!short_enums)
    { 
        if (there_are_negatives)
        {
            result = &(signed_types[2]); // get_signed_int_type()
        }
        else
        {
            result = &(unsigned_types[2]); // get_unsigned_int_type()
        }
    }
    else
    {
        if (there_are_negatives)
        {
            result = signed_types;
        }
        else
        {
            result = unsigned_types;
        }
    }

    while (*result != NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "[BUILDSCOPE] Checking enum values range '%s..%s' with range '%s..%s' of %s\n",
                    prettyprint_in_buffer(const_value_to_tree(min_value)),
                    prettyprint_in_buffer(const_value_to_tree(max_value)),
                    prettyprint_in_buffer(const_value_to_tree(integer_type_get_minimum(*result))),
                    prettyprint_in_buffer(const_value_to_tree(integer_type_get_maximum(*result))),
                    print_declarator(*result));
        }

        if (B_(const_value_lte(integer_type_get_minimum(*result), min_value))
                && B_(const_value_lte(max_value, integer_type_get_maximum(*result))))
        {
            return *result;
        }
        result++;
    }
#undef B_
    internal_error("Cannot come up with a wide enough integer type for range %s..%s\n",
            prettyprint_in_buffer(const_value_to_tree(min_value)),
            prettyprint_in_buffer(const_value_to_tree(max_value)));
}

/*
 * This function is called for enum specifiers. It saves all enumerated values
 * and if it has been given a name, it is registered in the scope.
 */
void gather_type_spec_from_enum_specifier(AST a, type_t** type_info,
        gather_decl_spec_t* gather_info,
        decl_context_t decl_context)
{
    ast_set_link_to_child(a, LANG_IS_ENUM_SPECIFIER, a);

    type_t* enum_type = NULL;

    AST enum_name = ASTSon0(a);

    scope_entry_t* new_entry;

    // If it has name, we register this type name in the symbol table
    // but only if it has not been declared previously
    if (enum_name != NULL)
    {
        const char* enum_name_str = ASTText(enum_name);

        C_LANGUAGE()
        {
            enum_name_str = strappend("enum ", enum_name_str);
        }

        scope_entry_list_t* enum_entry_list = query_in_scope_str(decl_context, enum_name_str);

        if (enum_entry_list != NULL 
                && entry_list_size(enum_entry_list) == 1
                && entry_list_head(enum_entry_list)->kind == SK_ENUM)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Enum '%s' already declared\n", enum_name_str);
            }

            new_entry = entry_list_head(enum_entry_list);

            entry_list_free(enum_entry_list);
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Registering enum '%s' in '%p'\n", enum_name_str, decl_context.current_scope);
            }

            new_entry = new_symbol(decl_context, decl_context.current_scope, enum_name_str);
            new_entry->line = ASTLine(enum_name);
            new_entry->file = ASTFileName(enum_name);
            new_entry->point_of_declaration = get_enclosing_declaration(a);
            new_entry->kind = SK_ENUM;
            new_entry->type_information = get_new_enum_type(decl_context);
        }

        gather_info->defined_type = new_entry;
    }
    else
    {
        // Give it a fake name
        static int anonymous_enums = 0;
        char c[256];
        C_LANGUAGE()
        {
            snprintf(c, 255, "enum __mcc_enum_anon_%d", anonymous_enums);
        }
        CXX_LANGUAGE()
        {
            snprintf(c, 255, "__mcc_enum_anon_%d", anonymous_enums);
        }
        c[255] = '\0';
        anonymous_enums++;

        new_entry = new_symbol(decl_context, decl_context.current_scope, uniquestr(c));
        new_entry->line = ASTLine(a);
        new_entry->file = ASTFileName(a);
        new_entry->point_of_declaration = get_enclosing_declaration(a);
        new_entry->kind = SK_ENUM;
        new_entry->type_information = get_new_enum_type(decl_context);

        new_entry->entity_specs.is_anonymous = 1;
    }

    if (decl_context.current_scope->kind == CLASS_SCOPE
            && is_dependent_type(decl_context.current_scope->related_entry->type_information))
    {
        set_is_dependent_type(new_entry->type_information, 1);
    }

    enum_type = new_entry->type_information;

    char short_enums = CURRENT_CONFIGURATION->code_shape.short_enums;
    type_t* underlying_type = get_signed_int_type();

    new_entry->defined = 1;
    new_entry->point_of_definition = get_enclosing_declaration(a);
    // Since this type is not anonymous we'll want that type_info
    // refers to this newly created type
    *type_info = get_user_defined_type(new_entry);

    ASTAttrSetValueType(a, LANG_ENUM_SPECIFIER_SYMBOL, tl_type_t, tl_symbol(new_entry));

    AST list, iter;
    list = ASTSon1(a);
    
    if (list != NULL)
    {
        decl_context_t enumerators_context = decl_context;

        C_LANGUAGE()
        {
            // In C, enumerators are ALWAYS in global scope since CLASS_SCOPE
            // can't be nested actually
            if (enumerators_context.current_scope->kind == CLASS_SCOPE)
            {
                // Switch to the enclosing NAMESPACE scope
                enumerators_context.current_scope = enumerators_context.namespace_scope;
            }
        }

        int num_enumerator = 0;

        // Delta respect the previous to latest
        int delta = 0;
        // Latest known base enumerator
        AST base_enumerator = NULL;

        const_value_t* min_value = NULL;
        const_value_t* max_value = NULL;

        // For every enumeration, sign them up in the symbol table
        for_each_element(list, iter)
        {
            AST enumeration = ASTSon1(iter);
            AST enumeration_name = ASTSon0(enumeration);
            AST enumeration_expr = ASTSon1(enumeration);

            // Note that enums do not define an additional scope
            scope_entry_t* enumeration_item = new_symbol(enumerators_context, enumerators_context.current_scope, ASTText(enumeration_name));
            enumeration_item->line = ASTLine(enumeration_name);
            enumeration_item->file = ASTFileName(enumeration_name);
            enumeration_item->point_of_declaration = get_enclosing_declaration(enumeration_name);
            enumeration_item->kind = SK_ENUMERATOR;
            C_LANGUAGE()
            {
                enumeration_item->type_information = get_signed_int_type();
            }

            if (enumeration_expr != NULL)
            {
                if (!check_expression(enumeration_expr, enumerators_context))
                {
                    running_error("%s: error: invalid enumerator initializer '%s'\n",
                            ast_location(enumeration_expr),
                            prettyprint_in_buffer(enumeration_expr));
                }
                else
                {
                    if (!expression_is_constant(enumeration_expr))
                    {
                        C_LANGUAGE()
                        {
                            running_error("%s: error: expression '%s' is not constant\n",
                                    ast_location(enumeration_expr),
                                    prettyprint_in_buffer(enumeration_expr));
                        }
                        CXX_LANGUAGE()
                        {
                            underlying_type = get_dependent_expr_type();
                        }
                    }
                    CXX_LANGUAGE()
                    {
                        enumeration_item->type_information = expression_get_type(enumeration_expr);
                    }
                }

                enumeration_item->language_dependent_value = enumeration_expr;
                enumeration_item->value = expression_get_nodecl(enumeration_expr);

                delta = 1;
                base_enumerator = enumeration_expr;
            }
            else
            {
                if (num_enumerator == 0)
                {
                    // FIXME - We should be using the size in bytes of signed int
                    const_value_t* zero = const_value_get_signed_int(0);
                    AST zero_tree = const_value_to_tree(zero);

                    CXX_LANGUAGE()
                    {
                        enumeration_item->type_information = get_signed_int_type();
                    }
                    enumeration_item->language_dependent_value = zero_tree;
                    enumeration_item->value = const_value_to_nodecl(zero);
                    base_enumerator = zero_tree;
                    delta = 1;
                }
                else
                {
                    if (expression_is_constant(base_enumerator))
                    {
                        const_value_t* val_plus_one = 
                            const_value_add(
                                    expression_get_constant(base_enumerator),
                                    const_value_get_signed_int(delta));

                        enumeration_item->language_dependent_value = const_value_to_tree(val_plus_one);
                        enumeration_item->value = const_value_to_nodecl(val_plus_one);
                        enumeration_item->type_information = expression_get_type(enumeration_item->language_dependent_value);
                    }
                    else
                    {
                        char c[64];
                        snprintf(c, 63, "%d", delta);
                        c[63] = '\0';

                        const char *source = strappend( strappend( strappend("(", prettyprint_in_buffer(base_enumerator)) , ") + "), c);
                        AST add_one = internal_expression_parse(source, decl_context);
                        enumeration_item->language_dependent_value = add_one;
                        enumeration_item->value = expression_get_nodecl(add_one);

                        CXX_LANGUAGE()
                        {
                            enumeration_item->type_information = get_dependent_expr_type();
                        }
                        underlying_type = get_dependent_expr_type();
                    }

                    delta++;
                }
            }

            DEBUG_CODE()
            {
                if (expression_is_constant(enumeration_item->language_dependent_value))
                {
                    fprintf(stderr, "Registering enumerator '%s' with constant value '%lld' and type '%s'\n", ASTText(enumeration_name),
                            (long long int)const_value_cast_to_8(expression_get_constant(enumeration_item->language_dependent_value)),
                            print_declarator(enumeration_item->type_information));
                }
                else
                {
                    fprintf(stderr, "Registering enumerator '%s' with value '%s' and type '%s'\n", ASTText(enumeration_name),
                            prettyprint_in_buffer(enumeration_item->language_dependent_value),
                            print_declarator(enumeration_item->type_information));
                }
            }

            enum_type_add_enumerator(enum_type, enumeration_item);
            num_enumerator++;

#define B_(x) const_value_is_nonzero(x)
            if (expression_is_constant(enumeration_item->language_dependent_value)
                    && !is_dependent_expr_type(underlying_type))
            {
                const_value_t* current_value = expression_get_constant(enumeration_item->language_dependent_value);

                if (min_value == NULL
                        || B_(const_value_lt(current_value, min_value)))
                {
                    min_value = current_value;
                }
                if (max_value == NULL
                        || B_(const_value_lt(max_value, current_value)))
                {
                    max_value = current_value;
                }
            }
#undef B_
        }

        if (min_value == NULL)
            min_value = const_value_get_unsigned_int(0);
        if (max_value == NULL)
            max_value = const_value_get_unsigned_int(0);

        underlying_type = compute_underlying_type_enum(min_value, max_value, underlying_type, short_enums);
        enum_type_set_underlying_type(enum_type, underlying_type);

        DEBUG_CODE()
        {
            fprintf(stderr, "[BUILDSCOPE] Underlying type for '%s' computed to '%s'\n", 
                    print_declarator(enum_type),
                    print_declarator(underlying_type));
        }

        CXX_LANGUAGE()
        {
            // Now set the type of all the enumerators to be of the enumerator
            // type
            int i;
            for (i = 0; i < enum_type_get_num_enumerators(enum_type); i++)
            {
                scope_entry_t* enumerator = enum_type_get_enumerator_num(enum_type, i);
                enumerator->type_information = *type_info;
            }
        }
    }

    set_is_complete_type(enum_type, /* is_complete */ 1);
}

void build_scope_base_clause(AST base_clause, type_t* class_type, decl_context_t decl_context)
{
    AST list = ASTSon0(base_clause);
    AST iter;
    for_each_element(list, iter)
    {
        AST base_specifier = ASTSon1(iter);

        AST virtual_spec = ASTSon0(base_specifier);
        AST access_spec_tree = ASTSon1(base_specifier);
        AST class_name = ASTSon2(base_specifier);

        access_specifier_t access_specifier = AS_UNKNOWN;
        switch (class_type_get_class_kind(class_type))
        {
            case CK_CLASS :
                {
                    access_specifier = AS_PRIVATE;
                    break;
                }
            case CK_STRUCT :
                {
                    access_specifier = AS_PUBLIC;
                    break;
                }
            case CK_UNION:
                {
                    running_error("%s: a union cannot have bases\n", ast_location(base_clause));
                    break;
                }
            default:
                {
                    internal_error("Invalid class kind", 0);
                }
        }

        if (access_spec_tree != NULL)
        {
            switch (ASTType(access_spec_tree))
            {
                case AST_PUBLIC_SPEC:
                    {
                        access_specifier = AS_PUBLIC;
                        break;
                    }
                case AST_PRIVATE_SPEC:
                    {
                        access_specifier = AS_PRIVATE;
                        break;
                    }
                case AST_PROTECTED_SPEC:
                    {
                        access_specifier = AS_PROTECTED;
                        break;
                    }
                default:
                    {
                        internal_error("Unexpected tree '%s'\n", ast_print_node_type(ASTType(access_spec_tree)));
                    }
            }
        }

        char is_virtual = (virtual_spec != NULL);
        char is_dependent = 0;

        enum cxx_symbol_kind filter[] =
        {
            SK_CLASS,
            SK_TEMPLATE_TYPE_PARAMETER, 
            SK_TEMPLATE_TEMPLATE_PARAMETER, 
            SK_TYPEDEF, 
            SK_DEPENDENT_ENTITY
        };

        // We do not want to examine uninstantiated typenames
        scope_entry_list_t* result_list = query_id_expression_flags(decl_context, 
                class_name, DF_DEPENDENT_TYPENAME);

        scope_entry_list_t* filtered_result_list = filter_symbol_kind_set(result_list, STATIC_ARRAY_LENGTH(filter), filter);

        entry_list_free(result_list);

        if (filtered_result_list == NULL)
        {
            running_error("%s: base class '%s' not found\n", 
                    ast_location(class_name),
                    prettyprint_in_buffer(class_name));
        }

        scope_entry_t* result = entry_list_head(filtered_result_list);

        entry_list_free(filtered_result_list);

        if (result->kind != SK_TEMPLATE_TYPE_PARAMETER
                && result->kind != SK_TEMPLATE_TEMPLATE_PARAMETER
                && !is_dependent_type(result->type_information)
                && (result->kind == SK_CLASS
                    || result->kind == SK_TYPEDEF))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Base class '%s' IS NOT a dependent type\n", prettyprint_in_buffer(base_specifier));
            }
            
            scope_entry_t* base_class_symbol = result;
            // Update symbol because it might have been a typedef
            if (base_class_symbol->kind == SK_TYPEDEF)
            {
                base_class_symbol = named_type_get_symbol(
                        advance_over_typedefs(base_class_symbol->type_information)
                        );
            }

            type_t* base_class_type = base_class_symbol->type_information;
            
            // If the entity (being an independent one) has not been completed, then instantiate it
            if (class_type_is_incomplete_independent(get_actual_class_type(base_class_type)))
            {
                instantiate_template_class(base_class_symbol, decl_context, ASTFileName(base_specifier), ASTLine(base_specifier));
            }

            result = base_class_symbol;
        }
        else if (result->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                || result->kind == SK_TEMPLATE_TYPE_PARAMETER
                || is_dependent_type(result->type_information))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Base class '%s' found IS a dependent type\n", prettyprint_in_buffer(base_specifier));
            }
            is_dependent = 1;
        }
        else
        {
            running_error("%s: error: invalid class name '%s'\n",
                    ast_location(class_name),
                    prettyprint_in_buffer(class_name));
        }

        // Add the base to the class type
        class_type_add_base_class(get_actual_class_type(class_type), result, is_virtual, is_dependent, access_specifier);
    }
}

static char class_has_const_copy_assignment_operator(type_t* t)
{
    scope_entry_list_t* copy_assignment_ops = class_type_get_copy_assignment_operators(t);
    ERROR_CONDITION(!is_unnamed_class_type(t), "Must be an unnamed class", 0);
    ERROR_CONDITION(copy_assignment_ops == NULL, 
            "Bad class type", 0);

    char result = 0;

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(copy_assignment_ops);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* copy_assig_op = entry_list_iterator_current(it);

        // Check that operator= is actually
        //    operator=(T)
        //    operator=(const T&)
        //    operator=(const volatile T&)
        type_t* first_param_type = function_type_get_parameter_type_num(copy_assig_op->type_information, 0);

        //    operator=(const T&)
        //    operator=(const volatile T&)
        if (is_lvalue_reference_to_class_type(first_param_type)
                && is_const_qualified_type(reference_type_get_referenced_type(first_param_type)))
        {
                result = 1;
                break;
        }
        //    operator=(T)
        else if (is_class_type(first_param_type))
        {
            result = 1;
            break;
        }
    }
    entry_list_iterator_free(it);
    entry_list_free(copy_assignment_ops);

    return result;
}

static char class_has_const_copy_constructor(type_t* t)
{
    scope_entry_list_t* copy_constructors = class_type_get_copy_constructors(t);

    ERROR_CONDITION(!is_unnamed_class_type(t), "Must be an unnamed class", 0);
    ERROR_CONDITION(copy_constructors == NULL,
            "Bad class type", 0);

    char result = 0;
    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(copy_constructors);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* copy_assig_op = entry_list_iterator_current(it);

        // Check that the constructor is actually
        //  A(const A&)
        //  A(const volatile A&)
        type_t* first_param_type = function_type_get_parameter_type_num(copy_assig_op->type_information, 0);

        if (is_lvalue_reference_to_class_type(first_param_type)
                && is_const_qualified_type(reference_type_get_referenced_type(first_param_type)))
        {
            result = 1;
            break;
        }
    }

    entry_list_iterator_free(it);
    entry_list_free(copy_constructors);

    return result;
}

static char is_virtual_destructor(type_t* class_type);

#if 0
static void compute_code_for_default_constructor(type_t* class_type, nodecl_t* nodecl_output, 
        scope_entry_t* entry, const char* filename, int line)
{
    scope_entry_list_t* virtual_base_classes = class_type_get_virtual_base_classes(class_type);
    scope_entry_list_t* direct_base_classes = class_type_get_direct_base_classes(class_type);
    scope_entry_list_t* nonstatic_data_members = class_type_get_nonstatic_data_members(class_type);

    nodecl_t nodecl_initializer_list = nodecl_null();
    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(virtual_base_classes);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* current_member = entry_list_iterator_current(it);

        nodecl_initializer_list = nodecl_append_to_list(
                nodecl_initializer_list,
                nodecl_make_object_init(nodecl_null(), current_member, filename, line));
                
    }
    entry_list_iterator_free(it);

    for (it = entry_list_iterator_begin(direct_base_classes);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* current_member = entry_list_iterator_current(it);

        nodecl_initializer_list = nodecl_append_to_list(
                nodecl_initializer_list,
                nodecl_make_object_init(nodecl_null(), current_member, filename, line));
    }
    entry_list_iterator_free(it);

    for (it = entry_list_iterator_begin(nonstatic_data_members);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* current_member = entry_list_iterator_current(it);

        nodecl_initializer_list = nodecl_append_to_list(
                nodecl_initializer_list,
                nodecl_make_object_init(nodecl_null(), current_member, filename, line));
    }
    entry_list_iterator_free(it);

    entry_list_free(virtual_base_classes);
    entry_list_free(direct_base_classes);
    entry_list_free(nonstatic_data_members);

    decl_context_t block_context = new_block_context(entry->decl_context);

    *nodecl_output = nodecl_make_function_code(
            nodecl_make_list_1(nodecl_make_compound_statement(nodecl_null(), 
                    /* destructors??? */nodecl_null(), 
                    new_scope_symbol(block_context),
                    filename, line)),
            nodecl_initializer_list,
            nodecl_null(), entry, filename, line);
}
#endif

static void build_scope_ctor_initializer(
        AST ctor_initializer, 
        scope_entry_t* function_entry,
        decl_context_t decl_context,
        const char* filename,
        int line,
        nodecl_t* nodecl_output)
{
    scope_entry_t* class_sym = named_type_get_symbol(function_entry->entity_specs.class_type);

    char dependent_context =  (is_dependent_type(class_sym->type_information)
            || is_dependent_type(function_entry->type_information));

    scope_entry_list_t* virtual_bases = NULL;
    scope_entry_list_t* direct_base_classes = NULL;
    scope_entry_list_t* nonstatic_data_members = NULL;

    if (!dependent_context)
    {
        virtual_bases =
            class_type_get_virtual_base_classes(class_sym->type_information);
        direct_base_classes =
            class_type_get_direct_base_classes(class_sym->type_information);
        nonstatic_data_members =
            class_type_get_nonstatic_data_members(class_sym->type_information);
    }

    scope_entry_list_t* already_initialized = NULL;

    if (ctor_initializer != NULL)
    {
        ERROR_CONDITION(decl_context.current_scope->kind != BLOCK_SCOPE,
                "Block scope is not valid", 0);

        AST mem_initializer_list = ASTSon0(ctor_initializer);
        AST iter;

        for_each_element(mem_initializer_list, iter)
        {
            AST mem_initializer = ASTSon1(iter);

            switch (ASTType(mem_initializer))
            {
                case AST_MEM_INITIALIZER :
                    {
                        AST mem_initializer_id = ASTSon0(mem_initializer);
                        AST initializer = ASTSon1(mem_initializer);

                        AST id_expression = ASTSon0(mem_initializer_id);

                        scope_entry_list_t* result_list = NULL;
                        result_list = query_id_expression(decl_context, id_expression);

                        // In dependent contexts, just disambiguate by
                        // querying, but nothing else)
                        if (dependent_context)
                            break;

                        if (result_list == NULL)
                        {
                            running_error("%s: initialized entity '%s' not found\n", 
                                    ast_location(id_expression),
                                    prettyprint_in_buffer(id_expression));
                        }

                        scope_entry_t* entry = entry_list_head(result_list);
                        entry_list_free(result_list);

                        if (entry->kind == SK_TYPEDEF)
                        {
                            if (is_named_type(advance_over_typedefs(entry->type_information)))
                            {
                                entry = named_type_get_symbol(advance_over_typedefs(entry->type_information));
                            }
                        }

                        // Chances are that through class-scope lookup we have found the injected name
                        if (entry->kind == SK_CLASS 
                                && entry->entity_specs.is_injected_class_name)
                        {
                            // The injected class name is a member
                            entry = named_type_get_symbol(entry->entity_specs.class_type);
                        }

                        if (entry_list_contains(already_initialized, entry))
                        {
                            running_error("%s: error: '%s' initialized twice in member initializer list\n",
                                    ast_location(id_expression),
                                    get_qualified_symbol_name(entry, entry->decl_context));
                        }

                        if (entry->kind == SK_VARIABLE)
                        {
                            if (!entry_list_contains(nonstatic_data_members, entry))
                            {
                                if (!entry->entity_specs.is_member
                                        || !equivalent_types(entry->entity_specs.class_type, function_entry->entity_specs.class_type))
                                {
                                    running_error("%s: symbol '%s' is not a member of class %s\n",
                                            ast_location(id_expression),
                                            get_qualified_symbol_name(entry, entry->decl_context),
                                            get_qualified_symbol_name(named_type_get_symbol(function_entry->entity_specs.class_type), 
                                                function_entry->decl_context));
                                }
                                if (entry->entity_specs.is_static)
                                {
                                    running_error("%s: static data member '%s' cannot be initialized here\n", 
                                            ast_location(id_expression),
                                            prettyprint_in_buffer(id_expression));
                                }
                            }

                            check_initialization(initializer,
                                    entry->decl_context,
                                    get_unqualified_type(entry->type_information));

                            entry_list_add(already_initialized, entry);
                        }
                        else if (entry->kind == SK_CLASS)
                        {
                            if (!entry_list_contains(direct_base_classes, entry)
                                    && !entry_list_contains(virtual_bases, entry))
                            {
                                fprintf(stderr, "%s: class '%s' is not a direct base or virtual base of class '%s'\n",
                                        ast_location(id_expression),
                                        get_qualified_symbol_name(entry, entry->decl_context),
                                        get_qualified_symbol_name(class_sym, class_sym->decl_context));
                            }

                            check_initialization(initializer,
                                    entry->decl_context,
                                    get_user_defined_type(entry));

                            entry_list_add(already_initialized, entry);
                        }
                        else
                        {
                            running_error("%s: symbol '%s' cannot be initialized here\n",
                                    ast_location(id_expression),
                                    get_qualified_symbol_name(entry, entry->decl_context));
                        }

                        nodecl_t nodecl_object_init = nodecl_make_object_init(
                                expression_get_nodecl(initializer),
                                entry,
                                ASTFileName(id_expression),
                                ASTLine(id_expression));

                        *nodecl_output = nodecl_append_to_list(*nodecl_output, nodecl_object_init);

                        break;
                    }
                default : 
                    {
                        internal_error("Unexpected node '%s' in constructor declaration", ast_print_node_type(ASTType(mem_initializer)));
                        break;
                    }
            }
        }
    }

    if (dependent_context)
        return;

    // Now review the remaining objects not initialized yet
    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(virtual_bases);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);
        scope_entry_t* constructor = NULL;
        check_default_initialization(entry, entry->decl_context, filename, line, &constructor);
    }
    entry_list_iterator_free(it);

    for (it = entry_list_iterator_begin(direct_base_classes);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);
        scope_entry_t* constructor = NULL;
        check_default_initialization(entry, entry->decl_context, filename, line, &constructor);
    }
    entry_list_iterator_free(it);

    for (it = entry_list_iterator_begin(nonstatic_data_members);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);
        scope_entry_t* constructor = NULL;
        check_default_initialization(entry, entry->decl_context, filename, line, &constructor);
    }
    entry_list_iterator_free(it);
}


static void apply_function_to_data_layout_members(
        scope_entry_t* entry,
        void (*fun)(scope_entry_t*, void*),
        void *data)
{
    ERROR_CONDITION(entry->kind != SK_CLASS, "Invalid class symbol", 0);

    scope_entry_list_t* virtual_base_classes = class_type_get_virtual_base_classes(entry->type_information);
    scope_entry_list_t* direct_base_classes = class_type_get_direct_base_classes(entry->type_information);
    scope_entry_list_t* nonstatic_data_members = class_type_get_nonstatic_data_members(entry->type_information);

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(virtual_base_classes);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* current_entry = entry_list_iterator_current(it);
        fun(current_entry, data);
    }
    entry_list_iterator_free(it);

    for (it = entry_list_iterator_begin(direct_base_classes);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* current_entry = entry_list_iterator_current(it);
        fun(current_entry, data);
    }
    entry_list_iterator_free(it);

    for (it = entry_list_iterator_begin(nonstatic_data_members);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* current_entry = entry_list_iterator_current(it);
        fun(current_entry, data);
    }
    entry_list_iterator_free(it);

    entry_list_free(virtual_base_classes);
    entry_list_free(direct_base_classes);
    entry_list_free(nonstatic_data_members);
}

struct check_constructor_helper
{
    const char* filename;
    int line;
    char has_const;
};

static void ensure_default_constructor_is_emitted(scope_entry_t* entry, void* data)
{
    ERROR_CONDITION(entry->kind != SK_CLASS && entry->kind != SK_VARIABLE, "Invalid symbol", 0);
    struct check_constructor_helper* p = (struct check_constructor_helper*)data;

    scope_entry_t* constructor = NULL;
    check_default_initialization(entry, entry->decl_context, p->filename, p->line, &constructor);
}

static void ensure_copy_constructor_is_emitted(scope_entry_t* entry, void* data)
{
    ERROR_CONDITION(entry->kind != SK_CLASS && entry->kind != SK_VARIABLE , "Invalid symbol", 0);
    struct check_constructor_helper* p = (struct check_constructor_helper*)data;

    scope_entry_t* constructor = NULL;
    check_copy_constructor(entry, entry->decl_context, p->has_const,
            p->filename, p->line, &constructor);
}

static void ensure_copy_assignment_operator_is_emitted(scope_entry_t* entry, void* data)
{
    ERROR_CONDITION(entry->kind != SK_CLASS && entry->kind != SK_VARIABLE, "Invalid symbol", 0);
    struct check_constructor_helper* p = (struct check_constructor_helper*)data;

    scope_entry_t* constructor = NULL;
    check_copy_assignment_operator(entry, entry->decl_context, p->has_const,
            p->filename, p->line, &constructor);
}

static void ensure_destructor_is_emitted(scope_entry_t* entry, void* data)
{
    ERROR_CONDITION(entry->kind != SK_CLASS && entry->kind != SK_VARIABLE, "Invalid symbol", 0);

    if (!is_class_type(entry->type_information))
        return;

    struct check_constructor_helper* p = (struct check_constructor_helper*)data;

    scope_entry_t* destructor = class_type_get_destructor(entry->type_information);
    ensure_function_is_emitted(destructor, p->filename, p->line);
    ERROR_CONDITION(destructor == NULL, "Bad class lacking destructor", 0);
}

// These functions emit special members that may have to be defined by the compiler itself
// they ensure that every other function that might have to be called will be emitted too
// (this implies calling other implicitly defined members)
//
// Currently they do not generate nodecl (we are unsure what we would do with it) but maybe
// they will have to in the future

static void emit_implicit_default_constructor(scope_entry_t* entry, const char* filename, int line)
{
    entry->entity_specs.is_non_emitted = 0;
    entry->entity_specs.emission_handler = NULL;

    struct check_constructor_helper l = { .filename = filename, .line = line, .has_const = 0 };
    apply_function_to_data_layout_members(named_type_get_symbol(entry->entity_specs.class_type), 
            ensure_default_constructor_is_emitted, &l);
}

static void emit_implicit_copy_constructor(scope_entry_t* entry,
        const char* filename, 
        int line)
{
    entry->entity_specs.is_non_emitted = 0;
    entry->entity_specs.emission_handler = NULL;

    char has_const = is_const_qualified_type(
            no_ref(function_type_get_parameter_type_num(entry->type_information, 0)));

    struct check_constructor_helper l = { .filename = filename, .line = line, .has_const = has_const };
    apply_function_to_data_layout_members(named_type_get_symbol(entry->entity_specs.class_type), 
            ensure_copy_constructor_is_emitted, &l);
}

static void emit_implicit_copy_assignment_operator(scope_entry_t* entry,
        const char* filename, 
        int line)
{
    entry->entity_specs.is_non_emitted = 0;
    entry->entity_specs.emission_handler = NULL;

    char has_const = is_const_qualified_type(
            no_ref(function_type_get_parameter_type_num(entry->type_information, 0)));

    struct check_constructor_helper l = { .filename = filename, .line = line, .has_const = has_const };
    apply_function_to_data_layout_members(named_type_get_symbol(entry->entity_specs.class_type), 
            ensure_copy_assignment_operator_is_emitted, &l);
}

static void emit_implicit_destructor(scope_entry_t* entry,
        const char* filename,
        int line)
{
    entry->entity_specs.is_non_emitted = 0;
    entry->entity_specs.emission_handler = NULL;

    struct check_constructor_helper l = { .filename = filename, .line = line, .has_const = 0 };
    apply_function_to_data_layout_members(named_type_get_symbol(entry->entity_specs.class_type), 
            ensure_destructor_is_emitted, &l);
}

// See gather_type_spec_from_class_specifier to know what are class_type and type_info
// This function is only for C++
//
// FIXME - This function is HUGE
static void finish_class_type_cxx(type_t* class_type, type_t* type_info, decl_context_t decl_context,
        const char *filename, int line,
        nodecl_t* nodecl_output UNUSED_PARAMETER)
{
    // Finish the class creating implicit special members
    //
    // Only for non-dependent classes
    if (is_dependent_type(class_type))
        return;

    DEBUG_CODE()
    {
        fprintf(stderr, "BUILDSCOPE: Finishing class\n");
    }

    scope_entry_list_t* virtual_base_classes = class_type_get_virtual_base_classes(class_type);
    scope_entry_list_t* direct_base_classes = class_type_get_direct_base_classes(class_type);
    scope_entry_list_t* nonstatic_data_members = class_type_get_nonstatic_data_members(class_type);
    scope_entry_list_t* all_bases = entry_list_merge(direct_base_classes, virtual_base_classes);
    // Force instantiation of required types since we need them full when laying out
    {
        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t *data_member = entry_list_iterator_current(it);
            DEBUG_CODE()
            {
                fprintf(stderr, "BUILDSCOPE: Completing member '%s' at '%s:%d' with type '%s'\n",
                        data_member->symbol_name,
                        data_member->file,
                        data_member->line,
                        print_type_str(data_member->type_information, decl_context));
            }

            type_t* current_type = data_member->type_information;

            if (is_lvalue_reference_type(current_type)
                    || is_rvalue_reference_type(current_type))
            {
                current_type = reference_type_get_referenced_type(current_type);
            }
            if (is_array_type(current_type))
            {
                current_type = array_type_get_element_type(current_type);
            }

            if (is_named_class_type(current_type)
                    && class_type_is_incomplete_independent(get_actual_class_type(current_type)))
            {
                scope_entry_t* named_type_sym = named_type_get_symbol(current_type);

                instantiate_template_class(named_type_sym, decl_context, filename, line);
            }
            DEBUG_CODE()
            {
                fprintf(stderr, "BUILDSCOPE: Member '%s' completed\n",
                        data_member->symbol_name);
            }
        }
        entry_list_iterator_free(it);

        // Base classes have already been instantiated, no need to do it here
    }
    
    // Virtual members
    {
        // Discover members inherited
        scope_entry_list_t* member_functions = class_type_get_member_functions(class_type);
        scope_entry_list_iterator_t* it0 = NULL;

        for (it0 = entry_list_iterator_begin(member_functions);
                !entry_list_iterator_end(it0);
                entry_list_iterator_next(it0))
        {
            scope_entry_t *entry = entry_list_iterator_current(it0);

            if (entry->entity_specs.is_static)
                continue;

            if (entry->entity_specs.is_virtual)
            {
                if (entry->entity_specs.is_pure)
                {
                    class_type_set_is_abstract(class_type, 1);
                }
                continue;
            }

            char found_virtual = 0;

            scope_entry_list_iterator_t* it1 = NULL;
            for (it1 = entry_list_iterator_begin(all_bases);
                    !entry_list_iterator_end(it1) && !found_virtual;
                    entry_list_iterator_next(it1))
            {
                scope_entry_t *base_class = entry_list_iterator_current(it1);

                scope_entry_list_t* virtual_functions = class_type_get_virtual_functions(base_class->type_information);

                scope_entry_list_iterator_t* it2 = NULL;

                for (it2 = entry_list_iterator_begin(virtual_functions);
                        !entry_list_iterator_end(it2) && !found_virtual;
                        entry_list_iterator_next(it2))
                {
                    scope_entry_t* current_virtual = entry_list_iterator_current(it2);

                    if (strcmp(entry->symbol_name, current_virtual->symbol_name) == 0
                            && function_type_can_override(entry->type_information, current_virtual->type_information))
                    {
                        entry->entity_specs.is_virtual = 1;

                        found_virtual = 1;

                        DEBUG_CODE()
                        {
                            fprintf(stderr, "Function '%s' of '%s:%d' is inheritedly virtual\n",
                                    print_decl_type_str(entry->type_information, decl_context, entry->symbol_name),
                                    entry->file,
                                    entry->line);
                        }
                    }
                }

                entry_list_iterator_free(it2);
                entry_list_free(virtual_functions);
            }
            entry_list_iterator_free(it1);
        }
        entry_list_iterator_free(it0);
        entry_list_free(member_functions);
    }

    scope_entry_list_t* virtual_functions = class_type_get_virtual_functions(class_type);
    char has_virtual_functions = virtual_functions != NULL;
    entry_list_free(virtual_functions);

    // Implicit default constructor
    scope_entry_list_t* constructors = class_type_get_constructors(class_type);
    char no_constructors = (constructors == NULL);
    entry_list_free(constructors);

    if (no_constructors)
    {
        type_t* default_constructor_type = get_new_function_type(
                NULL, // Constructors do not return anything
                NULL, // Default constructor does not receive anything
                0);

        scope_t* sc = class_type_get_inner_context(class_type).current_scope;

        char constructor_name[256] = { 0 };
        if (is_named_class_type(type_info))
        {
            snprintf(constructor_name, 256, "constructor %s", named_type_get_symbol(type_info)->symbol_name);
        }
        else
        {
            snprintf(constructor_name, 256, "%s", "constructor ");
        }

        scope_entry_t* implicit_default_constructor = new_symbol(class_type_get_inner_context(class_type), sc,
                constructor_name);

        implicit_default_constructor->kind = SK_FUNCTION;
        implicit_default_constructor->entity_specs.is_member = 1;
        implicit_default_constructor->entity_specs.access = AS_PUBLIC;
        implicit_default_constructor->entity_specs.class_type = type_info;
        implicit_default_constructor->entity_specs.is_inline = 1;
        implicit_default_constructor->entity_specs.is_constructor = 1;
        implicit_default_constructor->entity_specs.is_default_constructor = 1;

        implicit_default_constructor->type_information = default_constructor_type;

        implicit_default_constructor->defined = 1;

        implicit_default_constructor->entity_specs.num_parameters = 0;

        class_type_add_member(class_type, implicit_default_constructor);
        class_type_set_default_constructor(class_type, implicit_default_constructor);

        nodecl_t nodecl_ctor_initializer = nodecl_null();
        build_scope_ctor_initializer(/* ctor_initializer */ NULL,
                implicit_default_constructor, decl_context,
                filename, line,
                &nodecl_ctor_initializer);

        // nodecl_t nodecl_default_ctor = nodecl_null();
        // compute_code_for_default_constructor(class_type, &nodecl_default_ctor,
        //         implicit_default_constructor, filename, line);
        // *nodecl_output = nodecl_append_to_list(*nodecl_output, nodecl_default_ctor);

        char has_virtual_bases = (virtual_base_classes != NULL);

        char has_bases_with_non_trivial_constructors = 0;

        // Now figure out if it is trivial
        scope_entry_list_iterator_t* it0 = NULL;
        for (it0 = entry_list_iterator_begin(direct_base_classes);
                !entry_list_iterator_end(it0) && !has_bases_with_non_trivial_constructors;
                entry_list_iterator_next(it0))
        {
            scope_entry_t* base_class = entry_list_iterator_current(it0);

            type_t* base_class_type = get_actual_class_type(base_class->type_information);

            scope_entry_list_t* base_constructors = class_type_get_constructors(base_class_type);
            scope_entry_list_iterator_t* it = NULL;
            for (it = entry_list_iterator_begin(base_constructors);
                    !entry_list_iterator_end(it);
                    entry_list_iterator_next(it))
            {
                scope_entry_t* current_constructor 
                    = entry_list_iterator_current(it);

                has_bases_with_non_trivial_constructors
                    |= !current_constructor->entity_specs.is_trivial;
            }
            entry_list_iterator_free(it);
            entry_list_free(base_constructors);
        }
        entry_list_iterator_free(it0);

        char has_nonstatic_data_member_with_no_trivial_constructor = 0;

        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it) 
                && !has_nonstatic_data_member_with_no_trivial_constructor;
                entry_list_iterator_next(it))
        {
            scope_entry_t *data_member = entry_list_iterator_current(it);

            if (is_class_type(data_member->type_information)
                    || (is_array_type(data_member->type_information)
                        && is_class_type(array_type_get_element_type(data_member->type_information))))
            {
                type_t* member_class_type = data_member->type_information;
                if (is_array_type(data_member->type_information))
                {
                    member_class_type = array_type_get_element_type(member_class_type);
                }

                type_t* member_actual_class_type = get_actual_class_type(member_class_type);

                scope_entry_t* default_constructor 
                    = class_type_get_default_constructor(member_actual_class_type);

                ERROR_CONDITION(default_constructor == NULL, "Invalid class", 0);

                has_nonstatic_data_member_with_no_trivial_constructor 
                    |= !default_constructor->entity_specs.is_trivial;
            }
        }
        entry_list_iterator_free(it);

        // After all these tests we can state that this constructor is
        // trivial
        if (!has_virtual_bases
                && !has_bases_with_non_trivial_constructors
                && !has_virtual_functions
                && !has_nonstatic_data_member_with_no_trivial_constructor)
        {
            implicit_default_constructor->entity_specs.is_trivial = 1;
        }

        implicit_default_constructor->entity_specs.is_non_emitted = 1;
        implicit_default_constructor->entity_specs.emission_handler = emit_implicit_default_constructor;
    }


    scope_entry_list_t* copy_constructors = class_type_get_copy_constructors(class_type);
    char no_copy_constructors = (copy_constructors == NULL);
    entry_list_free(copy_constructors);

    // Copy constructors 
    if (no_copy_constructors)
    {
        char const_parameter = 1; 
        // Now check bases for a const qualified version
        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(all_bases);
                !entry_list_iterator_end(it) && const_parameter;
                entry_list_iterator_next(it))
        {
            scope_entry_t *base_class = entry_list_iterator_current(it);

            type_t* base_class_type = get_actual_class_type(base_class->type_information);

            const_parameter = const_parameter && 
                class_has_const_copy_constructor(base_class_type);

        }
        entry_list_iterator_free(it);

        // Now check my nonstatic members that are classes (or arrays to classes)
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it) && const_parameter;
                entry_list_iterator_next(it))
        {
            scope_entry_t *data_member = entry_list_iterator_current(it);

            if (is_class_type(data_member->type_information)
                    || (is_array_type(data_member->type_information)
                        && is_class_type(array_type_get_element_type(data_member->type_information))))
            {
                type_t* member_class_type = data_member->type_information;
                if (is_array_type(data_member->type_information))
                {
                    member_class_type = array_type_get_element_type(member_class_type);
                }

                type_t* member_actual_class_type = get_actual_class_type(member_class_type);

                if (is_named_class_type(member_actual_class_type))
                {
                    const_parameter = const_parameter &&
                        class_has_const_copy_constructor(member_actual_class_type);
                }
            }
        }
        entry_list_iterator_free(it);

        parameter_info_t parameter_info[1];
        memset(parameter_info, 0, sizeof(parameter_info));
        parameter_info[0].is_ellipsis = 0;

        if (const_parameter)
        {
            parameter_info[0].type_info = 
                get_lvalue_reference_type(get_const_qualified_type(type_info));
        }
        else
        {
            parameter_info[0].type_info = get_lvalue_reference_type(type_info);
        }

        type_t* copy_constructor_type = get_new_function_type(
                NULL, // Constructors do not return anything
                parameter_info,
                1);

        scope_t* sc = class_type_get_inner_context(class_type).current_scope;

        char constructor_name[256] = { 0 };
        if (is_named_class_type(type_info))
        {
            snprintf(constructor_name, 256, "constructor %s", named_type_get_symbol(type_info)->symbol_name);
        }
        else
        {
            snprintf(constructor_name, 256, "%s", "constructor ");
        }

        scope_entry_t* implicit_copy_constructor = new_symbol(class_type_get_inner_context(class_type), sc,
                constructor_name);

        implicit_copy_constructor->kind = SK_FUNCTION;
        implicit_copy_constructor->entity_specs.is_member = 1;
        implicit_copy_constructor->entity_specs.access = AS_PUBLIC;
        implicit_copy_constructor->entity_specs.class_type = type_info;
        implicit_copy_constructor->entity_specs.is_constructor = 1;
        implicit_copy_constructor->entity_specs.is_copy_constructor = 1;
        implicit_copy_constructor->entity_specs.is_conversor_constructor = 1;
        implicit_copy_constructor->entity_specs.is_inline = 1;

        implicit_copy_constructor->type_information = copy_constructor_type;

        implicit_copy_constructor->defined = 1;

        implicit_copy_constructor->entity_specs.num_parameters = 1;
        implicit_copy_constructor->entity_specs.default_argument_info = empty_default_argument_info(1);

        class_type_add_member(class_type, implicit_copy_constructor);

        // We have to see whether this copy constructor is trivial
        char has_virtual_bases = (virtual_base_classes != NULL);

        char has_bases_with_no_trivial_copy_constructor = 0;

        // Now figure out if it is trivial
        for (it = entry_list_iterator_begin(direct_base_classes);
                !entry_list_iterator_end(it)
                && !has_virtual_bases
                && !has_bases_with_no_trivial_copy_constructor; 
                entry_list_iterator_next(it))
        {
            scope_entry_t *base_class = entry_list_iterator_current(it);

            type_t* base_class_type = get_actual_class_type(base_class->type_information);

            scope_entry_list_t* base_copy_constructors = class_type_get_copy_constructors(base_class_type);
            scope_entry_list_iterator_t* it2 = NULL;
            for (it2 = entry_list_iterator_begin(base_copy_constructors);
                    !entry_list_iterator_end(it2) 
                    && !has_virtual_bases 
                    && !has_bases_with_no_trivial_copy_constructor; 
                    entry_list_iterator_next(it2))
            {
                scope_entry_t* copy_constructor = entry_list_iterator_current(it2);

                has_bases_with_no_trivial_copy_constructor
                    |= !copy_constructor->entity_specs.is_trivial;
            }
            entry_list_iterator_free(it2);
            entry_list_free(base_copy_constructors);
        }
        entry_list_iterator_free(it);

        char has_nonstatic_data_member_with_no_trivial_copy_constructor = 0;

        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it) && !has_nonstatic_data_member_with_no_trivial_copy_constructor;
                entry_list_iterator_next(it))
        {
            scope_entry_t *data_member = entry_list_iterator_current(it);

            if (is_class_type(data_member->type_information)
                    || (is_array_type(data_member->type_information)
                        && is_class_type(array_type_get_element_type(data_member->type_information))))
            {
                type_t* member_class_type = data_member->type_information;
                if (is_array_type(data_member->type_information))
                {
                    member_class_type = array_type_get_element_type(member_class_type);
                }

                type_t* member_actual_class_type = get_actual_class_type(member_class_type);


                scope_entry_list_t* member_copy_constructors = class_type_get_copy_constructors(member_actual_class_type);
                scope_entry_list_iterator_t* it2 = NULL;
                for (it2 = entry_list_iterator_begin(member_copy_constructors);
                        !entry_list_iterator_end(it2) && !has_nonstatic_data_member_with_no_trivial_copy_constructor;
                        entry_list_iterator_next(it2))
                {
                    scope_entry_t* copy_constructor = entry_list_iterator_current(it2);

                    has_nonstatic_data_member_with_no_trivial_copy_constructor 
                        |= !copy_constructor->entity_specs.is_trivial;
                }
                entry_list_iterator_free(it2);
                entry_list_free(member_copy_constructors);
            }
        }
        entry_list_iterator_free(it);

        // It is trivial
        if (!has_virtual_bases
                && !has_bases_with_no_trivial_copy_constructor
                && !has_virtual_functions
                && !has_nonstatic_data_member_with_no_trivial_copy_constructor)
        {
            implicit_copy_constructor->entity_specs.is_trivial = 1;
        }

        implicit_copy_constructor->entity_specs.is_non_emitted = 1;
        implicit_copy_constructor->entity_specs.emission_handler = emit_implicit_copy_constructor;
    }

    // Copy assignment operators
    scope_entry_list_t* copy_asssignment_operators = class_type_get_copy_assignment_operators(class_type);
    char no_copy_assignment_operators = (copy_asssignment_operators == NULL);
    entry_list_free(copy_asssignment_operators);

    if (no_copy_assignment_operators)
    {
        char const_parameter = 1; 
        // Now check bases for a const qualified version
        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(all_bases);
                !entry_list_iterator_end(it) && const_parameter;
                entry_list_iterator_next(it))
        {
            scope_entry_t *base_class = entry_list_iterator_current(it);

            type_t* base_class_type = get_actual_class_type(base_class->type_information);

            // Bases have always been instantiated
            const_parameter = const_parameter && 
                class_has_const_copy_assignment_operator(base_class_type);
        }
        entry_list_iterator_free(it);

        // Now check my nonstatic members that are classes (or arrays to classes)
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it) && const_parameter;
                entry_list_iterator_next(it))
        {
            scope_entry_t *data_member = entry_list_iterator_current(it);

            if (is_class_type(data_member->type_information)
                    || (is_array_type(data_member->type_information)
                        && is_class_type(array_type_get_element_type(data_member->type_information))))
            {
                type_t* member_class_type = data_member->type_information;
                if (is_array_type(data_member->type_information))
                {
                    member_class_type = array_type_get_element_type(member_class_type);
                }

                type_t* member_actual_class_type = get_actual_class_type(member_class_type);

                const_parameter = const_parameter &&
                    class_has_const_copy_assignment_operator(member_actual_class_type);
            }
        }
        entry_list_iterator_free(it);

        parameter_info_t parameter_info[1];
        memset(parameter_info, 0, sizeof(parameter_info));
        parameter_info[0].is_ellipsis = 0;

        if (const_parameter)
        {
            parameter_info[0].type_info = 
                get_lvalue_reference_type(get_const_qualified_type(type_info));
        }
        else
        {
            parameter_info[0].type_info = get_lvalue_reference_type(type_info);
        }

        type_t* copy_assignment_type = get_new_function_type(
                /* returns T& */ get_lvalue_reference_type(type_info), 
                parameter_info,
                1);

        scope_t* sc = class_type_get_inner_context(class_type).current_scope;
        scope_entry_t* implicit_copy_assignment_function = new_symbol(class_type_get_inner_context(class_type), sc,
                STR_OPERATOR_ASSIGNMENT);

        implicit_copy_assignment_function->kind = SK_FUNCTION;
        implicit_copy_assignment_function->entity_specs.is_member = 1;
        implicit_copy_assignment_function->entity_specs.access = AS_PUBLIC;
        implicit_copy_assignment_function->entity_specs.class_type = type_info;
        implicit_copy_assignment_function->entity_specs.is_inline = 1;

        implicit_copy_assignment_function->type_information = copy_assignment_type;

        implicit_copy_assignment_function->defined = 1;

        implicit_copy_assignment_function->entity_specs.num_parameters = 1;
        implicit_copy_assignment_function->entity_specs.default_argument_info = empty_default_argument_info(1);

        implicit_copy_assignment_function->entity_specs.is_copy_assignment_operator = 1;

        class_type_add_member(class_type, implicit_copy_assignment_function);

        // Now check whether it is trivial
        char has_virtual_bases = (virtual_base_classes != NULL);

        char has_base_classes_with_no_trivial_copy_assignment = 0;

        for (it = entry_list_iterator_begin(direct_base_classes);
                !entry_list_iterator_end(it)
                && !has_virtual_bases
                && !has_base_classes_with_no_trivial_copy_assignment; 
                entry_list_iterator_next(it))
        {
            scope_entry_t* base_class = entry_list_iterator_current(it);

            type_t* base_class_type = get_actual_class_type(base_class->type_information);

            scope_entry_list_t* base_copy_assignment_operators = class_type_get_copy_assignment_operators(base_class_type);
            scope_entry_list_iterator_t* it2 = NULL;
            for (it2 = entry_list_iterator_begin(base_copy_assignment_operators);
                    !entry_list_iterator_end(it2)
                    && !has_virtual_bases
                    && !has_base_classes_with_no_trivial_copy_assignment;  
                    entry_list_iterator_next(it2))
            {
                scope_entry_t* copy_assignment_op = entry_list_iterator_current(it2);

                has_base_classes_with_no_trivial_copy_assignment |= 
                    !copy_assignment_op->entity_specs.is_trivial;
            }
            entry_list_iterator_free(it2);
            entry_list_free(base_copy_assignment_operators);
        }

        char has_nonstatic_data_member_with_no_trivial_copy_assignment = 0;

        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it) 
                && !has_nonstatic_data_member_with_no_trivial_copy_assignment;
                entry_list_iterator_next(it))
        {
            scope_entry_t *data_member = entry_list_iterator_current(it);

            if (is_class_type(data_member->type_information)
                    || (is_array_type(data_member->type_information)
                        && is_class_type(array_type_get_element_type(data_member->type_information))))
            {
                type_t* member_class_type = data_member->type_information;
                if (is_array_type(data_member->type_information))
                {
                    member_class_type = array_type_get_element_type(member_class_type);
                }

                type_t* member_actual_class_type = get_actual_class_type(member_class_type);

                scope_entry_list_t* member_copy_assignment_operators = class_type_get_copy_assignment_operators(member_actual_class_type);
                scope_entry_list_iterator_t* it2 = NULL;
                for (it2 = entry_list_iterator_begin(member_copy_assignment_operators);
                        !entry_list_iterator_end(it2)
                        && !has_nonstatic_data_member_with_no_trivial_copy_assignment; 
                        entry_list_iterator_next(it2))
                {
                    scope_entry_t* copy_assignment = entry_list_iterator_current(it2);

                    has_nonstatic_data_member_with_no_trivial_copy_assignment 
                        |= !copy_assignment->entity_specs.is_trivial;
                }
                entry_list_iterator_free(it2);
                entry_list_free(member_copy_assignment_operators);
            }
        }
        entry_list_iterator_free(it);

        // It is trivial
        if (!has_virtual_bases
                && !has_base_classes_with_no_trivial_copy_assignment
                && !has_virtual_functions
                && !has_nonstatic_data_member_with_no_trivial_copy_assignment)
        {
            implicit_copy_assignment_function->entity_specs.is_trivial = 1;
        }

        implicit_copy_assignment_function->entity_specs.is_non_emitted = 1;
        implicit_copy_assignment_function->entity_specs.emission_handler = emit_implicit_copy_assignment_operator;
    }

    // Implicit destructor
    if (class_type_get_destructor(class_type) == NULL)
    {
        char destructor_name[256] = { 0 };
        if (is_named_class_type(type_info))
        {
            snprintf(destructor_name, 255, "~%s", named_type_get_symbol(type_info)->symbol_name);
        }
        else
        {
            snprintf(destructor_name, 255, "%s", "~destructor");
        }
        destructor_name[255] = '\0';

        scope_t* sc = class_type_get_inner_context(class_type).current_scope;

        scope_entry_t* implicit_destructor = new_symbol(class_type_get_inner_context(class_type), sc,
                destructor_name);

        type_t* destructor_type = get_const_qualified_type(
                get_new_function_type(
                    /* returns void */ get_void_type(), 
                    NULL, 0)
                );

        implicit_destructor->kind = SK_FUNCTION;
        implicit_destructor->type_information = destructor_type;
        implicit_destructor->entity_specs.is_member = 1;
        implicit_destructor->entity_specs.access = AS_PUBLIC;
        implicit_destructor->entity_specs.is_destructor = 1;
        implicit_destructor->entity_specs.class_type = type_info;
        implicit_destructor->entity_specs.is_inline = 1;
        implicit_destructor->defined = 1;

        implicit_destructor->entity_specs.num_parameters = 0;
        class_type_add_member(class_type, implicit_destructor);
        class_type_set_destructor(class_type, implicit_destructor);
        if (is_virtual_destructor(class_type))
        {
            implicit_destructor->entity_specs.is_virtual = 1;
        }

        // Let's see whether it is trivial
        char base_has_nontrivial_destructor = 0;
        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(all_bases);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t *base_class = entry_list_iterator_current(it);

            scope_entry_t* destructor 
                = class_type_get_destructor(get_actual_class_type(base_class->type_information));

            ERROR_CONDITION(destructor == NULL, "Invalid class without destructor!\n", 0);

            base_has_nontrivial_destructor |= !destructor->entity_specs.is_trivial;
        }
        entry_list_iterator_free(it);

        char has_nonstatic_data_member_with_no_trivial_destructor = 0;

        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t *data_member = entry_list_iterator_current(it);
            if (is_class_type(data_member->type_information)
                    || (is_array_type(data_member->type_information)
                        && is_class_type(array_type_get_element_type(data_member->type_information))))
            {
                type_t* member_class_type = data_member->type_information;
                if (is_array_type(data_member->type_information))
                {
                    member_class_type = array_type_get_element_type(member_class_type);
                }

                type_t* member_actual_class_type = get_actual_class_type(member_class_type);

                scope_entry_t* destructor
                    = class_type_get_destructor(
                            get_actual_class_type(member_actual_class_type));

                ERROR_CONDITION(destructor == NULL, "Invalid class without destructor!\n", 0);

                has_nonstatic_data_member_with_no_trivial_destructor
                    |= !destructor->entity_specs.is_trivial;
            }
        }
        entry_list_iterator_free(it);

        // It is a trivial destructor
        if (!base_has_nontrivial_destructor
                && !has_nonstatic_data_member_with_no_trivial_destructor)
        {
            implicit_destructor->entity_specs.is_trivial = 1;
        }

        implicit_destructor->entity_specs.is_non_emitted = 1;
        implicit_destructor->entity_specs.emission_handler = emit_implicit_destructor;
    }

    entry_list_free(all_bases);
    entry_list_free(nonstatic_data_members);
    entry_list_free(direct_base_classes);
    entry_list_free(virtual_base_classes);

    DEBUG_CODE()
    {
        fprintf(stderr, "BUILDSCOPE: Ended class finalization\n");
    }
}

void finish_class_type(type_t* class_type, type_t* type_info, decl_context_t decl_context,
        const char *filename, int line,
        nodecl_t* nodecl_output)
{
    CXX_LANGUAGE()
    {
        finish_class_type_cxx(class_type, type_info, decl_context, filename, line, nodecl_output);
    }
}


struct delayed_function_tag
{
    AST function_def_tree;
    scope_entry_t* entry;
    decl_context_t decl_context;
    char is_template;
    char is_explicit_instantiation;
};

static int _next_delayed_function = 0;
static struct delayed_function_tag _max_delayed_functions[MCXX_MAX_FUNCTIONS_PER_CLASS];

static void build_scope_delayed_add_delayed_function_def(AST function_def_tree, 
        scope_entry_t* entry,
        decl_context_t decl_context,
        char is_template,
        char is_explicit_instantiation)
{
    ERROR_CONDITION(_next_delayed_function == MCXX_MAX_FUNCTIONS_PER_CLASS,
            "Too many delayed member functions!\n", 0);

    DEBUG_CODE()
    {
        fprintf(stderr, "BUILDSCOPE: Adding '%s' function definition for delayed processing\n",
                ast_location(function_def_tree));
    }

    _max_delayed_functions[_next_delayed_function].function_def_tree = function_def_tree;
    _max_delayed_functions[_next_delayed_function].entry = entry;
    _max_delayed_functions[_next_delayed_function].decl_context = decl_context;
    _max_delayed_functions[_next_delayed_function].is_template = is_template;
    _max_delayed_functions[_next_delayed_function].is_explicit_instantiation = is_explicit_instantiation;
    _next_delayed_function++;
}

static void build_scope_delayed_clear_pending(void)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "BUILDSCOPE: Clearing pending functions\n");
    }
    _next_delayed_function = 0;
}

static void build_scope_delayed_functions(nodecl_t* nodecl_output)
{
    int i;
    for (i = 0;  i < _next_delayed_function; i++)
    {
        struct delayed_function_tag current = _max_delayed_functions[i];

        AST function_def = current.function_def_tree;
        decl_context_t decl_context = current.decl_context;
        scope_entry_t* previous_symbol = current.entry;
        char is_template = current.is_template;
        char is_explicit_instantiation = current.is_explicit_instantiation;
        
        DEBUG_CODE()
        {
            fprintf(stderr, "=== Delayed member function definition at '%s' ===\n",
                    ast_location(function_def));
        }

        nodecl_t nodecl_funct_def = nodecl_null();
        build_scope_function_definition(function_def, previous_symbol, decl_context, 
                is_template, is_explicit_instantiation, &nodecl_funct_def);
        *nodecl_output = nodecl_concat_lists(*nodecl_output, nodecl_funct_def);
    }
    build_scope_delayed_clear_pending();
}

static int _class_specifier_nesting = 0;

void enter_class_specifier(void)
{
    _class_specifier_nesting++;
    DEBUG_CODE()
    {
        fprintf(stderr, "BUILDSCOPE: Entering class specifier. Nesting = %d->%d\n", 
                _class_specifier_nesting - 1,
                _class_specifier_nesting);
    }
}

void leave_class_specifier(nodecl_t* nodecl_output)
{
    if (_class_specifier_nesting == 1)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: Building scope of delayed functions\n");
        }
        build_scope_delayed_functions(nodecl_output);
    }
    _class_specifier_nesting--;
    ERROR_CONDITION(_class_specifier_nesting < 0,
            "This can't be negative (nesting=%d)!\n", _class_specifier_nesting);
    DEBUG_CODE()
    {
        fprintf(stderr, "BUILDSCOPE: Left class specifier. Nesting = %d->%d\n", 
                _class_specifier_nesting + 1, _class_specifier_nesting );
    }
}

/*
 * This function is called for class specifiers
 */
void gather_type_spec_from_class_specifier(AST a, type_t** type_info,
        gather_decl_spec_t* gather_info,
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    ASTAttrSetValueType(a, LANG_IS_CLASS_SPECIFIER, tl_type_t, tl_bool(1));
    /*
     * This function should strictly maintain these two variables.
     *
     * class_entry will hold the symbol associated to a class specifier with name (or template-id),
     * otherwise it will be NULL
     *
     * class_type will hold the class_type (NEVER a user defined type) to the class being declared
     *
     * if class_entry is not NULL then class_entry->type_information == class_type
     *
     * *type_info must be computed as follows:
     *   if (class_entry == NULL)
     *      *type_info = class_type
     *   else
     *      *type_info = get_user_defined_type(class_entry);
     *
     *  Note: build_scope_member_specification must receive *type_info, not class_type
     */

    if (gather_info->is_friend
            && gather_info->no_declarators)
    {
        running_error("%s: error: friend applied to class-definition\n",
                ast_location(a));
    }

    scope_entry_t* class_entry = NULL;
    type_t* class_type = NULL;
    /* --- */

    enter_class_specifier();

    AST class_head = ASTSon0(a);

    AST class_key = ASTSon0(class_head);
    AST class_id_expression = ASTSon1(class_head);
    AST base_clause = ASTSon2(class_head);

    enum class_kind_t class_kind = CK_INVALID;
    const char *class_kind_name = NULL;

    char members_belong_to_enclosing = 0;

    switch (ASTType(class_key))
    {
        case AST_CLASS_KEY_CLASS:
            {
                class_kind = CK_CLASS;
                class_kind_name = "class";
                break;
            }
        case AST_CLASS_KEY_STRUCT:
            {
                class_kind = CK_STRUCT;
                class_kind_name = "struct";
                break;
            }
        case AST_CLASS_KEY_UNION:
            {
                class_kind = CK_UNION;
                class_kind_name = "union";

                break;
            }
        default:
            internal_error("Code unreachable", 0);
    }

    decl_context_t inner_decl_context;
    // Empty it
    memset(&inner_decl_context, 0, sizeof(inner_decl_context));

    if (class_id_expression != NULL)
    {
        // If the class has name, register it in the symbol table but only if
        // it does not exist
        scope_entry_list_t* class_entry_list = NULL;

        // Look up the symbol
        CXX_LANGUAGE()
        {
            if (is_unqualified_id_expression(class_id_expression))
            {
                class_entry_list = query_in_scope(decl_context,
                        class_id_expression);
            }
            else
            {
                // If the template specialization was already declared
                // we want to update its template arguments properly
                class_entry_list = query_id_expression(decl_context,
                        class_id_expression);
            }
        }

        C_LANGUAGE()
        {
            // This can only be an AST_SYMBOL in C
            const char* class_name = ASTText(class_id_expression);
            class_name = strappend(class_kind_name, strappend(" ", class_name));

            class_entry_list = query_unqualified_name_str(decl_context, class_name);
        }

        enum cxx_symbol_kind filter_classes[] = 
        {
            SK_CLASS, 
            SK_TEMPLATE, // For template-names
        };

        scope_entry_list_t* filtered_class_entry_list = filter_symbol_kind_set(class_entry_list, 
                STATIC_ARRAY_LENGTH(filter_classes), filter_classes);

        entry_list_free(class_entry_list);

        if (filtered_class_entry_list != NULL)
        {
            // If a valid class was found
            // Get the class entry
            class_entry = entry_list_head(filtered_class_entry_list);

            entry_list_free(filtered_class_entry_list);

            class_type = class_entry->type_information;

            // If this is the primary template, we will get the template type.
            // Ask for the real primary type
            if (class_entry->kind == SK_TEMPLATE
                    && ASTType(class_id_expression) != AST_TEMPLATE_ID)
            {
                if (decl_context.template_parameters == NULL)
                {
                    running_error("%s: error: template parameters required for declaration of '%s'\n",
                            ast_location(class_id_expression),
                            get_qualified_symbol_name(class_entry, decl_context));

                }
                if (decl_context.template_parameters->num_parameters
                        != template_type_get_template_parameters(class_entry->type_information)->num_parameters)
                {
                    running_error("%s: error: redeclaration with %d template parameters while previous declaration used %d\n",
                            ast_location(class_id_expression),
                            decl_context.template_parameters->num_parameters,
                            template_type_get_template_parameters(class_entry->type_information)->num_parameters);
                }

                template_type_update_template_parameters(class_entry->type_information,
                        decl_context.template_parameters);

                // If it was friend-declared, it is not anymore
                if (class_entry->entity_specs.is_friend_declared)
                    class_entry->entity_specs.is_friend_declared = 0;

                // This is a named type
                type_t* primary_type = template_type_get_primary_type(class_entry->type_information);
                class_entry = named_type_get_symbol(primary_type);
                class_type = class_entry->type_information;

                ERROR_CONDITION((class_entry->kind != SK_CLASS
                            && class_entry->kind != SK_FUNCTION), 
                        "Invalid symbol type for a template entity\n", 0);

                if (class_entry->kind == SK_FUNCTION)
                {
                    running_error("%s: error: invalid template-name redeclaration\n", 
                            ast_location(class_id_expression));
                }

                ERROR_CONDITION(!is_class_type(class_entry->type_information), "This must be a class type", 0);
            }
            else if (class_entry->kind == SK_CLASS
                    && !is_template_specialized_type(class_entry->type_information)
                    && gather_info->is_template)
            {
                running_error("%s: error: '%s' is not a template type\n", 
                        ast_location(class_id_expression),
                        get_qualified_symbol_name(class_entry, decl_context));
            }

            // If it was friend-declared, it is not anymore
            if (class_entry->entity_specs.is_friend_declared)
                class_entry->entity_specs.is_friend_declared = 0;

            DEBUG_CODE()
            {
                fprintf(stderr, "Class '%s' already declared as %p in scope %p (%s:%d)\n", 
                        prettyprint_in_buffer(class_id_expression),
                        class_entry, 
                        class_entry->decl_context.current_scope,
                        class_entry->file,
                        class_entry->line);
            }

            if (class_entry->defined)
            {
                running_error("%s: class '%s' already defined in %s:%d\n",
                        ast_location(class_id_expression),
                        get_qualified_symbol_name(class_entry, class_entry->decl_context),
                        class_entry->file,
                        class_entry->line);
            }

            if (is_template_specialized_type(class_entry->type_information))
            {
                // Check the enclosing namespace scope
                // This is only valid if the scope of the entry is an inlined namespace of the current one
                if ((class_entry->decl_context.namespace_scope != decl_context.namespace_scope)
                        && !is_inline_namespace_of(class_entry->decl_context, decl_context))
                {
                    running_error("%s: specialization of '%s' in different namespace from definition\n",
                            ast_location(class_id_expression),
                            prettyprint_in_buffer(class_id_expression));
                }

                // Update the template parameters only if they are not empty
                // (this may happen with explicit instantiations)
                if (decl_context.template_parameters != NULL)
                {
                    template_specialized_type_update_template_parameters(class_entry->type_information,
                            decl_context.template_parameters);
                }
            }

            // Update the template_scope
            DEBUG_CODE()
            {
                fprintf(stderr, "Updating template scope\n");
            }
            class_entry->decl_context.template_parameters = decl_context.template_parameters;

            // Update point of declaration
            class_entry->point_of_declaration = get_enclosing_declaration(class_id_expression);

            inner_decl_context = new_class_context(class_entry->decl_context, class_entry);
            class_type_set_inner_context(class_type, inner_decl_context);
        }
        else if (filtered_class_entry_list == NULL
                && is_unqualified_id_expression(class_id_expression))
        {
            // If no class found and no nested name, the symbol must be created
            // here
            if (ASTType(class_id_expression) == AST_SYMBOL)
            {
                C_LANGUAGE()
                {
                    const char* class_name = ASTText(class_id_expression);
                    class_name = strappend(class_kind_name, strappend(" ", class_name));

                    class_entry = new_symbol(decl_context, 
                            decl_context.current_scope, class_name);
                }

                CXX_LANGUAGE()
                {
                    class_entry = new_symbol(decl_context, 
                            decl_context.current_scope, 
                            ASTText(class_id_expression));
                }
            }
            else if (ASTType(class_id_expression) == AST_TEMPLATE_ID)
            {
                running_error("%s: error: template class-name '%s' not found in the current scope\n",
                        ast_location(class_id_expression),
                        prettyprint_in_buffer(ASTSon0(class_id_expression)));
            }
            else
            {
                internal_error("Code unreachable", 0);
            }

            DEBUG_CODE()
            {
                fprintf(stderr, "Registering class '%s' (%p) in scope %p\n", 
                        prettyprint_in_buffer(class_id_expression), class_entry, decl_context.current_scope);
            }

            class_entry->line = ASTLine(class_id_expression);
            class_entry->file = ASTFileName(class_id_expression);
            class_entry->point_of_declaration = get_enclosing_declaration(class_id_expression);

            // Create the class type for this newly created class
            if (!gather_info->is_template)
            {
                // Normal, non-template class
                class_entry->kind = SK_CLASS;
                class_entry->type_information = get_new_class_type(decl_context, class_kind);
                class_type = class_entry->type_information;
            }
            else 
            {
                if (ASTType(class_id_expression) != AST_TEMPLATE_ID)
                {
                    class_entry->kind = SK_TEMPLATE;
                    class_entry->type_information = get_new_template_type(decl_context.template_parameters, 
                            get_new_class_type(decl_context, class_kind),
                            ASTText(class_id_expression), decl_context,
                            ASTLine(class_id_expression), 
                            ASTFileName(class_id_expression));
                    template_type_set_related_symbol(class_entry->type_information, class_entry);

                    class_entry->file = ASTFileName(class_id_expression);
                    class_entry->line = ASTLine(class_id_expression);
                    class_entry->point_of_declaration = get_enclosing_declaration(class_id_expression);

                    // Set it as a member if needed
                    if (decl_context.current_scope->kind == CLASS_SCOPE)
                    {
                        class_entry->entity_specs.is_member = 1;
                        // FIXME
                        // class_entry->entity_specs.access = current_access;
                        class_entry->entity_specs.class_type = 
                            get_user_defined_type(decl_context.current_scope->related_entry);
                    }

                    // Now update class_entry to be a real class
                    class_entry = named_type_get_symbol(
                            template_type_get_primary_type(class_entry->type_information)
                            );
                    // Update some fields
                    class_entry->file = ASTFileName(class_id_expression);
                    class_entry->line = ASTLine(class_id_expression);
                    class_entry->point_of_declaration = get_enclosing_declaration(class_id_expression);

                    class_type = class_entry->type_information;
                }
                else
                {
                    running_error("%s: error: invalid template-name '%s'\n", 
                            ast_location(class_id_expression),
                            prettyprint_in_buffer(class_id_expression));
                }
            }

            inner_decl_context = new_class_context(decl_context, class_entry);
            class_type_set_inner_context(class_type, inner_decl_context);
        }
        else
        {
            // Other cases are error, not finding a nested class means that
            // something was amiss
            //
            // struct A::B <-- if 'A::B' is not found it means that there is an error
            // {
            // };
            running_error("%s: error: class '%s' not found\n",
                    ast_location(a), prettyprint_in_buffer(class_id_expression));
        }
    }
    else
    {
        // Give it a fake name
        static int anonymous_classes = 0;
        char c[256];
        C_LANGUAGE()
        {
            snprintf(c, 255, "%s __mcc_%s_anon_%d", class_kind_name, class_kind_name, anonymous_classes);
        }
        CXX_LANGUAGE()
        {
            snprintf(c, 255, "__mcc_%s_anon_%d", class_kind_name, anonymous_classes);
        }
        c[255] = '\0';
        anonymous_classes++;

        class_entry = new_symbol(decl_context, 
                decl_context.current_scope, 
                uniquestr(c));

        class_entry->kind = SK_CLASS;
        class_entry->type_information = get_new_class_type(decl_context, class_kind);
        class_type = class_entry->type_information;

        class_entry->point_of_declaration = get_enclosing_declaration(a);
        class_entry->line = ASTLine(a);
        class_entry->file = ASTFileName(a);

        class_entry->entity_specs.is_anonymous = 1;

        class_type = class_entry->type_information;
        inner_decl_context = new_class_context(decl_context, class_entry);
        class_type_set_inner_context(class_type, inner_decl_context);

        C_LANGUAGE()
        {
            members_belong_to_enclosing = (gather_info->no_declarators
                    // Namespace scope is not allowed in C
                    && decl_context.current_scope->kind != NAMESPACE_SCOPE);
        }
        CXX_LANGUAGE()
        {
            members_belong_to_enclosing = (class_kind == CK_UNION
                    && gather_info->no_declarators);
        }
    }

    ERROR_CONDITION(inner_decl_context.current_scope == NULL,
            "The inner context was incorrectly set", 0);


    gather_info->defined_type = class_entry;
    
    // Compute *type_info as it is needed by build_scope_member_specification
    *type_info = get_user_defined_type(class_entry);

    // Save the class symbol
    ASTAttrSetValueType(a, LANG_CLASS_SPECIFIER_SYMBOL, tl_type_t, tl_symbol(class_entry));

    // If the class is being declared in class-scope it means
    // it is a nested class
    if (decl_context.current_scope->kind == CLASS_SCOPE)
    {
        // If the enclosing class is dependent, so is this one
        char c = is_dependent_type(class_type);
        type_t* enclosing_class_type = decl_context.current_scope->related_entry->type_information;
        c = c || is_dependent_type(enclosing_class_type);
        set_is_dependent_type(class_type, c);

        class_type_set_enclosing_class_type(class_type, enclosing_class_type);
    }
    else if (decl_context.current_scope->kind == BLOCK_SCOPE)
    {
        // This is a local class
        scope_entry_t* enclosing_function = decl_context.current_scope->related_entry;
        if (is_dependent_type(enclosing_function->type_information))
        {
            set_is_dependent_type(class_type, 1);
        }
    }

    // The inner scope is properly adjusted here thus we can link it with the AST
    scope_link_set(CURRENT_COMPILED_FILE->scope_link, a, inner_decl_context);

    // Build scope of members
    AST member_specification = ASTSon1(a);

    // Now add the bases
    if (base_clause != NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Adding the bases of this class\n");
        }

        build_scope_base_clause(base_clause, 
                class_type, 
                inner_decl_context);

        DEBUG_CODE()
        {
            fprintf(stderr, "Bases added\n");
        }
    }

    // Inject the class symbol in the scope
    CXX_LANGUAGE()
    {
        scope_entry_t* injected_symbol = new_symbol(inner_decl_context, 
                inner_decl_context.current_scope, 
                class_entry->symbol_name);

        *injected_symbol = *class_entry;
        injected_symbol->do_not_print = 1;

        injected_symbol->entity_specs.is_member = 1;
        injected_symbol->entity_specs.access = AS_PUBLIC;
        injected_symbol->entity_specs.class_type = get_user_defined_type(class_entry);

        injected_symbol->entity_specs.is_injected_class_name = 1;
    }

    access_specifier_t current_access;
    // classes have a private by default
    if (ASTType(class_key) == AST_CLASS_KEY_CLASS)
    {
        current_access = AS_PRIVATE;
    }
    // otherwise this is public (for union and structs)
    else
    {
        current_access = AS_PUBLIC;
    }

    scope_entry_list_t* declared_symbols = NULL;
    build_scope_member_specification(inner_decl_context, member_specification, 
            current_access, *type_info, nodecl_output, &declared_symbols);

    if (members_belong_to_enclosing)
    {
        scope_entry_t* enclosing = decl_context.current_scope->related_entry;

        while (enclosing != NULL
                && enclosing->kind == SK_CLASS
                && enclosing->entity_specs.is_anonymous)
        {
            enclosing = enclosing->decl_context.current_scope->related_entry;
        }

        ERROR_CONDITION(enclosing == NULL
                || enclosing->kind != SK_CLASS,
                "Invalid nesting for anonymous nested class", 0);

        decl_context_t inner_context = class_type_get_inner_context(enclosing->type_information);

        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(declared_symbols);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* entry = entry_list_iterator_current(it);
            insert_entry(inner_context.current_scope, entry);
        }
        entry_list_iterator_free(it);
    }
    entry_list_free(declared_symbols);

    class_entry->defined = 1;
    class_entry->point_of_definition = get_enclosing_declaration(a);
    
    class_type_set_instantiation_trees(class_type, member_specification, base_clause);

    nodecl_t nodecl_finish_class = nodecl_null();
    finish_class_type(class_type, *type_info, decl_context, ASTFileName(a), ASTLine(a), &nodecl_finish_class);
    *nodecl_output = nodecl_concat_lists(*nodecl_output, nodecl_finish_class);
    set_is_complete_type(class_type, /* is_complete */ 1);
    set_is_complete_type(get_actual_class_type(class_type), /* is_complete */ 1);
    
    // DO NOT run this before setting the nature of the class or we will try
    // to instantiate independent complete classes within member functions!
    leave_class_specifier(nodecl_output);
    
    // Function definitions are built their scope delayed, once after all
    // the class scope has been seen. This is because inline function
    // definitions are no diferent when it concerns to completeness of the
    // class they belong.
    //
    // struct A
    // {
    //   struct B
    //   {
    //     int f()
    //     {
    //        // This is valid so we will delay 'A::B::f' till we end with 'A'
    //        return sizeof(A);
    //     }
    //   };
    // };
    //

    // Keep the definition tree
    class_entry->entity_specs.definition_tree = a;
    
    // Save the template parameters
    if (is_template_specialized_type(class_type))
    {
        // State this symbol has been created by the code and not by the type system
        class_entry->entity_specs.is_user_declared = 1;
    }

    ERROR_CONDITION(class_entry != NULL
            && class_type != class_entry->type_information,
            "Inconsistency between class_entry and class_type", 0);
}

void build_scope_member_specification_first_step(decl_context_t inner_decl_context,
        AST member_specification_tree,
        access_specifier_t default_current_access,
        type_t* type_info,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols)
{
    if (member_specification_tree == NULL)
    {
        return;
    }

    // Member specification
    access_specifier_t current_access = default_current_access;

    AST iter;
    for_each_element(member_specification_tree, iter)
    {
        AST member_specification = ASTSon1(iter);

        // If this is an access specifier update its related access info
        if (ASTType(member_specification) == AST_MEMBER_ACCESS_SPEC)
        {
            switch (ASTType(ASTSon0(member_specification)))
            {
                case AST_PRIVATE_SPEC : 
                    current_access = AS_PRIVATE;
                    break;
                case AST_PUBLIC_SPEC :
                    current_access = AS_PUBLIC;
                    break;
                case AST_PROTECTED_SPEC :
                    current_access = AS_PROTECTED;
                    break;
                default :
                    internal_error("Unknown node type '%s'\n", ast_print_node_type(ASTType(ASTSon0(member_specification))));
            }
        }
        else 
        {
            // This is a simple member_declaration
            build_scope_member_declaration(inner_decl_context, member_specification,
                    current_access, type_info, 
                    nodecl_output, declared_symbols);
        }
    }
}

static void build_scope_member_specification(decl_context_t inner_decl_context, AST member_specification_tree, 
        access_specifier_t default_current_access, type_t* type_info, nodecl_t *nodecl_output,
        scope_entry_list_t** declared_symbols)
{
    ERROR_CONDITION(inner_decl_context.current_scope->kind != CLASS_SCOPE,
            "Error, current scope should be a class scope", 0);

    // First pass, sign up only prototypes and simple declarations and
    // queue function definitions
    // FIXME
    build_scope_member_specification_first_step(inner_decl_context, member_specification_tree, 
            default_current_access, type_info, 
            nodecl_output, declared_symbols);
}


/*
 * This function just computes the type of a declarator
 * it does not sign in any entry
 */
void compute_declarator_type(AST a, gather_decl_spec_t* gather_info,
        type_t* type_info, type_t** declarator_type, 
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    build_scope_declarator_with_parameter_context(a,
            gather_info, type_info, declarator_type, decl_context, NULL, nodecl_output);
}

/*
 * This is the actual implementation of 'compute_declarator_type'
 */
static void build_scope_declarator_with_parameter_context(AST a, 
        gather_decl_spec_t* gather_info, type_t* type_info, type_t** declarator_type,
        decl_context_t decl_context, decl_context_t *prototype_context,
        nodecl_t* nodecl_output)
{
    *declarator_type = type_info;
    
    if (a != NULL)
    {
        // First traversal along the declarator
        // just to get all attributes
        gather_gcc_attributes_spread(a, gather_info, decl_context);
    }
    
    // Now we can update the base type because of attributes if needed
    if (gather_info->is_vector)
    {
        if (gather_info->mode_type != NULL)
        {
            //Generic Vector
            if (gather_info->vector_size == 0)
            {
                *declarator_type = get_generic_vector_type(gather_info->mode_type);
            }
            else
            {
                *declarator_type = get_vector_type(gather_info->mode_type, 
                        gather_info->vector_size);
            }
        }
        else
        {
            // We do not want a 'vector 16 to volatile float' but a 
            // 'volatile vector to 16 float'

            cv_qualifier_t cv_qualif = get_cv_qualifier(type_info);
            type_t* base_vector_type = get_unqualified_type(type_info);

            //Generic Vector
            if (gather_info->vector_size == 0)
            {
                *declarator_type = get_cv_qualified_type(
                        get_generic_vector_type(base_vector_type),
                        cv_qualif);
            }
            else
            {
                *declarator_type = get_cv_qualified_type(
                        get_vector_type(base_vector_type,
                            gather_info->vector_size), 
                        cv_qualif);
            }
        }
    }
    else if (gather_info->is_overriden_type)
    {
        if (!is_integral_type(*declarator_type)
                && !is_floating_type(*declarator_type))
        {
            error_printf("%s: error: 'mode' attribute is only valid for integral or floating types\n",
                    ast_location(a));
        }
        else
        {
            *declarator_type = gather_info->mode_type;
        }
    }

    if (a != NULL)
    {
        AST declarator_name = get_declarator_name(a, decl_context);

        decl_context_t entity_context = decl_context;
        // Adjust context if the name is qualified
        if (declarator_name != NULL)
        {
            if (ASTType(declarator_name) == AST_QUALIFIED_ID
                    || ASTType(declarator_name) == AST_QUALIFIED_TEMPLATE)
            {
                // If it is qualified it must be declared previously
                // We are not interested in anything but the context of any of the symbols

                AST global_op = ASTSon0(declarator_name);
                AST nested_name = ASTSon1(declarator_name);
                AST name = ASTSon2(declarator_name);

                decl_flags_t decl_flags = DF_NONE;

                if (BITMAP_TEST(decl_context.decl_flags, DF_CONSTRUCTOR))
                {
                    decl_flags |= DF_CONSTRUCTOR;
                }

                scope_entry_list_t* symbols = query_nested_name_flags(decl_context, 
                        global_op, nested_name, name, decl_flags);

                if (symbols == NULL)
                {
                    running_error("%s: error: qualified name '%s' not found",
                            ast_location(declarator_name),
                            prettyprint_in_buffer(declarator_name));
                }
                else
                {
                    // Update the entity context, inheriting the template_scope
                    entity_context = entry_list_head(symbols)->decl_context;
                    entity_context.template_parameters = decl_context.template_parameters;

                    if (prototype_context != NULL)
                    {
                        prototype_context->current_scope->contained_in = entry_list_head(symbols)->decl_context.current_scope;
                        prototype_context->namespace_scope = entry_list_head(symbols)->decl_context.namespace_scope;
                        prototype_context->class_scope = entry_list_head(symbols)->decl_context.class_scope;
                    }
                }

                entry_list_free(symbols);
            }

            {
                // KLUDGE
                // FIXME - Rework declarators to avoid this
                AST set_declarator = a;
                if (ASTType(ASTParent(a)) == AST_INIT_DECLARATOR
                        || ASTType(ASTParent(a)) == AST_GCC_INIT_DECLARATOR)
                {
                    set_declarator = ASTParent(a);
                }
                ASTAttrSetValueType(set_declarator, LANG_IS_DECLARED_NAME, tl_type_t, tl_bool(1));
                ast_set_link_to_child(set_declarator, LANG_DECLARED_NAME, declarator_name);
            }


            ASTAttrSetValueType(declarator_name, LANG_DECLARATOR_ID_EXPR, tl_type_t, tl_bool(1));

            scope_link_set(CURRENT_COMPILED_FILE->scope_link, a, entity_context);
        }

        // Second traversal, here we build the type
        build_scope_declarator_rec(a, declarator_type, 
                gather_info, decl_context, entity_context, prototype_context, nodecl_output);

        if (declarator_name != NULL)
        {
            // Special case for conversion function ids
            // We fix the return type according to the standard
            if (is_function_type(*declarator_type))
            {
                if (function_type_get_return_type(*declarator_type) == NULL)
                {
                    AST id_expression = declarator_name;

                    AST conversion_function_id = NULL;
                    if (ASTType(id_expression) == AST_QUALIFIED_ID)
                    {
                        if (ASTType(ASTSon2(id_expression)) == AST_CONVERSION_FUNCTION_ID)
                        {
                            conversion_function_id = ASTSon2(id_expression);
                        }
                    }

                    if (ASTType(id_expression) == AST_CONVERSION_FUNCTION_ID)
                    {
                        conversion_function_id = id_expression;
                    }

                    if (conversion_function_id != NULL)
                    {
                        // Conversion functions do not haver parameters and just return their conversion
                        cv_qualifier_t cv_qualif = get_cv_qualifier(*declarator_type);

                        type_t* conversion_function_type;
                        get_conversion_function_name(entity_context, conversion_function_id, &conversion_function_type, nodecl_output);
                        *declarator_type = get_new_function_type(conversion_function_type, 
                                /*parameter_info*/ NULL, /*num_parameters=*/0);

                        // Keep the const-qualification in the crafted type
                        if ((cv_qualif & CV_CONST) == CV_CONST)
                        {
                            *declarator_type = get_const_qualified_type(*declarator_type);
                        }
                    }
                }
                else if (ASTType(declarator_name) == AST_DESTRUCTOR_ID
                        || ASTType(declarator_name) == AST_DESTRUCTOR_TEMPLATE_ID)
                {
                    // Patch the type of the function of a destructor so it
                    // works for const objects as well
                    *declarator_type = get_const_qualified_type(*declarator_type);
                }
            }
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "Computed type of '%s' is  '%s'\n", 
                    prettyprint_in_buffer(a),
                    print_declarator(*declarator_type));
        }
    }

}

/*
 * This functions converts a type "T" to a "pointer to T"
 */
static void set_pointer_type(type_t** declarator_type, AST pointer_tree, 
        decl_context_t decl_context)
{
    type_t* pointee_type = *declarator_type;

    switch (ASTType(pointer_tree))
    {
        case AST_POINTER_SPEC :
            {
                if (ASTSon0(pointer_tree) == NULL)
                {
                    *declarator_type = get_pointer_type(pointee_type);
                }
                else
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "BUILDSCOPE: This is a pointer to member\n");
                    }

                    scope_entry_list_t* entry_list = NULL;

                    AST id_type_expr = ASTSon0(pointer_tree);

                    entry_list = query_id_expression(decl_context, id_type_expr);

                    if (entry_list != NULL)
                    {
                        *declarator_type = get_pointer_to_member_type(pointee_type, 
                                entry_list_head(entry_list));
                    }
                    else
                    {
                        running_error("%s: error: class-name '%s' not found\n", 
                                ast_location(id_type_expr),
                                prettyprint_in_buffer(id_type_expr));
                    }

                    entry_list_free(entry_list);
                }
                *declarator_type = get_cv_qualified_type(*declarator_type, 
                        compute_cv_qualifier(ASTSon1(pointer_tree)));
                break;
            }
        case AST_REFERENCE_SPEC :
            {
                if (!is_lvalue_reference_type(pointee_type)
                        && !is_rvalue_reference_type(pointee_type))
                {
                    *declarator_type = get_lvalue_reference_type(pointee_type);
                }
                else
                {
                    /* 
                     * FIXME: It is unclear what happens with qualifiers here
                     */
                    /* 
                     * Something that was a reference (either lvalue or rvalue)
                     * and is lvalue-referenced turns into rvalue referenced
                     */
                    *declarator_type = get_lvalue_reference_type(
                            reference_type_get_referenced_type(pointee_type)
                            );
                }
                break;
            }
        case AST_RVALUE_REFERENCE_SPEC :
            {
                if (!is_lvalue_reference_type(pointee_type)
                        && !is_rvalue_reference_type(pointee_type))
                {
                    *declarator_type = get_rvalue_reference_type(pointee_type);
                }
                else
                {
                    /* 
                     * FIXME: It is unclear what happens with qualifiers here
                     */
                    *declarator_type = pointee_type;
                }
                break;
            }
        case AST_GCC_REFERENCE_SPEC :
            {
                *declarator_type = get_lvalue_reference_type(pointee_type);
                *declarator_type = get_cv_qualified_type(*declarator_type, 
                        compute_cv_qualifier(ASTSon0(pointer_tree)));
                break;
            }
        default :
            internal_error("Unhandled node type '%s'\n", ast_print_node_type(ASTType(pointer_tree)));
            break;
    }
}

/*
 * This function converts a type "T" to a "array x of T"
 */
static void set_array_type(type_t** declarator_type, 
        AST constant_expr, AST static_qualifier UNUSED_PARAMETER, 
        AST cv_qualifier_seq UNUSED_PARAMETER,
        decl_context_t decl_context)
{
    type_t* element_type = *declarator_type;

    if (constant_expr != NULL)
    {
        if (!check_expression(constant_expr, decl_context))
        {
            error_printf("%s: error: could not check array size expression '%s'\n",
                    ast_location(constant_expr),
                    prettyprint_in_buffer(constant_expr));

            *declarator_type = get_error_type();
            return;
        }

        if (!expression_is_value_dependent(constant_expr)
                && !expression_is_constant(constant_expr))
        {
            if (decl_context.current_scope->kind == NAMESPACE_SCOPE
                    || decl_context.current_scope->kind == CLASS_SCOPE)
            {
                error_printf("%s: error: declaring a variable sized object in a scope not allowing them\n",
                        ast_location(constant_expr));
            }

            *declarator_type = get_error_type();
            return;
        }
    }

    *declarator_type = get_array_type(element_type, expression_get_nodecl(constant_expr), decl_context);
}

/*
 * This function fetches information for every declarator in the
 * parameter_declaration_clause of a functional declarator
 */
static void set_function_parameter_clause(type_t** function_type, 
        AST parameters, decl_context_t decl_context,
        gather_decl_spec_t* gather_info,
        nodecl_t* nodecl_output)
{
    parameter_info_t parameter_info[MCXX_MAX_FUNCTION_PARAMETERS];
    memset(parameter_info, 0, sizeof(parameter_info));
    int num_parameters = 0;

    if (ASTType(parameters) == AST_EMPTY_PARAMETER_DECLARATION_CLAUSE)
    {
        C_LANGUAGE()
        {
            // In C this is a function definition lacking a prototype
            *function_type = get_nonproto_function_type(*function_type, /*num_parameters=*/0);
        }
        // An empty parameter declaration clause is like (void) in C++
        CXX_LANGUAGE()
        {
            // In C++ this is a function with 0 parameters
            *function_type = get_new_function_type(*function_type, parameter_info, /*num_parameters=*/0);
        }
        return;
    }

    AST iter = NULL;
    AST list = parameters;
    
    // Do not contaminate the current symbol table if we are in
    // a function declaration, otherwise this should already be 
    // a block scope
    
    // Link the scope of the parameters
    scope_link_set(CURRENT_COMPILED_FILE->scope_link, parameters, decl_context);

    C_LANGUAGE()
    {
        // Nothing to do here with K&R parameters
        if (ASTType(parameters) == AST_KR_PARAMETER_LIST)
        {
            // The list is inside this wrapping tree
            list = ASTSon0(parameters);

            // Count them and create a function lacking prototype
            num_parameters = 0;
            for_each_element(list, iter)
            {
                num_parameters++;
            }

            *function_type = get_nonproto_function_type(*function_type, num_parameters);

            // Nothing else to do
            return;
        }
    }

    for_each_element(list, iter)
    {
        if (num_parameters > MCXX_MAX_FUNCTION_PARAMETERS)
        {
            running_error("%s: error: too many parameters (more than %d) in function declaration", 
                    ast_location(parameters),
                    num_parameters);
        }

        // Clear this parameter_info 
        memset(&(parameter_info[num_parameters]), 0, sizeof(parameter_info[num_parameters]));

        AST parameter_declaration = ASTSon1(iter);

        if (ASTType(parameter_declaration) == AST_AMBIGUITY)
        {
            solve_ambiguous_parameter_decl(parameter_declaration, decl_context);
            ERROR_CONDITION((ASTType(parameter_declaration) == AST_AMBIGUITY), "Ambiguity not solved %s", 
                    ast_location(parameter_declaration));
        }

        if (ASTType(parameter_declaration) == AST_VARIADIC_ARG)
        {
            // Nothing more to do
            parameter_info[num_parameters].is_ellipsis = 1;
            num_parameters++;
            continue;
        }

        ERROR_CONDITION(ASTType(parameter_declaration) != AST_PARAMETER_DECL
                && ASTType(parameter_declaration) != AST_GCC_PARAMETER_DECL, 
                "Invalid node", 0);

        ASTAttrSetValueType(parameter_declaration, LANG_IS_PARAMETER_DECLARATION, tl_type_t, tl_bool(1));

        ast_set_link_to_child(parameter_declaration, LANG_DECLARATION_SPECIFIERS, ASTSon0(parameter_declaration));
        ast_set_link_to_child(parameter_declaration, LANG_DECLARATION_DECLARATORS, ASTSon1(parameter_declaration));

        // This is never null
        AST parameter_decl_spec_seq = ASTSon0(parameter_declaration);
        // Declarator can be null
        AST parameter_declarator = ASTSon1(parameter_declaration);
        // Default value can be null
        // The scope of this parameter declaration should be "st" and not parameters_scope
        AST default_argument = ASTSon2(parameter_declaration);

        // Only check the expression if we are not in the middle
        // of an instantiation
        if (default_argument != NULL)
        {
            if (!check_expression(default_argument, decl_context))
            {
                error_printf("%s: error: could not check default argument expression '%s'\n",
                        ast_location(default_argument),
                        prettyprint_in_buffer(default_argument));

                *function_type = get_error_type();
                return;
            }
        }

        gather_decl_spec_t param_decl_gather_info;
        memset(&param_decl_gather_info, 0, sizeof(param_decl_gather_info));
        
        type_t* simple_type_info;

        decl_context_t param_decl_context = decl_context;
        param_decl_gather_info.parameter_declaration = 1;

        build_scope_decl_specifier_seq(parameter_decl_spec_seq, &param_decl_gather_info, &simple_type_info,
                param_decl_context, nodecl_output);

        if (is_error_type(simple_type_info))
        {
            *function_type = get_error_type();
            return;
        }

        if (ASTType(parameter_declaration) == AST_GCC_PARAMETER_DECL)
        {
            AST attribute_list = ASTSon3(parameter_declaration);
            gather_gcc_attribute_list(attribute_list, &param_decl_gather_info, param_decl_context);
        }

        if (param_decl_gather_info.is_extern)
        {
            running_error("%s: error: parameter declared as 'extern'\n", 
                    ast_location(parameter_decl_spec_seq));

            *function_type = get_error_type();
            return;
        }
        if (param_decl_gather_info.is_static)
        {
            running_error("%s: error: parameter declared as 'static'\n", 
                    ast_location(parameter_decl_spec_seq));
            *function_type = get_error_type();
            return;
        }

        // It is valid in a function declaration not having a declarator at all
        // (note this is different from having an abstract declarator).
        //
        // int f(int, int*);
        //
        // The first "int" does not contain any declarator while the second has
        // an abstract one

        scope_entry_t* entry = NULL;
        type_t* type_info = NULL;

        compute_declarator_type(parameter_declarator, 
                &param_decl_gather_info, simple_type_info, &type_info, param_decl_context, nodecl_output);
        if (is_error_type(type_info))
        {
            *function_type = get_error_type();
            return;
        }

        if (parameter_declarator != NULL)
        {
            entry = build_scope_declarator_name(parameter_declarator, type_info, &param_decl_gather_info, param_decl_context, nodecl_output);

            AST declarator_name = get_declarator_name(parameter_declarator, param_decl_context);
            if (declarator_name != NULL)
            {
                ASTAttrSetValueType(parameter_declaration, LANG_IS_NAMED_PARAMETER_DECLARATION, tl_type_t, tl_bool(1));
                ast_set_link_to_child(parameter_declaration, LANG_PARAMETER_DECLARATION_NAME, declarator_name);
            }
        }

        // Now normalize the types

        // First save the original type for the entry itself (but not for the function prototype)
        type_t* original_type = type_info;
        // If the original type is a typedef then we want to ignore
        // all the indirections
        type_info = advance_over_typedefs(type_info);

        // function to pointer-to-function standard conversion
        if (is_function_type(type_info))
        {
            type_info = get_pointer_type(type_info);
        }
        // Array to pointer standard conversion
        else if (is_array_type(type_info))
        {
            type_info = array_type_get_element_type(type_info);
            type_info = get_pointer_type(type_info);
        }

        if (entry != NULL)
        {
            // A parameter is always a variable entity
            entry->kind = SK_VARIABLE;
            entry->entity_specs.is_parameter = 1;
            entry->entity_specs.parameter_position = num_parameters;

            // Update the type info
            entry->type_information = type_info;
            entry->defined = 1;
        }

        parameter_info[num_parameters].is_ellipsis = 0;
        parameter_info[num_parameters].type_info = get_unqualified_type(type_info);
        parameter_info[num_parameters].nonadjusted_type_info = original_type;

        ERROR_CONDITION(num_parameters == MCXX_MAX_FUNCTION_PARAMETERS, 
                "Too many function parameters %d\n", MCXX_MAX_FUNCTION_PARAMETERS);

        gather_info->arguments_info[num_parameters].entry = entry;
        gather_info->arguments_info[num_parameters].argument = default_argument;
        gather_info->arguments_info[num_parameters].context = decl_context;

        num_parameters++;
    }

    if (parameter_info[num_parameters-1].is_ellipsis)
    {
        gather_info->num_parameters = num_parameters - 1;
    }
    else
    {
        gather_info->num_parameters = num_parameters;
    }

    if ((num_parameters == 1)
            && !parameter_info[0].is_ellipsis)
    {
        type_t* parameter_type = parameter_info[0].type_info;

        if (is_void_type(parameter_type))
        {
            // This list was really empty
            num_parameters = 0;
            gather_info->num_parameters = 0;
        }
    }

    // Now create the type
    *function_type = get_new_function_type(*function_type, parameter_info, num_parameters);
}

/*
 * This function converts a type "T" into a "function (...) returning T" type
 */
static void set_function_type(type_t** declarator_type,  
        gather_decl_spec_t* gather_info, AST parameter, AST cv_qualif_tree, AST except_spec, 
        decl_context_t decl_context, decl_context_t *p_prototype_context,
        nodecl_t* nodecl_output)
{
    decl_context_t prototype_context;
    memset(&prototype_context, 0, sizeof(prototype_context));
    if (p_prototype_context == NULL)
    {
        // Allocate one here
        prototype_context = new_prototype_context(decl_context);
    }
    else
    {
        prototype_context = *p_prototype_context;
    }

    /*
     * FIXME - Many things saved in the type actually belong to the symbol thus
     * hindering type information sharing accross symbols
     */
    set_function_parameter_clause(declarator_type, parameter, prototype_context, gather_info, nodecl_output);

    cv_qualifier_t cv_qualif = compute_cv_qualifier(cv_qualif_tree);

    *declarator_type = get_cv_qualified_type(*declarator_type, cv_qualif);

    build_exception_spec(*declarator_type, except_spec, gather_info, decl_context, nodecl_output);
}

// This function traverses the declarator tree gathering all attributes that might appear there
// We need to traverse the declarator twice because of gcc allowing attributes appear in many
// places
static void gather_gcc_attributes_spread(AST a, gather_decl_spec_t* gather_info, decl_context_t declarator_context)
{
    if (a == NULL)
        return;

    switch(ASTType(a))
    {
        case AST_DECLARATOR :
        case AST_PARENTHESIZED_DECLARATOR :
            {
                gather_gcc_attributes_spread(ASTSon0(a), gather_info, declarator_context);
                break;
            }
        case AST_POINTER_DECLARATOR :
            {
                gather_gcc_attributes_spread(ASTSon1(a), gather_info, declarator_context);
                break;
            }
        case AST_DECLARATOR_ARRAY :
            {
                gather_gcc_attributes_spread(ASTSon0(a), gather_info, declarator_context);
                break;
            }
        case AST_DECLARATOR_FUNC :
            {
                gather_gcc_attributes_spread(ASTSon0(a), gather_info, declarator_context);
                break;
            }
        case AST_DECLARATOR_ID_EXPR :
            {
                // Do nothing
                break;
            }
            // GNU extensions
            // attribute declarator
        case AST_GCC_DECLARATOR :
            {
                AST attribute_list = ASTSon0(a);
                gather_gcc_attribute_list(attribute_list, gather_info, declarator_context);

                gather_gcc_attributes_spread(ASTSon1(a), 
                        gather_info, declarator_context); 
                break;
            }
            // attribute * declarator
            // attribute & declarator
        case AST_GCC_POINTER_DECLARATOR :
            {
                AST attribute_list = ASTSon0(a);
                gather_gcc_attribute_list(attribute_list, gather_info, declarator_context);

                gather_gcc_attributes_spread(ASTSon2(a), 
                        gather_info, declarator_context);
                break;
            }
            // functional-declarator attribute
        case AST_GCC_FUNCTIONAL_DECLARATOR :
            {
                AST attribute_list = ASTSon1(a);
                gather_gcc_attribute_list(attribute_list, gather_info, declarator_context);

                gather_gcc_attributes_spread(ASTSon0(a), 
                        gather_info, declarator_context);
                break;
            }
        case AST_AMBIGUITY :
            {
                solve_ambiguous_declarator(a, declarator_context);
                // Restart function
                gather_gcc_attributes_spread(a, gather_info, declarator_context);
                break;
            }
        default:
            {
                internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(a)));
            }
    }
}

/*
 * This function builds the full type a declarator is representing.  For
 * instance
 *
 *   int (*f)[3];
 *
 * Starts with a base type of "int" and ends being a "pointer to array 3 of int"
 */
static void build_scope_declarator_rec(
        AST a, 
        // AST top_declarator, AST a, 
        type_t** declarator_type, 
        gather_decl_spec_t* gather_info, 
        // This one contains the context of the occurring declarator
        // e.g void A::f(T) will contain the context of 'void A::f(T)'
        decl_context_t declarator_context,
        // This context contains the real context for the entity named
        // e.g void A::f(T) will contain the context of 'A::f'
        decl_context_t entity_context,
        // This one is used to sign in parameters, this is a block context
        // in function definitions and a prototype context for function
        // declarations or functional types
        decl_context_t *prototype_context,
        nodecl_t* nodecl_output)
{
    if (a == NULL)
        return;

    switch(ASTType(a))
    {
        case AST_DECLARATOR :
        case AST_PARENTHESIZED_DECLARATOR :
            {
                build_scope_declarator_rec(ASTSon0(a), declarator_type, 
                        gather_info, declarator_context, entity_context, prototype_context, nodecl_output); 
                break;
            }
        case AST_POINTER_DECLARATOR :
            {
                set_pointer_type(declarator_type, ASTSon0(a), declarator_context);
                if (is_error_type(*declarator_type))
                {
                    return;
                }
                build_scope_declarator_rec(ASTSon1(a), declarator_type, 
                        gather_info, declarator_context, entity_context, prototype_context, nodecl_output);
                break;
            }
        case AST_DECLARATOR_ARRAY :
            {
                set_array_type(declarator_type, 
                        /* expr */ASTSon1(a), 
                        /* (C99)static_qualif */ ASTSon3(a),
                        /* (C99)cv_qualifier_seq */ ASTSon2(a),
                        entity_context);
                if (is_error_type(*declarator_type))
                {
                    return;
                }
                build_scope_declarator_rec(ASTSon0(a), declarator_type, 
                        gather_info, declarator_context, entity_context, prototype_context, nodecl_output);
                break;
            }
        case AST_DECLARATOR_FUNC :
            {
                // This is a bit awkward. For compatibility reasons we must get
                // the enclosing declarator or the enclosing init_declarator
                // (the latter if it exists, the former otherwise)
                AST enclosing_init_decl = ASTParent(a);
                while (enclosing_init_decl != NULL
                        && ASTType(enclosing_init_decl) != AST_DECLARATOR
                        && ASTType(enclosing_init_decl) != AST_GCC_DECLARATOR)
                {
                    enclosing_init_decl = ASTParent(enclosing_init_decl);
                }
                AST enclosing_decl = enclosing_init_decl;
                while (enclosing_init_decl != NULL
                        && ASTType(enclosing_init_decl) != AST_INIT_DECLARATOR
                        && ASTType(enclosing_init_decl) != AST_GCC_INIT_DECLARATOR)
                {
                    enclosing_init_decl = ASTParent(enclosing_init_decl);
                }
                if (enclosing_init_decl != NULL)
                {
                    ASTAttrSetValueType(enclosing_init_decl, LANG_IS_FUNCTIONAL_DECLARATOR, tl_type_t, tl_bool(1));
                }
                else
                {
                    ASTAttrSetValueType(enclosing_decl, LANG_IS_FUNCTIONAL_DECLARATOR, tl_type_t, tl_bool(1));
                }

                set_function_type(declarator_type, gather_info, ASTSon1(a), 
                        ASTSon2(a), ASTSon3(a), entity_context, prototype_context, nodecl_output);
                if (is_error_type(*declarator_type))
                {
                    return;
                }

                build_scope_declarator_rec(ASTSon0(a), declarator_type, 
                        gather_info, declarator_context, entity_context, prototype_context, nodecl_output);
                break;
            }
        case AST_DECLARATOR_ID_EXPR :
            {
                // Do nothing
                break;
            }
            // GNU extensions
            // attribute declarator
        case AST_GCC_DECLARATOR :
            {
                ASTAttrSetValueType(a, LANG_IS_GCC_DECLARATOR, tl_type_t, tl_bool(1));
                build_scope_declarator_rec(ASTSon1(a), declarator_type, 
                        gather_info, declarator_context, entity_context, prototype_context, nodecl_output); 
                break;
            }
            // attribute * declarator
            // attribute & declarator
        case AST_GCC_POINTER_DECLARATOR :
            {
                ASTAttrSetValueType(a, LANG_IS_GCC_DECLARATOR, tl_type_t, tl_bool(1));
                set_pointer_type(declarator_type, ASTSon1(a), declarator_context);
                if (is_error_type(*declarator_type))
                {
                    return;
                }
                build_scope_declarator_rec(ASTSon2(a), declarator_type, 
                        gather_info, declarator_context, entity_context, prototype_context, nodecl_output);
                break;
            }
            // functional-declarator attribute
        case AST_GCC_FUNCTIONAL_DECLARATOR :
            {
                build_scope_declarator_rec(ASTSon0(a), declarator_type,
                        gather_info, declarator_context, entity_context, prototype_context, nodecl_output);
                break;
            }
        case AST_AMBIGUITY :
            {
                solve_ambiguous_declarator(a, declarator_context);
                // Restart function
                build_scope_declarator_rec(a, declarator_type, 
                        gather_info, declarator_context, entity_context, prototype_context, nodecl_output);
                break;
            }
        default:
            {
                internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(a)));
            }
    }
}

static char is_constructor_declarator_rec(AST a, char seen_decl_func)
{
    ERROR_CONDITION((a == NULL), "This function does not admit NULL trees", 0);

    switch(ASTType(a))
    {
        case AST_INIT_DECLARATOR :
        case AST_GCC_MEMBER_DECLARATOR :
        case AST_MEMBER_DECLARATOR :
        case AST_DECLARATOR :
        case AST_PARENTHESIZED_DECLARATOR :
            {
                return is_constructor_declarator_rec(ASTSon0(a), seen_decl_func); 
                break;
            }
        case AST_DECLARATOR_ID_EXPR :
            {
                if (!seen_decl_func)
                {
                    // A function declarator has not been seen
                    return 0;
                }
                else
                {
                    switch (ASTType(ASTSon0(a)))
                    {
                        case AST_QUALIFIED_ID :
                        case AST_SYMBOL :
                            return 1;
                        default :
                            return 0;
                    }
                }
            }
        case AST_POINTER_DECLARATOR :
        case AST_GCC_POINTER_DECLARATOR :
        case AST_DECLARATOR_ARRAY :
            {
                return 0;
            }
        case AST_GCC_FUNCTIONAL_DECLARATOR :
            {
                return is_constructor_declarator_rec(ASTSon0(a), seen_decl_func);
            }
        case AST_GCC_DECLARATOR :
            {
                return is_constructor_declarator_rec(ASTSon1(a), seen_decl_func);
            }
        case AST_DECLARATOR_FUNC :
            {
                if (!seen_decl_func)
                {
                    return is_constructor_declarator_rec(ASTSon0(a), 1);
                }
                else
                {
                    // More than one function found
                    return 0;
                }
            }
        default:
            {
                internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(a)));
            }
    }
}

static char is_constructor_declarator(AST a)
{
    return is_constructor_declarator_rec(a, 0);
}

/*
 * This function fills the symbol table with the information of this declarator
 */
static scope_entry_t* build_scope_declarator_name(AST declarator, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    AST declarator_id_expr = get_declarator_id_expression(declarator, decl_context);

    if (declarator_id_expr == NULL)
        return NULL;

    ERROR_CONDITION(ASTType(declarator_id_expr) != AST_DECLARATOR_ID_EXPR,
            "Invalid node '%s'\n", ast_print_node_type(ASTType(declarator_id_expr)));

    scope_entry_t* entry = build_scope_declarator_id_expr(declarator_id_expr, declarator_type, gather_info, 
            decl_context, nodecl_output);

    if (entry != NULL)
    {
        AST declarator_name = get_declarator_name(declarator, decl_context);

        if (declarator_name != NULL)
        {
            ASTAttrSetValueType(declarator_name, LANG_DECLARED_SYMBOL, tl_type_t, tl_symbol(entry));
        }
    }

    return entry;
}

void update_function_default_arguments(scope_entry_t* function_symbol, 
        type_t* declarator_type, 
        gather_decl_spec_t* gather_info)
{
    if (!is_named_type(declarator_type))
    {
        // We should mix here default argument info because the declarator has function-type form
        ERROR_CONDITION(gather_info->num_parameters != function_symbol->entity_specs.num_parameters,
                "This two should be the same and they are %d != %d", 
                gather_info->num_parameters, 
                function_symbol->entity_specs.num_parameters);

        int i;
        for (i = 0; i < gather_info->num_parameters; i++)
        {
            if (function_symbol->entity_specs.default_argument_info[i] == NULL
                    && gather_info->arguments_info[i].argument != NULL)
            {
                if (function_symbol->entity_specs.default_argument_info[i] == NULL)
                {
                    function_symbol->entity_specs.default_argument_info[i] 
                        = (default_argument_info_t*)counted_calloc(1, sizeof(default_argument_info_t), &_bytes_used_buildscope);
                }
                function_symbol->entity_specs.default_argument_info[i]->argument = gather_info->arguments_info[i].argument;
                function_symbol->entity_specs.default_argument_info[i]->context = gather_info->arguments_info[i].context;
            }
        }
    }
}

/*
 * This function fills information for a declarator_id_expr. Actually only
 * unqualified names can be signed up since qualified names should have been
 * declared elsewhere.
 */
static scope_entry_t* build_scope_declarator_id_expr(AST declarator_name, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, 
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    AST declarator_id = ASTSon0(declarator_name);

    switch (ASTType(declarator_id))
    {
        // Unqualified ones
        case AST_SYMBOL :
            {
                // A simply unqualified symbol "name"

                // We are not declaring a variable but a type
                if (gather_info->is_typedef)
                {
                    return register_new_typedef_name(declarator_id, declarator_type, gather_info, decl_context);
                }
                else
                {
                    return register_new_variable_name(declarator_id, declarator_type, gather_info, decl_context);
                }
                break;
            }
            // It should not appear here
            // case AST_DESTRUCTOR_TEMPLATE_ID : 
        case AST_DESTRUCTOR_ID :
            {
                // An unqualified destructor name "~name"
                // 'name' should be a class in this scope
                AST destructor_id = ASTSon0(declarator_id);
                // Adjust to 'function () returning void'
                declarator_type = get_const_qualified_type(get_new_function_type(get_void_type(), NULL, 0));
                return register_new_variable_name(destructor_id, declarator_type, gather_info, decl_context);
                break;
            }
        case AST_TEMPLATE_ID :
            {
                if (!is_function_type(declarator_type))
                {
                    scope_entry_list_t* entry_list = query_nested_name(decl_context,
                            NULL, NULL,
                            declarator_id);

                    ERROR_CONDITION((entry_list == NULL), "Qualified id '%s' name not found (%s)", 
                            prettyprint_in_buffer(declarator_id), ast_location(declarator_id));

                    scope_entry_t* result = entry_list_head(entry_list);

                    entry_list_free(entry_list);

                    return result;
                }
                else
                {
                    scope_entry_t *entry = NULL;
                    entry = find_function_declaration(declarator_id, declarator_type, gather_info, decl_context);

                    CXX_LANGUAGE()
                    {
                        if (entry != NULL)
                        {
                            update_function_default_arguments(entry, declarator_type, gather_info);
                        }
                    }
                    return entry;
                }

                break;
            }
        case AST_OPERATOR_FUNCTION_ID :
        case AST_OPERATOR_FUNCTION_ID_TEMPLATE :
            {
                // An unqualified operator_function_id "operator +"
                const char* operator_function_name = get_operator_function_name(declarator_id);
                AST operator_id = ASTLeaf(AST_SYMBOL, 
                        ASTFileName(declarator_id), ASTLine(declarator_id), 
                        operator_function_name);
                // Keep the parent of the original declarator
                ast_set_parent(operator_id, ast_get_parent(declarator_id)); 

                if (ASTType(declarator_id) == AST_OPERATOR_FUNCTION_ID_TEMPLATE)
                {
                    if (!solve_possibly_ambiguous_template_id(declarator_id, decl_context))
                    {
                        internal_error("Unresolved ambiguity '%s' at '%s'\n", 
                                prettyprint_in_buffer(declarator_id),
                                ast_location(declarator_id));
                    }
                }

                return register_new_variable_name(operator_id, declarator_type, gather_info, decl_context);
                break;
            }
        case AST_CONVERSION_FUNCTION_ID :
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Registering a conversion function in %s\n", ast_location(declarator_id));
                }
                // Ok, according to the standard, this function returns the
                // type defined in the conversion function id
                type_t* conversion_type_info = NULL;

                // Get the type and its name
                char* conversion_function_name = get_conversion_function_name(decl_context, declarator_id, 
                        &conversion_type_info, nodecl_output);
                AST conversion_id = ASTLeaf(AST_SYMBOL, 
                        ASTFileName(declarator_id),
                        ASTLine(declarator_id), 
                        conversion_function_name);
                // Keep the parent of the original declarator
                ast_set_parent(conversion_id, ast_get_parent(declarator_id));
                return register_new_variable_name(conversion_id, declarator_type, gather_info, decl_context);
                break;
            }
        // Qualified ones
        case AST_QUALIFIED_ID :
        case AST_QUALIFIED_TEMPLATE :
            {
                // A qualified id "a::b::c"
                if (!is_function_type(declarator_type))
                {
                    scope_entry_list_t* entry_list = query_nested_name(decl_context,
                            ASTSon0(declarator_id),
                            ASTSon1(declarator_id),
                            ASTSon2(declarator_id));

                    ERROR_CONDITION((entry_list == NULL), "Qualified id '%s' name not found (%s)", 
                            prettyprint_in_buffer(declarator_id), ast_location(declarator_id));

                    scope_entry_t* entry = entry_list_head(entry_list);

                    entry_list_free(entry_list);

                    return entry;
                }
                else
                {
                    scope_entry_t *entry = NULL;

                    if (ASTType(ASTSon2(declarator_id)) == AST_DESTRUCTOR_ID
                            || ASTType(ASTSon2(declarator_id)) == AST_DESTRUCTOR_TEMPLATE_ID)
                    {
                        // Adjust the type to 'const function () returning void'
                        declarator_type = get_const_qualified_type(get_new_function_type(get_void_type(), NULL, 0));
                    }

                    entry = find_function_declaration(declarator_id, declarator_type, gather_info, decl_context);

                    CXX_LANGUAGE()
                    {
                        if (entry != NULL)
                        {
                            update_function_default_arguments(entry, declarator_type, gather_info);
                        }
                    }
                    return entry;
                }
                break;
            }
        default :
            {
                internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(declarator_id)));
                break;
            }
    }

    return NULL;
}

/*
 * This function registers a new typedef name.
 */
static scope_entry_t* register_new_typedef_name(AST declarator_id, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, decl_context_t decl_context)
{
    // First query for an existing entry in this scope
    scope_entry_list_t* list = query_in_scope(decl_context, declarator_id);

    // Only enum or classes can exist, otherwise this is an error
    if (list != NULL)
    {
        // Check that the symbol is eligible for "typedeffing"
        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(list);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* entry = entry_list_iterator_current(it);
            if (entry->kind != SK_ENUM 
                    && entry->kind != SK_CLASS
                    && entry->kind != SK_TYPEDEF)
            {
                running_error("%s: error: symbol '%s' has been redeclared as a different symbol kind (look at '%s:%d').", 
                        ast_location(declarator_id), 
                        prettyprint_in_buffer(declarator_id), 
                        entry->file,
                        entry->line);
            }
        }
        entry_list_iterator_free(it);

        scope_entry_t* entry = entry_list_head(list);

        entry_list_free(list);

        // We have to allow 
        // typedef struct A { .. } A;
        //
        // In this case the declarator_id (rightmost "A") will be a SK_CLASS
        // with TK_DIRECT of STK_USER_DEFINED pointing exactly to entry (the
        // leftmost "A"). In this case, this is not an error.
        //
        // Consider
        //
        // typedef struct A { .. } *A;
        //
        // This is ill-formed because the rightmost A should be the same typename for the leftmost one.
        //
        if (!is_named_type(declarator_type)
                || named_type_get_symbol(declarator_type) != entry)
        {
            if(!equivalent_types(entry->type_information, declarator_type))
            {
                running_error("%s: error: symbol '%s' has been redeclared as a different symbol kind (look at '%s:%d').", 
                        ast_location(declarator_id), 
                        prettyprint_in_buffer(declarator_id), 
                        entry->file,
                        entry->line);
            }

            if (is_function_type(declarator_type)
                    && !is_named_type(declarator_type))
            {
                // If the declarator is a functional one, we have to mix the arguments here
                int i;
                for (i = 0; i < gather_info->num_parameters; i++)
                {
                    if (entry->entity_specs.default_argument_info[i] == NULL
                            && gather_info->arguments_info[i].argument != NULL)
                    {
                        entry->entity_specs.default_argument_info[i] 
                            = (default_argument_info_t*)counted_calloc(1, sizeof(default_argument_info_t), &_bytes_used_buildscope);

                        entry->entity_specs.default_argument_info[i]->argument = gather_info->arguments_info[i].argument;
                        entry->entity_specs.default_argument_info[i]->context = gather_info->arguments_info[i].context;
                    }
                }
            }
        }
        else
        {
            // In this special case, "A" will not be redefined, lets undefine
            // here and let it be redefined again later
            entry->defined = 0;
        }

        return entry;
    }

    scope_entry_t* entry = new_symbol(decl_context, decl_context.current_scope, ASTText(declarator_id));

    DEBUG_CODE()
    {
        fprintf(stderr, "Registering typedef '%s'\n", ASTText(declarator_id));
    }

    entry->line = ASTLine(declarator_id);
    entry->file = ASTFileName(declarator_id);
    entry->point_of_declaration = get_enclosing_declaration(declarator_id);

    // Dealing with typedefs against function types
    //
    // Case 1 - typedef against a functional declarator
    //
    //   typedef float T(int i = 3);
    //   T k; 
    //
    // this is equivalent to
    //
    //   float k(int i = 3);
    //
    // FIXME - Should we copy also exception specs ?
    if (is_function_type(declarator_type)
            && !is_named_type(declarator_type))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "This is a typedef to function type, saving gathered information\n");
            fprintf(stderr, "Number of parameters %d\n", gather_info->num_parameters);
        }

        entry->entity_specs.num_parameters = gather_info->num_parameters;
        entry->entity_specs.default_argument_info = counted_calloc(entry->entity_specs.num_parameters,
                sizeof(*(entry->entity_specs.default_argument_info)), 
                &_bytes_used_buildscope);
        int i;
        for (i = 0; i < gather_info->num_parameters; i++)
        {
            if (gather_info->arguments_info[i].argument != NULL)
            {
                entry->entity_specs.default_argument_info[i] = 
                    (default_argument_info_t*)counted_calloc(1, sizeof(default_argument_info_t), &_bytes_used_buildscope);
                entry->entity_specs.default_argument_info[i]->argument = gather_info->arguments_info[i].argument;
                entry->entity_specs.default_argument_info[i]->context = gather_info->arguments_info[i].context;
            }
        }
        
        // Copy exception info as well
        entry->entity_specs.any_exception = gather_info->any_exception;
        entry->entity_specs.num_exceptions = gather_info->num_exceptions;
        entry->entity_specs.exceptions = gather_info->exceptions;
    }

    // Case 2 - typedef against a typedef that was a functional declarator typedef ...
    //
    //   typedef float T(int i = 3);
    //   typedef T Q;
    //   Q q;
    //
    // this is equivalent to
    //
    //   float q(int i = 3);
    //
    {
        scope_entry_t* named_type = NULL;
        if (is_named_type(declarator_type)
                && ((named_type = named_type_get_symbol(declarator_type))->kind == SK_TYPEDEF)
                && is_function_type(named_type->type_information))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "This is a typedef to typedef of function type, copying gathered information\n");
                fprintf(stderr, "Number of parameters %d\n", named_type->entity_specs.num_parameters);
            }

            // Case 1 above will have copied such information in the symbol
            int i;
            for (i = 0; i < named_type->entity_specs.num_parameters; i++)
            {
                P_LIST_ADD(entry->entity_specs.default_argument_info,
                        entry->entity_specs.num_parameters,
                        named_type->entity_specs.default_argument_info[i]);
            }
            
            // Copy exception info as well
            entry->entity_specs.any_exception = gather_info->any_exception;
            entry->entity_specs.num_exceptions = named_type->entity_specs.num_exceptions;
            entry->entity_specs.exceptions = named_type->entity_specs.exceptions;
        }
    }

    entry->kind = SK_TYPEDEF;
    entry->type_information = declarator_type;

    return entry;
}

/*
 * This function registers a new "variable" (non type) name
 */
static scope_entry_t* register_new_variable_name(AST declarator_id, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, decl_context_t decl_context)
{
    if (!is_function_type(declarator_type)
            // A parameter might have function type but we do not want to do
            // anything special on it, later on its type will be adjusted
            // to pointer to function
            || gather_info->parameter_declaration)
    {
        decl_flags_t decl_flags = DF_NONE;
        // Check for existence of this symbol in this scope
        scope_entry_list_t* entry_list = query_in_scope_flags(decl_context, declarator_id, decl_flags);

        scope_entry_list_t* check_list = filter_symbol_kind(entry_list, SK_VARIABLE);

        // Return the found symbol
        if (check_list != NULL)
        {
            scope_entry_t* entry = entry_list_head(check_list);
            
            // Always use the latest one, unfortunately a variable can be
            // declared several times by means of "extern" keyword
            if (!entry->entity_specs.is_member)
            {
                entry->point_of_declaration = get_enclosing_declaration(declarator_id);
            }
            return entry;
        }

        entry_list_free(check_list);

        enum cxx_symbol_kind valid_symbols[] = {
            SK_CLASS, 
            SK_ENUM
        };
        check_list = filter_symbol_non_kind_set(entry_list, STATIC_ARRAY_LENGTH(valid_symbols), valid_symbols);

        entry_list_free(entry_list);

        if (check_list != NULL)
        {
            scope_entry_t* entry = entry_list_head(check_list);
            running_error("%s: error: incompatible redeclaration of '%s' (look at '%s:%d')\n",
                    ast_location(declarator_id),
                    prettyprint_in_buffer(declarator_id),
                    entry->file,
                    entry->line);
        }

        entry_list_free(check_list);

        DEBUG_CODE()
        {
            fprintf(stderr, "Registering variable '%s' in %p\n", ASTText(declarator_id), decl_context.current_scope);
        }

        scope_entry_t* entry = NULL;
        entry = new_symbol(decl_context, decl_context.current_scope, ASTText(declarator_id));

        entry->line = ASTLine(declarator_id);
        entry->file = ASTFileName(declarator_id);
        entry->point_of_declaration = get_enclosing_declaration(declarator_id);
        entry->kind = SK_VARIABLE;
        entry->type_information = declarator_type;

        entry->entity_specs.is_static = gather_info->is_static;
        entry->entity_specs.is_mutable = gather_info->is_mutable;
        entry->entity_specs.is_extern = gather_info->is_extern;
        entry->entity_specs.is_register = gather_info->is_register;
        entry->entity_specs.is_thread = gather_info->is_thread;

        // Copy gcc attributes
        entry->entity_specs.num_gcc_attributes = gather_info->num_gcc_attributes;
        memcpy(entry->entity_specs.gcc_attributes, 
                gather_info->gcc_attributes, sizeof(gather_info->gcc_attributes));

        return entry;
    }
    else
    {
        return register_function(declarator_id, declarator_type, gather_info, decl_context);
    }
}

static scope_entry_t* register_function(AST declarator_id, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, decl_context_t decl_context)
{
    scope_entry_t* entry = NULL;
    entry = find_function_declaration(declarator_id, declarator_type, gather_info, decl_context);

    if (entry == NULL)
    {
        // No existing function was found
        const char* function_name = ASTText(declarator_id);

        if (BITMAP_TEST(decl_context.decl_flags, DF_CONSTRUCTOR))
        {
            function_name = strprepend(function_name, "constructor ");
        }
        scope_entry_t* new_entry = NULL;

        if (!gather_info->is_template)
        {
            // Create the symbol as a normal function type

            new_entry = new_symbol(decl_context, decl_context.current_scope, function_name);
            new_entry->type_information = declarator_type;

            new_entry->kind = SK_FUNCTION;
            new_entry->line = ASTLine(declarator_id);
            new_entry->file = ASTFileName(declarator_id);
            new_entry->point_of_declaration = get_enclosing_declaration(declarator_id);

            new_entry->entity_specs.is_friend_declared = gather_info->is_friend;
        }
        else /* gather_info->is_template */
        {
            if (decl_context.template_parameters == NULL)
            {
                running_error("%s: error: explicit specialization '%s' does not match any template of '%s'\n",
                        ast_location(declarator_id),
                        print_decl_type_str(declarator_type, decl_context, function_name),
                        function_name);
            }

            ERROR_CONDITION(decl_context.template_parameters == NULL,
                    "Error, there must be template parameters", 0);

            // If this function is template we have to create a template type
            type_t* template_type = get_new_template_type(decl_context.template_parameters,
                    declarator_type,
                    function_name,
                    decl_context,
                    ASTLine(declarator_id),
                    ASTFileName(declarator_id));

            new_entry = new_symbol(decl_context, decl_context.current_scope, function_name);
            new_entry->type_information = template_type;

            // This is a template, not a plain function
            new_entry->kind = SK_TEMPLATE;
            new_entry->line = ASTLine(declarator_id);
            new_entry->file = ASTFileName(declarator_id);
            new_entry->point_of_declaration = get_enclosing_declaration(declarator_id);

            new_entry->entity_specs.is_friend_declared = gather_info->is_friend;

            if (decl_context.current_scope->kind == CLASS_SCOPE)
            {
                new_entry->entity_specs.is_member = 1;
                // FIXME
                // new_entry->entity_specs.access = AS_PUBLIC;
                new_entry->entity_specs.class_type = 
                    get_user_defined_type(decl_context.current_scope->related_entry);
            }

            template_type_set_related_symbol(template_type, new_entry);

            // Now update the symbol, we are not working anymore on the
            // template type itself but on its main specialization (primary
            // template type)
            new_entry = named_type_get_symbol(
                    template_type_get_primary_type(template_type));

            // Update info
            new_entry->line = ASTLine(declarator_id);
            new_entry->file = ASTFileName(declarator_id);
            new_entry->point_of_declaration = get_enclosing_declaration(declarator_id);
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "Registering new function '%s' at %s. symbol=%p scope=%p is_template=%d\n", 
                    function_name, 
                    ast_location(declarator_id),
                    new_entry,
                    new_entry->decl_context.current_scope,
                    (new_entry->kind == SK_TEMPLATE)
                   );
        }

        new_entry->entity_specs.is_static = gather_info->is_static;
        new_entry->entity_specs.is_extern = gather_info->is_extern;
        new_entry->entity_specs.is_inline = gather_info->is_inline;
        new_entry->entity_specs.is_virtual = gather_info->is_virtual;
        
        // "is_pure" of a function is computed in "build_scope_member_simple_declaration"

        new_entry->entity_specs.any_exception = gather_info->any_exception;
        new_entry->entity_specs.num_exceptions = gather_info->num_exceptions;
        new_entry->entity_specs.exceptions = gather_info->exceptions;

        new_entry->entity_specs.num_parameters = gather_info->num_parameters;

        new_entry->entity_specs.default_argument_info = 
            counted_calloc(gather_info->num_parameters, 
                    sizeof(*new_entry->entity_specs.default_argument_info),
                    &_bytes_used_buildscope);

        int i;
        for (i = 0; i < gather_info->num_parameters; i++)
        {
            if (gather_info->arguments_info[i].argument != NULL)
            {
                new_entry->entity_specs.default_argument_info[i] = 
                    (default_argument_info_t*)counted_calloc(1, sizeof(default_argument_info_t), 
                            &_bytes_used_buildscope);
                new_entry->entity_specs.default_argument_info[i]->argument = 
                    gather_info->arguments_info[i].argument;
                new_entry->entity_specs.default_argument_info[i]->context = 
                    gather_info->arguments_info[i].context;
            }
        }
        
        if (is_named_type(new_entry->type_information))
        {
            ERROR_CONDITION((named_type_get_symbol(new_entry->type_information)->kind != SK_TYPEDEF)
                    || !is_function_type(named_type_get_symbol(new_entry->type_information)->type_information),
                    "Wrong function type declaration underway", 0);
            // Adjustment because of typedef
            /*
             * typedef int T(float);
             *
             * T f;
             *
             * In this case we are registering an 'f' that does not have any number of parameters
             * nor any default argument info, so we have to fix and make that 'f' looks like
             * it was normally declared as 'int f(float)' instead of that awkward thing of 
             * 'T f' (with T being a function type)
             *
             */

            // Now adjust the entry
            scope_entry_t* named_function_type = named_type_get_symbol(new_entry->type_information);

            DEBUG_CODE()
            {
                fprintf(stderr , "This function declaration comes from a typedef of function type.\n");
                fprintf(stderr , "Num parameters %d\n", named_function_type->entity_specs.num_parameters);
            }

            // Adjust the parameter info
            int j;
            for (j = 0; j < named_function_type->entity_specs.num_parameters; j++)
            {
                P_LIST_ADD(new_entry->entity_specs.default_argument_info, 
                        new_entry->entity_specs.num_parameters,
                        named_function_type->entity_specs.default_argument_info[j]);
            }

            // Copy exception info as well
            new_entry->entity_specs.num_exceptions = named_function_type->entity_specs.num_exceptions;
            new_entry->entity_specs.exceptions = named_function_type->entity_specs.exceptions;
        }

        // Copy gcc attributes
        new_entry->entity_specs.num_gcc_attributes = gather_info->num_gcc_attributes;
        memcpy(new_entry->entity_specs.gcc_attributes, 
                gather_info->gcc_attributes, sizeof(gather_info->gcc_attributes));

        // Set the 'template' nature of the class
        
        return new_entry;
    }
    else
    {
        // Merge inline attribute
        entry->entity_specs.is_inline |= gather_info->is_inline;

        // Remove the friend-declared attribute if we find the function but
        // this is not a friend declaration
        if (!is_template_specialized_type(entry->type_information))
        {
            if (!gather_info->is_friend
                    && entry->entity_specs.is_friend_declared)
            {
                entry->entity_specs.is_friend_declared = 0;
            }
        }
        else
        {
            type_t* template_type = template_specialized_type_get_related_template_type(entry->type_information);
            scope_entry_t* template_symbol = template_type_get_related_symbol(template_type);
            if (!gather_info->is_friend
                    && template_symbol->entity_specs.is_friend_declared)
            {
                template_symbol->entity_specs.is_friend_declared = 0;
            }
        }

        // An existing function was found
        CXX_LANGUAGE()
        {
            update_function_default_arguments(entry, declarator_type, gather_info);
        }

        C_LANGUAGE()
        {
            // If the type we got does not have prototype but the new declaration
            // has, use the type coming from the new declaration
            if (!function_type_get_lacking_prototype(declarator_type)
                    && function_type_get_lacking_prototype(entry->type_information))
            {
                // Update the type
                entry->type_information = declarator_type;
            }
        }

        return entry;
    }
}

static scope_entry_t* find_function_declaration(AST declarator_id, 
        type_t* declarator_type, 
        gather_decl_spec_t* gather_info,
        decl_context_t decl_context)
{
    decl_flags_t decl_flags = DF_NONE;
    if (BITMAP_TEST(decl_context.decl_flags, DF_CONSTRUCTOR))
    {
        decl_flags |= DF_CONSTRUCTOR;
    }
    
    // Restrict ourselves to the current scope
    // if the declarator-id is unqualified
    // and we are not naming a friend
    if (!gather_info->is_friend)
    {
        switch (ASTType(declarator_id))
        {
            case AST_SYMBOL :
            case AST_TEMPLATE_ID :
            case AST_DESTRUCTOR_ID :
            case AST_DESTRUCTOR_TEMPLATE_ID :
            case AST_CONVERSION_FUNCTION_ID :
            case AST_OPERATOR_FUNCTION_ID :
            case AST_OPERATOR_FUNCTION_ID_TEMPLATE :
                decl_flags |= DF_ONLY_CURRENT_SCOPE;
                break;
            default:
                break;
        }
    }

    scope_entry_list_t* entry_list 
        = query_id_expression_flags(decl_context, declarator_id, decl_flags);

    type_t* function_type_being_declared = declarator_type;

    scope_entry_t* equal_entry = NULL;

    char found_equal = 0;

    char templates_available = 0;

    // First attempt an exact match against either SK_FUNCTION or
    // the primary of every SK_TEMPLATE
    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it) && !found_equal;
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        // Ignore this case
        if (entry->kind == SK_DEPENDENT_ENTITY)
            continue;

        // This is so C90's ;)
        if (entry->kind == SK_CLASS
                || entry->kind == SK_ENUM)
            continue;

        entry = entry_advance_aliases(entry);

        if (entry->kind != SK_FUNCTION
                && (entry->kind != SK_TEMPLATE
                    || named_type_get_symbol(template_type_get_primary_type(entry->type_information))->kind != SK_FUNCTION))
        {
            running_error("%s: name '%s' has already been declared as a different entity kind",
                    ast_location(declarator_id),
                    prettyprint_in_buffer(declarator_id));
        }

        if (entry->entity_specs.is_member
                && decl_context.current_scope->kind == CLASS_SCOPE
                && !equivalent_types(get_actual_class_type(entry->entity_specs.class_type), 
                    get_actual_class_type(decl_context.current_scope->related_entry->type_information)))
            continue;

        scope_entry_t* considered_symbol = NULL;
        if (entry->kind == SK_TEMPLATE)
        {
            type_t* primary_named_type = template_type_get_primary_type(entry->type_information);
            considered_symbol = named_type_get_symbol(primary_named_type);
            templates_available |= 1;
        }
        else if (entry->kind == SK_FUNCTION)
        {
            considered_symbol = entry;
        }
        else
        {
            internal_error("Unreachable code", 0);
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "Checking function declaration of '%s' at '%s' (%s) against the declaration at '%s:%d' (%s)\n",
                    prettyprint_in_buffer(declarator_id), 
                    ast_location(declarator_id),
                    print_declarator(function_type_being_declared),
                    considered_symbol->file, 
                    considered_symbol->line,
                    print_declarator(considered_symbol->type_information)
                   );
        }

        type_t* considered_type = advance_over_typedefs(considered_symbol->type_information);

        found_equal = equivalent_types(function_type_being_declared, considered_type);
        if (found_equal)
        {
            equal_entry = considered_symbol;
            DEBUG_CODE()
            {
                fprintf(stderr, "Function declarator '%s' at '%s' matches symbol '%s' declared at '%s:%d'\n",
                        prettyprint_in_buffer(declarator_id), 
                        ast_location(declarator_id),
                        considered_symbol->symbol_name,
                        considered_symbol->file,
                        considered_symbol->line);
            }
        }
        else
        {
            C_LANGUAGE()
            {
                if (!function_type_get_lacking_prototype(function_type_being_declared)
                        && !function_type_get_lacking_prototype(considered_type))
                {
                    running_error("%s: error: function '%s' has been declared with different prototype (see '%s:%d')", 
                            ast_location(declarator_id),
                            ASTText(declarator_id),
                            entry->file,
                            entry->line
                            );
                }
                equal_entry = considered_symbol;
                found_equal = 1;
            }
        }
    }
    entry_list_iterator_free(it);

    // Second attempt, match a specialization of a template function
    if (templates_available
            && gather_info->is_explicit_instantiation)
    {
        // Under explicit instantiation we do not allow friends
        scope_entry_list_t* old_entry_list = entry_list;
        entry_list = filter_friend_declared(entry_list);
        entry_list_free(old_entry_list);

        template_parameter_list_t *explicit_template_parameters = NULL;

        AST considered_tree = declarator_id;
        if (ASTType(declarator_id) == AST_QUALIFIED_ID
                || ASTType(declarator_id) == AST_QUALIFIED_TEMPLATE)
        {
            considered_tree = ASTSon2(declarator_id);
        }

        if (ASTType(considered_tree) == AST_TEMPLATE_ID
                || ASTType(considered_tree) == AST_OPERATOR_FUNCTION_ID_TEMPLATE
           )
        {
            explicit_template_parameters = 
                get_template_parameters_from_syntax(ASTSon1(considered_tree), decl_context);
        }

        scope_entry_t* result = solve_template_function(
                entry_list,
                explicit_template_parameters,
                function_type_being_declared,
                ASTFileName(declarator_id),
                ASTLine(declarator_id)
                );

        if (result != NULL)
        {
            found_equal = 1;
            equal_entry = result;
        }
    }

    entry_list_free(entry_list);

    if (!found_equal)
    {
        return NULL;
    }
    else
    {
        return equal_entry;
    }
}

/*
 * This function saves the current linkage, sets the new and restores it back.
 */
static void build_scope_linkage_specifier(AST a, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    AST declaration_sequence = ASTSon1(a);

    if (declaration_sequence == NULL)
        return;

    const char* previous_linkage = current_linkage;

    AST linkage_spec = ASTSon0(a);
    current_linkage = ASTText(linkage_spec);

    build_scope_declaration_sequence(declaration_sequence, decl_context, nodecl_output);

    current_linkage = previous_linkage;
}

/*
 * Similar to build_scope_linkage_specifier but for just one declaration
 */
static void build_scope_linkage_specifier_declaration(AST a, 
        AST top_linkage_decl, 
        decl_context_t decl_context,
        nodecl_t *nodecl_output,
        scope_entry_list_t** declared_symbols
        )
{
    AST declaration = ASTSon1(a);

    const char* previous_linkage = current_linkage;

    AST linkage_spec = ASTSon0(a);
    current_linkage = ASTText(linkage_spec);

    if (ASTType(declaration)  == AST_LINKAGE_SPEC_DECL)
    {
        build_scope_linkage_specifier_declaration(declaration, top_linkage_decl, decl_context, 
                nodecl_output, declared_symbols);
    }
    else
    {
        build_scope_declaration(declaration, decl_context, nodecl_output, declared_symbols);
    }

    ASTAttrSetValueType(declaration, LANG_HAS_LINKAGE_SPECIFIER, tl_type_t, tl_bool(1));
    ast_set_link_to_child(declaration, LANG_LINKAGE_SPECIFIER_HEADER, top_linkage_decl);
    ASTAttrSetValueType(linkage_spec, LANG_IS_LINKAGE_SPECIFIER, tl_type_t, tl_bool(1));

    current_linkage = previous_linkage;
}

/*
 * This function registers a template declaration
 */
static void build_scope_template_declaration(AST a, 
        AST top_template_decl, 
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    /*
     * The declaration after the template parameter list can be
     * a simple declaration or a function definition.
     *
     * For the case of a simple_declaration, the following are examples
     * of what can appear there
     *
     *   template <class P, class Q>
     *   class A                 // A primary template class
     *   {
     *   };
     *
     *   template <class P>
     *   class A<P, int>         // A partial specialized class
     *   {
     *   };
     *
     *   template <class P>
     *   T A<P>::d = expr;       // For static member initialization
     *   
     *   template <class P>           
     *   void f(..., P q, ...);  // Function declaration
     *
     * Template classes are saved in a special form since the may be
     * specialized in several ways.
     *
     */

    /*
     * Template parameter information is constructed first
     */
    decl_context_t template_context;
    build_scope_template_header(ASTSon0(a), decl_context, &template_context, nodecl_output);
    
    AST templated_decl = ASTSon1(a);
    if (ASTType(templated_decl) == AST_AMBIGUITY)
    {
        solve_ambiguous_declaration(templated_decl, template_context);
    }

    // Link the AST with the scope in the templated declaration
    scope_link_set(CURRENT_COMPILED_FILE->scope_link, templated_decl, template_context);

    switch (ASTType(templated_decl))
    {
        case AST_FUNCTION_DEFINITION :
            {
                build_scope_template_function_definition(templated_decl, template_context, 
                        /* is_explicit_instantiation */ 0, nodecl_output);

                ASTAttrSetValueType(templated_decl, LANG_IS_TEMPLATED_FUNCTION_DEFINITION, tl_type_t, tl_bool(1));
                break;
            }
        case AST_SIMPLE_DECLARATION :
            {
                build_scope_template_simple_declaration(templated_decl, template_context, 
                        /* is_explicit_instantiation */ 0, nodecl_output);

                ASTAttrSetValueType(templated_decl, LANG_IS_TEMPLATED_DECLARATION, tl_type_t, tl_bool(1));
                break;
            }
        case AST_TEMPLATE_DECLARATION :
            {
                build_scope_template_declaration(templated_decl, top_template_decl, template_context, nodecl_output);
                break;
            }
        default :
            internal_error("Unknown node type '%s' (line=%s)\n", ast_print_node_type(ASTType(templated_decl)), 
                    ast_location(templated_decl));
    }

    ASTAttrSetValueType(ASTSon0(a), LANG_IS_TEMPLATE_HEADER, tl_type_t, tl_bool(1));
    ast_set_link_to_child(templated_decl, LANG_TEMPLATE_HEADER, top_template_decl);
}

void build_scope_template_header(AST template_parameter_list, 
        decl_context_t decl_context, 
        decl_context_t *template_context,
        nodecl_t* nodecl_output)
{
    (*template_context) = decl_context;
    // A new level of template nesting
    (*template_context).template_parameters = counted_calloc(1, sizeof(*(*template_context).template_parameters), &_bytes_used_buildscope);
    (*template_context).template_parameters->enclosing = decl_context.template_parameters;

    int nesting = get_template_nesting_of_context(decl_context) + 1;
    build_scope_template_parameter_list(template_parameter_list, 
            (*template_context).template_parameters, 
            nesting,
            (*template_context), nodecl_output);
}

/*
 * This function registers an explicit template specialization
 */
static void build_scope_explicit_template_specialization(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    decl_context_t template_context = decl_context;

    // Note that we do not increment here the template_nesting because complete
    // specializations do not introduce a new template nesting. 
    //
    // For instance:
    //
    //   template <typename _T>
    //   struct A { 
    //     void f(); <-- template nesting of 1
    //   };
    //
    //   template <typename _Q> <-- as many times as "template nesting" has the entity
    //   void A<_Q>::f() { }
    //
    //   template<>
    //   struct A<int> {
    //     void f(); <-- template nesting of 0
    //   };
    //
    //   <-- no template "header" required since A<int>::f has a nesting of 0
    //   void A<int>::f() { } 
    //
    // More examples
    //
    //   struct A1
    //   {
    //      template <typename _T>
    //      void f(_T t);
    //   };
    //
    //   template <typename _Q>
    //   void A1::f(_Q q)
    //   {
    //   }
    //
    //   template <>
    //   void A1::f(int q)
    //   {
    //   }
    //
    //   template <typename _T>
    //   struct A2
    //   {
    //      template <typename _Q>
    //      void f(_Q q);
    //
    //      template <>
    //      void f(float q);
    //   };
    //
    //   template <typename _T1>
    //   template <typename _Q1>
    //   void A2<_T1>::f(_Q1 q) { }
    //
    //   template <typename _T1>
    //   void A2<_T1>::f(float q) { }
    //
    //   template <>
    //   struct A3<int>
    //   {
    //      template <typename _Q>
    //      void f(_Q q);
    //
    //      template <>
    //      void f(float q);
    //   };
    //
    //   template <typename _Q1>
    //   void A3<int>::f(_Q1 q) { }
    //
    //   void A3<int>::f(float q) { }
    //
    
    if (ASTType(ASTSon0(a)) == AST_AMBIGUITY)
    {
        solve_ambiguous_declaration(ASTSon0(a), template_context);
    }

    switch (ASTType(ASTSon0(a)))
    {
        case AST_FUNCTION_DEFINITION :
            {
                build_scope_template_function_definition(ASTSon0(a), template_context, 
                        /* is_explicit_instantiation */ 1, nodecl_output);
                break;
            }
        case AST_SIMPLE_DECLARATION :
            {
                build_scope_template_simple_declaration(ASTSon0(a), template_context, 
                        /* is_explicit_instantiation */ 1, nodecl_output);
                break;
            }
        case AST_EXPLICIT_SPECIALIZATION :
            {
                build_scope_explicit_template_specialization(ASTSon0(a), template_context, nodecl_output);
                break;
            }
        default :
            {
                internal_error("Unknown node type '%s'\n", ast_print_node_type(ASTType(ASTSon0(a))));
            }
    }
}

static void build_scope_template_function_definition(AST a, decl_context_t decl_context, 
        char is_explicit_instantiation, nodecl_t* nodecl_output)
{
    /* scope_entry_t* entry = */ build_scope_function_definition(a, /* previous_symbol */ NULL, decl_context, 
            /* is_template */ 1, is_explicit_instantiation, nodecl_output);
}

static void build_scope_template_simple_declaration(AST a, decl_context_t decl_context, 
        char is_explicit_instantiation, 
        nodecl_t* nodecl_output)
{
    /*
     * A templated simple declaration can be 
     *
     *   template <class P, class Q>
     *   class A                 // A primary template class
     *   {
     *   };
     *
     *   template <class P>
     *   class A<P, int>         // A partial specialized class
     *   {
     *   };
     *
     *   template <class P>
     *   const T A<P>::d = expr;       // For static const member initialization
     *
     * For classes if it is a primary template we will register it in the
     * current scope as a SK_TEMPLATE_CLASS. Otherwise nothing is done since
     * when declaring a specialization the primary template is extended to hold
     * the specialization.
     */

    AST decl_specifier_seq = ASTSon0(a);
    // This list should only contain one element according
    // to the standard
    AST init_declarator_list = ASTSon1(a);

    type_t* simple_type_info = NULL;
    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));

    gather_info.is_template = 1;
    gather_info.is_explicit_instantiation = is_explicit_instantiation;

    char is_constructor = 0;
    
    if (decl_specifier_seq != NULL)
    {
        if (init_declarator_list == NULL)
        {
            gather_info.no_declarators = 1;
        }
        build_scope_decl_specifier_seq(decl_specifier_seq, &gather_info, &simple_type_info, decl_context, nodecl_output);
    }

    // There can be just one declarator here if this is not a class specifier nor a function declaration
    // otherwise no declarator can appear
    //
    //    template <class P>
    //    const T A<P>::d = expr;       // For static const member initialization
    //            ^^^^^^^^^^^^^^
    //            we are handling this

    if (init_declarator_list != NULL)
    {
        AST init_declarator = ASTSon1(init_declarator_list);

        if (ASTType(init_declarator) == AST_AMBIGUITY)
        {
            solve_ambiguous_init_declarator(init_declarator, decl_context);
        }

        AST declarator = ASTSon0(init_declarator);
        AST initializer = ASTSon1(init_declarator);


        if (decl_specifier_seq != NULL 
                && ((ASTType(decl_specifier_seq) != AST_AMBIGUITY && ASTSon1(decl_specifier_seq) != NULL)
                    || (ASTType(decl_specifier_seq) == AST_AMBIGUITY)))
        {
            // This is not a constructor
            is_constructor = 0;
        }
        else
        {
            if (is_constructor_declarator(declarator))
            {
                is_constructor = 1;
            }
        }

        // Note that the scope where this declarator will be declared includes
        // the template parameters, since the symbol will have to be qualified
        // it will not create a symbol in "st" but will fetch the previously
        // declared one within the class.
        type_t* declarator_type = NULL;

        decl_context_t new_decl_context = decl_context;
        if (is_constructor)
        {
            new_decl_context.decl_flags |= DF_CONSTRUCTOR;
        }

        compute_declarator_type(declarator,
                &gather_info, simple_type_info, &declarator_type,
                new_decl_context, nodecl_output);
        scope_entry_t *entry = build_scope_declarator_name(declarator, declarator_type, 
                &gather_info, new_decl_context, nodecl_output);
        
        // This is a simple declaration, thus if it does not declare an
        // extern variable or function, the symbol is already defined here
        if (!gather_info.is_extern
                && !is_function_type(declarator_type))
        {
            if (initializer != NULL)
            {
                // Here the template scope is the one of the template declaration, 
                // not for the symbol

                decl_context_t initializer_context = entry->decl_context;
                initializer_context.template_parameters = new_decl_context.template_parameters;

                check_initialization(initializer, 
                        initializer_context, 
                        get_unqualified_type(declarator_type));
                entry->language_dependent_value = initializer;
                entry->value = expression_get_nodecl(initializer);
            }
        }
        else if (is_function_type(declarator_type))
        {
            // At the moment do nothing for it
        }
    }
}

/*
 * This function registers templates parameters in a given scope
 */
static void build_scope_template_parameter_list(AST a, 
        template_parameter_list_t* template_parameters,
        int nesting,
        decl_context_t template_context,
        nodecl_t* nodecl_output)
{
    AST iter;
    AST list = a;

    for_each_element(list, iter)
    {
        AST template_parameter_tree = ASTSon1(iter);

        build_scope_template_parameter(template_parameter_tree, 
                template_parameters, 
                nesting,
                template_context,
                nodecl_output);
    }
}

/*
 * This function registers one template parameter in a given scope
 */
static void build_scope_template_parameter(AST a, 
        template_parameter_list_t* template_parameter_list, 
        int nesting,
        decl_context_t template_context,
        nodecl_t* nodecl_output)
{
    switch (ASTType(a))
    {
        case AST_GCC_PARAMETER_DECL :
            // We are ignoring here attributes
            build_scope_nontype_template_parameter(a, template_parameter_list, nesting, template_context, nodecl_output);
            break;
        case AST_PARAMETER_DECL :
            build_scope_nontype_template_parameter(a, template_parameter_list, nesting, template_context, nodecl_output);
            break;
        case AST_TYPE_PARAMETER_CLASS :
        case AST_TYPE_PARAMETER_TYPENAME :
            build_scope_type_template_parameter(a, template_parameter_list, nesting, template_context, nodecl_output);
            break;
        case AST_TYPE_PARAMETER_TEMPLATE :
            build_scope_template_template_parameter(a, template_parameter_list, nesting, template_context, nodecl_output);
            break;
        case AST_AMBIGUITY :
            // The ambiguity here is parameter_class vs parameter_decl
            solve_parameter_declaration_vs_type_parameter_class(a, template_context);
            // Restart this routine
            build_scope_template_parameter(a, template_parameter_list, nesting, template_context, nodecl_output);
            break;
        default :
            internal_error("Unknown node type '%s'", ast_print_node_type(ASTType(a)));
    }
}

static void build_scope_template_template_parameter(AST a,
        template_parameter_list_t* template_parameters,
        int nesting,
        decl_context_t template_context,
        nodecl_t* nodecl_output)
{
    // These parameters have the form
    
    //    TEMPLATE < template_param_list > CLASS [identifier] [= id_expr]
    //
    // "identifier" is then a template-name
    //
    // Construct parameter information
    decl_context_t template_params_context = template_context;

    template_params_context.template_parameters = counted_calloc(1, 
            sizeof(*template_params_context.template_parameters), &_bytes_used_buildscope);

    build_scope_template_parameter_list(ASTSon0(a), template_params_context.template_parameters,
            /* nesting */ 1, template_params_context, nodecl_output);

    ASTAttrSetValueType(a, LANG_IS_TEMPLATE_PARAMETER, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_IS_TEMPLATE_TEMPLATE_PARAMETER, tl_type_t, tl_bool(1));

    const char* template_parameter_name = NULL;
    if (ASTSon1(a) != NULL)
    {
        AST symbol = ASTSon1(a);
        template_parameter_name = ASTText(symbol);

        ASTAttrSetValueType(a, LANG_IS_NAMED_TEMPLATE_PARAMETER, tl_type_t, tl_bool(1));
        ast_set_link_to_child(a, LANG_TEMPLATE_PARAMETER_NAME, symbol);
    }
    else
    {
        char artificial_template_param_name[256];

        sprintf(artificial_template_param_name, 
                "__tpl_tpl_param_%d_%d__", nesting, template_parameters->num_parameters);

        template_parameter_name = uniquestr(artificial_template_param_name);
    }
    
    DEBUG_CODE()
    {
        fprintf(stderr, "[%d] Registering template template-parameter '%s'\n",
                template_parameters->num_parameters,
                template_parameter_name);
    }

    scope_entry_t* new_entry = counted_calloc(1, sizeof(*new_entry), &_bytes_used_buildscope);
    new_entry->symbol_name = template_parameter_name;
    new_entry->decl_context = template_context;

    ASTAttrSetValueType(a, LANG_TEMPLATE_PARAMETER_SYMBOL, tl_type_t, tl_symbol(new_entry));

    new_entry->line = ASTLine(a);
    new_entry->file = ASTFileName(a);
    new_entry->point_of_declaration = 
        (ASTSon1(a) == NULL) ? a : ASTSon1(a);

    new_entry->kind = SK_TEMPLATE_TEMPLATE_PARAMETER;

    new_entry->entity_specs.is_template_parameter = 1;
    new_entry->entity_specs.template_parameter_nesting = nesting;
    new_entry->entity_specs.template_parameter_position = template_parameters->num_parameters;

    // This is a faked class type
    type_t* primary_type = get_new_class_type(template_context, CK_CLASS);

    new_entry->type_information = get_new_template_type(template_params_context.template_parameters, 
            /* primary_type = */ primary_type, template_parameter_name, template_context,
            new_entry->line, new_entry->file);

    template_type_set_related_symbol(new_entry->type_information, new_entry);

    template_parameter_t *template_parameter = counted_calloc(1, sizeof(*template_parameter), &_bytes_used_buildscope);
    template_parameter->entry = new_entry;

    template_parameter_value_t* default_argument = NULL;

    AST id_expr = ASTSon2(a);
    if (id_expr != NULL)
    {
        // This might be ambiguous
        // check_expression(id_expr, template_context);

        scope_entry_list_t* entry_list = query_id_expression(template_context, id_expr);

        enum cxx_symbol_kind valid_templates_arguments[] = 
        { 
            SK_TEMPLATE,
            SK_TEMPLATE_TEMPLATE_PARAMETER
        };

        scope_entry_list_t* filtered_entry_list = 
            filter_symbol_kind_set(entry_list, STATIC_ARRAY_LENGTH(valid_templates_arguments), valid_templates_arguments);
        entry_list_free(entry_list);

        if (filtered_entry_list == NULL)
            running_error("%s: error: '%s' does not name a template class\n",
                    ast_location(id_expr),
                    prettyprint_in_buffer(id_expr));

        scope_entry_t* entry = entry_list_head(filtered_entry_list);
        entry_list_free(filtered_entry_list);

        if (entry->kind == SK_TEMPLATE
                    && named_type_get_symbol(template_type_get_primary_type(entry->type_information))->kind != SK_CLASS)
        {
            running_error("%s: error: '%s' does not name a template class\n",
                    ast_location(id_expr),
                    prettyprint_in_buffer(id_expr));
        }

        default_argument = counted_calloc(1, sizeof(*default_argument), 
                &_bytes_used_buildscope);
        // We need a named type
        default_argument->type = get_user_defined_type(entry);
        default_argument->is_default = 1;
        default_argument->kind = TPK_TEMPLATE;
    }

    template_parameter->kind = TPK_TEMPLATE;

    // We do this because P_LIST_ADD modifies the number of parameters
    int num_parameters = template_parameters->num_parameters;
    P_LIST_ADD(template_parameters->parameters, 
            num_parameters,
            template_parameter);
    P_LIST_ADD(template_parameters->arguments, 
            template_parameters->num_parameters,
            default_argument);
}

static void build_scope_type_template_parameter(AST a,
        template_parameter_list_t* template_parameters,
        int nesting,
        decl_context_t template_context,
        nodecl_t* nodecl_output)
{
    // These parameters have the form
    //    CLASS [name] [ = type_id]
    //    TYPENAME [name] [ = type_id]
    //
    // The trick here is create a simple_type that will be of type
    // STK_TYPE_TEMPLATE_PARAMETER. If it is named, register it in the symbol
    // table
    //
    // Create the type
    AST name = ASTSon0(a);
    AST type_id = ASTSon1(a);

    int line;
    const char *file;

    ASTAttrSetValueType(a, LANG_IS_TEMPLATE_PARAMETER, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_IS_TYPE_TEMPLATE_PARAMETER, tl_type_t, tl_bool(1));

    const char* template_parameter_name = NULL;
    if (name != NULL)
    {
        // This is a named type parameter. Register it in the symbol table
        DEBUG_CODE()
        {
            fprintf(stderr, "[%d] Registering type template-parameter '%s' with nesting %d\n",
                    template_parameters->num_parameters,
                    ASTText(name), 
                    nesting);
        }
        // Note that we sign it in the template_scope !
        template_parameter_name = ASTText(name);

        ASTAttrSetValueType(a, LANG_IS_NAMED_TEMPLATE_PARAMETER, tl_type_t, tl_bool(1));
        ast_set_link_to_child(a, LANG_TEMPLATE_PARAMETER_NAME, name);

        line = ASTLine(name);
        file = ASTFileName(name);
    }
    else
    {
        char template_param_name[256];
        sprintf(template_param_name, "__type_tpl__param_%d_%d__", nesting, 
                template_parameters->num_parameters);
        template_parameter_name = uniquestr(template_param_name);

        line = ASTLine(a);
        file = ASTFileName(a);
    }

    scope_entry_t* new_entry = counted_calloc(1, sizeof(*new_entry), &_bytes_used_buildscope);
    new_entry->decl_context = template_context;
    new_entry->symbol_name = template_parameter_name;

    ASTAttrSetValueType(a, LANG_TEMPLATE_PARAMETER_SYMBOL, tl_type_t, tl_symbol(new_entry));

    new_entry->line = line;
    new_entry->file = file;
    new_entry->point_of_declaration = name;
    new_entry->kind = SK_TEMPLATE_TYPE_PARAMETER;

    new_entry->entity_specs.is_template_parameter = 1;
    new_entry->entity_specs.template_parameter_nesting = nesting;
    new_entry->entity_specs.template_parameter_position = template_parameters->num_parameters;

    template_parameter_t* template_parameter = counted_calloc(1, sizeof(*template_parameter), &_bytes_used_buildscope);
    template_parameter->entry = new_entry;

    template_parameter_value_t *default_argument = NULL;
    if (type_id != NULL)
    {
        // This might be ambiguous, disambiguate
        AST type_specifier_seq = ASTSon0(type_id);
        AST abstract_decl = ASTSon1(type_id);

        type_t *type_info = NULL;

        gather_decl_spec_t gather_info;
        memset(&gather_info, 0, sizeof(gather_info));

        build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &type_info, template_context, nodecl_output);

        type_t* declarator_type = type_info;
        compute_declarator_type(abstract_decl, 
                &gather_info, type_info, &declarator_type,
                template_context,
                nodecl_output
                );

        default_argument = counted_calloc(1, sizeof(*default_argument), &_bytes_used_buildscope);
        default_argument->type = declarator_type;
        default_argument->is_default = 1;
        default_argument->kind = TPK_TYPE;
    }

    template_parameter->kind = TPK_TYPE;

    // We do this because P_LIST_ADD modifies the number of parameters
    int num_parameters = template_parameters->num_parameters;
    P_LIST_ADD(template_parameters->parameters, 
            num_parameters,
            template_parameter);
    P_LIST_ADD(template_parameters->arguments, 
            template_parameters->num_parameters,
            default_argument);
}

static void build_scope_nontype_template_parameter(AST a,
        template_parameter_list_t* template_parameters,
        int nesting,
        decl_context_t template_context,
        nodecl_t* nodecl_output)
{
    // As usual there are three parts
    //     decl_specifier_seq [declarator] [ = expression ]
    AST decl_specifier_seq = ASTSon0(a);
    AST parameter_declarator = ASTSon1(a);
    AST default_expression = ASTSon2(a);

    // Compute the type
    type_t *type_info = NULL;

    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));

    build_scope_decl_specifier_seq(decl_specifier_seq, &gather_info, &type_info, template_context, nodecl_output);

    scope_entry_t* entry = NULL;

    type_t* declarator_type = type_info;
    compute_declarator_type(parameter_declarator, 
            &gather_info, type_info, &declarator_type,
            template_context, nodecl_output);

    ASTAttrSetValueType(a, LANG_IS_TEMPLATE_PARAMETER, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_IS_NONTYPE_TEMPLATE_PARAMETER, tl_type_t, tl_bool(1));

    const char* template_parameter_name = NULL;
    AST declarator_name = get_declarator_name(parameter_declarator, template_context);
    if (declarator_name != NULL)
    {
        // This is a bit sloppy, this declarator should be a simple text
        template_parameter_name = uniquestr(prettyprint_in_buffer(declarator_name));
        DEBUG_CODE()
        {
            fprintf(stderr, "[%d] Registering '%s' as a non-type template parameter\n", 
                    template_parameters->num_parameters,
                    template_parameter_name);
        }
        ASTAttrSetValueType(a, LANG_IS_NAMED_TEMPLATE_PARAMETER, tl_type_t, tl_bool(1));
        ast_set_link_to_child(a, LANG_TEMPLATE_PARAMETER_NAME, declarator_name);
    }
    else
    {
        char template_param_name[256];

        sprintf(template_param_name, "__nontype_tpl_param_%d_%d__", 
                nesting, 
                template_parameters->num_parameters);
        template_parameter_name = uniquestr(template_param_name);
    }
    entry = counted_calloc(1, sizeof(*entry), &_bytes_used_buildscope);
    entry->symbol_name = template_parameter_name;
    entry->decl_context = template_context;

    ASTAttrSetValueType(a, LANG_TEMPLATE_PARAMETER_SYMBOL, tl_type_t, tl_symbol(entry));

    // This is not a variable, but a template parameter
    entry->kind = SK_TEMPLATE_PARAMETER;
    entry->type_information = declarator_type;
    entry->entity_specs.is_template_parameter = 1;
    entry->entity_specs.template_parameter_nesting = nesting;
    entry->entity_specs.template_parameter_position = template_parameters->num_parameters;

    // Save its symbol
    template_parameter_t* template_parameter = counted_calloc(1, sizeof(*template_parameter), &_bytes_used_buildscope);
    template_parameter->entry = entry;
    template_parameter_value_t* default_argument = NULL;
    if (default_expression != NULL)
    {
        if (!check_nontype_template_argument_expression(default_expression, template_context))
        {
            error_printf("%s: error: could not check default argument of template parameter '%s'\n",
                    ast_location(default_expression),
                    prettyprint_in_buffer(default_expression));
        }

        default_argument = counted_calloc(1, sizeof(*default_argument), &_bytes_used_buildscope);
        default_argument->value = expression_get_nodecl(default_expression);
        default_argument->type = declarator_type;
        default_argument->is_default = 1;
        default_argument->kind = TPK_NONTYPE;
    }

    template_parameter->kind = TPK_NONTYPE;

    // We do this because P_LIST_ADD modifies the number of parameters
    int num_parameters = template_parameters->num_parameters;
    P_LIST_ADD(template_parameters->parameters, 
            num_parameters,
            template_parameter);
    P_LIST_ADD(template_parameters->arguments, 
            template_parameters->num_parameters,
            default_argument);
}

static void build_scope_namespace_alias(AST a, decl_context_t decl_context)
{
    if (decl_context.current_scope->kind != NAMESPACE_SCOPE)
    {
        running_error("%s: error: namespace alias in a non namespace scope\n",
                ast_location(a));
    }

    AST alias_ident = ASTSon0(a);
    AST id_expression = ASTSon1(a);

    scope_entry_list_t* entry_list = query_id_expression(decl_context, id_expression);

    if (entry_list == NULL
            || entry_list_head(entry_list)->kind != SK_NAMESPACE)
    {
        running_error("%s: error: '%s' does not name any namespace\n", 
                ast_location(id_expression),
                prettyprint_in_buffer(id_expression));
    }

    scope_entry_t* entry = entry_list_head(entry_list);
    entry_list_free(entry_list);

    const char* alias_name = ASTText(alias_ident);

    scope_entry_t* alias_entry = new_symbol(decl_context, decl_context.current_scope, alias_name);

    alias_entry->line = ASTLine(alias_ident);
    alias_entry->file = ASTFileName(alias_ident);
    alias_entry->point_of_declaration = alias_ident;
    alias_entry->kind = SK_NAMESPACE;
    alias_entry->related_decl_context = entry->related_decl_context;
}

/*
 * This function builds symbol table information for a namespace definition
 */
static void build_scope_namespace_definition(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    AST namespace_name = ASTSon0(a);

    char is_inline = 0; 
    if (ASTType(a) == AST_GCC_NAMESPACE_DEFINITION)
    {
        // Technically this is not a gcc extension but c++0x
        AST namespace_inline = ASTSon3(a);

        if (namespace_inline != NULL)
        {
            ERROR_CONDITION(ASTType(namespace_inline) != AST_INLINE_SPEC,
                    "Invalid inline specifier tree", 0);

            is_inline = 1;
        }
    }

    if (namespace_name != NULL)
    {
        ERROR_CONDITION((decl_context.current_scope->kind != NAMESPACE_SCOPE), 
                "Incorrect scope, it should be a namespace scope", 0);

        // Register this namespace if it does not exist in this scope
        scope_entry_list_t* list = query_in_scope_str_flags(decl_context, ASTText(namespace_name), DF_ONLY_CURRENT_SCOPE);

        scope_entry_list_t* check_list = filter_symbol_non_kind(list, SK_NAMESPACE);

        if (check_list != NULL)
        {
            running_error("%s: error: '%s' has already been declared as another entity kind\n",
                    ast_location(namespace_name),
                    prettyprint_in_buffer(namespace_name));
        }
        entry_list_free(check_list);

        scope_entry_t* entry = NULL;
        decl_context_t namespace_context;
        if (list != NULL && 
                entry_list_head(list)->kind == SK_NAMESPACE)
        {
            entry = entry_list_head(list);
            namespace_context = entry->related_decl_context;

            if (is_inline
                    && !entry->entity_specs.is_inline)
            {
                running_error("%s: error: inline namespace extension of a non-inlined namespace\n",
                        ast_location(a));
            }
        }
        else
        {
            entry = new_symbol(decl_context, decl_context.current_scope, ASTText(namespace_name));
            namespace_context = new_namespace_context(decl_context, entry);

            entry->line = ASTLine(namespace_name);
            entry->file = ASTFileName(namespace_name);
            entry->point_of_declaration = namespace_name;
            entry->kind = SK_NAMESPACE;
            entry->related_decl_context = namespace_context;

            // Link the scope of this newly created namespace
            scope_link_set(CURRENT_COMPILED_FILE->scope_link, a, namespace_context);

            if (is_inline)
            {
                entry->entity_specs.is_inline = 1;

                // An inline namespace is an associated namespace of the current namespace
                scope_t* namespace_scope = decl_context.current_scope;

                P_LIST_ADD_ONCE(namespace_scope->use_namespace, namespace_scope->num_used_namespaces, 
                        entry);
            }
        }

        entry_list_free(list);

        if (ASTSon1(a) != NULL)
        {
            build_scope_declaration_sequence(ASTSon1(a), namespace_context, nodecl_output);
        }
    }
    else
    {
        // Register this namespace if it does not exist in this scope
        const char* unnamed_namespace = uniquestr("(unnamed)");
        scope_entry_list_t* list = query_in_scope_str_flags(decl_context, unnamed_namespace, DF_ONLY_CURRENT_SCOPE);

        decl_context_t namespace_context;
        if (list != NULL && 
                entry_list_head(list)->kind == SK_NAMESPACE)
        {
            scope_entry_t* entry = entry_list_head(list);

            entry_list_free(list);

            namespace_context = entry->related_decl_context;

            if (is_inline
                    && !entry->entity_specs.is_inline)
            {
                running_error("%s: error: inline namespace extension of a non-inlined namespace\n",
                        ast_location(a));
            }
        }
        else
        {
            scope_entry_t* entry = new_symbol(decl_context, decl_context.current_scope, unnamed_namespace);
            namespace_context = new_namespace_context(decl_context, entry);

            entry->line = ASTLine(a);
            entry->file = ASTFileName(a);
            entry->point_of_declaration = a;
            entry->kind = SK_NAMESPACE;
            entry->related_decl_context = namespace_context;

            // Link the scope of this newly created namespace
            scope_link_set(CURRENT_COMPILED_FILE->scope_link, a, namespace_context);

            // And associate it to the current namespace
            scope_t* namespace_scope = decl_context.current_scope;
            
            // Anonymous namespace is implemented as an associated namespace of the current scope
            P_LIST_ADD_ONCE(namespace_scope->use_namespace, namespace_scope->num_used_namespaces, 
                    entry);
        }

        build_scope_declaration_sequence(ASTSon1(a), namespace_context, nodecl_output);
    }
}


// This function is only intended for C99
void build_scope_kr_parameter_declaration(scope_entry_t* function_entry UNUSED_PARAMETER,
        AST kr_parameter_declaration, 
        AST kr_parameters,
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    CXX_LANGUAGE()
    {
        internal_error("This function is intended only for C99", 0);
    }


    // This can be empty, but undefined parameters must be signed up
    if (kr_parameter_declaration != NULL)
    {
        AST iter;
        for_each_element(kr_parameter_declaration, iter)
        {
            AST simple_decl = ASTSon1(iter);

            build_scope_simple_declaration(simple_decl, decl_context, 
                    /* is_template */ 0, /* is_explicit_instantiation */ 0,
                    nodecl_output, /* declared_symbols */ NULL);
        }
    }

    // Perform some adjustments. FIXME - YUse
    if (kr_parameters != NULL)
    {
        AST kr_parameter_list = ASTSon0(kr_parameters);
        int i = 0;
        AST iter;
        for_each_element(kr_parameter_list, iter)
        {
            AST kr_param = ASTSon1(iter);

            scope_entry_list_t* entry_list = query_in_scope(decl_context, kr_param);
            scope_entry_t* entry = NULL;
            if (entry_list == NULL)
            {
                // Sign in an integer
                entry = new_symbol(decl_context, decl_context.current_scope, ASTText(kr_param));
                entry->kind = SK_VARIABLE;
                entry->type_information = get_signed_int_type();
                entry->line = ASTLine(kr_param);
                entry->file = ASTFileName(kr_param);
                
                entry->point_of_declaration = kr_param;
            }
            else
            {
                entry = entry_list_head(entry_list);
            }
            entry_list_free(entry_list);

            entry->entity_specs.is_parameter = 1;
            entry->entity_specs.parameter_position = i;
            entry->defined = 1;

            if (entry->entity_specs.is_extern)
            {
                running_error("%s: error: parameter declared as 'extern'\n", 
                        ast_location(kr_param));
            }
            if (entry->entity_specs.is_static)
            {
                running_error("%s: error: parameter declared as 'static'\n", 
                        ast_location(kr_param));
            }

            i++;
        }
    }
}

static void common_defaulted_or_deleted(AST a, decl_context_t decl_context, 
        void (*check)(AST, scope_entry_t*, decl_context_t),
        void (*set)(AST, scope_entry_t*, decl_context_t),
        nodecl_t* nodecl_output)
{
    AST decl_spec_seq = ASTSon0(a);
    AST declarator = ASTSon1(a);
    /* AST attributes = ASTSon2(a); */

    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));
    type_t* type_info = NULL;

    char is_constructor = 0;

    if (decl_spec_seq != NULL)
    {
        build_scope_decl_specifier_seq(decl_spec_seq, &gather_info, &type_info, decl_context, nodecl_output);
    }
    else
    {
        if (is_constructor_declarator(ASTSon1(a)))
        {
            is_constructor = 1;
        }
    }

    // declarator
    type_t* declarator_type = NULL;
    scope_entry_t* entry = NULL;

    decl_context_t new_decl_context = decl_context;
    if (is_constructor)
    {
        new_decl_context.decl_flags |= DF_CONSTRUCTOR;
    }

    compute_declarator_type(declarator, &gather_info, type_info, &declarator_type, new_decl_context, nodecl_output);
    entry = build_scope_declarator_name(ASTSon1(a), declarator_type, &gather_info, new_decl_context, nodecl_output);

    if (check)
        check(a, entry, decl_context);

    if (entry->defined)
    {
        running_error("%s: function already defined at '%s'\n",
                ast_location(a),
                entry->file,
                entry->line);
    }
    entry->defined = 1;

    set(a, entry, decl_context);
}

static void set_deleted(AST a UNUSED_PARAMETER, scope_entry_t* entry, decl_context_t decl_context UNUSED_PARAMETER)
{
    entry->entity_specs.is_deleted = 1;
}

static void build_scope_deleted_function_definition(AST a, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    common_defaulted_or_deleted(a, decl_context, /* check */ NULL, set_deleted, nodecl_output);
}

static void check_defaulted(AST a, 
        scope_entry_t* entry, 
        decl_context_t decl_context) 
{ 
    if (!entry->entity_specs.is_default_constructor
            && !entry->entity_specs.is_copy_constructor
            && !entry->entity_specs.is_move_constructor
            && !entry->entity_specs.is_copy_assignment_operator
            && !entry->entity_specs.is_move_assignment_operator
            && !entry->entity_specs.is_destructor)
    {
        const char* qualified_name = get_qualified_symbol_name(entry, decl_context);

        running_error("%s: function '%s' cannot be defaulted\n",
                ast_location(a),
                print_decl_type_str(entry->type_information, 
                    decl_context,
                    qualified_name));
    }
}

static void set_defaulted(AST a UNUSED_PARAMETER, 
        scope_entry_t* entry, 
        decl_context_t decl_context UNUSED_PARAMETER)
{
    entry->entity_specs.is_defaulted = 1;
}

static void build_scope_defaulted_function_definition(AST a, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    common_defaulted_or_deleted(a, decl_context, check_defaulted, set_defaulted, nodecl_output);
}

/*
 * This function builds symbol table information for a function definition
 *  
 * If previous_symbol != NULL, the found symbol should match
 */
scope_entry_t* build_scope_function_definition(AST a, scope_entry_t* previous_symbol, 
        decl_context_t decl_context, 
        char is_template,
        char is_explicit_instantiation,
        nodecl_t* nodecl_output)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "Function definition!\n");
    }
    // A function definition has four parts
    //   decl_specifier_seq declarator ctor_initializer function_body

    // decl_specifier_seq [optional]
    // If there is no decl_specifier_seq this has to be a destructor, constructor or conversion function
    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));

    gather_info.is_template = is_template;
    gather_info.is_explicit_instantiation = is_explicit_instantiation;

    type_t* type_info = NULL;

    AST decl_spec_seq = ASTSon0(a);
    char is_constructor = 0;
    if (decl_spec_seq != NULL 
            && ((ASTType(decl_spec_seq) != AST_AMBIGUITY && ASTSon1(decl_spec_seq) != NULL)
             || (ASTType(decl_spec_seq) == AST_AMBIGUITY)))
    {
        build_scope_decl_specifier_seq(decl_spec_seq, &gather_info, &type_info, decl_context, nodecl_output);
    }
    else
    {
        CXX_LANGUAGE()
        {
            if (is_constructor_declarator(ASTSon1(a)))
            {
                is_constructor = 1;
            }
        }
        C_LANGUAGE()
        {
            // There is no decl specifier sequence at all
            if (decl_spec_seq == NULL
                    // Or its type is null
                    || ASTSon1(decl_spec_seq) == NULL)
            {
                if (decl_spec_seq == NULL)
                {
                    fprintf(stderr, "%s: warning: function definition does not have decl-specifier, assuming 'int'\n",
                            ast_location(a));
                }
                else
                {
                    fprintf(stderr, "%s: warning: function definition does not have type-specifier, assuming 'int'\n",
                            ast_location(a));
                }

                type_info = get_signed_int_type();
            }
        }
    }

    // declarator
    type_t* declarator_type = NULL;
    scope_entry_t* entry = NULL;

    decl_context_t new_decl_context = decl_context;
    if (is_constructor)
    {
        new_decl_context.decl_flags |= DF_CONSTRUCTOR;
    }

    decl_context_t block_context = new_block_context(decl_context);
    
    // This does not modify block_context.current_scope, it simply adds a function_scope to the context
    block_context = new_function_context(block_context);

    // block-context will be updated for qualified-id to reflect the exact context
    build_scope_declarator_with_parameter_context(ASTSon1(a), &gather_info, type_info, &declarator_type, 
            new_decl_context, &block_context, nodecl_output);
    entry = build_scope_declarator_name(ASTSon1(a), declarator_type, &gather_info, new_decl_context, nodecl_output);

    // ERROR_CONDITION((entry == NULL), "Function '%s' does not exist! %s", prettyprint_in_buffer(ASTSon1(a)), ast_location(a));
    if (entry == NULL)
    {
        running_error("%s: error: function '%s' was not found in the scope\n", 
                ast_location(a), 
                print_decl_type_str(declarator_type, new_decl_context, 
                    prettyprint_in_buffer(get_declarator_name(ASTSon1(a), new_decl_context))));
    }

    // Set the related entry
    block_context.current_scope->related_entry = entry;

    if (previous_symbol != NULL
            && previous_symbol != entry)
    {
        internal_error("inconsistent symbol created %s at '%s:%d' [%s] vs %s at '%s:%d' [%s] \n", 
                previous_symbol->symbol_name,
                previous_symbol->file,
                previous_symbol->line,
                print_declarator(previous_symbol->type_information),
                entry->symbol_name,
                entry->file,
                entry->line,
                print_declarator(entry->type_information)
                );
    }

    if (entry->defined)
    {
        const char *funct_name = entry->symbol_name;
        CXX_LANGUAGE()
        {
            const char* qualified_name = get_qualified_symbol_name(entry, decl_context);

            funct_name = get_declaration_string_internal(entry->type_information,
                    decl_context,
                    qualified_name, "", 0, 0, NULL, 0);
        }
        running_error("%s: error: function '%s' already defined (look at '%s:%d')\n",
                ast_location(a),
                funct_name,
                entry->file,
                entry->line);
    }

    // Inline may be added in a function definition
    entry->entity_specs.is_inline |= gather_info.is_inline;

    // Set defined now, otherwise some infinite recursion may happen when
    // instantiating template functions
    entry->defined = 1;
    entry->point_of_definition = get_enclosing_declaration(a);

    // Keep parameter names
    int i;
    for (i = 0; i < gather_info.num_parameters; i++)
    {
        // In C they must have name
        C_LANGUAGE()
        {
            if (gather_info.arguments_info[i].entry == NULL)
            {
                running_error("%s: error: parameter '%d' does not have name\n", 
                        ast_location(ASTSon1(a)),
                        i);
            }
        }
        P_LIST_ADD(entry->entity_specs.related_symbols,
                entry->entity_specs.num_related_symbols,
                gather_info.arguments_info[i].entry);
    }

    {
        // Function declaration name
        AST declarator_name = get_declarator_name(ASTSon1(a), decl_context);
        ast_set_link_to_child(a, LANG_FUNCTION_NAME, declarator_name);

        if (ASTType(declarator_name) == AST_QUALIFIED_ID)
        {
            AST global_qualif = ASTSon0(declarator_name);
            AST nested_name_spec = ASTSon1(declarator_name);
            AST unqualified_id = ASTSon2(declarator_name);

            ASTAttrSetValueType(declarator_name, LANG_IS_ID_EXPRESSION, tl_type_t, tl_bool(1));
            ASTAttrSetValueType(declarator_name, LANG_IS_QUALIFIED_ID, tl_type_t, tl_bool(1));

            if (global_qualif != NULL)
            {
                ASTAttrSetValueType(declarator_name, LANG_IS_GLOBAL_QUALIFIED, tl_type_t, tl_bool(1));
            }

            if (nested_name_spec != NULL)
            {
                ast_set_link_to_child(declarator_name, LANG_NESTED_NAME_SPECIFIER, nested_name_spec);
            }

            ast_set_link_to_child(declarator_name, LANG_UNQUALIFIED_ID, unqualified_id);

            if (ASTType(unqualified_id) == AST_TEMPLATE_ID)
            {
                ASTAttrSetValueType(unqualified_id, LANG_IS_TEMPLATE_ID, tl_type_t, tl_bool(1));
                ast_set_link_to_child(unqualified_id, LANG_TEMPLATE_NAME, ASTSon0(unqualified_id));
                ast_set_link_to_child(unqualified_id, LANG_TEMPLATE_ARGS, ASTSon1(unqualified_id));
            }
        }
        else if (ASTType(declarator_name) == AST_SYMBOL)
        {
            ASTAttrSetValueType(declarator_name, LANG_IS_UNQUALIFIED_ID, tl_type_t, tl_bool(1));
            ast_set_link_to_child(declarator_name, LANG_UNQUALIFIED_ID, declarator_name);
        }
        else if (ASTType(declarator_name) == AST_TEMPLATE_ID)
        {
            ASTAttrSetValueType(declarator_name, LANG_IS_TEMPLATE_ID, tl_type_t, tl_bool(1));
            ast_set_link_to_child(declarator_name, LANG_TEMPLATE_NAME, ASTSon0(declarator_name));
            ast_set_link_to_child(declarator_name, LANG_TEMPLATE_ARGS, ASTSon1(declarator_name));
        }
    }

    // The scope seen by this function definition
    scope_link_set(CURRENT_COMPILED_FILE->scope_link, a, decl_context);

    ERROR_CONDITION((entry->kind != SK_FUNCTION), 
            "This is not a function!!!", 0);

    ASTAttrSetValueType(a, LANG_IS_DECLARATION, tl_type_t, tl_bool(1));
    ast_set_link_to_child(a, LANG_DECLARATION_SPECIFIERS, ASTSon0(a));
    ast_set_link_to_child(a, LANG_DECLARATION_DECLARATORS, ASTSon1(a));
    ASTAttrSetValueType(a, LANG_FUNCTION_SYMBOL, tl_type_t, tl_symbol(entry));

    // Keep the function definition, it may be needed later
    entry->entity_specs.definition_tree = a;

    // Function_body
    AST function_body = ASTSon3(a);
    AST statement = ASTSon0(function_body);

    if (entry->entity_specs.is_member)
    {
        // If is a member function sign up additional information
        if (!entry->entity_specs.is_static)
        {
            // The class we belong to
            type_t* pointed_this = entry->entity_specs.class_type;
            // Qualify likewise the function unless it is a destructor
            if (!entry->entity_specs.is_destructor)
            {
                pointed_this = get_cv_qualified_type(pointed_this, get_cv_qualifier(entry->type_information));
            }

            type_t* this_type = get_pointer_type(pointed_this);
            // It is a constant pointer, so qualify like it is
            this_type = get_cv_qualified_type(this_type, CV_CONST);

            // This will put the symbol in the function scope, but this is fine
            scope_entry_t* this_symbol = new_symbol(block_context, block_context.current_scope, "this");

            this_symbol->line = ASTLine(function_body);
            this_symbol->file = ASTFileName(function_body);

            this_symbol->point_of_declaration = function_body;
            this_symbol->kind = SK_VARIABLE;
            this_symbol->type_information = this_type;
            this_symbol->defined = 1;
        }
    }

    C_LANGUAGE()
    {
        AST kr_parameter_declaration = ASTSon2(a);
        AST kr_parameter_list = get_function_declarator_parameter_list(ASTSon1(a), decl_context);

        if (kr_parameter_declaration != NULL
                || ASTType(kr_parameter_list) == AST_KR_PARAMETER_LIST)
        {
            build_scope_kr_parameter_declaration(entry, kr_parameter_declaration, 
                    kr_parameter_list, block_context, nodecl_output);
        }
    }

    nodecl_t nodecl_initializers = nodecl_null();
    CXX_LANGUAGE()
    {
        AST ctor_initializer = ASTSon2(a);
        if (entry->entity_specs.is_member
                && entry->entity_specs.is_constructor)
        {
            AST location = ctor_initializer;
            if (ctor_initializer == NULL)
                location = a;
            build_scope_ctor_initializer(ctor_initializer, entry, block_context, 
                    ASTFileName(location), ASTLine(location),
                    &nodecl_initializers);
        }
        else
        {
            if (ctor_initializer != NULL)
            {
                running_error("%s: error: member-initializer-lists are only valid in constructors\n");
            }
        }
    }

    // FIXME - Think how to make this better maintained
    if (CURRENT_CONFIGURATION->enable_cuda
            && (gather_info.cuda.is_global
                || gather_info.cuda.is_device))
    {
        cuda_kernel_symbols_for_function_body(function_body, &gather_info, decl_context, block_context);
    }

    // Fix inherited template context
    template_parameter_list_t* enclosing_template_parameters = block_context.template_parameters;
    block_context.template_parameters = counted_calloc(1, sizeof(*block_context.template_parameters), &_bytes_used_buildscope);
    block_context.template_parameters->enclosing = enclosing_template_parameters;

    // Sign in __func__ (C99) and GCC's __FUNCTION__ and __PRETTY_FUNCTION__
    {
        type_t* const_char_ptr_const_type =
            get_cv_qualified_type(
                    get_pointer_type(
                        get_cv_qualified_type(get_char_type(), CV_CONST)),
                    CV_CONST);

        // For C++ we should elaborate a bit more __PRETTY_FUNCTION__
        // but this is very gcc specific
        char c[256] = { 0 };
        snprintf(c, 255, "\"%s\"", entry->symbol_name);
        c[255] = '\0';
        AST function_name_tree = internal_expression_parse(c, block_context);

        const char* func_names[] = 
        {
            "__func__",
            "__FUNCTION__",
            "__PRETTY_FUNCTION__"
        };

        unsigned int j;
        for (j = 0; j < STATIC_ARRAY_LENGTH(func_names); j++)
        {
            scope_entry_t* func_var = new_symbol(block_context, block_context.current_scope, func_names[j]);
            func_var->kind = SK_VARIABLE;
            func_var->type_information = const_char_ptr_const_type;
            func_var->language_dependent_value = function_name_tree;
            func_var->value = expression_get_nodecl(function_name_tree);
            func_var->entity_specs.is_builtin = 1;
        }
    }

    nodecl_t body_nodecl = nodecl_null();
    if (ASTType(statement) == AST_COMPOUND_STATEMENT)
    {
        // We want to inherit the block context to this compound statement
        // so build_scope_statement cannot be used, because it would create
        // one for the compound statement
        AST list = ASTSon0(statement);
        if (list != NULL)
        {
            scope_link_set(CURRENT_COMPILED_FILE->scope_link, list, block_context);

            build_scope_statement_seq(list, block_context, &body_nodecl);
        }
        ASTAttrSetValueType(statement, LANG_IS_COMPOUND_STATEMENT, tl_type_t, tl_bool(1));
        ast_set_link_to_child(statement, LANG_COMPOUND_STATEMENT_LIST, list);

        nodecl_t nodecl_destructors = nodecl_null();
        CXX_LANGUAGE()
        {
            call_destructors_of_classes(block_context, &nodecl_destructors);
        }

        // We manually create a compound statement here for nodecl
        body_nodecl = nodecl_make_compound_statement(body_nodecl, nodecl_destructors, 
                new_scope_symbol(block_context), 
                ASTFileName(statement), ASTLine(statement));
    }
    else if (ASTType(statement) == AST_TRY_BLOCK)
    {
        // This only can be a try-except, but a normal context is created
        // for this one
        build_scope_statement(statement, block_context, &body_nodecl);
    }
    else
    {
        internal_error("Unreachable code", 0);
    }

    scope_link_set(CURRENT_COMPILED_FILE->scope_link, statement, block_context);

    ASTAttrSetValueType(a, LANG_IS_FUNCTION_DEFINITION, tl_type_t, tl_bool(1));
    {
        // AST non_nested_declarator = advance_over_declarator_nests(ASTSon1(a), decl_context);
        ast_set_link_to_child(a, LANG_FUNCTION_DECLARATOR, ASTSon1(a));
    }
    ast_set_link_to_child(a, LANG_FUNCTION_BODY, statement);

    // Create nodecl (only if not dependent)
    if (!is_dependent_type(entry->type_information)
            && !(entry->entity_specs.is_member
                && is_dependent_type(entry->entity_specs.class_type)))
    {
        nodecl_t nodecl_function_def = nodecl_make_function_code(
                nodecl_make_list_1(body_nodecl), 
                nodecl_initializers,
                /* internal_functions */ nodecl_null(),
                entry,
                ASTFileName(a), ASTLine(a));
        *nodecl_output = nodecl_make_list_1(nodecl_function_def);
    }

    return entry;
}

static void build_scope_member_declaration(decl_context_t inner_decl_context,
        AST a, access_specifier_t current_access, 
        type_t* class_info,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols)
{
    DEBUG_CODE()
    {
    fprintf(stderr, "==== Member declaration line: [%s] ====\n",
            ast_location(a));
    }
    switch (ASTType(a))
    {
        case AST_MEMBER_DECLARATION :
            {
                build_scope_member_simple_declaration(inner_decl_context, a, current_access, class_info, 
                        /* is_template */ 0, /* is_explicit_instantiation */ 0,
                        nodecl_output, declared_symbols);
                break;
            }
        case AST_FUNCTION_DEFINITION :
            {
                build_scope_member_function_definition(inner_decl_context, a, current_access, class_info, 
                        /* is_template */ 0, /* is_explicit_instantiation */ 0,
                        nodecl_output);
                break;
            }
        case AST_DEFAULTED_FUNCTION_DEFINITION:
        case AST_DELETED_FUNCTION_DEFINITION:
            {
                build_scope_default_or_delete_member_function_definition(inner_decl_context, a, current_access, class_info, nodecl_output);
                break;
            }
        case AST_GCC_EXTENSION : // __extension__
            {
                build_scope_member_declaration(inner_decl_context, ASTSon0(a), current_access, class_info, nodecl_output, 
                        declared_symbols);
                // FIXME Enable a gcc_extension bit for these symbols
                break;
            }
        case AST_TEMPLATE_DECLARATION :
            {
                build_scope_member_template_declaration(inner_decl_context, a, current_access, 
                        class_info, /* is_explicit_instantiation */ 0, nodecl_output);
                break;
            }
        case AST_USING_DECLARATION :
            {
                build_scope_using_declaration(a, inner_decl_context, current_access, nodecl_output);
                break;
            }
        case AST_MEMBER_DECLARATION_QUALIF:
            {
                // This is a deprecated syntax meaning the same of AST_USING_DECLARATION
                build_scope_member_declaration_qualified(a, inner_decl_context, current_access, nodecl_output);
                break;
            }
        case AST_STATIC_ASSERT:
            {
                build_scope_static_assert(a, inner_decl_context);
                break;
            }
        case AST_AMBIGUITY :
            {
                solve_ambiguous_declaration(a, inner_decl_context);
                // Restart
                build_scope_member_declaration(inner_decl_context, a, current_access, class_info, nodecl_output, declared_symbols);
                break;
            }
        case AST_EMPTY_DECL :
            {
                break;
            }
        case AST_UNKNOWN_PRAGMA :
            {
                // What can we do here?
                // FIXME!
                break;
            }
        case AST_VERBATIM :
            {
                break;
            }
        case AST_PRAGMA_CUSTOM_DIRECTIVE :
            {
                build_scope_pragma_custom_directive(a, inner_decl_context, NULL, nodecl_output);
                break;
            }
        case AST_PRAGMA_CUSTOM_CONSTRUCT: 
            {
                build_scope_pragma_custom_construct_member_declaration(a, inner_decl_context, current_access, class_info, nodecl_output);
                break;
            }
        default:
            {
                internal_error("Unsupported node '%s' (%s)\n", ast_print_node_type(ASTType(a)),
                        ast_location(a));
                break;
            }
    }
}

/*
 * This function registers a member template declaration
 */
static void build_scope_member_template_declaration(decl_context_t decl_context, AST a, 
        access_specifier_t current_access, type_t* class_info,
        char is_explicit_instantiation,
        nodecl_t* nodecl_output
        )
{
    /*
     * The declaration after the template parameter list can be
     * a simple declaration or a function definition.
     *
     * For the case of a simple_declaration, the following are examples
     * of what can appear there
     *
     *   template <class P, class Q>
     *   class A                 // A primary template class
     *   {
     *   };
     *
     *   template <class P>
     *   class A<P, int>         // A partial specialized class
     *   {
     *   };
     *
     *   template <class P>
     *   T A<P>::d = expr;       // For static member initialization
     *   
     *   template <class P>           
     *   void f(..., P q, ...);  // Function declaration
     *
     * Template classes are saved in a special form since the may be
     * specialized in several ways.
     *
     */

    /*
     * Template parameter information is constructed first
     */
    decl_context_t template_context;
    build_scope_template_header(ASTSon0(a), decl_context, &template_context, nodecl_output);
    
    AST templated_decl = ASTSon1(a);
    if (ASTType(templated_decl) == AST_AMBIGUITY)
    {
        solve_ambiguous_declaration(templated_decl, template_context);
    }

    // Link the AST with the scope
    scope_link_set(CURRENT_COMPILED_FILE->scope_link, a, template_context);

    switch (ASTType(templated_decl))
    {
        case AST_FUNCTION_DEFINITION :
            {
                build_scope_member_template_function_definition(template_context, ASTSon1(a), current_access, class_info, 
                        is_explicit_instantiation, nodecl_output);
                ASTAttrSetValueType(ASTSon1(a), LANG_IS_TEMPLATED_FUNCTION_DEFINITION, tl_type_t, tl_bool(1));
            }
            break;
        case AST_SIMPLE_DECLARATION :
            {
                build_scope_member_template_simple_declaration(template_context, ASTSon1(a), current_access, class_info, 
                        is_explicit_instantiation, nodecl_output);
                ASTAttrSetValueType(ASTSon1(a), LANG_IS_TEMPLATED_DECLARATION, tl_type_t, tl_bool(1));
                break;
            }
        default :
            internal_error("Unknown node type '%s'\n", ast_print_node_type(ASTType(a)));
    }

    ASTAttrSetValueType(ASTSon0(a), LANG_IS_TEMPLATE_HEADER, tl_type_t, tl_bool(1));
    ast_set_link_to_child(ASTSon1(a), LANG_TEMPLATE_HEADER, a);
}

static void build_scope_member_template_function_definition(decl_context_t decl_context,
        AST a, access_specifier_t current_access, type_t* class_info, 
        char is_explicit_instantiation,
        nodecl_t* nodecl_output)
{
    // Define the function within st scope but being visible template_scope
    build_scope_member_function_definition(decl_context, a, current_access, class_info, 
            /* is_template */ 1, is_explicit_instantiation,
            nodecl_output);
}

static void build_scope_member_template_simple_declaration(decl_context_t decl_context,
        AST a, access_specifier_t current_access, type_t* class_info,
        char is_explicit_instantiation,
        nodecl_t* nodecl_output)
{
    build_scope_member_simple_declaration(decl_context, a, current_access, class_info, 
            /* is_template */ 1, is_explicit_instantiation,
            nodecl_output, /* declared_symbols */ NULL);
}

static char is_copy_assignment_operator(scope_entry_t* entry, type_t* class_type)
{
    // Remember copy assignment operators
    if ((strcmp(entry->symbol_name, STR_OPERATOR_ASSIGNMENT) == 0)
            && (function_type_get_num_parameters(entry->type_information) == 1))
    {
        type_t* first_parameter = function_type_get_parameter_type_num(entry->type_information, 0);

        // Check that its form is either
        //
        //  operator=(cv T&)
        //  operator=(T)
        if ((is_lvalue_reference_to_class_type(first_parameter)
                    && equivalent_types(get_actual_class_type(class_type),
                        get_actual_class_type(get_unqualified_type(reference_type_get_referenced_type(first_parameter)))))
                || (is_class_type(first_parameter) 
                    && equivalent_types(get_actual_class_type(class_type), get_actual_class_type(first_parameter))))
        {
            return 1;
        }
    }
    return 0;
}

static char is_move_assignment_operator(scope_entry_t* entry, type_t* class_type)
{
    // Remember copy assignment operators
    if ((strcmp(entry->symbol_name, STR_OPERATOR_ASSIGNMENT) == 0)
            && (function_type_get_num_parameters(entry->type_information) == 1))
    {
        type_t* first_parameter = function_type_get_parameter_type_num(entry->type_information, 0);

        // Check that its form is either
        //
        //  operator=(cv T&&)
        if (is_rvalue_reference_to_class_type(first_parameter)
                    && equivalent_types(get_actual_class_type(class_type),
                        get_actual_class_type(get_unqualified_type(reference_type_get_referenced_type(first_parameter)))))
        {
            return 1;
        }
    }
    return 0;
}

static char is_copy_constructor(scope_entry_t* entry, type_t* class_type)
{
    if (entry->entity_specs.is_constructor
            && can_be_called_with_number_of_arguments(entry, 1)
            // It might be callable with one parameter because of A(...) 
            // [but note that A(const A&, ...) is a valid copy constructor]
            && !(function_type_get_has_ellipsis(entry->type_information)
                && function_type_get_num_parameters(entry->type_information) == 1))
    {
        type_t* first_parameter = function_type_get_parameter_type_num(entry->type_information, 0);
        // Check that its form is either
        //
        // A(const A&, X = x);
        // A(A&, X = x);

        if (is_lvalue_reference_to_class_type(first_parameter)
                    && equivalent_types(get_actual_class_type(class_type),
                        get_actual_class_type(get_unqualified_type(reference_type_get_referenced_type(first_parameter)))))
        {
            return 1;
        }
    }
    return 0;
}

static char is_move_constructor(scope_entry_t* entry, type_t* class_type)
{
    if (entry->entity_specs.is_constructor
            && can_be_called_with_number_of_arguments(entry, 1)
            // It might be callable with one parameter because of A(...) 
            // [but note that A(const A&, ...) is a valid copy constructor]
            && !(function_type_get_has_ellipsis(entry->type_information)
                && function_type_get_num_parameters(entry->type_information) == 1))
    {
        type_t* first_parameter = function_type_get_parameter_type_num(entry->type_information, 0);
        // Check that its form is either
        //
        // A(const A&&, X = x);
        // A(A&&, X = x);

        if (is_rvalue_reference_to_class_type(first_parameter)
                    && equivalent_types(get_actual_class_type(class_type),
                        get_actual_class_type(get_unqualified_type(reference_type_get_referenced_type(first_parameter)))))
        {
            return 1;
        }
    }
    return 0;
}

static char is_virtual_destructor(type_t* class_type)
{
    // If any base has virtual destructor, so it is the current one
    int i;
    for (i = 0; i < class_type_get_num_bases(class_type); i++)
    {
        char is_virtual = 0;
        char is_dependent = 0;
        access_specifier_t access_specifier = AS_UNKNOWN;
        scope_entry_t* base_class = class_type_get_base_num(class_type, i, 
                &is_virtual, &is_dependent, &access_specifier);

        if (is_dependent)
            continue;

        type_t* base_class_type = get_actual_class_type(base_class->type_information);

        scope_entry_t* destructor = class_type_get_destructor(base_class_type);

        if (destructor->entity_specs.is_virtual)
            return 1;
    }

    return 0;
}

static void update_member_function_info(AST declarator_name, 
        char is_constructor, 
        scope_entry_t* entry, 
        type_t* class_type)
{
    // Update information in the class about this member function
    switch (ASTType(declarator_name))
    {
        case AST_SYMBOL :
            {
                entry->entity_specs.is_user_declared = 1;
                if (is_constructor)
                {
                    // This is a constructor
                    entry->entity_specs.is_constructor = 1;

                    DEBUG_CODE()
                    {
                        fprintf(stderr, "BUILDSCOPE: Symbol '%s' at '%s:%d' is a constructor\n", 
                                entry->symbol_name,
                                entry->file,
                                entry->line);
                    }

                    if (!entry->entity_specs.is_explicit
                            && can_be_called_with_number_of_arguments(entry, 1))
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "BUILDSCOPE: Symbol '%s' at '%s:%d' is a conversor constructor\n", 
                                    entry->symbol_name,
                                    entry->file,
                                    entry->line);
                        }
                        entry->entity_specs.is_conversor_constructor = 1;
                    }

                    if (!entry->entity_specs.is_explicit
                            && can_be_called_with_number_of_arguments(entry, 0))
                    {
                        entry->entity_specs.is_default_constructor = 1;
                        class_type_set_default_constructor(class_type, entry);
                    }

                    if (is_copy_constructor(entry, class_type))
                    {
                        entry->entity_specs.is_copy_constructor = 1;
                    }

                    CXX1X_LANGUAGE()
                    {
                        if (is_move_constructor(entry, class_type))
                        {
                            entry->entity_specs.is_move_constructor = 1;
                        }
                    }
                }
                break;
            }
            // Special members
        case AST_DESTRUCTOR_TEMPLATE_ID : // This can appear here
        case AST_DESTRUCTOR_ID :
            {
                // This is the destructor
                if (entry->entity_specs.is_virtual
                        || is_virtual_destructor(class_type))
                {
                    entry->entity_specs.is_virtual = 1;
                }
                entry->entity_specs.is_destructor = 1;
                entry->entity_specs.is_user_declared = 1;
                class_type_set_destructor(get_actual_class_type(class_type), entry);
                break;
            }
        case AST_OPERATOR_FUNCTION_ID :
        case AST_OPERATOR_FUNCTION_ID_TEMPLATE :
            {
                if (is_copy_assignment_operator(entry, class_type))
                {
                    entry->entity_specs.is_copy_assignment_operator = 1;
                    entry->entity_specs.is_user_declared = 1;
                }

                CXX1X_LANGUAGE()
                {
                    if (is_move_assignment_operator(entry, class_type))
                    {
                        entry->entity_specs.is_move_assignment_operator = 1;
                    }
                }

                // These are always static
                if (ASTType(ASTSon0(declarator_name)) == AST_NEW_OPERATOR
                        || ASTType(ASTSon0(declarator_name)) == AST_DELETE_OPERATOR)
                {
                    entry->entity_specs.is_static = 1;
                }
                break;
            }
        case AST_CONVERSION_FUNCTION_ID :
            {
                entry->entity_specs.is_conversion = 1;
                break;
            }
        case AST_QUALIFIED_ID :
        case AST_TEMPLATE_ID :
            {
                internal_error("Unreachable code", 0);
                break;
            }
        default :
            {
                internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(declarator_name)));
                break;
            }
    }
}

static void hide_using_declarations(type_t* class_info, scope_entry_t* currently_declared)
{
    decl_context_t class_context = class_type_get_inner_context(class_info);

    scope_entry_list_t* member_functions = query_in_scope_str(class_context, currently_declared->symbol_name);

    scope_entry_t* hidden = NULL;

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(member_functions);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* orig_entry = entry_list_iterator_current(it);

        if (orig_entry->kind == SK_USING)
        {
            scope_entry_t* entry = entry_advance_aliases(orig_entry);
            if (entry->kind == SK_FUNCTION
                    && function_type_same_parameter_types(entry->type_information, 
                        currently_declared->type_information))
            {
                hidden = orig_entry;
            }
        }
    }
    entry_list_iterator_free(it);
    entry_list_free(member_functions);

    if (hidden != NULL)
    {
        remove_entry(class_context.current_scope, hidden);
    }
}

/*
 * This is a function definition inlined in a class
 */
static scope_entry_t* build_scope_member_function_definition(decl_context_t decl_context, AST a, 
        access_specifier_t current_access, 
        type_t* class_info,
        char is_template,
        char is_explicit_instantiation,
        nodecl_t* nodecl_output)
{
    type_t* class_type = NULL;

    if (is_named_type(class_info))
    {
        class_type = named_type_get_symbol(class_info)->type_information;
    }
    else if (is_unnamed_class_type(class_info))
    {
        class_type = class_info;
    }
    else
    {
        internal_error("Type is not a class type", 0);
    }

    scope_entry_t* entry = NULL;
    // Handle this as if it was a plain declaration
    // decl_specifier_seq [optional]
    // If there is no decl_specifier_seq this has to be a destructor, constructor or conversion function
    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));

    gather_info.is_template = is_template;
    gather_info.is_explicit_instantiation = is_explicit_instantiation;

    type_t* type_info = NULL;

    AST declarator = ASTSon1(a);
    // Get the declarator name
    AST declarator_name = get_declarator_name(declarator, decl_context);

    char is_constructor = 0;
    AST decl_spec_seq = ASTSon0(a);

    // If ambiguous is due because we don't know how to "lay" the type_specifier
    // but it has type_specifier
    if (decl_spec_seq != NULL 
            && ((ASTType(decl_spec_seq) != AST_AMBIGUITY && ASTSon1(decl_spec_seq) != NULL)
                || (ASTType(decl_spec_seq) == AST_AMBIGUITY)))
    {
        build_scope_decl_specifier_seq(decl_spec_seq, &gather_info, &type_info,
                decl_context, nodecl_output);

    }
    else
    {
        // This is a constructor
        if (is_constructor_declarator(declarator))
        {
            is_constructor = 1;
        }
    }

    // declarator
    type_t* declarator_type = NULL;

    if (is_constructor)
    {
        decl_context.decl_flags |= DF_CONSTRUCTOR;
    }

    compute_declarator_type(ASTSon1(a), &gather_info, type_info, &declarator_type, decl_context, nodecl_output);
    entry = build_scope_declarator_name(ASTSon1(a), declarator_type, &gather_info, decl_context, nodecl_output);

    ERROR_CONDITION(entry == NULL, "Invalid entry computed", 0);

    entry->entity_specs.access = current_access;
    entry->entity_specs.is_defined_inside_class_specifier = 1;

    update_member_function_info(declarator_name, is_constructor, entry, class_type);

    DEBUG_CODE()
    {
        fprintf(stderr, "Setting member function definition at '%s' of '%s' as a member\n", 
                ast_location(a),
                entry->symbol_name); 
    }
    entry->entity_specs.is_member = 1;
    entry->entity_specs.access = current_access;
    entry->entity_specs.class_type = class_info;
    class_type_add_member(get_actual_class_type(class_type), entry);

    // This function might be hiding using declarations, remove those
    hide_using_declarations(class_info, entry);

    build_scope_delayed_add_delayed_function_def(a, entry, decl_context, is_template, is_explicit_instantiation);

    return entry;
}


static void build_scope_default_or_delete_member_function_definition(decl_context_t decl_context, AST a,
        access_specifier_t current_access, type_t* class_info,
        nodecl_t* nodecl_output)
{
    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));

    type_t* class_type = get_actual_class_type(class_info);
    const char* class_name = named_type_get_symbol(class_info)->symbol_name;

    AST decl_spec_seq = ASTSon0(a);
    AST declarator = ASTSon1(a);

    // AST attributes = ASTSon2(a);
    type_t* member_type = NULL;

    decl_context_t new_decl_context = decl_context;

    if (decl_spec_seq != NULL)
    {
        build_scope_decl_specifier_seq(decl_spec_seq, &gather_info,
                &member_type, new_decl_context, nodecl_output);
    }

    AST declarator_name = get_declarator_name(declarator, decl_context);
    char is_constructor = 0;
    if (is_constructor_declarator(declarator))
    {
        if (strcmp(class_name, ASTText(declarator_name)) == 0)
        {
            is_constructor = 1;
        }
    }

    if (is_constructor)
    {
        new_decl_context.decl_flags |= DF_CONSTRUCTOR;
    }

    type_t* declarator_type = NULL;

    compute_declarator_type(declarator, &gather_info, 
            member_type, &declarator_type, 
            new_decl_context, nodecl_output);
    scope_entry_t *entry =
        build_scope_declarator_name(declarator,
                declarator_type, &gather_info,
                new_decl_context, nodecl_output);

    ERROR_CONDITION(entry == NULL, "Invalid entry computed", 0);

    entry->entity_specs.access = current_access;

    ERROR_CONDITION(entry->kind != SK_FUNCTION, "Invalid symbol for default/delete", 0);

    update_member_function_info(declarator_name, is_constructor, entry, class_type);

    switch (ASTType(a))
    {
        case AST_DEFAULTED_FUNCTION_DEFINITION :
            {
                check_defaulted(a, entry, decl_context);
                set_defaulted(a, entry, decl_context);
                break;
            }
        case AST_DELETED_FUNCTION_DEFINITION :
            {
                set_deleted(a, entry, decl_context);
                break;
            }
        default:
            {
                internal_error("Code unreachable", 0);
            }
    }
}

static void build_scope_friend_declarator(decl_context_t decl_context, 
        gather_decl_spec_t *gather_info,
        type_t* class_type,
        type_t* member_type, AST declarator)
{
    nodecl_t nodecl_output = nodecl_null();

    decl_context.current_scope = decl_context.namespace_scope;

    type_t* declarator_type = NULL;
    compute_declarator_type(ASTSon0(declarator), gather_info, 
            member_type, &declarator_type, 
            decl_context, &nodecl_output);

    scope_entry_t *entry =
        build_scope_declarator_name(ASTSon0(declarator),
                declarator_type, gather_info,
                decl_context, &nodecl_output);

    if (entry->kind != SK_FUNCTION)
    {
        running_error("%s: error: invalid friend declaration, does not name a function\n",
                declarator);
    }

    class_type_add_friend_symbol(class_type, entry);
}


/*
 * This is a member declaration inlined in a class, not a function definition
 */
static void build_scope_member_simple_declaration(decl_context_t decl_context, AST a, 
        access_specifier_t current_access, type_t* class_info, 
        char is_template,
        char is_explicit_instantiation,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols)
{
    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));

    gather_info.is_template = is_template;
    gather_info.is_explicit_instantiation = is_explicit_instantiation;

    type_t* class_type = NULL;
    const char* class_name = "";
    if (is_named_type(class_info))
    {
        class_type = get_actual_class_type(class_info);
        class_name = named_type_get_symbol(class_info)->symbol_name;
    }
    else
    {
        internal_error("Invalid class type", 0);
    }

    type_t* member_type = NULL;

    if (ASTSon0(a) != NULL)
    {
        decl_context_t new_decl_context = decl_context;
        if (ASTSon1(a) == NULL)
        {
            gather_info.no_declarators = 1;
        }

        build_scope_decl_specifier_seq(ASTSon0(a), &gather_info,
                &member_type, new_decl_context, nodecl_output);

        if (gather_info.defined_type != NULL
                && declared_symbols != NULL)
        {
            *declared_symbols = entry_list_add(*declared_symbols, gather_info.defined_type);
        }

        AST decl_spec_seq = ASTSon0(a);
        AST type_specifier = ASTSon1(decl_spec_seq);

        if (member_type != NULL
                && is_named_type(member_type))
        {
            // Register the typename properly
            // FIXME Rewrite this using gather_info
            if (type_specifier != NULL
                    && !gather_info.is_friend
                    && (ASTType(type_specifier) == AST_CLASS_SPECIFIER // class A { } [x];
                        // class A; (no declarator)
                        || ((ASTType(type_specifier) == AST_ELABORATED_TYPE_CLASS_SPEC 
                                // class __attribute__((foo)) A; (no declarator)
                                || ASTType(type_specifier) == AST_GCC_ELABORATED_TYPE_CLASS_SPEC) 
                            && (ASTSon1(a) == NULL))
                        // enum E { } [x];
                        || ASTType(type_specifier) == AST_ENUM_SPECIFIER
                        // enum __attribute__((foo)) E { } [x];
                        || ASTType(type_specifier) == AST_GCC_ENUM_SPECIFIER
                        // enum E; (no declarator)
                        || ((ASTType(type_specifier) == AST_ELABORATED_TYPE_ENUM_SPEC
                                // enum  __attribute__((foo)) E; (no declarator)
                                || ASTType(type_specifier) == AST_GCC_ELABORATED_TYPE_ENUM_SPEC) 
                            && (ASTSon1(a) == NULL))))
            {
                scope_entry_t* entry = named_type_get_symbol(member_type);
                DEBUG_CODE()
                {
                    fprintf(stderr, "Setting type '%s' as member\n", 
                            entry->symbol_name);
                }

                char is_elaborated = (ASTType(type_specifier) == AST_ELABORATED_TYPE_CLASS_SPEC)
                    || (ASTType(type_specifier) == AST_GCC_ELABORATED_TYPE_CLASS_SPEC)
                    || (ASTType(type_specifier) == AST_ELABORATED_TYPE_ENUM_SPEC)
                    || (ASTType(type_specifier) == AST_GCC_ELABORATED_TYPE_ENUM_SPEC);

                entry->entity_specs.is_member = 1;
                entry->entity_specs.access = current_access;
                entry->entity_specs.is_defined_inside_class_specifier = !is_elaborated;
                entry->entity_specs.class_type = class_info;
                class_type_add_member(get_actual_class_type(class_type), entry);

                // Register enumerators as well
                if ((ASTType(type_specifier) == AST_ENUM_SPECIFIER 
                            || ASTType(type_specifier) == AST_GCC_ENUM_SPECIFIER))
                {
                    ERROR_CONDITION(!is_enum_type(member_type), 
                            "AST_ENUM_SPECIFIER did not compute an enumerator type", 0);

                    int i, num_enums  = enum_type_get_num_enumerators(member_type);
                    for (i = 0; i < num_enums; i++)
                    {
                        scope_entry_t* enumerator = enum_type_get_enumerator_num(member_type, i);

                        enumerator->entity_specs.is_member = 1;
                        enumerator->entity_specs.access = current_access;
                        enumerator->entity_specs.is_defined_inside_class_specifier = 1;
                        enumerator->entity_specs.class_type  = class_info;
                        class_type_add_member(get_actual_class_type(class_type), enumerator);
                    }
                }

                // Update the type specifier to be a dependent typename
                if (is_dependent_type(class_type))
                {
                    member_type = get_dependent_typename_type_from_parts(entry, NULL);
                }
            }
        }
    }

    if (ASTSon1(a) != NULL)
    {
        ASTAttrSetValueType(a, LANG_IS_DECLARATION, tl_type_t, tl_bool(1));
        ast_set_link_to_child(a, LANG_DECLARATION_SPECIFIERS, ASTSon0(a));
        ast_set_link_to_child(a, LANG_DECLARATION_DECLARATORS, ASTSon1(a));

        AST list = ASTSon1(a);
        AST iter;

        for_each_element(list, iter)
        {
            AST declarator = ASTSon1(iter);
            char is_constructor = 0;

            switch (ASTType(declarator))
            {
                case AST_AMBIGUITY:
                    {
                        solve_ambiguous_init_declarator(declarator, decl_context);
                        // Restart the function
                        build_scope_member_simple_declaration(decl_context, a, current_access, class_info, 
                                is_template, is_explicit_instantiation,
                                nodecl_output, declared_symbols);
                        return;
                        break;
                    }
                case AST_GCC_BITFIELD_DECLARATOR :
                case AST_BITFIELD_DECLARATOR :
                    {
                        if (gather_info.is_friend)
                        {
                            running_error("%s: error: a bit-field cannot be declared as friend\n",
                                    ast_location(declarator));
                        }

                        if (ASTType(declarator) == AST_GCC_BITFIELD_DECLARATOR)
                        {
                            AST attribute_list = ASTSon0(a);
                            gather_gcc_attribute_list(attribute_list, &gather_info, decl_context);
                        }

                        AST identifier = ASTSon0(declarator);
                        type_t* declarator_type = member_type;

                        scope_entry_t* bitfield_symbol = NULL;
                        compute_declarator_type(identifier, &gather_info, 
                                member_type, &declarator_type, 
                                decl_context, nodecl_output);

                        if (identifier != NULL)
                        {
                            bitfield_symbol = build_scope_declarator_name(identifier, declarator_type, &gather_info, 
                                    decl_context, nodecl_output);
                            ASTAttrSetValueType(declarator, LANG_IS_DECLARED_NAME, tl_type_t, tl_bool(1));

                            AST declarator_name = get_declarator_name(identifier, decl_context);
                            ast_set_link_to_child(declarator, LANG_DECLARED_NAME, declarator_name);
                        }
                        else
                        {
                            // Invent some name to sign it up because we will
                            // need it when computing the size of a class
                            bitfield_symbol = new_symbol(decl_context, decl_context.current_scope, 
                                    get_unique_name());
                            bitfield_symbol->kind = SK_VARIABLE;
                            bitfield_symbol->type_information = declarator_type;
                            // Remember that is unnamed, this is relevant for the size
                            bitfield_symbol->entity_specs.is_unnamed_bitfield = 1;
                        }

                        bitfield_symbol->entity_specs.access = current_access;
                        bitfield_symbol->entity_specs.is_member = 1;
                        bitfield_symbol->entity_specs.class_type = class_info;
                        class_type_add_member(get_actual_class_type(class_type), bitfield_symbol);

                        if (gather_info.is_static)
                        {
                            running_error("%s: error: a bitfield declaration cannot be static", 
                                    ast_location(declarator));
                        }

                        AST expression = ASTSon1(declarator);
                        if (!check_expression(expression, decl_context))
                        {
                            error_printf("%s: error: could not check bitfield size expression '%s'\n",
                                    ast_location(expression),
                                    prettyprint_in_buffer(expression));
                        }

                        bitfield_symbol->entity_specs.is_bitfield = 1;
                        bitfield_symbol->entity_specs.bitfield_expr = expression;
                        bitfield_symbol->entity_specs.bitfield_expr_context = decl_context;

                        bitfield_symbol->defined = 1;
                        bitfield_symbol->point_of_declaration = get_enclosing_declaration(declarator);
                        bitfield_symbol->point_of_definition = get_enclosing_declaration(declarator);

                        if (declared_symbols != NULL)
                        {
                            *declared_symbols = entry_list_add(*declared_symbols, bitfield_symbol);
                        }

                        break;
                    }
                    // init declarator may appear here because of templates
                case AST_INIT_DECLARATOR :
                case AST_MEMBER_DECLARATOR :
                case AST_GCC_MEMBER_DECLARATOR :
                    {
                        if (ASTType(declarator) == AST_GCC_MEMBER_DECLARATOR)
                        {
                            AST attribute_list = ASTSon2(declarator);
                            gather_gcc_attribute_list(attribute_list, &gather_info, decl_context);
                        }

                        // Friend declarations are so special
                        if (gather_info.is_friend)
                        {
                            build_scope_friend_declarator(decl_context, &gather_info, class_info, member_type, declarator);
                            break;
                        }

                        AST declarator_name = get_declarator_name(declarator, decl_context);
                        AST initializer = ASTSon1(declarator);

                        {
                            ASTAttrSetValueType(declarator, LANG_IS_DECLARED_NAME, tl_type_t, tl_bool(1));
                            ast_set_link_to_child(declarator, LANG_DECLARED_NAME, declarator_name);
                            ast_set_link_to_child(declarator, LANG_INITIALIZER, initializer);
                            ast_set_link_to_child(declarator, LANG_DECLARATOR, ASTSon0(declarator));
                        }
                        
                        // Change name of constructors
                        AST decl_spec_seq = ASTSon0(a);
                        if (decl_spec_seq != NULL 
                                && ((ASTType(decl_spec_seq) != AST_AMBIGUITY && ASTSon1(decl_spec_seq) != NULL)
                                    || (ASTType(decl_spec_seq) == AST_AMBIGUITY)))
                        {
                            // It is not a constructor
                        }
                        else
                        {
                            if (is_constructor_declarator(declarator))
                            {
                                if (strcmp(class_name, ASTText(declarator_name)) == 0)
                                {
                                    is_constructor = 1;
                                }
                            }
                        }

                        decl_context_t new_decl_context = decl_context;
                        if (is_constructor)
                        {
                            new_decl_context.decl_flags |= DF_CONSTRUCTOR;
                        }

                        AST too_much_qualified_declarator_name = get_declarator_name(declarator, decl_context);

                        if (ASTType(too_much_qualified_declarator_name) == AST_QUALIFIED_ID
                                || ASTType(too_much_qualified_declarator_name) == AST_QUALIFIED_TEMPLATE)
                        {
                            running_error("%s: error: extra qualification of member declaration is not allowed: '%s'. Did you mean '%s'?", 
                                    ast_location(too_much_qualified_declarator_name),
                                    prettyprint_in_buffer(declarator),
                                    prettyprint_in_buffer(ASTSon2(too_much_qualified_declarator_name))
                                    );
                        }

                        type_t* declarator_type = NULL;

                        compute_declarator_type(ASTSon0(declarator), &gather_info, 
                                member_type, &declarator_type, 
                                new_decl_context, nodecl_output);
                        scope_entry_t *entry =
                            build_scope_declarator_name(ASTSon0(declarator),
                                    declarator_type, &gather_info,
                                    new_decl_context, nodecl_output);

                        ERROR_CONDITION(entry == NULL, "Member declaration '%s' at '%s' yielded an unresolved name",
                                prettyprint_in_buffer(a), ast_location(a));

                        DEBUG_CODE()
                        {
                            fprintf(stderr, "Setting symbol '%s' as a member of class '%s'\n", entry->symbol_name, 
                                    class_name);
                        }
                        entry->entity_specs.is_member = 1;
                        entry->entity_specs.access = current_access;
                        entry->entity_specs.class_type = class_info;
                        class_type_add_member(get_actual_class_type(class_type), entry);

                        if (entry->kind == SK_FUNCTION)
                        {
                            update_member_function_info(declarator_name, is_constructor, entry, class_type);
                            entry->point_of_declaration = get_enclosing_declaration(declarator);
                            
                            // This function might be hiding using declarations, remove those
                            hide_using_declarations(class_type, entry);
                        }
                        else if (entry->kind == SK_VARIABLE)
                        {
                            if (!gather_info.is_static)
                            {
                                DEBUG_CODE()
                                {
                                    fprintf(stderr, "Registering a nonstatic data member '%s' of class '%s'\n",
                                            entry->symbol_name, class_name);
                                }
                                // This is a nonstatic data member
                                entry->defined = 1;
                                entry->point_of_declaration = get_enclosing_declaration(declarator);
                                entry->point_of_definition = get_enclosing_declaration(declarator);
                            }
                            else
                            {
                                DEBUG_CODE()
                                {
                                    fprintf(stderr, "Registering a static data member '%s' of class '%s'\n",
                                            entry->symbol_name, class_name);
                                }
                                // This is a static data member
                                entry->point_of_declaration = get_enclosing_declaration(declarator);
                            }
                        }
                        else if (entry->kind == SK_TYPEDEF
                                || entry->kind == SK_TEMPLATE)
                        {
                            entry->point_of_declaration = get_enclosing_declaration(declarator);
                        }

                        if (initializer != NULL)
                        {
                            if (gather_info.is_friend)
                            {
                                running_error("%s: error: a friend-declaration cannot have initializer\n",
                                        ast_location(declarator));
                            }

                            if (entry->kind == SK_VARIABLE)
                            {
                                check_initialization(initializer,
                                        entry->decl_context,
                                        get_unqualified_type(entry->type_information));

                                entry->language_dependent_value = initializer;
                                entry->value = expression_get_nodecl(initializer);
                            }
                            // Special initializer for functions
                            else if (entry->kind == SK_FUNCTION)
                            {
                                // Check that it is '= 0'
                                char wrong_initializer = 1;
                                if (entry->entity_specs.is_virtual)
                                {
                                    AST equal_initializer = initializer;
                                    if (ASTType(equal_initializer) == AST_EQUAL_INITIALIZER)
                                    {
                                        AST octal_literal = ASTSon0(equal_initializer);
                                        if (ASTType(octal_literal) == AST_OCTAL_LITERAL)
                                        {
                                            if (strcmp(ASTText(octal_literal), "0") == 0)
                                            {
                                                // It is pure and the initializer was fine
                                                entry->entity_specs.is_pure = 1;
                                                wrong_initializer = 0;
                                            }
                                        }
                                    }
                                }

                                if (wrong_initializer)
                                {
                                    running_error("%s: error: function declaration '%s' has an invalid initializer '%s'"
                                            " or has not been declared as a virtual function\n", 
                                            ast_location(declarator),
                                            prettyprint_in_buffer(declarator),
                                            prettyprint_in_buffer(initializer));
                                }
                            }
                            else
                            {
                                running_error("%s: error: no initializer allowed in current member declaration",
                                        ast_location(initializer));
                            }
                        }
                        if (declared_symbols != NULL)
                        {
                            *declared_symbols = entry_list_add(*declared_symbols, entry);
                        }
                        break;
                    }
                default :
                    {
                        internal_error("Unhandled node '%s' (%s)", ast_print_node_type(ASTType(declarator)), ast_location(declarator));
                        break;
                    }
            }
        }
    }
}

/*
 * This function computes a cv_qualifier_t from an AST
 * containing a list of cv_qualifiers
 */
static cv_qualifier_t compute_cv_qualifier(AST a)
{
    cv_qualifier_t result = CV_NONE;

    // Allow empty trees to ease us the use of this function
    if (a == NULL)
    {
        return result;
    }

    ERROR_CONDITION((ASTType(a) != AST_NODE_LIST), "This function expects a list", 0);

    AST list, iter;
    list = a;

    for_each_element(list, iter)
    {
        AST cv_qualifier = ASTSon1(iter);

        switch (ASTType(cv_qualifier))
        {
            case AST_CONST_SPEC :
                result |= CV_CONST;
                break;
            case AST_VOLATILE_SPEC :
                result |= CV_VOLATILE;
                break;
            case AST_GCC_RESTRICT_SPEC :
                result |= CV_RESTRICT;
                break;
            default:
                internal_error("Unknown node type '%s'", ast_print_node_type(ASTType(cv_qualifier)));
                break;
        }
    }

    return result;
}

// This function fills returns an exception_spec_t* It returns NULL if no
// exception spec has been defined. Note that 'throw ()' is an exception spec
// and non-NULL is returned in this case.
static void build_exception_spec(type_t* function_type UNUSED_PARAMETER, 
        AST a, gather_decl_spec_t *gather_info, 
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    // No exception specifier at all
    if (a == NULL)
    {
        gather_info->any_exception = 1;
        return;
    }

    // function_type_set_exception_spec(function_type);

    AST type_id_list = ASTSon0(a);

    if (type_id_list == NULL)
        return;

    AST iter;

    for_each_element(type_id_list, iter)
    {
        AST type_id = ASTSon1(iter);

        // A type_id is a type_specifier_seq followed by an optional abstract
        // declarator
        AST type_specifier_seq = ASTSon0(type_id);
        AST abstract_decl = ASTSon1(type_id);

        // A type_specifier_seq is essentially a subset of a
        // declarator_specifier_seq so we can reuse existing functions
        type_t* type_info = NULL;
        gather_decl_spec_t inner_gather_info;
        memset(&inner_gather_info, 0, sizeof(inner_gather_info));
    
        build_scope_decl_specifier_seq(type_specifier_seq, &inner_gather_info, &type_info,
                decl_context, nodecl_output);

        if (is_error_type(type_info))
            continue;

        type_t* declarator_type = type_info;
        compute_declarator_type(abstract_decl, &inner_gather_info, type_info, &declarator_type,
                decl_context, nodecl_output);

        if (is_error_type(declarator_type))
            continue;

        P_LIST_ADD_ONCE(gather_info->exceptions, gather_info->num_exceptions, declarator_type);
    }
}

// Gives a name to an operation node
// 'a + b' will return 'operator+'
const char *get_operation_function_name(AST operation_tree)
{
    switch (ASTType(operation_tree))
    {
        case AST_ADD :
            return STR_OPERATOR_ADD;
        case AST_MUL :
            return STR_OPERATOR_MULT;
        case AST_DIV :
            return STR_OPERATOR_DIV;
        case AST_MOD :
            return STR_OPERATOR_MOD;
        case AST_MINUS :
            return STR_OPERATOR_MINUS;
        case AST_SHL :
            return STR_OPERATOR_SHIFT_LEFT;
        case AST_SHR :
            return STR_OPERATOR_SHIFT_RIGHT;
        case AST_LOWER_THAN :
            return STR_OPERATOR_LOWER_THAN;
        case AST_GREATER_THAN :
            return STR_OPERATOR_GREATER_THAN;
        case AST_GREATER_OR_EQUAL_THAN :
            return STR_OPERATOR_GREATER_EQUAL;
        case AST_LOWER_OR_EQUAL_THAN :
            return STR_OPERATOR_LOWER_EQUAL;
        case AST_EQUAL :
            return STR_OPERATOR_EQUAL;
        case AST_DIFFERENT :
            return STR_OPERATOR_DIFFERENT;
        case AST_BITWISE_AND :
            return STR_OPERATOR_BIT_AND;
        case AST_BITWISE_XOR :
            return STR_OPERATOR_BIT_XOR;
        case AST_BITWISE_OR :
            return STR_OPERATOR_BIT_OR;
        case AST_LOGICAL_AND :
            return STR_OPERATOR_LOGIC_AND;
        case AST_LOGICAL_OR :
            return STR_OPERATOR_LOGIC_OR;
        case AST_DERREFERENCE :
            return STR_OPERATOR_DERREF;
        case AST_REFERENCE : 
            return STR_OPERATOR_REFERENCE;
        case AST_PLUS :
            return STR_OPERATOR_UNARY_PLUS;
        case AST_NEG :
            return STR_OPERATOR_UNARY_NEG;
        case AST_LOGICAL_NOT :
            return STR_OPERATOR_LOGIC_NOT;
        case AST_BITWISE_NOT :
            return STR_OPERATOR_BIT_NOT;
        case AST_ASSIGNMENT :
            return STR_OPERATOR_ASSIGNMENT;
        case AST_MUL_ASSIGNMENT :
            return STR_OPERATOR_MUL_ASSIGNMENT;
        case AST_DIV_ASSIGNMENT :
            return STR_OPERATOR_DIV_ASSIGNMENT;
        case AST_ADD_ASSIGNMENT :
            return STR_OPERATOR_ADD_ASSIGNMENT;
        case AST_SUB_ASSIGNMENT :
            return STR_OPERATOR_MINUS_ASSIGNMENT;
        case AST_SHL_ASSIGNMENT :
            return STR_OPERATOR_SHL_ASSIGNMENT;
        case AST_SHR_ASSIGNMENT :
            return STR_OPERATOR_SHR_ASSIGNMENT;
        case AST_BITWISE_AND_ASSIGNMENT :
            return STR_OPERATOR_AND_ASSIGNMENT;
        case AST_BITWISE_OR_ASSIGNMENT :
            return STR_OPERATOR_OR_ASSIGNMENT;
        case AST_BITWISE_XOR_ASSIGNMENT :
            return STR_OPERATOR_XOR_ASSIGNMENT;
        case AST_MOD_ASSIGNMENT :
            return STR_OPERATOR_MOD_ASSIGNMENT;
        case AST_PREINCREMENT :
            return STR_OPERATOR_PREINCREMENT;
        case AST_POSTINCREMENT :
            return STR_OPERATOR_POSTINCREMENT;
        case AST_PREDECREMENT :
            return STR_OPERATOR_PREDECREMENT;
        case AST_POSTDECREMENT :
            return STR_OPERATOR_POSTDECREMENT;
        default:
            internal_error("Invalid operation node %s", ast_print_node_type(ASTType(operation_tree)));
    }
}

// Gives a name to an operator function name
// 'operator +'
const char* get_operator_function_name(AST declarator_id)
{
    ERROR_CONDITION((ASTType(declarator_id) != AST_OPERATOR_FUNCTION_ID
                && ASTType(declarator_id) != AST_OPERATOR_FUNCTION_ID_TEMPLATE), 
            "This node is not valid here '%s'", ast_print_node_type(ASTType(declarator_id)));

    AST operator  = ASTSon0(declarator_id);

    switch (ASTType(operator))
    {
        case AST_NEW_OPERATOR :
            return STR_OPERATOR_NEW;
        case AST_DELETE_OPERATOR :
            return STR_OPERATOR_DELETE;
        case AST_NEW_ARRAY_OPERATOR :
            return STR_OPERATOR_NEW_ARRAY;
        case AST_DELETE_ARRAY_OPERATOR :
            return STR_OPERATOR_DELETE_ARRAY;
        case AST_ADD_OPERATOR :
            return STR_OPERATOR_ADD;
        case AST_MINUS_OPERATOR :
            return STR_OPERATOR_MINUS;
        case AST_MUL_OPERATOR :
            return STR_OPERATOR_MULT;
        case AST_DIV_OPERATOR :
            return STR_OPERATOR_DIV;
        case AST_MOD_OPERATOR :
            return STR_OPERATOR_MOD;
        case AST_BITWISE_XOR_OPERATOR :
            return STR_OPERATOR_BIT_XOR;
        case AST_BITWISE_AND_OPERATOR :
            return STR_OPERATOR_BIT_AND;
        case AST_BITWISE_OR_OPERATOR :
            return STR_OPERATOR_BIT_OR;
        case AST_BITWISE_NEG_OPERATOR :
            return STR_OPERATOR_BIT_NOT;
        case AST_LOGICAL_NOT_OPERATOR :
            return STR_OPERATOR_LOGIC_NOT;
        case AST_ASSIGNMENT_OPERATOR :
            return STR_OPERATOR_ASSIGNMENT;
        case AST_LOWER_OPERATOR :
            return STR_OPERATOR_LOWER_THAN;
        case AST_GREATER_OPERATOR :
            return STR_OPERATOR_GREATER_THAN;
        case AST_ADD_ASSIGN_OPERATOR :
            return STR_OPERATOR_ADD_ASSIGNMENT;
        case AST_SUB_ASSIGN_OPERATOR :
            return STR_OPERATOR_MINUS_ASSIGNMENT;
        case AST_MUL_ASSIGN_OPERATOR :
            return STR_OPERATOR_MUL_ASSIGNMENT;
        case AST_DIV_ASSIGN_OPERATOR :
            return STR_OPERATOR_DIV_ASSIGNMENT;
        case AST_MOD_ASSIGN_OPERATOR :
            return STR_OPERATOR_MOD_ASSIGNMENT;
        case AST_BITWISE_XOR_ASSIGN_OPERATOR :
            return STR_OPERATOR_XOR_ASSIGNMENT;
        case AST_BITWISE_AND_ASSIGN_OPERATOR :
            return STR_OPERATOR_AND_ASSIGNMENT;
        case AST_BITWISE_OR_ASSIGN_OPERATOR :
            return STR_OPERATOR_OR_ASSIGNMENT;
        case AST_LEFT_OPERATOR :
            return STR_OPERATOR_SHIFT_LEFT;
        case AST_RIGHT_OPERATOR :
            return STR_OPERATOR_SHIFT_RIGHT;
        case AST_LEFT_ASSIGN_OPERATOR :
            return STR_OPERATOR_SHL_ASSIGNMENT;
        case AST_RIGHT_ASSIGN_OPERATOR :
            return STR_OPERATOR_SHR_ASSIGNMENT;
        case AST_EQUAL_OPERATOR :
            return STR_OPERATOR_EQUAL;
        case AST_DIFFERENT_OPERATOR :
            return STR_OPERATOR_DIFFERENT;
        case AST_LESS_OR_EQUAL_OPERATOR :
            return STR_OPERATOR_LOWER_EQUAL;
        case AST_GREATER_OR_EQUAL_OPERATOR :
            return STR_OPERATOR_GREATER_EQUAL;
        case AST_LOGICAL_AND_OPERATOR :
            return STR_OPERATOR_LOGIC_AND;
        case AST_LOGICAL_OR_OPERATOR :
            return STR_OPERATOR_LOGIC_OR;
        case AST_INCREMENT_OPERATOR :
            return STR_OPERATOR_POSTINCREMENT;
        case AST_DECREMENT_OPERATOR :
            return STR_OPERATOR_POSTDECREMENT;
        case AST_COMMA_OPERATOR :
            return STR_OPERATOR_COMMA;
        case AST_POINTER_OPERATOR :
            return STR_OPERATOR_ARROW;
        case AST_POINTER_DERREF_OPERATOR :
            return STR_OPERATOR_ARROW_POINTER;
        case AST_FUNCTION_CALL_OPERATOR :
            return STR_OPERATOR_CALL;
        case AST_SUBSCRIPT_OPERATOR :
            return STR_OPERATOR_SUBSCRIPT;
        default :
            internal_error("Invalid node type '%s'\n", ast_print_node_type(ASTType(declarator_id)));
    }
}


typedef
struct call_to_destructor_data_tag
{
    nodecl_t* nodecl_output;
    scope_t* scope;
} call_to_destructor_data_t;

static void call_to_destructor(scope_entry_list_t* entry_list, void *data)
{
    call_to_destructor_data_t* destructor_data = (call_to_destructor_data_t*)data;

    scope_entry_t* entry = entry_list_head(entry_list);

    if (entry->kind == SK_VARIABLE
            && is_class_type(entry->type_information)
            && !is_dependent_type(entry->type_information)
            && !entry->entity_specs.is_static
            && !entry->entity_specs.is_extern)
    {
        type_t* class_type = get_actual_class_type(entry->type_information);

        nodecl_t nodecl_call_to_destructor = 
            nodecl_make_expression_statement(
            cxx_nodecl_make_function_call(
                    nodecl_make_symbol(class_type_get_destructor(class_type), NULL, 0),
                    nodecl_make_list_1(nodecl_make_symbol(entry, NULL, 0)),
                    get_void_type(),
                    NULL, 0), NULL, 0);

        *(destructor_data->nodecl_output) = nodecl_append_to_list(
                *(destructor_data->nodecl_output), 
                nodecl_call_to_destructor);
    }

    entry_list_free(entry_list);
}

static void call_destructors_of_classes(decl_context_t block_context, 
        nodecl_t* nodecl_output)
{
    call_to_destructor_data_t call_to_destructor_data = { 
        .nodecl_output = nodecl_output,
        .scope = block_context.current_scope
    };

    scope_for_each_entity(block_context.current_scope, &call_to_destructor_data, call_to_destructor);
}

/*
 * Building scope for statements
 */

typedef void (*stmt_scope_handler_t)(AST a, decl_context_t decl_context, char* attrib_to_set, nodecl_t* nodecl_output);
typedef 
struct stmt_scope_handler_map_tag
{
    stmt_scope_handler_t handler;
    char* attr_name;
} stmt_scope_handler_map_t;


static void build_scope_compound_statement(AST a, 
        decl_context_t decl_context, 
        char* attr_name UNUSED_PARAMETER,
        nodecl_t* nodecl_output)
{
    decl_context_t block_context = new_block_context(decl_context);

    // Note that we set the scope link to the list node and not to the compound
    // statement node because we want to have the scope just before the
    // compound and after the compound
    //
    //   the scope of the compound -> *  {
    //                                      * <-- the list scope
    //                                   }
    //
    // This adds a bit of burden when enlarging an empty compound

    nodecl_t nodecl_output_list = nodecl_null();

    AST list = ASTSon0(a);
    if (list != NULL)
    {
        scope_link_set(CURRENT_COMPILED_FILE->scope_link, list, block_context);

        nodecl_t current_nodecl_output = nodecl_null();
        build_scope_statement_seq(list, block_context, &current_nodecl_output);

        nodecl_output_list = nodecl_concat_lists(nodecl_output_list, current_nodecl_output);
    }

    ASTAttrSetValueType(a, LANG_IS_COMPOUND_STATEMENT, tl_type_t, tl_bool(1));
    ast_set_link_to_child(a, LANG_COMPOUND_STATEMENT_LIST, ASTSon0(a));

    nodecl_t nodecl_destructors = nodecl_null();
    CXX_LANGUAGE()
    {
        call_destructors_of_classes(block_context, &nodecl_destructors);
    }

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_compound_statement(nodecl_output_list, nodecl_destructors, new_scope_symbol(block_context), 
                ASTFileName(a), ASTLine(a)));
}

static void build_scope_condition(AST a, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    ASTAttrSetValueType(a, LANG_IS_CONDITION, tl_type_t, tl_bool(1));

    if (ASTType(a) == AST_AMBIGUITY)
    {
        solve_condition_ambiguity(a, decl_context);
    }

    if (ASTSon0(a) != NULL 
            && ASTSon1(a) != NULL)
    {
        // This condition declares something in this scope
        AST type_specifier_seq = ASTSon0(a);
        AST declarator = ASTSon1(a);

        if (ASTType(type_specifier_seq) == AST_AMBIGUITY)
        {
            solve_ambiguous_decl_specifier_seq(type_specifier_seq, decl_context);
        }
        
        ERROR_CONDITION((ASTType(declarator) == AST_AMBIGUITY), "Unexpected ambiguity", 0);

        // A type_specifier_seq is essentially a subset of a
        // declarator_specifier_seq so we can reuse existing functions
        type_t* type_info = NULL;
        gather_decl_spec_t gather_info;
        memset(&gather_info, 0, sizeof(gather_info));
    
        build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &type_info,
                decl_context, nodecl_output);

        type_t* declarator_type = NULL;

        compute_declarator_type(declarator, &gather_info, type_info, &declarator_type,
                decl_context, nodecl_output);
        scope_entry_t* entry = build_scope_declarator_name(declarator, declarator_type, &gather_info, decl_context, nodecl_output);

        AST initializer = ASTSon2(a);

        if (check_initialization(initializer, decl_context, entry->type_information))
        {
            error_printf("%s: error: initializer '%s' could not be checked\n",
                    ast_location(initializer),
                    prettyprint_in_buffer(initializer));
        }

        entry->language_dependent_value = initializer;
        entry->value = expression_get_nodecl(initializer);

        C_LANGUAGE()
        {
            standard_conversion_t dummy;
            if (!standard_conversion_between_types(&dummy, entry->type_information, get_bool_type()))
            {
                error_printf("%s: error: value of type '%s' where a scalar was expected\n",
                        ast_location(a),
                        print_type_str(entry->type_information, decl_context));
            }

            *nodecl_output = nodecl_make_object_init(nodecl_null(), entry, ASTFileName(initializer), ASTLine(initializer));
        }
        CXX_LANGUAGE()
        {
            if (!is_dependent_expr_type(expression_get_type(initializer))
                    && !is_dependent_type(declarator_type))
            {
                char ambiguous_conversion = 0;
                scope_entry_t* conversor = NULL;
                if (!type_can_be_implicitly_converted_to(entry->type_information, get_bool_type(), decl_context, 
                            &ambiguous_conversion, &conversor))
                {
                    error_printf("%s: error: value of type '%s' cannot be converted to 'bool' type\n",
                            ast_location(a),
                            print_type_str(entry->type_information, decl_context));
                }

                *nodecl_output = nodecl_make_object_init(nodecl_null(), entry, ASTFileName(initializer), ASTLine(initializer));
                if (conversor != NULL)
                {
                    ERROR_CONDITION((conversor->entity_specs.is_conversion),
                            "I expected a conversion function!", 0);
                    *nodecl_output = cxx_nodecl_make_function_call(nodecl_make_symbol(conversor, ASTFileName(initializer), ASTLine(initializer)),
                            nodecl_make_list_1(*nodecl_output),
                            function_type_get_return_type(conversor->type_information), ASTFileName(initializer), ASTLine(initializer));
                }
            }
        }


        ASTAttrSetValueType(a, LANG_IS_DECLARATION, tl_type_t, tl_bool(1));
        ast_set_link_to_child(a, LANG_DECLARATION_SPECIFIERS, type_specifier_seq);
        ast_set_link_to_child(a, LANG_DECLARATION_DECLARATORS, declarator);

        ASTAttrSetValueType(a, LANG_IS_CONDITION_DECLARATION, tl_type_t, tl_bool(1));

    }
    else
    {
        if (!check_expression(ASTSon2(a), decl_context))
        {
            error_printf("%s: error: condition '%s' could not be checked\n",
                    ast_location(ASTSon2(a)),
                    prettyprint_in_buffer(ASTSon2(a)));
        }

        ASTAttrSetValueType(a, LANG_IS_CONDITION_EXPRESSION, tl_type_t, tl_bool(1));
        ASTAttrSetValueType(a, LANG_IS_EXPRESSION_NEST, tl_type_t, tl_bool(1));
        ast_set_link_to_child(a, LANG_EXPRESSION_NESTED, ASTSon2(a));

        *nodecl_output = expression_get_nodecl(ASTSon2(a));
    }
}

static void build_scope_while_statement(AST a, 
        decl_context_t decl_context, 
        char* attr_name UNUSED_PARAMETER,
        nodecl_t* nodecl_output)
{
    decl_context_t block_context = new_block_context(decl_context);

    nodecl_t nodecl_condition = nodecl_null();
    build_scope_condition(ASTSon0(a), block_context, &nodecl_condition);
    scope_link_set(CURRENT_COMPILED_FILE->scope_link, ASTSon0(a), block_context);

    nodecl_t nodecl_statement = nodecl_null();
    if (ASTSon1(a) != NULL)
    {
        build_scope_statement(ASTSon1(a), block_context, &nodecl_statement);
        scope_link_set(CURRENT_COMPILED_FILE->scope_link, ASTSon1(a), block_context);
    }

    ASTAttrSetValueType(a, LANG_IS_WHILE_STATEMENT, tl_type_t, tl_bool(1));
    ast_set_link_to_child(a, LANG_WHILE_STATEMENT_CONDITION, ASTSon0(a));
    ast_set_link_to_child(a, LANG_WHILE_STATEMENT_BODY, ASTSon1(a));

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_while_statement(nodecl_condition, nodecl_statement, new_scope_symbol(block_context), ASTFileName(a), ASTLine(a)));
}

static void build_scope_ambiguity_handler(AST a, 
        decl_context_t decl_context, 
        char* attrib_to_set UNUSED_PARAMETER,
        nodecl_t* nodecl_output)
{
    solve_ambiguous_statement(a, decl_context);
    // Restart
    build_scope_statement(a, decl_context, nodecl_output);
}

static void build_scope_declaration_statement(AST a, 
        decl_context_t decl_context, 
        char* attr_name UNUSED_PARAMETER,
        nodecl_t* nodecl_output)
{
    AST declaration = ASTSon0(a);

    build_scope_declaration(declaration, decl_context, nodecl_output, /* declared_symbols */ NULL);

    ASTAttrSetValueType(a, LANG_IS_DECLARATION_STATEMENT, tl_type_t, tl_bool(1));
    ast_set_link_to_child(a, LANG_DECLARATION_STATEMENT_DECLARATION, declaration);
}


static void build_scope_expression_statement(AST a, 
        decl_context_t decl_context, 
        char* attr_name UNUSED_PARAMETER,
        nodecl_t* nodecl_output)
{
    AST expr = ASTSon0(a);
    if (!check_expression(expr, decl_context))
    {
        if (CURRENT_CONFIGURATION->strict_typecheck)
        {
            internal_error("Could not check expression '%s' at '%s'\n",
                    prettyprint_in_buffer(ASTSon0(a)),
                    ast_location(ASTSon0(a)));
        }
        return;
    }

    if (expression_get_type(expr) != NULL)
    {
        expression_set_type(a, expression_get_type(expr));
        expression_set_is_lvalue(a, expression_is_lvalue(a));

        if (is_unresolved_overloaded_type(expression_get_type(expr)))
        {
            error_printf("%s: error: invalid unresolved overloaded expression '%s'\n", 
                    ast_location(expr),
                    prettyprint_in_buffer(expr));
            scope_entry_list_t* candidates = unresolved_overloaded_type_get_overload_set(expression_get_type(expr));

            diagnostic_candidates(candidates, ASTFileName(expr), ASTLine(expr));

            return;
        }
    }

    ASTAttrSetValueType(a, LANG_IS_EXPRESSION_STATEMENT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_IS_EXPRESSION_COMPONENT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_IS_EXPRESSION_NEST, tl_type_t, tl_bool(1));
    ast_set_link_to_child(a, LANG_EXPRESSION_NESTED, expr);        

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_expression_statement(expression_get_nodecl(expr), ASTFileName(expr), ASTLine(expr)));
}

static void build_scope_if_else_statement(AST a, 
        decl_context_t decl_context, 
        char* attr_name UNUSED_PARAMETER,
        nodecl_t* nodecl_output)
{
    decl_context_t block_context = new_block_context(decl_context);

    AST condition = ASTSon0(a);
    nodecl_t nodecl_condition = nodecl_null();
    build_scope_condition(condition, block_context, &nodecl_condition);
    scope_link_set(CURRENT_COMPILED_FILE->scope_link, condition, block_context);

    AST then_branch = ASTSon1(a);
    nodecl_t nodecl_then = nodecl_null();
    build_scope_statement(then_branch, block_context, &nodecl_then);
    scope_link_set(CURRENT_COMPILED_FILE->scope_link, then_branch, block_context);

    nodecl_t nodecl_else = nodecl_null();
    AST else_branch = ASTSon2(a);
    if (else_branch != NULL)
    {
        build_scope_statement(else_branch, block_context, &nodecl_else);
        scope_link_set(CURRENT_COMPILED_FILE->scope_link, else_branch, block_context);
    }

    ASTAttrSetValueType(a, LANG_IS_IF_STATEMENT, tl_type_t, tl_bool(1));
    ast_set_link_to_child(a, LANG_IF_STATEMENT_CONDITION, condition);
    ast_set_link_to_child(a, LANG_IF_STATEMENT_THEN_BODY, then_branch);
    ast_set_link_to_child(a, LANG_IF_STATEMENT_ELSE_BODY, else_branch);

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_if_else_statement(nodecl_condition, nodecl_then, nodecl_else, new_scope_symbol(block_context),
                ASTFileName(a), ASTLine(a)));
}

static void build_scope_for_statement(AST a, 
        decl_context_t decl_context, 
        char* attr_name UNUSED_PARAMETER,
        nodecl_t *nodecl_output)
{
    AST loop_control = ASTSon0(a);

    AST for_init_statement = ASTSon0(loop_control);
    AST condition = ASTSon1(loop_control);
    AST expression = ASTSon2(loop_control);

    AST statement = ASTSon1(a);

    if (ASTType(for_init_statement) == AST_AMBIGUITY)
    {
        solve_ambiguous_for_init_statement(for_init_statement, decl_context);
    }

    decl_context_t block_context = new_block_context(decl_context);

    nodecl_t nodecl_loop_init = nodecl_null();
    if (ASTType(for_init_statement) == AST_SIMPLE_DECLARATION)
    {
        build_scope_simple_declaration(for_init_statement, block_context, 
                /* is_template */ 0, /* is_explicit_instantiation */ 0,
                &nodecl_loop_init, /* declared_symbols */ NULL);
        if (!nodecl_is_null(nodecl_loop_init))
        {
            nodecl_loop_init = nodecl_list_head(nodecl_loop_init);
        }
    }
    else if (ASTType(for_init_statement) == AST_EXPRESSION_STATEMENT)
    {
        build_scope_expression_statement(for_init_statement, block_context, NULL, &nodecl_loop_init);
        nodecl_loop_init = nodecl_list_head(nodecl_loop_init);
        // Get the expression itself instead of an expression statement
        nodecl_loop_init = nodecl_get_child(nodecl_loop_init, 0); 
    }
    else if (ASTType(for_init_statement) == AST_EMPTY_STATEMENT)
    {
        build_scope_statement(for_init_statement, block_context, &nodecl_loop_init);
        // Make it empty
        nodecl_loop_init = nodecl_null();
    }
    else
    {
        internal_error("unexpected node '%s'", ast_print_node_type(ASTType(for_init_statement)));
    }
    scope_link_set(CURRENT_COMPILED_FILE->scope_link, for_init_statement, block_context);

    nodecl_t nodecl_loop_condition = nodecl_null();
    if (condition != NULL)
    {
        build_scope_condition(condition, block_context, &nodecl_loop_condition);
        scope_link_set(CURRENT_COMPILED_FILE->scope_link, condition, block_context);
    }

    nodecl_t nodecl_loop_iter = nodecl_null();
    if (expression != NULL)
    {
        if (!check_expression(expression, block_context))
        {
            error_printf("%s: error: could not check iterating expression '%s'\n",
                    ast_location(expression),
                    prettyprint_in_buffer(expression));
        }

        nodecl_loop_iter = expression_get_nodecl(expression);

        scope_link_set(CURRENT_COMPILED_FILE->scope_link, expression, block_context);
    }


    nodecl_t nodecl_statement = nodecl_null();
    build_scope_statement(statement, block_context, &nodecl_statement);
    scope_link_set(CURRENT_COMPILED_FILE->scope_link, statement, block_context);

    ASTAttrSetValueType(a, LANG_IS_FOR_STATEMENT, tl_type_t, tl_bool(1));
    ast_set_link_to_child(a, LANG_FOR_INIT_CONSTRUCT, for_init_statement);
    ast_set_link_to_child(a, LANG_FOR_CONDITION, condition);
    ast_set_link_to_child(a, LANG_FOR_ITERATION_EXPRESSION, expression);
    ast_set_link_to_child(a, LANG_FOR_BODY_STATEMENT, statement);

    nodecl_t nodecl_loop_control = nodecl_make_loop_control(nodecl_loop_init, nodecl_loop_condition, nodecl_loop_iter,
            ASTFileName(a), ASTLine(a));
    *nodecl_output = nodecl_make_list_1(
            nodecl_make_for_statement(nodecl_loop_control, nodecl_statement, 
                new_scope_symbol(block_context), 
                ASTFileName(a), ASTLine(a)));
}

static void build_scope_switch_statement(AST a, 
        decl_context_t decl_context, 
        char* attr_name UNUSED_PARAMETER,
        nodecl_t* nodecl_output)
{
    decl_context_t block_context = new_block_context(decl_context);

    AST condition = ASTSon0(a);
    AST statement = ASTSon1(a);

    nodecl_t nodecl_condition = nodecl_null();
    build_scope_condition(condition, block_context, &nodecl_condition);
    scope_link_set(CURRENT_COMPILED_FILE->scope_link, condition, block_context);

    nodecl_t nodecl_statement = nodecl_null();
    build_scope_statement(statement, block_context, &nodecl_statement);
    scope_link_set(CURRENT_COMPILED_FILE->scope_link, statement, block_context);

    ASTAttrSetValueType(a, LANG_IS_SWITCH_STATEMENT, tl_type_t, tl_bool(1));
    ast_set_link_to_child(a, LANG_SWITCH_STATEMENT_CONDITION, condition);
    ast_set_link_to_child(a, LANG_SWITCH_STATEMENT_BODY, statement);

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_switch_statement(nodecl_condition, nodecl_statement, ASTFileName(a), ASTLine(a)));
}

static scope_entry_t* add_label_if_not_found(AST label, decl_context_t decl_context)
{
    const char* label_text = ASTText(label);
    scope_entry_list_t* entry_list = query_unqualified_name_str_flags(decl_context, label_text, DF_LABEL);

    scope_entry_t* sym_label = NULL;
    if (entry_list == NULL)
    {
        sym_label = new_symbol(decl_context, decl_context.function_scope, ASTText(label));
        sym_label->kind = SK_LABEL;
        sym_label->line = ASTLine(label);
        sym_label->file = ASTFileName(label);
    }
    else
    {
        sym_label = entry_list_head(entry_list);
    }

    entry_list_free(entry_list);

    return sym_label;
}
static void build_scope_goto_statement(AST a, 
        decl_context_t decl_context, 
        char* attr_name UNUSED_PARAMETER,
        nodecl_t* nodecl_output)
{
    AST label = ASTSon0(a);
    scope_entry_t* sym_label = add_label_if_not_found(label, decl_context);

    ASTAttrSetValueType(a, LANG_IS_GOTO_STATEMENT, tl_type_t, tl_bool(1));
    ast_set_link_to_child(a, LANG_GOTO_STATEMENT_LABEL, label);

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_goto_statement(sym_label, ASTFileName(a), ASTLine(a)));
}

static void build_scope_labeled_statement(AST a, 
        decl_context_t decl_context, 
        char* attr_name UNUSED_PARAMETER,
        nodecl_t* nodecl_output)
{
    AST label = ASTSon0(a);
    scope_entry_t* sym_label = add_label_if_not_found(label, decl_context);

    AST statement = ASTSon1(a);

    nodecl_t nodecl_statement = nodecl_null();
    build_scope_statement_(statement, decl_context, &nodecl_statement);

    ASTAttrSetValueType(a, LANG_IS_LABELED_STATEMENT, tl_type_t, tl_bool(1));
    ast_set_link_to_child(a, LANG_STATEMENT_LABEL, label);
    ast_set_link_to_child(a, LANG_LABELED_STATEMENT, statement);

    *nodecl_output = 
        nodecl_make_list_1(
                nodecl_make_labeled_statement(nodecl_statement, sym_label, ASTFileName(a), ASTLine(a)));
}

static void build_scope_default_statement(AST a, 
        decl_context_t decl_context, 
        char* attr_name UNUSED_PARAMETER,
        nodecl_t* nodecl_output)
{
    AST statement = ASTSon0(a);
    nodecl_t nodecl_statement = nodecl_null();
    build_scope_statement(statement, decl_context, &nodecl_statement);

    ASTAttrSetValueType(a, LANG_IS_DEFAULT_STATEMENT, tl_type_t, tl_bool(1));
    ast_set_link_to_child(a, LANG_DEFAULT_STATEMENT_BODY, statement);

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_default_statement(nodecl_statement, ASTFileName(a), ASTLine(a)));
}

static void build_scope_case_statement(AST a, 
        decl_context_t decl_context, 
        char* attr_name UNUSED_PARAMETER,
        nodecl_t* nodecl_output)
{
    AST constant_expression = ASTSon0(a);
    AST statement = ASTSon1(a);
    if (!check_expression(constant_expression, decl_context))
    {
        fprintf(stderr, "%s: could not check case expression '%s'\n",
                ast_location(constant_expression),
                prettyprint_in_buffer(constant_expression));
    }

    nodecl_t nodecl_statement = nodecl_null();
    build_scope_statement(statement, decl_context, &nodecl_statement);

    ASTAttrSetValueType(a, LANG_IS_CASE_STATEMENT, tl_type_t, tl_bool(1));
    ast_set_link_to_child(a, LANG_CASE_EXPRESSION, constant_expression);
    ast_set_link_to_child(a, LANG_CASE_STATEMENT_BODY, statement);

    // FIXME - We can try to coalesce several cases alla Fortran
    *nodecl_output = nodecl_make_list_1(
            nodecl_make_case_statement(
                nodecl_make_list_1(expression_get_nodecl(constant_expression)), 
                nodecl_statement, 
                ASTFileName(a), 
                ASTLine(a)));
}

static void build_scope_return_statement(AST a, 
        decl_context_t decl_context, 
        char* attr_name UNUSED_PARAMETER,
        nodecl_t* nodecl_output)
{
    AST expression = ASTSon0(a);

    scope_entry_t* function = decl_context.current_scope->related_entry;
    ERROR_CONDITION(function->kind != SK_FUNCTION, "Invalid related entry!", 0);

    type_t* return_type = function_type_get_return_type(function->type_information);
    if (return_type == NULL)
        return_type = get_void_type();

    nodecl_t nodecl_return = nodecl_null();

    if (expression != NULL)
    {

        char valid_expr = 0;
        ast_set_link_to_child(a, LANG_RETURN_EXPRESSION, expression);
        if (ASTType(expression) == AST_INITIALIZER_BRACES)
        {
            valid_expr = check_initializer_clause(expression, decl_context, return_type);
        }
        else 
        {
            valid_expr = check_expression(expression, decl_context);
        }

        if (is_void_type(return_type))
        {
            if (!IS_CXX_LANGUAGE
                    || (!is_dependent_expr_type(expression_get_type(expression))
                        && !is_void_type(expression_get_type(expression))))
            {
                error_printf("%s: error: return with non-void expression in a void function\n", ast_location(a));
            }
        }

        if (valid_expr)
        {
            nodecl_return = expression_get_nodecl(expression);

            if (!is_dependent_type(return_type)
                    && !is_dependent_expr_type(expression_get_type(expression)))
            {
                CXX_LANGUAGE()
                {
                    char ambiguous_conversion = 0;
                    scope_entry_t* conversor = NULL;
                    if (!type_can_be_implicitly_converted_to(expression_get_type(expression),
                                return_type, decl_context, &ambiguous_conversion, &conversor))
                    {
                        error_printf("%s: error: cannot convert type '%s' to '%s'\n",
                                ast_location(expression),
                                print_type_str(expression_get_type(expression), decl_context),
                                print_type_str(return_type, decl_context));
                    }

                    if (conversor != NULL)
                    {
                        nodecl_return = cxx_nodecl_make_function_call(
                                nodecl_make_symbol(conversor, ASTFileName(expression), ASTLine(expression)),
                                nodecl_make_list_1(nodecl_return),
                                actual_type_of_conversor(conversor),
                                ASTFileName(expression), ASTLine(expression));
                    }
                }
                C_LANGUAGE()
                {
                    standard_conversion_t scs;
                    if (!standard_conversion_between_types(&scs, expression_get_type(expression), return_type))
                    {
                        error_printf("%s: error: invalid returned value of type '%s' in a function returning '%s'\n",
                                ast_location(expression),
                                print_type_str(expression_get_type(expression), decl_context),
                                print_type_str(return_type, decl_context));
                    }
                }
            }
        }
    }
	else
	{
		if (!return_type && NULL
				&& !is_void_type(return_type))
		{
			error_printf("%s: error: return with no expression in a non-void function\n", ast_location(a));
		}
	}

    ASTAttrSetValueType(a, LANG_IS_RETURN_STATEMENT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_RETURN_STATEMENT_HAS_EXPRESSION, tl_type_t, tl_bool(expression != NULL));

    *nodecl_output = 
        nodecl_make_list_1(
                nodecl_make_return_statement(nodecl_return, ASTFileName(a), ASTLine(a)));
}

static void build_scope_try_block(AST a, 
        decl_context_t decl_context, 
        char* attr_name UNUSED_PARAMETER,
        nodecl_t* nodecl_output)
{
    AST protected_block = ASTSon0(a);

    nodecl_t nodecl_statement = nodecl_null();
    build_scope_statement(protected_block, decl_context, &nodecl_statement);

    nodecl_t nodecl_catch_list = nodecl_null();
    nodecl_t nodecl_catch_any = nodecl_null();

    AST handler_seq = ASTSon1(a);
    AST iter;

    char seen_any_case = 0;
    for_each_element(handler_seq, iter)
    {
        AST handler = ASTSon1(iter);

        AST exception_declaration = ASTSon0(handler);
        AST handler_compound_statement = ASTSon1(handler);

        decl_context_t block_context = new_block_context(decl_context);
        if (ASTType(exception_declaration) == AST_AMBIGUITY)
        {
            solve_ambiguous_exception_decl(exception_declaration, block_context);
        }

        if (ASTType(exception_declaration) != AST_ANY_EXCEPTION)
        {
            AST type_specifier_seq = ASTSon0(exception_declaration);
            // This declarator can be null
            AST declarator = ASTSon1(exception_declaration);

            type_t* type_info = NULL;
            gather_decl_spec_t gather_info;
            memset(&gather_info, 0, sizeof(gather_info));

            nodecl_t dummy = nodecl_null();
            build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &type_info,
                    block_context, &dummy);

            scope_link_set(CURRENT_COMPILED_FILE->scope_link, exception_declaration, block_context);

            type_t* declarator_type = NULL;

            nodecl_t exception_name = nodecl_null();
            if (declarator != NULL)
            {
                dummy = nodecl_null();
                compute_declarator_type(declarator, &gather_info, type_info, &declarator_type,
                        block_context, &dummy);

                dummy = nodecl_null();
                scope_entry_t* entry = build_scope_declarator_name(declarator,
                        declarator_type, &gather_info, block_context, &dummy);

                exception_name = nodecl_make_object_init(nodecl_null(), entry, ASTFileName(declarator), ASTLine(declarator));
            }

            nodecl_t nodecl_catch_statement = nodecl_null();
            build_scope_statement(handler_compound_statement, block_context, &nodecl_catch_statement);

            nodecl_catch_list = nodecl_append_to_list(nodecl_catch_list, 
                    nodecl_make_catch_handler(exception_name, nodecl_catch_statement, 
                        declarator_type,
                        ASTFileName(exception_declaration), 
                        ASTLine(exception_declaration)));
        }
        else
        {
            if (seen_any_case)
            {
                running_error("%s: error: more than one 'catch(...)' handler in try-block\n",
                        ast_location(exception_declaration));
            }
            seen_any_case = 1;
            build_scope_statement(handler_compound_statement, block_context, &nodecl_catch_any);
        }
    }

    ASTAttrSetValueType(a, LANG_IS_TRY_BLOCK, tl_type_t, tl_bool(1));
    ast_set_link_to_child(a, LANG_TRY_BLOCK_BODY, protected_block);
    ast_set_link_to_child(a, LANG_TRY_BLOCK_HANDLER_LIST, handler_seq);

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_try_block(nodecl_statement, nodecl_catch_list, nodecl_catch_any, ASTFileName(a), ASTLine(a)));
}

static void build_scope_do_statement(AST a, 
        decl_context_t decl_context, 
        char* attr_name UNUSED_PARAMETER,
        nodecl_t* nodecl_output) 
{
    AST statement = ASTSon0(a);
    AST expression = ASTSon1(a);

    nodecl_t nodecl_statement = nodecl_null();
    build_scope_statement(statement, decl_context, &nodecl_statement);
    if (!check_expression(expression, decl_context))
    {
        error_printf("%s: error: could not check do expression '%s'\n",
                ast_location(expression),
                prettyprint_in_buffer(expression));
    }

    ASTAttrSetValueType(a, LANG_IS_DO_STATEMENT, tl_type_t, tl_bool(1));
    ast_set_link_to_child(a, LANG_DO_STATEMENT_BODY, statement);
    ast_set_link_to_child(a, LANG_DO_STATEMENT_EXPRESSION, expression);

    *nodecl_output = nodecl_make_list_1(nodecl_make_do_statement(nodecl_statement, 
                expression_get_nodecl(expression), ASTFileName(a), ASTLine(a)));
}

static void build_scope_empty_statement(AST a, 
        decl_context_t decl_context UNUSED_PARAMETER, 
        char* attr_name,
        nodecl_t* nodecl_output)
{
    // Do nothing
    if (attr_name != NULL)
    {
        ASTAttrSetValueType(a, attr_name, tl_type_t, tl_bool(1));
    }

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_empty_statement(ASTFileName(a), ASTLine(a)));
}

static void build_scope_break(AST a, 
        decl_context_t decl_context UNUSED_PARAMETER, 
        char* attr_name,
        nodecl_t* nodecl_output)
{
    if (attr_name != NULL)
    {
        ASTAttrSetValueType(a, attr_name, tl_type_t, tl_bool(1));
    }

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_break_statement(ASTFileName(a), ASTLine(a)));
}

static void build_scope_continue(AST a, 
        decl_context_t decl_context UNUSED_PARAMETER, 
        char* attr_name,
        nodecl_t* nodecl_output)
{
    // Do nothing
    if (attr_name != NULL)
    {
        ASTAttrSetValueType(a, attr_name, tl_type_t, tl_bool(1));
    }

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_continue_statement(ASTFileName(a), ASTLine(a)));
}

static void build_scope_pragma_custom_directive(AST a, 
        decl_context_t decl_context, 
        char* _dummy UNUSED_PARAMETER,
        nodecl_t* nodecl_output)
{
    common_build_scope_pragma_custom_directive(a, decl_context, nodecl_output);
}

void build_scope_statement_pragma(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output, 
        void* info UNUSED_PARAMETER)
{
    build_scope_statement(a, decl_context, nodecl_output);
}

static void build_scope_pragma_custom_construct_statement(AST a, 
        decl_context_t decl_context, 
        char* attr_name UNUSED_PARAMETER,
        nodecl_t* nodecl_output)
{
    nodecl_t nodecl_pragma_line = nodecl_null();
    common_build_scope_pragma_custom_construct(a, decl_context, nodecl_output, &nodecl_pragma_line, build_scope_statement_pragma, NULL);
    ast_set_link_to_child(a, LANG_PRAGMA_CUSTOM_STATEMENT, ASTSon1(a));
}

static void build_scope_declaration_pragma(AST a,
        decl_context_t decl_context,
        nodecl_t* nodecl_output,
        void *p)
{
    build_scope_declaration(a, decl_context, nodecl_output, (scope_entry_list_t**)p);
}

static void build_scope_pragma_custom_construct_declaration(AST a, 
        decl_context_t decl_context, 
        char* attr_name UNUSED_PARAMETER,
        nodecl_t *nodecl_output)
{
    nodecl_t nodecl_pragma_line = nodecl_null();
    scope_entry_list_t* declared_symbols = NULL;
    common_build_scope_pragma_custom_construct(a, decl_context, 
            nodecl_output, &nodecl_pragma_line, build_scope_declaration_pragma, &declared_symbols);

    scope_entry_list_iterator_t* it = entry_list_iterator_begin(declared_symbols);
    while (!entry_list_iterator_end(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        P_LIST_ADD(entry->entity_specs.pragmas, entry->entity_specs.num_pragmas, nodecl_get_ast(nodecl_pragma_line));

        entry_list_iterator_next(it);
    }

    entry_list_iterator_free(it);
    entry_list_free(declared_symbols);
    ast_set_link_to_child(a, LANG_PRAGMA_CUSTOM_DECLARATION, ASTSon1(a));
}

struct declaration_pragma_member_info_tag
{
    type_t* class_info;
    access_specifier_t current_access;
    scope_entry_list_t** declared_symbols;
};

static void build_scope_member_declaration_pragma(AST a, decl_context_t decl_context, nodecl_t* nodecl_output, void* info)
{
    struct declaration_pragma_member_info_tag* mem = (struct declaration_pragma_member_info_tag*)info;
    build_scope_member_declaration(decl_context, ASTSon1(a), mem->current_access, mem->class_info, nodecl_output, mem->declared_symbols);
}

static void build_scope_pragma_custom_construct_member_declaration(AST a, 
        decl_context_t decl_context, 
        access_specifier_t current_access,
        type_t* class_info,
        nodecl_t* nodecl_output)
{
    scope_entry_list_t* declared_symbols = NULL;
    struct declaration_pragma_member_info_tag mem = { 
        .class_info = class_info, 
        .current_access = current_access, 
        .declared_symbols = &declared_symbols 
    };

    nodecl_t nodecl_pragma_line = nodecl_null();
    common_build_scope_pragma_custom_construct(a, decl_context, nodecl_output, &nodecl_pragma_line, build_scope_member_declaration_pragma, &mem);

    scope_entry_list_iterator_t* it = entry_list_iterator_begin(declared_symbols);
    while (!entry_list_iterator_end(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        P_LIST_ADD(entry->entity_specs.pragmas, entry->entity_specs.num_pragmas, nodecl_get_ast(nodecl_pragma_line));

        entry_list_iterator_next(it);
    }
    entry_list_iterator_free(it);

    ast_set_link_to_child(a, LANG_PRAGMA_CUSTOM_DECLARATION, ASTSon1(a));
}

static void build_scope_upc_synch_statement(AST a, 
        decl_context_t decl_context, 
        char *attr_name,
        nodecl_t* nodecl_output)
{
    ASTAttrSetValueType(a, attr_name, tl_type_t, tl_bool(1));

    nodecl_t nodecl_expression = nodecl_null();
    if (ASTSon0(a) != NULL)
    {
        check_expression(ASTSon0(a), decl_context);
        ast_set_link_to_child(a, UPC_SYNC_STMT_ARGUMENT, ASTSon0(a));

        nodecl_expression = expression_get_nodecl(ASTSon0(a));
    }
    else
    {
        nodecl_expression = nodecl_make_integer_literal(get_signed_int_type(), 
                const_value_get_signed_int(0), ASTFileName(a), ASTLine(a));
    }

    nodecl_expression = nodecl_make_list_1(nodecl_expression);

    *nodecl_output = nodecl_make_builtin_decl(nodecl_expression, attr_name, ASTFileName(a), ASTLine(a));
}

static void build_scope_upc_forall_statement(AST a, 
        decl_context_t decl_context, 
        char *attr_name UNUSED_PARAMETER,
        nodecl_t* nodecl_output)
{
    AST forall_header = ASTSon0(a);
    AST statement = ASTSon1(a);

    AST for_init_statement = ASTSon0(forall_header);
    AST condition = ASTSon1(forall_header);
    AST expression = ASTSon2(forall_header);
    AST affinity = ASTSon3(forall_header);

    if (ASTType(for_init_statement) == AST_AMBIGUITY)
    {
        solve_ambiguous_for_init_statement(for_init_statement, decl_context);
    }

    decl_context_t block_context = new_block_context(decl_context);

    nodecl_t nodecl_for_init = nodecl_null();
    if (ASTType(for_init_statement) == AST_SIMPLE_DECLARATION)
    {
        build_scope_simple_declaration(for_init_statement, block_context, 
                /* is_template */ 0, /* is_explicit_instantiation */ 0,
                &nodecl_for_init, /* declared_symbols */ NULL);
    }
    else if (ASTType(for_init_statement) == AST_EXPRESSION_STATEMENT)
    {
        build_scope_expression_statement(for_init_statement, block_context, NULL, &nodecl_for_init);
    }
    scope_link_set(CURRENT_COMPILED_FILE->scope_link, for_init_statement, block_context);

    nodecl_t nodecl_condition = nodecl_null();
    if (condition != NULL)
    {
        check_expression(condition, block_context);
        nodecl_condition = expression_get_nodecl(condition);
    }
    else
    {
        nodecl_condition = nodecl_make_integer_literal(get_signed_int_type(), 
                const_value_get_signed_int(0), ASTFileName(a), ASTLine(a));
    }

    nodecl_t nodecl_iter = nodecl_null();
    if (expression != NULL)
    {
        check_expression(expression, block_context);
        nodecl_iter = expression_get_nodecl(expression);
    }
    else
    {
        nodecl_iter = nodecl_make_integer_literal(get_signed_int_type(), 
                const_value_get_signed_int(0), ASTFileName(a), ASTLine(a));
    }

    nodecl_t nodecl_affinity = nodecl_null();
    if (affinity != NULL)
    {
        check_expression(affinity, block_context);
        nodecl_affinity = nodecl_make_integer_literal(get_signed_int_type(), 
                const_value_get_signed_int(0), ASTFileName(a), ASTLine(a));
    }
    else
    {
        nodecl_affinity = nodecl_make_integer_literal(get_signed_int_type(), 
                const_value_get_signed_int(0), ASTFileName(a), ASTLine(a));
    }

    nodecl_t* list[] = { 
        &nodecl_for_init, 
        &nodecl_condition,
        &nodecl_iter,
        &nodecl_affinity,
        NULL };

    nodecl_t nodecl_list = nodecl_null();

    int i;
    for (i = 0; list[i] != NULL; i++)
    {
        nodecl_list = nodecl_append_to_list(nodecl_list,
                *(list[i]));
    }

    build_scope_statement(statement, block_context, nodecl_output);
    scope_link_set(CURRENT_COMPILED_FILE->scope_link, statement, block_context);

    ASTAttrSetValueType(a, UPC_IS_FORALL_STATEMENT, tl_type_t, tl_bool(1));
    ast_set_link_to_child(a, UPC_FORALL_INIT_CONSTRUCT, for_init_statement);
    ast_set_link_to_child(a, UPC_FORALL_CONDITION, condition);
    ast_set_link_to_child(a, UPC_FORALL_ITERATION_EXPRESSION, expression);
    ast_set_link_to_child(a, UPC_FORALL_AFFINITY, affinity);
    ast_set_link_to_child(a, UPC_FORALL_BODY_STATEMENT, statement);

    *nodecl_output = nodecl_make_builtin_decl(nodecl_list, UPC_IS_FORALL_STATEMENT, ASTFileName(a), ASTLine(a));
}

#define STMT_HANDLER(type, hndl, attr_name_v) [type] = { .handler = (hndl), .attr_name = (attr_name_v) }

static stmt_scope_handler_map_t stmt_scope_handlers[] =
{
    STMT_HANDLER(AST_AMBIGUITY, build_scope_ambiguity_handler, NULL),
    STMT_HANDLER(AST_EXPRESSION_STATEMENT, build_scope_expression_statement, NULL),
    STMT_HANDLER(AST_DECLARATION_STATEMENT, build_scope_declaration_statement, NULL),
    STMT_HANDLER(AST_COMPOUND_STATEMENT, build_scope_compound_statement, NULL),
    STMT_HANDLER(AST_DO_STATEMENT, build_scope_do_statement, NULL),
    STMT_HANDLER(AST_WHILE_STATEMENT, build_scope_while_statement, NULL),
    STMT_HANDLER(AST_IF_ELSE_STATEMENT, build_scope_if_else_statement, NULL),
    STMT_HANDLER(AST_FOR_STATEMENT, build_scope_for_statement, NULL),
    STMT_HANDLER(AST_LABELED_STATEMENT, build_scope_labeled_statement, NULL),
    STMT_HANDLER(AST_DEFAULT_STATEMENT, build_scope_default_statement, NULL),
    STMT_HANDLER(AST_CASE_STATEMENT, build_scope_case_statement, NULL),
    STMT_HANDLER(AST_RETURN_STATEMENT, build_scope_return_statement, NULL),
    STMT_HANDLER(AST_TRY_BLOCK, build_scope_try_block, NULL),
    STMT_HANDLER(AST_SWITCH_STATEMENT, build_scope_switch_statement, NULL),
    STMT_HANDLER(AST_EMPTY_STATEMENT, build_scope_empty_statement, LANG_IS_EMPTY_STATEMENT),
    STMT_HANDLER(AST_BREAK_STATEMENT, build_scope_break, LANG_IS_BREAK_STATEMENT),
    STMT_HANDLER(AST_CONTINUE_STATEMENT, build_scope_continue, LANG_IS_CONTINUE_STATEMENT),
    STMT_HANDLER(AST_GOTO_STATEMENT, build_scope_goto_statement, NULL),
    // Pragma custom support
    STMT_HANDLER(AST_PRAGMA_CUSTOM_CONSTRUCT, build_scope_pragma_custom_construct_statement, NULL),
    STMT_HANDLER(AST_PRAGMA_CUSTOM_DIRECTIVE, build_scope_pragma_custom_directive, NULL),
    // UPC
    STMT_HANDLER(AST_UPC_NOTIFY, build_scope_upc_synch_statement, UPC_IS_NOTIFY_STATEMENT),
    STMT_HANDLER(AST_UPC_WAIT, build_scope_upc_synch_statement, UPC_IS_WAIT_STATEMENT),
    STMT_HANDLER(AST_UPC_BARRIER, build_scope_upc_synch_statement, UPC_IS_BARRIER_STATEMENT),
    STMT_HANDLER(AST_UPC_FENCE, build_scope_upc_synch_statement, UPC_IS_FENCE_STATEMENT),
    STMT_HANDLER(AST_UPC_FORALL, build_scope_upc_forall_statement, NULL),
};

void build_scope_member_specification_with_scope_link(
        decl_context_t class_context,
        scope_link_t* scope_link,
        AST member_specification_tree, 
        access_specifier_t current_access,
        type_t* simple_type_info,
        nodecl_t* nodecl_output)
{
    scope_link_t* old_scope_link = CURRENT_COMPILED_FILE->scope_link;
    CURRENT_COMPILED_FILE->scope_link = scope_link;

    build_scope_member_specification(class_context, member_specification_tree, 
            current_access, simple_type_info, nodecl_output, /* declared_symbols */ NULL);
    build_scope_delayed_functions(nodecl_output);

    CURRENT_COMPILED_FILE->scope_link = old_scope_link;
}

static void build_scope_statement_seq(AST a, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    nodecl_t nodecl_output_list = nodecl_null();
    AST list = a;
    if (list != NULL)
    {
        AST iter;
        for_each_element(list, iter)
        {
            nodecl_t current_nodecl_output = nodecl_null();

            build_scope_statement(ASTSon1(iter), decl_context, &current_nodecl_output);

            nodecl_output_list = nodecl_concat_lists(nodecl_output_list, current_nodecl_output);
        }
    }

    *nodecl_output = nodecl_output_list;
}

void build_scope_statement_seq_with_scope_link(AST a, 
        decl_context_t decl_context, 
        scope_link_t* scope_link,
        nodecl_t* nodecl_output)
{
    scope_link_t* old_scope_link = CURRENT_COMPILED_FILE->scope_link;
    CURRENT_COMPILED_FILE->scope_link = scope_link;

    build_scope_statement_seq(a, decl_context, nodecl_output);

    CURRENT_COMPILED_FILE->scope_link = old_scope_link;
}

void build_scope_declaration_sequence_with_scope_link(AST a, 
        decl_context_t decl_context, 
        scope_link_t* scope_link, 
        nodecl_t* nodecl_output)
{
    scope_link_t* old_scope_link = CURRENT_COMPILED_FILE->scope_link;
    CURRENT_COMPILED_FILE->scope_link = scope_link;

    build_scope_declaration_sequence(a, decl_context, nodecl_output);

    CURRENT_COMPILED_FILE->scope_link = old_scope_link;
}

static void build_scope_statement_(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "==== Statement line [%s] ====\n", ast_location(a));
    }
    
    stmt_scope_handler_t f = stmt_scope_handlers[ASTType(a)].handler;
    char* attr_name = stmt_scope_handlers[ASTType(a)].attr_name;
    
    if (f != NULL)
    {
        f(a, decl_context, attr_name, nodecl_output);
    }
    else
    {
        WARNING_MESSAGE("Statement node type '%s' does not have handler in %s", ast_print_node_type(ASTType(a)),
                        ast_location(a));
    }
}

void build_scope_statement(AST a, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    build_scope_statement_(a, decl_context, nodecl_output);
    
    ASTAttrSetValueType(a, LANG_IS_STATEMENT, tl_type_t, tl_bool(1));
}

AST get_function_declarator_parameter_list(AST funct_declarator, decl_context_t decl_context)
{
    ERROR_CONDITION((funct_declarator == NULL), "This function does not admit NULL trees", 0);

    switch (ASTType(funct_declarator))
    {
        case AST_DECLARATOR :
        case AST_PARENTHESIZED_DECLARATOR :
            {
                return get_function_declarator_parameter_list(ASTSon0(funct_declarator), decl_context); 
                break;
            }
        case AST_POINTER_DECLARATOR :
            {
                return get_function_declarator_parameter_list(ASTSon1(funct_declarator), decl_context);
                break;
            }
        case AST_DECLARATOR_FUNC :
        case AST_GCC_FUNCTIONAL_DECLARATOR :
            {
                return ASTSon1(funct_declarator);
                break;
            }
        case AST_GCC_DECLARATOR :
            {
                return get_function_declarator_parameter_list(ASTSon1(funct_declarator), decl_context);
                break;
            }
        case AST_GCC_POINTER_DECLARATOR :
            {
                return get_function_declarator_parameter_list(ASTSon2(funct_declarator), decl_context);
                break;
            }
        default:
            {
                internal_error("Unknown node '%s' at '%s'\n", ast_print_node_type(ASTType(funct_declarator)),
                        ast_location(funct_declarator));
                break;
            }
    }

    return NULL;
}

/*
 * This function returns the node that holds the name for a non-abstract
 * declarator
 */
AST get_declarator_name(AST a, decl_context_t decl_context)
{
    AST declarator_name = NULL;

    AST declarator_id_expr = get_declarator_id_expression(a, decl_context);

    if (declarator_id_expr != NULL)
    {
        declarator_name = ASTSon0(declarator_id_expr);
    }

    return declarator_name;
}

AST get_declarator_id_expression(AST a, decl_context_t decl_context)
{
    if (a == NULL)
        return NULL;

    switch(ASTType(a))
    {
        case AST_INIT_DECLARATOR :
        case AST_MEMBER_DECLARATOR :
        case AST_GCC_MEMBER_DECLARATOR :
        case AST_DECLARATOR :
        case AST_PARENTHESIZED_DECLARATOR :
            {
                return get_declarator_id_expression(ASTSon0(a), decl_context); 
                break;
            }
        case AST_POINTER_DECLARATOR :
            {
                return get_declarator_id_expression(ASTSon1(a), decl_context);
                break;
            }
        case AST_DECLARATOR_ARRAY :
            {
                return get_declarator_id_expression(ASTSon0(a), decl_context);
                break;
            }
        case AST_DECLARATOR_FUNC :
            {
                return get_declarator_id_expression(ASTSon0(a), decl_context);
                break;
            }
        case AST_DECLARATOR_ID_EXPR :
            {
                return a;
                break;
            }
        case AST_GCC_DECLARATOR :
            {
                return get_declarator_id_expression(ASTSon1(a), decl_context);
                break;
            }
        case AST_GCC_POINTER_DECLARATOR :
            {
                return get_declarator_id_expression(ASTSon2(a), decl_context);
                break;
            }
        case AST_GCC_FUNCTIONAL_DECLARATOR :
            {
                return get_declarator_id_expression(ASTSon0(a), decl_context);
            }
        case AST_AMBIGUITY :
            {
                solve_ambiguous_declarator(a, decl_context);

                // Restart function
                return get_declarator_id_expression(a, decl_context);
            }
        default:
            {
                internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(a)));
            }
    }
}

#if 0
static AST advance_over_declarator_nests(AST a, decl_context_t decl_context)
{
    ERROR_CONDITION(a == NULL, "This node cannot be null", 0);
    ERROR_CONDITION(ASTType(a) == AST_AMBIGUITY, "This node should not be ambiguous here", 0);
    ERROR_CONDITION(get_declarator_name(a, decl_context) == NULL, "This should be a non-abstract declarator", 0);

    switch(ASTType(a))
    {
        case AST_INIT_DECLARATOR :
        case AST_MEMBER_DECLARATOR :
        case AST_DECLARATOR :
        case AST_PARENTHESIZED_DECLARATOR :
            {
                return advance_over_declarator_nests(ASTSon0(a), decl_context); 
                break;
            }
	// Note: GCC declarators are not advanced as we want their attributes
	// to be visible later
        default:
            return a;
    }
}
#endif

// Returns non-null tree with the leftmost declarator
// that is not preceded by any other sign 
//
//   int f(); // Returns the tree holding 'f'
//   int (f()); // Returns null
//   int a[10]; // Returns the tree holding 'a'
AST get_leftmost_declarator_name(AST a, decl_context_t decl_context)
{
    if (a == NULL)
        return NULL;

    switch(ASTType(a))
    {
        case AST_DECLARATOR :
        case AST_MEMBER_DECLARATOR :
        case AST_INIT_DECLARATOR :
            {
                return get_leftmost_declarator_name(ASTSon0(a), decl_context); 
                break;
            }
        case AST_PARENTHESIZED_DECLARATOR :
            {
                return NULL;
            }
        case AST_POINTER_DECLARATOR :
            {
                return NULL;
            }
        case AST_DECLARATOR_ARRAY :
            {
                return get_leftmost_declarator_name(ASTSon0(a), decl_context);
                break;
            }
        case AST_DECLARATOR_FUNC :
            {
                return get_leftmost_declarator_name(ASTSon0(a), decl_context);
                break;
            }
        case AST_DECLARATOR_ID_EXPR :
            {
                return ASTSon0(a);
                break;
            }
        case AST_AMBIGUITY :
            {
                solve_ambiguous_declarator(a, decl_context);
                // Restart function
                return get_leftmost_declarator_name(a, decl_context);
            }
        default:
            {
                internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(a)));
            }
    }
}

char* get_conversion_function_name(decl_context_t decl_context, 
        AST conversion_function_id, 
        type_t** result_conversion_type,
        nodecl_t* nodecl_output)
{
    if (ASTType(conversion_function_id) != AST_CONVERSION_FUNCTION_ID)
    {
        internal_error("This node '%s' is not valid for this function", 
                ast_print_node_type(ASTType(conversion_function_id)));
    }

    AST conversion_type_id = ASTSon0(conversion_function_id);

    AST type_specifier = ASTSon0(conversion_type_id);
    AST conversion_declarator = ASTSon1(conversion_type_id);

    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));
    type_t* simple_type_info = NULL;

    build_scope_decl_specifier_seq(type_specifier, &gather_info, &simple_type_info,
            decl_context, nodecl_output);

    type_t* type_info = NULL;
    compute_declarator_type(conversion_declarator, &gather_info, simple_type_info, &type_info,
            decl_context, nodecl_output);

    if (result_conversion_type != NULL)
    {
        *result_conversion_type = type_info;
    }

    return "$.operator";
}

static AST get_enclosing_declaration(AST point_of_declarator)
{
    AST point = point_of_declarator;

    while (point != NULL
            && ASTType(point) != AST_SIMPLE_DECLARATION
            && ASTType(point) != AST_MEMBER_DECLARATION
            && ASTType(point) != AST_FUNCTION_DEFINITION
            && ASTType(point) != AST_DELETED_FUNCTION_DEFINITION
            && ASTType(point) != AST_DEFAULTED_FUNCTION_DEFINITION
            && ASTType(point) != AST_PARAMETER_DECL
            && ASTType(point) != AST_EXPLICIT_INSTANTIATION
            && ASTType(point) != AST_TYPE_ID)
    {
        point = ASTParent(point);
    }

    ERROR_CONDITION(point == NULL,
            "This cannot be NULL!", 0);

    return point;
}

AST internal_expression_parse(const char *source, decl_context_t decl_context)
{
    const char *mangled_text = strappend("@EXPRESSION@ ", source);

    CXX_LANGUAGE()
    {
        mcxx_prepare_string_for_scanning(mangled_text);
    }
    C_LANGUAGE()
    {
        mc99_prepare_string_for_scanning(mangled_text);
    }

    AST a = NULL;
    int parse_result = 0;

    CXX_LANGUAGE()
    {
        parse_result = mcxxparse(&a);
    }
    C_LANGUAGE()
    {
        parse_result = mc99parse(&a);
    }

    if (parse_result != 0)
    {
        internal_error("Could not parse the expression '%s'", source);
    }

    enter_test_expression();
    char c = check_expression(a, decl_context);
    leave_test_expression();

    if (!c)
    {
        internal_error("Internally parsed expression '%s' could not be properly checked\n",
                prettyprint_in_buffer(a));
    }

    scope_link_set(CURRENT_COMPILED_FILE->scope_link, a, decl_context);

    return a;
}

scope_entry_t* entry_advance_aliases(scope_entry_t* entry)
{
    if (entry != NULL 
            && entry->kind == SK_USING)
        return entry_advance_aliases(entry->entity_specs.alias_to);

    return entry;
}
