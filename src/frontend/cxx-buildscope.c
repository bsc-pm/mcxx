/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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
#include "cxx-gccsupport.h"
#include "cxx-gccbuiltins.h"
#include "cxx-gccspubuiltins.h"
#include "cxx-mssupport.h"
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
#include "cxx-codegen.h"
#include "cxx-placeholders.h"
#include "cxx-driver-utils.h"

/*
 * This file builds symbol table. If ambiguous nodes are found disambiguating
 * routines will be called prior to filling symbolic inormation. Note that
 * disambiguating routines will use the currently built symbol table.
 *
 * Note that some "semantic checks" performed here are intended only to verify
 * that lookup and symbol registration are performed correctly. By no means
 * this is a full type checking phase
 */

static void gather_extra_attributes(AST a, gather_decl_spec_t* gather_info,
        decl_context_t decl_context);

static void apply_attributes_to_type(type_t** type,
        AST attribute_seq,
        decl_context_t decl_context);

static void gather_virt_specifiers(AST a,
        gather_decl_spec_t* gather_info,
        decl_context_t decl_context);
static void gather_single_virt_specifier(AST item,
        gather_decl_spec_t* gather_info,
        decl_context_t decl_context);

static void build_scope_simple_declaration(AST a, decl_context_t decl_context, 
        char is_template, char is_explicit_specialization,
        nodecl_t *nodecl_output, 
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);

static scope_entry_t* build_scope_function_definition(
        AST function_definition,
        decl_context_t decl_context,
        char is_template,
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);

static void build_scope_namespace_alias(AST a, decl_context_t decl_context, nodecl_t *nodecl_output);
static void build_scope_namespace_definition(AST a, decl_context_t decl_context, nodecl_t* nodecl_output);
static void build_scope_declarator_with_parameter_context(AST a, 
        gather_decl_spec_t* gather_info, type_t* simple_type_info, type_t** declarator_type,
        decl_context_t decl_context, decl_context_t *prototype_context,
        nodecl_t* nodecl_output);

static void build_scope_member_specification(decl_context_t inner_decl_context, AST member_specification_tree, 
        access_specifier_t default_current_access, type_t* type_info, nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);

static void build_scope_member_declaration(decl_context_t inner_decl_context,
        AST a, access_specifier_t current_access, type_t* class_info,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);

static scope_entry_t* build_scope_member_function_definition(decl_context_t decl_context, AST a,
        access_specifier_t current_access,
        type_t* class_info,
        char is_template,
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);
static void build_scope_default_or_delete_member_function_definition(decl_context_t decl_context, 
        AST a, access_specifier_t current_access, type_t* class_info,
        char is_template,
        char is_explicit_specialization,
        nodecl_t* nodecl_output);
static void build_scope_member_simple_declaration(decl_context_t decl_context, AST a, 
        access_specifier_t current_access, type_t* class_info, 
        char is_template,
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);

static void common_gather_type_spec_from_simple_type_specifier(AST a, 
        decl_context_t decl_context, type_t** type_info,
        gather_decl_spec_t* gather_info, scope_entry_list_t* query_results);

static void gather_type_spec_from_simple_type_specifier(AST a, type_t** type_info,
        gather_decl_spec_t* gather_info, decl_context_t decl_context);

static void nodecl_gather_type_spec_from_simple_type_specifier(nodecl_t a, type_t** type_info,
        gather_decl_spec_t* gather_info, decl_context_t decl_context);

static void gather_type_spec_from_enum_specifier(AST a, type_t** type_info, 
        gather_decl_spec_t* gather_info,
        decl_context_t decl_context,
        nodecl_t* nodecl_output);
static void gather_type_spec_from_class_specifier(AST a, type_t** type_info,
        gather_decl_spec_t* gather_info,
        decl_context_t decl_context,
        nodecl_t* nodecl_output);
static void gather_type_spec_from_dependent_typename(AST a, 
        type_t** type_info,
        gather_decl_spec_t* gather_info,
        decl_context_t decl_context);

static void gather_type_spec_from_elaborated_friend_class_specifier(AST a,
        type_t** type_info,
        gather_decl_spec_t *gather_info,
        decl_context_t decl_context,
        nodecl_t* nodecl_output);

static void gather_type_spec_from_elaborated_class_specifier(AST a, 
        type_t** type_info,
        gather_decl_spec_t *gather_info,
        decl_context_t decl_context,
        nodecl_t* nodecl_output);
static void gather_type_spec_from_elaborated_enum_specifier(AST a, 
        type_t** type_info, 
        gather_decl_spec_t* gather_info, 
        decl_context_t decl_context,
        nodecl_t* nodecl_output);

static void gather_extra_attributes_in_declarator(AST a, gather_decl_spec_t* gather_info, 
        decl_context_t declarator_context);
static void build_scope_declarator_rec(
        AST a, type_t** declarator_type, 
        gather_decl_spec_t* gather_info,
        decl_context_t declarator_context,
        decl_context_t entity_context,
        decl_context_t *prototype_context,
        nodecl_t* nodecl_output);

static scope_entry_t* build_scope_declarator_name(AST declarator, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, decl_context_t decl_context);
static scope_entry_t* build_scope_declarator_id_expr(AST declarator_name, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, decl_context_t decl_context);

static void build_scope_linkage_specifier(AST a, decl_context_t decl_context, nodecl_t* nodecl_output);
static void build_scope_linkage_specifier_declaration(AST a, 
        AST top_linkage_decl, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);

static void build_scope_template_declaration(AST a, 
        AST top_template_decl, 
        decl_context_t decl_context,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);
static void build_scope_explicit_template_specialization(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);

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
        char is_template_pack,
        decl_context_t decl_context,
        nodecl_t* nodecl_output);
static void build_scope_template_template_parameter(AST a,
        template_parameter_list_t* template_parameter_list,
        int nesting,
        char is_template_pack,
        decl_context_t decl_context,
        nodecl_t* nodecl_output);

static void build_scope_member_template_declaration(decl_context_t decl_context, AST a, 
        access_specifier_t current_access, type_t* class_info,
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);
static void build_scope_member_template_function_definition(decl_context_t decl_context,
        AST a, access_specifier_t current_access, type_t* class_info, 
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);

static void build_scope_member_template_simple_declaration(decl_context_t decl_context, AST a,
        access_specifier_t current_access, type_t* class_info,
        char is_explicit_specialization,
        nodecl_t* nodecl_output);

static void build_scope_static_assert(AST a, decl_context_t decl_context);

static void build_scope_simple_alias_declaration(AST a, decl_context_t decl_context, nodecl_t* nodecl_output);
static void build_scope_member_alias_declaration(AST a, decl_context_t decl_context, nodecl_t* nodecl_output,
        type_t* class_info, access_specifier_t access_specifier);

static void build_scope_template_alias_declaration(AST a, decl_context_t decl_context, nodecl_t* nodecl_output);
static void build_scope_member_template_alias_declaration(decl_context_t decl_context,
        AST a, access_specifier_t current_access, type_t* class_info,
        char is_explicit_specialization,
        nodecl_t* nodecl_output);
static void build_scope_default_or_delete_template_member_function_definition(
        decl_context_t decl_context, 
        AST a, access_specifier_t current_access, type_t* class_info,
        char is_explicit_specialization,
        nodecl_t* nodecl_output);

static void common_defaulted_or_deleted(AST a, decl_context_t decl_context, 
        void (*set)(scope_entry_t*, decl_context_t, const locus_t*),
        char is_template,
        char is_explicit_specialization,
        scope_entry_list_t** declared_symbols,
        nodecl_t* nodecl_output);

static void build_scope_defaulted_function_definition(AST a, decl_context_t decl_context, nodecl_t* nodecl_output);
static void build_scope_deleted_function_definition(AST a, decl_context_t decl_context, nodecl_t* nodecl_output);

static void build_scope_using_directive(AST a, decl_context_t decl_context, nodecl_t* nodecl_output);
static void build_scope_using_declaration(AST a, decl_context_t decl_context, access_specifier_t, 
        char is_typename,
        nodecl_t* nodecl_output);

static void build_scope_member_declaration_qualified(AST a, decl_context_t decl_context, access_specifier_t, nodecl_t* nodecl_output);

static void build_scope_template_function_definition(AST a, decl_context_t decl_context, 
        char is_explicit_specialization, 
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols, 
        gather_decl_spec_list_t* gather_decl_spec_list);

static void build_scope_template_deleted_function_definition(
        AST function_declaration,
        decl_context_t decl_context,
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);

static void build_scope_template_defaulted_function_definition(
        AST function_declaration,
        decl_context_t decl_context,
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);

static void build_scope_explicit_instantiation(AST a, decl_context_t decl_context, nodecl_t* nodecl_output);

static scope_entry_t* register_new_typedef_name(AST declarator_id, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, decl_context_t decl_context);
static scope_entry_t* register_new_variable_name(AST declarator_id, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, decl_context_t decl_context);
static scope_entry_t* register_function(AST declarator_id, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, decl_context_t decl_context);

static void build_scope_template_simple_declaration(AST a, decl_context_t decl_context, 
        char is_explicit_specialization, 
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);

static void build_scope_gcc_asm_definition(AST a, decl_context_t decl_context, nodecl_t* nodecl_output);
static void build_scope_asm_definition(AST a, decl_context_t decl_context, nodecl_t* nodecl_output);

static cv_qualifier_t compute_cv_qualifier(AST a);

static void build_exception_spec(type_t* function_type, AST a,
        gather_decl_spec_t *gather_info,
        decl_context_t decl_context,
        decl_context_t prototype_context,
        nodecl_t* nodecl_output);

static char is_constructor_declarator(AST a);

static char find_function_declaration(AST declarator_id, 
        type_t* declarator_type, 
        gather_decl_spec_t* gather_info,
        decl_context_t decl_context,
        scope_entry_t** result_entry);

static void build_scope_pragma_custom_directive(AST a, decl_context_t decl_context, 
        nodecl_t* nodecl_output);
static void build_scope_pragma_custom_construct_declaration(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output);
static void build_scope_pragma_custom_construct_member_declaration(AST a, 
        decl_context_t decl_context, 
        access_specifier_t current_access,
        type_t* class_info,
        nodecl_t* nodecl_output);

static void call_destructors_of_classes(decl_context_t block_context, 
        const locus_t* locus,
        nodecl_t* nodecl_output);

typedef struct linkage_stack_tag { const char* name; char is_braced; } linkage_stack_t;

// Current linkage: NULL means the default linkage (if any) of the symbol
static linkage_stack_t _linkage_stack[MCXX_MAX_LINKAGE_NESTING] = { { NULL, 1 } };
static int _top_linkage_stack = 0;

static scope_entry_t* _extra_declaration[MCXX_MAX_EXTRA_DECLARATIONS] = { };
static int _extra_declaration_idx = 0;

void push_extra_declaration_symbol(scope_entry_t* entry)
{
    ERROR_CONDITION(_extra_declaration_idx == MCXX_MAX_EXTRA_DECLARATIONS,
            "Too many extra declarations in expression", 0);
    _extra_declaration[_extra_declaration_idx] = entry;
    _extra_declaration_idx++;
}

scope_entry_t* pop_extra_declaration_symbol(void)
{
    if (_extra_declaration_idx > 0)
    {
        _extra_declaration_idx--;
        return _extra_declaration[_extra_declaration_idx];
    }
    return NULL;
}

static const char* linkage_current_get_name(void)
{
    return _linkage_stack[_top_linkage_stack].name;
}

static char linkage_current_is_braced(void)
{
    return _linkage_stack[_top_linkage_stack].is_braced;
}

static void linkage_push(const char* linkage_name, char is_braced)
{
    _top_linkage_stack++;
    ERROR_CONDITION(_top_linkage_stack == MCXX_MAX_LINKAGE_NESTING, "Too many linkage nesting levels", 0);

    _linkage_stack[_top_linkage_stack].name = linkage_name;
    _linkage_stack[_top_linkage_stack].is_braced = is_braced;
}

static void linkage_pop(void)
{
    ERROR_CONDITION(_top_linkage_stack == 0, "Linkage stack is empty!", 0);
    _top_linkage_stack--;
}

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
}

void c_initialize_translation_unit_scope(translation_unit_t* translation_unit)
{
    decl_context_t decl_context;
    initialize_translation_unit_scope(translation_unit, &decl_context);
    c_initialize_builtin_symbols(decl_context);
}

static void build_scope_translation_unit_pre(translation_unit_t* translation_unit UNUSED_PARAMETER)
{
    C_LANGUAGE()
    {
        linkage_push("\"C\"", /* is_braced */ 1);
    }
    CXX_LANGUAGE()
    {
        instantiation_init();
    }
}

static void build_scope_translation_unit_post(
        translation_unit_t* translation_unit UNUSED_PARAMETER,
        nodecl_t* nodecl_output)
{
    CXX_LANGUAGE()
    {
        if (CURRENT_CONFIGURATION->explicit_instantiation)
        {
            instantiation_instantiate_pending_functions(nodecl_output);
        }
    }
    C_LANGUAGE()
    {
        linkage_pop();
    }
}

// Builds scope for the translation unit
nodecl_t build_scope_translation_unit(translation_unit_t* translation_unit)
{
    AST a = translation_unit->parsed_tree;
    decl_context_t decl_context = translation_unit->global_decl_context;

    nodecl_t nodecl = nodecl_null();

    build_scope_translation_unit_pre(translation_unit);

    AST list = ASTSon0(a);
    if (list != NULL)
    {
        build_scope_declaration_sequence(list, decl_context, &nodecl);
    }
    build_scope_translation_unit_post(translation_unit, &nodecl);

    return nodecl;
}

static default_argument_info_t** empty_default_argument_info(int num_parameters)
{
    return counted_xcalloc(sizeof(default_argument_info_t*), num_parameters, &_bytes_used_buildscope);
}

// This function initialize global symbols that exist in every translation unit
// prior to its translation
void c_initialize_builtin_symbols(decl_context_t decl_context)
{
    // __builtin_va_list is a very special type in GCC
    scope_entry_t* builtin_va_list;

    builtin_va_list = new_symbol(decl_context, decl_context.global_scope, uniquestr("__builtin_va_list"));
    builtin_va_list->kind = SK_TYPEDEF;
    builtin_va_list->defined = 1;
    builtin_va_list->type_information = get_gcc_builtin_va_list_type();
    builtin_va_list->do_not_print = 1;
    builtin_va_list->locus = make_locus("(global scope)", 0, 0);

    CXX_LANGUAGE()
    {
        {
            // Namespace std preexists
            scope_entry_t* namespace_std = new_symbol(decl_context, decl_context.global_scope, uniquestr("std"));
            namespace_std->kind = SK_NAMESPACE;
            namespace_std->entity_specs.is_user_declared = 1;

            decl_context_t namespace_std_context = new_namespace_context(decl_context, namespace_std);
            namespace_std->related_decl_context = namespace_std_context;
        }

        // There are two 'operator new' and two 'operator delete' at global scope
        {
            scope_entry_t* global_operator_new;
            global_operator_new = new_symbol(decl_context, decl_context.global_scope, uniquestr("operator new"));
            global_operator_new->kind = SK_FUNCTION;
            global_operator_new->do_not_print = 1;

            type_t* return_type = get_pointer_type(get_void_type());

            parameter_info_t parameter_info[1] = { 
                { .is_ellipsis = 0, .type_info = get_size_t_type() } 
            };

            global_operator_new->type_information = get_new_function_type(return_type, parameter_info, 1, REF_QUALIFIER_NONE);
            global_operator_new->entity_specs.num_parameters = 1;
            global_operator_new->entity_specs.default_argument_info 
                = empty_default_argument_info( /* num_parameters */ 1);

            global_operator_new->locus = make_locus("(global scope)", 0, 0);
        }
        // Version for arrays
        {
            scope_entry_t* global_operator_new;
            global_operator_new = new_symbol(decl_context, decl_context.global_scope, uniquestr("operator new[]"));
            global_operator_new->kind = SK_FUNCTION;
            global_operator_new->do_not_print = 1;

            type_t* return_type = get_pointer_type(get_void_type());

            parameter_info_t parameter_info[1] = { 
                { .is_ellipsis = 0, .type_info = get_size_t_type() } 
            };

            global_operator_new->type_information = get_new_function_type(return_type, parameter_info, 1, REF_QUALIFIER_NONE);
            global_operator_new->entity_specs.num_parameters = 1;
            global_operator_new->entity_specs.default_argument_info 
                = empty_default_argument_info(/* num_parameters */ 1);

            global_operator_new->locus = make_locus("(global scope)", 0, 0);
        }

        {
            scope_entry_t* global_operator_delete;
            global_operator_delete = new_symbol(decl_context, decl_context.global_scope, uniquestr("operator delete"));
            global_operator_delete->kind = SK_FUNCTION;
            global_operator_delete->do_not_print = 1;

            type_t* return_type = get_void_type();

            parameter_info_t parameter_info[1] = { 
                { .is_ellipsis = 0, .type_info = get_pointer_type(get_void_type()) } 
            };

            global_operator_delete->type_information = get_new_function_type(return_type, parameter_info, 1, REF_QUALIFIER_NONE);
            global_operator_delete->entity_specs.num_parameters = 1;
            global_operator_delete->entity_specs.default_argument_info
                = empty_default_argument_info(/* num_parameters */ 1);

            global_operator_delete->locus = make_locus("(global scope)", 0, 0);
        }
        {
            scope_entry_t* global_operator_delete;
            global_operator_delete = new_symbol(decl_context, decl_context.global_scope, uniquestr("operator delete[]"));
            global_operator_delete->kind = SK_FUNCTION;
            global_operator_delete->do_not_print = 1;

            type_t* return_type = get_void_type();

            parameter_info_t parameter_info[1] = { 
                { .is_ellipsis = 0, .type_info = get_pointer_type(get_void_type()) } 
            };

            global_operator_delete->type_information = get_new_function_type(return_type, parameter_info, 1, REF_QUALIFIER_NONE);
            global_operator_delete->entity_specs.num_parameters = 1;
            global_operator_delete->entity_specs.default_argument_info
                = empty_default_argument_info(/* num_parameters */ 1);

            global_operator_delete->locus = make_locus("(global scope)", 0, 0);
        }
    }

    gcc_sign_in_builtins(decl_context);

    C_LANGUAGE()
    {
        // This is reserved for C only
        gcc_sign_in_spu_builtins(decl_context);

        if (CURRENT_CONFIGURATION->enable_upc)
        {
            upc_sign_in_builtins(decl_context);
        }
    }

#ifdef HAVE_INT128
    {
        scope_entry_t* __int128_t_type = new_symbol(decl_context, decl_context.global_scope, uniquestr("__int128_t"));
        __int128_t_type->kind = SK_TYPEDEF;
        __int128_t_type->type_information = get_signed_int128_type();
        __int128_t_type->locus = make_locus("(global scope)", 0, 0);
    }
    {
        scope_entry_t* __uint128_t_type = new_symbol(decl_context, decl_context.global_scope, uniquestr("__uint128_t"));
        __uint128_t_type->kind = SK_TYPEDEF;
        __uint128_t_type->type_information = get_unsigned_int128_type();
        __uint128_t_type->locus = make_locus("(global scope)", 0, 0);
    }
#endif
    // Mercurium limit constants

    struct {
        const char* base_name;
        type_t* related_type;
    }
    mercurium_constant_limits[] = {
        { "mercurium_dbl", get_double_type() },
        { "mercurium_flt", get_float_type() },
        { "mercurium_int", get_signed_int_type() },
        { "mercurium_uint", get_unsigned_int_type() },
        { "mercurium_ldbl", get_long_double_type() },
        { "mercurium_long", get_signed_long_int_type() },
        { "mercurium_long_long", get_signed_long_long_int_type() },
        { "mercurium_ulong", get_unsigned_long_int_type() },
        { "mercurium_ulong_long", get_unsigned_long_long_int_type() },
        { "mercurium_ptrdiff", CURRENT_CONFIGURATION->type_environment->type_of_ptrdiff_t() },
        { "mercurium_char", get_char_type() },
        { "mercurium_schar", get_signed_char_type() },
        { "mercurium_uchar", get_unsigned_char_type() },
        { "mercurium_shrt", get_signed_short_int_type() },
        { "mercurium_ushrt", get_unsigned_short_int_type() },
        { "mercurium_size", get_size_t_type() },
        { "mercurium_wchar", get_wchar_t_type() },
        { NULL, NULL }
    };

    int i;
    for (i = 0; mercurium_constant_limits[i].base_name != NULL; i++)
    {
        const char* base_name = mercurium_constant_limits[i].base_name;
        type_t* current_type = mercurium_constant_limits[i].related_type;

        ERROR_CONDITION(current_type == NULL, "Invalid type %s\n", base_name);

        const_value_t *value_max = NULL, *value_min = NULL;
        if (is_floating_type(current_type))
        {
            value_max = floating_type_get_maximum(current_type);
            value_min = floating_type_get_minimum(current_type);
        }
        else if (is_integral_type(current_type))
        {
            value_max = integer_type_get_maximum(current_type);
            value_min = integer_type_get_minimum(current_type);
        }
        else
        {
            internal_error("Only integer or floating types are to be found here. Type is '%s'", 
                    print_declarator(current_type))
        }

        scope_entry_t* max_sym = new_symbol(decl_context, decl_context.global_scope, 
                strappend(base_name, "_max"));
        max_sym->kind = SK_VARIABLE;
        max_sym->type_information = get_const_qualified_type(current_type);
        max_sym->locus = make_locus("(global scope)", 0, 0);
        max_sym->value = const_value_to_nodecl(value_max);
        max_sym->entity_specs.is_user_declared = 1;

        scope_entry_t* min_sym = new_symbol(decl_context, decl_context.global_scope, 
                strappend(base_name, "_min"));
        min_sym->kind = SK_VARIABLE;
        min_sym->type_information = get_const_qualified_type(current_type);
        min_sym->locus = make_locus("(global scope)", 0, 0);
        min_sym->value = const_value_to_nodecl(value_min);
        min_sym->entity_specs.is_user_declared = 1;
    }
}

void build_scope_declaration_sequence(AST list, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output_list)
{
    AST iter;
    for_each_element(list, iter)
    {
        nodecl_t current_nodecl_output_list = nodecl_null();
        build_scope_declaration(ASTSon1(iter), decl_context, &current_nodecl_output_list, 
                /* declared_symbols */ NULL, /* gather_decl_spec_list_t */ NULL);

        *nodecl_output_list = nodecl_concat_lists(*nodecl_output_list, current_nodecl_output_list);
    }
}

// We need to keep some state here
static char gcc_extension = 0;

// Build scope for a declaration
void build_scope_declaration(AST a, decl_context_t decl_context, 
        nodecl_t* nodecl_output, 
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t *gather_decl_spec_list)
{
    // NOTE: if nodecl_output is not nodecl_null it should return a list
    DEBUG_CODE()
    {
        fprintf(stderr, "==== Declaration line [%s] ====\n", ast_location(a));
    }

    diagnostic_context_push_buffered();

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
                        /* is_explicit_specialization */ 0,
                        nodecl_output, 
                        declared_symbols, 
                        gather_decl_spec_list);
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
                build_scope_namespace_alias(a, decl_context, nodecl_output);
                break;
            }
        case AST_FUNCTION_DEFINITION :
            {
                // A function definition is of the form
                //   [T] f(T1 [t1], T2 [t2], T3 [t3])
                //   {
                //     ...
                //   }
                build_scope_function_definition(a, decl_context, 
                        /* is_template */ 0, /* is_explicit_specialization */ 0, 
                        nodecl_output, declared_symbols, gather_decl_spec_list);
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
                build_scope_linkage_specifier_declaration(a, a, decl_context, nodecl_output, declared_symbols, gather_decl_spec_list);
                break;
            }
        case AST_EXPORT_TEMPLATE_DECLARATION :
        case AST_TEMPLATE_DECLARATION :
            {
                // [export] template<typename _T> struct A;
                // [export] template<typename _T> struct A { };
                // [export] template<typename _T> void f(_T t);
                build_scope_template_declaration(a, a, decl_context, nodecl_output, declared_symbols, gather_decl_spec_list);
                break;
            }
        case AST_EXPLICIT_INSTANTIATION :
            {
                // template struct A<int>;
                build_scope_explicit_instantiation(a, decl_context, nodecl_output);
                break;
            }
        case AST_EXPLICIT_SPECIALIZATION :
            {
                // template<> struct A<int> { };
                build_scope_explicit_template_specialization(a, decl_context, 
                        nodecl_output, declared_symbols, gather_decl_spec_list);
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
                // using typename A::b;
                if (ASTType(a) == AST_USING_DECLARATION_TYPENAME)
                    error_printf("%s: error: 'using typename' is only valid in member-declarations\n",
                            ast_location(a));

                build_scope_using_declaration(a, decl_context, AS_UNKNOWN,
                        /* is_typename */ ASTType(a) == AST_USING_DECLARATION_TYPENAME,
                        nodecl_output);
                break;
            }
        case AST_STATIC_ASSERT:
            {
                build_scope_static_assert(a, decl_context);
                break;
            }
        case AST_ALIAS_DECLARATION:
            {
                build_scope_simple_alias_declaration(a, decl_context, nodecl_output);
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
                build_scope_declaration(a, decl_context, nodecl_output, 
                        declared_symbols, gather_decl_spec_list);
                break;
            }
        case AST_EMPTY_DECL :
            {
                // Do nothing
                break;
            }
        case AST_ASM_DEFINITION :
            {
                build_scope_asm_definition(a, decl_context, nodecl_output);
                break;
            }
        case AST_UNKNOWN_PRAGMA :
            {
                *nodecl_output = 
                    nodecl_make_list_1(
                            nodecl_make_unknown_pragma(ASTText(a), ast_get_locus(a)));
                break;
            }
        case AST_PRAGMA_CUSTOM_DIRECTIVE :
            {
                build_scope_pragma_custom_directive(a, decl_context, nodecl_output);
                break;
            }
        case AST_PRAGMA_CUSTOM_CONSTRUCT: 
            {
                build_scope_pragma_custom_construct_declaration(a, decl_context, nodecl_output);
                break;
            }
            // GCC Extensions
        case AST_GCC_EXTENSION : // __extension__
            {
                gcc_extension = 1;

                build_scope_declaration(ASTSon0(a), decl_context, nodecl_output,
                        declared_symbols, gather_decl_spec_list);

                gcc_extension = 0;
                break;
            }
        case AST_GCC_ASM_DEFINITION :
            {
                build_scope_gcc_asm_definition(a, decl_context, nodecl_output);
                break;
            }
        case AST_PP_COMMENT :
            {
                *nodecl_output = 
                    nodecl_make_list_1(
                            nodecl_make_source_comment(ASTText(a), ast_get_locus(a)));

                break;
            }
        case AST_PP_TOKEN :
            {
                *nodecl_output = 
                    nodecl_make_list_1(
                            nodecl_make_preprocessor_line(ASTText(a), ast_get_locus(a)));

                break;
            }
        case AST_VERBATIM :
            {
                *nodecl_output = 
                    nodecl_make_list_1(
                            nodecl_make_verbatim(ASTText(a), ast_get_locus(a)));
                break;
            }
        default :
            {
                internal_error("A declaration of kind '%s' is still unsupported (%s)\n", 
                        ast_print_node_type(ASTType(a)), ast_location(a));
                break;
            }
    }

    diagnostic_context_pop_and_commit();
}

static void build_scope_asm_definition(AST a, 
        decl_context_t decl_context UNUSED_PARAMETER, 
        nodecl_t* nodecl_output)
{
    AST string_literal = ASTSon0(a);
    AST volatile_optional = ASTSon1(a);

    nodecl_t text_list = nodecl_null();

    if (volatile_optional != NULL)
    {
        text_list = nodecl_append_to_list(text_list, 
                nodecl_make_text("volatile", ast_get_locus(volatile_optional)));
    }

    text_list = nodecl_append_to_list(text_list, 
            nodecl_make_text(ASTText(string_literal), ast_get_locus(string_literal)));

    nodecl_t nodecl_gcc_asm = nodecl_make_asm_definition(
            text_list, ast_get_locus(a));

    *nodecl_output = nodecl_make_list_1(nodecl_gcc_asm);
}

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
                    nodecl_t nodecl_expr = nodecl_null();
                    if (expression != NULL
                            && !check_expression(expression, decl_context, &nodecl_expr))
                    {
                        check_expression(expression, decl_context, &nodecl_expr);
                    }

                    nodecl_t nodecl_identifier = nodecl_null();
                    if (identifier != NULL)
                    {
                        nodecl_identifier = nodecl_make_text(ASTText(identifier), ast_get_locus(identifier));
                    }

                    nodecl_t nodecl_constraint = nodecl_null();
                    if (constraint != NULL)
                    {
                        nodecl_constraint = nodecl_make_text(ASTText(constraint), ast_get_locus(constraint));
                    }

                    nodecl_t nodecl_asm_param = 
                        nodecl_make_gcc_asm_operand(
                                nodecl_identifier,
                                nodecl_constraint,
                                nodecl_expr,
                                ast_get_locus(asm_operand));

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

    AST specs = ASTSon0(a);

    nodecl_t nodecl_specs = nodecl_null();
    if (specs != NULL)
    {
        nodecl_specs = nodecl_make_list_1(
                nodecl_make_text(ASTText(specs), ast_get_locus(specs)));
    }

    nodecl_t nodecl_gcc_asm = 
        nodecl_make_gcc_asm_definition(
                nodecl_asm_params[0],
                nodecl_asm_params[1],
                nodecl_asm_params[2],
                nodecl_specs,
                ASTText(ASTSon0(asm_parms)),
                ast_get_locus(a));

    *nodecl_output = nodecl_make_list_1(nodecl_gcc_asm);
}

static void build_scope_explicit_instantiation(AST a,
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    // There are two forms of explicit instantiation: an explicit instantiation
    // definition and an explicit instantiation declaration. An explicit
    // instantiation declaration begins with the 'extern' keyword.
    char is_expl_inst_decl = 0;
    AST class_or_function_specifier = ASTSon0(a);
    if (class_or_function_specifier != NULL)
    {
        if (ASTType(class_or_function_specifier) == AST_EXTERN_SPEC)
        {
            is_expl_inst_decl = 1;
        }
        else
        {
            error_printf("%s: invalid specifier '%s' in an explicit instantiation\n",
                    ast_location(a), prettyprint_in_buffer(class_or_function_specifier));
        }
    }

    AST declarator = ASTSon2(a);
    type_t* simple_type_info = NULL;
    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));
    gather_info.is_template = 0;
    gather_info.is_explicit_instantiation = 1;
    gather_info.no_declarators = (declarator == NULL);

    AST decl_specifier_seq = ASTSon1(a);
    if (decl_specifier_seq != NULL)
    {
        build_scope_decl_specifier_seq(decl_specifier_seq,
                &gather_info,
                &simple_type_info,
                decl_context,
                nodecl_output);
    }

    decl_context_t current_decl_context = decl_context;
    if (simple_type_info == NULL
            && is_constructor_declarator(declarator))
    {
        current_decl_context.decl_flags |= DF_CONSTRUCTOR;
    }

    type_t* declarator_type = NULL;
    compute_declarator_type(declarator, &gather_info, simple_type_info,
            &declarator_type, current_decl_context, nodecl_output);
    // FIXME - We should instantiate here if no 'extern' is given

    nodecl_t declarator_name_opt = nodecl_null();
    scope_entry_t* entry = NULL;
    if (declarator != NULL)
    {
        entry = build_scope_declarator_name(declarator, declarator_type, &gather_info, current_decl_context);

        keep_gcc_attributes_in_symbol(entry, &gather_info);
        keep_ms_declspecs_in_symbol(entry, &gather_info);

        AST id_expr = get_declarator_id_expression(declarator, current_decl_context);
        compute_nodecl_name_from_id_expression(ASTSon0(id_expr), current_decl_context, &declarator_name_opt);
        // We do this to fix the declarator_name_opt
        query_nodecl_name_flags(current_decl_context, declarator_name_opt, NULL, DF_DEPENDENT_TYPENAME);

        if (entry == NULL
                || (entry->kind != SK_FUNCTION
                    && !(entry->kind == SK_VARIABLE
                        && entry->entity_specs.is_member
                        && entry->entity_specs.is_static)))
        {
            error_printf("%s: invalid explicit instantiation of '%s %s'\n", ast_location(a),
                    prettyprint_in_buffer(decl_specifier_seq),
                    prettyprint_in_buffer(declarator));
        }
    }
    else
    {
        if (is_named_type(declarator_type)
                && named_type_get_symbol(declarator_type)->kind == SK_CLASS)
        {
            entry = named_type_get_symbol(declarator_type);
        }
        else
        {
            error_printf("%s: error: declaration should declare a class\n", ast_location(a));
        }
    }

    if (entry != NULL)
    {
        // GCC crashes when the declarator is qualified and it's declared inside
        // of this qualified context
        // Example:
        //
        // namespace A
        // {
        //      template < typename T >
        //      void f(T)
        //      {
        //      }
        //
        //      template void A::f<int>(int); // GCC crashes
        //      template void ::A::f<int>(int); // GCC Crashes
        //
        // }
        // template void ::A::f<int>(int); // GCC ok
        //
        // For this reason, we need the original context and declarator

        nodecl_t nodecl_context =
            nodecl_make_context(/* optional statement sequence */ nodecl_null(),
                    decl_context, ast_get_locus(a));

        *nodecl_output = (is_expl_inst_decl) ?
            nodecl_make_list_1(
                    nodecl_make_cxx_extern_explicit_instantiation(
                        declarator_name_opt,
                        nodecl_context,
                        entry,
                        ast_get_locus(a)))
            :
            nodecl_make_list_1(
                    nodecl_make_cxx_explicit_instantiation(
                        declarator_name_opt,
                        nodecl_context,
                        entry,
                        ast_get_locus(a)));
    }
}

static void build_scope_using_directive(AST a, decl_context_t decl_context, nodecl_t* nodecl_output UNUSED_PARAMETER)
{
    // First get the involved namespace
    AST id_expression = ASTSon0(a);

    char turn_into_inline = 0;

    AST attr_list = ASTSon1(a);

    if (attr_list != NULL)
    {
        gather_decl_spec_t gather_info;
        memset(&gather_info, 0, sizeof(gather_info));

        gather_extra_attributes(attr_list, &gather_info, decl_context);

        if (gather_info.is_inline)
        {
            turn_into_inline = 1;
        }
    }

    scope_entry_list_t* result_list = query_id_expression(decl_context, 
            id_expression, NULL);

    if (result_list == NULL)
    {
        error_printf("%s: error: unknown namespace '%s'\n",
                ast_location(a), 
                prettyprint_in_buffer(id_expression));
        return;
    }

    if (entry_list_size(result_list) > 1
            || entry_list_head(result_list)->kind != SK_NAMESPACE)
    {
        error_printf("%s: error: '%s' does not name a namespace\n",
                ast_location(a), 
                prettyprint_in_buffer(id_expression));
        return;
    }

    scope_entry_t* entry = entry_list_head(result_list);

    entry_list_free(result_list);

    entry->entity_specs.is_inline = turn_into_inline;

    // Now add this namespace to the used namespaces of this scope
    scope_t* namespace_scope = decl_context.current_scope;

    ERROR_CONDITION(entry->related_decl_context.current_scope->kind != NAMESPACE_SCOPE,
            "Error, related scope is not namespace scope", 0);

    P_LIST_ADD_ONCE(namespace_scope->use_namespace,
            namespace_scope->num_used_namespaces,
            entry);

    nodecl_t cxx_using_namespace =
        nodecl_make_cxx_using_namespace(
                nodecl_make_context(
                    /* optional statement sequence */ nodecl_null(),
                    decl_context,
                    ast_get_locus(a)),
                entry,
                ast_get_locus(a));

    *nodecl_output =
        nodecl_make_list_1(cxx_using_namespace);
}

void introduce_using_entities_in_class(
        nodecl_t nodecl_name,
        scope_entry_list_t* used_entities,
        decl_context_t decl_context,
        scope_entry_t* current_class,
        access_specifier_t current_access,
        char is_typename,
        const locus_t* locus)
{
    scope_entry_list_t* existing_usings =
        query_in_scope_str(decl_context, entry_list_head(used_entities)->symbol_name, NULL);

    enum cxx_symbol_kind filter_usings[] = { SK_USING, SK_USING_TYPENAME };

    existing_usings = filter_symbol_kind_set(existing_usings, STATIC_ARRAY_LENGTH(filter_usings), filter_usings);

    scope_entry_list_t* already_using = NULL;

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(existing_usings);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* current_using = entry_list_iterator_current(it);

        already_using = entry_list_add_once(already_using, current_using->entity_specs.alias_to);
    }
    entry_list_iterator_free(it);

    entry_list_free(existing_usings);

    const char* symbol_name = NULL;
    // Now add all the used entities to the current scope
    for (it = entry_list_iterator_begin(used_entities);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        entry = entry_advance_aliases(entry);

        symbol_name = entry->symbol_name;

        char is_hidden = 0;

        if (entry->kind != SK_DEPENDENT_ENTITY)
        {
            if (!entry->entity_specs.is_member
                    || !class_type_is_base_instantiating(entry->entity_specs.class_type,
                        get_user_defined_type(current_class), locus))
            {
                error_printf("%s: error: '%s' is not a member of a base class\n",
                        locus_to_str(locus),
                        get_qualified_symbol_name(entry, 
                            decl_context));
                return;
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
                        && entry->kind == SK_FUNCTION
                        && function_type_same_parameter_types_and_cv_qualif(current_member->type_information, entry->type_information))
                {
                    // This one is being hidden by a member function of the current class
                    is_hidden = 1;
                    break;
                }
            }
            entry_list_iterator_free(it2);
            entry_list_free(member_functions);
        }
        else
        {
            // Dependent entity like _Base::f where _Base is a template parameter
            if (nodecl_is_null(nodecl_name))
            {
                internal_error("Invalid dependent name found", 0);
            }

            // The name of the symbol will be _Base but we do not want that one, we want f
            nodecl_t nodecl_last_part = nodecl_name_get_last_part(nodecl_name);
            symbol_name = nodecl_get_text(nodecl_last_part);
        }

        if (is_hidden)
            continue;

        // Do not add it twice in the scope
        if (entry_list_contains(already_using, entry))
            continue;

        scope_entry_t* used_name = new_symbol(decl_context, decl_context.current_scope, symbol_name);
        used_name->kind = !is_typename ? SK_USING : SK_USING_TYPENAME;
        used_name->locus = locus;
        used_name->entity_specs.alias_to = entry;

        used_name->entity_specs.is_member = 1;
        used_name->entity_specs.class_type = get_user_defined_type(current_class);
        used_name->entity_specs.access = current_access;

        insert_entry(decl_context.current_scope, used_name);
    }
    entry_list_iterator_free(it);

    entry_list_free(already_using);

    scope_entry_t* used_hub_symbol = counted_xcalloc(1, sizeof(*used_hub_symbol), &_bytes_used_buildscope);
    used_hub_symbol->kind = !is_typename ? SK_USING : SK_USING_TYPENAME;
    used_hub_symbol->type_information = get_unresolved_overloaded_type(used_entities, NULL);
    used_hub_symbol->entity_specs.access = current_access;
    used_hub_symbol->locus = locus;

    class_type_add_member(current_class->type_information,
            used_hub_symbol,
            /* is_definition */ 1);
}

// This is for using found in non class scope
static void introduce_using_entities(
        scope_entry_list_t* used_entities,
        decl_context_t decl_context,
        char is_typename,
        const locus_t* locus)
{
    scope_entry_list_t* existing_usings =
        query_in_scope_str(decl_context, entry_list_head(used_entities)->symbol_name, NULL);

    enum cxx_symbol_kind filter_usings[] = { SK_USING, SK_USING_TYPENAME };

    existing_usings = filter_symbol_kind_set(existing_usings, STATIC_ARRAY_LENGTH(filter_usings), filter_usings);

    scope_entry_list_t* already_using = NULL;

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(existing_usings);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* current_using = entry_list_iterator_current(it);

        already_using = entry_list_add_once(already_using, current_using->entity_specs.alias_to);
    }
    entry_list_iterator_free(it);

    entry_list_free(existing_usings);

    const char* symbol_name = NULL;
    // Now add all the used entities to the current scope
    for (it = entry_list_iterator_begin(used_entities);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);
        symbol_name = entry->symbol_name;

        scope_entry_t* original_entry = entry;
        if (original_entry->kind == SK_USING
                || original_entry->kind == SK_USING_TYPENAME)
        {
            // We want the ultimate alias
            original_entry = original_entry->entity_specs.alias_to;
        }

        // Do not add it twice in the scope
        if (entry_list_contains(already_using, original_entry))
            continue;

        scope_entry_t* used_name = new_symbol(decl_context, decl_context.current_scope, symbol_name);
        used_name->kind = !is_typename ? SK_USING : SK_USING_TYPENAME;
        used_name->locus = locus;
        used_name->entity_specs.alias_to = original_entry;

        insert_entry(decl_context.current_scope, used_name);
    }
    entry_list_iterator_free(it);

    entry_list_free(already_using);
}

static void introduce_using_entity_nodecl_name(nodecl_t nodecl_name,
        decl_context_t decl_context,
        access_specifier_t current_access,
        char is_typename,
        nodecl_t* nodecl_output)
{
    scope_entry_list_t* used_entities = query_nodecl_name_flags(decl_context,
            nodecl_name,
            NULL,
            // Do not examine uninstantiated templates
            DF_DEPENDENT_TYPENAME);

    if (used_entities == NULL)
    {
        error_printf("%s: error: entity '%s' in using-declaration is unknown",
                nodecl_locus_to_str(nodecl_name),
                codegen_to_str(nodecl_name, decl_context));
        return;
    }

    scope_entry_t* current_class = NULL;
    char is_class_scope = 0;
    if (decl_context.current_scope->kind == CLASS_SCOPE)
    {
        current_class = decl_context.current_scope->related_entry;
        is_class_scope = 1;
    }

    if (is_class_scope)
    {
        introduce_using_entities_in_class(
                nodecl_name, used_entities,
                decl_context,
                current_class,
                current_access,
                is_typename,
                nodecl_get_locus(nodecl_name));
    }
    else
    {
        introduce_using_entities(
                used_entities,
                decl_context,
                is_typename,
                nodecl_get_locus(nodecl_name));

        scope_entry_t* entry = entry_list_head(used_entities);
        *nodecl_output =
            nodecl_make_list_1(
                    nodecl_make_cxx_using_decl(
                        nodecl_make_context(
                            /* optional statement sequence */ nodecl_null(),
                            decl_context,
                            nodecl_get_locus(nodecl_name)),
                        entry,
                        nodecl_get_locus(nodecl_name)));
    }

    entry_list_free(used_entities);
}

static void build_scope_using_declaration(AST a, decl_context_t decl_context,
        access_specifier_t current_access, char is_typename, nodecl_t* nodecl_output)
{
    AST id_expression = ASTSon0(a);

    if (decl_context.current_scope->kind != CLASS_SCOPE
            && decl_context.current_scope->kind != NAMESPACE_SCOPE
            && decl_context.current_scope->kind != BLOCK_SCOPE)
    {
        error_printf("%s: error: using-declaration not in a class, namespace or block scope\n",
                ast_location(a));
        return;
    }

    nodecl_t nodecl_name = nodecl_null();
    compute_nodecl_name_from_id_expression(id_expression, decl_context, &nodecl_name);

    if (nodecl_is_err_expr(nodecl_name))
        return;

    introduce_using_entity_nodecl_name(nodecl_name, decl_context, current_access, is_typename, nodecl_output);
}

static void build_scope_member_declaration_qualified(AST a, decl_context_t decl_context,
        access_specifier_t current_access,
        nodecl_t* nodecl_output)
{
    AST id_expression = ASTSon0(a);

    nodecl_t nodecl_name = nodecl_null();
    compute_nodecl_name_from_id_expression(id_expression, decl_context, &nodecl_name);

    if (nodecl_is_err_expr(nodecl_name))
        return;

    introduce_using_entity_nodecl_name(nodecl_name, decl_context, current_access, /* is_typename */ 0, nodecl_output);
}

static void build_scope_static_assert(AST a, decl_context_t decl_context)
{
    AST constant_expr = ASTSon0(a);
    AST message = ASTSon1(a);

    nodecl_t nodecl_expr = nodecl_null();
    if (!check_expression(constant_expr, decl_context, &nodecl_expr))
    {
        error_printf("%s: error: static_assert expression is invalid\n",
                ast_location(a));
    }

    if (!nodecl_expr_is_value_dependent(nodecl_expr))
    {
        if (!nodecl_is_constant(nodecl_expr))
        {
            error_printf("%s: error: static_assert expression is not constant\n",
                    ast_location(a));
        }
        else
        {
            const_value_t * val = nodecl_get_constant(nodecl_expr);

            if (const_value_is_zero(val))
            {
                error_printf("%s: error: static_assert failed: %s\n",
                        ast_location(a),
                        prettyprint_in_buffer(message));
            }
        }
    }

    // FIXME - static_assert is not properly implemented for classes where they
    // should be signed in as if they were members
}

static void build_scope_common_template_alias_declaration(AST a,
        decl_context_t decl_context,
        nodecl_t* nodecl_output,
        char is_member_declaration,
        type_t* class_info,
        access_specifier_t access_specifier,
        char is_explicit_specialization)
{
    ERROR_CONDITION(decl_context.template_parameters == NULL,
            "There must be template parameters", 0);

    if (IS_CXX03_LANGUAGE)
    {
        warn_printf("%s: warning: template-alias are only valid in C++11\n",
                ast_location(a));
    }

    if (is_explicit_specialization)
    {
        error_printf("%s: error: invalid alias-declaration in explicit template specialization\n",
                ast_location(a));
        return;
    }

    AST identifier = ASTSon0(a);

    AST type_id = ASTSon1(a);
    type_t* aliased_type = compute_type_for_type_id_tree(type_id, decl_context, NULL, NULL);

    if (is_error_type(aliased_type))
        return;

    scope_entry_t* entry = NULL;
    scope_entry_list_t* entry_list = query_in_scope_str_flags(
            decl_context, ASTText(identifier), NULL, DF_ONLY_CURRENT_SCOPE);

    if (entry_list != NULL)
    {
        entry = entry_list_head(entry_list);

        if (entry->kind != SK_TEMPLATE)
        {
            error_printf("%s: error: symbol '%s' has been redeclared as a different symbol kind\n",
                    ast_location(identifier),
                    ASTText(identifier));
        }
        else
        {
            error_printf("%s: error: alias template '%s' has already been defined\n",
                    ast_location(identifier),
                    ASTText(identifier));
        }

        info_printf("%s: info: previous declaration of '%s'\n",
                locus_to_str(entry->locus),
                entry->symbol_name);
        return;
    }
    else
    {
        entry = new_symbol(decl_context, decl_context.current_scope, ASTText(identifier));
        entry->kind = SK_TEMPLATE;
        entry->type_information = get_new_template_alias_type(
                decl_context.template_parameters,
                aliased_type,
                ASTText(identifier),
                decl_context,
                ast_get_locus(identifier));

        entry->entity_specs.is_user_declared = 1;
        entry->defined = 1;

        template_type_set_related_symbol(entry->type_information, entry);
    }

    if (!is_member_declaration)
    {
        nodecl_t nodecl_context =
            nodecl_make_context(/* optional statement sequence */ nodecl_null(),
                    decl_context, ast_get_locus(a));

        *nodecl_output = nodecl_concat_lists(
                *nodecl_output,
                nodecl_make_list_1(
                    nodecl_make_cxx_def(
                        nodecl_context,
                        named_type_get_symbol(template_type_get_primary_type(entry->type_information)),
                        ast_get_locus(a))));
    }
    else
    {
        entry->entity_specs.is_member = 1;
        entry->entity_specs.class_type = class_info;
        entry->entity_specs.access = access_specifier;

        scope_entry_t* primary_symbol = named_type_get_symbol(template_type_get_primary_type(entry->type_information));
        primary_symbol->entity_specs.is_member = 1;
        primary_symbol->entity_specs.class_type = class_info;
        primary_symbol->entity_specs.access = access_specifier;

        class_type_add_member(class_info, primary_symbol, /* is_definition */ 1);
    }
}

static void build_scope_template_alias_declaration(AST a, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    build_scope_common_template_alias_declaration(a, decl_context, nodecl_output,
            /* is_member_declaration */ 0, NULL, AS_UNKNOWN, 0);
}

static void build_scope_nontemplate_alias_declaration(AST a, decl_context_t decl_context, nodecl_t* nodecl_output,
        char is_member_declaration, type_t* class_info, access_specifier_t access_specifier)
{
    AST identifier = ASTSon0(a);

    AST type_id = ASTSon1(a);
    type_t* aliased_type = compute_type_for_type_id_tree(type_id, decl_context, NULL, NULL);

    if (is_error_type(aliased_type))
        return;

    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));

    scope_entry_t* entry = register_new_typedef_name(identifier, aliased_type, &gather_info, decl_context);

    if (entry == NULL)
        return;

    if (!is_member_declaration)
    {
        nodecl_t (*make_cxx_decl_or_def)(nodecl_t, scope_entry_t*, const locus_t*) =
            (entry->defined) ? nodecl_make_cxx_def : nodecl_make_cxx_decl;

        nodecl_t nodecl_context =
            nodecl_make_context(/* optional statement sequence */ nodecl_null(),
                    decl_context, ast_get_locus(a));

        *nodecl_output = nodecl_concat_lists(
                *nodecl_output,
                nodecl_make_list_1(
                    make_cxx_decl_or_def(
                        nodecl_context,
                        entry,
                        ast_get_locus(a))));
    }
    else
    {
        entry->entity_specs.is_member = 1;
        entry->entity_specs.class_type = class_info;
        entry->entity_specs.access = access_specifier;
        class_type_add_member(class_info, entry, /* is_definition */ 1);
    }
}

static void build_scope_simple_alias_declaration(AST a, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    build_scope_nontemplate_alias_declaration(a, decl_context, nodecl_output,
            /* is_member_declaration */ 0, NULL, AS_UNKNOWN);
}

static void build_scope_member_alias_declaration(AST a, decl_context_t decl_context, nodecl_t* nodecl_output,
        type_t* class_info, access_specifier_t access_specifier)
{
    build_scope_nontemplate_alias_declaration(a, decl_context, nodecl_output,
            /* is_member_declaration */ 1, class_info, access_specifier);
}

static void gather_cxx11_attributes(AST a, gather_decl_spec_t* gather_info UNUSED_PARAMETER)
{
    if (a != NULL)
    {
        internal_error("C++11 attributes not yet implemented\n", 0);
    }
}

static void copy_gather_info(gather_decl_spec_t* dest, gather_decl_spec_t* src)
{
    *dest = *src;

#define COPY_ARRAY(dest, orig, num_items) \
    (dest) = xcalloc((num_items), sizeof(*(dest))); \
    memcpy((dest), (orig), sizeof(*(dest))*(num_items));

    COPY_ARRAY(dest->arguments_info, src->arguments_info, dest->num_arguments_info);
    COPY_ARRAY(dest->gcc_attributes, src->gcc_attributes, dest->num_gcc_attributes);
    COPY_ARRAY(dest->ms_attributes, src->ms_attributes, dest->num_ms_attributes);

#undef COPY_ARRAY
}

static nodecl_t flush_extra_declared_symbols(const locus_t* loc)
{
    nodecl_t result = nodecl_null();

    scope_entry_t* extra_decl_symbol = pop_extra_declaration_symbol();
    while (extra_decl_symbol != NULL)
    {
        if (extra_decl_symbol->entity_specs.is_saved_expression)
        {
            result = nodecl_append_to_list(
                    result,
                    nodecl_make_object_init(
                        extra_decl_symbol,
                        loc));
        }
        else if (IS_CXX_LANGUAGE
                && (is_class_type(extra_decl_symbol->type_information)
                    || is_enum_type(extra_decl_symbol->type_information)))
        {
            // This happens in this case (C can handle this automatically but not C++)
            //
            // (union { int x; int y; }){1,2}
            //
            result = nodecl_append_to_list(
                    result,
                    nodecl_make_cxx_def(
                        /* optional scope */ nodecl_null(),
                        extra_decl_symbol,
                        loc));
        }
        else
        {
            internal_error("Unhandled extra declared symbol '%s'", extra_decl_symbol->symbol_name);
        }

        extra_decl_symbol = pop_extra_declaration_symbol();
    }

    return result;
}

// Builds scope for a simple declaration
static void build_scope_simple_declaration(AST a, decl_context_t decl_context,
        char is_template, char is_explicit_specialization,
        nodecl_t *nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list)
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
    gather_info.is_explicit_specialization = is_explicit_specialization;

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
    //
    AST decl_specifier_seq = ASTSon0(a);
    AST declarator_list = ASTSon1(a);

    if (decl_specifier_seq != NULL)
    {
        // This declaration can define a type if it is a class specifier or enum specifier
        // and just declare it if the declaration contains an elaborated-type-spec
        gather_info.no_declarators = (declarator_list == NULL);

        nodecl_t nodecl_decl_specifier = nodecl_null();

        build_scope_decl_specifier_seq(decl_specifier_seq, &gather_info, &simple_type_info,
                decl_context, &nodecl_decl_specifier);

        *nodecl_output = nodecl_concat_lists(*nodecl_output, nodecl_decl_specifier);
    }
    else
    {
        C_LANGUAGE()
        {
            warn_printf("%s: warning: declaration does not have decl-specifier, assuming 'int'\n",
                    ast_location(a));

            simple_type_info = get_signed_int_type();
        }
    }

    if (gather_info.is_friend)
    {
        error_printf("%s: error: friend specifier is not allowed here\n", ast_location(a));
        gather_info.is_friend = 0;
    }

    // If the type specifier defined a type, add it to the declared symbols
    if (gather_info.defined_type != NULL
            && declared_symbols != NULL)
    {
        *declared_symbols = entry_list_add(*declared_symbols, gather_info.defined_type);
        P_LIST_ADD(gather_decl_spec_list->items, gather_decl_spec_list->num_items, gather_info);
    }


    // There are declarators ahead
    if (declarator_list != NULL)
    {
        // For every declarator create its full type based on the type
        // specified in the decl_specifier_seq
        AST iter;
        for_each_element(declarator_list, iter)
        {
            // Copy because of attributes that might modify this info
            gather_decl_spec_t current_gather_info;
            copy_gather_info(&current_gather_info, &gather_info);

            AST init_declarator = ASTSon1(iter);

            if (ASTType(init_declarator) == AST_AMBIGUITY)
            {
                solve_ambiguous_init_declarator(init_declarator, decl_context);
            }

            decl_context_t current_decl_context = decl_context;

            CXX_LANGUAGE()
            {
                if (simple_type_info == NULL
                        && is_constructor_declarator(init_declarator))
                {
                    current_decl_context.decl_flags |= DF_CONSTRUCTOR;
                }
            }

            ERROR_CONDITION(ASTType(init_declarator) != AST_INIT_DECLARATOR,
                    "Invalid node", 0);

            AST asm_specification_or_gcc_attributes = ASTSon2(init_declarator);
            if (asm_specification_or_gcc_attributes)
            {
                gather_extra_attributes(asm_specification_or_gcc_attributes,
                        &current_gather_info, current_decl_context);
            }

            AST declarator = ASTSon0(init_declarator);
            AST initializer = ASTSon1(init_declarator);

            type_t* declarator_type = NULL;

            // This will create the symbol if it is unqualified
            nodecl_t nodecl_declarator = nodecl_null();
            compute_declarator_type(declarator, &current_gather_info,
                    simple_type_info, &declarator_type, current_decl_context, &nodecl_declarator);

            scope_entry_t *entry = build_scope_declarator_name(declarator, declarator_type,
                    &current_gather_info, current_decl_context);

            // Something is wrong
            if (entry == NULL)
                continue;

            if (entry->entity_specs.is_constructor)
            {
                error_printf("%s: error: declaration of a constructor not valid in this scope\n",
                        ast_location(a));
                continue;
            }

            if (entry->entity_specs.is_conversion)
            {
                error_printf("%s: error: declaration of a conversion function not valid in this scope\n",
                        ast_location(a));
                continue;
            }

            if (entry->entity_specs.is_destructor)
            {
                error_printf("%s: error: declaration of a destructor not valid in this scope\n",
                        ast_location(a));
                continue;
            }

            // Note that in C99 we craft a signed int here internally, so this
            // can only happen in C++
            if (simple_type_info == NULL)
            {
                error_printf("%s: error: declaration of '%s' lacks a type-specifier\n",
                        ast_location(a),
                        get_qualified_symbol_name(entry, current_decl_context));
                continue;
            }

            if (entry->entity_specs.is_member)
            {
                if (entry->kind != SK_VARIABLE
                        || !entry->entity_specs.is_static)
                {
                    error_printf("%s: error: declaration of member '%s' not valid in this scope\n",
                            ast_location(a),
                            get_qualified_symbol_name(entry, current_decl_context));
                    continue;
                }
            }

            if (current_gather_info.is_transparent_union)
            {
                set_is_transparent_union(entry->type_information, /* is_transparent_union */ 1);
            }

            // Copy gcc attributes
            keep_gcc_attributes_in_symbol(entry, &current_gather_info);
            keep_ms_declspecs_in_symbol(entry, &current_gather_info);

            // Propagate the __extension__ attribute to the symbol
            entry->entity_specs.gcc_extension = gcc_extension;

            // Only variables can be initialized
            if (initializer != NULL)
            {
                if (current_gather_info.is_extern)
                {
                    error_printf("%s: error: cannot initialize an 'extern' declaration\n", ast_location(a));
                }

                if (entry->kind == SK_TYPEDEF)
                {
                    error_printf("%s: error: cannot initialize an typedef\n", ast_location(a));
                }
            }

            if (initializer == NULL
                    && current_gather_info.is_auto_type)
            {
                error_printf("%s: error: declaration with auto type-specifier requires an initializer\n",
                        ast_location(a));
            }

            if (entry->kind == SK_FUNCTION)
            {
                if (current_decl_context.current_scope->kind == BLOCK_SCOPE
                        && !entry->entity_specs.is_nested_function)
                {
                    // Ensure that the symbol is marked as extern
                    entry->entity_specs.is_extern = 1;
                }

                CXX11_LANGUAGE()
                {
                    if ((function_type_get_ref_qualifier(entry->type_information) != REF_QUALIFIER_NONE)
                            && (!entry->entity_specs.is_member
                                || entry->entity_specs.is_static))
                    {
                        // No member function can reach here, so this is wrong
                        error_printf("%s: error: only nonstatic member functions may have ref-qualifier\n",
                                ast_location(a));
                    }
                }
            }

            if (entry->kind == SK_VARIABLE
                    || entry->kind == SK_TYPEDEF)
            {
                if (current_decl_context.current_scope->kind == BLOCK_SCOPE)
                {
                    int i;
                    for (i = 0; i < current_gather_info.num_vla_dimension_symbols; i++)
                    {
                        scope_entry_t* vla_dim = current_gather_info.vla_dimension_symbols[i];

                        *nodecl_output = nodecl_concat_lists(
                                *nodecl_output,
                                nodecl_make_list_1(nodecl_make_object_init(
                                        vla_dim, 
                                        ast_get_locus(init_declarator)))); 
                    }

                    current_gather_info.num_vla_dimension_symbols = 0;
                    xfree(current_gather_info.vla_dimension_symbols);
                    current_gather_info.vla_dimension_symbols = NULL;

                }
            }

            if (entry->kind == SK_VARIABLE)
            {
                if (entry->defined
                        && !BITMAP_TEST(current_decl_context.decl_flags, DF_ALLOW_REDEFINITION)
                        && !current_gather_info.is_extern
                        // In C, an entity may be redefined at file-scope
                        && !(IS_C_LANGUAGE
                            && (entry->decl_context.current_scope == entry->decl_context.global_scope)
                            && nodecl_is_null(entry->value)))
                {
                    error_printf("%s: error: redefined entity '%s', first declared in '%s'\n",
                            ast_location(declarator),
                            get_qualified_symbol_name(entry, current_decl_context),
                            locus_to_str(entry->locus));
                }
                else
                {
                    // Update the location
                    entry->locus = ast_get_locus(declarator);
                }

                if (is_array_type(declarator_type)
                        && is_array_type(entry->type_information)
                        && !nodecl_is_null(array_type_get_array_size_expr(declarator_type))
                        && nodecl_is_null(array_type_get_array_size_expr(entry->type_information)))
                {
                    // extern int c[];
                    // int c[10];       <-- We are in this declaration
                    // Update the array type
                    entry->type_information = declarator_type;
                }

                // Weird case where a non global but extern entity may get the dimension from the global scope...
                // int c[10];
                // int f(void)
                // {
                //   extern int c[]; // Must behave as 'extern int c[10]'
                //   return sizeof(c); // OK == sizeof(int[10])
                // }
                if (entry->entity_specs.is_extern
                        && entry->decl_context.current_scope != entry->decl_context.global_scope
                        && is_array_type(entry->type_information)
                        && nodecl_is_null(array_type_get_array_size_expr(entry->type_information)))
                {
                    // Perform a query in the global scope
                    scope_entry_list_t* extern_scope_entry_list = query_in_scope_str(
                            CURRENT_COMPILED_FILE->global_decl_context,
                            entry->symbol_name,
                            NULL);

                    if (extern_scope_entry_list != NULL)
                    {
                        scope_entry_t* extern_entry = entry_list_head(extern_scope_entry_list);
                        if (extern_entry->kind != entry->kind)
                        {
                            error_printf("%s: error: extern entity redeclared as a different entity kind\n",
                                   ast_location(declarator));
                        }
                        else if (is_array_type(extern_entry->type_information)
                                && !nodecl_is_null(array_type_get_array_size_expr(extern_entry->type_information)))
                        {
                            // Update the array dimension here
                            entry->type_information = 
                                get_array_type(array_type_get_element_type(entry->type_information),
                                        array_type_get_array_size_expr(extern_entry->type_information),
                                        array_type_get_array_size_expr_context(extern_entry->type_information));
                        }
                    }
                    entry_list_free(extern_scope_entry_list);
                }

                nodecl_t nodecl_initializer = nodecl_null();
                if (initializer != NULL)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "BUILDSCOPE: Initializer: '%s'\n", ast_print_node_type(ASTType(initializer)));
                    }

                    char init_check = check_initialization(initializer,
                            entry->decl_context,
                            entry,
                            get_unqualified_type(declarator_type),
                            &nodecl_initializer,
                            current_gather_info.is_auto_type);

                    // Update unbounded arrays, bounded by their initialization
                    if (init_check)
                    {
                        type_t* initializer_type = nodecl_get_type(nodecl_initializer);

                        if (is_array_type(declarator_type)
                                && nodecl_is_null(array_type_get_array_size_expr(declarator_type))
                                && is_array_type(no_ref(initializer_type))
                                && !nodecl_is_null(array_type_get_array_size_expr(no_ref(initializer_type))))
                        {
                            cv_qualifier_t cv_qualif = get_cv_qualifier(entry->type_information);
                            entry->type_information = get_cv_qualified_type(no_ref(initializer_type), cv_qualif);
                        }
                    }

                    entry->value = nodecl_initializer;
                }
                // If it does not have initializer and it is not an extern entity
                // check a zero args constructor
                else if (!current_gather_info.is_extern)
                {
                    CXX_LANGUAGE()
                    {
                        if (!is_dependent_type(declarator_type))
                        {
                            check_default_initialization_and_destruction_declarator(entry, current_decl_context, 
                                    ast_get_locus(declarator));
                        }
                    }
                }

                if (!current_gather_info.is_extern
                        || current_gather_info.emit_always)
                {
                    if (!entry->entity_specs.is_member // Not a member
                            || ( // Static member definition (outside of the class)
                                entry->entity_specs.is_static  
                                && current_decl_context.current_scope->kind == NAMESPACE_SCOPE))
                    {
                        // Define the symbol
                        entry->defined = 1;
                    }
                }

                *nodecl_output = nodecl_concat_lists(
                        *nodecl_output,
                        flush_extra_declared_symbols(ast_get_locus(declarator)));

                // We create object-init nodecls for several cases:
                // - if the initializer is not null
                if (!nodecl_is_null(nodecl_initializer)
                        // (some cases only for C++)
                        // (some cases only for C99)
                        || (IS_C_LANGUAGE
                            // - variably modified types in block scopes must be declared here
                            && current_decl_context.current_scope->kind == BLOCK_SCOPE
                            && is_variably_modified_type(entry->type_information))
                        || (IS_CXX_LANGUAGE
                            // - class variables in block scope
                            && current_decl_context.current_scope->kind == BLOCK_SCOPE
                            && (is_class_type(entry->type_information)
                                // - array of class type variables
                                || (is_array_type(entry->type_information) 
                                    && is_class_type(array_type_get_element_type(entry->type_information)))))
                        || (current_decl_context.current_scope->kind == NAMESPACE_SCOPE
                            // - namespace-scope declarations of non-member
                            // entities that are non-extern since they are
                            // definitions (global definitions are here)
                            && ((!entry->entity_specs.is_member 
                                    && !entry->entity_specs.is_extern
                                    && !current_gather_info.is_extern)
                                // - static member definitions (at
                                // namespace-scope these are definitions too)
                                || (entry->entity_specs.is_member 
                                    && entry->entity_specs.is_static)))
                        // - __attribute__((used)) 
                        // (maybe we should elaborate this one a bit more)
                        || current_gather_info.emit_always)
                        {
                            *nodecl_output = nodecl_concat_lists(
                                    *nodecl_output,
                                    nodecl_make_list_1(nodecl_make_object_init(
                                            entry, 
                                            ast_get_locus(init_declarator)))); 
                        }
            }


            // For typedefs we will emit a nodecl_cxx_decl if they involve
            // variably modified types.
            //
            // External global variables are also represented with nodecl_cxx_decl
            //
            // (We could use an object init but we reserve those for real data
            // entities)
            if ((entry->kind == SK_TYPEDEF
                    && is_variably_modified_type(entry->type_information))
                    || (IS_C_LANGUAGE
                        && entry->kind == SK_VARIABLE
                        && entry->entity_specs.is_extern
                        && entry->decl_context.current_scope == entry->decl_context.global_scope))
            {
                *nodecl_output = nodecl_concat_lists(
                        *nodecl_output,
                        nodecl_make_list_1(
                            nodecl_make_cxx_decl(
                                nodecl_make_context(nodecl_null(),
                                    current_decl_context,
                                    ast_get_locus(init_declarator)),
                                entry,
                                ast_get_locus(init_declarator))));
            }

            // GCC weird stuff
            if (current_gather_info.gcc_asm_spec != NULL)
            {
                nodecl_t asm_spec = nodecl_make_gcc_asm_spec(
                        ASTText(ASTSon0(current_gather_info.gcc_asm_spec)), 
                        ast_get_locus(current_gather_info.gcc_asm_spec));
                entry->entity_specs.asm_specification = asm_spec;
            }

            if (declared_symbols != NULL)
            {
                *declared_symbols = entry_list_add(*declared_symbols, entry);
                P_LIST_ADD(gather_decl_spec_list->items, gather_decl_spec_list->num_items, current_gather_info);
            }

            CXX_LANGUAGE()
            {
                nodecl_t (*make_cxx_decl_or_def)(nodecl_t, scope_entry_t*, const locus_t*) =
                    (entry->defined) ? nodecl_make_cxx_def : nodecl_make_cxx_decl;

                nodecl_t nodecl_context =
                    nodecl_make_context(/* optional statement sequence */ nodecl_null(),
                            current_decl_context, ast_get_locus(init_declarator));

                *nodecl_output = nodecl_concat_lists(
                        *nodecl_output,
                        nodecl_make_list_1(
                            make_cxx_decl_or_def(
                                nodecl_context,
                                entry,
                                ast_get_locus(init_declarator))));
            }
        }
    }
    else if (simple_type_info != NULL
            && declarator_list == NULL)
    {
        // Anonymous union special treatment
        if (is_named_type(simple_type_info)
                && is_class_type(simple_type_info)
                && named_type_get_symbol(simple_type_info)->entity_specs.is_anonymous_union)
        {
            scope_entry_t* named_type = named_type_get_symbol(simple_type_info);
            finish_anonymous_class(named_type, decl_context);
        }
    }
    else
    {
        internal_error("Code unreachable", 0);
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

    // Gather decl specifier sequence information previous to type_spec
    list = ASTSon0(a);
    if (list != NULL)
    {
        for_each_element(list, iter)
        {
            AST spec = ASTSon1(iter);
            // GCC attributes (previous to type_spec) must be ignored at this point
            // Reason: these attributes refer to declarator_list
            if (ASTType(spec) != AST_GCC_ATTRIBUTE)
            {
                gather_decl_spec_information(spec, gather_info, decl_context);
            }
        }
    }

    AST type_spec = ASTSon1(a);

    AST attributes_of_class_type_specifier = NULL;

    // Gather decl specifier sequence information after type_spec
    list = ASTSon2(a);
    if (list != NULL)
    {
        char only_seen_attributes = 1;
        for_each_element(list, iter)
        {
            AST spec = ASTSon1(iter);

            if (only_seen_attributes
                    && ASTType(spec) == AST_GCC_ATTRIBUTE
                    && type_spec != NULL
                    && ASTType(type_spec) == AST_CLASS_SPECIFIER)
            {
                // This attribute must be applied to the type not to
                // the declaration
                //
                // Example
                //
                // struct A { int x; } __attribute__((packed)) a;
                //
                // This 'packed' goes to 'struct A' not to 'a'
                //
                // Note that
                //
                // struct A { int x; } const __attribute__((packed)) a;
                //
                // The attribut does not go to struct A because it does not
                // follow the closing bracket of the class specifier
                //
                // We keep these attribute in a special list that we handle later
                // once we have processed the type specifier
                attributes_of_class_type_specifier = iter;
            }
            else
            {
                gather_decl_spec_information(spec, gather_info, decl_context);
                only_seen_attributes = 0;
            }
        }
    }

    // Now gather information of the type_spec
    if (type_spec != NULL
            || gather_info->is_unsigned
            || gather_info->is_signed
            || gather_info->is_short
            || gather_info->is_long
            || gather_info->is_complex
            // Mercurium extension
            || gather_info->is_boolean_integer
            || gather_info->is_mask_integer)
    {

        if (type_spec == NULL)
        {
            if( gather_info->is_unsigned
                    || gather_info->is_signed
                    || gather_info->is_short
                    || gather_info->is_long
                    // Mercurium extension
                    || gather_info->is_boolean_integer
                    || gather_info->is_mask_integer)
            {
                // Manually add the int tree to make things easier
                ast_set_child(a, 1, ASTLeaf(AST_IMPLICIT_INT_TYPE, ast_get_locus(a), NULL));
                type_spec = ASTSon1(a);
            }
            // This is a GCC extension
            else if (gather_info->is_complex)
            {
                ast_set_child(a, 1, ASTLeaf(AST_DOUBLE_TYPE, ast_get_locus(a), NULL));
                type_spec = ASTSon1(a);
            }
            else
            {
                internal_error("Code unreachable", 0);
            }
        }

        // We need a local copy of the gather info because some information
        // cannot be shared between the type and the declarators
        gather_decl_spec_t local_gather_info;
        copy_gather_info(&local_gather_info, gather_info);

        // Now apply the trailing attributes (if any) of this class specifier
        // to the class specifier itself
        list = attributes_of_class_type_specifier;
        if (list != NULL)
        {
            for_each_element(list, iter)
            {
                AST spec = ASTSon1(iter);

                gather_decl_spec_information(spec, &local_gather_info, decl_context);
            }
        }
        gather_type_spec_information(type_spec, type_info, &local_gather_info, decl_context, nodecl_output);


        // Now, we are safe to gather the information of GCC attributes,
        // skipped in the first loop of this function
        list = ASTSon0(a);
        if (list != NULL)
        {
            for_each_element(list, iter)
            {
                AST spec = ASTSon1(iter);
                if (ASTType(spec) == AST_GCC_ATTRIBUTE)
                {
                    gather_decl_spec_information(spec, gather_info, decl_context);
                }
            }
        }

        // Copy bits of local_gather_info that are needed in gather_info
        gather_info->defined_type = local_gather_info.defined_type;
        gather_info->is_short = local_gather_info.is_short;
        gather_info->is_long = local_gather_info.is_long;
        gather_info->is_unsigned = local_gather_info.is_unsigned;
        gather_info->is_signed = local_gather_info.is_signed;
        gather_info->is_auto_type = local_gather_info.is_auto_type;

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
            if (*type_info == get_signed_byte_type())
            {
                *type_info = get_unsigned_byte_type();
            }
            else if (*type_info == get_char_type())
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
#ifdef HAVE_INT128
            else if (*type_info == get_signed_int128_type())
            {
                *type_info = get_unsigned_int128_type();
            }
#endif
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

        // Mercurium extension
        if (gather_info->is_boolean_integer)
        {
            if (!is_integer_type(*type_info))
            {
                error_printf("%s: error: a boolean type requires an integer type\n", ast_location(a));
                *type_info = get_error_type();
                return;
            }
            *type_info = get_bool_of_integer_type(*type_info);
        }

        if (gather_info->is_mask_integer)
        {
            if (!is_integer_type(*type_info))
            {
                error_printf("%s: error: a mask type requires an integer type\n", ast_location(a));
                *type_info = get_error_type();
                return;
            }
            *type_info = get_mask_type(/* mask_size_bits */ 8 * type_get_size(*type_info));
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
            warn_printf("%s: warning: declaration does not have a type-specifier, assuming 'int'\n",
                    ast_location(a));

            // Manually add the int tree to make things easier
            ast_set_child(a, 1, ASTLeaf(AST_IMPLICIT_INT_TYPE, ast_get_locus(a), NULL));
            type_spec = ASTSon1(a);

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
        case AST_AUTO_STORAGE_SPEC :
            gather_info->is_auto_storage = 1;
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
        case AST_THREAD_LOCAL_SPEC :
            gather_info->is_thread_local = 1;
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
        case AST_CONSTEXPR_SPEC:
            gather_info->is_constexpr = 1;
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
            // Mercurium extensions
        case AST_MCC_BOOL:
            gather_info->is_boolean_integer = 1;
            break;
        case AST_MCC_MASK:
            gather_info->is_mask_integer = 1;
            // Currently masks are always unsigned types
            gather_info->is_unsigned = 1;
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
                            // We should be doing something useful with this
                            nodecl_t nodecl_dummy = nodecl_null();
                            check_expression(layout_qualif_kind, decl_context, &nodecl_dummy);
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
            // OPENCL stuff
        case AST_OPENCL_KERNEL:
            gather_info->opencl.is_kernel = 1;
            break;
        case AST_OPENCL_GLOBAL:
            gather_info->opencl.is_global = 1;
            break;
        case AST_OPENCL_LOCAL:
            gather_info->opencl.is_local = 1;
            break;
        case AST_OPENCL_CONSTANT:
            gather_info->opencl.is_constant = 1;
            break;
        case AST_XL_BUILTIN_SPEC :
            // Do nothing at the moment
            break;
            // Unknown node
        case AST_MS_DECLSPEC:
            // __declspec(X)
            // __declspec(X(Y0, Y1, ...))
            gather_ms_declspec(a, gather_info, decl_context);
            break;
        default:
            internal_error("Unknown node '%s' (%s)", ast_print_node_type(ASTType(a)), ast_location(a));
            break;
    }
}


/*
 * This function fills simple_type_info with type information.
 */
void gather_type_spec_information(AST a, type_t** simple_type_info,
        gather_decl_spec_t* gather_info,
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    switch (ASTType(a))
    {
        case AST_SIMPLE_TYPE_SPEC :
            gather_type_spec_from_simple_type_specifier(a, simple_type_info, gather_info, decl_context);
            break;
        case AST_ELABORATED_TYPENAME_SPEC :
            gather_type_spec_from_dependent_typename(a, simple_type_info, gather_info, decl_context);
            break;
        case AST_ENUM_SPECIFIER :
            gather_type_spec_from_enum_specifier(a, simple_type_info, gather_info, decl_context, nodecl_output);
            break;
        case AST_CLASS_SPECIFIER :
            gather_type_spec_from_class_specifier(a, simple_type_info, gather_info, decl_context, nodecl_output);
            break;
        case AST_ELABORATED_TYPE_ENUM_SPEC :
            gather_type_spec_from_elaborated_enum_specifier(a, simple_type_info, gather_info, decl_context, nodecl_output);
            break;
        case AST_ELABORATED_TYPE_CLASS_SPEC :
            gather_type_spec_from_elaborated_class_specifier(a, simple_type_info, gather_info, decl_context, nodecl_output);
            break;
        case AST_CHAR_TYPE :
            // It can be either signed or unsigned, so do not assume it is
            // signed like we do for int
            *simple_type_info = get_char_type();
            break;
        case AST_WCHAR_TYPE :
            *simple_type_info = get_wchar_t_type();
            break;
        case AST_CHAR16_T:
            *simple_type_info = get_char16_t_type();
            break;
        case AST_CHAR32_T:
            *simple_type_info = get_char32_t_type();
            break;
        case AST_BOOL_TYPE :
            *simple_type_info = get_bool_type();
            break;
        case AST_SHORT_TYPE :
            *simple_type_info = get_signed_int_type();
            gather_info->is_short = 1;
            break;
        case AST_INT_TYPE :
        case AST_IMPLICIT_INT_TYPE :
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
        case AST_AUTO_TYPE:
            *simple_type_info = get_auto_type();
            gather_info->is_auto_type = 1;
            break;
        case AST_GCC_COMPLEX_TYPE :
            *simple_type_info = get_signed_int_type();
            gather_info->is_complex = 1;
            break;
            // C++0x
        case AST_DECLTYPE :
            {
                // Advance just before parentheses
                // (a normal call to 'advance_expression_nest' would advance after them)
                AST expression = advance_expression_nest_flags(ASTSon0(a), /* advance_parentheses */ 0);

                // Compute the expression type and use it for the whole type
                nodecl_t nodecl_expr = nodecl_null();
                if (check_expression_non_executable(expression, decl_context, &nodecl_expr)
                        && (nodecl_get_type(nodecl_expr) != NULL))
                {
                    // Do not remove the reference here, we will do this later
                    // if mandated
                    type_t* computed_type = nodecl_get_type(nodecl_expr);

                    if (is_unresolved_overloaded_type(computed_type))
                    {
                        scope_entry_list_t* entry_list =
                            unresolved_overloaded_type_get_overload_set(computed_type);

                        if (entry_list_size(entry_list) > 1)
                        {
                            error_printf("%s: error: '%s' yields an unresolved overload type\n",
                                    ast_location(a), 
                                    prettyprint_in_buffer(a));
                            *simple_type_info = get_error_type();
                            return;
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
                                    entry->entity_specs.class_type);
                        }
                    }

                    char is_removed_reference = 0;
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
                            // class member accesses
                        case AST_CLASS_MEMBER_ACCESS :
                        case AST_CLASS_TEMPLATE_MEMBER_ACCESS :
                        case AST_POINTER_CLASS_MEMBER_ACCESS :
                        case AST_POINTER_CLASS_TEMPLATE_MEMBER_ACCESS :
                            {
                                // If the 'e' expression is an id-expression or class member
                                // access, 'decltype(e)' is defined as the type of the entity
                                // named by 'e'. We remove the reference type.
                                is_removed_reference = 1;
                                break;
                            }
                        default:
                            {
                                // Function calls or other expressions will
                                // return 'lvalues' with form of 'reference to
                                // type'. So, we do not need to update the type
                                break;
                            }
                    }

                    if (is_dependent_type(computed_type))
                    {
                        // The expression type is dependent, wrap it in a typeof
                        computed_type = get_typeof_expr_dependent_type(nodecl_expr, decl_context,
                                /* is_decltype */ 1,
                                is_removed_reference);
                    }
                    else if (is_removed_reference)
                    {
                        computed_type = no_ref(computed_type);
                    }

                    *simple_type_info = computed_type;
                }
                else
                {
                    error_printf("%s: error: could not solve type '%s'\n",
                            ast_location(a),
                            prettyprint_in_buffer(a));
                    *simple_type_info = get_error_type();
                }
                break;
            }
            // GCC Extensions
        case AST_GCC_TYPEOF_EXPR :
            {
                // Compute the expression type and use it for the whole type
                nodecl_t nodecl_expr = nodecl_null();
                if (check_expression_non_executable(ASTSon0(a), decl_context, &nodecl_expr)
                        && (nodecl_get_type(nodecl_expr) != NULL))
                {
                    type_t* computed_type = nodecl_get_type(nodecl_expr);

                    C_LANGUAGE()
                    {
                        computed_type = no_ref(computed_type);
                    }

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
                                error_printf("%s: error: '%s' yields an unresolved overload type\n",
                                        ast_location(a),
                                        prettyprint_in_buffer(a));
                                *simple_type_info = get_error_type();
                                return;
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
                                        entry->entity_specs.class_type);
                            }
                        }
                        else if (nodecl_expr_is_type_dependent(nodecl_expr))
                        {
                            // The expression type is dependent, so we will wrap in an typeof expression
                            computed_type = get_typeof_expr_dependent_type(nodecl_expr, decl_context,
                                    /* is_decltype */ 0,
                                    /* is_removed_reference */ 0);
                        }
                    }

                    *simple_type_info = computed_type;
                }
                else
                {
                    error_printf("%s: error: could not solve type '%s'\n",
                            ast_location(a),
                            prettyprint_in_buffer(a));
                    *simple_type_info = get_error_type();
                }
                break;
            }
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
                memset(&typeof_gather_info, 0, sizeof(typeof_gather_info));

                // First declarator is NULL because types cannot be defined in 'typeof' expressions
                build_scope_decl_specifier_seq(type_specifier_seq, &typeof_gather_info,
                        &type_info, decl_context, nodecl_output);

                if (is_error_type(type_info))
                {
                    *simple_type_info = get_error_type();
                    return;
                }

                type_t* declarator_type = type_info;
                compute_declarator_type(abstract_decl,
                        &typeof_gather_info, type_info, &declarator_type,
                        decl_context, nodecl_output);

                *simple_type_info = declarator_type;
                break;
            }
        case AST_GCC_INT128:
            {
#if HAVE_INT128
                *simple_type_info = get_signed_int128_type();
#else
                error_printf("%s: error: __int128 support not available\n", ast_location(a));
                *simple_type_info = get_error_type();
#endif
                break;
            }
        case AST_GCC_FLOAT128:
            {
#ifdef HAVE_QUADMATH_H
                *simple_type_info = get_float128_type();
#else
                error_printf("%s: error: __float128 support not available\n", ast_location(a));
                *simple_type_info = get_error_type();
#endif
                break;
            }
        case AST_GXX_UNDERLYING_TYPE:
            {
                AST type_id = ASTSon0(a);
                AST type_specifier_seq = ASTSon0(type_id);
                AST abstract_decl = ASTSon1(type_id);

                type_t *type_info = NULL;

                gather_decl_spec_t typeof_gather_info;
                memset(&typeof_gather_info, 0, sizeof(typeof_gather_info));

                // First declarator is NULL because types cannot be defined in 'typeof' expressions
                build_scope_decl_specifier_seq(type_specifier_seq, &typeof_gather_info,
                        &type_info, decl_context, nodecl_output);

                if (is_error_type(type_info))
                {
                    *simple_type_info = get_error_type();
                    return;
                }

                type_t* declarator_type = type_info;
                compute_declarator_type(abstract_decl,
                        &typeof_gather_info, type_info, &declarator_type,
                        decl_context, nodecl_output);

                if (is_dependent_type(declarator_type)
                        || (is_enum_type(declarator_type)
                            && is_dependent_type(enum_type_get_underlying_type(declarator_type))))
                {
                    *simple_type_info = get_gxx_underlying_type(declarator_type);
                }
                else if (is_enum_type(declarator_type))
                {
                    *simple_type_info = enum_type_get_underlying_type(declarator_type);
                }
                else
                {
                    error_printf("%s: error: type-id of an __underlying_type must be an enum type\n",
                            ast_location(a));
                    *simple_type_info = get_error_type();
                }

                break;
            }
            // Mercurium extension @byte@
        case AST_MCC_BYTE:
            {
                // This type is an integer of sizeof(char) but not a char per se
                // this is useful only when declaring prototypes for Fortran
                *simple_type_info = get_signed_byte_type();
                break;
            }
            // Microsoft builtin types
        case AST_MS_INT8:
            {
                // Behaves like a char
                *simple_type_info = get_char_type();
                break;
            }
        case AST_MS_INT16:
            {
                // Behaves like a short
                *simple_type_info = get_signed_int_type();
                gather_info->is_short = 1;
                break;
            }
        case AST_MS_INT32:
            {
                // Behaves like int
                *simple_type_info = get_signed_int_type();
                break;
            }
        case AST_MS_INT64:
            {
                // May be long int or long long int depending on the environment
                *simple_type_info = get_signed_int_type();

                if (type_get_size(get_signed_long_int_type()) == 8)
                {
                    gather_info->is_long = 1;
                }
                else if (type_get_size(get_signed_long_long_int_type()) == 8)
                {
                    gather_info->is_long = 2;
                }
                else
                {
                    error_printf("%s: error: __int64 not supported\n", ast_location(a));
                    *simple_type_info = get_error_type();
                }
                break;
            }
            // Mercurium internal mechanism
        case AST_TYPE_LITERAL_REF:
            {
                const char *tmp = ASTText(ASTSon0(a));

                const char * prefix = NULL;
                void *p = NULL;
                unpack_pointer(tmp, &prefix, &p);

                ERROR_CONDITION(prefix == NULL || p == NULL || strcmp(prefix, "type") != 0,
                        "Failure during unpack of type", 0);

                *simple_type_info = (type_t*)p;
                break;
            }
        case AST_AMBIGUITY:
            {
                // GCC typeof
                solve_ambiguous_type_specifier(a, decl_context);
                gather_type_spec_information(a, simple_type_info, gather_info, decl_context, nodecl_output);
                break;
            }
            // Internal nodes for dependent stuff
        case NODECL_CXX_DEP_NAME_SIMPLE:
        case NODECL_CXX_DEP_TEMPLATE_ID:
        case NODECL_CXX_DEP_NAME_CONVERSION:
        case NODECL_CXX_DEP_NAME_NESTED:
        case NODECL_CXX_DEP_GLOBAL_NAME_NESTED:
            {
                nodecl_gather_type_spec_from_simple_type_specifier(_nodecl_wrap(a), simple_type_info, gather_info, decl_context);
                break;
            }
        default:
            {
                internal_error("Unknown node '%s'", ast_print_node_type(ASTType(a)));
            }
    }
}

/*
 * This function returns 1 if the class scope exists and it's dependent.
 * Otherwise returns 0.
 */
static char is_dependent_class_scope(decl_context_t decl_context)
{
    return (decl_context.class_scope != NULL 
            && is_dependent_type(decl_context.class_scope->related_entry->type_information));  
}

static void gather_type_spec_from_elaborated_friend_class_specifier(AST a,
        type_t** type_info,
        gather_decl_spec_t *gather_info,
        decl_context_t decl_context,
        nodecl_t* nodecl_output UNUSED_PARAMETER)
{
    /* FIXME
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

    AST class_key = ASTSon0(a);

    enum type_tag_t class_kind = TT_INVALID;
    decl_flags_t class_kind_flag = DF_NONE;
    switch (ASTType(class_key))
    {
        case AST_CLASS_KEY_CLASS:
            {
                class_kind = TT_CLASS;
                class_kind_flag = DF_CLASS;
                break;
            }
        case AST_CLASS_KEY_STRUCT:
            {
                class_kind = TT_STRUCT;
                class_kind_flag = DF_STRUCT;
                break;
            }
        case AST_CLASS_KEY_UNION:
            {
                class_kind = TT_UNION;
                class_kind_flag = DF_UNION;
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
        decl_flags |= class_kind_flag;
    }

    // decl_context_query is a new decl_context_t created  for the queries
    decl_context_t decl_context_query = decl_context;

    // If a friend declaration appears in a local class and the specified name is not qualified,
    // we only look up in the innermost enclosing non-class scope
    //
    //  class X {};
    //  class Y {};
    //  void foo()
    //  {
    //       class Y {};
    //       class A
    //       {
    //           friend class X; // X is not found in ::Foo()
    //           friend class Y; // Y is found  ::Foo()::Y
    //       };
    //
    //       X x; // ::X;
    //       Y y; // ::Foo()::Y
    //  }
    char is_local_class_friend_decl = 0;
    if (gather_info->is_friend
            && is_unqualified_id_expression(id_expression)
            && decl_context.current_scope->kind == CLASS_SCOPE
            && decl_context.current_scope->contained_in != NULL
            && decl_context.current_scope->contained_in->kind == BLOCK_SCOPE)

    {
        is_local_class_friend_decl = 1;
        decl_flags |= DF_ONLY_CURRENT_SCOPE;
        decl_context_query.current_scope = decl_context.current_scope->contained_in;
    }

    result_list = query_id_expression_flags(decl_context_query,
            id_expression, NULL, decl_flags | DF_DEPENDENT_TYPENAME);

    enum cxx_symbol_kind filter_classes[] =
    {
        SK_CLASS,
        SK_DEPENDENT_ENTITY,
        SK_TEMPLATE,
    };

    scope_entry_list_t* entry_list = filter_symbol_kind_set(result_list,
            STATIC_ARRAY_LENGTH(filter_classes), filter_classes);

    entry_list_free(result_list);
    scope_entry_t* entry = (entry_list != NULL) ? entry_list_head(entry_list) : NULL;
    entry_list_free(entry_list);

    if (entry == NULL
            // In theses cases the name must exist
            && (ASTType(id_expression) == AST_TEMPLATE_ID
                || is_qualified_id_expression(id_expression)))
    {
        error_printf("%s: error: class name '%s' not found\n",
                ast_location(id_expression),
                prettyprint_in_buffer(id_expression));
        *type_info = get_error_type();
        return;
    }

    scope_entry_t* class_symbol = decl_context.current_scope->related_entry;
    ERROR_CONDITION(class_symbol->kind != SK_CLASS, "Invalid symbol", 0);

    const char* class_name = NULL;
    switch (ASTType(id_expression))
    {
        case AST_SYMBOL:
            {
                class_name = ASTText(id_expression);
                break;
            }
        case AST_TEMPLATE_ID:
            {
                class_name = ASTText(ASTSon0(id_expression));
                break;
            }
        case AST_QUALIFIED_ID:
            {
                class_name = ASTText(ASTSon2(id_expression));
                break;
            }
        default:
            {
                error_printf("%s: invalid class specifier '%s'\n",
                        ast_location(id_expression),
                        prettyprint_in_buffer(id_expression));
                *type_info = get_error_type();
                return;
            }
    }

    class_entry = entry;
    if (entry != NULL)
    {
        if (entry->entity_specs.is_injected_class_name)
        {
            if (gather_info->is_template
                    && is_template_specialized_type(entry->type_information))
            {
                /*
                   It may happen that we find the injected class name like in the following case

                   template <typename T>
                   struct A;

                   template <typename T>
                   struct A<T*>
                   {
                       template <typename S>
                       friend struct A;
                   };
                 */
                // Get the template-name instead
                entry = template_type_get_related_symbol(
                            template_specialized_type_get_related_template_type(entry->type_information)
                            );
            }
            else
            {
                entry = named_type_get_symbol(entry->entity_specs.class_type);
            }
        }

        if (gather_info->is_template
                && entry->kind == SK_CLASS
                && is_template_specialized_type(entry->type_information)
                && is_dependent_type(entry->type_information)
                && !equivalent_types(
                    get_user_defined_type(entry),
                    template_type_get_primary_type(
                        template_specialized_type_get_related_template_type(entry->type_information)
                        )))
        {
            // The user is attempting something like this
            /*
               struct B
               {
                   template <typename S>
                   friend struct A<S*>;
               };

             */
            // If this dependent specialized type is not the primary we are
            // attempting to be friends with a partial specialization
            error_printf("%s: error: cannot declare as friend a partial specialization\n",
                    ast_location(id_expression));
        }

        scope_entry_t* alias_to_entry = class_entry;
        if (gather_info->is_template
                || ASTType(id_expression) == AST_TEMPLATE_ID
                || ASTType(id_expression) == AST_QUALIFIED_ID)
        {
            // We create a fake symbol with the right context and an alias to
            // the real friend (entry)
            alias_to_entry =
                counted_xcalloc(1, sizeof(*entry), &_bytes_used_buildscope);
            alias_to_entry->kind = SK_DEPENDENT_FRIEND_CLASS;
            alias_to_entry->locus = ast_get_locus(a);

            alias_to_entry->symbol_name = class_name;
            alias_to_entry->decl_context = decl_context;

            alias_to_entry->entity_specs.is_friend_declared = 1;
            alias_to_entry->entity_specs.is_user_declared = 0;

            alias_to_entry->entity_specs.alias_to = entry;
        }

        // Promote a SK_DEPENDENT_ENTITY to SK_DEPENDENT_FRIEND_CLASS
        if (class_entry->kind == SK_DEPENDENT_ENTITY)
        {
            class_entry->kind = SK_DEPENDENT_FRIEND_CLASS;
            set_dependent_entry_kind(class_entry->type_information, class_kind);
        }

        *type_info = get_void_type();
        class_type_add_friend_symbol(class_symbol->type_information, alias_to_entry);
    }
    else
    {
        // Note that entry can be NULL only if the class-name is unqualified
        if (is_dependent_type(class_symbol->type_information))
        {
            class_entry = counted_xcalloc(1, sizeof(*entry), &_bytes_used_buildscope);
            class_entry->kind = SK_DEPENDENT_FRIEND_CLASS;
            class_entry->locus = ast_get_locus(a);

            if (gather_info->is_template)
            {
                class_entry->kind = SK_TEMPLATE;
                class_entry->type_information = get_new_template_type(decl_context.template_parameters,
                        get_new_class_type(decl_context, class_kind),
                        ASTText(id_expression), decl_context,
                        ast_get_locus(id_expression));
                template_type_set_related_symbol(class_entry->type_information, class_entry);

                // Get the primary class
                scope_entry_t* primary_symbol =
                    named_type_get_symbol(template_type_get_primary_type(class_entry->type_information));

                // Update some fields
                primary_symbol->kind = SK_DEPENDENT_FRIEND_CLASS;
                primary_symbol->locus = ast_get_locus(a);

                primary_symbol->entity_specs.is_friend_declared = 1;
                primary_symbol->entity_specs.is_user_declared = 0;

                class_type = primary_symbol->type_information;

                *type_info = get_void_type();
                // The template symbol is added as a friend!
                class_type_add_friend_symbol(class_symbol->type_information, class_entry);
                class_entry = primary_symbol;
            }
            else
            {
                *type_info = get_void_type();
                class_type_add_friend_symbol(class_symbol->type_information, class_entry);
            }
        }
        else
        {
            if (is_local_class_friend_decl)
            {
                // The new symbol will be created in a BLOCK_SCOPE
                decl_context.current_scope = decl_context.current_scope->contained_in;
            }
            else
            {
                decl_context.current_scope = decl_context.namespace_scope;
            }

            scope_entry_t* new_class = NULL;
            new_class = new_symbol(decl_context, decl_context.current_scope, class_name);
            new_class->locus = ast_get_locus(id_expression);
            
            new_class->entity_specs.is_friend_declared = 1;
            new_class->entity_specs.is_user_declared = 0;

            if(gather_info->is_template)
            {
                // A template class is declared inside a non-template class
                new_class->kind = SK_TEMPLATE;
                new_class->type_information = get_new_template_type(decl_context.template_parameters,
                        get_new_class_type(decl_context, class_kind),
                        ASTText(id_expression), decl_context,
                        ast_get_locus(id_expression));
                template_type_set_related_symbol(new_class->type_information, new_class);

                if (decl_context.current_scope->kind == CLASS_SCOPE)
                {
                    new_class->entity_specs.is_member = 1;
                    // FIXME!
                    // new_class->entity_specs.access = current_access;
                    new_class->entity_specs.class_type =
                        get_user_defined_type(decl_context.current_scope->related_entry);
                }

                // Get the primary class
                class_entry = named_type_get_symbol(template_type_get_primary_type(new_class->type_information));

                // Update some fields
                class_entry->locus = ast_get_locus(a);

                class_entry->entity_specs.is_friend_declared = 1;
                class_entry->entity_specs.is_user_declared = 0;

                class_type = class_entry->type_information;
            }
            else
            {
                // A non-template class is declared inside an other non-template class
                new_class->type_information = get_new_class_type(decl_context, class_kind);
                new_class->kind = SK_CLASS;
                class_entry = new_class;
                class_type = class_entry->type_information;
            }

            // If the class is being declared in class-scope it means
            // it is a nested class
            if (decl_context.current_scope->kind == CLASS_SCOPE)
            {
                // If the enclosing class is dependent, so is this one
                scope_entry_t* enclosing_class_symbol = decl_context.current_scope->related_entry;
                type_t* enclosing_class_type = enclosing_class_symbol->type_information;

                char c = is_dependent_type(class_entry->type_information);
                c = c || is_dependent_type(enclosing_class_type);
                set_is_dependent_type(class_entry->type_information, c);

                class_type_set_enclosing_class_type(class_type, get_user_defined_type(enclosing_class_symbol));
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
            class_type_add_friend_symbol(class_symbol->type_information, class_entry);
            *type_info = get_user_defined_type(class_entry);

            // Friend class declarations do not create a new cxx_decl nodecl
        }
    }
}

static char check_class_template_parameters(const locus_t* locus, template_parameter_list_t* template_parameters)
{
    int i;
    for (i = 0; i < template_parameters->num_parameters; i++)
    {
        if (template_parameter_kind_is_pack(template_parameters->parameters[i]->kind)
                && i != (template_parameters->num_parameters - 1))
        {
            error_printf("%s: error: a template-pack of a classe template must be the last template parameter\n",
                    locus_to_str(locus));
            return 0;
        }
    }
    return 1;
}

static void gather_extra_attributes(AST a,
        gather_decl_spec_t* gather_info,
        decl_context_t decl_context)
{
    if (a == NULL)
        return;

    ERROR_CONDITION(ASTType(a) != AST_NODE_LIST, "Invalid node", 0);

    AST it;
    for_each_element(a, it)
    {
        AST item = ASTSon1(it);
        switch (ASTType(item))
        {
            case AST_GCC_ATTRIBUTE:
                {
                    gather_gcc_attribute(item, gather_info, decl_context);
                    break;
                }
            case AST_GCC_ASM_SPEC:
                {
                    gather_info->gcc_asm_spec = item;
                    break;
                }
            case AST_CLASS_VIRT_SPEC:
            case AST_MEMBER_VIRT_SPEC:
                {
                    gather_single_virt_specifier(item, gather_info, decl_context);
                    break;
                }
            case AST_MS_DECLSPEC:
                {
                    gather_ms_declspec(item, gather_info, decl_context);
                    break;
                }
            default:
                {
                    internal_error("Unexpected node '%s'\n", ast_print_node_type(ASTType(item)));
                    break;
                }
        }
    }
}

static void apply_attributes_to_type(type_t** type,
        AST a,
        decl_context_t decl_context)
{
    if (a == NULL)
        return;

    ERROR_CONDITION(ASTType(a) != AST_NODE_LIST, "Invalid node", 0);

    AST it;
    for_each_element(a, it)
    {
        AST item = ASTSon1(it);
        switch (ASTType(item))
        {
            case AST_GCC_ATTRIBUTE:
                {
                    apply_gcc_attribute_to_type(item, type, decl_context);
                    break;
                }
            case AST_MS_DECLSPEC:
                {
                    apply_ms_attribute_to_type(item, type, decl_context);
                    break;
                }
                // FIXME - Standard attributes
            default:
                {
                    internal_error("Unexpected node '%s'\n", ast_print_node_type(ASTType(item)));
                    break;
                }
        }
    }
}

static void gather_type_spec_from_elaborated_class_specifier(AST a,
        type_t** type_info,
        gather_decl_spec_t *gather_info,
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
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
    gather_decl_spec_t class_gather_info;
    copy_gather_info(&class_gather_info, gather_info);

    scope_entry_t* class_entry = NULL;
    type_t* class_type = NULL;

    AST class_key = ASTSon0(a);

    enum type_tag_t class_kind = TT_INVALID;
    const char *class_kind_name = NULL;
    decl_flags_t class_kind_flag = DF_NONE;
    switch (ASTType(class_key))
    {
        case AST_CLASS_KEY_CLASS:
            {
                class_kind = TT_CLASS;
                class_kind_flag = DF_CLASS;
                class_kind_name = "class";
                break;
            }
        case AST_CLASS_KEY_STRUCT:
            {
                class_kind = TT_STRUCT;
                class_kind_flag = DF_STRUCT;
                class_kind_name = "struct";
                break;
            }
        case AST_CLASS_KEY_UNION:
            {
                class_kind = TT_UNION;
                class_kind_flag = DF_UNION;
                class_kind_name = "union";
                break;
            }
        default:
            internal_error("Code unreachable", 0);
    }

    AST id_expression = ASTSon1(a);
    AST extra_attributes = ASTSon2(a);

    if (extra_attributes != NULL)
    {
        gather_extra_attributes(extra_attributes, &class_gather_info, decl_context);
    }

    scope_entry_list_t* result_list = NULL;

    decl_flags_t decl_flags = DF_NONE;

    if (is_unqualified_id_expression(id_expression))
    {
        decl_flags |= class_kind_flag;
    }

    char is_friend_class_declaration =
        (class_gather_info.no_declarators && class_gather_info.is_friend);

    // The friend class declarations are a bit special. For this reason, they are treated
    // in a separate 'gather_type_spec_from_elaborated_friend_class_specifier' function
    if (is_friend_class_declaration)
    {
        gather_type_spec_from_elaborated_friend_class_specifier(a, type_info, &class_gather_info, decl_context, nodecl_output);
        return;
    }

    char declare_something = !class_gather_info.no_declarators ||
        !(
                // These examples do not declare anything:
                // template < typename T1>
                // struct B
                // {
                //      class F {};
                // };
                // struct A
                // {
                //      struct B<T>;
                //      struct B<T>::F;
                // };
                !class_gather_info.is_template &&
                !class_gather_info.parameter_declaration &&
                (ASTType(id_expression) == AST_TEMPLATE_ID || ASTType(id_expression) == AST_QUALIFIED_ID)
         );

    CXX_LANGUAGE()
    {
        if (class_gather_info.no_declarators
                && !class_gather_info.parameter_declaration
                && ASTType(id_expression) != AST_TEMPLATE_ID)
        {
            if (is_unqualified_id_expression(id_expression))
            {
                result_list = query_in_scope_flags(decl_context, id_expression, NULL, decl_flags);
            }
            else
            {
                result_list = query_id_expression_flags(decl_context,
                        id_expression, NULL, decl_flags | DF_DEPENDENT_TYPENAME);
            }
        }
        else
        {
            result_list = query_id_expression_flags(decl_context,
                    id_expression, NULL, decl_flags | DF_DEPENDENT_TYPENAME);
        }
    }

    C_LANGUAGE()
    {
        const char* class_name = ASTText(id_expression);
        class_name = strappend(class_kind_name, strappend(" ", class_name));

        result_list = query_name_str(decl_context, class_name, NULL);
    }

    // Now look for a type
    enum cxx_symbol_kind filter_classes[] =
    {
        SK_CLASS,
        SK_DEPENDENT_ENTITY,
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
            error_printf("%s: error: redeclaration with %d template parameters while previous declaration used %d\n",
                    ast_location(id_expression),
                    decl_context.template_parameters->num_parameters,
                    template_type_get_template_parameters(entry->type_information)->num_parameters);
            *type_info = get_error_type();
            return;
        }

        template_type_update_template_parameters(entry->type_information,
                decl_context.template_parameters);

        // This is the class name
        type_t* primary_template_type = template_type_get_primary_type(entry->type_information);
        entry = named_type_get_symbol(primary_template_type);
    }

    if (entry == NULL)
    {
        // The name was not found.
        // We will have to create a symbol (if unqualified, otherwise this is an error)
        if (is_unqualified_id_expression(id_expression))
        {
            if (!class_gather_info.no_declarators
                    || class_gather_info.parameter_declaration)
            {
                // Note that in a parameter declaration no type can be defined actually
                if (class_gather_info.parameter_declaration)
                {
                    while (decl_context.current_scope->kind == BLOCK_SCOPE)
                    {
                        decl_context.current_scope = decl_context.current_scope->contained_in;
                    }
                }
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
                error_printf("%s: invalid class specifier '%s'\n",
                        ast_location(id_expression),
                        prettyprint_in_buffer(id_expression));
                *type_info = get_error_type();
                return;
            }

            C_LANGUAGE()
            {
                class_name = strappend(class_kind_name, strappend(" ", class_name));
            }

            scope_entry_t* new_class = NULL;
            new_class = new_symbol(decl_context, decl_context.current_scope, class_name);

            new_class->locus = ast_get_locus(id_expression);

            if ((!class_gather_info.is_template
                        || !class_gather_info.no_declarators)
                    && ASTType(id_expression) != AST_TEMPLATE_ID)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "BUILDSCOPE: Type not found, creating a stub in scope %p for '%s' %p\n",
                            decl_context.current_scope,
                            class_name,
                            new_class);
                }

                new_class->type_information = get_new_class_type(decl_context, class_kind);
                new_class->kind = SK_CLASS;

                class_entry = new_class;
                class_type = class_entry->type_information;
            }
            else
            {
                if (ASTType(id_expression) != AST_TEMPLATE_ID)
                {
                    if (!check_class_template_parameters(ast_get_locus(a), decl_context.template_parameters))
                    {
                        *type_info = get_error_type();
                        return;
                    }

                    new_class->kind = SK_TEMPLATE;
                    new_class->type_information = get_new_template_type(decl_context.template_parameters,
                            get_new_class_type(decl_context, class_kind),
                            ASTText(id_expression), decl_context,
                            ast_get_locus(id_expression));
                    template_type_set_related_symbol(new_class->type_information, new_class);

                    new_class->locus = ast_get_locus(a);

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
                    class_entry->locus = ast_get_locus(a);

                    class_type = class_entry->type_information;
                }
                else
                {
                    // This is invalid because it is "class A<int>" but we
                    // didn't find any symbol related to it
                    error_printf("%s: error: invalid template-name '%s'\n",
                            ast_location(id_expression),
                            prettyprint_in_buffer(id_expression));
                    *type_info = get_error_type();
                    return;
                }
            }
        }
        else
        {
            error_printf("%s: error: class name '%s' not found\n",
                    ast_location(id_expression),
                    prettyprint_in_buffer(id_expression));
            *type_info = get_error_type();
            return;
        }

        // If the class is being declared in class-scope it means
        // it is a nested class
        if (decl_context.current_scope->kind == CLASS_SCOPE)
        {
            scope_entry_t* enclosing_class_symbol = decl_context.current_scope->related_entry;
            type_t* enclosing_class_type = enclosing_class_symbol->type_information;
            class_type_add_member(enclosing_class_type, class_entry,
                    /* is_definition */ 0);

            CXX_LANGUAGE()
            {
                class_entry->entity_specs.is_member = 1;
                class_entry->entity_specs.access = class_gather_info.current_access;
                class_entry->entity_specs.class_type = get_user_defined_type(enclosing_class_symbol);
            }

            class_type_set_enclosing_class_type(class_type, get_user_defined_type(enclosing_class_symbol));

            // If the enclosing class is dependent, so is this one
            char c = is_dependent_type(class_entry->type_information);
            c = c || is_dependent_type(enclosing_class_type);
            set_is_dependent_type(class_entry->type_information, c);
        }
        else if (decl_context.current_scope->kind == BLOCK_SCOPE)
        {
            // This is a local class
            scope_entry_t* enclosing_function = decl_context.current_scope->related_entry;

            // A local class is dependent if enclosed in a template function or
            // a member function of a template class
            if (enclosing_function != NULL
                    && (is_dependent_type(enclosing_function->type_information)
                        || (enclosing_function->entity_specs.is_member
                            && is_dependent_type(enclosing_function->entity_specs.class_type))))
            {
                set_is_dependent_type(class_entry->type_information, 1);
            }
        }
    }
    else
    {
        if (entry->kind == SK_DEPENDENT_ENTITY)
        {
            if (!declare_something)
            {
                error_printf("%s: error: declaration '%s' does not declare anything\n",
                        ast_location(id_expression),
                        prettyprint_in_buffer(id_expression));
                *type_info = get_error_type();
                return;
            }
            *type_info = entry->type_information;

            if (class_gather_info.num_gcc_attributes != 0
                    || class_gather_info.num_ms_attributes != 0)
            {
                // Clone the symbol to avoid modifying the SK_DEPENDENT_ENTITY
                // returned by the scope
                scope_entry_t* old_entry = entry;
                entry = xcalloc(1, sizeof(*entry));
                *entry = *old_entry;

                keep_gcc_attributes_in_symbol(entry, &class_gather_info);
                keep_ms_declspecs_in_symbol(entry, &class_gather_info);
            }
            return;
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: Class type found already declared in %s, using it\n", locus_to_str(entry->locus));
        }

        ERROR_CONDITION(entry->kind != SK_CLASS, "This must be a class", 0);

        if (class_gather_info.no_declarators
                && (ASTType(id_expression) == AST_TEMPLATE_ID
                    || ASTType(id_expression) == AST_QUALIFIED_ID)
                && !class_gather_info.is_template
                && !class_gather_info.is_explicit_specialization
                && !class_gather_info.is_explicit_instantiation)
        {
            error_printf("%s: error: declaration '%s' does not declare anything\n",
                    ast_location(id_expression),
                    prettyprint_in_buffer(id_expression));
            *type_info = get_error_type();
            return;
        }

        class_entry = entry;
        class_type = class_entry->type_information;

        if (!class_gather_info.is_friend
                && entry->entity_specs.is_friend_declared)
        {
            entry->entity_specs.is_friend_declared = 0;
        }

        // Check the enclosing namespace scope
        // This is only valid if the scope of the entry is an inlined namespace of the current one
        if (!class_gather_info.is_explicit_specialization
                && !class_gather_info.is_explicit_instantiation
                && is_template_specialized_type(class_entry->type_information)
                && (class_entry->decl_context.namespace_scope != decl_context.namespace_scope)
                && !is_inline_namespace_of(class_entry->decl_context, decl_context))
        {
            error_printf("%s: specialization of '%s' in different namespace from definition\n",
                    ast_location(id_expression),
                    prettyprint_in_buffer(id_expression));
            *type_info = get_error_type();
            return;
        }
        // We only update the template parameters of decl_context of the class symbol if:
        //  1. This is a template declaration
        //  2. The class has not been defined (we want to store the template parameters of its definition!)
        //  3. It is a template specialized class
        //  4. It is not a explicit instantiation (they have not template parameters!)
        if (gather_info->is_template
                && !class_entry->defined
                && is_template_specialized_type(class_entry->type_information)
                && !class_gather_info.is_explicit_specialization
                && !class_gather_info.is_explicit_instantiation)
        {
            template_specialized_type_update_template_parameters(
                    class_entry->type_information,
                    decl_context.template_parameters);

            // Update the template_scope
            DEBUG_CODE()
            {
                fprintf(stderr, "BUILDSCOPE: Updating template scope\n");
            }
            class_entry->decl_context.template_parameters = decl_context.template_parameters;
        }
    }

    ERROR_CONDITION(class_entry == NULL, "Invalid class entry", 0);

    if ((!is_template_specialized_type(class_entry->type_information) ||
            (class_gather_info.is_template && class_gather_info.no_declarators)))
    {
        // State this symbol has been created by the code and not by the type system
        class_entry->entity_specs.is_user_declared = 1;
        class_entry->entity_specs.is_instantiable = 1;
    }

    keep_gcc_attributes_in_symbol(class_entry, &class_gather_info);
    keep_ms_declspecs_in_symbol(class_entry, &class_gather_info);

    *type_info = get_user_defined_type(class_entry);

    CXX_LANGUAGE()
    {
        nodecl_t nodecl_context =
            nodecl_make_context(/* optional statement sequence */ nodecl_null(),
                    decl_context, ast_get_locus(a));

        // Remember the declaration
        *nodecl_output =
            nodecl_concat_lists(
                    *nodecl_output,
                    nodecl_make_list_1(
                        nodecl_make_cxx_decl(
                            nodecl_context,
                            class_entry,
                            ast_get_locus(a))));
    }
}

static void gather_type_spec_from_elaborated_enum_specifier(AST a,
        type_t** type_info,
        gather_decl_spec_t* gather_info,
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    AST enum_key = ASTSon3(a);
    AST enum_attribute_specifier = ASTSon1(a);
    AST id_expression = ASTSon0(a);
    AST enum_base = ASTSon2(a);

    gather_cxx11_attributes(enum_attribute_specifier, gather_info);

    char enum_is_scoped = ASTType(enum_key) == AST_SCOPED_ENUM_KEY;

    if(IS_CXX03_LANGUAGE
            && enum_is_scoped)
    {
        warn_printf("%s: warning: scoped enumerators are only valid in C++11\n", ast_location(enum_key));
    }

    scope_entry_list_t* result_list = NULL;

    decl_flags_t decl_flags = DF_NONE;

    if (is_unqualified_id_expression(id_expression))
    {
        decl_flags |= DF_ENUM;
    }

    CXX_LANGUAGE()
    {
        if (gather_info->no_declarators
                && !gather_info->parameter_declaration)
        {
            if (is_unqualified_id_expression(id_expression))
            {
                result_list = query_in_scope_flags(decl_context, id_expression, NULL, decl_flags);
            }
            else
            {
                result_list = query_id_expression_flags(decl_context, id_expression, NULL, decl_flags);
            }
        }
        else
        {
            result_list = query_id_expression_flags(decl_context, id_expression, NULL, decl_flags);
        }
    }

    C_LANGUAGE()
    {
        const char* enum_name = ASTText(id_expression);

        enum_name = strappend("enum ", enum_name);
        result_list = query_name_str(decl_context, enum_name, NULL);
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
            error_printf("%s: error: '%s' is not an enum-name\n",
                    locus_to_str(current_entry->locus), current_entry->symbol_name);
            *type_info = get_error_type();
            return;
        }
        else
        {
            entry = current_entry;
        }
    }
    entry_list_iterator_free(it);
    entry_list_free(result_list);

    type_t* underlying_type = NULL;
    if (enum_base != NULL)
    {
        if (IS_CXX03_LANGUAGE)
        {
            warn_printf("%s: warning: enum-base is only valid in C++11\n",
                    ast_location(a));
        }

        underlying_type = compute_type_for_type_id_tree(enum_base, decl_context, NULL, NULL);

        if (is_error_type(underlying_type))
        {
            *type_info = underlying_type;
            return;
        }
    }
    else if (enum_is_scoped)
    {
        underlying_type = get_signed_int_type();
    }

    if (entry == NULL)
    {
        // Create a stub but only if it is unqualified, otherwise it should exist elsewhere
        char gcc_extern_enum = 0;
        if (is_unqualified_id_expression(id_expression)
                // If does not exist and there are no declarators
                && ((gather_info->no_declarators
                        && !gather_info->parameter_declaration)
                    || (gcc_extern_enum = (IS_C_LANGUAGE && gather_info->is_extern))
                    || (IS_CXX11_LANGUAGE && (enum_base != NULL))))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "BUILDSCOPE: Enum type not found, creating a stub for this scope\n");
            }

            if (ASTType(id_expression) != AST_SYMBOL)
            {
                error_printf("%s: invalid enum-name '%s'\n",
                        ast_location(id_expression),
                        prettyprint_in_buffer(id_expression));
                *type_info = get_error_type();
                return;
            }

            const char* enum_name = ASTText(id_expression);

            C_LANGUAGE()
            {
                enum_name = strappend("enum ", enum_name);
            }

            if (gcc_extern_enum)
            {
                warn_printf("%s: warning: previously undeclared '%s' is a GCC extension\n",
                        ast_location(id_expression),
                        enum_name);
            }

            decl_context_t new_decl_context = decl_context;

            if (!gather_info->no_declarators)
            {
                // If no declarators declare it in the first non-class enclosing namespace
                new_decl_context.current_scope = decl_context.namespace_scope;
            }

            scope_entry_t* new_enum = new_symbol(new_decl_context, new_decl_context.current_scope, enum_name);
            new_enum->locus = ast_get_locus(id_expression);
            new_enum->kind = SK_ENUM;
            new_enum->type_information = get_new_enum_type(decl_context, enum_is_scoped);

            new_enum->entity_specs.is_user_declared = 1;

            *type_info = get_user_defined_type(new_enum);

            if (new_decl_context.current_scope->kind == CLASS_SCOPE)
            {
                scope_entry_t* class_symbol = new_decl_context.current_scope->related_entry;
                type_t* class_type = class_symbol->type_information;
                class_type_add_member(get_actual_class_type(class_type), new_enum,
                        /* is_definition */ 0);

                CXX_LANGUAGE()
                {
                    new_enum->entity_specs.is_member = 1;
                    new_enum->entity_specs.access = gather_info->current_access;
                    new_enum->entity_specs.class_type = get_user_defined_type(class_symbol);
                }

                set_is_dependent_type(new_enum->type_information,
                        is_dependent_type(class_type));
            }

            entry = new_enum;
        }
        else
        {
            error_printf("%s: error: enum type '%s' not found\n", ast_location(a), prettyprint_in_buffer(a));
            *type_info = get_error_type();
            return;
        }
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: Enum type found in %s, using it\n", locus_to_str(entry->locus));
        }

        *type_info = get_user_defined_type(entry);

        if (enum_type_get_underlying_type_is_fixed(entry->type_information)
                && underlying_type != NULL
                && !equivalent_types(enum_type_get_underlying_type(entry->type_information),
                    underlying_type))
        {
            error_printf("%s: error: enumerator previously declared with different underlying-type\n",
                    ast_location(a));
        }
    }

    if (underlying_type != NULL)
    {
        enum_type_set_underlying_type(entry->type_information, underlying_type);
        enum_type_set_underlying_type_is_fixed(entry->type_information, 1);
    }

    CXX_LANGUAGE()
    {
        nodecl_t nodecl_context =
            nodecl_make_context(/* optional statement sequence */ nodecl_null(),
                    decl_context, ast_get_locus(a));

        *nodecl_output =
            nodecl_concat_lists(
                    *nodecl_output,
                    nodecl_make_list_1(
                        nodecl_make_cxx_decl(
                            nodecl_context,
                            entry,
                            ast_get_locus(a))));
    }
}

#if 0
static char dependent_entry_is_same_class_base_or_nested(scope_entry_t* dependent_entry, 
        scope_entry_t* current_class)
{
    type_t* enclosing_class = NULL;
    if ((current_class == dependent_entry)
            || class_type_is_base(
                dependent_entry->type_information, 
                current_class->type_information))
    {
        return 1;
    }
    else if ((enclosing_class = class_type_get_enclosing_class_type(current_class->type_information)))
    {
        scope_entry_t* enclosing_class_symbol = named_type_get_symbol(enclosing_class);
        return dependent_entry_is_same_class_base_or_nested(dependent_entry, enclosing_class_symbol);
    }
    return 0;
}

static char entry_of_dependent_typename_is_in_an_enclosing_class(type_t* dependent_typename,
        decl_context_t decl_context)
{
     if (decl_context.class_scope == NULL)
         return 0;

     scope_entry_t* dependent_entry = NULL;
     nodecl_t nodecl_dependent_parts = nodecl_null();

     dependent_typename_get_components(dependent_typename, &dependent_entry, &nodecl_dependent_parts);

     if (dependent_entry->kind != SK_CLASS)
         return 0;

     scope_entry_t* class_in_scope = decl_context.class_scope->related_entry;

     return dependent_entry_is_same_class_base_or_nested(dependent_entry, class_in_scope);
}
#endif

static void gather_type_spec_from_dependent_typename(AST a, 
        type_t** type_info,
        gather_decl_spec_t* gather_info,
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
            NULL,
            // We do not want to use uninstantiated 
            // templates when looking up
            DF_DEPENDENT_TYPENAME);

    if (result == NULL)
    {
        error_printf("%s: error: typename '%s' not found\n", 
                ast_location(id_expression),
                prettyprint_in_buffer(id_expression));
        *type_info = get_error_type();
        return;
    }

    scope_entry_t* entry = entry_list_head(result);
    // Peek the list to see if it is a dependent typename
    if (entry->kind != SK_DEPENDENT_ENTITY)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: Dependent typename refers to an existing type '%s'\n",
                    print_declarator(entry->type_information));
        }

        // Follow the usual path here
        common_gather_type_spec_from_simple_type_specifier(a,
                decl_context,
                type_info,
                gather_info,
                result);
        return;
    }

    entry_list_free(result);

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

static void common_gather_type_spec_from_simple_type_specifier(AST a, 
        decl_context_t decl_context UNUSED_PARAMETER,
        type_t** type_info, gather_decl_spec_t* gather_info, scope_entry_list_t* query_results)
{
    if (query_results == NULL)
    {
        error_printf("%s: error: type name '%s' has not been found in the current scope\n",
                ast_location(a), prettyprint_in_buffer(a));
        *type_info = get_error_type();
        return;
    }

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(query_results);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_advance_aliases(entry_list_iterator_current(it));
        if (entry->kind != SK_ENUM
                && entry->kind != SK_CLASS
                && entry->kind != SK_TYPEDEF
                && entry->kind != SK_TEMPLATE_TYPE_PARAMETER
                && entry->kind != SK_TEMPLATE_TYPE_PARAMETER_PACK
                && (!gather_info->allow_class_template_names
                    || entry->kind != SK_TEMPLATE
                    // Do not allow template-names of template functions
                    || named_type_get_symbol(template_type_get_primary_type(entry->type_information))->kind == SK_FUNCTION)
                && (!gather_info->allow_class_template_names || entry->kind != SK_TEMPLATE_TEMPLATE_PARAMETER)
                && (!gather_info->allow_class_template_names || entry->kind != SK_TEMPLATE_TEMPLATE_PARAMETER_PACK)
                && entry->kind != SK_GCC_BUILTIN_TYPE
                && entry->kind != SK_USING_TYPENAME
                && entry->kind != SK_TEMPLATE_ALIAS)
        {
            error_printf("%s: error: identifier '%s' does not name a type\n",
                    ast_location(a),
                    prettyprint_in_buffer(a));
            if (entry->kind == SK_DEPENDENT_ENTITY)
            {
                info_printf("%s: info: maybe you meant '%s'\n",
                        ast_location(a),
                        print_type_str(entry->type_information, entry->decl_context));
            }
            *type_info = get_error_type();
            return;
        }
    }
    entry_list_iterator_free(it);

    scope_entry_t* entry = entry_advance_aliases(entry_list_head(query_results));

    // template < typename T >
    // struct A
    // {
    //      struct MyStruct { int x };
    // };
    //
    // template < typename T >
    // struct B : public A<T>
    // {
    //      using typename A<T>::MyStruct;
    //
    //      MyStruct foo() // (1)
    //      {
    //      }
    // };
    //
    // At (1), MyStruct is a simple type specifier. In this case we should do the same
    // as gather_type_spec_from_dependent_typename function
    if (entry->kind == SK_USING_TYPENAME)
    {
        ERROR_CONDITION(entry->entity_specs.alias_to->kind != SK_DEPENDENT_ENTITY, "Expecting a dependent entity", 0);
        *type_info = entry->entity_specs.alias_to->type_information;
        return;
    }
    // Chances are that through class-scope lookup we have found the injected name
    if (entry->entity_specs.is_injected_class_name)
    {
        entry = named_type_get_symbol(entry->entity_specs.class_type);
    }

    entry_list_free(query_results);

#if 0
    // If this is a member of a dependent class or a local entity of a template
    // function craft a dependent typename for it
    if (is_dependent_type(entry->type_information)
            && symbol_is_member_of_dependent_class(entry))
    {
        // Craft a nodecl name for it
        nodecl_t nodecl_simple_name = nodecl_make_cxx_dep_name_simple(
                entry->symbol_name,
                ast_get_locus(a));

        nodecl_t nodecl_name = nodecl_simple_name;

        if (is_template_specialized_type(entry->type_information))
        {
            nodecl_name = nodecl_make_cxx_dep_template_id(
                    nodecl_name,
                    // If our enclosing class is dependent
                    // this template id will require a 'template '
                    "template ",
                    template_specialized_type_get_template_arguments(entry->type_information),
                    ast_get_locus(a));
        }

        // Craft a dependent typename since we will need it later for proper updates
        (*type_info) = build_dependent_typename_for_entry(
                get_function_or_class_where_symbol_depends(entry),
                nodecl_name,
                ast_get_locus(a));
    }
    else
#endif
    {
        (*type_info) = get_user_defined_type(entry);
    }
}

/*
 * This routine is called in gather_type_spec_information and its purpose is to
 * fill the simple_type with the proper reference of the user defined type.
 */
static void gather_type_spec_from_simple_type_specifier(AST a, type_t** type_info,
        gather_decl_spec_t* gather_info,
        decl_context_t decl_context)
{
    AST id_expression = ASTSon0(a);
    decl_flags_t flags = DF_IGNORE_FRIEND_DECL;
    scope_entry_list_t* entry_list = query_id_expression_flags(decl_context, id_expression, NULL, flags);

    common_gather_type_spec_from_simple_type_specifier(a, decl_context, type_info, gather_info, entry_list);
}

static void nodecl_gather_type_spec_from_simple_type_specifier(nodecl_t a, type_t** type_info,
        gather_decl_spec_t* gather_info, decl_context_t decl_context)
{
    decl_flags_t flags = DF_IGNORE_FRIEND_DECL;
    scope_entry_list_t* entry_list = query_nodecl_name_flags(decl_context, a, NULL, flags);

    common_gather_type_spec_from_simple_type_specifier(nodecl_get_ast(a), decl_context, type_info, gather_info, entry_list);
}

type_t* compute_underlying_type_enum(
        const_value_t* min_value,
        const_value_t* max_value,
        type_t* underlying_type,
        char short_enums)
{
    if (is_dependent_type(underlying_type)
            || is_error_type(underlying_type))
        return underlying_type;

    struct checked_types_t
    {
        type_t *signed_type;
        /* type_t *unsigned_type ; */
    }
    checked_types[] =
    {
        { get_signed_char_type(),          /* get_unsigned_char_type() */ },
        { get_signed_short_int_type(),     /* get_unsigned_short_int_type() */  },
        { get_signed_int_type(),           /* get_unsigned_int_type() */ },
        { get_signed_long_int_type(),      /* get_unsigned_long_int_type() */ },
        { get_signed_long_long_int_type(), /* get_unsigned_long_long_int_type() */ },
        { NULL /* , NULL */ }
    };

#define B_(x) const_value_is_nonzero(x)

    struct checked_types_t* result = NULL;
    if (!short_enums)
    {
        result = &(checked_types[2]); // {int, unsigned int}
    }
    else
    {
        result = checked_types;
    }

    while (result->signed_type != NULL)
    {
        const_value_t* min_int = NULL;
        const_value_t* max_int = NULL;
#if 0
        min_int = integer_type_get_minimum(result->unsigned_type);
        max_int = integer_type_get_maximum(result->unsigned_type);

        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: Checking enum values range '%s..%s' with range '%s..%s' of %s\n",
                    const_value_to_str(min_value),
                    const_value_to_str(max_value),
                    const_value_to_str(min_int),
                    const_value_to_str(max_int),
                    print_declarator(result->signed_type));
        }

        if (B_(const_value_lte(min_int,
                        const_value_cast_as_another(min_value, min_int)))
                && B_(const_value_lte(
                        const_value_cast_as_another(max_value, max_int),
                        max_int)))
        {
            return result->unsigned_type;
        }
#endif

        min_int = integer_type_get_minimum(result->signed_type);
        max_int = integer_type_get_maximum(result->signed_type);

        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: Checking enum values range '%s..%s' with range '%s..%s' of %s\n",
                    const_value_to_str(min_value),
                    const_value_to_str(max_value),
                    const_value_to_str(min_int),
                    const_value_to_str(max_int),
                    print_declarator(result->signed_type));
        }

        if (B_(const_value_lte(min_int,
                        const_value_cast_as_another(min_value, min_int)))
                && B_(const_value_lte(
                        const_value_cast_as_another(max_value, max_int),
                        max_int)))
        {
            return result->signed_type;
        }
        result++;
    }

#undef B_
    internal_error("Cannot come up with a wide enough integer type for range %s..%s\n",
            const_value_to_str(min_value),
            const_value_to_str(max_value));
}

/*
 * This function is called for enum specifiers. It saves all enumerated values
 * and if it has been given a name, it is registered in the scope.
 */
void gather_type_spec_from_enum_specifier(AST a, type_t** type_info,
        gather_decl_spec_t* gather_info,
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    AST enum_head = ASTSon0(a);
    AST enumeration_list = ASTSon1(a);

    AST enum_key = ASTSon0(enum_head);
    AST enum_attribute_specifier = ASTSon1(enum_head);
    AST enum_name = ASTSon2(enum_head);
    AST enum_base = ASTSon3(enum_head);

    char enum_is_scoped = ASTType(enum_key) == AST_SCOPED_ENUM_KEY;

    if(IS_CXX03_LANGUAGE
            && enum_is_scoped)
    {
        warn_printf("%s: warning: scoped enumerators are only valid in C++11\n", ast_location(enum_key));
    }

    gather_cxx11_attributes(enum_attribute_specifier, gather_info);

    scope_entry_t* new_enum = NULL;

    // If it has name, we register this type name in the symbol table
    // but only if it has not been declared previously
    if (enum_name != NULL)
    {
        const char* enum_name_str = ASTText(enum_name);

        C_LANGUAGE()
        {
            enum_name_str = strappend("enum ", enum_name_str);
        }

        scope_entry_list_t* enum_entry_list = query_in_scope_str(decl_context, enum_name_str, NULL);

        if (enum_entry_list != NULL
                && entry_list_size(enum_entry_list) == 1
                && entry_list_head(enum_entry_list)->kind == SK_ENUM)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "BUILDSCOPE: Enum '%s' already declared\n", enum_name_str);
            }

            new_enum = entry_list_head(enum_entry_list);

            entry_list_free(enum_entry_list);
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "BUILDSCOPE: Registering enum '%s' in '%p'\n", enum_name_str, decl_context.current_scope);
            }

            new_enum = new_symbol(decl_context, decl_context.current_scope, enum_name_str);
            new_enum->locus = ast_get_locus(enum_name);
            new_enum->kind = SK_ENUM;
            new_enum->type_information = get_new_enum_type(decl_context, enum_is_scoped);
            new_enum->entity_specs.is_user_declared = 1;
        }

        gather_info->defined_type = new_enum;
    }
    else
    {
        // Give it a fake name
        static int anonymous_enums = 0;
        const char* symbol_name;
        C_LANGUAGE()
        {
            uniquestr_sprintf(&symbol_name, "enum mcc_enum_anon_%d", anonymous_enums);
            new_enum = new_symbol(decl_context, decl_context.current_scope, symbol_name);
        }
        CXX_LANGUAGE()
        {
            uniquestr_sprintf(&symbol_name, "mcc_enum_anon_%d", anonymous_enums);
            new_enum = counted_xcalloc(1, sizeof(*new_enum), &_bytes_used_buildscope);
            new_enum->symbol_name = symbol_name;
            new_enum->decl_context = decl_context;
        }
        anonymous_enums++;

        new_enum->locus = ast_get_locus(a);
        new_enum->kind = SK_ENUM;
        new_enum->type_information = get_new_enum_type(decl_context, enum_is_scoped);

        new_enum->entity_specs.is_unnamed = 1;
        new_enum->entity_specs.is_user_declared = 1;
    }

    if (decl_context.current_scope->kind == CLASS_SCOPE)
    {
        scope_entry_t* class_symbol = decl_context.current_scope->related_entry;
        type_t* class_type = class_symbol->type_information;
        class_type_add_member(get_actual_class_type(class_type), new_enum,
                /* is_definition */ 1);
        CXX_LANGUAGE()
        {
            new_enum->entity_specs.is_member = 1;
            new_enum->entity_specs.access = gather_info->current_access;
            new_enum->entity_specs.class_type = get_user_defined_type(class_symbol);
            new_enum->entity_specs.is_defined_inside_class_specifier = 1;

            set_is_dependent_type(new_enum->type_information,
                    is_dependent_type(class_type));
        }
    }

    type_t* enum_type = new_enum->type_information;

    char short_enums = CURRENT_CONFIGURATION->code_shape.short_enums;
    type_t* underlying_type = get_signed_int_type();
    char underlying_type_is_fixed = 0;

    if (enum_base != NULL)
    {
        if (IS_CXX03_LANGUAGE)
        {
            warn_printf("%s: warning: enum-base is only valid in C++11\n",
                    ast_location(a));
        }

        underlying_type_is_fixed = 1;
        underlying_type = compute_type_for_type_id_tree(enum_base, decl_context, NULL, NULL);

        if (is_error_type(underlying_type))
        {
            *type_info = get_error_type();
            return;
        }
    }
    else if (enum_is_scoped)
    {
        underlying_type = get_signed_int_type();
        underlying_type_is_fixed = 1;
    }

    if (underlying_type_is_fixed)
    {
        if (enum_type_get_underlying_type_is_fixed(new_enum->type_information)
                && !equivalent_types(
                    enum_type_get_underlying_type(new_enum->type_information),
                    underlying_type))
        {
            error_printf("%s: error: enumerator previously declared with different underlying type\n",
                    ast_location(a));
            *type_info = get_error_type();
            return;
        }
    }

    new_enum->defined = 1;
    // Since this type is not anonymous we'll want that type_info
    // refers to this newly created type
    *type_info = get_user_defined_type(new_enum);

    if (enumeration_list != NULL)
    {
        decl_context_t enumerators_context = decl_context;

        C_LANGUAGE()
        {
            if (enumerators_context.current_scope->kind == CLASS_SCOPE)
            {
                // Switch to the enclosing NAMESPACE scope
                enumerators_context.current_scope = enumerators_context.namespace_scope;
            }
        }

        CXX11_LANGUAGE()
        {
            // In C++11 we always create a class context
            // We will later add the enumerator in the enclosing scope if the
            // enumerator is unscoped
            enumerators_context = new_class_context(decl_context, new_enum);
            new_enum->related_decl_context = enumerators_context;
        }

        int num_enumerator = 0;

        // Delta value between uninitialized enumerators
        int delta = 0;
        // Latest initialized enumerator
        nodecl_t base_enumerator = nodecl_null();

        const_value_t* min_value = NULL;
        const_value_t* max_value = NULL;

        // For every enumeration, sign them up in the symbol table
        AST iter = NULL;
        for_each_element(enumeration_list, iter)
        {
            AST enumeration = ASTSon1(iter);
            AST enumeration_name = ASTSon0(enumeration);
            AST enumeration_expr = ASTSon1(enumeration);

            scope_entry_t* enumeration_item = new_symbol(enumerators_context,
                    enumerators_context.current_scope, ASTText(enumeration_name));
            enumeration_item->locus = ast_get_locus(enumeration_name);
            enumeration_item->kind = SK_ENUMERATOR;
            enumeration_item->type_information = get_signed_int_type();

            CXX03_LANGUAGE()
            {
                if (decl_context.current_scope->kind == CLASS_SCOPE)
                {
                    scope_entry_t* enclosing_class_symbol = decl_context.current_scope->related_entry;
                    type_t* enclosing_class_type = enclosing_class_symbol->type_information;
                    class_type_add_member(get_actual_class_type(enclosing_class_type), enumeration_item,
                            /* is_definition */ 1);

                    enumeration_item->entity_specs.is_member = 1;
                    enumeration_item->entity_specs.access = gather_info->current_access;
                    enumeration_item->entity_specs.is_defined_inside_class_specifier = 1;
                    enumeration_item->entity_specs.class_type  = get_user_defined_type(enclosing_class_symbol);
                }
            }

            CXX11_LANGUAGE()
            {
                if (ASTType(enum_key) == AST_UNSCOPED_ENUM_KEY)
                {
                    // Insert entry in the enclosing scope
                    // (note that in C and C++2003 we are already registering
                    // them in the enclosing scope)
                    insert_entry(decl_context.current_scope, enumeration_item);

                    if (decl_context.current_scope->kind == CLASS_SCOPE)
                    {
                        scope_entry_t* enclosing_class_symbol = decl_context.current_scope->related_entry;
                        type_t* enclosing_class_type = enclosing_class_symbol->type_information;
                        class_type_add_member(get_actual_class_type(enclosing_class_type), enumeration_item,
                                /* is_definition */ 1);

                        enumeration_item->entity_specs.is_member = 1;
                        enumeration_item->entity_specs.access = gather_info->current_access;
                        enumeration_item->entity_specs.is_defined_inside_class_specifier = 1;
                        enumeration_item->entity_specs.class_type  = get_user_defined_type(enclosing_class_symbol);
                    }
                }
            }

            if (enumeration_expr != NULL)
            {
                nodecl_t nodecl_expr = nodecl_null();
                if (!check_expression(enumeration_expr, enumerators_context, &nodecl_expr))
                {
                    error_printf("%s: error: invalid enumerator expression '%s'\n",
                            ast_location(enumeration_expr),
                            prettyprint_in_buffer(enumeration_expr));
                    if (!underlying_type_is_fixed)
                        underlying_type = get_error_type();
                }
                else
                {
                    if (nodecl_is_constant(nodecl_expr))
                    {
                        enumeration_item->type_information = nodecl_get_type(nodecl_expr);
                    }
                    else if (IS_CXX_LANGUAGE
                            && nodecl_expr_is_value_dependent(nodecl_expr))
                    {
                        if (nodecl_expr_is_type_dependent(nodecl_expr))
                        {
                            if (!underlying_type_is_fixed)
                                underlying_type = get_unknown_dependent_type();
                        }
                        enumeration_item->type_information = nodecl_get_type(nodecl_expr);
                    }
                    else
                    {
                        error_printf("%s: error: enumerator expression '%s' is not constant\n",
                                ast_location(enumeration_expr),
                                prettyprint_in_buffer(enumeration_expr));
                        if (!underlying_type_is_fixed)
                            underlying_type = get_error_type();
                    }
                }

                enumeration_item->value = nodecl_expr;

                delta = 1;
                base_enumerator = nodecl_expr;
            }
            else
            {
                if (num_enumerator == 0)
                {
                    const_value_t* zero_val = const_value_get_signed_int(0);
                    nodecl_t zero = const_value_to_nodecl(zero_val);

                    enumeration_item->value = zero;
                    base_enumerator = zero;
                    delta = 1;
                }
                else
                {
                    if (nodecl_is_constant(base_enumerator))
                    {
                        const_value_t* base_const = 
                            nodecl_get_constant(base_enumerator);
                        const_value_t* val_plus_one =
                            const_value_cast_to_bytes(
                                    const_value_add(
                                        base_const,
                                        const_value_get_signed_int(delta)),
                                    const_value_get_bytes(base_const),
                                    const_value_is_signed(base_const));

                        enumeration_item->value = const_value_to_nodecl_with_basic_type(val_plus_one,
                                no_ref(nodecl_get_type(base_enumerator)));
                        enumeration_item->type_information = nodecl_get_type(enumeration_item->value);
                    }
                    else if (IS_CXX_LANGUAGE
                            && nodecl_expr_is_value_dependent(base_enumerator))
                    {
                        nodecl_t add_one = nodecl_make_add(
                                nodecl_shallow_copy(base_enumerator),
                                const_value_to_nodecl(const_value_get_signed_int(delta)),
                                nodecl_get_type(base_enumerator),
                                nodecl_get_locus(base_enumerator));
                        nodecl_expr_set_is_value_dependent(add_one, 1);

                        enumeration_item->value = add_one;

                        enumeration_item->type_information = nodecl_get_type(base_enumerator);
                    }
                    else
                    {
                        // If the base was not constant it means there was an
                        // error, use the erroneous tree
                        enumeration_item->value = base_enumerator;
                    }

                    delta++;
                }
            }

            DEBUG_CODE()
            {
                if (nodecl_is_constant(enumeration_item->value))
                {
                    fprintf(stderr, "BUILDSCOPE: Registering enumerator '%s' with constant value '%lld' and type '%s'\n", ASTText(enumeration_name),
                            (long long int)const_value_cast_to_8(nodecl_get_constant(enumeration_item->value)),
                            print_declarator(enumeration_item->type_information));
                }
                else
                {
                    fprintf(stderr, "BUILDSCOPE: Registering enumerator '%s' with value '%s' and type '%s'\n", ASTText(enumeration_name),
                            codegen_to_str(enumeration_item->value, decl_context),
                            print_declarator(enumeration_item->type_information));
                }
            }

            enum_type_add_enumerator(enum_type, enumeration_item);
            num_enumerator++;

            if (!underlying_type_is_fixed)
            {
#define B_(x) const_value_is_nonzero(x)
                if (nodecl_is_constant(enumeration_item->value)
                        && !is_dependent_type(underlying_type))
                {
                    const_value_t* current_value = nodecl_get_constant(enumeration_item->value);

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

                if (min_value == NULL)
                    min_value = const_value_get_unsigned_int(0);
                if (max_value == NULL)
                    max_value = const_value_get_unsigned_int(0);

                underlying_type = compute_underlying_type_enum(min_value, max_value, underlying_type, short_enums);
            }
        }


        // Now set the type of all the enumerators to be of the enumerator
        // type
        int i;
        for (i = 0; i < enum_type_get_num_enumerators(enum_type); i++)
        {
            scope_entry_t* enumerator = enum_type_get_enumerator_num(enum_type, i);
            enumerator->type_information = *type_info;
        }
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "BUILDSCOPE: Underlying type for '%s' computed to '%s'\n", 
                print_declarator(enum_type),
                print_declarator(underlying_type));
    }

    enum_type_set_underlying_type(enum_type, underlying_type);
    enum_type_set_underlying_type_is_fixed(new_enum->type_information, underlying_type_is_fixed);

    set_is_complete_type(enum_type, /* is_complete */ 1);

    CXX_LANGUAGE()
    {
        *nodecl_output =
            nodecl_concat_lists(
                    *nodecl_output,
                    nodecl_make_list_1(
                        nodecl_make_cxx_def(
                            /* optative context */ nodecl_null(),
                            new_enum,
                            ast_get_locus(a))));
    }
}

static void build_scope_base_clause(AST base_clause, scope_entry_t* class_entry, decl_context_t decl_context)
{
    type_t* class_type = get_actual_class_type(class_entry->type_information);

    if (class_type_get_class_kind(class_type) == TT_UNION)
    {
        error_printf("%s: a union cannot have bases\n", ast_location(base_clause));
        return;
    }

    AST list = ASTSon0(base_clause);
    AST iter;
    for_each_element(list, iter)
    {
        AST base_specifier = ASTSon1(iter);

        char is_expansion = 0;
        if (ASTType(base_specifier) == AST_BASE_SPEC_PACK_EXPANSION)
        {
            is_expansion = 1;
            base_specifier = ASTSon0(base_specifier);
        }

        AST virtual_spec = ASTSon0(base_specifier);
        AST access_spec_tree = ASTSon1(base_specifier);
        AST class_name = ASTSon2(base_specifier);

        access_specifier_t access_specifier = AS_UNKNOWN;

        switch (class_type_get_class_kind(class_type))
        {
            case TT_CLASS :
                {
                    access_specifier = AS_PRIVATE;
                    break;
                }
            case TT_STRUCT :
                {
                    access_specifier = AS_PUBLIC;
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
            SK_TEMPLATE_ALIAS,
            SK_TEMPLATE_TYPE_PARAMETER,
            SK_TEMPLATE_TEMPLATE_PARAMETER, // ???
            SK_TEMPLATE_TYPE_PARAMETER_PACK,
            SK_TEMPLATE_TEMPLATE_PARAMETER_PACK, // ???
            SK_TYPEDEF,
            SK_DEPENDENT_ENTITY,
            SK_USING,
            SK_USING_TYPENAME,
        };

        // We do not want to examine uninstantiated typenames
        scope_entry_list_t* result_list = query_id_expression_flags(decl_context, 
                class_name, NULL, DF_DEPENDENT_TYPENAME);

        scope_entry_list_t* filtered_result_list = filter_symbol_kind_set(result_list, STATIC_ARRAY_LENGTH(filter), filter);

        entry_list_free(result_list);

        if (filtered_result_list == NULL)
        {
            error_printf("%s: error: base class '%s' not found\n", 
                    ast_location(class_name),
                    prettyprint_in_buffer(class_name));
            continue;
        }

        scope_entry_t* result = entry_list_head(filtered_result_list);
        entry_list_free(filtered_result_list);

        if (result->kind == SK_USING
                || result->kind == SK_USING_TYPENAME)
        {
            result = entry_advance_aliases(result);
        }

        if (result->kind != SK_TEMPLATE_TYPE_PARAMETER
                && result->kind != SK_TEMPLATE_TEMPLATE_PARAMETER // ???
                && !is_dependent_type(result->type_information)
                && (result->kind == SK_CLASS
                    || result->kind == SK_TYPEDEF))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "BUILDSCOPE: Base class '%s' IS NOT a dependent type\n", prettyprint_in_buffer(base_specifier));
            }

            scope_entry_t* base_class_symbol = result;
            // Update symbol because it might have been a typedef
            if (base_class_symbol->kind == SK_TYPEDEF)
            {
                base_class_symbol = named_type_get_symbol(
                        advance_over_typedefs(base_class_symbol->type_information)
                        );
            }

            // If the entity (being an independent one) has not been completed, then instantiate it
            instantiate_template_class_if_needed(base_class_symbol, decl_context, ast_get_locus(base_specifier));

            result = base_class_symbol;
        }
        else if (result->kind == SK_TEMPLATE_TEMPLATE_PARAMETER // ???
                || result->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK // ???
                || result->kind == SK_TEMPLATE_TYPE_PARAMETER
                || result->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK
                || is_dependent_type(result->type_information))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "BUILDSCOPE: Base class '%s' found IS a dependent type\n", prettyprint_in_buffer(base_specifier));
            }
            is_dependent = 1;

#if 0
            scope_entry_t* enclosing_class = NULL;
            if (class_entry->decl_context.current_scope->kind == CLASS_SCOPE)
            {
                enclosing_class = class_entry->decl_context.current_scope->related_entry;
            }

            if (result->kind != SK_DEPENDENT_ENTITY
                    && enclosing_class != NULL
                    && symbol_is_member_of_dependent_class(result))
            {
                // Craft a nodecl name for it
                nodecl_t nodecl_simple_name = nodecl_make_cxx_dep_name_simple(
                        result->symbol_name,
                        ast_get_locus(class_name));

                nodecl_t nodecl_name = nodecl_simple_name;

                if (is_template_specialized_type(result->type_information))
                {
                    nodecl_name = nodecl_make_cxx_dep_template_id(
                            nodecl_name,
                            // If our enclosing class is dependent
                            // this template id will require a 'template '
                            "template ",
                            template_specialized_type_get_template_arguments(result->type_information),
                            ast_get_locus(class_name));
                }

                // Craft a dependent typename since we will need it later for proper updates
                scope_entry_t* new_sym = counted_xcalloc(1, sizeof(*new_sym), &_bytes_used_buildscope);
                new_sym->kind = SK_DEPENDENT_ENTITY;
                new_sym->locus = nodecl_get_locus(nodecl_name);
                new_sym->symbol_name = result->symbol_name;
                new_sym->decl_context = decl_context;
                new_sym->type_information = build_dependent_typename_for_entry(
                        enclosing_class,
                        nodecl_name,
                        nodecl_get_locus(nodecl_name));

                result = new_sym;
            }
#endif
        }
        else
        {
            error_printf("%s: error: invalid class name '%s'\n",
                    ast_location(class_name),
                    prettyprint_in_buffer(class_name));
            continue;
        }

        // Add the base to the class type
        class_type_add_base_class(get_actual_class_type(class_type),
                result, is_virtual, is_dependent, is_expansion, access_specifier);
    }
}

static char class_has_const_copy_assignment_operator(type_t* t)
{
    scope_entry_list_t* copy_assignment_ops = class_type_get_copy_assignment_operators(t);
    ERROR_CONDITION(copy_assignment_ops == NULL, 
            "%s Bad class type", print_type_str(t, CURRENT_COMPILED_FILE->global_decl_context));

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
        if (is_lvalue_reference_type(first_param_type)
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

    ERROR_CONDITION(copy_constructors == NULL,
            "%s Bad class type", print_type_str(t, CURRENT_COMPILED_FILE->global_decl_context));

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

        if (is_lvalue_reference_type(first_param_type)
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

static char is_nested_in_class(type_t* class_of_entry, type_t* class_of_constructor)
{
    if (equivalent_types(class_of_entry, class_of_constructor))
    {
        return 1;
    }
    else
    {
        scope_entry_t* class_sym_of_entry = named_type_get_symbol(class_of_entry);

        if (class_sym_of_entry->entity_specs.is_member)
        {
            return is_nested_in_class(class_sym_of_entry->entity_specs.class_type, 
                    class_of_constructor);
        }
        else
        {
            return 0;
        }
    }
}

typedef
enum nesting_check_tag
{
    NESTING_CHECK_OK,
    NESTING_CHECK_INVALID,
    NESTING_CHECK_NOT_A_TEMPLATE,
} nesting_check_t;

static nesting_check_t check_template_nesting_of_name(scope_entry_t* entry, template_parameter_list_t* template_parameters)
{
    // Do not pass SK_TEMPLATE here
    ERROR_CONDITION(entry->kind != SK_CLASS
            && entry->kind != SK_FUNCTION, "Invalid symbol", 0);

    if (is_template_specialized_type(entry->type_information))
    {
        if (is_dependent_type(entry->type_information)
                || !entry->entity_specs.is_user_declared)
        {
            /*
             * We do this check for dependent types which will obviously
             * require some template header. But note that user-defined
             * explicit specializations are not dependent and their
             * members cannot have template headers.
             *
             * a)
             *
             * template <typename T>
             * struct A
             * {
             *   template <typename Q>
             *   void f(T, Q);
             * };
             *
             * template <>
             * template <>
             * void A<int>::f(int, float)
             * {
             * }
             *
             * b)
             *
             * template <typename T>
             * struct A
             * {
             *   template <typename Q>
             *   void f(T, Q);
             * };
             *
             * template <>
             * struct A<int>
             * {
             *   template <typename Q>
             *   void f(int, Q);
             * };
             *
             * template <>
             * void A<int>::f(int, float)
             * {
             * }
             *
             * c) but note that
             *
             * template <typename T>
             * struct A;
             *
             * template <>
             * struct A<int> { void f(); };
             *
             * // a template header would be an error here
             * void A<int>::f() { };
             *
             */

            if (template_parameters == NULL ||
                    (!template_parameters->is_explicit_specialization &&
                        template_parameters->num_parameters == 0))
            {
                return NESTING_CHECK_INVALID;
            }

            if (entry->entity_specs.is_member)
            {
                template_parameter_list_t* current_template_args = template_specialized_type_get_template_arguments(
                        entry->type_information);

                if (entry->kind == SK_CLASS
                        && current_template_args->num_parameters == 0)
                {
                    // This is a non-template member class, which looks like a
                    // specialized template type with zero template arguments.
                    // Handle it as if this were not an explicit specialization
                    //
                    // In the example below, when B is instantiated it will be like a template
                    // but with "zero" parameters. We do this because C++ standard makes these
                    // classes behave much in this way.
                    //
                    // template <typename T>
                    // struct A
                    // {
                    //   struct B
                    //   {
                    //      template <typename Q>
                    //      void f(T, Q);
                    //   };
                    // };
                    //
                    // template <>
                    // template <>
                    // void A<int>::B::f(int, float) { }
                    //
                    return check_template_nesting_of_name(named_type_get_symbol(entry->entity_specs.class_type),
                            template_parameters);
                }
                else
                {
                    return check_template_nesting_of_name(named_type_get_symbol(entry->entity_specs.class_type),
                            template_parameters->enclosing);
                }
            }
            else
            {
                // If this is not a member, but there is still another level of template declarations, there is something amiss
                if (template_parameters->enclosing != NULL)
                {
                    return NESTING_CHECK_INVALID;
                }
            }
            return NESTING_CHECK_OK;
        }
    }
    else
    {
        if (entry->entity_specs.is_member)
        {
            return check_template_nesting_of_name(named_type_get_symbol(entry->entity_specs.class_type), template_parameters);
        }

        if (template_parameters != NULL)
        {
            // Local classes cannot have template parameters but may inherit them from an enclosing declaration
            //
            // template <typename T>
            // void f(T)
            // {
            //   struct A
            //   {
            //      void f(T) { } // Here f(T)::A::f(T) inherits the enclosing template parameters
            //   };
            // }
            if (entry->kind == SK_CLASS
                    && entry->decl_context.current_scope->kind == BLOCK_SCOPE)
                return NESTING_CHECK_OK;

            return NESTING_CHECK_NOT_A_TEMPLATE;
        }
    }

    return NESTING_CHECK_OK;
}

static void build_scope_ctor_initializer(
        AST ctor_initializer, 
        scope_entry_t* function_entry,
        decl_context_t decl_context,
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    decl_context.decl_flags = DF_NONE;
    scope_entry_t* class_sym = named_type_get_symbol(function_entry->entity_specs.class_type);

    char is_dependent_context =  (is_dependent_type(class_sym->type_information)
            || is_dependent_type(function_entry->type_information));

    scope_entry_list_t* virtual_bases = NULL;
    scope_entry_list_t* direct_base_classes = NULL;
    scope_entry_list_t* nonstatic_data_members = NULL;

    if (!is_dependent_context)
    {
        virtual_bases =
            class_type_get_virtual_base_classes_canonical(class_sym->type_information);
        direct_base_classes =
            class_type_get_direct_base_classes_canonical(class_sym->type_information);
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

            ERROR_CONDITION(ASTType(mem_initializer) != AST_MEM_INITIALIZER, "Invalid tree", 0);

            AST mem_initializer_id = ASTSon0(mem_initializer);
            AST id_expression = ASTSon0(mem_initializer_id);

            AST initializer = ASTSon1(mem_initializer);

            if (is_dependent_context)
            {
                nodecl_t nodecl_name = nodecl_null();
                nodecl_t nodecl_init = nodecl_null();

                compute_nodecl_name_from_id_expression(id_expression, decl_context, &nodecl_name);
                check_initialization(initializer,
                        decl_context,
                        NULL, /* We do not really know what is being initialized */
                        get_unknown_dependent_type(),
                        &nodecl_init,
                        /* is_auto_type */ 0);

                nodecl_t nodecl_cxx_init = nodecl_make_cxx_member_init(
                        nodecl_name, nodecl_init,
                        get_unknown_dependent_type(),
                        ast_get_locus(mem_initializer_id));

                *nodecl_output = nodecl_append_to_list(*nodecl_output, nodecl_cxx_init);
                continue;
            }


            scope_entry_list_t* result_list = NULL;
            decl_context_t class_context = class_type_get_inner_context(class_sym->type_information);
            class_context.template_parameters = decl_context.template_parameters;
            result_list = query_id_expression(class_context, id_expression, NULL);

            if (result_list == NULL)
            {
                error_printf("%s: initialized entity '%s' not found\n", 
                        ast_location(id_expression),
                        prettyprint_in_buffer(id_expression));
                continue;
            }

            scope_entry_t* entry = entry_list_head(result_list);
            entry_list_free(result_list);

            nodecl_t nodecl_init = nodecl_null();

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
                error_printf("%s: error: '%s' initialized twice in member initializer list\n",
                        ast_location(id_expression),
                        get_qualified_symbol_name(entry, entry->decl_context));
                continue;
            }

            if (entry->kind == SK_VARIABLE)
            {
                if (!entry_list_contains(nonstatic_data_members, entry))
                {
                    if (!entry->entity_specs.is_member
                            || !is_nested_in_class(entry->entity_specs.class_type, function_entry->entity_specs.class_type))
                    {
                        error_printf("%s: symbol '%s' is not a member of class %s\n",
                                ast_location(id_expression),
                                get_qualified_symbol_name(entry, entry->decl_context),
                                get_qualified_symbol_name(named_type_get_symbol(function_entry->entity_specs.class_type), 
                                    function_entry->decl_context));
                        continue;
                    }
                    if (entry->entity_specs.is_static)
                    {
                        error_printf("%s: static data member '%s' cannot be initialized here\n", 
                                ast_location(id_expression),
                                prettyprint_in_buffer(id_expression));
                        continue;
                    }
                }

                check_initialization(initializer,
                        decl_context,
                        entry,
                        get_unqualified_type(entry->type_information),
                        &nodecl_init,
                        /* is_auto_type */ 0);

                already_initialized = entry_list_add(already_initialized, entry);
            }
            else if (entry->kind == SK_CLASS)
            {
                if (!entry_list_contains(direct_base_classes, class_symbol_get_canonical_symbol(entry))
                        && !entry_list_contains(virtual_bases, class_symbol_get_canonical_symbol(entry)))
                {
                    error_printf("%s: error: class '%s' is not a direct base or virtual base of class '%s'\n",
                            ast_location(id_expression),
                            get_qualified_symbol_name(entry, entry->decl_context),
                            get_qualified_symbol_name(class_sym, class_sym->decl_context));
                }

                check_initialization(initializer,
                        decl_context,
                        entry,
                        get_unqualified_type(get_user_defined_type(entry)),
                        &nodecl_init,
                        /* is_auto_type */ 0);

                already_initialized = entry_list_add(already_initialized, class_symbol_get_canonical_symbol(entry));
            }
            else
            {
                error_printf("%s: symbol '%s' cannot be initialized here\n",
                        ast_location(id_expression),
                        get_qualified_symbol_name(entry, entry->decl_context));
                break;
            }

            nodecl_t nodecl_object_init = nodecl_make_member_init(
                    nodecl_init,
                    entry,
                    ast_get_locus(id_expression));

            *nodecl_output = nodecl_append_to_list(*nodecl_output, nodecl_object_init);
        }
    }

    if (is_dependent_context)
        return;

    // Now review the remaining objects not initialized yet
    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(virtual_bases);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        if (entry->kind == SK_CLASS)
            entry = class_symbol_get_canonical_symbol(entry);

        if (entry_list_contains(already_initialized, entry))
            continue;

        scope_entry_t* constructor = NULL;
        char valid = check_default_initialization(entry, entry->decl_context, locus, &constructor);

        if (valid)
        {
            nodecl_t nodecl_call_to_ctor = cxx_nodecl_make_function_call(
                    nodecl_make_symbol(constructor, locus),
                    /* called_name */ nodecl_null(),
                    /* args */ nodecl_null(),
                    nodecl_make_cxx_function_form_implicit(locus),
                    constructor->entity_specs.class_type,
                    decl_context,
                    locus);

            nodecl_t nodecl_object_init = nodecl_make_implicit_member_init(
                    nodecl_call_to_ctor,
                    entry,
                    locus);
            *nodecl_output = nodecl_append_to_list(*nodecl_output, nodecl_object_init);
        }
    }
    entry_list_iterator_free(it);

    for (it = entry_list_iterator_begin(direct_base_classes);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        if (entry->kind == SK_CLASS)
            entry = class_symbol_get_canonical_symbol(entry);

        if (entry_list_contains(already_initialized, entry))
            continue;

        scope_entry_t* constructor = NULL;
        char valid = check_default_initialization(entry, entry->decl_context, locus, &constructor);

        if (valid)
        {
            nodecl_t nodecl_call_to_ctor = cxx_nodecl_make_function_call(
                    nodecl_make_symbol(constructor, locus),
                    /* called_name */ nodecl_null(),
                    /* args */ nodecl_null(),
                    nodecl_make_cxx_function_form_implicit(locus),
                    constructor->entity_specs.class_type,
                    decl_context,
                    locus);

            nodecl_t nodecl_object_init = nodecl_make_implicit_member_init(
                    nodecl_call_to_ctor,
                    entry,
                    locus);
            *nodecl_output = nodecl_append_to_list(*nodecl_output, nodecl_object_init);
        }
    }
    entry_list_iterator_free(it);

    for (it = entry_list_iterator_begin(nonstatic_data_members);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);
        if (entry_list_contains(already_initialized, entry))
            continue;

        scope_entry_t* constructor = NULL;
        char valid = check_default_initialization(entry, entry->decl_context, locus, &constructor);

        if (valid)
        {
            type_t* t = entry->type_information;

            if (is_array_type(t))
                t = array_type_get_element_type(t);

            if (is_pod_type(t))
            {
                // No initialization for POD-types
            }
            else if (is_class_type(t))
            {
                nodecl_t nodecl_call_to_ctor = cxx_nodecl_make_function_call(
                        nodecl_make_symbol(constructor, locus),
                        /* called_name */ nodecl_null(),
                        /* args */ nodecl_null(),
                        nodecl_make_cxx_function_form_implicit(locus),
                        constructor->entity_specs.class_type,
                        decl_context,
                        locus);

                nodecl_t nodecl_object_init = nodecl_make_implicit_member_init(
                        nodecl_call_to_ctor,
                        entry,
                        locus);
                *nodecl_output = nodecl_append_to_list(*nodecl_output, nodecl_object_init);
            }
            else
            {
                internal_error("Code unreachable", 0);
            }
        }
    }
    entry_list_iterator_free(it);
}

#if 0
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
#endif

#if 0
struct check_constructor_helper
{
    const char* filename;
    const locus_t* locus;
    char has_const;
};
#endif

#if 0
static void ensure_default_constructor_is_emitted(scope_entry_t* entry, void* data)
{
    ERROR_CONDITION(entry->kind != SK_CLASS && entry->kind != SK_VARIABLE, "Invalid symbol", 0);
    struct check_constructor_helper* p = (struct check_constructor_helper*)data;

    scope_entry_t* constructor = NULL;
    check_default_initialization(entry, entry->decl_context, p->locus, &constructor);
}
#endif

#if 0
static void ensure_copy_constructor_is_emitted(scope_entry_t* entry, void* data)
{
    ERROR_CONDITION(entry->kind != SK_CLASS && entry->kind != SK_VARIABLE , "Invalid symbol", 0);
    struct check_constructor_helper* p = (struct check_constructor_helper*)data;

    scope_entry_t* constructor = NULL;
    check_copy_constructor(entry, entry->decl_context, p->has_const,
            p->locus, &constructor);
}
#endif

#if 0
static void ensure_copy_assignment_operator_is_emitted(scope_entry_t* entry, void* data)
{
    ERROR_CONDITION(entry->kind != SK_CLASS && entry->kind != SK_VARIABLE, "Invalid symbol", 0);
    struct check_constructor_helper* p = (struct check_constructor_helper*)data;

    scope_entry_t* constructor = NULL;
    check_copy_assignment_operator(entry, entry->decl_context, p->has_const,
            p->locus, &constructor);
}
#endif

#if 0
static void ensure_move_assignment_operator_is_emitted(scope_entry_t* entry, void* data)
{
    ERROR_CONDITION(entry->kind != SK_CLASS && entry->kind != SK_VARIABLE, "Invalid symbol", 0);
    struct check_constructor_helper* p = (struct check_constructor_helper*)data;

    scope_entry_t* constructor = NULL;
    check_move_assignment_operator(entry, entry->decl_context, p->has_const,
            p->locus, &constructor);
}
#endif

#if 0
static void ensure_destructor_is_emitted(scope_entry_t* entry, void* data)
{
    ERROR_CONDITION(entry->kind != SK_CLASS && entry->kind != SK_VARIABLE, "Invalid symbol", 0);

    if (!is_class_type(entry->type_information))
        return;

    struct check_constructor_helper* p = (struct check_constructor_helper*)data;

    scope_entry_t* destructor = class_type_get_destructor(entry->type_information);
    ensure_function_is_emitted(destructor, p->locus);
    ERROR_CONDITION(destructor == NULL, "Bad class %s lacking destructor", 
            print_type_str(get_user_defined_type(entry), entry->decl_context));
}
#endif

// These functions emit special members that may have to be defined by the compiler itself
// they ensure that every other function that might have to be called will be emitted too
// (this implies calling other implicitly defined members)
//
// Currently they do not generate nodecl (we are unsure what we would do with it) but maybe
// they will have to in the future

#if 0
static void emit_implicit_default_constructor(scope_entry_t* entry, const locus_t* locus)
{
    struct check_constructor_helper l = { .locus = locus, .has_const = 0 };
    apply_function_to_data_layout_members(named_type_get_symbol(entry->entity_specs.class_type), 
            ensure_default_constructor_is_emitted, &l);
}
#endif

#if 0
static void emit_implicit_copy_constructor(scope_entry_t* entry,
        const locus_t* locus)
{
    char has_const = is_const_qualified_type(
            no_ref(function_type_get_parameter_type_num(entry->type_information, 0)));

    struct check_constructor_helper l = { .locus = locus, .has_const = has_const };
    apply_function_to_data_layout_members(named_type_get_symbol(entry->entity_specs.class_type), 
            ensure_copy_constructor_is_emitted, &l);
}
#endif

#if 0
static void emit_implicit_copy_assignment_operator(scope_entry_t* entry,
        const locus_t* locus)
{
    char has_const = is_const_qualified_type(
            no_ref(function_type_get_parameter_type_num(entry->type_information, 0)));

    struct check_constructor_helper l = { .locus = locus, .has_const = has_const };
    apply_function_to_data_layout_members(named_type_get_symbol(entry->entity_specs.class_type), 
            ensure_copy_assignment_operator_is_emitted, &l);
}
#endif

#if 0
static void emit_implicit_move_assignment_operator(scope_entry_t* entry,
        const locus_t* locus)
{
    char has_const = is_const_qualified_type(
            no_ref(function_type_get_parameter_type_num(entry->type_information, 0)));

    struct check_constructor_helper l = { .locus = locus, .has_const = has_const };
    apply_function_to_data_layout_members(named_type_get_symbol(entry->entity_specs.class_type), 
            ensure_move_assignment_operator_is_emitted, &l);
}
#endif

#if 0
static void emit_implicit_destructor(scope_entry_t* entry,
        const locus_t* locus)
{
    struct check_constructor_helper l = { .locus = locus, .has_const = 0 };
    apply_function_to_data_layout_members(named_type_get_symbol(entry->entity_specs.class_type), 
            ensure_destructor_is_emitted, &l);
}
#endif

static char name_is_accessible_from_context(scope_entry_t* entry UNUSED_PARAMETER,
        decl_context_t decl_context UNUSED_PARAMETER)
{
    // FIXME - Not properly implemented yet
    return 1;
}

static char one_function_is_usable(
        scope_entry_list_t* candidates,
        type_t* first_arg_type,
        type_t* second_arg_type,
        decl_context_t decl_context,
        const locus_t* locus)
{
    if (IS_CXX03_LANGUAGE)
        return 1;

    if (candidates == NULL)
        return 0;

    DEBUG_CODE()
    {
        fprintf(stderr, "BUILDSCOPE: Checking usability of functions\n");
    }

    int num_arguments = 0;
    if (first_arg_type != NULL)
        num_arguments++;
    if (second_arg_type != NULL)
        num_arguments++;
    ERROR_CONDITION(second_arg_type != NULL
            && first_arg_type == NULL,
            "First type cannot be NULL if second type is not NULL", 0);

    type_t* argument_types[] = { first_arg_type, second_arg_type };

    scope_entry_list_t* overload_set = unfold_and_mix_candidate_functions(
            candidates,
            NULL, &second_arg_type, second_arg_type != NULL ? 1 : 0,
            decl_context,
            locus, /* explicit_template_parameters */ NULL);

    candidate_t* candidate_set = NULL;

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(overload_set);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        candidate_set = candidate_set_add(candidate_set,
                entry_list_iterator_current(it),
                num_arguments,
                argument_types);
    }
    entry_list_iterator_free(it);

    scope_entry_t* augmented_conversors[num_arguments + 1];
    memset(augmented_conversors, 0, sizeof(augmented_conversors));

    scope_entry_t* overload_resolution = solve_overload(candidate_set,
            decl_context,
            locus, 
            augmented_conversors);

    if (overload_resolution == NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: No function was found to be usable\n");
        }
        return 0;
    }

    if (overload_resolution->entity_specs.is_deleted)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: Function '%s', cannot be called because it has been deleted\n",
                    print_decl_type_str(overload_resolution->type_information,
                        overload_resolution->decl_context,
                        get_qualified_symbol_name(overload_resolution, overload_resolution->decl_context)));
        }
        return 0;
    }

    if (!name_is_accessible_from_context(overload_resolution, decl_context))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: Function '%s', although callable, is not accessible\n",
                    print_decl_type_str(overload_resolution->type_information,
                        overload_resolution->decl_context,
                        get_qualified_symbol_name(overload_resolution, overload_resolution->decl_context)));
        }
        return 0;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "BUILDSCOPE: Function '%s' has been found to be usable\n",
                print_decl_type_str(overload_resolution->type_information,
                    overload_resolution->decl_context,
                    get_qualified_symbol_name(overload_resolution, overload_resolution->decl_context)));
    }

    return 1;
}

static char one_function_is_nontrivial(scope_entry_list_t* constructors)
{
    char found = 0;

    scope_entry_list_iterator_t* it;
    for (it = entry_list_iterator_begin(constructors);
            !entry_list_iterator_end(it) && !found;
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);
        found = !entry->entity_specs.is_trivial;
    }
    entry_list_iterator_free(it);

    return found;
}

static char is_class_type_or_array_thereof(type_t* t)
{
    return is_class_type(t)
        || (is_array_type(t) && is_class_type(array_type_get_element_type(t)));
}

static void default_constructor_determine_if_trivial(
        scope_entry_t* default_constructor,
        scope_entry_list_t* nonstatic_data_members,
        scope_entry_list_t* direct_base_classes,
        char has_virtual_bases,
        char has_virtual_functions)
{
    // If it was not deleted, figure out if it is trivial
    //
    // A constructor is trivial if it is an implicitly-declared default constructor and if:
    //  1. its class has no virtual functions and no virtual base classes
    //  2. no non-static data member of its class has a brace-or-equal-initializer
    //  3. all the direct base classes of its class have trivial constructors
    //  4. for all the nonstatic data members of its class that are of class type (or array thereof),
    //     each such class has a trivial constructor.
    //  Otherwise, the constructor is non-trivial.

    // 1.

    // 2.
    scope_entry_list_iterator_t* it = NULL;
    char has_nonstatic_data_member_with_initializer = 0;
    for (it = entry_list_iterator_begin(nonstatic_data_members);
            !entry_list_iterator_end(it)
            && !has_nonstatic_data_member_with_initializer;
            entry_list_iterator_next(it))
    {
        scope_entry_t *data_member = entry_list_iterator_current(it);
        has_nonstatic_data_member_with_initializer = !nodecl_is_null(data_member->value);
    }
    entry_list_iterator_free(it);

    // 3.
    char has_bases_with_non_trivial_constructors = 0;
    scope_entry_list_iterator_t* it0 = NULL;
    for (it0 = entry_list_iterator_begin(direct_base_classes);
            !entry_list_iterator_end(it0) && !has_bases_with_non_trivial_constructors;
            entry_list_iterator_next(it0))
    {
        scope_entry_t* base_class = entry_list_iterator_current(it0);

        type_t* base_class_type = get_actual_class_type(base_class->type_information);

        scope_entry_list_t* base_constructors = class_type_get_constructors(base_class_type);
        has_bases_with_non_trivial_constructors =
            one_function_is_nontrivial(base_constructors);
        entry_list_free(base_constructors);
    }
    entry_list_iterator_free(it0);

    // 4.
    char has_nonstatic_data_member_with_no_trivial_constructor = 0;
    for (it = entry_list_iterator_begin(nonstatic_data_members);
            !entry_list_iterator_end(it) 
            && !has_nonstatic_data_member_with_no_trivial_constructor;
            entry_list_iterator_next(it))
    {
        scope_entry_t *data_member = entry_list_iterator_current(it);

        if (is_class_type_or_array_thereof(data_member->type_information))
        {
            type_t* member_class_type = data_member->type_information;
            if (is_array_type(data_member->type_information))
                member_class_type = array_type_get_element_type(member_class_type);

            type_t* member_actual_class_type = get_actual_class_type(member_class_type);

            scope_entry_t* current_default_constructor
                = class_type_get_default_constructor(member_actual_class_type);

            has_nonstatic_data_member_with_no_trivial_constructor
                |= !(current_default_constructor != NULL &&
                        current_default_constructor->entity_specs.is_trivial);
        }
    }
    entry_list_iterator_free(it);

    // After all these tests we can state that this constructor is
    // trivial
    if (!has_virtual_bases
            && !has_bases_with_non_trivial_constructors
            && !has_virtual_functions
            && !has_nonstatic_data_member_with_initializer
            && !has_nonstatic_data_member_with_no_trivial_constructor)
    {
        default_constructor->entity_specs.is_trivial = 1;
    }
}

static void copy_constructor_determine_if_trivial(
        scope_entry_t* copy_constructor,
        scope_entry_list_t* direct_base_classes,
        scope_entry_list_t* nonstatic_data_members,
        char has_virtual_bases,
        char has_virtual_functions)
{
    char has_bases_with_no_trivial_copy_constructor = 0;

    // Now figure out if it is trivial
    scope_entry_list_iterator_t* it = NULL;
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
            scope_entry_t* current_copy_constructor = entry_list_iterator_current(it2);

            has_bases_with_no_trivial_copy_constructor
                |= !current_copy_constructor->entity_specs.is_trivial;
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

        if (is_class_type_or_array_thereof(data_member->type_information))
        {
            type_t* member_class_type = data_member->type_information;
            if (is_array_type(data_member->type_information))
                member_class_type = array_type_get_element_type(member_class_type);

            type_t* member_actual_class_type = get_actual_class_type(member_class_type);


            scope_entry_list_t* member_copy_constructors = class_type_get_copy_constructors(member_actual_class_type);
            has_nonstatic_data_member_with_no_trivial_copy_constructor =
                one_function_is_nontrivial(member_copy_constructors);

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
        copy_constructor->entity_specs.is_trivial = 1;
    }
}

static void move_constructor_determine_if_trivial(
        scope_entry_t* move_constructor,
        scope_entry_list_t* all_bases,
        char has_virtual_bases,
        char has_virtual_functions,
        char has_member_with_nontrivial_move_constructor)
{
    /*
       A copy/move constructor for class X is trivial if it is neither user-provided nor deleted and if
       1. class X has no virtual functions (10.3) and no virtual base classes (10.1), and
       2. the constructor selected to copy/move each direct base class subobject is trivial, and
       3. for each non-static data member of X that is of class type (or array thereof), the constructor selected
       to copy/move that member is trivial;
       otherwise the copy/move constructor is non-trivial.
       */

    char has_base_with_nontrivial_move_constructor = 0;

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(all_bases);
            !entry_list_iterator_end(it) && !has_base_with_nontrivial_move_constructor;
            entry_list_iterator_next(it))
    {
        scope_entry_t* base_class = entry_list_iterator_current(it);
        scope_entry_list_t* move_constructors = class_type_get_move_constructors(base_class->type_information);
        has_base_with_nontrivial_move_constructor = one_function_is_nontrivial(move_constructors);
    }
    entry_list_iterator_free(it);

    if (!has_virtual_functions
            && !has_virtual_bases
            && !has_base_with_nontrivial_move_constructor
            && !has_member_with_nontrivial_move_constructor)
    {
        move_constructor->entity_specs.is_trivial = 1;
    }
}

static void copy_assignment_operator_determine_if_trivial(
        scope_entry_t* copy_assignment_operator,
        scope_entry_list_t* direct_base_classes,
        scope_entry_list_t* nonstatic_data_members,
        char has_virtual_bases,
        char has_virtual_functions)
{
    char has_base_classes_with_no_trivial_copy_assignment = 0;

    scope_entry_list_iterator_t* it;
    for (it = entry_list_iterator_begin(direct_base_classes);
            !entry_list_iterator_end(it)
            && !has_virtual_bases
            && !has_base_classes_with_no_trivial_copy_assignment; 
            entry_list_iterator_next(it))
    {
        scope_entry_t* base_class = entry_list_iterator_current(it);

        type_t* base_class_type = get_actual_class_type(base_class->type_information);

        scope_entry_list_t* base_copy_assignment_operators = class_type_get_copy_assignment_operators(base_class_type);
        has_base_classes_with_no_trivial_copy_assignment =
            one_function_is_nontrivial(base_copy_assignment_operators);
        entry_list_free(base_copy_assignment_operators);
    }
    entry_list_iterator_free(it);

    char has_nonstatic_data_member_with_no_trivial_copy_assignment = 0;

    for (it = entry_list_iterator_begin(nonstatic_data_members);
            !entry_list_iterator_end(it)
            && !has_nonstatic_data_member_with_no_trivial_copy_assignment;
            entry_list_iterator_next(it))
    {
        scope_entry_t *data_member = entry_list_iterator_current(it);

        if (is_class_type_or_array_thereof(data_member->type_information))
        {
            type_t* member_class_type = data_member->type_information;
            if (is_array_type(data_member->type_information))
                member_class_type = array_type_get_element_type(member_class_type);

            type_t* member_actual_class_type = get_actual_class_type(member_class_type);

            scope_entry_list_t* member_copy_assignment_operators = class_type_get_copy_assignment_operators(member_actual_class_type);
            has_nonstatic_data_member_with_no_trivial_copy_assignment =
                one_function_is_nontrivial(member_copy_assignment_operators);
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
        copy_assignment_operator->entity_specs.is_trivial = 1;
    }
}

static void move_assignment_operator_determine_if_trivial(
        scope_entry_t* move_assignment_operator,
        scope_entry_list_t* direct_base_classes,
        scope_entry_list_t* nonstatic_data_members,
        char has_virtual_bases,
        char has_virtual_functions)
{
    // If not deleted it may be trivial
    char has_base_classes_with_no_trivial_move_assignment = 0;

    scope_entry_list_iterator_t* it;
    for (it = entry_list_iterator_begin(direct_base_classes);
            !entry_list_iterator_end(it)
            && !has_virtual_bases
            && !has_base_classes_with_no_trivial_move_assignment; 
            entry_list_iterator_next(it))
    {
        scope_entry_t* base_class = entry_list_iterator_current(it);

        type_t* base_class_type = get_actual_class_type(base_class->type_information);

        scope_entry_list_t* base_move_assignment_operators = class_type_get_move_assignment_operators(base_class_type);
        has_base_classes_with_no_trivial_move_assignment =
            one_function_is_nontrivial(base_move_assignment_operators);
        entry_list_free(base_move_assignment_operators);
    }
    entry_list_iterator_free(it);

    char has_nonstatic_data_member_with_no_trivial_move_assignment = 0;

    for (it = entry_list_iterator_begin(nonstatic_data_members);
            !entry_list_iterator_end(it)
            && !has_nonstatic_data_member_with_no_trivial_move_assignment;
            entry_list_iterator_next(it))
    {
        scope_entry_t *data_member = entry_list_iterator_current(it);

        if (is_class_type_or_array_thereof(data_member->type_information))
        {
            type_t* member_class_type = data_member->type_information;
            if (is_array_type(data_member->type_information))
                member_class_type = array_type_get_element_type(member_class_type);

            type_t* member_actual_class_type = get_actual_class_type(member_class_type);

            scope_entry_list_t* member_move_assignment_operators = class_type_get_move_assignment_operators(member_actual_class_type);
            has_nonstatic_data_member_with_no_trivial_move_assignment =
                one_function_is_nontrivial(member_move_assignment_operators);
            entry_list_free(member_move_assignment_operators);
        }
    }
    entry_list_iterator_free(it);

    // It is trivial
    if (!has_virtual_bases
            && !has_base_classes_with_no_trivial_move_assignment
            && !has_virtual_functions
            && !has_nonstatic_data_member_with_no_trivial_move_assignment)
    {
        move_assignment_operator->entity_specs.is_trivial = 1;
    }
}

static void destructor_determine_if_trivial(
        scope_entry_t* destructor,
        scope_entry_list_t* all_bases,
        scope_entry_list_t* nonstatic_data_members)
{
    // Let's see whether it is trivial
    char base_has_nontrivial_destructor = 0;
    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(all_bases);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t *base_class = entry_list_iterator_current(it);

        scope_entry_t* current_destructor 
            = class_type_get_destructor(get_actual_class_type(base_class->type_information));

        ERROR_CONDITION(current_destructor == NULL, "Invalid class without destructor!\n", 0);

        base_has_nontrivial_destructor |= !current_destructor->entity_specs.is_trivial;
    }
    entry_list_iterator_free(it);

    char has_nonstatic_data_member_with_no_trivial_destructor = 0;

    for (it = entry_list_iterator_begin(nonstatic_data_members);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t *data_member = entry_list_iterator_current(it);
        if (is_class_type_or_array_thereof(data_member->type_information))
        {
            type_t* member_class_type = data_member->type_information;
            if (is_array_type(data_member->type_information))
                member_class_type = array_type_get_element_type(member_class_type);

            type_t* member_actual_class_type = get_actual_class_type(member_class_type);

            scope_entry_t* current_destructor
                = class_type_get_destructor(
                        get_actual_class_type(member_actual_class_type));

            ERROR_CONDITION(current_destructor == NULL, "Invalid class without destructor!\n", 0);

            has_nonstatic_data_member_with_no_trivial_destructor
                |= !current_destructor->entity_specs.is_trivial;
        }
    }
    entry_list_iterator_free(it);

    // It is a trivial destructor
    if (!base_has_nontrivial_destructor
            && !has_nonstatic_data_member_with_no_trivial_destructor)
    {
        destructor->entity_specs.is_trivial = 1;
    }
}

static void set_defaulted_outside_class_specifier(
        scope_entry_t* entry, 
        decl_context_t decl_context,
        const locus_t* locus);
static void build_noexcept_spec_delayed(scope_entry_t* entry);

// See gather_type_spec_from_class_specifier to know what are class_type and type_info
// This function is only for C++
//
// FIXME - This function is still HUGE
static void finish_class_type_cxx(type_t* class_type,
        type_t* type_info,
        decl_context_t decl_context,
        const locus_t* locus,
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
                fprintf(stderr, "BUILDSCOPE: Completing member '%s' at '%s' with type '%s'\n",
                        data_member->symbol_name,
                        locus_to_str(data_member->locus),
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

            if (is_named_class_type(current_type))
            {
                scope_entry_t* named_type_sym = named_type_get_symbol(current_type);

                instantiate_template_class_if_needed(named_type_sym, decl_context, locus);
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

        // Check if this class is trivially virtual
        for (it0 = entry_list_iterator_begin(member_functions);
                !entry_list_iterator_end(it0);
                entry_list_iterator_next(it0))
        {
            scope_entry_t *entry = entry_list_iterator_current(it0);

            if (entry->entity_specs.is_static)
                continue;

            if (entry->entity_specs.is_virtual
                    && entry->entity_specs.is_pure)
            {
                class_type_set_is_abstract(class_type, 1);
                // Nothing else to do if this a virtual pure
                continue;
            }

        }
        entry_list_iterator_free(it0);

        // For each base, get all its virtual functions and see if one of our
        // member functions is an override. If none is an override and the
        // function is a virtual pure, our class is abstract
        scope_entry_list_iterator_t* it1 = NULL;
        for (it1 = entry_list_iterator_begin(all_bases);
                !entry_list_iterator_end(it1);
                entry_list_iterator_next(it1))
        {
            scope_entry_t *base_class = entry_list_iterator_current(it1);

            scope_entry_list_t* virtual_functions = class_type_get_virtual_functions(base_class->type_information);

            scope_entry_list_iterator_t* it2 = NULL;

            for (it2 = entry_list_iterator_begin(virtual_functions);
                    !entry_list_iterator_end(it2);
                    entry_list_iterator_next(it2))
            {
                scope_entry_t* current_virtual = entry_list_iterator_current(it2);

                // I know this loop looks is weird but otherwise is not possible to know if the member functions
                // of the current class override the virtual functions of the base
                char is_overriden = 0;
                scope_entry_list_iterator_t* it3 = NULL;
                for (it3 = entry_list_iterator_begin(member_functions);
                        !entry_list_iterator_end(it3);
                        entry_list_iterator_next(it3))
                {
                    scope_entry_t *entry = entry_list_iterator_current(it3);

                    if (strcmp(entry->symbol_name, current_virtual->symbol_name) == 0
                            && function_type_can_override(entry->type_information, current_virtual->type_information))
                    {
                        if (current_virtual->entity_specs.is_final)
                        {
                            error_printf("%s: error: member function '%s' overrides final '%s'\n",
                                    locus_to_str(entry->locus),
                                    print_decl_type_str(entry->type_information,
                                        entry->decl_context,
                                        get_qualified_symbol_name(entry, entry->decl_context)),
                                    print_decl_type_str(current_virtual->type_information,
                                        current_virtual->decl_context,
                                        get_qualified_symbol_name(current_virtual, current_virtual->decl_context)));
                        }

                        entry->entity_specs.is_virtual = 1;

                        is_overriden = 1;

                        DEBUG_CODE()
                        {
                            fprintf(stderr, "BUILDSCOPE: Function '%s' of '%s' is inheritedly virtual\n",
                                    print_decl_type_str(entry->type_information, decl_context, entry->symbol_name),
                                    locus_to_str(entry->locus));
                        }
                    }
                }
                entry_list_iterator_free(it3);

                // If the current virtual of the base class is not being
                // overriden and it is virtual pure this converts this class in
                // abstract
                if (!is_overriden
                        && current_virtual->entity_specs.is_pure)
                {
                    class_type_set_is_abstract(class_type, 1);
                }
            }
            entry_list_iterator_free(it2);
            entry_list_free(virtual_functions);
        }
        entry_list_iterator_free(it1);

        scope_entry_list_iterator_t* it3 = NULL;
        for (it3 = entry_list_iterator_begin(member_functions);
                !entry_list_iterator_end(it3);
                entry_list_iterator_next(it3))
        {
            scope_entry_t *entry = entry_list_iterator_current(it3);

            if (entry->entity_specs.is_final && !entry->entity_specs.is_virtual)
            {
                error_printf("%s: error: member function '%s' declared as final but it is not virtual\n",
                        locus_to_str(entry->locus),
                        print_decl_type_str(entry->type_information,
                            entry->decl_context,
                            get_qualified_symbol_name(entry, entry->decl_context)));
            }

            if (entry->entity_specs.is_override)
            {
                char does_override = 0;
                for (it1 = entry_list_iterator_begin(all_bases);
                        !entry_list_iterator_end(it1);
                        entry_list_iterator_next(it1))
                {
                    scope_entry_t *base_class = entry_list_iterator_current(it1);

                    scope_entry_list_t* virtual_functions = class_type_get_virtual_functions(base_class->type_information);

                    scope_entry_list_iterator_t* it2 = NULL;

                    for (it2 = entry_list_iterator_begin(virtual_functions);
                            !entry_list_iterator_end(it2);
                            entry_list_iterator_next(it2))
                    {
                        scope_entry_t* current_virtual = entry_list_iterator_current(it2);

                        if (strcmp(entry->symbol_name, current_virtual->symbol_name) == 0
                                && function_type_can_override(entry->type_information, current_virtual->type_information))
                        {
                            does_override = 1;
                        }
                    }
                    entry_list_iterator_free(it2);
                    entry_list_free(virtual_functions);
                }
                entry_list_iterator_free(it1);

                if (!does_override)
                {
                    error_printf("%s: error: member function '%s' declared as override but it does not override\n",
                            locus_to_str(entry->locus),
                            print_decl_type_str(entry->type_information,
                                entry->decl_context,
                                get_qualified_symbol_name(entry, entry->decl_context)));
                }
            }
        }
        entry_list_iterator_free(it3);
        entry_list_free(member_functions);
    }

    // Defaulted functions inside the class specifier, verify them now
    {
        scope_entry_list_t* member_functions = class_type_get_member_functions(class_type);

        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(member_functions);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* current_member_function = entry_list_iterator_current(it);

            if (current_member_function->entity_specs.is_defaulted
                    && current_member_function->entity_specs.is_defined_inside_class_specifier)
            {
                // Verify again that this function can be defaulted
                current_member_function->defined = 0;
                current_member_function->entity_specs.is_defaulted = 0;

                // Make sure the exception specifier has been fully parsed at this point
                if (!nodecl_is_null(current_member_function->entity_specs.noexception)
                        && nodecl_get_kind(current_member_function->entity_specs.noexception) == NODECL_CXX_PARSE_LATER)
                {
                    build_noexcept_spec_delayed(current_member_function);
                }

                set_defaulted_outside_class_specifier(
                        current_member_function,
                        current_member_function->decl_context,
                        current_member_function->locus);
            }
        }
        entry_list_iterator_free(it);

        entry_list_free(member_functions);
    }

    scope_entry_list_t* virtual_functions = class_type_get_virtual_functions(class_type);
    char has_virtual_functions = virtual_functions != NULL;
    char has_virtual_bases = virtual_base_classes != NULL;
    entry_list_free(virtual_functions);

    // Implicit default constructor
    scope_entry_list_t* constructors = class_type_get_constructors(class_type);
    char no_constructors = (constructors == NULL);

    decl_context_t class_context = class_type_get_inner_context(class_type);
    scope_t* class_scope = class_context.current_scope;

    if (no_constructors)
    {
        type_t* default_constructor_type = get_new_function_type(
                NULL, // Constructors do not return anything
                NULL, // Default constructor does not receive anything
                0, REF_QUALIFIER_NONE);

        const char* constructor_name = NULL;
        if (is_named_class_type(type_info))
        {
            uniquestr_sprintf(&constructor_name, "constructor %s", named_type_get_symbol(type_info)->symbol_name);
        }
        else
        {
            uniquestr_sprintf(&constructor_name, "%s", "constructor ");
        }

        scope_entry_t* implicit_default_constructor = new_symbol(class_type_get_inner_context(class_type), class_scope,
                constructor_name);

        implicit_default_constructor->kind = SK_FUNCTION;
        implicit_default_constructor->locus = locus;
        implicit_default_constructor->entity_specs.is_member = 1;
        implicit_default_constructor->entity_specs.access = AS_PUBLIC;
        implicit_default_constructor->entity_specs.class_type = type_info;
        implicit_default_constructor->entity_specs.is_inline = 1;
        implicit_default_constructor->entity_specs.is_constructor = 1;
        implicit_default_constructor->entity_specs.is_default_constructor = 1;

        implicit_default_constructor->type_information = default_constructor_type;

        implicit_default_constructor->defined = 1;

        implicit_default_constructor->entity_specs.num_parameters = 0;

        class_type_add_member(class_type, implicit_default_constructor, /* is_definition */ 0);
        class_type_set_default_constructor(class_type, implicit_default_constructor);

        // Now check if the implicitly declared constructor is deleted
        // 1. X is a union-like class that has a variant member with a non-trivial default constructor,
        // 2. any non-static data member with no brace-or-equal-initializer is of reference type,
        // 3. any non-variant non-static data member of const-qualified type (or array thereof) with no brace-or-
        // equal-initializer does not have a user-provided default constructor,
        // 4. X is a union and all of its variant members are of const-qualified type (or array thereof),
        // 5. X is a non-union class and all members of any anonymous union member are of const-qualified type
        // (or array thereof), or
        // 6. any direct or virtual base class, or non-static data member with no brace-or-equal-initializer, has class
        // type M (or array thereof) and either M has no default constructor or overload resolution (13.3) as applied
        // to M’s default constructor results in an ambiguity or in a function that is deleted or inaccessible from
        // the defaulted default constructor.
        scope_entry_list_iterator_t* it = NULL;

        // 1.
        char has_variant_member_with_nontrivial_default_ctor = 0;
        if (is_union_type(class_type)
                // This does not apply to anonymous unions
                && !named_type_get_symbol(type_info)->entity_specs.is_anonymous_union)
        {
            for (it = entry_list_iterator_begin(nonstatic_data_members);
                    !entry_list_iterator_end(it)
                    && !has_variant_member_with_nontrivial_default_ctor;
                    entry_list_iterator_next(it))
            {
                scope_entry_t *data_member = entry_list_iterator_current(it);
                has_variant_member_with_nontrivial_default_ctor = (is_class_type(data_member->type_information)
                        && class_type_get_default_constructor(data_member->type_information) != NULL
                        && !class_type_get_default_constructor(data_member->type_information)->entity_specs.is_trivial);
            }
            entry_list_iterator_free(it);
        }

        // 2.
        char has_nonstatic_data_member_with_reference_type_not_initialized = 0;
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it)
                && !has_nonstatic_data_member_with_reference_type_not_initialized;
                entry_list_iterator_next(it))
        {
            scope_entry_t *data_member = entry_list_iterator_current(it);
            has_nonstatic_data_member_with_reference_type_not_initialized =
                    is_any_reference_type(data_member->type_information)
                    && nodecl_is_null(data_member->value);
        }
        entry_list_iterator_free(it);

        // 3.
        char has_nonstatic_data_member_const_without_initializer_and_no_default_ctor = 0;
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it)
                && !has_nonstatic_data_member_const_without_initializer_and_no_default_ctor;
                entry_list_iterator_next(it))
        {
            scope_entry_t *data_member = entry_list_iterator_current(it);
            if (data_member->entity_specs.is_member_of_anonymous)
                continue;

            has_nonstatic_data_member_const_without_initializer_and_no_default_ctor =
                is_const_qualified_type(data_member->type_information)
                && nodecl_is_null(data_member->value);

            if (is_class_type_or_array_thereof(data_member->type_information))
            {
                type_t* member_type = data_member->type_information;
                if (is_array_type(member_type))
                    member_type = array_type_get_element_type(member_type);

                has_nonstatic_data_member_const_without_initializer_and_no_default_ctor =
                    class_type_get_default_constructor(member_type) == NULL;
            }
        }
        entry_list_iterator_free(it);

        char all_members_of_union_are_const = 0;
        // 4.
        if (is_union_type(class_type))
        {
            char nonconst_member = 0;
            for (it = entry_list_iterator_begin(nonstatic_data_members);
                    !entry_list_iterator_end(it)
                    && !nonconst_member;
                    entry_list_iterator_next(it))
            {
                scope_entry_t *data_member = entry_list_iterator_current(it);
                nonconst_member = !is_const_qualified_type(data_member->type_information);
            }
            entry_list_iterator_free(it);

            all_members_of_union_are_const = !nonconst_member;
        }

        // 5.
        char has_one_anonymous_union_with_all_const_members = 0;
        if (!is_union_type(class_type))
        {
            for (it = entry_list_iterator_begin(nonstatic_data_members);
                    !entry_list_iterator_end(it)
                    && !has_one_anonymous_union_with_all_const_members;
                    entry_list_iterator_next(it))
            {
                scope_entry_t *data_member = entry_list_iterator_current(it);
                if (data_member->entity_specs.is_member_of_anonymous)
                {
                    char nonconst_member = 0;
                    scope_entry_list_t* union_members = class_type_get_nonstatic_data_members(data_member->type_information);
                    scope_entry_list_iterator_t* it0 = NULL;
                    for (it0 = entry_list_iterator_begin(union_members);
                            !entry_list_iterator_end(it0) && !nonconst_member;
                            entry_list_iterator_next(it0))
                    {
                        scope_entry_t *union_member = entry_list_iterator_current(it0);
                        nonconst_member = !is_const_qualified_type(union_member->type_information);
                    }
                    entry_list_iterator_free(it0);

                    has_one_anonymous_union_with_all_const_members = !nonconst_member;
                }
            }
            entry_list_iterator_free(it);
        }

        // 6.
        char has_nonstatic_data_member_with_unusable_base_default_constructor = 0;
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it)
                && !has_nonstatic_data_member_with_unusable_base_default_constructor;
                entry_list_iterator_next(it))
        {
            scope_entry_t *data_member = entry_list_iterator_current(it);
            if (!nodecl_is_null(data_member->value))
                continue;

            if (is_class_type_or_array_thereof(data_member->type_information))
            {
                type_t* member_type = data_member->type_information;
                if (is_array_type(data_member->type_information))
                    member_type = array_type_get_element_type(data_member->type_information);

                scope_entry_t* default_constructor = class_type_get_default_constructor(member_type);

                has_nonstatic_data_member_with_unusable_base_default_constructor =
                    default_constructor == NULL
                    || !one_function_is_usable(
                                entry_list_new(default_constructor),
                                NULL, NULL,
                                decl_context, locus);
            }
        }

        entry_list_iterator_free(it);

        char has_base_with_unusable_default_constructor = 0;
        for (it = entry_list_iterator_begin(all_bases);
                !entry_list_iterator_end(it)
                && !has_base_with_unusable_default_constructor;
                entry_list_iterator_next(it))
        {
            scope_entry_t *base = entry_list_iterator_current(it);

            scope_entry_t* default_constructor = class_type_get_default_constructor(base->type_information);

            if (default_constructor == NULL
                    || !one_function_is_usable(
                        entry_list_new(default_constructor),
                        NULL, NULL,
                        decl_context,
                        locus))
            {
                has_base_with_unusable_default_constructor = 1;
            }
        }
        entry_list_iterator_free(it);

        if (has_variant_member_with_nontrivial_default_ctor
                || has_nonstatic_data_member_with_reference_type_not_initialized
                || has_nonstatic_data_member_const_without_initializer_and_no_default_ctor
                || all_members_of_union_are_const
                || has_one_anonymous_union_with_all_const_members
                || has_nonstatic_data_member_with_unusable_base_default_constructor
                || has_base_with_unusable_default_constructor)
        {
            implicit_default_constructor->entity_specs.is_deleted = 1;
        }
        else
        {
            default_constructor_determine_if_trivial(
                    implicit_default_constructor,
                    nonstatic_data_members,
                    direct_base_classes,
                    has_virtual_bases,
                    has_virtual_functions);
        }
    }
    else
    {
        // Check for triviality of defaulted default constructors
        scope_entry_list_iterator_t* it;
        for (it = entry_list_iterator_begin(constructors);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* current_constructor = entry_list_iterator_current(it);

            if (current_constructor->entity_specs.is_default_constructor
                    && current_constructor->entity_specs.is_defaulted)
            {
                default_constructor_determine_if_trivial(
                        current_constructor,
                        nonstatic_data_members,
                        direct_base_classes,
                        has_virtual_bases,
                        has_virtual_functions);
            }
        }
        entry_list_iterator_free(it);
    }
    entry_list_free(constructors);

    scope_entry_list_t* user_declared_copy_constructors = class_type_get_copy_constructors(class_type);
    scope_entry_list_t* user_declared_copy_assignment_operators = class_type_get_copy_assignment_operators(class_type);

    scope_entry_list_t* user_declared_move_constructors = class_type_get_move_constructors(class_type);
    scope_entry_list_t* user_declared_move_assignment_operators = class_type_get_move_assignment_operators(class_type);

    scope_entry_t* user_declared_destructor = class_type_get_destructor(class_type);

    char have_to_emit_implicit_copy_constructor = (user_declared_copy_constructors == NULL)
        && (user_declared_move_constructors == NULL)
        && (user_declared_move_assignment_operators == NULL);

    // Copy constructor
    if (have_to_emit_implicit_copy_constructor)
    {
        if (IS_CXX11_LANGUAGE)
        {
            if (user_declared_copy_assignment_operators != NULL
                    || user_declared_destructor != NULL)
            {
                /* Though deprecated seems to be common... */
                // warn_printf("%s: declaring an implicit copy constructor of a class with a user-provided "
                //         "destructor or copy assignment operator is deprecated in C++11\n",
                //         locus_to_str(locus));
            }
        }

        char const_parameter = 1;
        // Now check bases for a const qualified version
        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(all_bases);
                !entry_list_iterator_end(it) && const_parameter;
                entry_list_iterator_next(it))
        {
            scope_entry_t *base_class = entry_list_iterator_current(it);

            const_parameter = const_parameter &&
                class_has_const_copy_constructor(get_user_defined_type(base_class));

        }
        entry_list_iterator_free(it);

        // Now check my nonstatic members that are classes (or arrays to classes)
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it) && const_parameter;
                entry_list_iterator_next(it))
        {
            scope_entry_t *data_member = entry_list_iterator_current(it);

            if (is_class_type_or_array_thereof(data_member->type_information))
            {
                type_t* member_class_type = data_member->type_information;
                if (is_array_type(data_member->type_information))
                    member_class_type = array_type_get_element_type(member_class_type);

                const_parameter = const_parameter &&
                    class_has_const_copy_constructor(member_class_type);
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
                1, REF_QUALIFIER_NONE);

        const char* constructor_name = NULL;
        if (is_named_class_type(type_info))
        {
            uniquestr_sprintf(&constructor_name, "constructor %s", named_type_get_symbol(type_info)->symbol_name);
        }
        else
        {
            uniquestr_sprintf(&constructor_name, "%s", "constructor ");
        }

        scope_entry_t* implicit_copy_constructor = new_symbol(class_type_get_inner_context(class_type),
                class_scope,
                constructor_name);

        implicit_copy_constructor->kind = SK_FUNCTION;
        implicit_copy_constructor->locus = locus;
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

        class_type_add_member(class_type, implicit_copy_constructor, /* is_definition */ 1);

        // We have to see whether this copy constructor is trivial
        copy_constructor_determine_if_trivial(
                implicit_copy_constructor,
                direct_base_classes,
                nonstatic_data_members,
                has_virtual_bases,
                has_virtual_functions);
    }
    else
    {
        // Check for triviality of default copy constructors
        scope_entry_list_iterator_t *it;
        for (it = entry_list_iterator_begin(user_declared_copy_constructors);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* current_constructor = entry_list_iterator_current(it);
            if (current_constructor->entity_specs.is_defaulted)
            {
                copy_constructor_determine_if_trivial(
                        current_constructor,
                        direct_base_classes,
                        nonstatic_data_members,
                        has_virtual_bases,
                        has_virtual_functions);
            }
        }
        entry_list_iterator_free(it);
    }

    /*
       If the definition of a class X does not explicitly declare a move constructor, one will be implicitly declared
       as defaulted if and only if
       1. X does not have a user-declared copy constructor,
       2. X does not have a user-declared copy assignment operator,
       3. X does not have a user-declared move assignment operator,
       4. X does not have a user-declared destructor, and
       5. the move constructor would not be implicitly defined as deleted.
       */
    char may_have_to_emit_implicit_move_constructor =
        IS_CXX11_LANGUAGE
        && user_declared_copy_constructors == NULL
        && user_declared_copy_assignment_operators == NULL
        && user_declared_move_assignment_operators == NULL
        && user_declared_destructor == NULL;

    // We need to compute this for both an implicit or defaulted move constructor
    char has_member_with_nontrivial_move_constructor = 0;
    // 1. See below when determining the implicit move destructor
    if (is_union_type(class_type))
    {
        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it)
                && !has_member_with_nontrivial_move_constructor;
                entry_list_iterator_next(it))
        {
            scope_entry_t* data_member = entry_list_iterator_current(it);
            if (is_class_type(data_member->type_information))
            {
                scope_entry_list_t* move_constructors = class_type_get_move_constructors(data_member->type_information);
                has_member_with_nontrivial_move_constructor =
                    one_function_is_nontrivial(move_constructors);
                entry_list_free(move_constructors);
            }
        }
        entry_list_iterator_free(it);
    }

    if (may_have_to_emit_implicit_move_constructor)
    {
        /*
           An implicitly-declared copy/move constructor is an inline public member of its class. A defaulted copy-
           /move constructor for a class X is defined as deleted (8.4.3) if X has:
           1. a variant member with a non-trivial corresponding constructor and X is a union-like class,
           2. a non-static data member of class type M (or array thereof) that cannot be copied/moved because
           overload resolution (13.3), as applied to M’s corresponding constructor, results in an ambiguity or a
           function that is deleted or inaccessible from the defaulted constructor, or
           3. a direct or virtual base class B that cannot be copied/moved because overload resolution (13.3), as
           applied to B’s corresponding constructor, results in an ambiguity or a function that is deleted or
           inaccessible from the defaulted constructor, or
           4. for the copy constructor, a non-static data member of rvalue reference type, or
           5. for the move constructor, a non-static data member or direct or virtual base class with a type that
           does not have a move constructor and is not trivially copyable.
           */

        // 2.
        char has_member_with_unusable_move_constructor = 0;
        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it)
                && !has_member_with_unusable_move_constructor;
                entry_list_iterator_next(it))
        {
            scope_entry_t* data_member = entry_list_iterator_current(it);
            if (is_class_type_or_array_thereof(data_member->type_information))
            {
                type_t* member_type = data_member->type_information;
                if (is_array_type(data_member->type_information))
                    member_type = array_type_get_element_type(data_member->type_information);

                if (!one_function_is_usable(class_type_get_move_constructors(member_type),
                            member_type, member_type,
                            decl_context,
                            locus))
                {
                    has_member_with_unusable_move_constructor = 1;
                }
            }
        }
        entry_list_iterator_free(it);

        // 3.
        char has_base_with_unusable_move_constructor = 0;
        for (it = entry_list_iterator_begin(all_bases);
                !entry_list_iterator_end(it) && !has_base_with_unusable_move_constructor;
                entry_list_iterator_next(it))
        {
            scope_entry_t* base_class = entry_list_iterator_current(it);
            type_t* base_type = base_class->type_information;

            if (!one_function_is_usable(class_type_get_move_constructors(base_type),
                        get_user_defined_type(base_class), get_user_defined_type(base_class),
                        decl_context,
                        locus))
            {
                has_base_with_unusable_move_constructor = 1;
            }
        }
        entry_list_iterator_free(it);

        // 4. does not apply to move constructors

        // 5.
        char has_member_without_move_constructor_that_cannot_be_trivially_copied = 0;
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it)
                && !has_member_without_move_constructor_that_cannot_be_trivially_copied;
                entry_list_iterator_next(it))
        {
            scope_entry_t* data_member = entry_list_iterator_current(it);
            if (is_class_type_or_array_thereof(data_member->type_information))
            {
                type_t* member_type = data_member->type_information;
                if (is_array_type(data_member->type_information))
                    member_type = array_type_get_element_type(data_member->type_information);

                has_member_without_move_constructor_that_cannot_be_trivially_copied = 
                    class_type_get_move_constructors(member_type) == NULL
                    && !is_trivially_copiable_type(member_type);
            }
        }

        char has_base_without_move_constructor_that_cannot_be_trivially_copied = 0;
        for (it = entry_list_iterator_begin(all_bases);
                !entry_list_iterator_end(it) && !has_base_with_unusable_move_constructor;
                entry_list_iterator_next(it))
        {
            scope_entry_t* base_class = entry_list_iterator_current(it);
            type_t* base_type = base_class->type_information;

            has_base_without_move_constructor_that_cannot_be_trivially_copied = 
                class_type_get_move_constructors(base_type) == NULL
                && !is_trivially_copiable_type(base_type);
        }

        if (has_member_with_nontrivial_move_constructor
                    || has_member_with_unusable_move_constructor
                    || has_base_with_unusable_move_constructor
                    || has_member_without_move_constructor_that_cannot_be_trivially_copied
                    || has_base_without_move_constructor_that_cannot_be_trivially_copied)
        {
            // The move constructor is deleted
        }
        else
        {
            // Add the move constructor
            const char* constructor_name = NULL;
            if (is_named_class_type(type_info))
            {
                uniquestr_sprintf(&constructor_name, "constructor %s", named_type_get_symbol(type_info)->symbol_name);
            }
            else
            {
                uniquestr_sprintf(&constructor_name, "%s", "constructor ");
            }

            scope_entry_t* implicit_move_constructor = new_symbol(class_type_get_inner_context(class_type),
                    class_scope,
                    constructor_name);

            parameter_info_t parameter_info[1];
            memset(parameter_info, 0, sizeof(parameter_info));
            parameter_info[0].is_ellipsis = 0;
            parameter_info[0].type_info = get_rvalue_reference_type(type_info);

            type_t* move_constructor_type = get_new_function_type(
                    NULL, // Constructors do not return anything
                    parameter_info,
                    1, REF_QUALIFIER_NONE);

            implicit_move_constructor->kind = SK_FUNCTION;
            implicit_move_constructor->locus = locus;
            implicit_move_constructor->entity_specs.is_member = 1;
            implicit_move_constructor->entity_specs.access = AS_PUBLIC;
            implicit_move_constructor->entity_specs.class_type = type_info;
            implicit_move_constructor->entity_specs.is_constructor = 1;
            implicit_move_constructor->entity_specs.is_move_constructor = 1;
            implicit_move_constructor->entity_specs.is_conversor_constructor = 1;
            implicit_move_constructor->entity_specs.is_inline = 1;

            implicit_move_constructor->type_information = move_constructor_type;

            implicit_move_constructor->defined = 1;

            implicit_move_constructor->entity_specs.num_parameters = 1;
            implicit_move_constructor->entity_specs.default_argument_info = empty_default_argument_info(1);

            class_type_add_member(class_type, implicit_move_constructor, /* is_definition */ 1);

            // If it is not deleted we still have to figure if it is trivial
            move_constructor_determine_if_trivial(
                    implicit_move_constructor,
                    all_bases,
                    has_virtual_bases,
                    has_virtual_functions,
                    has_member_with_nontrivial_move_constructor);
        }
    }
    else
    {
        // Check for triviality of defaulted move constructors
        scope_entry_list_iterator_t *it;
        for (it = entry_list_iterator_begin(user_declared_move_constructors);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* current_constructor = entry_list_iterator_current(it);
            if (current_constructor->entity_specs.is_defaulted)
            {
                move_constructor_determine_if_trivial(
                        current_constructor,
                        all_bases,
                        has_virtual_bases,
                        has_virtual_functions,
                        has_member_with_nontrivial_move_constructor);
            }
        }
        entry_list_iterator_free(it);
    }

    // Copy assignment operators
    char no_copy_assignment_operators = user_declared_copy_assignment_operators == NULL;

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

            // Bases have always been instantiated
            const_parameter = const_parameter && 
                class_has_const_copy_assignment_operator(get_user_defined_type(base_class));
        }
        entry_list_iterator_free(it);

        // Now check my nonstatic members that are classes (or arrays to classes)
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it) && const_parameter;
                entry_list_iterator_next(it))
        {
            scope_entry_t *data_member = entry_list_iterator_current(it);

            if (is_class_type_or_array_thereof(data_member->type_information))
            {
                type_t* member_class_type = data_member->type_information;
                if (is_array_type(data_member->type_information))
                    member_class_type = array_type_get_element_type(member_class_type);

                const_parameter = const_parameter &&
                    class_has_const_copy_assignment_operator(member_class_type);
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
                1, REF_QUALIFIER_NONE);

        scope_entry_t* implicit_copy_assignment_function = new_symbol(class_type_get_inner_context(class_type),
                class_scope,
                STR_OPERATOR_ASSIGNMENT);

        implicit_copy_assignment_function->kind = SK_FUNCTION;
        implicit_copy_assignment_function->locus = locus;
        implicit_copy_assignment_function->entity_specs.is_member = 1;
        implicit_copy_assignment_function->entity_specs.access = AS_PUBLIC;
        implicit_copy_assignment_function->entity_specs.class_type = type_info;
        implicit_copy_assignment_function->entity_specs.is_inline = 1;

        implicit_copy_assignment_function->type_information = copy_assignment_type;

        implicit_copy_assignment_function->defined = 1;

        implicit_copy_assignment_function->entity_specs.num_parameters = 1;
        implicit_copy_assignment_function->entity_specs.default_argument_info = empty_default_argument_info(1);

        implicit_copy_assignment_function->entity_specs.is_copy_assignment_operator = 1;

        class_type_add_member(class_type, implicit_copy_assignment_function, /* is_definition */ 1);

        char union_has_member_with_nontrivial_copy_assignment = 0;
        if (is_union_type(class_type)
                // This does not apply to anonymous unions
                && !named_type_get_symbol(type_info)->entity_specs.is_anonymous_union)
        {
            for (it = entry_list_iterator_begin(nonstatic_data_members);
                    !entry_list_iterator_end(it) && union_has_member_with_nontrivial_copy_assignment;
                    entry_list_iterator_next(it))
            {
                scope_entry_t* data_member = entry_list_iterator_current(it);
                if (is_class_type_or_array_thereof(data_member->type_information))
                {
                    type_t* member_type = data_member->type_information;
                    if (is_array_type(member_type))
                        member_type = array_type_get_element_type(member_type);

                    scope_entry_list_t* copy_assignment_ops = class_type_get_copy_assignment_operators(member_type);
                    union_has_member_with_nontrivial_copy_assignment = one_function_is_nontrivial(copy_assignment_ops);
                    entry_list_free(copy_assignment_ops);
                }
            }
        }

        char has_nonstatic_data_member_const_of_non_class_type = 0;
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it) && has_nonstatic_data_member_const_of_non_class_type;
                entry_list_iterator_next(it))
        {
            scope_entry_t* data_member = entry_list_iterator_current(it);
            if (!is_class_type_or_array_thereof(data_member->type_information))
            {
                has_nonstatic_data_member_const_of_non_class_type = is_const_qualified_type(data_member->type_information);
            }
        }

        char has_nonstatic_data_member_reference = 0;
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it) && !has_nonstatic_data_member_reference;
                entry_list_iterator_next(it))
        {
            scope_entry_t* data_member = entry_list_iterator_current(it);
            has_nonstatic_data_member_reference = is_any_reference_type(data_member->type_information);
        }

        char has_non_assignment_operator_copiable_data_member = 0;
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it) && !has_non_assignment_operator_copiable_data_member;
                entry_list_iterator_next(it))
        {
            scope_entry_t* data_member = entry_list_iterator_current(it);
            if (is_class_type_or_array_thereof(data_member->type_information))
            {
                type_t* member_type = data_member->type_information;
                if (is_array_type(member_type))
                    member_type = array_type_get_element_type(member_type);

                has_non_assignment_operator_copiable_data_member = !one_function_is_usable(
                        class_type_get_copy_assignment_operators(member_type),
                        member_type, member_type,
                        decl_context,
                        locus);
            }
        }

        char has_non_assignment_operator_copiable_base = 0;
        for (it = entry_list_iterator_begin(all_bases);
                !entry_list_iterator_end(it) && !has_non_assignment_operator_copiable_base;
                entry_list_iterator_next(it))
        {
            scope_entry_t* base = entry_list_iterator_current(it);
            type_t* base_type = base->type_information;

            has_non_assignment_operator_copiable_base = !one_function_is_usable(
                    class_type_get_copy_assignment_operators(base_type),
                    get_user_defined_type(base), get_user_defined_type(base),
                    decl_context,
                    locus);
        }

        if (union_has_member_with_nontrivial_copy_assignment
                || has_nonstatic_data_member_const_of_non_class_type
                || has_nonstatic_data_member_reference
                || has_non_assignment_operator_copiable_data_member
                || has_non_assignment_operator_copiable_base)
        {
            implicit_copy_assignment_function->entity_specs.is_deleted = 1;
        }
        else
        {
            // If it is not deleted it may be trivial
            copy_assignment_operator_determine_if_trivial(
                    implicit_copy_assignment_function,
                    direct_base_classes,
                    nonstatic_data_members,
                    has_virtual_bases,
                    has_virtual_functions);
        }
    }
    else
    {
        // Check for triviality of defaulted copy assignment operators
        scope_entry_list_iterator_t *it;
        for (it = entry_list_iterator_begin(user_declared_copy_assignment_operators);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* current_assignment_operator = entry_list_iterator_current(it);
            if (current_assignment_operator->entity_specs.is_defaulted)
            {
                copy_assignment_operator_determine_if_trivial(
                        current_assignment_operator,
                        direct_base_classes,
                        nonstatic_data_members,
                        has_virtual_bases,
                        has_virtual_functions);
            }
        }
        entry_list_iterator_free(it);
    }

    char may_have_to_emit_implicit_move_assignment =
        IS_CXX11_LANGUAGE
        && user_declared_move_assignment_operators == NULL
        && user_declared_copy_constructors == NULL
        && user_declared_move_constructors == NULL
        && user_declared_copy_assignment_operators == NULL;

    if (may_have_to_emit_implicit_move_assignment)
    {
        scope_entry_list_iterator_t* it = NULL;

        char union_has_member_with_nontrivial_move_assignment = 0;
        if (is_union_type(class_type)
                // This does not apply to anonymous unions
                && !named_type_get_symbol(type_info)->entity_specs.is_anonymous_union)
        {
            for (it = entry_list_iterator_begin(nonstatic_data_members);
                    !entry_list_iterator_end(it) && union_has_member_with_nontrivial_move_assignment;
                    entry_list_iterator_next(it))
            {
                scope_entry_t* data_member = entry_list_iterator_current(it);
                if (is_class_type_or_array_thereof(data_member->type_information))
                {
                    type_t* member_type = data_member->type_information;
                    if (is_array_type(member_type))
                        member_type = array_type_get_element_type(member_type);

                    scope_entry_list_t* move_assignment_ops = class_type_get_move_assignment_operators(member_type);
                    union_has_member_with_nontrivial_move_assignment = one_function_is_nontrivial(move_assignment_ops);
                    entry_list_free(move_assignment_ops);
                }
            }
        }

        char has_nonstatic_data_member_const_of_non_class_type = 0;
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it) && has_nonstatic_data_member_const_of_non_class_type;
                entry_list_iterator_next(it))
        {
            scope_entry_t* data_member = entry_list_iterator_current(it);
            if (!is_class_type_or_array_thereof(data_member->type_information))
            {
                has_nonstatic_data_member_const_of_non_class_type = is_const_qualified_type(data_member->type_information);
            }
        }

        char has_nonstatic_data_member_reference = 0;
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it) && !has_nonstatic_data_member_reference;
                entry_list_iterator_next(it))
        {
            scope_entry_t* data_member = entry_list_iterator_current(it);
            has_nonstatic_data_member_reference = is_any_reference_type(data_member->type_information);
        }

        char has_non_assignment_operator_moveable_data_member = 0;
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it) && !has_non_assignment_operator_moveable_data_member;
                entry_list_iterator_next(it))
        {
            scope_entry_t* data_member = entry_list_iterator_current(it);
            if (is_class_type_or_array_thereof(data_member->type_information))
            {
                type_t* member_type = data_member->type_information;
                if (is_array_type(member_type))
                    member_type = array_type_get_element_type(member_type);

                has_non_assignment_operator_moveable_data_member = !one_function_is_usable(
                        class_type_get_move_assignment_operators(member_type),
                        member_type, member_type,
                        decl_context,
                        locus);
            }
        }

        char has_non_assignment_operator_moveable_base = 0;
        for (it = entry_list_iterator_begin(all_bases);
                !entry_list_iterator_end(it) && !has_non_assignment_operator_moveable_base;
                entry_list_iterator_next(it))
        {
            scope_entry_t* base = entry_list_iterator_current(it);
            type_t* base_type = base->type_information;

            has_non_assignment_operator_moveable_base = !one_function_is_usable(
                    class_type_get_move_assignment_operators(base_type),
                    get_user_defined_type(base), get_user_defined_type(base),
                    decl_context,
                    locus);
        }

        /*
           for the move assignment operator, a non-static data member or direct base class with a type that does
           not have a move assignment operator and is not trivially copyable, or any direct or indirect virtual
           base class.
           */
        char has_nonstatic_data_member_without_move_assignment_operator_and_not_trivially_copiable = 0;
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it)
                && !has_nonstatic_data_member_without_move_assignment_operator_and_not_trivially_copiable;
                entry_list_iterator_next(it))
        {
            scope_entry_t* data_member = entry_list_iterator_current(it);

            if (is_class_type_or_array_thereof(data_member->type_information))
            {
                type_t* member_type = data_member->type_information;
                if (is_array_type(member_type))
                    member_type = array_type_get_element_type(member_type);

                has_nonstatic_data_member_without_move_assignment_operator_and_not_trivially_copiable =
                    class_type_get_move_assignment_operators(member_type) == NULL
                    && !is_trivially_copiable_type(member_type);
            }
        }

        char has_base_without_move_assignment_operator_and_not_trivially_copiable = 0;
        for (it = entry_list_iterator_begin(direct_base_classes);
                !entry_list_iterator_end(it) && !has_base_without_move_assignment_operator_and_not_trivially_copiable;
                entry_list_iterator_next(it))
        {
            scope_entry_t* base = entry_list_iterator_current(it);
            type_t* base_type = base->type_information;

            has_non_assignment_operator_moveable_base = class_type_get_move_assignment_operators(base_type) == NULL
                && !is_trivially_copiable_type(base_type);
        }

        if (union_has_member_with_nontrivial_move_assignment
                || has_nonstatic_data_member_const_of_non_class_type
                || has_nonstatic_data_member_reference
                || has_non_assignment_operator_moveable_data_member
                || has_non_assignment_operator_moveable_base
                || has_nonstatic_data_member_without_move_assignment_operator_and_not_trivially_copiable
                || has_base_without_move_assignment_operator_and_not_trivially_copiable
                || has_virtual_bases)
        {
            // The move assignment operator is deleted
        }
        else
        {
            parameter_info_t parameter_info[1];
            memset(parameter_info, 0, sizeof(parameter_info));
            parameter_info[0].is_ellipsis = 0;

            parameter_info[0].type_info = get_rvalue_reference_type(type_info);

            type_t* move_assignment_type = get_new_function_type(
                    /* returns T& */ get_lvalue_reference_type(type_info), 
                    parameter_info,
                    1, REF_QUALIFIER_NONE);

            scope_entry_t* implicit_move_assignment_function = new_symbol(class_type_get_inner_context(class_type),
                    class_scope,
                    STR_OPERATOR_ASSIGNMENT);

            implicit_move_assignment_function->kind = SK_FUNCTION;
            implicit_move_assignment_function->locus = locus;
            implicit_move_assignment_function->entity_specs.is_member = 1;
            implicit_move_assignment_function->entity_specs.access = AS_PUBLIC;
            implicit_move_assignment_function->entity_specs.class_type = type_info;
            implicit_move_assignment_function->entity_specs.is_inline = 1;

            implicit_move_assignment_function->type_information = move_assignment_type;

            implicit_move_assignment_function->defined = 1;

            implicit_move_assignment_function->entity_specs.num_parameters = 1;
            implicit_move_assignment_function->entity_specs.default_argument_info = empty_default_argument_info(1);

            implicit_move_assignment_function->entity_specs.is_move_assignment_operator = 1;

            class_type_add_member(class_type, implicit_move_assignment_function, /* is_definition */ 1);

            move_assignment_operator_determine_if_trivial(
                    implicit_move_assignment_function,
                    direct_base_classes,
                    nonstatic_data_members,
                    has_virtual_bases,
                    has_virtual_functions);
        }
    }
    else
    {
        // Check for triviality of defaulted move assignment operators
        scope_entry_list_iterator_t *it;
        for (it = entry_list_iterator_begin(user_declared_move_assignment_operators);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* current_assignment_operator = entry_list_iterator_current(it);
            if (current_assignment_operator->entity_specs.is_defaulted)
            {
                move_assignment_operator_determine_if_trivial(
                        current_assignment_operator,
                        direct_base_classes,
                        nonstatic_data_members,
                        has_virtual_bases,
                        has_virtual_functions);
            }
        }
        entry_list_iterator_free(it);
    }

    // Implicit destructor
    if (user_declared_destructor == NULL)
    {
        const char* destructor_name = NULL;
        if (is_named_class_type(type_info))
        {
            uniquestr_sprintf(&destructor_name, "~%s", named_type_get_symbol(type_info)->symbol_name);
        }
        else
        {
            uniquestr_sprintf(&destructor_name, "%s", "~destructor");
        }

        scope_entry_t* implicit_destructor = new_symbol(class_type_get_inner_context(class_type),
                class_scope, 
                destructor_name);

        type_t* destructor_type = get_const_qualified_type(
                get_new_function_type(
                    /* returns void */ get_void_type(), 
                    NULL, 0, REF_QUALIFIER_NONE));

        implicit_destructor->kind = SK_FUNCTION;
        implicit_destructor->locus = locus;
        implicit_destructor->type_information = destructor_type;
        implicit_destructor->entity_specs.is_member = 1;
        implicit_destructor->entity_specs.access = AS_PUBLIC;
        implicit_destructor->entity_specs.is_destructor = 1;
        implicit_destructor->entity_specs.class_type = type_info;
        implicit_destructor->entity_specs.is_inline = 1;
        implicit_destructor->defined = 1;

        implicit_destructor->entity_specs.num_parameters = 0;
        class_type_add_member(class_type, implicit_destructor, /* is_definition */ 1);
        class_type_set_destructor(class_type, implicit_destructor);
        if (is_virtual_destructor(class_type))
        {
            implicit_destructor->entity_specs.is_virtual = 1;
        }

        destructor_determine_if_trivial(
                implicit_destructor,
                all_bases,
                nonstatic_data_members);
    }
    else
    {
        if (user_declared_destructor->entity_specs.is_defaulted)
        {
            destructor_determine_if_trivial(
                    user_declared_destructor,
                    all_bases,
                    nonstatic_data_members);
        }
    }

    // Free temporary lists used in the function

    entry_list_free(all_bases);
    entry_list_free(nonstatic_data_members);
    entry_list_free(direct_base_classes);
    entry_list_free(virtual_base_classes);

    entry_list_free(user_declared_move_constructors);
    entry_list_free(user_declared_copy_constructors);
    entry_list_free(user_declared_move_assignment_operators);
    entry_list_free(user_declared_copy_assignment_operators);

    DEBUG_CODE()
    {
        fprintf(stderr, "BUILDSCOPE: Ended class finalization\n");
    }
}

void finish_class_type(type_t* class_type, type_t* type_info, decl_context_t decl_context,
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    CXX_LANGUAGE()
    {
        finish_class_type_cxx(class_type, type_info, decl_context, locus, nodecl_output);
    }
}

// Delayed member function declarators (due to nonstatic member initialization)

struct delayed_member_initializer_tag
{
    scope_entry_t* entry;
    AST initializer;
};

static int _next_delayed_member_initializer = 0;
static struct delayed_member_initializer_tag _delayed_member_initializer[MCXX_MAX_FUNCTIONS_PER_CLASS];

static void build_scope_add_delayed_member_declarator_initializer(scope_entry_t* entry, AST initializer)
{
    ERROR_CONDITION(_next_delayed_member_initializer == MCXX_MAX_FUNCTIONS_PER_CLASS, "Too many function declarations delayed\n", 0);

    _delayed_member_initializer[_next_delayed_member_initializer].entry = entry;
    _delayed_member_initializer[_next_delayed_member_initializer].initializer = initializer;
    _next_delayed_member_initializer++;
}

static void build_scope_delayed_member_declarator_initializer_clear_pending(void)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "BUILDSCOPE: Clearing pending member declarator initializer\n");
    }
    _next_delayed_member_initializer = 0;
}

static void build_scope_delayed_member_declarator_initializers(void)
{
    int i;
    for (i = 0; i < _next_delayed_member_initializer; i++)
    {
        nodecl_t nodecl_init = nodecl_null();

        scope_entry_t* entry = _delayed_member_initializer[i].entry;
        AST initializer = _delayed_member_initializer[i].initializer;

        check_initialization(initializer,
                entry->decl_context,
                entry,
                get_unqualified_type(entry->type_information),
                &nodecl_init,
                // There should not be an auto type-specifier in a nonstatic member declarator
                /* is_auto_type */ 0);

        entry->value = nodecl_init;
    }
    build_scope_delayed_member_declarator_initializer_clear_pending();
}

// Delayed member function declarations (due to default arguments)
struct delayed_function_decl_tag
{
    scope_entry_t* entry;
    decl_context_t decl_context;
};

static int _next_delayed_function_decl = 0;
static struct delayed_function_decl_tag _delayed_functions_decl_list[MCXX_MAX_FUNCTIONS_PER_CLASS];

static void build_scope_delayed_add_function_declaration(scope_entry_t* entry, decl_context_t decl_context)
{
    ERROR_CONDITION(_next_delayed_function_decl == MCXX_MAX_FUNCTIONS_PER_CLASS, "Too many function declarations delayed\n", 0);

    _delayed_functions_decl_list[_next_delayed_function_decl].entry = entry;
    _delayed_functions_decl_list[_next_delayed_function_decl].decl_context = decl_context;
    _next_delayed_function_decl++;
}

static void build_scope_delayed_function_decl_clear_pending(void)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "BUILDSCOPE: Clearing pending function declarations\n");
    }
    _next_delayed_function_decl = 0;
}

static void build_noexcept_spec(type_t* function_type UNUSED_PARAMETER,
        AST a, decl_context_t decl_context,
        nodecl_t* nodecl_output);

static void build_noexcept_spec_delayed(scope_entry_t* entry)
{
    AST tree = nodecl_get_ast(nodecl_get_child(entry->entity_specs.noexception, 0));
    ERROR_CONDITION(tree == NULL, "Invalid tree", 0);

    DEBUG_CODE()
    {
        fprintf(stderr, "=== Delayed default argument parsing at '%s' ===\n",
                ast_location(tree));
    }

    decl_context_t relevant_context = entry->decl_context;

    if (entry->entity_specs.num_related_symbols > 0)
    {
        // Use the context of the parameters if possible
        relevant_context = entry->entity_specs.related_symbols[0]->decl_context;
    }

    build_noexcept_spec(entry->type_information,
            tree,
            relevant_context,
            &entry->entity_specs.noexception);
}

static void build_scope_delayed_function_decl(void)
{
    int i;
    for (i = 0; i < _next_delayed_function_decl; i++)
    {
        scope_entry_t* entry = _delayed_functions_decl_list[i].entry;
        decl_context_t decl_context = _delayed_functions_decl_list[i].decl_context;

        int num_parameters = function_type_get_num_parameters(entry->type_information);
        if (function_type_get_has_ellipsis(entry->type_information)) 
            num_parameters--;
        int j;
        for (j = 0; j < num_parameters; j++)
        {
            if (entry->entity_specs.default_argument_info[j] != NULL
                    && nodecl_get_kind(entry->entity_specs.default_argument_info[j]->argument) == NODECL_CXX_PARSE_LATER)
            {
                // Let's parse it now
                AST tree = nodecl_get_ast(nodecl_get_child(entry->entity_specs.default_argument_info[j]->argument, 0));
                ERROR_CONDITION(tree == NULL, "Invalid tree", 0);

                DEBUG_CODE()
                {
                    fprintf(stderr, "=== Delayed default argument parsing at '%s' ===\n",
                            ast_location(tree));
                }

                check_expression(tree, decl_context,
                        &(entry->entity_specs.default_argument_info[j]->argument));
            }
        }

        if (!nodecl_is_null(entry->entity_specs.noexception)
                && nodecl_get_kind(entry->entity_specs.noexception) == NODECL_CXX_PARSE_LATER)
        {
            build_noexcept_spec_delayed(entry);
        }
    }
    build_scope_delayed_function_decl_clear_pending();
}

// Delayed member function definitions

struct delayed_function_def_tag
{
    AST function_definition;
    scope_entry_t* entry;
    decl_context_t block_context;
    gather_decl_spec_t* gather_info;
};

static int _next_delayed_function_def = 0;
static struct delayed_function_def_tag _delayed_functions_def_list[MCXX_MAX_FUNCTIONS_PER_CLASS];

static void build_scope_delayed_add_delayed_function_def(AST function_definition,
        scope_entry_t* entry,
        decl_context_t block_context,
        gather_decl_spec_t* gather_info)
{
    ERROR_CONDITION(_next_delayed_function_def == MCXX_MAX_FUNCTIONS_PER_CLASS,
            "Too many delayed member functions!\n", 0);

    DEBUG_CODE()
    {
        fprintf(stderr, "BUILDSCOPE: Adding '%s' function definition for delayed processing\n",
                ast_location(function_definition));
    }

    _delayed_functions_def_list[_next_delayed_function_def].function_definition = function_definition;
    _delayed_functions_def_list[_next_delayed_function_def].entry = entry;
    _delayed_functions_def_list[_next_delayed_function_def].block_context = block_context;
    _delayed_functions_def_list[_next_delayed_function_def].gather_info = gather_info;
    _next_delayed_function_def++;
}

static void build_scope_delayed_function_def_clear_pending(void)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "BUILDSCOPE: Clearing pending function definition\n");
    }
    _next_delayed_function_def = 0;
}

static void build_scope_function_definition_body(
        AST function_definition,
        scope_entry_t* entry,
        decl_context_t block_context,
        gather_decl_spec_t* gather_info,
        nodecl_t *nodecl_output);

static void build_scope_delayed_function_def(nodecl_t* nodecl_output)
{
    int i;
    for (i = 0;  i < _next_delayed_function_def; i++)
    {
        struct delayed_function_def_tag current = _delayed_functions_def_list[i];

        AST function_definition = current.function_definition;
        decl_context_t block_context = current.block_context;
        scope_entry_t* entry = current.entry;
        gather_decl_spec_t* gather_info = current.gather_info;

        DEBUG_CODE()
        {
            fprintf(stderr, "=== Delayed member function definition at '%s' ===\n",
                    ast_location(function_definition));
        }

        nodecl_t nodecl_function_definition = nodecl_null();

        build_scope_function_definition_body(
                function_definition,
                entry,
                block_context,
                gather_info,
                &nodecl_function_definition);

        xfree(gather_info);

        *nodecl_output = nodecl_concat_lists(*nodecl_output, nodecl_function_definition);
    }
    build_scope_delayed_function_def_clear_pending();
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
            fprintf(stderr, "BUILDSCOPE: Building scope of delayed member declarator initializers\n");
        }
        build_scope_delayed_member_declarator_initializers();

        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: Building scope of delayed function declarations\n");
        }
        build_scope_delayed_function_decl();

        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: Building scope of delayed function definitions\n");
        }
        build_scope_delayed_function_def(nodecl_output);
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

static void insert_symbols_in_enclosing_context(decl_context_t enclosing_context,
        scope_entry_t* class_symbol,
        scope_entry_t* accessor_symbol)
{
    scope_entry_list_t* members = class_type_get_nonstatic_data_members(class_symbol->type_information);

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(members);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* member = entry_list_iterator_current(it);
        if (!member->entity_specs.is_member_of_anonymous)
        {
            member->entity_specs.is_member_of_anonymous = 1;
            member->entity_specs.anonymous_accessor =
                nodecl_make_symbol(accessor_symbol, accessor_symbol->locus);
        }
        else
        {
            member->entity_specs.is_member_of_anonymous = 1;

            nodecl_t nodecl_symbol = nodecl_make_symbol(accessor_symbol,
                             accessor_symbol->locus);
            nodecl_set_type(nodecl_symbol, lvalue_ref(accessor_symbol->type_information));

            nodecl_t nodecl_accessor = cxx_integrate_field_accesses(nodecl_symbol,
                    member->entity_specs.anonymous_accessor);
            nodecl_set_type(nodecl_accessor, lvalue_ref(member->type_information));
            nodecl_set_locus(nodecl_accessor, accessor_symbol->locus);

            member->entity_specs.anonymous_accessor = nodecl_accessor;
        }
        insert_entry(enclosing_context.current_scope, member);

        // If the members happen to be one of those faked members to access an
        // anonymous union then recursively add its members to the current scope
        if (member->kind == SK_VARIABLE
                && is_named_class_type(member->type_information)
                && named_type_get_symbol(member->type_information)->entity_specs.is_anonymous_union)
        {
            insert_symbols_in_enclosing_context(enclosing_context,
                    named_type_get_symbol(member->type_information),
                    accessor_symbol);
        }
    }
    entry_list_iterator_free(it);
    entry_list_free(members);
}

scope_entry_t* finish_anonymous_class(scope_entry_t* class_symbol, decl_context_t decl_context)
{
    ERROR_CONDITION(!class_symbol->entity_specs.is_anonymous_union, "This class is not anonymous", 0);

    // Sign in a fake symbol in the context of the class
    const char* accessing_name = class_symbol->symbol_name;
    C_LANGUAGE()
    {
        // Transform "union X" or "struct X" -> "X"
        const char* c = NULL;
        if ((c = has_prefix("union ", accessing_name)) != NULL)
        { }
        else if ((c = has_prefix("struct ", accessing_name)) != NULL)
        { }

        ERROR_CONDITION(c == NULL, "Invalid struct/union name in C '%s'\n", accessing_name);

        accessing_name = uniquestr(c);
    }
    accessing_name = strappend("var_", accessing_name);

    scope_entry_t* accessor_symbol = new_symbol(class_symbol->decl_context,
            class_symbol->decl_context.current_scope,
            accessing_name);

    accessor_symbol->kind = SK_VARIABLE;
    accessor_symbol->locus = class_symbol->locus;
    accessor_symbol->type_information = get_user_defined_type(class_symbol);

    class_symbol->entity_specs.anonymous_accessor =
        nodecl_make_symbol(accessor_symbol, class_symbol->locus);

    // Sign in members in the appropiate enclosing scope
    insert_symbols_in_enclosing_context(decl_context, class_symbol, accessor_symbol);

    return accessor_symbol;
}

/*
 * This function is called for class specifiers
 */
void gather_type_spec_from_class_specifier(AST a, type_t** type_info,
        gather_decl_spec_t* gather_info,
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
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

    C_LANGUAGE()
    {
        // In C flatten nested structures otherwise it becomes crazy (because
        // of C irregularities in this point) to handle them
        if (decl_context.current_scope->kind == CLASS_SCOPE)
        {
            decl_context = decl_context.current_scope->related_entry->decl_context;
        }
    }

    if (gather_info->is_friend
            && gather_info->no_declarators)
    {
        error_printf("%s: error: friend applied to class definition\n",
                ast_location(a));
        *type_info = get_error_type();
        return;
    }

    scope_entry_t* class_entry = NULL;
    type_t* class_type = NULL;
    /* --- */

    enter_class_specifier();

    AST class_head = ASTSon0(a);

    AST class_key = ASTSon0(class_head);
    AST class_id_expression = ASTSon1(class_head);
    AST base_clause = ASTSon2(class_head);

    AST extra_attributes = NULL;
    AST class_virt_specifiers = NULL;

    AST class_head_extra = ASTSon3(class_head);
    if (class_head_extra != NULL)
    {
        extra_attributes = ASTSon0(class_head_extra);
        class_virt_specifiers = ASTSon1(class_head_extra);
    }

    gather_extra_attributes(extra_attributes, gather_info, decl_context);
    gather_virt_specifiers(class_virt_specifiers, gather_info, decl_context);

    enum type_tag_t class_kind = TT_INVALID;
    const char *class_kind_name = NULL;

    switch (ASTType(class_key))
    {
        case AST_CLASS_KEY_CLASS:
            {
                class_kind = TT_CLASS;
                class_kind_name = "class";
                break;
            }
        case AST_CLASS_KEY_STRUCT:
            {
                class_kind = TT_STRUCT;
                class_kind_name = "struct";
                break;
            }
        case AST_CLASS_KEY_UNION:
            {
                class_kind = TT_UNION;
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
                        class_id_expression, NULL);
            }
            else
            {
                // If the template specialization was already declared
                // we want to update its template arguments properly
                class_entry_list = query_id_expression(decl_context,
                        class_id_expression, NULL);
            }
        }

        C_LANGUAGE()
        {
            // This can only be an AST_SYMBOL in C
            const char* class_name = ASTText(class_id_expression);
            class_name = strappend(class_kind_name, strappend(" ", class_name));

            class_entry_list = query_name_str(decl_context, class_name, NULL);
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
                scope_entry_t* template_sym = class_entry;
                if (decl_context.template_parameters == NULL)
                {
                    error_printf("%s: error: template parameters required for declaration of '%s'\n",
                            ast_location(class_id_expression),
                            get_qualified_symbol_name(template_sym, decl_context));
                    *type_info = get_error_type();
                    return;
                }

                if (decl_context.template_parameters->num_parameters
                        != template_type_get_template_parameters(template_sym->type_information)->num_parameters)
                {
                    error_printf("%s: error: redeclaration with %d template parameters while previous declaration used %d\n",
                            ast_location(class_id_expression),
                            decl_context.template_parameters->num_parameters,
                            template_type_get_template_parameters(template_sym->type_information)->num_parameters);
                    *type_info = get_error_type();
                    return;
                }

                template_type_update_template_parameters(template_sym->type_information,
                        decl_context.template_parameters);

                // If it was friend-declared, it is not anymore
                if (template_sym->entity_specs.is_friend_declared)
                    template_sym->entity_specs.is_friend_declared = 0;

                // This is a named type
                type_t* primary_type = template_type_get_primary_type(template_sym->type_information);

                class_entry = named_type_get_symbol(primary_type);
                class_type = class_entry->type_information;


                if (is_template_explicit_specialization(decl_context.template_parameters))
                {
                    // We are declaring a template class nested in at least one class
                    // explicit specialization
                    type_t* new_class_type =
                        get_new_class_type(class_entry->decl_context, class_kind);

                    // Propagate 'is_dependent' type property
                    set_is_dependent_type(new_class_type, is_dependent_type(class_entry->type_information));

                    class_entry->type_information = new_class_type;

                    // This is the only legitimate use of this function
                    set_as_template_specialized_type(
                            class_entry->type_information,
                            class_entry->decl_context.template_parameters,
                            template_sym->type_information);

                    class_type = class_entry->type_information;
                    inner_decl_context = new_class_context(class_entry->decl_context, class_entry);
                    class_type_set_inner_context(class_type, inner_decl_context);
                }

                ERROR_CONDITION((class_entry->kind != SK_CLASS
                            && class_entry->kind != SK_FUNCTION), 
                        "Invalid symbol type for a template entity\n", 0);

                if (class_entry->kind == SK_FUNCTION)
                {
                    error_printf("%s: error: invalid template-name redeclaration\n", 
                            ast_location(class_id_expression));
                    *type_info = get_error_type();
                    return;
                }

                ERROR_CONDITION(!is_class_type(class_entry->type_information), "This must be a class type", 0);
            }

            nesting_check_t nest_check = check_template_nesting_of_name(class_entry, decl_context.template_parameters);

            if (nest_check != NESTING_CHECK_OK)
            {
                if (nest_check == NESTING_CHECK_NOT_A_TEMPLATE)
                {
                    error_printf("%s: error: '%s' is not a template type\n", 
                            ast_location(class_id_expression),
                            get_qualified_symbol_name(class_entry, decl_context));
                }
                else if (nest_check == NESTING_CHECK_INVALID)
                {
                    error_printf("%s: error: invalid nesting of template parameters in template declaration\n",
                            ast_location(class_id_expression));
                    error_printf("%s: error: there are %d levels of template parameters but the symbol required exactly %d levels\n", 
                            ast_location(class_id_expression),
                            get_template_nesting_of_context(decl_context),
                            get_template_nesting_of_context(class_entry->decl_context));
                }
                else
                {
                    internal_error("Code unreachable", 0);
                }
                *type_info = get_error_type();
                return;
            }

            // If it was friend-declared, it is not anymore
            if (class_entry->entity_specs.is_friend_declared)
                class_entry->entity_specs.is_friend_declared = 0;

            DEBUG_CODE()
            {
                fprintf(stderr, "BUILDSCOPE: Class '%s' already declared as %p in scope %p (%s)\n", 
                        prettyprint_in_buffer(class_id_expression),
                        class_entry, 
                        class_entry->decl_context.current_scope,
                        locus_to_str(class_entry->locus));
            }

            if (class_entry->defined
                    || (class_entry->entity_specs.alias_to != NULL
                        && class_entry->entity_specs.alias_to->defined))
            {
                error_printf("%s: class '%s' already defined in %s\n",
                        ast_location(class_id_expression),
                        get_qualified_symbol_name(class_entry, class_entry->decl_context),
                        locus_to_str(class_symbol_get_canonical_symbol(class_entry)->locus));
                *type_info = get_error_type();
                return;
            }

            if (is_template_specialized_type(class_entry->type_information))
            {
                // Check the enclosing namespace scope
                // This is only valid if the scope of the entry is an inlined namespace of the current one
                if ((class_entry->decl_context.namespace_scope != decl_context.namespace_scope)
                        && !is_inline_namespace_of(class_entry->decl_context, decl_context))
                {
                    error_printf("%s: specialization of '%s' in different namespace from definition\n",
                            ast_location(class_id_expression),
                            prettyprint_in_buffer(class_id_expression));
                    *type_info = get_error_type();
                    return;
                }

                if (!gather_info->is_explicit_specialization)
                {
                    template_specialized_type_update_template_parameters(class_entry->type_information,
                            decl_context.template_parameters);
                }
            }

            // Update the template_scope
            DEBUG_CODE()
            {
                fprintf(stderr, "BUILDSCOPE: Updating template scope\n");
            }
            class_entry->decl_context.template_parameters = decl_context.template_parameters;
            inner_decl_context = new_class_context(class_entry->decl_context, class_entry);

            // Remove empty template headers if they are produced by an explicit template specialization
            while (inner_decl_context.template_parameters != NULL &&
                    inner_decl_context.template_parameters->is_explicit_specialization)
            {
                // We should ignore the innermost template header because it's a explicit
                // specialization declaration (we don't need them)
                // Example:
                //
                // template < typename T>
                //    struct A
                //    {
                //        void f(T);
                //    };
                //
                // template <>
                //    struct A<float>
                //    {
                //        void f(int*);
                //    };
                //
                // void A<float>::f(int*) {} OK!

                inner_decl_context.template_parameters = inner_decl_context.template_parameters->enclosing;
            }

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
                error_printf("%s: error: template class-name '%s' not found in the current scope\n",
                        ast_location(class_id_expression),
                        prettyprint_in_buffer(ASTSon0(class_id_expression)));
                *type_info = get_error_type();
                return;
            }
            else
            {
                internal_error("Code unreachable", 0);
            }

            DEBUG_CODE()
            {
                fprintf(stderr, "BUILDSCOPE: Registering class '%s' (%p) in scope %p\n", 
                        prettyprint_in_buffer(class_id_expression), class_entry, decl_context.current_scope);
            }

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
                    if (!check_class_template_parameters(ast_get_locus(a), decl_context.template_parameters))
                    {
                        *type_info = get_error_type();
                        return;
                    }

                    class_entry->kind = SK_TEMPLATE;
                    class_entry->type_information = get_new_template_type(decl_context.template_parameters,
                            get_new_class_type(decl_context, class_kind),
                            ASTText(class_id_expression), decl_context,
                            ast_get_locus(class_id_expression));
                    template_type_set_related_symbol(class_entry->type_information, class_entry);

                    class_entry->locus = ast_get_locus(class_id_expression);

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

                    class_type = class_entry->type_information;
                }
                else
                {
                    error_printf("%s: error: invalid template-name '%s'\n", 
                            ast_location(class_id_expression),
                            prettyprint_in_buffer(class_id_expression));
                    *type_info = get_error_type();
                    return;
                }
            }

            class_entry->locus = ast_get_locus(class_id_expression);
            class_symbol_get_canonical_symbol(class_entry)->locus = ast_get_locus(class_id_expression);

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
            error_printf("%s: error: class '%s' not found\n",
                    ast_location(a), prettyprint_in_buffer(class_id_expression));
            *type_info = get_error_type();
            return;
        }
    }
    else
    {
        // Give it a fake name
        static int anonymous_classes = 0;

        const char* symbol_name;
        C_LANGUAGE()
        {
            uniquestr_sprintf(&symbol_name, "%s mcc_%s_anon_%d", class_kind_name, class_kind_name, anonymous_classes);
            class_entry = new_symbol(decl_context, decl_context.current_scope, symbol_name);
        }
        CXX_LANGUAGE()
        {
            uniquestr_sprintf(&symbol_name, "mcc_%s_anon_%d", class_kind_name, anonymous_classes);
            class_entry = counted_xcalloc(1, sizeof(*class_entry), &_bytes_used_buildscope);
            class_entry->symbol_name = symbol_name;
            class_entry->decl_context = decl_context;
        }
        anonymous_classes++;

        class_entry->kind = SK_CLASS;
        class_entry->type_information = get_new_class_type(decl_context, class_kind);
        class_type = class_entry->type_information;

        class_entry->locus = ast_get_locus(a);

        class_entry->entity_specs.is_unnamed = 1;

        class_type = class_entry->type_information;
        inner_decl_context = new_class_context(decl_context, class_entry);
        class_type_set_inner_context(class_type, inner_decl_context);

        class_entry->entity_specs.is_anonymous_union = gather_info->no_declarators;
    }

    ERROR_CONDITION(inner_decl_context.current_scope == NULL,
            "The inner context was incorrectly set", 0);

    gather_info->defined_type = class_entry;

    C_LANGUAGE()
    {
        if (class_kind == TT_UNION
                && gather_info->is_transparent_union)
        {
            set_is_transparent_union(class_type, /* is_transparent_union */ 1);
        }
    }

    // Compute *type_info as it is needed by build_scope_member_specification
    *type_info = get_user_defined_type(class_entry);

    // Save the class symbol

    // If the class is being declared in class-scope it means
    // it is a nested class
    if (decl_context.current_scope->kind == CLASS_SCOPE)
    {
        scope_entry_t* enclosing_class_symbol = decl_context.current_scope->related_entry;
        type_t* enclosing_class_type = enclosing_class_symbol->type_information;

        class_type_add_member(enclosing_class_type, class_entry, /* is_definition */ 1);

        CXX_LANGUAGE()
        {
            class_entry->entity_specs.is_member = 1;
            class_entry->entity_specs.access = gather_info->current_access;
            class_entry->entity_specs.class_type = get_user_defined_type(enclosing_class_symbol);
            class_entry->entity_specs.is_defined_inside_class_specifier = 1;
        }
        class_type_set_enclosing_class_type(class_type, get_user_defined_type(enclosing_class_symbol));

        // If the enclosing class is dependent, so is this one
        char c = is_dependent_type(class_entry->type_information);
        c = c || is_dependent_type(enclosing_class_type);
        set_is_dependent_type(class_entry->type_information, c);
    }
    else if (decl_context.current_scope->kind == BLOCK_SCOPE)
    {
        // This is a local class
        scope_entry_t* enclosing_function = decl_context.current_scope->related_entry;
        if (enclosing_function != NULL
                && (is_dependent_type(enclosing_function->type_information)
                    || (enclosing_function->entity_specs.is_member
                        && is_dependent_type(enclosing_function->entity_specs.class_type))))
        {
            set_is_dependent_type(class_entry->type_information, 1);
        }
    }

    // The inner scope is properly adjusted here thus we can link it with the AST

    // Build scope of members
    AST member_specification = ASTSon1(a);

    // Now add the bases
    if (base_clause != NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: Adding the bases of this class\n");
        }

        build_scope_base_clause(base_clause, 
                class_entry, 
                inner_decl_context);

        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: Bases added\n");
        }
    }

    // Inject the class symbol in the scope if not unnamed
    CXX_LANGUAGE()
    {
        if (!class_entry->entity_specs.is_unnamed)
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


        // Create a 'this' only used for class-scope lexical scopes
        type_t* pointed_this = get_user_defined_type(class_entry);
        type_t* this_type = get_pointer_type(pointed_this);
        this_type = get_cv_qualified_type(this_type, CV_CONST);

        // This symbol must be detached because we do not want it be found
        // through any sort of lookup. It will be accessible throgh
        // the related_symbols of the class symbol
        scope_entry_t* this_symbol = xcalloc(1, sizeof(*this_symbol));
        this_symbol->symbol_name = UNIQUESTR_LITERAL("this");
        this_symbol->decl_context = inner_decl_context;
        this_symbol->locus = ast_get_locus(a);
        this_symbol->kind = SK_VARIABLE;
        this_symbol->type_information = this_type;
        this_symbol->defined = 1;
        this_symbol->do_not_print = 1;

        P_LIST_ADD(class_entry->entity_specs.related_symbols,
                class_entry->entity_specs.num_related_symbols,
                this_symbol);
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

    class_entry->entity_specs.is_user_declared = 1;
    if (class_type_is_incomplete_independent(class_entry->type_information))
    {
        class_entry->entity_specs.is_instantiated = 1;
    }

    linkage_push(NULL, /* is_braced */ 1);

    build_scope_member_specification(inner_decl_context, member_specification, 
            current_access, *type_info, nodecl_output, 
            /* declared_symbols */ NULL, /* gather_decl_spec_list_t */ NULL);

    linkage_pop();

    class_entry->defined = 1;
    class_symbol_get_canonical_symbol(class_entry)->defined = 1;

    nodecl_t nodecl_finish_class = nodecl_null();
    finish_class_type(class_type, *type_info, decl_context, ast_get_locus(a), &nodecl_finish_class);
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

    class_entry->entity_specs.is_instantiable = 1;

    keep_gcc_attributes_in_symbol(class_entry, gather_info);
    keep_ms_declspecs_in_symbol(class_entry, gather_info);

    // Keep class-virt-specifiers
    class_entry->entity_specs.is_explicit = gather_info->is_explicit;
    class_entry->entity_specs.is_final = gather_info->is_final;

    // Propagate the __extension__ attribute to the symbol
    class_entry->entity_specs.gcc_extension = gcc_extension;

    CXX_LANGUAGE()
    {
        nodecl_t nodecl_context =
            nodecl_make_context(/* optional statement sequence */ nodecl_null(),
                    decl_context, ast_get_locus(a));

        *nodecl_output =
            nodecl_concat_lists(
                    *nodecl_output,
                    nodecl_make_list_1(
                        nodecl_make_cxx_def(
                            nodecl_context,
                            class_entry,
                            ast_get_locus(a))));
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
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list)
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
                    nodecl_output, 
                    declared_symbols, 
                    gather_decl_spec_list);
        }
    }
}

static void build_scope_member_specification(decl_context_t inner_decl_context, AST member_specification_tree, 
        access_specifier_t default_current_access, type_t* type_info, nodecl_t *nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list)
{
    ERROR_CONDITION(inner_decl_context.current_scope->kind != CLASS_SCOPE,
            "Error, current scope should be a class scope", 0);

    // First pass, sign up only prototypes and simple declarations and
    // queue function definitions
    // FIXME
    build_scope_member_specification_first_step(inner_decl_context, member_specification_tree, 
            default_current_access, type_info, 
            nodecl_output, declared_symbols, gather_decl_spec_list);
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
            gather_info, type_info, declarator_type, decl_context,
            /* prototype_context */ NULL, nodecl_output);
}

static void register_this_symbol(decl_context_t decl_context,
        scope_entry_t* class_symbol,
        const locus_t* locus)
{
    ERROR_CONDITION(class_symbol == NULL || class_symbol->kind != SK_CLASS, "Invalid class", 0);

    // Early registration of 'this' to be used in the declarator Its exact type
    // will be updated prior analyzing the body of the function (e.g. when the
    // function is const)
    type_t* pointed_this = get_user_defined_type(class_symbol);
    type_t* this_type = get_pointer_type(pointed_this);
    // It is a constant pointer, so qualify like it is
    this_type = get_cv_qualified_type(this_type, CV_CONST);

    scope_entry_t* this_symbol = new_symbol(decl_context, decl_context.current_scope, UNIQUESTR_LITERAL("this"));

    this_symbol->locus = locus;

    this_symbol->kind = SK_VARIABLE;
    this_symbol->type_information = this_type;
    this_symbol->defined = 1;
    this_symbol->do_not_print = 1;
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
        gather_extra_attributes_in_declarator(a, gather_info, decl_context);
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
                && !is_floating_type(*declarator_type)
                && !is_complex_type(*declarator_type))
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
            if (ASTType(declarator_name) == AST_QUALIFIED_ID)
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
                        global_op, nested_name, name, NULL, decl_flags);

                if (symbols == NULL)
                {
                    error_printf("%s: error: qualified name '%s' not found\n",
                            ast_location(declarator_name),
                            prettyprint_in_buffer(declarator_name));
                    *declarator_type = get_error_type();
                    return;
                }

                scope_entry_t* first_symbol = entry_list_head(symbols);
                entry_list_free(symbols);

                // Update the entity context, inheriting the template_scope
                entity_context = first_symbol->decl_context;
                entity_context.template_parameters = decl_context.template_parameters;

                if (prototype_context != NULL)
                {
                    prototype_context->current_scope->contained_in = first_symbol->decl_context.current_scope;
                    prototype_context->namespace_scope = first_symbol->decl_context.namespace_scope;
                    prototype_context->class_scope = first_symbol->decl_context.class_scope;

                }
            }
        }

        // Register 'this' for a successful parsing of the declarator
        if (prototype_context != NULL
                && prototype_context->current_scope->kind == BLOCK_SCOPE)
        {
            if (entity_context.current_scope->kind == CLASS_SCOPE)
            {
                register_this_symbol(*prototype_context,
                        entity_context.current_scope->related_entry,
                        ast_get_locus(a));
            }
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
                        get_conversion_function_name(entity_context, conversion_function_id, &conversion_function_type);
                        *declarator_type = get_new_function_type(conversion_function_type, 
                                /*parameter_info*/ NULL, /*num_parameters=*/0, REF_QUALIFIER_NONE);

                        // Keep the cv-qualification in the crafted type
                        *declarator_type = get_cv_qualified_type(*declarator_type, cv_qualif);
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
            // fprintf(stderr, "BUILDSCOPE: Computed type of '%s' is  '%s'\n", 
            //         prettyprint_in_buffer(a),
            //         print_declarator(*declarator_type));
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

                    entry_list = query_id_expression(decl_context, id_type_expr, NULL);

                    if (entry_list != NULL)
                    {
                        scope_entry_t* entry = entry_list_head(entry_list);

                        if (entry->entity_specs.is_injected_class_name)
                        {
                            // Advance this case as it will lead to a simpler type-id
                            entry = named_type_get_symbol(entry->entity_specs.class_type);
                        }

                        *declarator_type = get_pointer_to_member_type(pointee_type, get_user_defined_type(entry));
                    }
                    else
                    {
                        error_printf("%s: error: class-name '%s' not found\n", 
                                ast_location(id_type_expr),
                                prettyprint_in_buffer(id_type_expr));
                        *declarator_type = get_error_type();
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
        case AST_REBINDABLE_REFERENCE_SPEC:
            {
                if (!is_lvalue_reference_type(pointee_type)
                        && !is_rvalue_reference_type(pointee_type))
                {
                    *declarator_type = get_rebindable_reference_type(pointee_type);
                }
                else
                {
                    internal_error("Rebindable references to existing reference types should not be created", 0);
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

static int vla_counter = -1;
int get_vla_counter(void)
{
    vla_counter++;
    return vla_counter;
}

/*
 * This function converts a type "T" to a "array x of T"
 */
static void set_array_type(type_t** declarator_type, 
        AST constant_expr, AST static_qualifier UNUSED_PARAMETER, 
        AST cv_qualifier_seq UNUSED_PARAMETER,
        gather_decl_spec_t* gather_info, 
        decl_context_t decl_context,
        const locus_t* locus)
{
    type_t* element_type = *declarator_type;

    if (element_type == NULL)
    {
        error_printf("%s: error: array declaration without a type-specifier\n",
                locus_to_str(locus));
        *declarator_type = get_error_type();
        return;
    }

    nodecl_t nodecl_expr = nodecl_null();
    if (constant_expr != NULL)
    {
        if (!check_expression(constant_expr, decl_context, &nodecl_expr))
        {
            error_printf("%s: error: could not check array size expression '%s'\n",
                    ast_location(constant_expr),
                    prettyprint_in_buffer(constant_expr));

            *declarator_type = get_error_type();
            return;
        }

        if (!nodecl_expr_is_value_dependent(nodecl_expr)
                && !nodecl_is_constant(nodecl_expr))
        {
            if (decl_context.current_scope->kind == NAMESPACE_SCOPE
                    || decl_context.current_scope->kind == CLASS_SCOPE)
            {
                error_printf("%s: error: declaring a variable sized object in a scope not allowing them\n",
                        ast_location(constant_expr));
                *declarator_type = get_error_type();
                return;
            }
            // // Maybe we should check for decl_context.block_scope != NULL
            else if (decl_context.current_scope->kind == BLOCK_SCOPE
                    && !gather_info->is_cxx_new_declarator)
            {
                const char* vla_name = NULL;
                uniquestr_sprintf(&vla_name, "mcc_vla_%d", get_vla_counter());

                scope_entry_t* new_vla_dim = new_symbol(decl_context, decl_context.current_scope, vla_name);

                new_vla_dim->kind = SK_VARIABLE;
                new_vla_dim->locus = ast_get_locus(constant_expr);
                new_vla_dim->value = nodecl_expr;
                new_vla_dim->type_information = get_const_qualified_type(no_ref(nodecl_get_type(nodecl_expr)));

                // It's not user declared code, but we must generate it.
                // For this reason, we do this trick
                new_vla_dim->entity_specs.is_user_declared = 1;
                new_vla_dim->entity_specs.is_saved_expression = 1;

                P_LIST_ADD(gather_info->vla_dimension_symbols,
                        gather_info->num_vla_dimension_symbols,
                        new_vla_dim);

                nodecl_expr = nodecl_make_symbol(new_vla_dim, new_vla_dim->locus);
                nodecl_set_type(nodecl_expr, new_vla_dim->type_information);
            }
            else if (decl_context.current_scope->kind == PROTOTYPE_SCOPE)
            {
                // Do nothing, keep the expression as is, this is what we want, actually
            }
        }
    }

    if (is_void_type(element_type))
    {
        error_printf("%s: error: invalid array of void type '%s'\n",
                locus_to_str(locus),
                print_type_str(element_type, decl_context));
        *declarator_type = get_error_type();
        return;
    }

    C_LANGUAGE()
    {
        if (is_incomplete_type(element_type))
        {
            error_printf("%s: error: invalid array of incomplete type '%s'\n",
                    locus_to_str(locus),
                    print_type_str(element_type, decl_context));
            *declarator_type = get_error_type();
            return;
        }
    }
    CXX_LANGUAGE()
    {
        if (is_array_type(element_type)
                && array_type_is_unknown_size(element_type))
        {
            error_printf("%s: error: declaration of array type of an unbounded array type '%s'\n",
                    locus_to_str(locus),
                    print_type_str(element_type, decl_context));
            *declarator_type = get_error_type();
            return;
        }
    }
    *declarator_type = get_array_type(element_type, nodecl_expr, decl_context);
}

// Returns a fake symbol used only to keep track of variables in declarators
//
// We need to remember that symbols appearing in function declarators are parameters
// but they need a function to be parameters of, so we use this fake symbol
//
// Note that we only use this in C++, for C there is no need to keep track of
// this stuff
//
// By using get_function_declaration_proxy() in
//
//    symbol_is_parameter_of_function(symbol, get_function_declaration_proxy())
//    symbol_get_parameter_nesting_in_function(symbol, get_function_declaration_proxy())
//    symbol_get_parameter_position_in_function(symbol, get_function_declaration_proxy())
//
// you can tell if "symbol" is a parameter of a given declarator and retrieve
// its nesting and position in the declarator itself. Note that the nesting
// counts only nested function declarators (not any sort of derived declarator)
scope_entry_t* get_function_declaration_proxy(void)
{
    static scope_entry_t* _decl_proxy = NULL;

    if (_decl_proxy == NULL)
    {
        _decl_proxy = xcalloc(1, sizeof(*_decl_proxy));
        _decl_proxy->symbol_name = UNIQUESTR_LITERAL("._function_declarator_");
        _decl_proxy->kind = SK_FUNCTION;
    }

    return _decl_proxy;
}

/*
 * This function fetches information for every declarator in the
 * parameter_declaration_clause of a functional declarator
 */
static void set_function_parameter_clause(type_t** function_type, 
        AST parameters, AST ref_qualifier_opt,
        decl_context_t decl_context,
        gather_decl_spec_t* gather_info,
        nodecl_t* nodecl_output)
{
    parameter_info_t parameter_info[MCXX_MAX_FUNCTION_PARAMETERS];
    memset(parameter_info, 0, sizeof(parameter_info));
    int num_parameters = 0;

    if (ASTType(parameters) == AST_AMBIGUITY)
    {
        solve_ambiguous_parameter_clause(parameters, decl_context);
    }

    ref_qualifier_t ref_qualifier = REF_QUALIFIER_NONE;
    if (ref_qualifier_opt != NULL)
    {
        CXX03_LANGUAGE()
        {
            error_printf("%s: error: ref-qualifier is only valid in C++2011\n", ast_location(ref_qualifier_opt));
        }
        switch (ASTType(ref_qualifier_opt))
        {
            case AST_REFERENCE_SPEC:
                ref_qualifier = REF_QUALIFIER_LVALUE;
                break;
            case AST_RVALUE_REFERENCE_SPEC:
                ref_qualifier = REF_QUALIFIER_RVALUE;
                break;
            default:
                internal_error("Invalid ref-qualifier '%s'\n", ast_print_node_type(ASTType(ref_qualifier_opt)));
        }
    }

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
            *function_type = get_new_function_type(*function_type, parameter_info, /*num_parameters=*/0, ref_qualifier);
        }
        return;
    }

    AST iter = NULL;
    AST list = parameters;

    // K&R parameters
    if (ASTType(parameters) == AST_KR_PARAMETER_LIST)
    {
        list = ASTSon0(parameters);

        if (list != NULL)
        {
            for_each_element(list, iter)
            {
                if (num_parameters > MCXX_MAX_FUNCTION_PARAMETERS)
                {
                    error_printf("%s: error: too many parameters (more than %d) in function declaration", 
                            ast_location(parameters),
                            num_parameters);
                }

                // Clear this parameter_info 
                memset(&(parameter_info[num_parameters]), 0, sizeof(parameter_info[num_parameters]));
                AST kr_id = ASTSon1(iter);

                decl_context_t param_decl_context = decl_context;
                scope_entry_t* new_parameter = new_symbol(param_decl_context, 
                        param_decl_context.current_scope,
                        ASTText(kr_id));

                new_parameter->kind = SK_VARIABLE;
                new_parameter->type_information = get_signed_int_type();
                new_parameter->locus = ast_get_locus(kr_id);

                parameter_info[num_parameters].is_ellipsis = 0;
                parameter_info[num_parameters].type_info = get_signed_int_type();
                parameter_info[num_parameters].nonadjusted_type_info = get_signed_int_type();

                arguments_info_t new_argument_info;
                new_argument_info.entry = new_parameter;
                new_argument_info.argument = nodecl_null();
                new_argument_info.context = decl_context;

                P_LIST_ADD(gather_info->arguments_info,
                        gather_info->num_arguments_info,
                        new_argument_info);

                num_parameters++;
            }
        }

        *function_type = get_nonproto_function_type(*function_type, num_parameters);
        return;
    }
    else
    {
        static int function_declarator_nesting_level = 0;
        for_each_element(list, iter)
        {
            if (num_parameters > MCXX_MAX_FUNCTION_PARAMETERS)
            {
                error_printf("%s: error: too many parameters (more than %d) in function declaration", 
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
                parameter_info[num_parameters].type_info = get_ellipsis_type();
                num_parameters++;
                continue;
            }

            ERROR_CONDITION(ASTType(parameter_declaration) != AST_PARAMETER_DECL,
                    "Invalid node", 0);

            // This is never null
            AST parameter_decl_spec_seq = ASTSon0(parameter_declaration);
            // Declarator can be null
            AST parameter_declarator = ASTSon1(parameter_declaration);
            // Default value can be null
            // The scope of this parameter declaration should be "st" and not parameters_scope
            AST default_argument = ASTSon2(parameter_declaration);
            nodecl_t nodecl_default_argument = nodecl_null();

            // Check the default argument
            if (default_argument != NULL)
            {
                if (gather_info->inside_class_specifier)
                {
                    // We have to allow this case (somebody decided that this made sense)
                    //
                    // struct A
                    // {
                    //   void f(int i = E);
                    //   enum { E = 3 };
                    // };
                    //
                    AST parent = ast_get_parent(default_argument);

                    nodecl_default_argument = nodecl_make_cxx_parse_later(ast_get_locus(default_argument));
                    // Keep the current tree
                    nodecl_set_child(nodecl_default_argument, 0, _nodecl_wrap(default_argument));
                    // the parent was changed by nodecl_set_child, so fix it (otherwise the tree becomes inconsistent)
                    ast_set_parent(default_argument, parent);

                    // We will delay these function declarations in register_function
                }
                else
                {
                    if (!check_expression(default_argument, decl_context, &nodecl_default_argument))
                    {
                        error_printf("%s: error: could not check default argument expression '%s'\n",
                                ast_location(default_argument),
                                prettyprint_in_buffer(default_argument));

                        *function_type = get_error_type();
                        return;
                    }
                }
            }

            gather_decl_spec_t param_decl_gather_info;
            memset(&param_decl_gather_info, 0, sizeof(param_decl_gather_info));

            type_t* simple_type_info;

            decl_context_t param_decl_context = decl_context;
            param_decl_gather_info.parameter_declaration = 1;

            build_scope_decl_specifier_seq(parameter_decl_spec_seq,
                    &param_decl_gather_info, &simple_type_info,
                    param_decl_context, nodecl_output);

            if (is_error_type(simple_type_info))
            {
                *function_type = get_error_type();
                return;
            }

            AST attribute_list = ASTSon3(parameter_declaration);
            gather_extra_attributes(attribute_list, &param_decl_gather_info, param_decl_context);

            if (param_decl_gather_info.is_extern)
            {
                error_printf("%s: error: parameter declared as 'extern'\n", 
                        ast_location(parameter_decl_spec_seq));

                *function_type = get_error_type();
                return;
            }
            if (param_decl_gather_info.is_static)
            {
                error_printf("%s: error: parameter declared as 'static'\n", 
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

            function_declarator_nesting_level++;

            compute_declarator_type(parameter_declarator, &param_decl_gather_info,
                    simple_type_info, &type_info, param_decl_context, nodecl_output);

            function_declarator_nesting_level--;

            if (is_error_type(type_info))
            {
                *function_type = get_error_type();
                return;
            }

            if (parameter_declarator != NULL)
            {
                entry = build_scope_declarator_name(parameter_declarator, type_info, &param_decl_gather_info, param_decl_context);
            }

            if (is_void_type(type_info))
            {
                if (entry != NULL)
                {
                    error_printf("%s: error: parameter '%s' declared as void\n", 
                            ast_location(parameter_decl_spec_seq),
                            entry->symbol_name);
                    *function_type = get_error_type();
                    return;
                }
                else if (ASTSon0(iter) != NULL
                        || (num_parameters != 0))
                {
                    error_printf("%s: error: parameter declared as void\n", 
                            ast_location(parameter_decl_spec_seq));
                    *function_type = get_error_type();
                    return;
                }
                else // entry == NULL && ASTSon(iter) == NULL && num_parameters == 0
                {
                    // We are done: this is of the form f(void)
                    break;
                }
            }

            CXX_LANGUAGE()
            {
                // Keep track of this parameter name as we may have to compare it later
                if (entry != NULL)
                {
                    symbol_set_as_parameter_of_function(
                            entry,
                            get_function_declaration_proxy(),
                            function_declarator_nesting_level,
                            num_parameters);
                }
            }

            if (entry == NULL)
            {
                // The declarator was abstract, craft a name
                const char* arg_name;
                if (function_declarator_nesting_level == 0)
                {
                    uniquestr_sprintf(&arg_name, "mcc_arg_%d", num_parameters);
                }
                else
                {
                    uniquestr_sprintf(&arg_name, "mcc_arg_%d_%d", function_declarator_nesting_level, num_parameters);
                }
                AST declarator_id = ASTLeaf(AST_SYMBOL, ast_get_locus(parameter_decl_spec_seq), arg_name);
                entry = register_new_variable_name(declarator_id, type_info, &param_decl_gather_info, param_decl_context);
                entry->do_not_print = 1;
            }

            // Copy gcc attributes
            keep_gcc_attributes_in_symbol(entry, &param_decl_gather_info);
            keep_ms_declspecs_in_symbol(entry, &param_decl_gather_info);

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

            if (param_decl_gather_info.is_template_pack)
            {
                type_info = get_pack_type(type_info);
                original_type = get_pack_type(original_type);

                entry->type_information = original_type;
            }

            if (entry != NULL)
            {
                // A parameter is always a variable entity
                entry->kind = SK_VARIABLE;
                // it was a parameter pack
                if (param_decl_gather_info.is_template_pack)
                {
                    entry->kind = SK_VARIABLE_PACK;
                }

                // Update the type info but try to to preserve the original if
                // possible
                if (!equivalent_types(type_info, original_type))
                    entry->type_information = type_info;

                entry->defined = 1;
            }

            parameter_info[num_parameters].is_ellipsis = 0;
            parameter_info[num_parameters].type_info = get_unqualified_type(type_info);
            parameter_info[num_parameters].nonadjusted_type_info = original_type;

            ERROR_CONDITION(num_parameters == MCXX_MAX_FUNCTION_PARAMETERS, 
                    "Too many function parameters %d\n", MCXX_MAX_FUNCTION_PARAMETERS);

            arguments_info_t new_argument_info;
            new_argument_info.entry = entry;
            new_argument_info.argument = nodecl_default_argument;
            new_argument_info.context = decl_context;

            P_LIST_ADD(gather_info->arguments_info,
                    gather_info->num_arguments_info,
                    new_argument_info);

            // Pass the VLA symbols of this parameter
            int i;
            for (i = 0 ; i < param_decl_gather_info.num_vla_dimension_symbols; i++)
            {
                P_LIST_ADD(gather_info->vla_dimension_symbols, 
                        gather_info->num_vla_dimension_symbols,
                        param_decl_gather_info.vla_dimension_symbols[i]);
            }

            xfree(param_decl_gather_info.vla_dimension_symbols);

            num_parameters++;
        }
    }

    if ((num_parameters == 1)
            && !parameter_info[0].is_ellipsis)
    {
        type_t* parameter_type = parameter_info[0].type_info;

        if (is_void_type(parameter_type))
        {
            // This list was really empty
            num_parameters = 0;
        }
    }

    // Now create the type
    *function_type = get_new_function_type(*function_type, parameter_info, num_parameters, ref_qualifier);
}

/*
 * This function converts a type "T" into a "function (...) returning T" type
 */
static void set_function_type(type_t** declarator_type,  
        gather_decl_spec_t* gather_info, AST parameters_and_qualifiers, 
        decl_context_t decl_context,
        decl_context_t *p_prototype_context,
        decl_context_t *out_prototype_context,
        nodecl_t* nodecl_output)
{
    decl_context_t prototype_context;

    AST parameters = ASTSon0(parameters_and_qualifiers);
    AST extra_stuff = ASTSon1(parameters_and_qualifiers);

    // AST attribute_specifiers = NULL;
    AST cv_qualif_opt = NULL;
    AST ref_qualifier_opt = NULL;
    AST except_spec = NULL;
    if (extra_stuff != NULL)
    {
        // attribute_specifiers = ASTSon0(extra_stuff);
        cv_qualif_opt = ASTSon1(extra_stuff);
        ref_qualifier_opt = ASTSon2(extra_stuff);
        except_spec = ASTSon3(extra_stuff);
    }


    if (p_prototype_context == NULL)
    {
        // Allocate one here
        prototype_context = new_prototype_context(decl_context);
    }
    else
    {
        prototype_context = *p_prototype_context;
    }

    if (out_prototype_context != NULL)
    {
        *out_prototype_context = prototype_context;
    }

    set_function_parameter_clause(declarator_type, parameters, ref_qualifier_opt,
            prototype_context, gather_info, nodecl_output);

    cv_qualifier_t cv_qualif = compute_cv_qualifier(cv_qualif_opt);

    *declarator_type = get_cv_qualified_type(*declarator_type, cv_qualif);

    build_exception_spec(*declarator_type,
            except_spec,
            gather_info,
            decl_context,
            prototype_context,
            nodecl_output);
}

// Used in C++11
void set_function_type_for_lambda(type_t** declarator_type,  
        gather_decl_spec_t* gather_info,
        AST parameters_and_qualifiers, 
        decl_context_t decl_context,
        decl_context_t *lambda_block_context,
        nodecl_t* nodecl_output)
{
    set_function_type(declarator_type,
            gather_info,
            parameters_and_qualifiers,
            decl_context,
            lambda_block_context,
            NULL,
            nodecl_output);
}

// This function traverses the declarator tree gathering all attributes that might appear there
// We need to traverse the declarator twice because of gcc allowing attributes appear in many
// places
static void gather_extra_attributes_in_declarator(AST a, gather_decl_spec_t* gather_info, decl_context_t declarator_context)
{
    // FIXME - This function is a no-op currently

    if (a == NULL)
        return;

    switch(ASTType(a))
    {
        case AST_PARENTHESIZED_DECLARATOR :
            {
                gather_extra_attributes_in_declarator(ASTSon0(a), gather_info, declarator_context);
                break;
            }
        case AST_DECLARATOR :
            {
                // AST attribute_list = ASTSon1(a);
                // gather_extra_attributes(attribute_list, gather_info, declarator_context);

                gather_extra_attributes_in_declarator(ASTSon0(a), gather_info, declarator_context);
                break;
            }
        case AST_POINTER_DECLARATOR :
            {
                // AST attribute_list = ASTSon2(a);
                // gather_extra_attributes(attribute_list, gather_info, declarator_context);

                gather_extra_attributes_in_declarator(ASTSon1(a), gather_info, declarator_context);
                break;
            }
        case AST_DECLARATOR_ARRAY :
            {
                gather_extra_attributes_in_declarator(ASTSon0(a), gather_info, declarator_context);
                break;
            }
        case AST_DECLARATOR_FUNC :
        case AST_DECLARATOR_FUNC_TRAIL:
            {
                gather_extra_attributes_in_declarator(ASTSon0(a), gather_info, declarator_context);
                break;
            }
        case AST_DECLARATOR_ID_EXPR :
        case AST_DECLARATOR_ID_PACK :
            {
                gather_extra_attributes_in_declarator(ASTSon1(a), gather_info, declarator_context);
                break;
            }
        case AST_AMBIGUITY :
            {
                solve_ambiguous_declarator(a, declarator_context);
                // Restart function
                gather_extra_attributes_in_declarator(a, gather_info, declarator_context);
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

    // In our grammar attributes appear before the ptr-op and before the declarator, so we first handle

    switch(ASTType(a))
    {
        case AST_DECLARATOR :
        case AST_PARENTHESIZED_DECLARATOR :
            {
                AST attributes = ASTSon1(a);
                apply_attributes_to_type(declarator_type, attributes, declarator_context);

                build_scope_declarator_rec(ASTSon0(a), declarator_type, 
                        gather_info, declarator_context, entity_context, prototype_context, nodecl_output); 
                break;
            }
        case AST_POINTER_DECLARATOR :
            {
                AST attributes = ASTSon2(a);
                apply_attributes_to_type(declarator_type, attributes, declarator_context);

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
                        gather_info,
                        entity_context,
                        ast_get_locus(a));
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
                set_function_type(declarator_type, gather_info, ASTSon1(a),
                        entity_context, prototype_context, /* out_prototype */ NULL, nodecl_output);
                if (is_error_type(*declarator_type))
                {
                    return;
                }

                build_scope_declarator_rec(ASTSon0(a), declarator_type,
                        gather_info, declarator_context, entity_context, prototype_context, nodecl_output);
                break;
            }
        case AST_DECLARATOR_FUNC_TRAIL:
            {
                CXX03_LANGUAGE()
                {
                    // Try to be helpful
                    if (gather_info->is_auto_storage)
                    {
                        error_printf("%s: error: a trailing return using an 'auto' type-specifier is only valid in C++11\n",
                                ast_location(a));
                    }
                    else
                    {
                        error_printf("%s: error: a trailing return is only valid in C++11\n",
                                ast_location(a));
                    }

                    *declarator_type = get_error_type();
                }
                CXX11_LANGUAGE()
                {
                    if (!gather_info->is_auto_type)
                    {
                        error_printf("%s: error: a trailing return requires an 'auto' type-specifier\n",
                                ast_location(a));
                    }
                }

                // This auto has already been used
                gather_info->is_auto_type = 0;

                AST declarator = ASTSon0(a);
                AST parameters_and_qualifiers = ASTSon1(a);
                AST trailing_return = ASTSon2(a);

                decl_context_t out_prototype_context;
                set_function_type(declarator_type, gather_info, parameters_and_qualifiers,
                        entity_context, prototype_context, &out_prototype_context, nodecl_output);
                if (is_error_type(*declarator_type))
                {
                    return;
                }

                // Compute the return
                AST type_id = ASTSon0(trailing_return);

                gather_decl_spec_t new_gather_info;
                memset(&new_gather_info, 0, sizeof(new_gather_info));

                type_t* return_type = compute_type_for_type_id_tree(type_id, out_prototype_context,
                        NULL, &new_gather_info);

                if (is_error_type(return_type))
                {
                    return;
                }

                // Now we have to replace the return type
                *declarator_type = function_type_replace_return_type_with_trailing_return(*declarator_type, return_type);

                // Proceed with the remaining declarator
                build_scope_declarator_rec(declarator, declarator_type,
                        gather_info, declarator_context, entity_context, prototype_context, nodecl_output);
                break;
            }
        case AST_DECLARATOR_ID_EXPR :
            {
                // Do nothing
                break;
            }
        case AST_DECLARATOR_ID_PACK :
            {
                // Do nothing
                if (gather_info->parameter_declaration)
                {
                    gather_info->is_template_pack = 1;
                }
                else
                {
                    error_printf("%s: error: invalid template-pack in non parameter declaration\n",
                            ast_location(a));
                    *declarator_type = get_error_type();
                }
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
                        case AST_SYMBOL :
                        case AST_TEMPLATE_ID :
                            return 1;
                        case AST_QUALIFIED_ID :
                            {
                                AST qualif = ASTSon0(a);
                                AST unqualif = ASTSon2(qualif);

                                return ASTType(unqualif) == AST_TEMPLATE_ID
                                    || ASTType(unqualif) == AST_SYMBOL;
                                break;
                            }
                        default :
                            return 0;
                    }
                }
            }
        case AST_POINTER_DECLARATOR :
        case AST_DECLARATOR_ARRAY :
            {
                return 0;
            }
        case AST_DECLARATOR_FUNC :
        case AST_DECLARATOR_FUNC_TRAIL :
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
        gather_decl_spec_t* gather_info, decl_context_t decl_context)
{
    AST declarator_id_expr = get_declarator_id_expression(declarator, decl_context);

    if (declarator_id_expr == NULL)
        return NULL;

    ERROR_CONDITION(ASTType(declarator_id_expr) != AST_DECLARATOR_ID_EXPR,
            "Invalid node '%s'\n", ast_print_node_type(ASTType(declarator_id_expr)));

    scope_entry_t* entry = build_scope_declarator_id_expr(declarator_id_expr, declarator_type, gather_info,
            decl_context);

    return entry;
}

void update_function_default_arguments(scope_entry_t* function_symbol, 
        type_t* declarator_type, 
        gather_decl_spec_t* gather_info)
{
    if (function_symbol->kind == SK_DEPENDENT_ENTITY
            || function_symbol->kind == SK_DEPENDENT_FRIEND_FUNCTION)
        return;

    ERROR_CONDITION(function_symbol->kind != SK_FUNCTION, "Invalid symbol", 0);

    if (!is_named_type(declarator_type))
    {
        // We should mix here default argument info because the declarator has function-type form
        ERROR_CONDITION(gather_info->num_arguments_info != function_symbol->entity_specs.num_parameters,
                "These two should be the same and they are %d != %d", 
                gather_info->num_arguments_info, 
                function_symbol->entity_specs.num_parameters);

        int i;
        for (i = 0; i < gather_info->num_arguments_info; i++)
        {
            if (function_symbol->entity_specs.default_argument_info[i] == NULL
                    && !nodecl_is_null(gather_info->arguments_info[i].argument))
            {
                if (function_symbol->entity_specs.default_argument_info[i] == NULL)
                {
                    function_symbol->entity_specs.default_argument_info[i] 
                        = (default_argument_info_t*)counted_xcalloc(1, sizeof(default_argument_info_t), &_bytes_used_buildscope);
                }
                function_symbol->entity_specs.default_argument_info[i]->argument = gather_info->arguments_info[i].argument;
                function_symbol->entity_specs.default_argument_info[i]->context = gather_info->arguments_info[i].context;
            }
        }
    }
}

static void update_function_specifiers(scope_entry_t* entry,
        gather_decl_spec_t* gather_info,
        type_t* declarator_type,
        const locus_t* locus)
{
    ERROR_CONDITION(entry->kind != SK_FUNCTION, "Invalid symbol", 0);
    entry->entity_specs.is_user_declared = 1;

    entry->entity_specs.is_constexpr |= gather_info->is_constexpr;

    // Merge inline attribute
    entry->entity_specs.is_inline |= (gather_info->is_inline
            || gather_info->is_constexpr);

    // Remove the friend-declared attribute if we find the function but
    // this is not a friend declaration
    if (!gather_info->is_friend)
    {
        entry->entity_specs.is_friend_declared = 0;
        if (is_template_specialized_type(entry->type_information))
        {
            // Propagate it to the template name as well if this is the primary
            if (named_type_get_symbol(
                        template_type_get_primary_type(
                            template_specialized_type_get_related_template_type(entry->type_information))) == entry)
            {
                template_type_get_related_symbol(
                        template_specialized_type_get_related_template_type(entry->type_information)
                        )->entity_specs.is_friend_declared = 0;
            }
        }
    }

    // Update parameter names
    set_parameters_as_related_symbols(entry,
            gather_info,
            /* is_definition */ 0,
            locus);

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
}


/*
 * This function fills information for a declarator_id_expr. Actually only
 * unqualified names can be signed up since qualified names should have been
 * declared elsewhere.
 */
static scope_entry_t* build_scope_declarator_id_expr(AST declarator_name, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, 
        decl_context_t decl_context)
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
                declarator_type = get_const_qualified_type(get_new_function_type(get_void_type(), NULL, 0, REF_QUALIFIER_NONE));
                return register_new_variable_name(destructor_id, declarator_type, gather_info, decl_context);
                break;
            }
        case AST_TEMPLATE_ID :
            {
                if (!is_function_type(declarator_type))
                {
                    scope_entry_list_t* entry_list = query_nested_name(decl_context,
                            NULL, NULL,
                            declarator_id,
                            NULL);

                    ERROR_CONDITION((entry_list == NULL), "Qualified id '%s' name not found (%s)", 
                            prettyprint_in_buffer(declarator_id), ast_location(declarator_id));

                    scope_entry_t* result = entry_list_head(entry_list);

                    entry_list_free(entry_list);

                    return result;
                }
                else
                {
                    scope_entry_t *entry = NULL;

                    char ok = find_function_declaration(declarator_id, declarator_type, gather_info, decl_context, &entry);

                    if (ok
                            && entry != NULL
                            && entry->kind == SK_FUNCTION)
                    {
                        update_function_specifiers(entry, gather_info, declarator_type, ast_get_locus(declarator_id));
                    }
                    return entry;
                }

                break;
            }

            {
                // An unqualified operator_function_id "operator +"
                const char* operator_function_name = get_operator_function_name(declarator_id);
                AST operator_id = ASTLeaf(AST_SYMBOL,
                        ast_get_locus(declarator_id),
                        operator_function_name);
                // Keep the parent of the original declarator
                ast_set_parent(operator_id, ast_get_parent(declarator_id));

                if (gather_info->is_friend)
                {
                    // We should check the template arguments even if we are not going to use them
                    nodecl_t nodecl_dummy = nodecl_null();
                    compute_nodecl_name_from_id_expression(declarator_id, decl_context, &nodecl_dummy);
                }


                break;
            }
        case AST_OPERATOR_FUNCTION_ID:
        case AST_OPERATOR_FUNCTION_ID_TEMPLATE:
            {
                // An unqualified operator_function_id "operator +"
                const char* operator_function_name = get_operator_function_name(declarator_id);
                AST operator_id = ASTLeaf(AST_SYMBOL,
                        ast_get_locus(declarator_id),
                        operator_function_name);
                // Keep the parent of the original declarator
                ast_set_parent(operator_id, ast_get_parent(declarator_id));

                if (gather_info->is_friend)
                {
                    // We should check the template arguments even if we are not going to use them
                    nodecl_t nodecl_dummy = nodecl_null();
                    compute_nodecl_name_from_id_expression(declarator_id, decl_context, &nodecl_dummy);
                }

                if (ASTType(declarator_id) == AST_OPERATOR_FUNCTION_ID)
                {
                    return register_new_variable_name(operator_id, declarator_type, gather_info, decl_context);
                }
                else
                {
                    scope_entry_t *entry = NULL;
                    char ok = find_function_declaration(declarator_id, declarator_type, gather_info, decl_context, &entry);
                    if (ok
                            && entry != NULL
                            && entry->kind == SK_FUNCTION)
                    {
                        update_function_specifiers(entry, gather_info, declarator_type, ast_get_locus(declarator_id));
                    }
                    return entry;
                }
                break;
            }
        case AST_CONVERSION_FUNCTION_ID :
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "BUILDSCOPE: Registering a conversion function in %s\n", ast_location(declarator_id));
                }
                // Ok, according to the standard, this function returns the
                // type defined in the conversion function id
                type_t* conversion_type_info = NULL;

                // Get the type and its name
                const char* conversion_function_name = get_conversion_function_name(decl_context, declarator_id, 
                        &conversion_type_info);
                AST conversion_id = ASTLeaf(AST_SYMBOL, 
                        ast_get_locus(declarator_id), 
                        conversion_function_name);
                // Keep the parent of the original declarator
                ast_set_parent(conversion_id, ast_get_parent(declarator_id));
                return register_new_variable_name(conversion_id, declarator_type, gather_info, decl_context);
                break;
            }
            // Qualified ones
        case AST_QUALIFIED_ID :
            {
                // A qualified id "a::b::c"
                if (!is_function_type(declarator_type))
                {
                    scope_entry_list_t* entry_list = query_nested_name(decl_context,
                            ASTSon0(declarator_id),
                            ASTSon1(declarator_id),
                            ASTSon2(declarator_id),
                            NULL);

                    if (entry_list == NULL)
                    {
                        return NULL;
                    }

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
                        declarator_type = get_const_qualified_type(get_new_function_type(get_void_type(), NULL, 0, REF_QUALIFIER_NONE));
                    }

                    char ok = find_function_declaration(declarator_id, declarator_type, gather_info, decl_context, &entry);

                    CXX_LANGUAGE()
                    {
                        if (ok
                                && entry != NULL
                                && entry->kind == SK_FUNCTION)
                        {
                            update_function_specifiers(entry, gather_info, declarator_type, ast_get_locus(declarator_id));
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

static void copy_related_symbols(scope_entry_t* dest, scope_entry_t* orig)
{
    dest->entity_specs.num_related_symbols = orig->entity_specs.num_related_symbols;
    dest->entity_specs.related_symbols = counted_xcalloc(dest->entity_specs.num_related_symbols, 
            sizeof(*dest->entity_specs.related_symbols), &_bytes_used_buildscope);

    int i;
    for (i = 0; i < dest->entity_specs.num_related_symbols; i++)
    {
        dest->entity_specs.related_symbols[i] = orig->entity_specs.related_symbols[i];
    }
}


/*
 * This function registers a new typedef name.
 */
static scope_entry_t* register_new_typedef_name(AST declarator_id, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, decl_context_t decl_context)
{
    // First query for an existing entry in this scope
    scope_entry_list_t* list = query_in_scope(decl_context, declarator_id, NULL);

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
                error_printf("%s: error: symbol '%s' has been redeclared as a different symbol kind\n", 
                        ast_location(declarator_id), 
                        prettyprint_in_buffer(declarator_id));
                info_printf("%s: info: previous declaration of '%s'\n",
                        locus_to_str(entry->locus),
                        entry->symbol_name);
                return NULL;
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
                error_printf("%s: error: symbol '%s' has been redeclared as a different symbol kind\n", 
                        ast_location(declarator_id), 
                        prettyprint_in_buffer(declarator_id));
                info_printf("%s: info: current declaration of '%s' (with type '%s')\n",
                        ast_location(declarator_id), 
                        prettyprint_in_buffer(declarator_id),
                        print_type_str(declarator_type, decl_context));
                info_printf("%s: info: previous declaration of '%s' (with type '%s')\n",
                        locus_to_str(entry->locus),
                        entry->symbol_name,
                        print_type_str(entry->type_information, entry->decl_context));
                return NULL;
            }

            if (is_function_type(declarator_type)
                    && !is_named_type(declarator_type))
            {
                // If the declarator is a functional one, we have to mix the arguments here
                int i;
                for (i = 0; i < gather_info->num_arguments_info; i++)
                {
                    if (entry->entity_specs.default_argument_info[i] == NULL
                            && !nodecl_is_null(gather_info->arguments_info[i].argument))
                    {
                        entry->entity_specs.default_argument_info[i] 
                            = (default_argument_info_t*)counted_xcalloc(1, sizeof(default_argument_info_t), &_bytes_used_buildscope);

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

    // C++ If we are in this case
    //
    // typedef struct { int x; } A, B;
    // typedef enum { int x; } A, B;
    //
    // The first declarator-id (A) must become the class-name (or enum-name) of
    // the type being declared, while B will still be a plain typedef-name
    CXX_LANGUAGE()
    {
        if ((is_named_class_type(declarator_type) || is_named_enumerated_type(declarator_type))
                && named_type_get_symbol(declarator_type)->entity_specs.is_unnamed)
        {
            scope_entry_t* unnamed_symbol = named_type_get_symbol(declarator_type);

            unnamed_symbol->symbol_name = ASTText(declarator_id);
            unnamed_symbol->entity_specs.is_unnamed = 0;

            insert_entry(unnamed_symbol->decl_context.current_scope, unnamed_symbol);

            return unnamed_symbol;
        }
    }

    scope_entry_t* entry = new_symbol(decl_context, decl_context.current_scope, ASTText(declarator_id));

    DEBUG_CODE()
    {
        fprintf(stderr, "BUILDSCOPE: Registering typedef '%s'\n", ASTText(declarator_id));
    }

    entry->locus = ast_get_locus(declarator_id);
    entry->entity_specs.is_user_declared = 1;

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
            fprintf(stderr, "BUILDSCOPE: This is a typedef to function type, saving gathered information\n");
            fprintf(stderr, "BUILDSCOPE: Number of parameters %d\n", gather_info->num_arguments_info);
        }

        entry->entity_specs.num_parameters = gather_info->num_arguments_info;
        entry->entity_specs.default_argument_info = counted_xcalloc(entry->entity_specs.num_parameters,
                sizeof(*(entry->entity_specs.default_argument_info)), 
                &_bytes_used_buildscope);
        int i;
        for (i = 0; i < gather_info->num_arguments_info; i++)
        {
            if (!nodecl_is_null(gather_info->arguments_info[i].argument))
            {
                entry->entity_specs.default_argument_info[i] = 
                    (default_argument_info_t*)counted_xcalloc(1, sizeof(default_argument_info_t), &_bytes_used_buildscope);
                entry->entity_specs.default_argument_info[i]->argument = gather_info->arguments_info[i].argument;
                entry->entity_specs.default_argument_info[i]->context = gather_info->arguments_info[i].context;
            }
        }

        // Copy exception info as well
        entry->entity_specs.any_exception = gather_info->any_exception;
        entry->entity_specs.num_exceptions = gather_info->num_exceptions;
        entry->entity_specs.exceptions = gather_info->exceptions;
        entry->entity_specs.noexception = gather_info->noexception;

        set_parameters_as_related_symbols(entry, gather_info, /* is_definition */ 0,
                ast_get_locus(declarator_id));
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
                fprintf(stderr, "BUILDSCOPE: This is a typedef to typedef of function type, copying gathered information\n");
                fprintf(stderr, "BUILDSCOPE: Number of parameters %d\n", named_type->entity_specs.num_parameters);
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
            entry->entity_specs.noexception = named_type->entity_specs.noexception;

            // Copy parameter info
            copy_related_symbols(entry, named_type);
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
        scope_entry_list_t* entry_list = query_in_scope_flags(decl_context, declarator_id, NULL, decl_flags);

        scope_entry_list_t* check_list = filter_symbol_kind(entry_list, SK_VARIABLE);

        // Return the found symbol
        if (check_list != NULL)
        {
            scope_entry_t* entry = entry_list_head(check_list);

            // Update extern attribute
            // Maybe other attributes must be updated too
            if (entry->entity_specs.is_extern)
            {
                entry->entity_specs.is_extern = gather_info->is_extern;
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
            error_printf("%s: error: incompatible redeclaration of '%s' (look at '%s')\n",
                    ast_location(declarator_id),
                    prettyprint_in_buffer(declarator_id),
                    locus_to_str(entry->locus));
            return NULL;
        }

        entry_list_free(check_list);

        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: Registering variable '%s' in %p\n", ASTText(declarator_id), decl_context.current_scope);
        }

        scope_entry_t* entry = NULL;
        entry = new_symbol(decl_context, decl_context.current_scope, ASTText(declarator_id));

        entry->locus = ast_get_locus(declarator_id);
        entry->kind = SK_VARIABLE;
        entry->type_information = declarator_type;

        entry->entity_specs.is_user_declared = 1;
        entry->entity_specs.is_static = gather_info->is_static;
        entry->entity_specs.is_mutable = gather_info->is_mutable;
        entry->entity_specs.is_extern = gather_info->is_extern 
            // 'extern "C" int x;'  is like 'extern "C" extern int x;'
            || (!entry->entity_specs.is_member && linkage_current_get_name() != NULL && !linkage_current_is_braced());
        entry->entity_specs.is_register = gather_info->is_register;
        entry->entity_specs.is_thread = gather_info->is_thread;
        entry->entity_specs.is_thread_local = gather_info->is_thread_local;
        entry->entity_specs.is_constexpr = gather_info->is_constexpr;
        entry->entity_specs.linkage_spec = linkage_current_get_name();

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
    char ok = find_function_declaration(declarator_id, declarator_type, gather_info, decl_context, &entry);

    if (!ok)
        return NULL;

    if (entry == NULL)
    {
        // No existing function was found
        const char* function_name = ASTText(declarator_id);

        if (BITMAP_TEST(decl_context.decl_flags, DF_CONSTRUCTOR))
        {
            function_name = strprepend(function_name, "constructor ");
            decl_context.decl_flags &= ~DF_CONSTRUCTOR;
        }
        scope_entry_t* new_entry = NULL;

        if (!gather_info->is_template)
        {
            // Create the symbol as a normal function type

            if (!gather_info->is_friend)
            {
                new_entry = new_symbol(decl_context, decl_context.current_scope, function_name);
            }
            // If it is friend it must be signed in in the enclosing namespace
            // and the current_scope must be the namespace_scope
            else
            {
                new_entry = new_symbol(decl_context, decl_context.namespace_scope, function_name);
                new_entry->decl_context.current_scope = decl_context.namespace_scope;
            }

            new_entry->type_information = declarator_type;

            new_entry->kind = SK_FUNCTION;
            new_entry->locus = ast_get_locus(declarator_id);

            new_entry->entity_specs.linkage_spec = linkage_current_get_name();
            new_entry->entity_specs.is_explicit = gather_info->is_explicit;
            new_entry->entity_specs.is_friend_declared = gather_info->is_friend;

            if (is_named_type(declarator_type))
            {
                // This must be a typedef to a function type (directly or indirectly)
                scope_entry_t* typedef_sym = named_type_get_symbol(declarator_type);

                // Copy parameter names
                copy_related_symbols(new_entry, typedef_sym);
            }
            else
            {
                // Keep parameter names
                set_parameters_as_related_symbols(new_entry,
                        gather_info,
                        /* is_definition */ 0,
                        ast_get_locus(declarator_id));
            }
        }
        else /* gather_info->is_template */
        {
            if (decl_context.template_parameters == NULL)
            {
                error_printf("%s: error: explicit specialization '%s' does not match any template of '%s'\n",
                        ast_location(declarator_id),
                        print_decl_type_str(declarator_type, decl_context, function_name),
                        function_name);
                return NULL;
            }

            ERROR_CONDITION(decl_context.template_parameters == NULL,
                    "Error, there must be template parameters", 0);

            decl_context_t template_context = decl_context;
            if (!gather_info->is_friend)
            {
                new_entry = new_symbol(decl_context, decl_context.current_scope, function_name);
            }
            // If it is friend it must be signed in in the enclosing namespace
            // and the current_scope must be the namespace_scope
            else
            {
                new_entry = new_symbol(decl_context, decl_context.namespace_scope, function_name);
                new_entry->decl_context.current_scope = decl_context.namespace_scope;

                template_context.current_scope = decl_context.namespace_scope;
            }

            // If this function is template we have to create a template type
            // in the right context (It may be friend declared)
            type_t* template_type = get_new_template_type(decl_context.template_parameters,
                    declarator_type,
                    function_name,
                    template_context,
                    ast_get_locus(declarator_id));

            new_entry->type_information = template_type;

            // This is a template, not a plain function
            new_entry->kind = SK_TEMPLATE;
            new_entry->locus = ast_get_locus(declarator_id);

            new_entry->entity_specs.is_friend_declared = 0;

            if (decl_context.current_scope->kind == CLASS_SCOPE
                    && !new_entry->entity_specs.is_friend_declared)
            {
                new_entry->entity_specs.is_member = 1;

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
            new_entry->locus = ast_get_locus(declarator_id);

            new_entry->entity_specs.is_explicit = gather_info->is_explicit;

            // Keep parameter names
            set_parameters_as_related_symbols(new_entry,
                    gather_info,
                    /* is_definition */ 0,
                    ast_get_locus(declarator_id));
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: Registering new function '%s' at %s. symbol=%p scope=%p is_template=%d\n", 
                    function_name, 
                    ast_location(declarator_id),
                    new_entry,
                    new_entry->decl_context.current_scope,
                    (new_entry->kind == SK_TEMPLATE)
                   );
        }

        new_entry->entity_specs.is_user_declared = 1;

        new_entry->entity_specs.is_static = gather_info->is_static;
        new_entry->entity_specs.is_extern = gather_info->is_extern;
        new_entry->entity_specs.is_constexpr = gather_info->is_constexpr;
        new_entry->entity_specs.is_virtual = gather_info->is_virtual;
        new_entry->entity_specs.is_inline = gather_info->is_inline
            || gather_info->is_constexpr;

        if (gather_info->is_static && gather_info->is_extern
                && !gather_info->is_auto_storage)
        {
            error_printf("%s: error: conflicting linkage specifiers extern and static\n",
                    ast_location(declarator_id));
        }

        if (gather_info->is_virtual && gather_info->is_extern)
        {
            error_printf("%s: error: a virtual function is member, so it cannot have extern linkage\n",
                    ast_location(declarator_id));
        }

        if (gather_info->is_virtual && gather_info->is_static)
        {
            error_printf("%s: error: a virtual function must be a nonstatic member\n",
                    ast_location(declarator_id));
        }

        if (gather_info->is_auto_storage)
        {
            if (!gather_info->is_static
                    && !gather_info->is_extern)
            {
                if (decl_context.current_scope->kind != BLOCK_SCOPE)
                {
                    error_printf("%s: error: invalid auto linkage specifier for functions in non block scope\n",
                            ast_location(declarator_id));
                }
                // This is the gcc way to declare (not define) a nested function
                new_entry->entity_specs.is_nested_function = 1;
            }
            else
            {
                error_printf("%s: error: conflicting linkage specifiers auto %s specified\n",
                        ast_location(declarator_id),
                        (gather_info->is_static && gather_info->is_extern)
                        ?  ", extern and static"
                        : (gather_info->is_static ? "and static" : "and extern"));
            }
        }

        if (decl_context.current_scope->kind == BLOCK_SCOPE)
        {
            if (gather_info->is_static)
            {
                error_printf("%s: error: invalid static linkage specifier for a function declared in block scope\n",
                        ast_location(declarator_id));
            }
        }

        if (decl_context.current_scope->kind != BLOCK_SCOPE)
        {
            new_entry->entity_specs.linkage_spec = linkage_current_get_name();
        }

        // "is_pure" of a function is computed in "build_scope_member_simple_declaration"

        new_entry->entity_specs.any_exception = gather_info->any_exception;
        new_entry->entity_specs.num_exceptions = gather_info->num_exceptions;
        new_entry->entity_specs.exceptions = gather_info->exceptions;
        new_entry->entity_specs.noexception = gather_info->noexception;

        char do_delay_function = 0;
        if (!nodecl_is_null(new_entry->entity_specs.noexception)
                && nodecl_get_kind(new_entry->entity_specs.noexception) == NODECL_CXX_PARSE_LATER)
        {
            do_delay_function = 1;
        }

        new_entry->entity_specs.num_parameters = gather_info->num_arguments_info;

        new_entry->entity_specs.default_argument_info =
            counted_xcalloc(gather_info->num_arguments_info,
                    sizeof(*new_entry->entity_specs.default_argument_info),
                    &_bytes_used_buildscope);

        new_entry->entity_specs.is_friend_declared = gather_info->is_friend;

        // If the declaration context is CLASS_SCOPE and the function definition is friend,
        // It is not a member class
        if (decl_context.current_scope->kind == CLASS_SCOPE
            && !new_entry->entity_specs.is_friend_declared)
        {
            new_entry->entity_specs.is_member = 1;
            new_entry->entity_specs.class_type =
                get_user_defined_type(decl_context.current_scope->related_entry);
        }

        int i;
        for (i = 0; i < gather_info->num_arguments_info; i++)
        {
            if (!nodecl_is_null(gather_info->arguments_info[i].argument))
            {
                if (gather_info->is_explicit_specialization)
                {
                    error_printf("%s: error: default template arguments in explicit specialization function declaration\n",
                        ast_location(declarator_id));
                }
                else if (gather_info->is_explicit_instantiation)
                {
                    error_printf("%s: error: default template arguments in explicit instantiation function declaration\n",
                        ast_location(declarator_id));
                }

                new_entry->entity_specs.default_argument_info[i] = 
                    (default_argument_info_t*)counted_xcalloc(1, sizeof(default_argument_info_t), 
                            &_bytes_used_buildscope);
                new_entry->entity_specs.default_argument_info[i]->argument = 
                    gather_info->arguments_info[i].argument;
                new_entry->entity_specs.default_argument_info[i]->context = 
                    gather_info->arguments_info[i].context;

                if (nodecl_get_kind(new_entry->entity_specs.default_argument_info[i]->argument) == NODECL_CXX_PARSE_LATER)
                {
                    do_delay_function = 1;
                }
            }
        }

        if (do_delay_function)
        {
            ERROR_CONDITION(decl_context.current_scope->kind != CLASS_SCOPE,
                    "Invalid parse later default argument", 0);
            build_scope_delayed_add_function_declaration(new_entry, decl_context);
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
                fprintf(stderr , "BUILDSCOPE: This function declaration comes from a typedef of function type.\n");
                fprintf(stderr , "BUILDSCOPE: Num parameters %d\n", named_function_type->entity_specs.num_parameters);
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
            new_entry->entity_specs.noexception = named_function_type->entity_specs.noexception;
        }

        return new_entry;
    }
    else if (entry->kind == SK_FUNCTION)
    {
        update_function_specifiers(entry, gather_info, declarator_type, ast_get_locus(declarator_id));
    }
    else if (entry->kind == SK_DEPENDENT_FRIEND_FUNCTION)
    {
        //Do nothing
    }
    else
    {
        internal_error("Code unreachable", 0);
    }
    return entry;
}

static char find_dependent_friend_function_declaration(AST declarator_id,
        type_t* declarator_type,
        gather_decl_spec_t* gather_info,
        decl_context_t decl_context,
        scope_entry_t** result_entry)
{
    ERROR_CONDITION(!(gather_info->is_friend
                && is_dependent_class_scope(decl_context)),
            "this is not a depedent friend function", 0);

    // We should create a new dependent friend function, but first we need to
    // check some constraints

    char is_template_function = gather_info->is_template;
    char is_qualified = is_qualified_id_expression(declarator_id);

    AST considered_tree = declarator_id;
    if (ASTType(declarator_id) == AST_QUALIFIED_ID)
    {
        considered_tree = ASTSon2(declarator_id);
    }

    char is_template_id = (ASTType(considered_tree) == AST_TEMPLATE_ID
            || ASTType(considered_tree) == AST_OPERATOR_FUNCTION_ID_TEMPLATE);

    decl_flags_t decl_flags = DF_DEPENDENT_TYPENAME;
    decl_context_t lookup_context = decl_context;
    if (!is_qualified)
    {
        decl_flags |= DF_ONLY_CURRENT_SCOPE;
    }

    lookup_context.current_scope = lookup_context.namespace_scope;

    scope_entry_list_t* entry_list
        = query_id_expression_flags(lookup_context, declarator_id, NULL, decl_flags);

    // Summary:
    //  1. The declaration is not a template function
    //      1.1 It's a qualified or unqualified template-id -> refers to a specialization of a function template
    //      1.2 It's a qualified name -> refers to:
    //          *   A nontemplate function, otherwise
    //          *   A matching specialization of a template function
    //      1.3 It's an unqualied name -> declares an nontemplate function
    //  2. Otherwise,
    //      2.1 It's a qualified name -> refers to a template function

    char found_candidate = 0;
    scope_entry_list_t* filtered_entry_list = NULL;
    enum cxx_symbol_kind filter_only_templates[] = { SK_TEMPLATE, SK_DEPENDENT_ENTITY };
    enum cxx_symbol_kind filter_only_functions[] = { SK_FUNCTION };
    enum cxx_symbol_kind filter_only_templates_and_functions[] = { SK_FUNCTION, SK_TEMPLATE, SK_DEPENDENT_ENTITY};

    // This code filters the query properly and does error detection
    if ((!is_template_function && is_template_id) //1.1
            || (is_template_function && is_qualified)) //2.1
    {
        filtered_entry_list = filter_symbol_kind_set(entry_list,
                STATIC_ARRAY_LENGTH(filter_only_templates),
                filter_only_templates);

        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(filtered_entry_list);
                !entry_list_iterator_end(it) && !found_candidate;
                entry_list_iterator_next(it))
        {
            scope_entry_t* current_sym = entry_list_iterator_current(it);
            if (current_sym->kind == SK_TEMPLATE)
            {
                current_sym =
                    named_type_get_symbol(template_type_get_primary_type(current_sym->type_information));
            }
            if (current_sym->kind == SK_FUNCTION || current_sym->kind == SK_DEPENDENT_ENTITY)
            {
                found_candidate = 1;
            }
        }
    }

    if (!is_template_function)
    {
        if (is_template_id) //1.1
        {
            if (!found_candidate)
            {
                error_printf("%s: template-id '%s' does not refer to a specialization of a function template\n",
                        ast_location(declarator_id), prettyprint_in_buffer(declarator_id));
                return 0;
            }
        }
        else if (is_qualified) // 1.2
        {
            filtered_entry_list = filter_symbol_kind_set(entry_list,
                    STATIC_ARRAY_LENGTH(filter_only_templates_and_functions),
                    filter_only_templates_and_functions);

            scope_entry_list_iterator_t* it = NULL;
            for (it = entry_list_iterator_begin(filtered_entry_list);
                    !entry_list_iterator_end(it) && !found_candidate;
                    entry_list_iterator_next(it))
            {
                scope_entry_t* current_sym = entry_list_iterator_current(it);
                if (current_sym->kind == SK_TEMPLATE)
                {
                    current_sym = named_type_get_symbol(template_type_get_primary_type(current_sym->type_information));
                }

                if (current_sym->kind == SK_FUNCTION || current_sym->kind == SK_DEPENDENT_ENTITY)
                {
                    found_candidate = 1;
                }
            }

            if (!found_candidate)
            {
                error_printf("%s: name '%s' does not match with any nontemplate function or specialization of a function template\n",
                        ast_location(declarator_id), prettyprint_in_buffer(declarator_id));
                return 0;
            }
        }
        else // 1.3
        {
            filtered_entry_list = filter_symbol_kind_set(entry_list,
                    STATIC_ARRAY_LENGTH(filter_only_functions),
                    filter_only_functions);
        }
    }
    else
    {
        // It is a friend template function declaration inside a template class definition
        scope_entry_t* func_templ = NULL;
        if (is_qualified) //2.1
        {
            if (!found_candidate)
            {
                error_printf("%s: Qualified id '%s' name not found\n",
                        ast_location(declarator_id), prettyprint_in_buffer(declarator_id));
                return 0;
            }
        }

        // We should create a new SK_TEMPLATE
        const char* declarator_name = NULL;
        switch (ASTType(declarator_id))
        {
            case AST_TEMPLATE_ID:
                {
                    declarator_name = ASTText(ASTSon0(declarator_id));
                    break;
                }
            case AST_QUALIFIED_ID:
                {
                    declarator_name = ASTText(ASTSon2(declarator_id));
                    break;
                }
            case AST_OPERATOR_FUNCTION_ID:
            case AST_OPERATOR_FUNCTION_ID_TEMPLATE:
                {
                    declarator_name = get_operator_function_name(declarator_id);
                    break;
                }
            default:
                {
                    declarator_name = ASTText(declarator_id);
                    break;
                }
        }

        func_templ = counted_xcalloc(1, sizeof(*func_templ), &_bytes_used_buildscope);
        func_templ->symbol_name = declarator_name;

        func_templ->kind = SK_TEMPLATE;
        func_templ->locus = ast_get_locus(declarator_id);
        func_templ->entity_specs.is_friend_declared = 1;

        func_templ->decl_context = decl_context;
        func_templ->decl_context.current_scope = decl_context.namespace_scope;

        func_templ->type_information =
            get_new_template_type(decl_context.template_parameters, declarator_type,
                    ASTText(declarator_id), decl_context, ast_get_locus(declarator_id));

        // Perhaps we may need to update some entity specs of the primary symbol
        type_t* primary_type = template_type_get_primary_type(func_templ->type_information);
        scope_entry_t* primary_symbol = named_type_get_symbol(primary_type);
        primary_symbol->entity_specs.any_exception = gather_info->any_exception;

        template_type_set_related_symbol(func_templ->type_information, func_templ);

        // We should update the declarator type with the primary type
        declarator_type = primary_symbol->type_information;
    }

    //We create a new symbol always
    scope_entry_t* new_entry =
        counted_xcalloc(1, sizeof(*new_entry), &_bytes_used_buildscope);

    new_entry->kind = SK_DEPENDENT_FRIEND_FUNCTION;
    new_entry->locus = ast_get_locus(declarator_id);

    new_entry->type_information = declarator_type;

    new_entry->decl_context = decl_context;
    new_entry->decl_context.current_scope = decl_context.namespace_scope;

    nodecl_t nodecl_name = nodecl_null();
    compute_nodecl_name_from_id_expression(declarator_id, decl_context, &nodecl_name);
    new_entry->value = nodecl_name;
    //The symbol name has been computed by Codegen!!
    new_entry->symbol_name = uniquestr(codegen_to_str(nodecl_name, decl_context));

    new_entry->entity_specs.is_friend_declared = 1;
    new_entry->entity_specs.any_exception = gather_info->any_exception;
    new_entry->entity_specs.num_parameters = gather_info->num_arguments_info;

    new_entry->entity_specs.default_argument_info =
        counted_xcalloc(new_entry->entity_specs.num_parameters,
                sizeof(*(new_entry->entity_specs.default_argument_info)),
                &_bytes_used_buildscope);
    int i;
    for (i = 0; i < gather_info->num_arguments_info; i++)
    {
        if (!nodecl_is_null(gather_info->arguments_info[i].argument))
        {
            new_entry->entity_specs.default_argument_info[i] =
                (default_argument_info_t*)counted_xcalloc(1, sizeof(default_argument_info_t), &_bytes_used_buildscope);
            new_entry->entity_specs.default_argument_info[i]->argument = gather_info->arguments_info[i].argument;
            new_entry->entity_specs.default_argument_info[i]->context = gather_info->arguments_info[i].context;
        }
    }

    if (is_template_id)
    {
        // We should store the candidates list because It will be used during
        // the instantiation of the current class
        entry_list_to_symbol_array(filtered_entry_list,
                &new_entry->entity_specs.friend_candidates,
                &new_entry->entity_specs.num_friend_candidates);
    }

    *result_entry = new_entry;
    entry_list_free(filtered_entry_list);
    entry_list_free(entry_list);
    return 1;
}

static char same_template_parameter_list(
        template_parameter_list_t* template_parameter_list_1,
        template_parameter_list_t* template_parameter_list_2,
        decl_context_t decl_context)
{
    ERROR_CONDITION(template_parameter_list_1 == NULL
            || template_parameter_list_2 == NULL,
            "Invalid template parameter list", 0);

    if (template_parameter_list_1->num_parameters !=
            template_parameter_list_2->num_parameters)
        return 0;

    int i;
    for (i = 0; i < template_parameter_list_1->num_parameters; i++)
    {
        if (template_parameter_list_1->parameters[i]->kind !=
                template_parameter_list_2->parameters[i]->kind)
            return 0;

        if (template_parameter_list_1->parameters[i]->kind == TPK_NONTYPE)
        {
            // Check both types
            if (!equivalent_types_in_context(
                        template_parameter_list_1->parameters[i]->entry->type_information,
                        template_parameter_list_2->parameters[i]->entry->type_information,
                        decl_context))
            {
                return 0;
            }
        }
        else if (template_parameter_list_1->parameters[i]->kind == TPK_TEMPLATE)
        {
            // Recursively check template parameters of template-template parameter
            if (!same_template_parameter_list(
                        template_type_get_template_parameters(template_parameter_list_1->parameters[i]->entry->type_information),
                        template_type_get_template_parameters(template_parameter_list_2->parameters[i]->entry->type_information),
                        decl_context))
                return 0;
        }
    }

    return 1;
}

static char find_function_declaration(AST declarator_id,
        type_t* declarator_type,
        gather_decl_spec_t* gather_info,
        decl_context_t decl_context,
        scope_entry_t** result_entry)
{
    *result_entry = NULL;

    AST considered_tree = declarator_id;
    if (ASTType(declarator_id) == AST_QUALIFIED_ID)
    {
        considered_tree = ASTSon2(declarator_id);
    }

    char declarator_is_template_id = (ASTType(considered_tree) == AST_TEMPLATE_ID
            || ASTType(considered_tree) == AST_OPERATOR_FUNCTION_ID_TEMPLATE);

    // Template function declarations that are friend declared cannot be partially specialized
    if (gather_info->is_friend
            && gather_info->is_template
            && declarator_is_template_id)
    {
        error_printf("%s: invalid use of a template-id '%s' in a template friend function declaration\n",
                ast_location(declarator_id),
                prettyprint_in_buffer(declarator_id));
        return 0;
    }

    if (gather_info->is_friend
            && is_dependent_class_scope(decl_context))
    {
        return find_dependent_friend_function_declaration(declarator_id,
                declarator_type, gather_info, decl_context, result_entry);
    }

    decl_flags_t decl_flags = DF_NONE;
    if (BITMAP_TEST(decl_context.decl_flags, DF_CONSTRUCTOR))
    {
        decl_flags |= DF_CONSTRUCTOR;
    }

    // Restrict ourselves to the current scope
    // if the declarator-id is unqualified
    // and we are not naming a friend
    decl_context_t lookup_context = decl_context;
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
    else
    {
        // Friends:
        // Restrict the lookup to the innermost enclosing namespace
        // if the declarator_id is unqualified and we are not naming a template
        // C++ Standard 7.3.1.2 Namespace member definitions
        lookup_context.current_scope = lookup_context.namespace_scope;

        // The class or function is not a template class or template function
        if (!gather_info->is_template
                // The 'declarator_id' is not a template-id
                && ASTType(declarator_id) != AST_TEMPLATE_ID)
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
    }

    // Dependent friends are handled first
    if (!gather_info->is_template
            && gather_info->is_friend
            && is_dependent_type(declarator_type))
    {
        scope_entry_t* result = counted_xcalloc(1, sizeof(*result), &_bytes_used_buildscope);
        result->kind = SK_DEPENDENT_FRIEND_FUNCTION;
        result->locus = ast_get_locus(declarator_id);

        if (ASTType(declarator_id) == AST_TEMPLATE_ID)
        {
            result->symbol_name = ASTText(ASTSon0(declarator_id));
        }
        else
        {
            result->symbol_name = ASTText(declarator_id);
        }

        result->entity_specs.any_exception = gather_info->any_exception;
        result->type_information = declarator_type;

        result->decl_context = decl_context;

        *result_entry = result;
        return 1;
    }

    scope_entry_list_t* entry_list
        = query_id_expression_flags(lookup_context, declarator_id, NULL, decl_flags);

    type_t* function_type_being_declared = declarator_type;

    scope_entry_list_t* candidates = NULL;

    // First we compute a set of candidates
    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        // Ignore this case unless we are in a friend declaration
        if (entry->kind == SK_DEPENDENT_ENTITY)
        {
            if (gather_info->is_friend)
            {
                *result_entry = entry;
                return 1;
            }
            continue;
        }

        // This is so C90's ;)
        if (entry->kind == SK_CLASS
                || entry->kind == SK_ENUM)
            continue;

        // Ignore using symbols
        if (entry->kind == SK_USING
                || entry->kind == SK_USING_TYPENAME)
            continue;

        // At this point this must a function or a template function
        if (entry->kind != SK_FUNCTION
                && (entry->kind != SK_TEMPLATE
                    || named_type_get_symbol(template_type_get_primary_type(entry->type_information))->kind != SK_FUNCTION))
        {
            error_printf("%s: name '%s' has already been declared as a different entity kind\n",
                    ast_location(declarator_id),
                    prettyprint_in_buffer(declarator_id));
            return 0;
        }

        scope_entry_t* considered_symbol = NULL;
        if (entry->kind == SK_TEMPLATE)
        {
            type_t* primary_named_type = template_type_get_primary_type(entry->type_information);
            considered_symbol = named_type_get_symbol(primary_named_type);
        }
        else if (entry->kind == SK_FUNCTION)
        {
            considered_symbol = entry;
        }
        else
        {
            internal_error("Unreachable code", 0);
        }

        if (!gather_info->is_explicit_instantiation
                && !(gather_info->is_friend && declarator_is_template_id))
        {
            nesting_check_t nest_check = check_template_nesting_of_name(considered_symbol, decl_context.template_parameters);

            if (nest_check == NESTING_CHECK_OK)
            {
                // Do nothing, this is fine
            }
            else if (nest_check == NESTING_CHECK_NOT_A_TEMPLATE)
            {
                // We will enter here when we find the (nontemplate) 'A::f(bool, bool)'
                //
                // struct A
                // {
                //    template <typename T> void f(T, T);
                //    void f(bool, bool);
                // };
                //
                // template <>
                // void A::f(bool, bool)
                // {
                // }
                //
                // Skip
                continue;
            }
            else if (nest_check == NESTING_CHECK_INVALID)
            {
                // We will enter here when we find 'template <typename Q> A<int>::f(Q, Q)'
                //
                // template <typename T>
                // struct B
                // {
                //    template <typename Q> void f(Q, Q);
                //    void f(bool, bool);
                // };
                //
                // template <>
                // void B<int>::f(bool, bool)
                // {
                // }
                //
                continue;
            }
            else
            {
                internal_error("Unexpected nesting check value", 0);
            }
        }

        candidates = entry_list_add(candidates, entry);

    }
    entry_list_iterator_free(it);

    scope_entry_list_t* result_function_list = NULL;

    // 1. Non template functions or template functions unless the declaration
    // is an explicit specialization or instantiation
    {
        // In this case a simple scan through the candidates should be enough
        for (it = entry_list_iterator_begin(candidates);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            char function_matches = 0;

            scope_entry_t* entry = entry_list_iterator_current(it);

            scope_entry_t* considered_symbol = NULL;
            if (entry->kind == SK_TEMPLATE)
            {
                // Do not handle these here if this is an explicit specialization,
                // explicit instantiation or a friend with template-id. We will handle them later
                if (gather_info->is_explicit_specialization
                        || gather_info->is_explicit_instantiation
                        || (gather_info->is_friend && declarator_is_template_id))
                    continue;

                type_t* primary_named_type = template_type_get_primary_type(entry->type_information);
                considered_symbol = named_type_get_symbol(primary_named_type);
            }
            else if (entry->kind == SK_FUNCTION)
            {
                considered_symbol = entry;
            }
            else
            {
                internal_error("Code unreachable", 0);
            }

            DEBUG_CODE()
            {
                fprintf(stderr, "BUILDSCOPE: Checking function declaration of '%s' at '%s' (%s) against the declaration at '%s' (%s)\n",
                        prettyprint_in_buffer(declarator_id),
                        ast_location(declarator_id),
                        print_declarator(function_type_being_declared),
                        locus_to_str(considered_symbol->locus),
                        print_declarator(considered_symbol->type_information)
                       );
            }

            type_t* considered_type = advance_over_typedefs(considered_symbol->type_information);

            if (entry->kind == SK_TEMPLATE)
            {
                // A simple match by types in this case because this is
                // not an explicit specialization
                //
                // struct A
                // {
                //    template <typename T>
                //    void f(T);
                // };
                //
                // template <typename T>
                // void A::f(T)
                // {
                // }
                //
                if (equivalent_types_in_context(function_type_being_declared, considered_type, entry->decl_context))
                {
                    template_parameter_list_t* decl_template_parameters = decl_context.template_parameters;

                    // Now the types match, but it might happen that template parameters don't, like here
                    //
                    // We do not want it to be confused with 'template <typename T, typename Q> void A::f(T)'
                    //
                    // struct A
                    // {
                    //    template <typename T, typename Q>
                    //    void f(T);
                    //
                    //    template <typename T>
                    //    void f(T);
                    // };
                    //
                    // template <typename T>
                    // void A::f(T) { }

                    if (decl_template_parameters == NULL)
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "BUILDSCOPE: Types match with a template specialization but there "
                                    "are no template parameters available\n");
                        }
                    }
                    else
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "BUILDSCOPE: Types match with a template specialization and this is a plain "
                                    "template declaration, we have to check template parameters\n");
                        }

                        template_parameter_list_t* considered_type_template_parameters =
                            template_specialized_type_get_template_parameters(considered_type);

                        function_matches =
                            (decl_template_parameters->num_parameters
                             == considered_type_template_parameters->num_parameters)
                            && same_template_parameter_list(
                                    decl_template_parameters,
                                    considered_type_template_parameters,
                                    entry->decl_context);

                        DEBUG_CODE()
                        {
                            if (!function_matches)
                            {
                                fprintf(stderr, "BUILDSCOPE: Template parameters did NOT match\n");
                            }
                            else
                            {
                                fprintf(stderr, "BUILDSCOPE: Template parameters DID match\n");
                            }
                        }
                    }
                }
            }
            else if (entry->kind == SK_FUNCTION)
            {
                // Just attempt a match by type
                function_matches = equivalent_function_types_may_differ_ref_qualifier(
                        function_type_being_declared,
                        considered_type,
                        entry->decl_context);

                CXX11_LANGUAGE()
                {
                    if ((function_type_get_ref_qualifier(function_type_being_declared) != REF_QUALIFIER_NONE)
                            != (function_type_get_ref_qualifier(considered_type) != REF_QUALIFIER_NONE))
                    {
                        function_matches = 0;
                        error_printf("%s: error: declaration cannot overload '%s'\n",
                                ast_location(declarator_id),
                                print_decl_type_str(considered_type,
                                    entry->decl_context,
                                    get_qualified_symbol_name(entry, entry->decl_context)));
                        return 0;
                    }
                }
            }

            if (function_matches)
            {
                result_function_list = entry_list_add(result_function_list, considered_symbol);
                DEBUG_CODE()
                {
                    fprintf(stderr, "BUILDSCOPE: Function declarator '%s' at '%s' matches symbol '%s' declared at '%s'\n",
                            prettyprint_in_buffer(declarator_id),
                            ast_location(declarator_id),
                            considered_symbol->symbol_name,
                            locus_to_str(considered_symbol->locus));
                }
            }
            else
            {
                C_LANGUAGE()
                {
                    // We found something but it did not match the current type
                    if (!function_type_get_lacking_prototype(function_type_being_declared)
                            && !function_type_get_lacking_prototype(considered_type))
                    {
                        error_printf("%s: error: function '%s' has been declared with different prototype (see '%s')\n",
                                ast_location(declarator_id),
                                ASTText(declarator_id),
                                locus_to_str(entry->locus)
                                );
                        return 0;
                    }
                    result_function_list = entry_list_add(result_function_list, considered_symbol);
                }
            }
        }
        entry_list_iterator_free(it);
    }

    // 2. Template functions when the declaration is an explicit
    // specialization/instantiation or when this is a friend declaration with a
    // template-id in the unqualified-id of the id-expression
    if (gather_info->is_explicit_specialization
            || gather_info->is_explicit_instantiation
            || (gather_info->is_friend && declarator_is_template_id))
    {
        // This is an explicit specialization
        //
        // struct A
        // {
        //   template <typename T> void f(T);
        // };
        //
        // template <>
        // void A::f(int) { }
        //
        // We have to solve the template

        template_parameter_list_t *explicit_template_parameters = NULL;
        if (declarator_is_template_id)
        {
            explicit_template_parameters =
                get_template_arguments_from_syntax(ASTSon1(considered_tree), decl_context);
        }

        // This function ignores non-templates
        scope_entry_list_t* solved_templates = solve_template_function(
                candidates,
                explicit_template_parameters,
                function_type_being_declared,
                ast_get_locus(declarator_id));

        if (solved_templates != NULL)
        {
            result_function_list = entry_list_merge(result_function_list, solved_templates);
        }
    }

    entry_list_free(candidates);

    if (result_function_list != NULL)
    {
        if (entry_list_size(result_function_list) == 1)
        {
            scope_entry_t* result  = entry_list_head(result_function_list);

            // if the candidate symbol is an explicit specialization we enable
            // the 'is_explicit_specialization' flag from its template arguments
            if (gather_info->is_explicit_specialization
                    && is_template_specialized_type(result->type_information))
            {
                template_parameter_list_t* template_args =
                    template_specialized_type_get_template_arguments(result->type_information);
                template_args->is_explicit_specialization = 1;
            }

            *result_entry = result;
            entry_list_free(result_function_list);

            // No error
            return 1;
        }
        else
        {
            const char* full_name = prettyprint_in_buffer(declarator_id);

            error_printf("%s: error: ambiguous template specialization '%s'\n",
                    locus_to_str(ast_get_locus(declarator_id)),
                    print_decl_type_str(function_type_being_declared, decl_context, full_name));

            for (it = entry_list_iterator_begin(result_function_list);
                    !entry_list_iterator_end(it);
                    entry_list_iterator_next(it))
            {
                scope_entry_t* current_entry = entry_list_iterator_current(it);

                info_printf("%s: note:   %s\n",
                        locus_to_str(current_entry->locus),
                        print_decl_type_str(current_entry->type_information, current_entry->decl_context, 
                            get_qualified_symbol_name(current_entry, current_entry->decl_context)));
            }
            entry_list_iterator_free(it);
            entry_list_free(result_function_list);

            // Error due to ambiguity
            return 0;
        }
    }

    // No error
    return 1;
}

/*
 * This function saves the current linkage, sets the new and restores it back.
 */
static void build_scope_linkage_specifier(AST a, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    AST declaration_sequence = ASTSon1(a);

    if (declaration_sequence == NULL)
        return;

    AST linkage_spec = ASTSon0(a);
    const char* current_linkage = uniquestr(ASTText(linkage_spec));
    // Ignore C++ linkage as it is the default
    if (strcmp(current_linkage, "\"C++\"") == 0)
        current_linkage = NULL;

    linkage_push(current_linkage, /* is braced */ 1);

    build_scope_declaration_sequence(declaration_sequence, decl_context, nodecl_output);

    linkage_pop();
}

/*
 * Similar to build_scope_linkage_specifier but for just one declaration
 */
static void build_scope_linkage_specifier_declaration(AST a, 
        AST top_linkage_decl, 
        decl_context_t decl_context,
        nodecl_t *nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list
        )
{
    AST declaration = ASTSon1(a);

    AST linkage_spec = ASTSon0(a);
    const char* current_linkage = uniquestr(ASTText(linkage_spec));
    // Ignore C++ linkage as it is the default
    if (strcmp(current_linkage, "\"C++\"") == 0)
        current_linkage = NULL;

    linkage_push(current_linkage, /* is braced */ 0);

    if (ASTType(declaration)  == AST_LINKAGE_SPEC_DECL)
    {
        build_scope_linkage_specifier_declaration(declaration, top_linkage_decl, decl_context, 
                nodecl_output, declared_symbols, gather_decl_spec_list);
    }
    else
    {
        build_scope_declaration(declaration, decl_context, nodecl_output, declared_symbols, gather_decl_spec_list);
    }

    linkage_pop();
}

static void set_deleted(
        scope_entry_t* entry,
        decl_context_t decl_context,
        const locus_t* locus)
{
    char can_delete = 1;
    if (entry->defined)
    {
        const char* qualified_name = get_qualified_symbol_name(entry, decl_context);

        error_printf("%s: cannot delete function '%s' already defined\n",
                locus_to_str(locus),
                print_decl_type_str(entry->type_information,
                    decl_context,
                    qualified_name));

        can_delete = 0;
    }

    if (can_delete)
    {
        entry->entity_specs.is_deleted = 1;
        entry->defined = 1;
    }
}

static void build_scope_deleted_function_definition(AST a, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    common_defaulted_or_deleted(a, decl_context,
            set_deleted,
            /* is_template */ 0,
            /* is_explicit_specialization */ 0,
            /* declared_symbols */ NULL,
            nodecl_output);
}

static char function_has_default_arguments(scope_entry_t* entry)
{
    int num_parameters = function_type_get_num_parameters(entry->type_information);
    if (function_type_get_has_ellipsis(entry->type_information)) 
        num_parameters--;
    int j;
    for (j = 0; j < num_parameters; j++)
    {
        if (entry->entity_specs.default_argument_info[j] != NULL)
            return 1;
    }
    return 0;
}

static char function_has_exception_specification_valid_for_defaulted(scope_entry_t* entry)
{
    if (nodecl_is_null(entry->entity_specs.noexception))
    {
        return entry->entity_specs.any_exception;
    }
    else
    {
        if (nodecl_expr_is_value_dependent(entry->entity_specs.noexception))
            return 1;

        // Must be true
        return (nodecl_is_constant(entry->entity_specs.noexception)
                && const_value_is_nonzero(
                    nodecl_get_constant(entry->entity_specs.noexception)));
    }
}

static void set_defaulted_outside_class_specifier(
        scope_entry_t* entry,
        decl_context_t decl_context,
        const locus_t* locus)
{
    char can_default = 1;

    if (entry->defined)
    {
        const char* qualified_name = get_qualified_symbol_name(entry, decl_context);

        error_printf("%s: cannot default function '%s' already defined\n",
                locus_to_str(locus),
                print_decl_type_str(entry->type_information,
                    decl_context,
                    qualified_name));

        can_default = 0;
    }

    // Must be a special member
    if ((!entry->entity_specs.is_default_constructor
            && !entry->entity_specs.is_copy_constructor
            && !entry->entity_specs.is_move_constructor
            && !entry->entity_specs.is_copy_assignment_operator
            && !entry->entity_specs.is_move_assignment_operator
            && !entry->entity_specs.is_destructor)
            // Without default arguments
            || function_has_default_arguments(entry)
            // Without exception specification
            || !function_has_exception_specification_valid_for_defaulted(entry))
    {
        const char* qualified_name = get_qualified_symbol_name(entry, decl_context);

        error_printf("%s: function '%s' cannot be defaulted\n",
                locus_to_str(locus),
                print_decl_type_str(entry->type_information,
                    decl_context,
                    qualified_name));
        can_default = 0;
    }

    if (can_default)
    {
        entry->entity_specs.is_defaulted = 1;
        entry->defined = 1;
    }
}

static void set_defaulted_inside_class_specifier(
        scope_entry_t* entry,
        decl_context_t decl_context,
        const locus_t* locus)
{
    char can_default = 1;

    if (entry->defined)
    {
        const char* qualified_name = get_qualified_symbol_name(entry, decl_context);

        error_printf("%s: cannot default function '%s' already defined\n",
                locus_to_str(locus),
                print_decl_type_str(entry->type_information,
                    decl_context,
                    qualified_name));

        can_default = 0;
    }

    if (can_default)
    {
        entry->defined = 1;
        entry->entity_specs.is_defaulted = 1;
        entry->entity_specs.is_defined_inside_class_specifier = 1;
    }
}

static void build_scope_defaulted_function_definition(AST a, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    common_defaulted_or_deleted(a, decl_context,
            set_defaulted_outside_class_specifier,
            /* is_template */ 0,
            /* is_explicit_specialization */ 0,
            /* declared_symbols */ NULL,
            nodecl_output);
}

/*
 * This function registers a template declaration
 */
static void build_scope_template_declaration(AST a, 
        AST top_template_decl, 
        decl_context_t decl_context,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list)
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

    switch (ASTType(templated_decl))
    {
        case AST_FUNCTION_DEFINITION :
            {
                build_scope_template_function_definition(templated_decl, template_context,
                        /* is_explicit_specialization */ 0, nodecl_output,
                        declared_symbols, gather_decl_spec_list);

                break;
            }
        case AST_SIMPLE_DECLARATION :
            {
                build_scope_template_simple_declaration(templated_decl, template_context,
                        /* is_explicit_specialization */ 0, nodecl_output,
                        declared_symbols, gather_decl_spec_list);

                break;
            }
        case AST_ALIAS_DECLARATION:
            {
                build_scope_template_alias_declaration(templated_decl, template_context, nodecl_output);
                break;
            }
        case AST_DELETED_FUNCTION_DEFINITION:
            {
                build_scope_template_deleted_function_definition(templated_decl, template_context,
                        /* is_explicit_specialization */ 0, nodecl_output,
                        declared_symbols, gather_decl_spec_list);
                break;
            }
        case AST_DEFAULTED_FUNCTION_DEFINITION:
            {
                build_scope_template_defaulted_function_definition(templated_decl, template_context,
                        /* is_explicit_specialization */ 0, nodecl_output,
                        declared_symbols, gather_decl_spec_list);
                break;
            }
        case AST_TEMPLATE_DECLARATION :
            {
                build_scope_template_declaration(templated_decl, top_template_decl, template_context, nodecl_output,
                        declared_symbols, gather_decl_spec_list);
                break;
            }
        default :
            internal_error("Unknown node type '%s' (line=%s)\n", ast_print_node_type(ASTType(templated_decl)),
                    ast_location(templated_decl));
    }

}

static void push_new_template_header_level(decl_context_t decl_context,
        decl_context_t *template_context,
        char is_explicit_specialization)
{
    (*template_context) = decl_context;
    // A new level of template nesting
    (*template_context).template_parameters =
        counted_xcalloc(1, sizeof(*(*template_context).template_parameters), &_bytes_used_buildscope);

    (*template_context).template_parameters->is_explicit_specialization = is_explicit_specialization;
    (*template_context).template_parameters->enclosing = decl_context.template_parameters;
}

void build_scope_template_header(AST template_parameter_list, 
        decl_context_t decl_context, 
        decl_context_t *template_context,
        nodecl_t* nodecl_output)
{
    push_new_template_header_level(decl_context, template_context, /* is explicit specialization */ 0);

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
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list)
{
    decl_context_t template_context;
    push_new_template_header_level(decl_context, &template_context, /* is explicit specialization */ 1);

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
                        /* is_explicit_specialization */ 1, nodecl_output,
                        declared_symbols, gather_decl_spec_list);
                break;
            }
        case AST_SIMPLE_DECLARATION :
            {
                build_scope_template_simple_declaration(ASTSon0(a), template_context,
                        /* is_explicit_specialization */ 1, nodecl_output,
                        declared_symbols, gather_decl_spec_list);
                break;
            }
        case AST_DELETED_FUNCTION_DEFINITION:
            {
                build_scope_template_deleted_function_definition(ASTSon0(a), template_context,
                        /* is_explicit_specialization */ 1, nodecl_output,
                        declared_symbols, gather_decl_spec_list);
                break;
            }
        case AST_DEFAULTED_FUNCTION_DEFINITION:
            {
                build_scope_template_defaulted_function_definition(ASTSon0(a), template_context,
                        /* is_explicit_specialization */ 1, nodecl_output,
                        declared_symbols, gather_decl_spec_list);
                break;
            }
        case AST_EXPLICIT_SPECIALIZATION :
            {
                build_scope_explicit_template_specialization(ASTSon0(a), template_context,
                        nodecl_output,
                        declared_symbols, gather_decl_spec_list);
                break;
            }
        case AST_TEMPLATE_DECLARATION:
            {
                build_scope_template_declaration(ASTSon0(a), a, template_context,
                        nodecl_output,
                        declared_symbols, gather_decl_spec_list);
                break;
            }
        case AST_ALIAS_DECLARATION:
            {
                error_printf("%s: error: invalid alias-declaration in explicit template specialization\n",
                        ast_location(ASTSon0(a)));
                break;
            }
        default :
            {
                internal_error("Unknown node type '%s'\n", ast_print_node_type(ASTType(ASTSon0(a))));
            }
    }
}

static void build_scope_template_function_definition(
        AST function_definition,
        decl_context_t decl_context,
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list)
{
    /* scope_entry_t* entry = */ build_scope_function_definition(
            function_definition,
            decl_context,
            /* is_template */ 1, is_explicit_specialization, nodecl_output, 
            declared_symbols, gather_decl_spec_list);
}

static void build_scope_template_deleted_function_definition(
        AST function_declaration,
        decl_context_t decl_context,
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list UNUSED_PARAMETER)
{
    common_defaulted_or_deleted(function_declaration, decl_context,
            set_deleted,
            /* is_template */ 1,
            is_explicit_specialization,
            declared_symbols,
            nodecl_output);
}

static void build_scope_template_defaulted_function_definition(
        AST function_declaration,
        decl_context_t decl_context,
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list UNUSED_PARAMETER)
{
    common_defaulted_or_deleted(function_declaration, decl_context,
            set_defaulted_outside_class_specifier,
            /* is_template */ 1,
            is_explicit_specialization,
            declared_symbols,
            nodecl_output);
}

static void build_scope_template_simple_declaration(AST a, decl_context_t decl_context,
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list)
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
    gather_info.is_explicit_specialization = is_explicit_specialization;

    char is_constructor = 0;

    AST type_specifier = NULL;
    if (decl_specifier_seq != NULL)
    {
        type_specifier = ASTSon1(decl_specifier_seq);
        if (init_declarator_list == NULL)
        {
            gather_info.no_declarators = 1;
        }
        else
        {
            if (type_specifier != NULL
                    && ASTType(type_specifier) == AST_CLASS_SPECIFIER)
            {
                error_printf("%s: error: invalid declarator in class template definition\n",
                        ast_location(init_declarator_list));
                init_declarator_list = NULL;
            }
        }

        build_scope_decl_specifier_seq(decl_specifier_seq, &gather_info,
                &simple_type_info, decl_context, nodecl_output);
    }

    // If the type specifier defined a type, add it to the declared symbols
    if (gather_info.defined_type != NULL
            && declared_symbols != NULL)
    {
        *declared_symbols = entry_list_add(*declared_symbols, gather_info.defined_type);
        P_LIST_ADD(gather_decl_spec_list->items, gather_decl_spec_list->num_items, gather_info);
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

        if (ASTSon0(init_declarator_list) != NULL)
        {
            error_printf("%s: error: too many declarators in template declaration\n",
                    ast_location(init_declarator));
        }

        AST declarator = ASTSon0(init_declarator);
        AST initializer = ASTSon1(init_declarator);

        if (simple_type_info != NULL)
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
                &gather_info, new_decl_context);

        char ok = 1;
        if (entry == NULL)
        {
            ok = 0;
        }
        else if (entry->kind == SK_VARIABLE)
        {
            if (!entry->entity_specs.is_member
                    || !entry->entity_specs.is_static)
            {
                error_printf("%s: error: entity '%s' must be a static data member\n",
                        ast_location(a),
                        get_qualified_symbol_name(entry, decl_context));
                ok = 0;
            }
        }
        else if (entry->kind == SK_CLASS
                || entry->kind == SK_TEMPLATE
                || entry->kind == SK_FUNCTION)
        {
            // Fine
            if (initializer != NULL)
            {
                error_printf("%s: error: cannot initialize non-data member '%s\n", 
                        ast_location(a),
                        get_qualified_symbol_name(entry, decl_context));
                ok = 0;
            }
        }
        else
        {
            error_printf("%s: error: invalid declaration of entity '%s'\n", 
                    ast_location(a),
                    get_qualified_symbol_name(entry, decl_context));
            ok = 0;
        }

        if (!ok)
            return;

        // Copy gcc attributes
        keep_gcc_attributes_in_symbol(entry, &gather_info);
        keep_ms_declspecs_in_symbol(entry, &gather_info);

        // Propagate the __extension__ attribute to the symbol
        entry->entity_specs.gcc_extension = gcc_extension;

        if (declared_symbols != NULL)
        {
            *declared_symbols = entry_list_new(entry);
            P_LIST_ADD(gather_decl_spec_list->items, gather_decl_spec_list->num_items, gather_info);
        }

        // This is a simple declaration, thus if it does not declare an
        // extern variable or function, the symbol is already defined here
        if (entry->kind == SK_VARIABLE)
        {
            if (initializer != NULL)
            {
                // Here the template scope is the one of the template declaration,
                // not for the symbol
                decl_context_t initializer_context = entry->decl_context;
                initializer_context.template_parameters = new_decl_context.template_parameters;

                nodecl_t nodecl_expr = nodecl_null();
                check_initialization(initializer,
                        initializer_context,
                        entry,
                        get_unqualified_type(declarator_type),
                        &nodecl_expr,
                        /* is_auto_type */ 0);
                entry->value = nodecl_expr;
            }
            // This is always a definition actually
            entry->defined = 1;
        }

        // Mark this as user declared from now
        entry->entity_specs.is_user_declared = 1;

        nodecl_t (*make_cxx_decl_or_def)(nodecl_t, scope_entry_t*, const locus_t*) =
            // Only variables are actually defined, everything else is a declaration
            (entry->kind == SK_VARIABLE) ? nodecl_make_cxx_def : nodecl_make_cxx_decl;

        decl_context.template_parameters = entry->decl_context.template_parameters;

        nodecl_t nodecl_context =
            nodecl_make_context(/* optional statement sequence */ nodecl_null(),
                    decl_context, ast_get_locus(init_declarator));

        // Keep declaration
        *nodecl_output = nodecl_concat_lists(
                *nodecl_output,
                nodecl_make_list_1(
                    make_cxx_decl_or_def(
                        nodecl_context,
                        entry,
                        ast_get_locus(init_declarator))));
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
        case AST_PARAMETER_DECL :
            build_scope_nontype_template_parameter(a, template_parameter_list, nesting, template_context, nodecl_output);
            break;
        case AST_TYPE_PARAMETER_CLASS :
        case AST_TYPE_PARAMETER_TYPENAME :
            build_scope_type_template_parameter(a, template_parameter_list, nesting,
                    /* is_template_pack */ 0, template_context, nodecl_output);
            break;
        case AST_TYPE_PARAMETER_TYPENAME_PACK:
        case AST_TYPE_PARAMETER_CLASS_PACK:
            if (!IS_CXX11_LANGUAGE)
            {
                warn_printf("%s: warning: template packs are only valid in C++11\n",
                        ast_location(a));
            }
            build_scope_type_template_parameter(a, template_parameter_list, nesting,
                    /* is_template_pack */ 1, template_context, nodecl_output);
            break;
        case AST_TYPE_PARAMETER_TEMPLATE :
            build_scope_template_template_parameter(a, template_parameter_list, nesting,
                    /* is_template_pack */ 0, template_context, nodecl_output);
            break;
        case AST_TYPE_PARAMETER_TEMPLATE_PACK :
            if (!IS_CXX11_LANGUAGE)
            {
                warn_printf("%s: warning: template packs are only valid in C++11\n",
                        ast_location(a));
            }
            build_scope_template_template_parameter(a, template_parameter_list, nesting,
                    /* is_template_pack */ 1, template_context, nodecl_output);
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
        char is_template_pack,
        decl_context_t template_context,
        nodecl_t* nodecl_output)
{
    // These parameters have the form

    //    TEMPLATE < template_param_list > CLASS [...] [identifier] [= id_expr]
    //
    // "identifier" is then a template-name
    //
    // Construct parameter information
    decl_context_t template_params_context = template_context;

    template_params_context.template_parameters = counted_xcalloc(1, 
            sizeof(*template_params_context.template_parameters), &_bytes_used_buildscope);

    build_scope_template_parameter_list(ASTSon0(a), template_params_context.template_parameters,
            /* nesting */ 1, template_params_context, nodecl_output);


    const char* template_parameter_name = NULL;
    if (ASTSon1(a) != NULL)
    {
        AST symbol = ASTSon1(a);
        template_parameter_name = ASTText(symbol);
    }
    else
    {
        uniquestr_sprintf(&template_parameter_name,
                "__tpl_tpl_param_%d_%d__", nesting, template_parameters->num_parameters);
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "BUILDSCOPE: Registering template template-parameter '%s' at position %d\n",
                template_parameter_name,
                template_parameters->num_parameters);
    }

    scope_entry_t* new_entry = counted_xcalloc(1, sizeof(*new_entry), &_bytes_used_buildscope);
    new_entry->symbol_name = template_parameter_name;
    new_entry->decl_context = template_context;

    new_entry->locus = ast_get_locus(a);

    if (!is_template_pack)
        new_entry->kind = SK_TEMPLATE_TEMPLATE_PARAMETER;
    else
        new_entry->kind = SK_TEMPLATE_TEMPLATE_PARAMETER_PACK;

    new_entry->entity_specs.is_template_parameter = 1;
    new_entry->entity_specs.template_parameter_nesting = nesting;
    new_entry->entity_specs.template_parameter_position = template_parameters->num_parameters;

    // This is a faked class type
    type_t* primary_type = get_new_class_type(template_context, TT_CLASS);

    new_entry->type_information = get_new_template_type(template_params_context.template_parameters, 
            /* primary_type = */ primary_type, template_parameter_name, template_context,
            new_entry->locus);

    template_type_set_related_symbol(new_entry->type_information, new_entry);

    template_parameter_t *template_parameter = counted_xcalloc(1, sizeof(*template_parameter), &_bytes_used_buildscope);
    template_parameter->entry = new_entry;

    template_parameter_value_t* default_argument = NULL;

    AST id_expr = ASTSon2(a);
    if (id_expr != NULL)
    {
        // This might be ambiguous
        // check_expression(id_expr, template_context);

        scope_entry_list_t* entry_list = query_id_expression(template_context, id_expr, NULL);

        enum cxx_symbol_kind valid_templates_arguments[] = 
        { 
            SK_TEMPLATE,
            SK_TEMPLATE_TEMPLATE_PARAMETER
        };

        scope_entry_list_t* filtered_entry_list = 
            filter_symbol_kind_set(entry_list, STATIC_ARRAY_LENGTH(valid_templates_arguments), valid_templates_arguments);
        entry_list_free(entry_list);

        if (filtered_entry_list == NULL)
        {
            error_printf("%s: error: '%s' does not name a template class\n",
                    ast_location(id_expr),
                    prettyprint_in_buffer(id_expr));
            return;
        }

        scope_entry_t* entry = entry_list_head(filtered_entry_list);
        entry_list_free(filtered_entry_list);

        if (entry->kind == SK_TEMPLATE
                && named_type_get_symbol(template_type_get_primary_type(entry->type_information))->kind != SK_CLASS)
        {
            error_printf("%s: error: '%s' does not name a template class\n",
                    ast_location(id_expr),
                    prettyprint_in_buffer(id_expr));
            return;
        }

        default_argument = counted_xcalloc(1, sizeof(*default_argument), 
                &_bytes_used_buildscope);
        // We need a named type
        default_argument->type = get_user_defined_type(entry);
        default_argument->is_default = 1;
        default_argument->kind = TPK_TEMPLATE;

        if (is_template_pack)
        {
            error_printf("%s: error: a template-template pack cannot have a default argument\n",
                    ast_location(id_expr));
            default_argument = NULL;
        }
    }

    if (!is_template_pack)
        template_parameter->kind = TPK_TEMPLATE;
    else
        template_parameter->kind = TPK_TEMPLATE_PACK;

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
        char is_template_pack,
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


    const char* template_parameter_name = NULL;
    if (name != NULL)
    {
        // This is a named type parameter. Register it in the symbol table
        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: Registering type template-parameter '%s' with nesting %d and position %d\n",
                    ASTText(name), 
                    nesting,
                    template_parameters->num_parameters);
        }
        // Note that we sign it in the template_scope !
        template_parameter_name = ASTText(name);


        line = ASTLine(name);
        file = ASTFileName(name);
    }
    else
    {
        uniquestr_sprintf(&template_parameter_name, "__type_tpl__param_%d_%d__", nesting, 
                template_parameters->num_parameters);

        line = ASTLine(a);
        file = ASTFileName(a);
    }

    scope_entry_t* new_entry = counted_xcalloc(1, sizeof(*new_entry), &_bytes_used_buildscope);
    new_entry->decl_context = template_context;
    new_entry->symbol_name = template_parameter_name;


    new_entry->locus = make_locus(file, line, 0);
    if (!is_template_pack)
        new_entry->kind = SK_TEMPLATE_TYPE_PARAMETER;
    else
        new_entry->kind = SK_TEMPLATE_TYPE_PARAMETER_PACK;

    new_entry->entity_specs.is_template_parameter = 1;
    new_entry->entity_specs.template_parameter_nesting = nesting;
    new_entry->entity_specs.template_parameter_position = template_parameters->num_parameters;

    template_parameter_t* template_parameter = counted_xcalloc(1, sizeof(*template_parameter), &_bytes_used_buildscope);
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

        build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &type_info,
                template_context, nodecl_output);

        type_t* declarator_type = type_info;
        compute_declarator_type(abstract_decl,
                &gather_info, type_info, &declarator_type,
                template_context, nodecl_output);

        default_argument = counted_xcalloc(1, sizeof(*default_argument), &_bytes_used_buildscope);
        default_argument->type = declarator_type;
        default_argument->is_default = 1;
        default_argument->kind = TPK_TYPE;

        if (is_template_pack)
        {
            error_printf("%s: error: a type-template parameter pack cannot have a default argument\n",
                    ast_location(type_id));
            default_argument = NULL;
        }
    }

    if (!is_template_pack)
        template_parameter->kind = TPK_TYPE;
    else
        template_parameter->kind = TPK_TYPE_PACK;

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

    // Nontype templates parameters are actually parameter declarators
    gather_info.parameter_declaration = 1;

    build_scope_decl_specifier_seq(decl_specifier_seq, &gather_info, &type_info,
            template_context, nodecl_output);

    scope_entry_t* entry = NULL;

    type_t* declarator_type = type_info;
    compute_declarator_type(parameter_declarator,
            &gather_info, type_info, &declarator_type,
            template_context, nodecl_output);

    const char* template_parameter_name = NULL;
    AST declarator_name = get_declarator_name(parameter_declarator, template_context);
    if (declarator_name != NULL)
    {
        // This is a bit sloppy, this declarator should be a simple text
        template_parameter_name = uniquestr(prettyprint_in_buffer(declarator_name));
        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: Registering '%s' as a non-type template parameter at position %d\n", 
                    template_parameter_name,
                    template_parameters->num_parameters);
        }
    }
    else
    {
        uniquestr_sprintf(&template_parameter_name, "__nontype_tpl_param_%d_%d__", 
                nesting, 
                template_parameters->num_parameters);
    }
    entry = counted_xcalloc(1, sizeof(*entry), &_bytes_used_buildscope);
    entry->symbol_name = template_parameter_name;
    entry->decl_context = template_context;

    if (!IS_CXX11_LANGUAGE && gather_info.is_template_pack)
    {
        warn_printf("%s: warning: template-packs are only valid in C++11\n",
                ast_location(a));
    }

    // This is not a variable, but a template parameter
    if (!gather_info.is_template_pack)
        entry->kind = SK_TEMPLATE_NONTYPE_PARAMETER;
    else
        entry->kind = SK_TEMPLATE_NONTYPE_PARAMETER_PACK;

    entry->type_information = declarator_type;
    entry->entity_specs.is_template_parameter = 1;
    entry->entity_specs.template_parameter_nesting = nesting;
    entry->entity_specs.template_parameter_position = template_parameters->num_parameters;

    // Save its symbol
    template_parameter_t* template_parameter = counted_xcalloc(1, sizeof(*template_parameter), &_bytes_used_buildscope);
    template_parameter->entry = entry;
    template_parameter_value_t* default_argument = NULL;
    if (default_expression != NULL)
    {
        nodecl_t nodecl_expr;
        if (!check_nontype_template_argument_expression(default_expression, template_context, &nodecl_expr))
        {
            error_printf("%s: error: could not check default argument of template parameter '%s'\n",
                    ast_location(default_expression),
                    prettyprint_in_buffer(default_expression));
        }

        default_argument = counted_xcalloc(1, sizeof(*default_argument), &_bytes_used_buildscope);
        default_argument->value = nodecl_expr;
        default_argument->type = declarator_type;
        default_argument->is_default = 1;
        default_argument->kind = TPK_NONTYPE;

        if (gather_info.is_template_pack)
        {
            error_printf("%s: error: a nontype-template pack cannot have a default argument\n",
                    ast_location(default_expression));
        }
    }

    if (!gather_info.is_template_pack)
        template_parameter->kind = TPK_NONTYPE;
    else
        template_parameter->kind = TPK_NONTYPE_PACK;

    // We do this because P_LIST_ADD modifies the number of parameters
    int num_parameters = template_parameters->num_parameters;
    P_LIST_ADD(template_parameters->parameters, 
            num_parameters,
            template_parameter);
    P_LIST_ADD(template_parameters->arguments, 
            template_parameters->num_parameters,
            default_argument);
}

static void build_scope_namespace_alias(AST a, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    if (decl_context.current_scope->kind != NAMESPACE_SCOPE)
    {
        error_printf("%s: error: namespace alias in a non namespace scope\n",
                ast_location(a));
        return;
    }

    AST alias_ident = ASTSon0(a);
    AST id_expression = ASTSon1(a);

    scope_entry_list_t* entry_list = query_id_expression(decl_context, id_expression, NULL);

    if (entry_list == NULL
            || entry_list_head(entry_list)->kind != SK_NAMESPACE)
    {
        error_printf("%s: error: '%s' does not name any namespace\n", 
                ast_location(id_expression),
                prettyprint_in_buffer(id_expression));
        return;
    }

    scope_entry_t* entry = entry_list_head(entry_list);
    entry_list_free(entry_list);

    const char* alias_name = ASTText(alias_ident);

    scope_entry_t* alias_entry = new_symbol(decl_context, decl_context.current_scope, alias_name);

    alias_entry->locus = ast_get_locus(alias_ident);
    alias_entry->kind = SK_NAMESPACE;
    alias_entry->related_decl_context = entry->related_decl_context;
    alias_entry->defined = 1;
    alias_entry->entity_specs.is_user_declared = 1;

    *nodecl_output =
        nodecl_make_list_1(
                nodecl_make_cxx_def(
                    nodecl_null(),
                    alias_entry,
                    ast_get_locus(a)));
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
    AST namespace_inline = ASTSon3(a);

    if (namespace_inline != NULL)
    {
        ERROR_CONDITION(ASTType(namespace_inline) != AST_INLINE_SPEC,
                "Invalid inline specifier tree", 0);

        is_inline = 1;
    }

    if (namespace_name != NULL)
    {
        ERROR_CONDITION((decl_context.current_scope->kind != NAMESPACE_SCOPE),
                "Incorrect scope, it should be a namespace scope", 0);

        // Register this namespace if it does not exist in this scope
        scope_entry_list_t* list = query_in_scope_str_flags(decl_context, ASTText(namespace_name), NULL, DF_ONLY_CURRENT_SCOPE);

        scope_entry_list_t* check_list = filter_symbol_non_kind(list, SK_NAMESPACE);

        if (check_list != NULL)
        {
            error_printf("%s: error: '%s' has already been declared as another entity kind\n",
                    ast_location(namespace_name),
                    prettyprint_in_buffer(namespace_name));
            return;
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
                error_printf("%s: error: inline namespace extension of a non-inlined namespace\n",
                        ast_location(a));
                return;
            }
        }
        else
        {
            entry = new_symbol(decl_context, decl_context.current_scope, ASTText(namespace_name));
            namespace_context = new_namespace_context(decl_context, entry);

            entry->locus = ast_get_locus(namespace_name);
            entry->kind = SK_NAMESPACE;
            entry->related_decl_context = namespace_context;
            entry->entity_specs.is_user_declared = 1;

            // Link the scope of this newly created namespace
            if (is_inline)
            {
                entry->entity_specs.is_inline = 1;

                // An inline namespace is an associated namespace of the current namespace
                scope_t* namespace_scope = decl_context.current_scope;

                P_LIST_ADD_ONCE(namespace_scope->use_namespace, namespace_scope->num_used_namespaces,
                        entry);
            }
        }

        // Anonymous namespace cannot have gcc attributes
        AST attributes = ASTSon2(a);
        gather_decl_spec_t gather_info;
        memset(&gather_info, 0, sizeof(gather_info));
        gather_extra_attributes(attributes, &gather_info, decl_context);

        // Copy the gcc attributes
        keep_gcc_attributes_in_symbol(entry, &gather_info);
        keep_ms_declspecs_in_symbol(entry, &gather_info);

        entry_list_free(list);

        if (ASTSon1(a) != NULL)
        {
            build_scope_declaration_sequence(ASTSon1(a), namespace_context, nodecl_output);
        }
    }
    else
    {
        // Register this namespace if it does not exist in this scope
        const char* unnamed_namespace = UNIQUESTR_LITERAL("(unnamed)");
        scope_entry_list_t* list = query_in_scope_str_flags(decl_context, unnamed_namespace, NULL, DF_ONLY_CURRENT_SCOPE);

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
                error_printf("%s: error: inline namespace extension of a non-inlined namespace\n",
                        ast_location(a));
                return;
            }
        }
        else
        {
            scope_entry_t* entry = new_symbol(decl_context, decl_context.current_scope, unnamed_namespace);
            namespace_context = new_namespace_context(decl_context, entry);

            entry->locus = ast_get_locus(a);
            entry->kind = SK_NAMESPACE;
            entry->related_decl_context = namespace_context;

            // Link the scope of this newly created namespace
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
void build_scope_kr_parameter_declaration(scope_entry_t* function_entry,
        AST kr_parameter_declaration, 
        AST kr_parameters UNUSED_PARAMETER,
        decl_context_t decl_context,
        nodecl_t* nodecl_output UNUSED_PARAMETER)
{
    CXX_LANGUAGE()
    {
        internal_error("This function is intended only for C99", 0);
    }

    int real_num_parameters = function_type_get_num_parameters(function_entry->type_information);
    int num_parameters = real_num_parameters;
    if (function_type_get_has_ellipsis(function_entry->type_information))
        num_parameters--;

    parameter_info_t parameter_info[1 + num_parameters];
    memset(parameter_info, 0, sizeof(parameter_info));

    int i;
    for (i = 0; i < num_parameters; i++)
    {
        parameter_info[i].is_ellipsis = 0;
        parameter_info[i].type_info = 
            function_type_get_parameter_type_num(function_entry->type_information, i);
        parameter_info[i].nonadjusted_type_info = parameter_info[i].type_info;
    }

    if (function_type_get_has_ellipsis(function_entry->type_information))
    {
        parameter_info[real_num_parameters - 1].is_ellipsis = 1;
        parameter_info[real_num_parameters - 1].type_info = get_ellipsis_type();
    }

    if (kr_parameter_declaration != NULL)
    {
        AST iter;
        for_each_element(kr_parameter_declaration, iter)
        {
            AST simple_decl = ASTSon1(iter);

            AST decl_spec_seq = ASTSon0(simple_decl);
            AST init_declarator_list = ASTSon1(simple_decl);

            gather_decl_spec_t gather_info;
            memset(&gather_info, 0, sizeof(gather_info));

            type_t* simple_type_info = NULL;
            nodecl_t nodecl_decl_spec = nodecl_null();

            build_scope_decl_specifier_seq(decl_spec_seq, 
                    &gather_info, &simple_type_info,
                    decl_context, &nodecl_decl_spec);

            AST init_declarator_it = NULL;

            i = 0;
            for_each_element (init_declarator_list, init_declarator_it)
            {
                gather_decl_spec_t current_gather_info;
                copy_gather_info(&current_gather_info, &gather_info);

                AST init_declarator = ASTSon1(init_declarator_it);

                AST declarator = ASTSon0(init_declarator);
                AST initializer = ASTSon1(init_declarator);

                nodecl_t nodecl_declarator = nodecl_null();

                type_t* declarator_type = NULL;
                compute_declarator_type(declarator, &current_gather_info,
                        simple_type_info, &declarator_type, decl_context,
                        &nodecl_declarator);

                scope_entry_t *entry = build_scope_declarator_name(declarator, declarator_type, 
                        &current_gather_info, decl_context);

                if (!symbol_is_parameter_of_function(entry, function_entry))
                {
                    error_printf("%s: error: '%s' is not a parameter\n",
                            ast_location(init_declarator),
                            entry->symbol_name);
                    continue;
                }

                if (initializer != NULL)
                {
                    error_printf("%s: error: initializer given to a parameter\n",
                            ast_location(initializer));
                }

                if (current_gather_info.is_static)
                {
                    error_printf("%s: error: parameter '%s' defined to be static\n",
                            ast_location(init_declarator),
                            entry->symbol_name);
                }

                if (current_gather_info.is_extern)
                {
                    error_printf("%s: error: parameter '%s' defined to be extern\n",
                            ast_location(init_declarator),
                            entry->symbol_name);
                }

                entry->type_information = declarator_type;

                // Copy gcc attributes
                keep_gcc_attributes_in_symbol(entry, &current_gather_info);
                keep_ms_declspecs_in_symbol(entry, &current_gather_info);

                int parameter_position = -1;

                int j;
                for (j = 0; j < function_entry->entity_specs.num_related_symbols && parameter_position == -1; j++)
                {
                    if (function_entry->entity_specs.related_symbols[j] == entry)
                    {
                        parameter_position = j;
                    }
                }

                ERROR_CONDITION(parameter_position < 0, "Parameter not found", 0);

                type_t* adjusted_type_info = entry->type_information;
                // If the original type is a typedef then we want to ignore
                // all the indirections
                adjusted_type_info = advance_over_typedefs(adjusted_type_info);

                // function to pointer-to-function standard conversion
                if (is_function_type(adjusted_type_info))
                {
                    adjusted_type_info = get_pointer_type(adjusted_type_info);
                }
                // Array to pointer standard conversion
                else if (is_array_type(adjusted_type_info))
                {
                    adjusted_type_info = array_type_get_element_type(adjusted_type_info);
                    adjusted_type_info = get_pointer_type(adjusted_type_info);
                }

                parameter_info[parameter_position].type_info = get_unqualified_type (adjusted_type_info);
                parameter_info[parameter_position].nonadjusted_type_info = declarator_type;

                // Fix the type of the parameter
                if (!equivalent_types(entry->type_information, adjusted_type_info))
                    entry->type_information = adjusted_type_info;

                i++;
            }

            if (i == 0)
            {
                error_printf("%s: error: declaration does not declare anything\n",
                        ast_location(simple_decl));
            }
        }
    }

    // Update the type
    function_entry->type_information = get_new_function_type(
            function_type_get_return_type(function_entry->type_information),
            parameter_info,
            real_num_parameters, REF_QUALIFIER_NONE);
}

static void common_defaulted_or_deleted(AST a, decl_context_t decl_context, 
        void (*set)(scope_entry_t*, decl_context_t, const locus_t* locus),
        char is_template,
        char is_explicit_specialization,
        scope_entry_list_t** declared_symbols,
        nodecl_t* nodecl_output)
{
    CXX03_LANGUAGE()
    {
        warn_printf("%s: warning: default/delete functions are a C++11 feature\n",
                ast_location(a));
    }

    AST function_header = ASTSon0(a);

    if (ASTType(function_header) == AST_AMBIGUITY)
    {
        solve_ambiguous_function_header(function_header, decl_context);
    }

    AST decl_spec_seq = ASTSon0(function_header);
    AST function_declarator = ASTSon1(function_header);
    /* AST attributes = ASTSon2(function_header); */

    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));

    gather_info.is_template = is_template;
    gather_info.is_explicit_specialization = is_explicit_specialization;

    type_t* type_info = NULL;

    char is_constructor = 0;

    if (decl_spec_seq != NULL)
    {
        build_scope_decl_specifier_seq(decl_spec_seq, &gather_info,
                &type_info, decl_context, nodecl_output);
    }

    if (type_info == NULL)
    {
        if (is_constructor_declarator(function_declarator))
        {
            is_constructor = 1;
        }
    }

    // function_declarator
    type_t* declarator_type = NULL;
    scope_entry_t* entry = NULL;

    decl_context_t new_decl_context = decl_context;
    if (is_constructor)
    {
        new_decl_context.decl_flags |= DF_CONSTRUCTOR;
    }

    compute_declarator_type(function_declarator, &gather_info, type_info,
            &declarator_type, new_decl_context, nodecl_output);
    entry = build_scope_declarator_name(function_declarator, declarator_type, &gather_info, new_decl_context);

    ERROR_CONDITION(entry == NULL, "Invalid symbol", 0);

    set(entry, decl_context, ast_get_locus(a));

    // Copy gcc attributes
    keep_gcc_attributes_in_symbol(entry, &gather_info);
    keep_ms_declspecs_in_symbol(entry, &gather_info);

    if (declared_symbols != NULL)
    {
        *declared_symbols = entry_list_add(*declared_symbols, entry);
    }

    *nodecl_output = nodecl_append_to_list(*nodecl_output,
            nodecl_make_cxx_decl(
                nodecl_make_context(
                    nodecl_null(),
                    decl_context,
                    ast_get_locus(a)),
                entry,
                ast_get_locus(a)));
}


void set_parameters_as_related_symbols(scope_entry_t* entry,
        gather_decl_spec_t* gather_info,
        char is_definition,
        const locus_t* locus)
{
    if (entry->entity_specs.related_symbols == NULL)
    {
        // Allocated for the first time
        entry->entity_specs.num_related_symbols = gather_info->num_arguments_info;
        entry->entity_specs.related_symbols = counted_xcalloc(gather_info->num_arguments_info,
                sizeof(*entry->entity_specs.related_symbols),
                &_bytes_used_buildscope);
    }
    else
    {
        if (entry->entity_specs.num_related_symbols != gather_info->num_arguments_info)
        {
            // A mismatching number of parameters, xrealloc
            xfree(entry->entity_specs.related_symbols);

            entry->entity_specs.num_related_symbols = gather_info->num_arguments_info;
            entry->entity_specs.related_symbols = counted_xcalloc(gather_info->num_arguments_info,
                    sizeof(*entry->entity_specs.related_symbols),
                    &_bytes_used_buildscope);
        }
    }

    int i;
    for (i = 0; i < gather_info->num_arguments_info; i++)
    {
        // In C they must have name in a definition
        C_LANGUAGE()
        {
            if (is_definition
                    && gather_info->arguments_info[i].entry == NULL)
            {
                error_printf("%s: error: parameter %d does not have name\n",
                        locus_to_str(locus), i + 1);
            }
        }

        scope_entry_t* current_param = gather_info->arguments_info[i].entry;
        if (current_param != NULL)
        {
            if (current_param->decl_context.current_scope->related_entry == NULL
                    && current_param->decl_context.current_scope->kind == PROTOTYPE_SCOPE)
            {
                // Make sure this prototype scope knows what function it refers
                // (A prototype scope may not have related entry because there
                // may be none. For instance in a nested function declarator)
                current_param->decl_context.current_scope->related_entry = entry;
            }

            // Remember this symbol as a parameter of entry
            symbol_set_as_parameter_of_function(current_param, entry, /* nesting */ 0, /* position */ i);
        }

        // We keep the first parameter declaration or the definition (ignoring any other declaration)
        if (is_definition
                || entry->entity_specs.related_symbols[i] == NULL)
        {
            entry->entity_specs.related_symbols[i] = current_param;
        }
    }
}

static char mercurium_pretty_function_has_been_used(scope_entry_t* mercurium_pretty_function,
        nodecl_t node)
{
    if (nodecl_is_null(node))
        return 0;

    // Stop in nested functions
    if (nodecl_get_kind(node) == NODECL_FUNCTION_CODE)
        return 0;

    if (nodecl_get_symbol(node) == mercurium_pretty_function)
        return 1;

    int i;
    for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
    {
        if (mercurium_pretty_function_has_been_used(
                    mercurium_pretty_function,
                    nodecl_get_child(node, i)))
            return 1;
    }

    return 0;
}

char check_constexpr_function(scope_entry_t* entry, const locus_t* locus,
        char emit_error)
{
    if (entry->entity_specs.is_virtual)
    {
        warn_or_error_printf(emit_error,
                "%s: %s: a constexpr function cannot be virtual\n",
                emit_error ? "error" : "warning",
                locus_to_str(locus));
        return 0;
    }

    int num_types = function_type_get_num_parameters(entry->type_information);
    if (function_type_get_has_ellipsis(entry->type_information))
        num_types--;

    int i;
    for (i = 0; i < num_types; i++)
    {
        type_t* param_type = no_ref(function_type_get_parameter_type_num(entry->type_information, i));
        if (!is_dependent_type(param_type)
                && !is_literal_type(param_type))
        {
            warn_or_error_printf(
                    emit_error,
                    "%s: %s: parameter types of a constexpr function or constructor must be a literal type or "
                    "reference to literal type\n",
                    emit_error ? "error" : "warning",
                    locus_to_str(locus));
            return 0;
        }
    }

    if (!entry->entity_specs.is_constructor)
    {
        type_t* return_type = function_type_get_return_type(entry->type_information);

        if (!is_dependent_type(return_type)
                && !is_literal_type(no_ref(return_type)))
        {
            warn_or_error_printf(
                    emit_error,
                    "%s: %s: the return type of a constexpr function must be a literal type or reference to literal type\n",
                    emit_error ? "error" : "warning",
                    locus_to_str(locus));
            return 0;
        }
    }

    return 1;
}

static char check_constexpr_function_body(scope_entry_t* entry, nodecl_t nodecl_body,
        char emit_error)
{
    if (nodecl_get_kind(nodecl_body) != NODECL_COMPOUND_STATEMENT)
    {
        warn_or_error_printf(emit_error,
                "%s: %s: the body of a constexpr function or constructor must be a compound-statement\n",
                emit_error ? "error" : "warning",
                nodecl_locus_to_str(nodecl_body));
        return 0;
    }

    nodecl_t compound_list = nodecl_get_child(nodecl_body, 0);

    if (entry->entity_specs.is_constructor)
    {
        if (nodecl_list_length(compound_list) != 0)
        {
            warn_or_error_printf(
                    emit_error,
                    "%s: %s: the compound-statement of a constexpr constructor must contain no statements\n",
                    emit_error ? "error" : "warning",
                    nodecl_locus_to_str(nodecl_body));
            return 0;
        }
    }
    else
    {
        int num_seen_returns = 0;
        int num_seen_other_statements = 0;

        int num_items = 0;
        nodecl_t* l = nodecl_unpack_list(compound_list, &num_items);

        int i;
        for (i = 0; i < num_items; i++)
        {
            if (nodecl_get_kind(l[i]) == NODECL_CXX_DECL
                    || nodecl_get_kind(l[i]) == NODECL_CXX_DEF
                    || nodecl_get_kind(l[i]) == NODECL_CXX_USING_DECL
                    || nodecl_get_kind(l[i]) == NODECL_CXX_USING_NAMESPACE)
            {
                // These are declarations, ignore them
            }
            else if (nodecl_get_kind(l[i]) == NODECL_RETURN_STATEMENT)
            {
                num_seen_returns++;
            }
            else
            {
                num_seen_other_statements++;
            }
        }

        if (num_seen_other_statements != 0
                || num_seen_returns != 1)
        {
            warn_or_error_printf(
                    emit_error,
                    "%s: %s: the body of a constexpr function must contain a single return-statement\n",
                    emit_error ? "error" : "warning",
                    nodecl_locus_to_str(nodecl_body));
            return 0;
        }
    }

    return 1;
}

char check_constexpr_function_code(scope_entry_t* entry, nodecl_t nodecl_function_code,
        char emit_error)
{
    nodecl_t nodecl_context = nodecl_get_child(nodecl_function_code, 0);

    nodecl_t nodecl_list = nodecl_get_child(nodecl_context, 0);
    ERROR_CONDITION(nodecl_list_length(nodecl_list) != 1, "Invalid function code", 0);

    nodecl_t nodecl_body = nodecl_list_head(nodecl_list);

    return check_constexpr_function_body(entry, nodecl_body, emit_error);
}

static scope_entry_t* build_scope_function_definition_declarator(
        AST function_definition,
        decl_context_t decl_context,
        char is_template,
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list,

        gather_decl_spec_t * gather_info,
        decl_context_t* block_context,
        char *is_constructor
        )
{
    DEBUG_CODE()
    {
        fprintf(stderr, "BUILDSCOPE: Function definition\n");
    }
    // A function definition has four parts
    //   decl_specifier_seq declarator ctor_initializer function_body

    // decl_specifier_seq [optional]
    // If there is no decl_specifier_seq this has to be a destructor, constructor or conversion function
    gather_info->is_template = is_template;
    gather_info->is_explicit_specialization = is_explicit_specialization;

    type_t* type_info = NULL;

    AST function_header = ASTSon0(function_definition);

    if (ASTType(function_header) == AST_AMBIGUITY)
    {
        solve_ambiguous_function_header(function_header, decl_context);
    }

    AST decl_spec_seq = ASTSon0(function_header);
    AST function_declarator = ASTSon1(function_header);
    AST attributes = ASTSon2(function_header);

    gather_extra_attributes(attributes, gather_info, decl_context);

    if (decl_spec_seq != NULL)
    {
        build_scope_decl_specifier_seq(decl_spec_seq, gather_info,
                &type_info, decl_context, nodecl_output);
    }

    if (type_info == NULL)
    {
        CXX_LANGUAGE()
        {
            // This is a constructor
            if (is_constructor_declarator(function_declarator))
            {
                *is_constructor = 1;

                AST declarator_name = get_declarator_name(function_declarator, decl_context);
                if (decl_context.current_scope->kind == CLASS_SCOPE
                        && ASTType(declarator_name) == AST_TEMPLATE_ID)
                {
                    scope_entry_list_t* entry_list = query_id_expression(decl_context, declarator_name, NULL);
                    if (entry_list == NULL
                            || entry_list_head(entry_list)->kind != SK_CLASS
                            || (class_symbol_get_canonical_symbol(entry_list_head(entry_list))
                                != decl_context.current_scope->related_entry))
                    {
                        error_printf("%s: error: invalid constructor declaration '%s'\n",
                                ast_location(declarator_name),
                                prettyprint_in_buffer(declarator_name));
                        return NULL;
                    }
                    // Clobber declarator_name with something sane
                    //
                    // A<int>() will become A()
                    //
                    // This is mean but constructors in this form are too unwieldy
                    AST parent = ASTParent(declarator_name);
                    int child_num = ast_num_of_given_child(parent, declarator_name);
                    ast_replace(declarator_name, ASTSon0(declarator_name));
                    ast_set_child(parent, child_num, declarator_name);
                }
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
                    warn_printf("%s: warning: function definition does not have decl-specifier, assuming 'int'\n",
                            ast_location(function_definition));
                }
                else
                {
                    warn_printf("%s: warning: function definition does not have type-specifier, assuming 'int'\n",
                            ast_location(function_definition));
                }

                type_info = get_signed_int_type();
            }
        }
    }

    // block context
    *block_context = new_block_context(decl_context);
    // This does not modify block_context.current_scope, it simply adds a function_scope to the context
    *block_context = new_function_context(*block_context);

    // declarator type
    type_t* declarator_type = NULL;
    scope_entry_t* entry = NULL;

    decl_context_t new_decl_context = decl_context;
    if (*is_constructor)
    {
        new_decl_context.decl_flags |= DF_CONSTRUCTOR;
    }

    // block-context will be updated for qualified-id to reflect the exact context
    build_scope_declarator_with_parameter_context(function_declarator, gather_info, type_info, &declarator_type,
            new_decl_context, block_context, nodecl_output);

    if (is_error_type(declarator_type))
    {
        fprintf(stderr, "%s: error: discarding function definition due to errors in the declarator\n",
                ast_location(function_header));
        return NULL;
    }

    entry = build_scope_declarator_name(function_declarator, declarator_type, gather_info, new_decl_context);

    if (entry == NULL)
    {
        if (!is_error_type(declarator_type))
        {
            error_printf("%s: error: function '%s' was not found in the current scope\n",
                    ast_location(function_header),
                    print_decl_type_str(declarator_type, new_decl_context,
                        prettyprint_in_buffer(get_declarator_name(function_declarator, new_decl_context))));
        }
        else
        {
            // If no type was synthesized at all use the declarator instead (less nice, though)
            error_printf("%s: error: function '%s' was not found in the current scope\n",
                    ast_location(function_header),
                    prettyprint_in_buffer(function_declarator));
        }
        return NULL;
    }

    // Copy gcc attributes
    keep_gcc_attributes_in_symbol(entry, gather_info);
    keep_ms_declspecs_in_symbol(entry, gather_info);

    // Propagate the __extension__ attribute to the symbol
    entry->entity_specs.gcc_extension = gcc_extension;

    if (declared_symbols != NULL)
    {
        *declared_symbols = entry_list_new(entry);
        P_LIST_ADD(gather_decl_spec_list->items, gather_decl_spec_list->num_items, *gather_info);
    }

    // Set the related entry
    block_context->current_scope->related_entry = entry;

    if (decl_context.current_scope->kind == BLOCK_SCOPE)
    {
        if (entry->entity_specs.is_extern)
        {
            error_printf("%s: error: definition of a nested function already declared as an extern\n",
                    ast_location(function_header));
        }
        entry->entity_specs.is_nested_function = 1;
    }

    if (entry->defined)
    {
        const char *funct_name = entry->symbol_name;
        CXX_LANGUAGE()
        {
            const char* qualified_name = get_qualified_symbol_name(entry, decl_context);

            funct_name = print_decl_type_str(entry->type_information,
                    decl_context,
                    qualified_name);
        }
        error_printf("%s: error: function '%s' already defined in '%s'\n",
                ast_location(function_definition),
                funct_name,
                locus_to_str(entry->locus));
        return NULL;
    }

    entry->entity_specs.is_constexpr |= gather_info->is_constexpr;
    entry->entity_specs.is_inline |= (gather_info->is_inline || gather_info->is_constexpr);

    // Set defined now, otherwise some infinite recursion may happen when
    // instantiating template functions
    entry->defined = 1;

    ERROR_CONDITION((entry->kind != SK_FUNCTION
                && entry->kind != SK_DEPENDENT_FRIEND_FUNCTION),
            "This is not a function!!!", 0);

    // Keep parameter names
    set_parameters_as_related_symbols(entry, gather_info,
            /* is_definition */ 1,
            ast_get_locus(function_definition));

    C_LANGUAGE()
    {
        // Ensure we use the type of the definition
        entry->type_information = declarator_type;

        AST kr_parameter_declaration = ASTSon1(function_definition);
        AST kr_parameter_list = get_function_declarator_parameter_list(function_declarator, decl_context);

        if (kr_parameter_declaration != NULL
                || ASTType(kr_parameter_list) == AST_KR_PARAMETER_LIST)
        {
            build_scope_kr_parameter_declaration(entry, kr_parameter_declaration, 
                    kr_parameter_list, *block_context, nodecl_output);
        }
    }

    return entry;
}

static void build_scope_function_definition_body(
        AST function_definition,
        scope_entry_t* entry,
        decl_context_t block_context,
        gather_decl_spec_t* gather_info,
        nodecl_t *nodecl_output)
{
    // Function_body
    AST function_body = ASTSon2(function_definition);
    AST statement = ASTSon0(function_body);

    // Here we update the type of 'this'
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

            scope_entry_list_t* entry_list = query_name_str(block_context, UNIQUESTR_LITERAL("this"), NULL);
            // If the function is defined inside the class specifier, build_scope_function_definition_declarator
            ERROR_CONDITION(entry_list == NULL, "Symbol 'this' somehow got lost in this context\n", 0);
            scope_entry_t *this_symbol = entry_list_head(entry_list);
            entry_list_free(entry_list);

            this_symbol->type_information = this_type;
        }
    }

    CXX11_LANGUAGE()
    {
        if (function_type_get_ref_qualifier(entry->type_information) != REF_QUALIFIER_NONE
                && (!entry->entity_specs.is_member
                    || entry->entity_specs.is_static))
        {
            error_printf("%s: error: only nonstatic member functions may have ref-qualifier\n",
                    ast_location(function_definition));
        }
    }

    nodecl_t nodecl_initializers = nodecl_null();
    CXX_LANGUAGE()
    {
        AST ctor_initializer = ASTSon1(function_definition);
        if (entry->entity_specs.is_member
                && entry->entity_specs.is_constructor)
        {
            AST location = ctor_initializer;
            if (ctor_initializer == NULL)
                location = function_definition;
            build_scope_ctor_initializer(ctor_initializer, 
                    entry, block_context, 
                    ast_get_locus(location),
                    &nodecl_initializers);
        }
        else
        {
            if (ctor_initializer != NULL)
            {
                error_printf("%s: error: member-initializer-lists are only valid in constructors\n",
                        ast_location(function_definition));
            }
        }
    }

    // FIXME - Think how to make this better maintained
    if (CURRENT_CONFIGURATION->enable_cuda
            && (gather_info->cuda.is_global
                || gather_info->cuda.is_device))
    {
        cuda_kernel_symbols_for_function_body(function_body, gather_info, entry->decl_context, block_context);
    }

    // Sign in __func__ (C99) and GCC's __FUNCTION__ and __PRETTY_FUNCTION__
    scope_entry_t* mercurium_pretty_function = NULL;
    {
        nodecl_t nodecl_expr = const_value_to_nodecl(
                const_value_make_string_null_ended(entry->symbol_name, strlen(entry->symbol_name)));

        // Adjust type to include room for the final \0
        nodecl_set_type(nodecl_expr,
                get_array_type(
                    get_const_qualified_type(get_char_type()),
                    nodecl_make_integer_literal(
                        get_signed_int_type(),
                        const_value_get_signed_int(strlen(entry->symbol_name) + 1),
                        make_locus("", 0, 0)),
                    block_context));

        const char* func_names[] =
        {
            "__func__",
            "__FUNCTION__",
        };

        unsigned int j;
        for (j = 0; j < STATIC_ARRAY_LENGTH(func_names); j++)
        {
            scope_entry_t* func_var = new_symbol(block_context, block_context.current_scope, func_names[j]);
            func_var->kind = SK_VARIABLE;
            func_var->type_information = no_ref(nodecl_get_type(nodecl_expr));
            func_var->value = nodecl_expr;
            func_var->entity_specs.is_builtin = 1;
        }

        // if (is_dependent_function(entry))
        // {
        //     // Insert a dependent __PRETTY_FUNCTION__
        //     scope_entry_t* pretty_function = new_symbol(block_context,
        //             block_context.current_scope,
        //             "__PRETTY_FUNCTION__");
        //     pretty_function->kind = SK_VARIABLE;
        //     pretty_function->type_information = get_unknown_dependent_type();
        //     pretty_function->entity_specs.is_builtin = 1;
        // }
        // else
        {
            const char* nice_name =
                print_decl_type_str(entry->type_information,
                        entry->decl_context, get_qualified_symbol_name(entry, entry->decl_context));
            const_value_t* nice_name_value = const_value_make_string_null_ended(nice_name, strlen(nice_name));
            nodecl_t nice_name_tree = const_value_to_nodecl(nice_name_value);

            // Adjust type to include room for the final \0
            nodecl_set_type(nice_name_tree,
                    get_array_type(
                        get_const_qualified_type(get_char_type()),
                        nodecl_make_integer_literal(get_signed_int_type(),
                            const_value_get_signed_int(strlen(nice_name) + 1),
                            make_locus("", 0, 0)),
                        block_context));

            // __PRETTY_FUNCTION__ is very compiler specific, so we will sign in a
            // __MERCURIUM_PRETTY_FUNCTION__ and make __PRETTY_FUNCTION__ an alias
            // to it
            //
            // Sign in __MERCURIUM_PRETTY_FUNCTION__
            mercurium_pretty_function = new_symbol(block_context,
                    block_context.current_scope,
                    "__MERCURIUM_PRETTY_FUNCTION__");
            mercurium_pretty_function->kind = SK_VARIABLE;
            mercurium_pretty_function->type_information = no_ref(nodecl_get_type(nice_name_tree));
            mercurium_pretty_function->value = nice_name_tree;
            mercurium_pretty_function->entity_specs.is_user_declared = 1;
            mercurium_pretty_function->entity_specs.is_static = 1;

            // Register __PRETTY_FUNCTION__ as an alias to __MERCURIUM_PRETTY_FUNCTION__
            insert_alias(block_context.current_scope, mercurium_pretty_function, "__PRETTY_FUNCTION__");
        }
    }

    // Result symbol only if the function returns something
    if (function_type_get_return_type(entry->type_information) != NULL
            && !is_void_type(function_type_get_return_type(entry->type_information)))
    {
        scope_entry_t* result_sym = new_symbol(block_context,
                block_context.current_scope,
                ".result"); // This name is currently not user accessible
        result_sym->kind = SK_VARIABLE;
        result_sym->entity_specs.is_result_var = 1;
        result_sym->type_information = get_unqualified_type(function_type_get_return_type(entry->type_information));

        entry->entity_specs.result_var = result_sym;
    }

    linkage_push(NULL, /* is_braced */ 1);

    nodecl_t body_nodecl = nodecl_null();
    if (ASTType(statement) == AST_COMPOUND_STATEMENT)
    {
        // We want to inherit the block context to this compound statement
        // so build_scope_statement cannot be used, because it would create
        // one for the compound statement
        AST list = ASTSon0(statement);
        if (list != NULL)
        {
            build_scope_statement_seq(list, block_context, &body_nodecl);
        }

        // Emit __MERCURIUM_PRETTY_FUNCTION__ if needed, otherwise do not emit it
        if (mercurium_pretty_function != NULL
                && mercurium_pretty_function_has_been_used(mercurium_pretty_function, body_nodecl))
        {
            nodecl_t emit_mercurium_pretty_function = nodecl_null();
            CXX_LANGUAGE()
            {
                emit_mercurium_pretty_function = nodecl_append_to_list(
                        emit_mercurium_pretty_function,
                        nodecl_make_cxx_def(
                            nodecl_null(),
                            mercurium_pretty_function,
                            ast_get_locus(statement)));
            }
            emit_mercurium_pretty_function = nodecl_append_to_list(
                    emit_mercurium_pretty_function,
                    nodecl_make_object_init(mercurium_pretty_function,
                        ast_get_locus(statement)));

            body_nodecl = nodecl_concat_lists(emit_mercurium_pretty_function, body_nodecl);
        }

        // C99 VLA object-inits
        C_LANGUAGE()
        {
            nodecl_t nodecl_vla_init = nodecl_null();
            int i;
            for (i = 0; i < gather_info->num_vla_dimension_symbols; i++)
            {
                scope_entry_t* vla_dim = gather_info->vla_dimension_symbols[i];

                nodecl_vla_init = nodecl_append_to_list(
                        nodecl_vla_init,
                        nodecl_make_object_init(
                            vla_dim,
                            ast_get_locus(statement)));
            }

            gather_info->num_vla_dimension_symbols = 0;
            xfree(gather_info->vla_dimension_symbols);
            gather_info->vla_dimension_symbols = NULL;

            // Note that we are prepending an object init list for VLAs
            body_nodecl = nodecl_concat_lists(nodecl_vla_init, body_nodecl);
        }

        // C++ destructors
        nodecl_t nodecl_destructors = nodecl_null();
        CXX_LANGUAGE()
        {
            call_destructors_of_classes(block_context, ast_get_locus(statement), &nodecl_destructors);
        }

        // We manually create a compound statement here for nodecl
        body_nodecl = nodecl_make_compound_statement(body_nodecl, nodecl_destructors, 
                ast_get_locus(statement));
    }
    else if (ASTType(statement) == AST_TRY_BLOCK)
    {
        // FIXME - Wrap this inside a big compound statement
        // This only can be a try-except, but a normal context is created
        // for this one
        build_scope_statement(statement, block_context, &body_nodecl);
    }
    else
    {
        internal_error("Unreachable code", 0);
    }

    linkage_pop();

    if (entry->entity_specs.is_constexpr)
    {
        check_constexpr_function(entry, nodecl_get_locus(body_nodecl), /* emit_error */ 1);
        check_constexpr_function_body(entry, body_nodecl, /* emit_error */ 1);
    }

    nodecl_t (*ptr_nodecl_make_func_code)(nodecl_t, nodecl_t, scope_entry_t*, const locus_t* locus) = NULL;

    ptr_nodecl_make_func_code = is_dependent_function(entry)
        ? &nodecl_make_template_function_code : &nodecl_make_function_code;

    // Create nodecl
    nodecl_t nodecl_function_def = ptr_nodecl_make_func_code(
            nodecl_make_context(
                nodecl_make_list_1(body_nodecl),
                block_context,
                ast_get_locus(function_definition)),
            nodecl_initializers,
            entry,
            ast_get_locus(function_definition));

    *nodecl_output = nodecl_make_list_1(nodecl_function_def);
    entry->entity_specs.function_code = nodecl_function_def;
}


/*
 * This function builds symbol table information for a function definition
 *
 * If previous_symbol != NULL, the found symbol should match
 */
static scope_entry_t* build_scope_function_definition(
        AST function_definition,
        decl_context_t decl_context,
        char is_template,
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list)
{
    decl_context_t block_context;

    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));

    char is_constructor = 0;
    scope_entry_t* entry = build_scope_function_definition_declarator(
            function_definition,
            decl_context,
            is_template,
            is_explicit_specialization,
            nodecl_output,
            declared_symbols,
            gather_decl_spec_list,

            &gather_info,
            &block_context,
            &is_constructor);

    if (entry == NULL)
        return NULL;

    build_scope_function_definition_body(
            function_definition,
            entry,
            block_context,
            &gather_info,
            nodecl_output);

    return entry;
}

static void build_scope_member_declaration(decl_context_t inner_decl_context,
        AST a, access_specifier_t current_access, 
        type_t* class_info,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list)
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
                        /* is_template */ 0, /* is_explicit_specialization */ 0,
                        nodecl_output, declared_symbols, gather_decl_spec_list);
                break;
            }
        case AST_FUNCTION_DEFINITION :
            {
                build_scope_member_function_definition(inner_decl_context, a, current_access, class_info, 
                        /* is_template */ 0, /* is_explicit_specialization */ 0,
                        nodecl_output,
                        declared_symbols, gather_decl_spec_list);
                break;
            }
        case AST_DEFAULTED_FUNCTION_DEFINITION:
        case AST_DELETED_FUNCTION_DEFINITION:
            {
                build_scope_default_or_delete_member_function_definition(inner_decl_context, a, current_access, class_info, 
                        /* is_template */ 0,
                        /* is_explicit_specialization */ 0,
                        nodecl_output);
                break;
            }
        case AST_GCC_EXTENSION : // __extension__
            {
                gcc_extension = 1;

                build_scope_member_declaration(inner_decl_context, ASTSon0(a), current_access, class_info,
                        nodecl_output, declared_symbols, gather_decl_spec_list);

                gcc_extension = 0;
                break;
            }
        case AST_TEMPLATE_DECLARATION :
            {
                build_scope_member_template_declaration(inner_decl_context, a, current_access,
                        class_info, /* is_explicit_specialization */ 0, nodecl_output,
                        declared_symbols, gather_decl_spec_list);
                break;
            }
        case AST_USING_DECLARATION :
        case AST_USING_DECLARATION_TYPENAME :
            {
                build_scope_using_declaration(a, inner_decl_context, current_access, 
                        /* is_typename */ ASTType(a) == AST_USING_DECLARATION_TYPENAME,
                        nodecl_output);
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
        case AST_ALIAS_DECLARATION:
            {
                build_scope_member_alias_declaration(a, inner_decl_context, nodecl_output,
                        class_info, current_access);
                break;
            }
        case AST_AMBIGUITY :
            {
                solve_ambiguous_declaration(a, inner_decl_context);
                // Restart
                build_scope_member_declaration(inner_decl_context, a, current_access, class_info, nodecl_output, 
                        declared_symbols, gather_decl_spec_list);
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
                build_scope_pragma_custom_directive(a, inner_decl_context, nodecl_output);
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
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list
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

    switch (ASTType(templated_decl))
    {
        case AST_FUNCTION_DEFINITION :
            {
                build_scope_member_template_function_definition(template_context, templated_decl, current_access, class_info, 
                        is_explicit_specialization, nodecl_output, declared_symbols, gather_decl_spec_list);
            }
            break;
        case AST_SIMPLE_DECLARATION :
            {
                build_scope_member_template_simple_declaration(template_context, templated_decl, current_access, class_info, 
                        is_explicit_specialization, nodecl_output);
                break;
            }
        case AST_ALIAS_DECLARATION:
            {
                build_scope_member_template_alias_declaration(template_context, templated_decl,
                        current_access, class_info, is_explicit_specialization, nodecl_output);
                break;
            }
        case AST_DELETED_FUNCTION_DEFINITION:
        case AST_DEFAULTED_FUNCTION_DEFINITION:
            {
                build_scope_default_or_delete_template_member_function_definition(template_context, templated_decl,
                        current_access, class_info, is_explicit_specialization, nodecl_output);
                break;
            }
        default :
            internal_error("Unknown node type '%s' at %s\n", ast_print_node_type(ASTType(templated_decl)), ast_location(templated_decl));
    }

}

static void build_scope_member_template_function_definition(decl_context_t decl_context,
        AST a, access_specifier_t current_access, type_t* class_info, 
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list)
{
    // Define the function within st scope but being visible template_scope
    build_scope_member_function_definition(decl_context, a, current_access, class_info, 
            /* is_template */ 1, is_explicit_specialization,
            nodecl_output,
            declared_symbols, gather_decl_spec_list);
}

static void build_scope_member_template_simple_declaration(decl_context_t decl_context,
        AST a, access_specifier_t current_access, type_t* class_info,
        char is_explicit_specialization,
        nodecl_t* nodecl_output)
{
    build_scope_member_simple_declaration(decl_context, a, current_access, class_info, 
            /* is_template */ 1, is_explicit_specialization,
            nodecl_output, /* declared_symbols */ NULL, /* gather_decl_spec_t */ NULL);
}

static void build_scope_member_template_alias_declaration(decl_context_t decl_context,
        AST a, access_specifier_t current_access, type_t* class_info,
        char is_explicit_specialization,
        nodecl_t* nodecl_output)
{
    build_scope_common_template_alias_declaration(a, decl_context, nodecl_output,
            /* is_member_declaration */ 1, class_info, current_access, is_explicit_specialization);
}

static void build_scope_default_or_delete_template_member_function_definition(
        decl_context_t decl_context, 
        AST a, access_specifier_t current_access, type_t* class_info,
        char is_explicit_specialization,
        nodecl_t* nodecl_output)
{
    build_scope_default_or_delete_member_function_definition(decl_context,
            a, current_access, class_info,
            /* is_template */ 1,
            is_explicit_specialization,
            nodecl_output);
}

char function_is_copy_assignment_operator(scope_entry_t* entry, type_t* class_type)
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
        if ((is_lvalue_reference_type(first_parameter)
                    && equivalent_types_in_context(class_type,
                        get_unqualified_type(reference_type_get_referenced_type(first_parameter)),
                        entry->decl_context))
                || (equivalent_types_in_context(class_type, first_parameter, entry->decl_context)))
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
        if (is_rvalue_reference_type(first_parameter)
                && equivalent_types_in_context(class_type,
                    get_unqualified_type(reference_type_get_referenced_type(first_parameter)),
                    entry->decl_context))
        {
            return 1;
        }
    }
    return 0;
}

char function_is_copy_constructor(scope_entry_t* entry, type_t* class_type)
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

        if (is_lvalue_reference_type(first_parameter)
                && equivalent_types_in_context(class_type,
                    get_unqualified_type(reference_type_get_referenced_type(first_parameter)),
                    entry->decl_context))
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

        if (is_rvalue_reference_type(first_parameter)
                && equivalent_types_in_context(class_type,
                    get_unqualified_type(reference_type_get_referenced_type(first_parameter)),
                    entry->decl_context))
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
        char is_expansion = 0;
        access_specifier_t access_specifier = AS_UNKNOWN;
        scope_entry_t* base_class = class_type_get_base_num(class_type, i, 
                &is_virtual, &is_dependent, &is_expansion, &access_specifier);

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
    entry->entity_specs.is_user_declared = 1;
    switch (ASTType(declarator_name))
    {
        case AST_SYMBOL :
            {
                if (is_constructor)
                {
                    // This is a constructor
                    entry->entity_specs.is_constructor = 1;

                    DEBUG_CODE()
                    {
                        fprintf(stderr, "BUILDSCOPE: Symbol '%s' at '%s' is a constructor\n", 
                                entry->symbol_name,
                                locus_to_str(entry->locus));
                    }

                    if (can_be_called_with_number_of_arguments(entry, 1))
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "BUILDSCOPE: Symbol '%s' at '%s' is a conversor constructor\n", 
                                    entry->symbol_name,
                                    locus_to_str(entry->locus));
                        }
                        entry->entity_specs.is_conversor_constructor = 1;
                    }

                    if (can_be_called_with_number_of_arguments(entry, 0))
                    {
                        entry->entity_specs.is_default_constructor = 1;
                        class_type_set_default_constructor(class_type, entry);
                    }

                    entry->entity_specs.is_copy_constructor =
                        function_is_copy_constructor(entry, class_type);

                    CXX11_LANGUAGE()
                    {
                        entry->entity_specs.is_move_constructor =
                            is_move_constructor(entry, class_type);
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
                class_type_set_destructor(get_actual_class_type(class_type), entry);
                break;
            }
        case AST_OPERATOR_FUNCTION_ID :
        case AST_OPERATOR_FUNCTION_ID_TEMPLATE :
            {
                entry->entity_specs.is_copy_assignment_operator =
                    function_is_copy_assignment_operator(entry, class_type);

                CXX11_LANGUAGE()
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

void hide_using_declarations(type_t* class_info, scope_entry_t* currently_declared)
{
    decl_context_t class_context = class_type_get_inner_context(class_info);

    scope_entry_list_t* member_functions = query_in_scope_str(class_context, currently_declared->symbol_name, NULL);

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
                    && function_type_same_parameter_types_and_cv_qualif(entry->type_information, 
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
static scope_entry_t* build_scope_member_function_definition(
        decl_context_t decl_context,
        AST function_definition,
        access_specifier_t current_access,
        type_t* class_info,
        char is_template,
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list)
{
    decl_context_t block_context;

    gather_decl_spec_t *gather_info = xcalloc(1, sizeof(*gather_info));
    gather_info->inside_class_specifier = 1;

    char is_constructor = 0;
    scope_entry_t* entry = build_scope_function_definition_declarator(
            function_definition,
            decl_context,
            is_template,
            is_explicit_specialization,
            nodecl_output,
            declared_symbols,
            gather_decl_spec_list,

            gather_info,
            &block_context,
            &is_constructor);

    if (entry == NULL)
        return NULL;

    AST function_header = ASTSon0(function_definition);
    AST function_declarator = ASTSon1(function_header);
    AST declarator_name = get_declarator_name(function_declarator, decl_context);

    // Propagate 'do_not_print' attribute to the current member
    entry->do_not_print = named_type_get_symbol(class_info)->do_not_print;

    entry->entity_specs.access = current_access;
    entry->entity_specs.is_defined_inside_class_specifier = 1;
    entry->entity_specs.is_inline = 1;
    entry->entity_specs.access = current_access;
    entry->entity_specs.class_type = class_info;

    if (gather_info->is_friend
            && is_template_specialized_type(entry->type_information)
            && !gather_info->is_template)
    {
        error_printf("%s: error: defining explicit specialization '%s' in friend declaration\n",
                ast_location(declarator_name),
                prettyprint_in_buffer(declarator_name));
        return NULL;
    }

    update_member_function_info(declarator_name, is_constructor, entry, class_info);

    DEBUG_CODE()
    {
        fprintf(stderr, "BUILDSCOPE: Setting member function definition at '%s' of '%s' as a member\n",
                ast_location(function_definition),
                entry->symbol_name);
    }

    // This function might be hiding using declarations, remove those
    hide_using_declarations(class_info, entry);

    if (gather_info->is_friend)
    {
        // If it is a friend function definition then we add entry symbol as a friend of the class
        class_type_add_friend_symbol(class_info, entry);
    }
    else
    {
        // Otherwise, we add this symbol as a member of the class
        class_type_add_member(get_actual_class_type(class_info), entry, /* is_definition */ 1);
    }

    build_scope_delayed_add_delayed_function_def(function_definition, entry, block_context, gather_info);

    return entry;
}

static void build_scope_default_or_delete_member_function_definition(
        decl_context_t decl_context,
        AST a,
        access_specifier_t current_access,
        type_t* class_info,
        char is_template,
        char is_explicit_specialization,
        nodecl_t* nodecl_output)
{
    CXX03_LANGUAGE()
    {
        warn_printf("%s: warning: default/delete functions are a C++11 feature\n",
                ast_location(a));
    }

    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));

    gather_info.inside_class_specifier = 1;
    gather_info.is_template = is_template;
    gather_info.is_explicit_specialization = is_explicit_specialization;

    const char* class_name = named_type_get_symbol(class_info)->symbol_name;

    AST function_header = ASTSon0(a);

    if (ASTType(function_header) == AST_AMBIGUITY)
    {
        solve_ambiguous_function_header(function_header, decl_context);
    }

    AST decl_spec_seq = ASTSon0(function_header);
    AST declarator = ASTSon1(function_header);

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
                new_decl_context);

    ERROR_CONDITION(entry == NULL, "Invalid entry computed", 0);

    entry->entity_specs.access = current_access;

    ERROR_CONDITION(entry->kind != SK_FUNCTION, "Invalid symbol for default/delete", 0);

    update_member_function_info(declarator_name, is_constructor, entry, class_info);

    switch (ASTType(a))
    {
        case AST_DEFAULTED_FUNCTION_DEFINITION :
            {
                set_defaulted_inside_class_specifier(entry, decl_context, ast_get_locus(a));
                break;
            }
        case AST_DELETED_FUNCTION_DEFINITION :
            {
                entry->entity_specs.is_defined_inside_class_specifier = 1;
                set_deleted(entry, decl_context, ast_get_locus(a));
                break;
            }
        default:
            {
                internal_error("Code unreachable", 0);
            }
    }

    // Copy gcc attributes
    keep_gcc_attributes_in_symbol(entry, &gather_info);
    keep_ms_declspecs_in_symbol(entry, &gather_info);

    // Propagate the __extension__ attribute to the symbol
    entry->entity_specs.gcc_extension = gcc_extension;

    // Add definition as a member
    class_type_add_member(get_actual_class_type(class_info), entry, /* is_definition */ 1);
}

void build_scope_friend_declarator(decl_context_t decl_context, 
        gather_decl_spec_t *gather_info,
        type_t* class_type,
        type_t* member_type, 
        AST declarator)
{
    // if (gather_info->is_template)
    // {
    //     warn_printf("%s: warning: friend template functions are not fully supported yet\n",
    //             ast_location(declarator));
    // }

    nodecl_t nodecl_output = nodecl_null();

    type_t* declarator_type = NULL;
    compute_declarator_type(declarator, gather_info, 
            member_type, &declarator_type, 
            decl_context, &nodecl_output);

    scope_entry_t *entry =
        build_scope_declarator_name(declarator,
                declarator_type, gather_info,
                decl_context);

    if (entry == NULL
            || (entry->kind != SK_FUNCTION
                && entry->kind != SK_DEPENDENT_FRIEND_FUNCTION
                && entry->kind != SK_DEPENDENT_ENTITY))
    {
        error_printf("%s: error: friend declaration '%s' does not name a function\n",
                ast_location(declarator),
                prettyprint_in_buffer(declarator));
        return;
    }

    if (entry->kind == SK_DEPENDENT_ENTITY)
    {
        // Fix the dependent entity here to be a dependent friend
        entry->kind = SK_DEPENDENT_FRIEND_FUNCTION;
        internal_error("Not yet implemented", 0);
    }
    class_type_add_friend_symbol(class_type, entry);
}

static void gather_single_virt_specifier(AST item,
        gather_decl_spec_t* gather_info,
        decl_context_t decl_context UNUSED_PARAMETER)
{
    switch (ASTType(item))
    {
        case AST_CLASS_VIRT_SPEC:
        case AST_MEMBER_VIRT_SPEC:
            {
                ERROR_CONDITION( (ASTText(item) == NULL), "Invalid node", 0);
                const char* spec = ASTText(item);

                if (IS_CXX03_LANGUAGE)
                {
                    warn_printf("%s: warning: virt-specifiers are a C+11 feature\n",
                            ast_location(item));
                }

                if (strcmp(spec, "final") == 0)
                {
                    gather_info->is_final = 1;
                }
                else if (strcmp(spec, "explicit") == 0)
                {
                    gather_info->is_explicit = 1;
                }
                else if (strcmp(spec, "new") == 0)
                {
                    gather_info->is_hides_member = 1;
                }
                else if (strcmp(spec, "override") == 0)
                {
                    gather_info->is_override = 1;
                }
                else
                {
                    internal_error("Unhandled valid keyword '%s' at %s\n", spec, ast_location(item));
                }

                break;
            }
        default:
            {
                internal_error("Invalid node '%s'\n", ast_print_node_type(ASTType(item)));
                break;
            }
    }
}

static void gather_virt_specifiers(AST a,
        gather_decl_spec_t* gather_info,
        decl_context_t decl_context UNUSED_PARAMETER)
{
    if (a == NULL)
        return;
    ERROR_CONDITION(ASTType(a) != AST_NODE_LIST, "Invalid node", 0);

    AST it;
    for_each_element(a, it)
    {
        AST item = ASTSon1(it);
        gather_single_virt_specifier(item, gather_info, decl_context);
    }
}

/*
 * This is a member declaration inlined in a class, not a function definition
 */
static void build_scope_member_simple_declaration(decl_context_t decl_context, AST a, 
        access_specifier_t current_access, type_t* class_info, 
        char is_template,
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t *gather_decl_spec_list)
{
    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));

    gather_info.is_template = is_template;
    gather_info.is_explicit_specialization = is_explicit_specialization;
    gather_info.current_access = current_access;
    gather_info.inside_class_specifier = 1;

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

    // This one is not converted to dependent typename type
    type_t* original_member_type = NULL;

    AST decl_spec_seq = ASTSon0(a);
    AST member_init_declarator_list = ASTSon1(a);

    AST type_specifier = NULL;

    if (decl_spec_seq != NULL)
    {
        type_specifier = ASTSon1(decl_spec_seq);
        decl_context_t new_decl_context = decl_context;
        if (member_init_declarator_list == NULL)
        {
            gather_info.no_declarators = 1;
        }

        build_scope_decl_specifier_seq(decl_spec_seq, &gather_info,
                &member_type, new_decl_context, nodecl_output);

        original_member_type = member_type;

        if (gather_info.defined_type != NULL
                && declared_symbols != NULL)
        {
            *declared_symbols = entry_list_add(*declared_symbols, gather_info.defined_type);
            P_LIST_ADD(gather_decl_spec_list->items, gather_decl_spec_list->num_items, gather_info);
        }

        CXX_LANGUAGE()
        {
            if (member_type != NULL
                    && is_named_type(member_type))
            {
                // Register the typename properly
                // FIXME Rewrite this using gather_info
                if (type_specifier != NULL
                        && !gather_info.is_friend
                        && (ASTType(type_specifier) == AST_CLASS_SPECIFIER // class A { } [x];
                            // class A; (no declarator)
                            || ((ASTType(type_specifier) == AST_ELABORATED_TYPE_CLASS_SPEC)
                                && (member_init_declarator_list == NULL))
                            // enum E { } [x];
                            || ASTType(type_specifier) == AST_ENUM_SPECIFIER
                            // enum E; (no declarator)
                            || ((ASTType(type_specifier) == AST_ELABORATED_TYPE_ENUM_SPEC)
                                && (member_init_declarator_list == NULL))))
                {
                    scope_entry_t* entry = named_type_get_symbol(member_type);

                    // Update the type specifier to be a dependent typename
                    if (is_dependent_type(class_type))
                    {
                        // This cannot be a template
                        nodecl_t nodecl_name = nodecl_make_cxx_dep_name_simple(entry->symbol_name, 
                                ast_get_locus(decl_spec_seq));

                        member_type = build_dependent_typename_for_entry(
                                named_type_get_symbol(class_info),
                                nodecl_name,
                                ast_get_locus(decl_spec_seq));
                    }
                }
            }
        }
    }

    if (member_init_declarator_list != NULL)
    {

        AST list = member_init_declarator_list;
        AST iter;

        for_each_element(list, iter)
        {
            AST declarator = ASTSon1(iter);
            char is_constructor = 0;

            gather_decl_spec_t current_gather_info;
            copy_gather_info(&current_gather_info, &gather_info);

            switch (ASTType(declarator))
            {
                case AST_AMBIGUITY:
                    {
                        solve_ambiguous_init_declarator(declarator, decl_context);
                        // Restart the function
                        build_scope_member_simple_declaration(decl_context, a, current_access, class_info, 
                                is_template, is_explicit_specialization,
                                nodecl_output, declared_symbols, gather_decl_spec_list);
                        return;
                        break;
                    }
                case AST_BITFIELD_DECLARATOR :
                    {
                        if (current_gather_info.is_friend)
                        {
                            error_printf("%s: error: a bit-field cannot be declared as friend\n",
                                    ast_location(declarator));
                            return;
                        }

                        AST attribute_list = ASTSon3(declarator);
                        gather_extra_attributes(attribute_list, &current_gather_info, decl_context);

                        AST identifier = ASTSon0(declarator);
                        type_t* declarator_type = member_type;

                        scope_entry_t* bitfield_symbol = NULL;
                        compute_declarator_type(identifier, &current_gather_info, 
                                member_type, &declarator_type,
                                decl_context, nodecl_output);

                        if (identifier != NULL)
                        {
                            bitfield_symbol = build_scope_declarator_name(identifier, declarator_type, &current_gather_info, 
                                    decl_context);
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
                        class_type_add_member(get_actual_class_type(class_type), bitfield_symbol, /* is_definition */ 1);

                        if (current_gather_info.is_static)
                        {
                            error_printf("%s: error: a bitfield declaration cannot be static\n",
                                    ast_location(declarator));
                            return;
                        }

                        AST expression = ASTSon1(declarator);
                        nodecl_t nodecl_bit_size = nodecl_null();
                        if (!check_expression(expression, decl_context, &nodecl_bit_size))
                        {
                            error_printf("%s: error: invalid bitfield size '%s'\n",
                                    ast_location(expression),
                                    prettyprint_in_buffer(expression));
                        }

                        if (!nodecl_is_constant(nodecl_bit_size))
                        {
                            error_printf("%s: error: bitfield size is not constant '%s'\n",
                                    ast_location(expression),
                                    prettyprint_in_buffer(expression));
                            nodecl_bit_size = const_value_to_nodecl(const_value_get_one( /* bytes */ 4, /* signed */ 1));
                        }

                        bitfield_symbol->entity_specs.is_bitfield = 1;
                        bitfield_symbol->entity_specs.bitfield_size = nodecl_bit_size;
                        bitfield_symbol->related_decl_context = decl_context;

                        bitfield_symbol->defined = 1;

                        if (declared_symbols != NULL)
                        {
                            *declared_symbols = entry_list_add(*declared_symbols, bitfield_symbol);
                            P_LIST_ADD(gather_decl_spec_list->items, gather_decl_spec_list->num_items, current_gather_info);
                        }

                        break;
                    }
                    // init declarator may appear here because of templates
                case AST_INIT_DECLARATOR :
                case AST_MEMBER_DECLARATOR :
                    {
                        AST attribute_list = ASTSon2(declarator);
                        gather_extra_attributes(attribute_list, &current_gather_info, decl_context);

                        // Friend declarations are so special
                        if (current_gather_info.is_friend)
                        {
                            build_scope_friend_declarator(decl_context, &current_gather_info, class_info, member_type, 
                                    ASTSon0(declarator));
                            break;
                        }

                        AST too_much_qualified_declarator_name = get_declarator_name(declarator, decl_context);

                        if (ASTType(too_much_qualified_declarator_name) == AST_QUALIFIED_ID)
                        {
                            error_printf("%s: error: extra qualification of member declaration is not allowed: '%s'. "
                                    "Did you mean '%s'?\n",
                                    ast_location(too_much_qualified_declarator_name),
                                    prettyprint_in_buffer(declarator),
                                    prettyprint_in_buffer(ASTSon2(too_much_qualified_declarator_name))
                                    );
                            return;
                        }

                        AST declarator_name = get_declarator_name(declarator, decl_context);
                        AST initializer = ASTSon1(declarator);

                        // Change name of constructors
                        if (member_type == NULL)
                        {
                            if (is_constructor_declarator(declarator))
                            {
                                if (ASTType(declarator_name) == AST_TEMPLATE_ID)
                                {
                                    scope_entry_list_t* entry_list = query_id_expression(decl_context, declarator_name, NULL);
                                    if (entry_list == NULL
                                            || entry_list_head(entry_list)->kind != SK_CLASS
                                            || (class_symbol_get_canonical_symbol(entry_list_head(entry_list))
                                                != decl_context.current_scope->related_entry))
                                    {
                                        error_printf("%s: error: invalid constructor declaration '%s'\n",
                                                ast_location(declarator_name),
                                                prettyprint_in_buffer(a));
                                        return;
                                    }
                                    // Clobber declarator_name with something sane
                                    //
                                    // A<int>() will become A()
                                    //
                                    // This is mean but constructors in this form are too unwieldy
                                    AST parent = ASTParent(declarator_name);
                                    int child_num = ast_num_of_given_child(parent, declarator_name);
                                    ast_replace(declarator_name, ASTSon0(declarator_name));
                                    ast_set_child(parent, child_num, declarator_name);
                                }

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

                        type_t* declarator_type = NULL;

                        compute_declarator_type(ASTSon0(declarator), &current_gather_info,
                                member_type, &declarator_type,
                                new_decl_context, nodecl_output);
                        scope_entry_t *entry =
                            build_scope_declarator_name(ASTSon0(declarator),
                                    declarator_type, &current_gather_info,
                                    new_decl_context);

                        if (entry == NULL)
                            continue;

                        DEBUG_CODE()
                        {
                            fprintf(stderr, "BUILDSCOPE: Setting symbol '%s' as a member of class '%s'\n",
                                    entry->symbol_name, class_name);
                        }

                        // Propagate 'do_not_print' attribute to the current member
                        entry->do_not_print = named_type_get_symbol(class_info)->do_not_print;

                        entry->entity_specs.is_member = 1;
                        entry->entity_specs.access = current_access;
                        entry->entity_specs.class_type = class_info;

                        // Copy some extra attributes
                        entry->entity_specs.is_override = current_gather_info.is_override;
                        entry->entity_specs.is_hides_member = current_gather_info.is_hides_member;
                        entry->entity_specs.is_final = current_gather_info.is_final;

                        if (entry->kind == SK_FUNCTION)
                        {
                            update_member_function_info(declarator_name, is_constructor, entry, class_info);

                            // This function might be hiding using declarations, remove those
                            hide_using_declarations(class_type, entry);

                            CXX11_LANGUAGE()
                            {
                                if (function_type_get_ref_qualifier(entry->type_information) != REF_QUALIFIER_NONE
                                        && entry->entity_specs.is_static)
                                {
                                    error_printf("%s: error: only nonstatic member functions may have ref-qualifier\n",
                                            ast_location(declarator_name));
                                }
                            }
                        }
                        else if (entry->kind == SK_VARIABLE)
                        {
                            if (!current_gather_info.is_static)
                            {
                                DEBUG_CODE()
                                {
                                    fprintf(stderr, "BUILDSCOPE: Registering a nonstatic data member '%s' of class '%s'\n",
                                            entry->symbol_name, class_name);
                                }
                                // This is a nonstatic data member
                                entry->defined = 1;
                            }
                            else
                            {
                                DEBUG_CODE()
                                {
                                    fprintf(stderr, "BUILDSCOPE: Registering a static data member '%s' of class '%s'\n",
                                            entry->symbol_name, class_name);
                                }
                                // This is a static data member
                                entry->defined = 0;
                            }
                        }
                        else if (entry->kind == SK_TYPEDEF
                                || entry->kind == SK_TEMPLATE)
                        {
                            // Do nothing
                        }
                        class_type_add_member(get_actual_class_type(class_type), entry, entry->defined);

                        if (!current_gather_info.is_static && current_gather_info.is_auto_type)
                        {
                            error_printf("%s: error: nonstatic member declared as auto\n",
                                    ast_location(declarator_name));
                        }

                        if (initializer != NULL)
                        {
                            if (entry->kind == SK_VARIABLE)
                            {
                                if (!current_gather_info.is_static
                                        && IS_CXX03_LANGUAGE)
                                {
                                    warn_printf("%s: warning: initialization of nonstatic data members is only valid in C++11\n",
                                            ast_location(initializer));
                                }

                                if (current_gather_info.is_static)
                                {
                                    nodecl_t nodecl_expr = nodecl_null();
                                    check_initialization(initializer,
                                            entry->decl_context,
                                            entry,
                                            get_unqualified_type(entry->type_information),
                                            &nodecl_expr,
                                            current_gather_info.is_auto_type);
                                    entry->value = nodecl_expr;
                                }
                                else
                                {
                                    // We need to delay them because this must work
                                    //
                                    // struct A
                                    // {
                                    //    size_t x = sizeof(A);
                                    // };
                                    build_scope_add_delayed_member_declarator_initializer(entry, initializer);
                                }
                                entry->entity_specs.is_defined_inside_class_specifier = 1;
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
                                    error_printf("%s: error: function declaration '%s' has an invalid initializer '%s'"
                                            " or has not been declared as a virtual function\n",
                                            ast_location(declarator),
                                            prettyprint_in_buffer(declarator),
                                            prettyprint_in_buffer(initializer));
                                    return;
                                }
                            }
                            else
                            {
                                error_printf("%s: error: no initializer allowed in current member declaration",
                                        ast_location(initializer));
                                return;
                            }
                        }
                        if (declared_symbols != NULL)
                        {
                            *declared_symbols = entry_list_add(*declared_symbols, entry);
                            P_LIST_ADD(gather_decl_spec_list->items, gather_decl_spec_list->num_items, current_gather_info);
                        }

                        keep_gcc_attributes_in_symbol(entry, &current_gather_info);
                        keep_ms_declspecs_in_symbol(entry, &current_gather_info);

                        // Propagate the __extension__ attribute to the symbol
                        entry->entity_specs.gcc_extension = gcc_extension;

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
    else
    {
        if (is_named_type(original_member_type)
                && is_class_type(original_member_type)
                && named_type_get_symbol(original_member_type)->entity_specs.is_anonymous_union)
        {
            scope_entry_t* named_type = named_type_get_symbol(original_member_type);

            // Anonymous unions are members even in C
            C_LANGUAGE()
            {
                named_type->entity_specs.is_member = 1;
                named_type->entity_specs.access = current_access;
                named_type->entity_specs.is_defined_inside_class_specifier = 1;
                named_type->entity_specs.class_type = class_info;
            }

            scope_entry_t* new_member = finish_anonymous_class(named_type, decl_context);
            new_member->type_information = original_member_type;

            // Add this member to the current class
            new_member->entity_specs.is_member = 1;
            new_member->entity_specs.access = current_access;
            new_member->entity_specs.class_type = class_info;

            class_type_add_member(class_type, new_member, /* is_definition */ 1);
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
static void build_dynamic_exception_spec(type_t* function_type UNUSED_PARAMETER, 
        AST a, gather_decl_spec_t *gather_info, 
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
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

        // We allow variadic typeid's
        inner_gather_info.parameter_declaration = 1;

        build_scope_decl_specifier_seq(type_specifier_seq, &inner_gather_info, &type_info,
                decl_context, nodecl_output);

        if (is_error_type(type_info))
            continue;

        type_t* declarator_type = type_info;
        compute_declarator_type(abstract_decl, &inner_gather_info, type_info, &declarator_type,
                decl_context, nodecl_output);

        if (is_error_type(declarator_type))
            continue;

        if (inner_gather_info.is_template_pack)
        {
            declarator_type = get_pack_type(declarator_type);
        }

        P_LIST_ADD_ONCE(gather_info->exceptions, gather_info->num_exceptions, declarator_type);
    }
}

static void build_noexcept_spec(type_t* function_type UNUSED_PARAMETER, 
        AST a, decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    AST const_expr = ASTSon0(a);

    if (const_expr == NULL)
    {
        nodecl_t true_expr = nodecl_make_boolean_literal(
                get_bool_type(),
                const_value_get_one(type_get_size(get_bool_type()), 0),
                ast_get_locus(a));
        *nodecl_output = true_expr;
    }
    else
    {
        check_expression(const_expr, decl_context, nodecl_output);

        if (!nodecl_is_err_expr(*nodecl_output))
        {
            if (!nodecl_is_constant(*nodecl_output)
                    && !nodecl_expr_is_value_dependent(*nodecl_output)
                    && !nodecl_expr_is_type_dependent(*nodecl_output))
            {
                error_printf("%s: error: noexcept must specify a constant expression\n",
                        nodecl_locus_to_str(*nodecl_output));
            }
            if (!nodecl_expr_is_type_dependent(*nodecl_output))
            {
                scope_entry_t* conversor = NULL;
                char ambiguous_conversion = 0;
                if (!type_can_be_implicitly_converted_to(nodecl_get_type(*nodecl_output),
                            get_bool_type(), decl_context, 
                            &ambiguous_conversion,
                            &conversor,
                            nodecl_get_locus(*nodecl_output))
                        || ambiguous_conversion)
                {
                    error_printf("%s: error: noexcept expression must be convertible to bool\n",
                            nodecl_locus_to_str(*nodecl_output));
                }
            }
        }
    }
}

static void build_exception_spec(type_t* function_type UNUSED_PARAMETER, 
        AST a, gather_decl_spec_t *gather_info, 
        decl_context_t decl_context,
        decl_context_t prototype_context,
        nodecl_t* nodecl_output)
{
    // No exception specifier at all
    if (a == NULL)
    {
        gather_info->any_exception = 1;
        return;
    }

    if (ASTType(a) == AST_EXCEPTION_SPECIFICATION)
    {
        build_dynamic_exception_spec(function_type, a, gather_info, decl_context, nodecl_output);
    }
    else if (ASTType(a) == AST_NOEXCEPT_SPECIFICATION)
    {
        if (gather_info->inside_class_specifier)
        {
            // Parse noexcept(E) later
            AST parent = ast_get_parent(a);

            gather_info->noexception = nodecl_make_cxx_parse_later(ast_get_locus(a));
            nodecl_set_child(gather_info->noexception, 0, _nodecl_wrap(a));

            // Restore parent modified by nodecl_set_child
            ast_set_parent(a, parent);
        }
        else
        {
            build_noexcept_spec(function_type, a, prototype_context, &gather_info->noexception);
        }
    }
    else
    {
        internal_error("Unexpected tree '%s'\n", ast_print_node_type(ASTType(a)));
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

#define RETURN_UNIQUESTR_NAME(x) \
    { \
        static const char* c = NULL; \
        if (c != NULL) return c; \
        return (c = uniquestr(x)); \
    } 

    switch (ASTType(operator))
    {
        case AST_NEW_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_NEW);
        case AST_DELETE_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_DELETE);
        case AST_NEW_ARRAY_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_NEW_ARRAY);
        case AST_DELETE_ARRAY_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_DELETE_ARRAY);
        case AST_ADD_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_ADD);
        case AST_MINUS_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_MINUS);
        case AST_MUL_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_MULT);
        case AST_DIV_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_DIV);
        case AST_MOD_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_MOD);
        case AST_BITWISE_XOR_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_BIT_XOR);
        case AST_BITWISE_AND_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_BIT_AND);
        case AST_BITWISE_OR_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_BIT_OR);
        case AST_BITWISE_NEG_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_BIT_NOT);
        case AST_LOGICAL_NOT_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_LOGIC_NOT);
        case AST_ASSIGNMENT_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_ASSIGNMENT);
        case AST_LOWER_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_LOWER_THAN);
        case AST_GREATER_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_GREATER_THAN);
        case AST_ADD_ASSIGN_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_ADD_ASSIGNMENT);
        case AST_SUB_ASSIGN_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_MINUS_ASSIGNMENT);
        case AST_MUL_ASSIGN_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_MUL_ASSIGNMENT);
        case AST_DIV_ASSIGN_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_DIV_ASSIGNMENT);
        case AST_MOD_ASSIGN_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_MOD_ASSIGNMENT);
        case AST_BITWISE_XOR_ASSIGN_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_XOR_ASSIGNMENT);
        case AST_BITWISE_AND_ASSIGN_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_AND_ASSIGNMENT);
        case AST_BITWISE_OR_ASSIGN_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_OR_ASSIGNMENT);
        case AST_LEFT_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_SHIFT_LEFT);
        case AST_RIGHT_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_SHIFT_RIGHT);
        case AST_LEFT_ASSIGN_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_SHL_ASSIGNMENT);
        case AST_RIGHT_ASSIGN_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_SHR_ASSIGNMENT);
        case AST_EQUAL_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_EQUAL);
        case AST_DIFFERENT_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_DIFFERENT);
        case AST_LESS_OR_EQUAL_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_LOWER_EQUAL);
        case AST_GREATER_OR_EQUAL_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_GREATER_EQUAL);
        case AST_LOGICAL_AND_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_LOGIC_AND);
        case AST_LOGICAL_OR_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_LOGIC_OR);
        case AST_INCREMENT_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_POSTINCREMENT);
        case AST_DECREMENT_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_POSTDECREMENT);
        case AST_COMMA_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_COMMA);
        case AST_POINTER_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_ARROW);
        case AST_POINTER_DERREF_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_ARROW_POINTER);
        case AST_FUNCTION_CALL_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_CALL);
        case AST_SUBSCRIPT_OPERATOR :
            RETURN_UNIQUESTR_NAME(STR_OPERATOR_SUBSCRIPT);
        default :
            internal_error("Invalid node type '%s'\n", ast_print_node_type(ASTType(declarator_id)));
    }

#undef RETURN_UNIQUESTR_NAME
}


typedef
struct call_to_destructor_data_tag
{
    nodecl_t* nodecl_output;
    scope_t* scope;
    const locus_t* locus;
    decl_context_t decl_context;
} call_to_destructor_data_t;

static void call_to_destructor(scope_entry_list_t* entry_list, void *data)
{
    call_to_destructor_data_t* destructor_data = (call_to_destructor_data_t*)data;

    scope_entry_t* entry = entry_list_head(entry_list);

    if (entry->kind == SK_VARIABLE
            && is_class_type(entry->type_information)
            && is_complete_type(entry->type_information)
            && !is_dependent_type(entry->type_information)
            && !entry->entity_specs.is_static
            && !entry->entity_specs.is_extern)
    {
        instantiate_template_class_if_needed(named_type_get_symbol(entry->type_information), 
                entry->decl_context, destructor_data->locus);

        nodecl_t sym_ref = nodecl_make_symbol(entry, make_locus("", 0, 0));
        type_t* t = entry->type_information;
        if (!is_any_reference_type(t))
            t = get_lvalue_reference_type(t);
        nodecl_set_type(sym_ref, t);

        nodecl_t nodecl_call_to_destructor = 
            nodecl_make_expression_statement(
                    cxx_nodecl_make_function_call(
                        nodecl_make_symbol(class_type_get_destructor(entry->type_information), make_locus("", 0, 0)),
                        /* called name */ nodecl_null(),
                        nodecl_make_list_1(sym_ref),
                        /* function_form */ nodecl_null(),
                        get_void_type(),
                        destructor_data->decl_context,
                        make_locus("", 0, 0)),
                    make_locus("", 0, 0));

        *(destructor_data->nodecl_output) = nodecl_append_to_list(
                *(destructor_data->nodecl_output), 
                nodecl_call_to_destructor);
    }

    entry_list_free(entry_list);
}

static void call_destructors_of_classes(decl_context_t block_context, 
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    call_to_destructor_data_t call_to_destructor_data = { 
        .nodecl_output = nodecl_output,
        .scope = block_context.current_scope,
        .decl_context = block_context,
        .locus = locus,
    };

    scope_for_each_entity(block_context.current_scope, &call_to_destructor_data, call_to_destructor);
}

/*
 * Building scope for statements
 */

typedef void (*stmt_scope_handler_t)(AST a, decl_context_t decl_context, nodecl_t* nodecl_output);
typedef 
struct stmt_scope_handler_map_tag
{
    stmt_scope_handler_t handler;
} stmt_scope_handler_map_t;


static void build_scope_compound_statement(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    decl_context_t block_context = new_block_context(decl_context);

    nodecl_t nodecl_output_list = nodecl_null();

    AST list = ASTSon0(a);
    if (list != NULL)
    {

        nodecl_t current_nodecl_output = nodecl_null();
        build_scope_statement_seq(list, block_context, &current_nodecl_output);

        nodecl_output_list = nodecl_concat_lists(nodecl_output_list, current_nodecl_output);
    }


    nodecl_t nodecl_destructors = nodecl_null();
    CXX_LANGUAGE()
    {
        call_destructors_of_classes(block_context, ast_get_locus(a), &nodecl_destructors);
    }

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_context(
                nodecl_make_list_1(
                    nodecl_make_compound_statement(nodecl_output_list, nodecl_destructors, ast_get_locus(a))
                    ),
                block_context, ast_get_locus(a)));
}

static void build_scope_implicit_compound_statement(AST list, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    nodecl_t nodecl_output_list = nodecl_null();

    if (list != NULL)
    {
        nodecl_t current_nodecl_output = nodecl_null();
        build_scope_statement_seq(list, decl_context, &current_nodecl_output);

        nodecl_output_list = nodecl_concat_lists(nodecl_output_list, current_nodecl_output);
    }

   *nodecl_output = nodecl_output_list;
}

static void build_scope_condition(AST a, decl_context_t decl_context, nodecl_t* nodecl_output)
{

    if (ASTType(a) == AST_AMBIGUITY)
    {
        solve_ambiguous_condition(a, decl_context);
    }

    if (ASTSon0(a) != NULL
            && ASTSon1(a) != NULL)
    {
        // This condition declares something in this scope
        AST type_specifier_seq = ASTSon0(a);
        AST declarator = ASTSon1(a);

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
        scope_entry_t* entry = build_scope_declarator_name(declarator, declarator_type, &gather_info, decl_context);

        // FIXME: Handle VLAs here
        ERROR_CONDITION(gather_info.num_vla_dimension_symbols > 0, "Unsupported VLAs at the declaration", 0);

        AST initializer = ASTSon2(a);

        nodecl_t nodecl_expr = nodecl_null();
        if (!check_initialization(initializer,
                    decl_context,
                    entry,
                    get_unqualified_type(entry->type_information),
                    &nodecl_expr,
                    gather_info.is_auto_type))
        {
            *nodecl_output = nodecl_expr;
            return;
        }

        // FIXME: Handle VLAs here
        ERROR_CONDITION (pop_extra_declaration_symbol() != NULL,
                "Unsupported extra declarations at the initialization expression", 0);

        entry->value = nodecl_expr;

        C_LANGUAGE()
        {
            standard_conversion_t dummy;
            if (!standard_conversion_between_types(&dummy, entry->type_information, get_bool_type(), ast_get_locus(a)))
            {
                error_printf("%s: error: value of type '%s' where a scalar was expected\n",
                        ast_location(a),
                        print_type_str(entry->type_information, decl_context));
                *nodecl_output = nodecl_make_err_expr(ast_get_locus(a));
                return;
            }

            *nodecl_output = nodecl_make_object_init(entry, ast_get_locus(initializer));
        }
        CXX_LANGUAGE()
        {
            if (!nodecl_expr_is_type_dependent(nodecl_expr))
            {
                char ambiguous_conversion = 0;
                scope_entry_t* conversor = NULL;
                if (!type_can_be_implicitly_converted_to(entry->type_information, get_bool_type(), decl_context, 
                            &ambiguous_conversion, &conversor, ast_get_locus(initializer))
                        || ambiguous_conversion)
                {
                    error_printf("%s: error: value of type '%s' cannot be converted to 'bool' type\n",
                            ast_location(a),
                            print_type_str(entry->type_information, decl_context));
                    *nodecl_output = nodecl_make_err_expr(ast_get_locus(a));
                    return;
                }

                *nodecl_output = nodecl_make_object_init(entry, ast_get_locus(initializer));
                if (conversor != NULL)
                {
                    ERROR_CONDITION(!conversor->entity_specs.is_conversion,
                            "I expected a conversion function!", 0);
                    *nodecl_output = cxx_nodecl_make_function_call(
                            nodecl_make_symbol(conversor, ast_get_locus(initializer)),
                            /* called name */ nodecl_null(),
                            nodecl_make_list_1(*nodecl_output),
                            /* function_form */ nodecl_make_cxx_function_form_implicit(ast_get_locus(initializer)),
                            function_type_get_return_type(conversor->type_information),
                            decl_context,
                            ast_get_locus(initializer));
                }
            }
            else
            {
                *nodecl_output = nodecl_make_object_init(entry, ast_get_locus(initializer));
            }
        }

        keep_gcc_attributes_in_symbol(entry, &gather_info);
        keep_ms_declspecs_in_symbol(entry, &gather_info);
    }
    else
    {
        check_expression(ASTSon2(a), decl_context, nodecl_output);

        // FIXME: Handle VLAs here
        ERROR_CONDITION (pop_extra_declaration_symbol() != NULL,
                "Unsupported extra declarations at the initialization expression", 0);
    }
}

static void build_scope_normalized_statement(AST a, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    if (ASTType(a) == AST_COMPOUND_STATEMENT)
        build_scope_statement(a, decl_context, nodecl_output);
    else
    {
        // Mimick the behaviour of a compound statement
        decl_context_t block_context = new_block_context(decl_context);

        nodecl_t nodecl_output_list = nodecl_null();

        nodecl_t current_nodecl_output = nodecl_null();
        build_scope_statement(a, block_context, &current_nodecl_output);

        nodecl_output_list = nodecl_concat_lists(nodecl_output_list, current_nodecl_output);

        nodecl_t nodecl_destructors = nodecl_null();
        CXX_LANGUAGE()
        {
            call_destructors_of_classes(block_context, ast_get_locus(a), &nodecl_destructors);
        }

        *nodecl_output = nodecl_make_list_1(
                nodecl_make_context(
                    nodecl_make_list_1(
                        nodecl_make_compound_statement(nodecl_output_list, nodecl_destructors, ast_get_locus(a))
                        ),
                    block_context, ast_get_locus(a)));
    }
}

static void build_scope_while_statement(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    decl_context_t block_context = new_block_context(decl_context);

    nodecl_t nodecl_condition = nodecl_null();
    build_scope_condition(ASTSon0(a), block_context, &nodecl_condition);

    nodecl_t nodecl_statement = nodecl_null();
    if (ASTSon1(a) != NULL)
    {
        build_scope_normalized_statement(ASTSon1(a), block_context, &nodecl_statement);
    }

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_context(
                nodecl_make_list_1(
                    nodecl_make_while_statement(nodecl_condition, nodecl_statement, 
                        /* loop_name */ nodecl_null(),
                        ast_get_locus(a))),
                block_context,
                ast_get_locus(a)));
}

static void build_scope_ambiguity_handler(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    solve_ambiguous_statement(a, decl_context);
    // Restart
    build_scope_statement(a, decl_context, nodecl_output);
}

static void build_scope_declaration_statement(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    AST declaration = ASTSon0(a);

    build_scope_declaration(declaration, decl_context, nodecl_output, 
            /* declared_symbols */ NULL, /* gather_decl_spec_t */ NULL);
}


static void build_scope_expression_statement(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    AST expr = ASTSon0(a);
    nodecl_t nodecl_expr = nodecl_null();
    if (!check_expression(expr, decl_context, &nodecl_expr))
    {
        if (CURRENT_CONFIGURATION->strict_typecheck)
        {
            internal_error("Could not check expression '%s' at '%s'\n",
                    prettyprint_in_buffer(ASTSon0(a)),
                    ast_location(ASTSon0(a)));
        }

        *nodecl_output = nodecl_append_to_list(
                *nodecl_output,
                nodecl_make_err_statement(ast_get_locus(a)));
        return;
    }

    if (nodecl_get_type(nodecl_expr) != NULL)
    {
        if (is_unresolved_overloaded_type(nodecl_get_type(nodecl_expr)))
        {
            const char* message = NULL;
            uniquestr_sprintf(&message,
                    "%s: error: invalid unresolved overloaded expression '%s'\n", 
                    ast_location(expr),
                    prettyprint_in_buffer(expr));
            scope_entry_list_t* candidates = unresolved_overloaded_type_get_overload_set(nodecl_get_type(nodecl_expr));

            diagnostic_candidates(candidates, &message, ast_get_locus(expr));
            error_printf("%s", message);

            return;
        }
    }

    *nodecl_output = flush_extra_declared_symbols(ast_get_locus(expr));

    *nodecl_output = nodecl_append_to_list(
            *nodecl_output,
            nodecl_make_expression_statement(nodecl_expr, ast_get_locus(expr)));
}

static void build_scope_if_else_statement(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    decl_context_t block_context = new_block_context(decl_context);

    AST condition = ASTSon0(a);
    nodecl_t nodecl_condition = nodecl_null();
    build_scope_condition(condition, block_context, &nodecl_condition);

    AST then_branch = ASTSon1(a);
    nodecl_t nodecl_then = nodecl_null();
    build_scope_normalized_statement(then_branch, block_context, &nodecl_then);

    // Normalize multiple statements

    nodecl_t nodecl_else = nodecl_null();
    AST else_branch = ASTSon2(a);
    if (else_branch != NULL)
    {
        build_scope_normalized_statement(else_branch, block_context, &nodecl_else);
    }

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_context(
                nodecl_make_list_1(
                    nodecl_make_if_else_statement(nodecl_condition, nodecl_then, nodecl_else, 
                        ast_get_locus(a))),
                block_context,
                ast_get_locus(a)));
}

static void solve_literal_symbol_scope(AST a, decl_context_t decl_context UNUSED_PARAMETER,
        nodecl_t* nodecl_output)
{
    ERROR_CONDITION(ASTType(a) != AST_SYMBOL_LITERAL_REF, "Invalid node", 0);

    const char *tmp = ASTText(ASTSon0(a));

    const char * prefix = NULL;
    void *p = NULL;
    unpack_pointer(tmp, &prefix, &p);

    ERROR_CONDITION(prefix == NULL || p == NULL || strcmp(prefix, "symbol") != 0,
            "Failure during unpack of symbol", 0);

    scope_entry_t* entry = (scope_entry_t*)p;

    *nodecl_output = nodecl_make_symbol(entry, ast_get_locus(a));
}

static void build_scope_for_statement_nonrange(AST a,
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    AST loop_control = ASTSon0(a);

    AST for_init_statement = ASTSon0(loop_control);
    AST condition = ASTSon1(loop_control);
    AST expression = ASTSon2(loop_control);

    AST statement = ASTSon1(a);

    // AST end_loop_statement = ASTSon2(a); // Fortran only
    AST synthesized_loop_name = ASTSon3(a); // Mercurium internal parsing

    if (ASTType(for_init_statement) == AST_AMBIGUITY)
    {
        solve_ambiguous_for_init_statement(for_init_statement, decl_context);
    }

    decl_context_t block_context = new_block_context(decl_context);

    nodecl_t nodecl_loop_init = nodecl_null();
    if (ASTType(for_init_statement) == AST_SIMPLE_DECLARATION)
    {
        nodecl_t nodecl_dummy = nodecl_null();
        scope_entry_list_t* declared_symbols = NULL;

        gather_decl_spec_list_t gather_decl_spec_list;
        memset(&gather_decl_spec_list, 0, sizeof(gather_decl_spec_list));

        build_scope_simple_declaration(for_init_statement, block_context,
                /* is_template */ 0,
                /* is_explicit_specialization */ 0,
                &nodecl_dummy,
                &declared_symbols, &gather_decl_spec_list);
        nodecl_free(nodecl_dummy);

        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(declared_symbols);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* entry = entry_list_iterator_current(it);
            nodecl_loop_init
                = nodecl_append_to_list(
                        nodecl_loop_init,
                        nodecl_make_object_init(entry, entry->locus));
        }
        entry_list_iterator_free(it);

        xfree(gather_decl_spec_list.items);
    }
    else if (ASTType(for_init_statement) == AST_EXPRESSION_STATEMENT)
    {
        build_scope_expression_statement(for_init_statement, block_context, &nodecl_loop_init);
        // This tree contains VLA object inits
        ERROR_CONDITION(!nodecl_is_null(nodecl_loop_init)
                && (nodecl_list_length(nodecl_loop_init) > 1), "Unsupported VLAs at this expression statement", 0);

        nodecl_loop_init = nodecl_list_head(nodecl_loop_init);
        // Get the expression itself instead of an expression statement
        nodecl_loop_init = nodecl_make_list_1(nodecl_get_child(nodecl_loop_init, 0));
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

    nodecl_t nodecl_loop_condition = nodecl_null();
    if (condition != NULL)
    {
        build_scope_condition(condition, block_context, &nodecl_loop_condition);
    }

    nodecl_t nodecl_loop_iter = nodecl_null();
    if (expression != NULL)
    {
        check_expression(expression, block_context, &nodecl_loop_iter);
    }


    nodecl_t nodecl_statement = nodecl_null();
    build_scope_normalized_statement(statement, block_context, &nodecl_statement);

    nodecl_t loop_name = nodecl_null();
    if (synthesized_loop_name != NULL)
    {
        solve_literal_symbol_scope(synthesized_loop_name, decl_context, &loop_name);
    }

    nodecl_t nodecl_loop_control = nodecl_make_loop_control(nodecl_loop_init, nodecl_loop_condition, nodecl_loop_iter,
            ast_get_locus(a));
    *nodecl_output =
        nodecl_make_list_1(
                nodecl_make_context(
                    nodecl_make_list_1(
                        nodecl_make_for_statement(nodecl_loop_control, nodecl_statement, 
                            loop_name,
                            ast_get_locus(a))),
                    block_context,
                    ast_get_locus(a)
                    ));
}

static void build_scope_for_statement_range(AST a,
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    AST loop_control = ASTSon0(a);
    AST statement = ASTSon1(a);

    ast_set_child(a, 0, NULL);
    ast_set_child(a, 1, NULL);

    CXX03_LANGUAGE()
    {
        warn_printf("%s: warning: range-based for is a C++11 feature\n", ast_location(a));
    }

    decl_context_t block_context = new_block_context(decl_context);

    AST for_range_decl = ASTSon0(loop_control);
    AST expr_or_init_braced = ASTSon1(loop_control);

    AST type_specifier = ASTSon0(for_range_decl);
    AST declarator = ASTSon1(for_range_decl);

    type_t* type_info = NULL;
    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));

    build_scope_decl_specifier_seq(type_specifier, &gather_info, &type_info,
            block_context, nodecl_output);

    if (is_error_type(type_info))
    {
        *nodecl_output = nodecl_make_list_1(
                nodecl_make_err_statement(ast_get_locus(a))
                );
        return;
    }

    type_t* declarator_type = NULL;
    compute_declarator_type(declarator, &gather_info, type_info, &declarator_type,
            block_context, nodecl_output);

    if (is_error_type(declarator_type))
    {
        *nodecl_output = nodecl_make_list_1(
                nodecl_make_err_statement(ast_get_locus(a))
                );
        return;
    }

    scope_entry_t* iterator_symbol = build_scope_declarator_name(declarator, declarator_type, &gather_info, block_context);

    ERROR_CONDITION(gather_info.num_vla_dimension_symbols > 0, "Unsupported VLAs at the declaration", 0);

    // Create __range
    scope_entry_t* range_symbol = new_symbol(block_context, block_context.current_scope, ".__range");
    range_symbol->symbol_name = UNIQUESTR_LITERAL("__range");
    range_symbol->kind = SK_VARIABLE;
    range_symbol->type_information = get_rvalue_reference_type(get_auto_type());
    range_symbol->locus = ast_get_locus(a);

    // Wrap this inside an equal initializer to verify the initialization
    expr_or_init_braced = ASTMake1(AST_EQUAL_INITIALIZER,
            expr_or_init_braced,
            ast_get_locus(expr_or_init_braced), NULL);

    nodecl_t nodecl_range_initializer = nodecl_null();
    if (!check_initialization(expr_or_init_braced,
            block_context,
            range_symbol,
            range_symbol->type_information,
            &nodecl_range_initializer,
            /* is_auto_type */ 1))
    {
        *nodecl_output = nodecl_make_list_1(
                nodecl_make_err_statement(ast_get_locus(a))
                );
        return;
    }

    nodecl_t nodecl_initializer_tmp = nodecl_null();;

    // Craft begin_expr and end_expr
    nodecl_t nodecl_begin_init = nodecl_null(),
             nodecl_end_init = nodecl_null();

    if (!is_dependent_type(range_symbol->type_information))
    {
        if (is_array_type(no_ref(range_symbol->type_information)))
        {
            nodecl_t nodecl_begin_symbol = nodecl_make_symbol(range_symbol, ast_get_locus(a));
            nodecl_set_type(nodecl_begin_symbol, lvalue_ref(range_symbol->type_information));

            nodecl_begin_init = nodecl_make_cxx_equal_initializer(
                    nodecl_make_cxx_initializer(
                        nodecl_begin_symbol,
                        nodecl_get_type(nodecl_begin_symbol),
                        ast_get_locus(a)),
                    nodecl_get_type(nodecl_begin_symbol),
                    ast_get_locus(a));

            nodecl_t nodecl_array_size = nodecl_shallow_copy(
                    array_type_get_array_size_expr(no_ref(range_symbol->type_information))
                    );

            type_t* pointer_type =
                get_pointer_type(
                        array_type_get_element_type(no_ref(range_symbol->type_information))
                        );

            nodecl_end_init =
                nodecl_make_cxx_equal_initializer(
                        nodecl_make_cxx_initializer(
                            nodecl_make_add(
                                nodecl_make_conversion(
                                    nodecl_shallow_copy(nodecl_begin_symbol),
                                    pointer_type,
                                    ast_get_locus(a)
                                    ),
                                nodecl_array_size,
                                pointer_type,
                                ast_get_locus(a)),
                            pointer_type,
                            ast_get_locus(a)),
                        pointer_type,
                        ast_get_locus(a));
        }
        else
        {
            // For the purpose of this lookup, std is an associated namespace
            decl_context_t global_context = decl_context;
            global_context.current_scope = global_context.global_scope;
            scope_entry_list_t* entry_list = query_in_scope_str(global_context, UNIQUESTR_LITERAL("std"), NULL);


            scope_entry_t* std_namespace = NULL;
            if (entry_list != NULL)
            {
                std_namespace = entry_list_head(entry_list);
                entry_list_free(entry_list);
            }

            char must_remove_std = (std_namespace != NULL);
            if (must_remove_std)
            {
                int i;
                for (i = 0;
                        i < block_context.current_scope->num_used_namespaces;
                        i++)
                {
                    if (block_context.current_scope->use_namespace[i] == std_namespace)
                    {
                        must_remove_std = 0;
                        break;
                    }
                }

            }

            AST begin_init_tree = ASTMake2(AST_FUNCTION_CALL,
                    ASTLeaf(AST_SYMBOL, ast_get_locus(a), UNIQUESTR_LITERAL("begin")),
                    ASTListLeaf(
                        ASTLeaf(AST_SYMBOL, ast_get_locus(a), UNIQUESTR_LITERAL(".__range"))
                        ),
                    ast_get_locus(a),
                    NULL);

            check_expression(begin_init_tree, block_context, &nodecl_begin_init);

            if (nodecl_is_err_expr(nodecl_begin_init))
            {
                *nodecl_output = nodecl_make_list_1(
                        nodecl_make_err_statement(ast_get_locus(a))
                        );
                return;
            }

            nodecl_begin_init = nodecl_make_cxx_equal_initializer(
                    nodecl_make_cxx_initializer(
                        nodecl_begin_init,
                        nodecl_get_type(nodecl_begin_init),
                        ast_get_locus(a)),
                    nodecl_get_type(nodecl_begin_init),
                    ast_get_locus(a));

            AST end_init_tree = ASTMake2(AST_FUNCTION_CALL,
                    ASTLeaf(AST_SYMBOL, ast_get_locus(a), UNIQUESTR_LITERAL("end")),
                    ASTListLeaf(
                        ASTLeaf(AST_SYMBOL, ast_get_locus(a), UNIQUESTR_LITERAL(".__range"))
                        ),
                    ast_get_locus(a),
                    NULL);

            check_expression(end_init_tree, block_context, &nodecl_end_init);

            if (nodecl_is_err_expr(nodecl_end_init))
            {
                *nodecl_output = nodecl_make_list_1(
                        nodecl_make_err_statement(ast_get_locus(a))
                        );
                return;
            }

            nodecl_end_init = nodecl_make_cxx_equal_initializer(
                    nodecl_make_cxx_initializer(
                        nodecl_end_init,
                        nodecl_get_type(nodecl_end_init),
                        ast_get_locus(a)),
                    nodecl_get_type(nodecl_end_init),
                    ast_get_locus(a));

            if (must_remove_std)
            {
                P_LIST_REMOVE(block_context.current_scope->use_namespace,
                        block_context.current_scope->num_used_namespaces,
                        std_namespace);
            }
        }

        // Create __begin and __end
        scope_entry_t* begin_symbol = new_symbol(block_context, block_context.current_scope, ".__begin");
        begin_symbol->symbol_name = UNIQUESTR_LITERAL("__begin");
        begin_symbol->kind = SK_VARIABLE;
        begin_symbol->type_information = get_auto_type();
        begin_symbol->locus = ast_get_locus(a);

        nodecl_initializer_tmp = nodecl_null();
        check_nodecl_initialization(
                    nodecl_begin_init,
                    block_context,
                    begin_symbol,
                    begin_symbol->type_information,
                    &nodecl_initializer_tmp,
                    /* is_auto_type */ 1);

        if (nodecl_is_err_expr(nodecl_initializer_tmp))
        {
            *nodecl_output = nodecl_make_list_1(
                    nodecl_make_err_statement(ast_get_locus(a))
                    );
            return;
        }

        scope_entry_t* end_symbol = new_symbol(block_context, block_context.current_scope, ".__end");
        end_symbol->symbol_name = UNIQUESTR_LITERAL("__end");
        end_symbol->kind = SK_VARIABLE;
        end_symbol->type_information = get_auto_type();
        end_symbol->locus = ast_get_locus(a);

        nodecl_initializer_tmp = nodecl_null();
        check_nodecl_initialization(nodecl_end_init,
                    block_context,
                    end_symbol,
                    end_symbol->type_information,
                    &nodecl_initializer_tmp,
                    /* is_auto_type */ 1);

        if (nodecl_is_err_expr(nodecl_initializer_tmp))
        {
            *nodecl_output = nodecl_make_list_1(
                    nodecl_make_err_statement(ast_get_locus(a))
                    );
            return;
        }

        AST initialize_iterator =
            ASTMake1(AST_EQUAL_INITIALIZER,
                    ASTMake1(AST_DERREFERENCE,
                        ASTLeaf(AST_SYMBOL, ast_get_locus(a), UNIQUESTR_LITERAL(".__begin")),
                        ast_get_locus(a), NULL),
                    ast_get_locus(a), NULL);

        nodecl_initializer_tmp = nodecl_null();
        check_initialization(initialize_iterator,
                block_context,
                iterator_symbol,
                iterator_symbol->type_information,
                &nodecl_initializer_tmp,
                gather_info.is_auto_type);

        if (nodecl_is_err_expr(nodecl_initializer_tmp))
        {
            *nodecl_output = nodecl_make_list_1(
                    nodecl_make_err_statement(ast_get_locus(a))
                    );
            return;
        }
    }


    nodecl_t nodecl_loop_control = nodecl_make_iterator_loop_control(
            nodecl_make_symbol(iterator_symbol, ast_get_locus(a)),
            nodecl_range_initializer,
            ast_get_locus(a));

    nodecl_t nodecl_statement = nodecl_null();
    build_scope_normalized_statement(statement, block_context, &nodecl_statement);

    *nodecl_output =
        nodecl_make_list_1(
                nodecl_make_context(
                    nodecl_make_list_1(
                        nodecl_make_for_statement(nodecl_loop_control, nodecl_statement, 
                            /* loop_name */ nodecl_null(),
                            ast_get_locus(a))),
                    block_context,
                    ast_get_locus(a)
                    ));
}

static void build_scope_for_statement(AST a,
        decl_context_t decl_context,
        nodecl_t *nodecl_output)
{
    AST loop_control = ASTSon0(a);

    if (ASTType(loop_control) == AST_LOOP_CONTROL)
        return build_scope_for_statement_nonrange(a,
                decl_context,
                nodecl_output);
    else if (IS_CXX_LANGUAGE && ASTType(loop_control) == AST_RANGE_LOOP_CONTROL)
        // C++2011
        // for (T t : e) S;
        return build_scope_for_statement_range(a,
                decl_context,
                nodecl_output);
    else
        internal_error("Code unreachable", 0);
}

static void build_scope_switch_statement(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    decl_context_t block_context = new_block_context(decl_context);

    AST condition = ASTSon0(a);
    AST statement = ASTSon1(a);

    nodecl_t nodecl_condition = nodecl_null();
    build_scope_condition(condition, block_context, &nodecl_condition);

    nodecl_t nodecl_statement = nodecl_null();
    build_scope_normalized_statement(statement, block_context, &nodecl_statement);

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_context(
                nodecl_make_list_1(
                    nodecl_make_switch_statement(nodecl_condition, nodecl_statement, ast_get_locus(a))),
                block_context,
                ast_get_locus(a)));
}

scope_entry_t* add_label_if_not_found(const char* label_text, decl_context_t decl_context, const locus_t* locus)
{
    scope_entry_list_t* entry_list = query_name_str_flags(decl_context, label_text, NULL, DF_LABEL);

    scope_entry_t* sym_label = NULL;
    if (entry_list == NULL)
    {
        sym_label = new_symbol(decl_context, decl_context.function_scope, label_text);
        sym_label->kind = SK_LABEL;
        sym_label->locus = locus;
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
        nodecl_t* nodecl_output)
{
    AST label = ASTSon0(a);
    scope_entry_t* sym_label = add_label_if_not_found(ASTText(label), decl_context, ast_get_locus(label));

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_goto_statement(sym_label, ast_get_locus(a)));
}

static void build_scope_labeled_statement(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    AST label = ASTSon0(a);
    scope_entry_t* sym_label = add_label_if_not_found(ASTText(label), decl_context, ast_get_locus(label));

    AST statement = ASTSon1(a);

    nodecl_t nodecl_statement = nodecl_null();
    build_scope_statement_(statement, decl_context, &nodecl_statement);

    if (nodecl_is_null(nodecl_statement))
    {
        // It can be null because of declarations, just use an empty statement instead
        nodecl_statement = nodecl_make_list_1(nodecl_make_empty_statement(ast_get_locus(a)));
    }

    *nodecl_output = 
        nodecl_make_list_1(
                nodecl_make_labeled_statement(nodecl_statement, sym_label, ast_get_locus(a)));
}

static void build_scope_default_statement(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    AST statement = ASTSon0(a);
    nodecl_t nodecl_statement = nodecl_null();
    build_scope_statement(statement, decl_context, &nodecl_statement);

    if (nodecl_get_kind(nodecl_list_head(nodecl_statement)) == NODECL_CASE_STATEMENT)
    {
        // If we find 'default: case X: S;' we will generate 'default: ; case X: S;'
        *nodecl_output = nodecl_concat_lists(
                nodecl_make_list_1(
                    nodecl_make_default_statement(
                        nodecl_make_list_1(
                            nodecl_make_empty_statement(ast_get_locus(a))),
                        ast_get_locus(a))),
                nodecl_statement);
    }
    else
    {
        *nodecl_output = nodecl_make_list_1(
                nodecl_make_default_statement(nodecl_statement, ast_get_locus(a)));
    }
}

static void build_scope_case_statement(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    AST constant_expression = ASTSon0(a);
    AST statement = ASTSon1(a);
    nodecl_t nodecl_expr = nodecl_null();
    if (!check_expression(constant_expression, decl_context, &nodecl_expr))
    {
        *nodecl_output = nodecl_make_list_1(
                nodecl_make_err_statement(ast_get_locus(a)));
        return;
    }

    nodecl_t nodecl_statement = nodecl_null();
    build_scope_statement(statement, decl_context, &nodecl_statement);

    if (nodecl_get_kind(nodecl_list_head(nodecl_statement)) == NODECL_CASE_STATEMENT)
    {
        // If we find a
        //
        //     case X:
        //     case Y:
        //      S
        //
        // Instead of case X : case Y : S; we will emit
        //
        //     case X:
        //       ;
        //     case Y:
        //       S
        //
        *nodecl_output =
            nodecl_concat_lists(
                    nodecl_make_list_1(
                        nodecl_make_case_statement(
                            nodecl_make_list_1(nodecl_expr),
                            nodecl_make_list_1(
                                nodecl_make_empty_statement(ast_get_locus(a))),
                            ast_get_locus(a))),
                    nodecl_statement);
    }
    else
    {
        *nodecl_output = nodecl_make_list_1(
                nodecl_make_case_statement(
                    nodecl_make_list_1(nodecl_expr), 
                    nodecl_statement, 
                    ast_get_locus(a)));
    }
}

static void build_scope_return_statement(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    AST expression = ASTSon0(a);

    scope_entry_t* function = decl_context.current_scope->related_entry;
    ERROR_CONDITION(function->kind != SK_FUNCTION
            && function->kind != SK_DEPENDENT_FRIEND_FUNCTION
            && function->kind != SK_LAMBDA,
            "Invalid related entry!", 0);

    type_t* return_type = function_type_get_return_type(function->type_information);
    if (return_type == NULL)
        return_type = get_void_type();

    nodecl_t nodecl_return = nodecl_null();

    if (expression != NULL)
    {
        nodecl_t nodecl_expr = nodecl_null();
        if (ASTType(expression) == AST_INITIALIZER_BRACES)
        {
            check_initializer_clause(expression, decl_context, return_type, &nodecl_expr);
        }
        else 
        {
            check_expression(expression, decl_context, &nodecl_expr);
        }

        char valid_expr = !nodecl_is_err_expr(nodecl_expr);

        if (is_void_type(return_type))
        {
            if ((!nodecl_expr_is_type_dependent(nodecl_expr)
                        && !is_void_type(nodecl_get_type(nodecl_expr))))
            {
                error_printf("%s: error: return with non-void expression in a void function\n", ast_location(a));
            }
        }

        if (valid_expr)
        {
            if (!nodecl_expr_is_type_dependent(nodecl_expr))
            {
                if (nodecl_get_kind(nodecl_expr) == NODECL_CXX_BRACED_INITIALIZER)
                {
                    check_nodecl_braced_initializer(
                            nodecl_expr,
                            decl_context,
                            return_type,
                            /* disallow_narrowing */ 0,
                            &nodecl_return);
                }
                else
                {
                    check_nodecl_expr_initializer(nodecl_expr, 
                            decl_context,
                            return_type,
                            /* disallow_narrowing */ 0,
                            &nodecl_return);
                }

                if (nodecl_is_err_expr(nodecl_return))
                {
                    error_printf("%s: error: no conversion is possible from '%s' to '%s' in return statement\n", 
                            ast_location(a),
                            print_type_str(nodecl_get_type(nodecl_expr), decl_context),
                            print_type_str(return_type, decl_context));
                }
            }
            else
            {
                nodecl_return = nodecl_expr;
            }
        }
    }
    else
    {
        if (return_type != NULL
                && !is_error_type(return_type)
                && !is_void_type(return_type))
        {
            error_printf("%s: error: return with no expression in a non-void function\n", ast_location(a));
        }
    }

    *nodecl_output = flush_extra_declared_symbols(ast_get_locus(a));

    *nodecl_output = 
        nodecl_append_to_list(
                *nodecl_output,
                nodecl_make_return_statement(nodecl_return, ast_get_locus(a)));
}

static void build_scope_try_block(AST a, 
        decl_context_t decl_context, 
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

            type_t* declarator_type = type_info;

            nodecl_t exception_name = nodecl_null();
            if (declarator != NULL)
            {
                dummy = nodecl_null();
                compute_declarator_type(declarator, &gather_info, type_info, &declarator_type,
                        block_context, &dummy);

                scope_entry_t* entry = build_scope_declarator_name(declarator,
                        declarator_type, &gather_info, block_context);

                if (entry != NULL)
                {
                    exception_name = nodecl_make_object_init(entry, ast_get_locus(declarator));

                    keep_gcc_attributes_in_symbol(entry, &gather_info);
                    keep_ms_declspecs_in_symbol(entry, &gather_info);
                }
            }

            nodecl_t nodecl_catch_statement = nodecl_null();
            build_scope_statement(handler_compound_statement, block_context, &nodecl_catch_statement);

            nodecl_catch_list = nodecl_append_to_list(nodecl_catch_list, 
                    nodecl_make_catch_handler(exception_name, 
                        nodecl_make_list_1(
                            nodecl_make_context(
                                nodecl_catch_statement,
                                block_context,
                                ast_get_locus(exception_declaration))),
                        declarator_type,
                        ast_get_locus(exception_declaration)));
        }
        else
        {
            if (seen_any_case)
            {
                error_printf("%s: error: more than one 'catch(...)' handler in try-block\n",
                        ast_location(exception_declaration));
                return;
            }
            seen_any_case = 1;
            build_scope_statement(handler_compound_statement, block_context, &nodecl_catch_any);
        }
    }


    *nodecl_output = nodecl_make_list_1(
            nodecl_make_try_block(nodecl_statement, nodecl_catch_list, nodecl_catch_any, ast_get_locus(a)));
}

static void build_scope_do_statement(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output) 
{
    AST statement = ASTSon0(a);
    AST expression = ASTSon1(a);

    nodecl_t nodecl_statement = nodecl_null();
    build_scope_normalized_statement(statement, decl_context, &nodecl_statement);

    nodecl_t nodecl_expr = nodecl_null();
    if (!check_expression(expression, decl_context, &nodecl_expr))
    {
        *nodecl_output = nodecl_make_list_1(
            nodecl_make_err_statement(ast_get_locus(a)));
        return;
    }

    *nodecl_output = nodecl_make_list_1(nodecl_make_do_statement(nodecl_statement, 
                nodecl_expr, ast_get_locus(a)));
}

static void build_scope_empty_statement(AST a, 
        decl_context_t decl_context UNUSED_PARAMETER, 
        nodecl_t* nodecl_output)
{
    *nodecl_output = nodecl_make_list_1(
            nodecl_make_empty_statement(ast_get_locus(a)));
}

static void build_scope_break(AST a, 
        decl_context_t decl_context UNUSED_PARAMETER, 
        nodecl_t* nodecl_output)
{
    *nodecl_output = nodecl_make_list_1(
            nodecl_make_break_statement(
                /* construct_name */ nodecl_null(),
                ast_get_locus(a)));
}

static void build_scope_continue(AST a, 
        decl_context_t decl_context UNUSED_PARAMETER, 
        nodecl_t* nodecl_output)
{
    *nodecl_output = nodecl_make_list_1(
            nodecl_make_continue_statement(
                /* construct_name */ nodecl_null(),
                ast_get_locus(a)));
}

static void build_scope_pragma_custom_directive(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    common_build_scope_pragma_custom_directive(a, decl_context, nodecl_output);

    // We expect lists of statements at statement level
    *nodecl_output = nodecl_make_list_1(*nodecl_output);
}

typedef
struct declaration_pragma_info_tag
{
    scope_entry_list_t** declared_symbols;
    gather_decl_spec_list_t *gather_decl_spec_list;

    int num_pragma_lines;
    nodecl_t *pragma_lines;
    int num_pragma_texts;
    const char** pragma_texts;

    char is_function_definition;
} declaration_pragma_info_t;

typedef
struct pragma_block_level_info_tag
{
    char is_declaration;
    declaration_pragma_info_t declaration_pragma;
} pragma_block_level_info_t;

static void build_scope_pragma_custom_construct_statement_or_decl_rec(AST pragma,
        decl_context_t decl_context, 
        nodecl_t* nodecl_output, 
        pragma_block_level_info_t* info)
{
    ERROR_CONDITION(ASTType(pragma) != AST_PRAGMA_CUSTOM_CONSTRUCT, "Invalid node", 0);

    AST pragma_line = ASTSon0(pragma);
    AST pragma_stmt = ASTSon1(pragma);
    AST end_clauses = ASTSon2(pragma);

    nodecl_t nodecl_pragma_line = nodecl_null();
    common_build_scope_pragma_custom_line(pragma_line, end_clauses, decl_context, &nodecl_pragma_line);

    if (ASTType(pragma_stmt) == AST_AMBIGUITY)
        solve_ambiguous_statement(pragma_stmt, decl_context);

    nodecl_t nodecl_statement = nodecl_null();

    switch (ASTType(pragma_stmt))
    {
        case AST_AMBIGUITY:
            {
                internal_error("This should not happen", 0);
            }
        case AST_PRAGMA_CUSTOM_DIRECTIVE:
            {
                error_printf("%s: error: invalid nesting of #pragma\n",
                        ast_location(pragma_stmt));
                *nodecl_output = nodecl_make_err_statement(ast_get_locus(pragma_stmt));
                return;
            }
        case AST_PRAGMA_CUSTOM_CONSTRUCT:
            {
                build_scope_pragma_custom_construct_statement_or_decl_rec(pragma_stmt,
                        decl_context, &nodecl_statement, info);
                break;
            }
        case AST_DECLARATION_STATEMENT:
            {
                // FIXME
                info->is_declaration = 1;

                AST declaration = ASTSon0(pragma_stmt);
                // Note that here we use nodecl_output instead of nodecl_statement
                build_scope_declaration(declaration, decl_context, nodecl_output,
                        info->declaration_pragma.declared_symbols,
                        info->declaration_pragma.gather_decl_spec_list);
                break;
            }
        default:
            {
                build_scope_statement(pragma_stmt, decl_context, &nodecl_statement);
                break;
            }
    }

    if (!info->is_declaration)
    {
        *nodecl_output = nodecl_make_list_1(
            nodecl_make_pragma_custom_statement(
                nodecl_pragma_line,
                nodecl_statement,
                ASTText(pragma),
                ast_get_locus(pragma)));
    }
    else
    {
        P_LIST_ADD(info->declaration_pragma.pragma_lines,
                info->declaration_pragma.num_pragma_lines,
                nodecl_pragma_line);
        P_LIST_ADD(info->declaration_pragma.pragma_texts,
                info->declaration_pragma.num_pragma_texts,
                ASTText(pragma));
    }
}

// Picks the context of the first non null parameter
static decl_context_t get_prototype_context_if_any(decl_context_t decl_context,
        scope_entry_t* entry,
        gather_decl_spec_t gather_info,
        declaration_pragma_info_t* info)
{
    decl_context_t result = decl_context;

    if (entry->kind == SK_FUNCTION)
    {
        int i;
        for (i = 0; i < gather_info.num_arguments_info; i++)
        {
            if (gather_info.arguments_info[i].entry != NULL)
            {
                result = gather_info.arguments_info[i].entry->decl_context;
                break;
            }
        }

        if (info->is_function_definition)
        {
            // If no parameters were found use the context of the function
            result = nodecl_get_decl_context(
                    nodecl_get_child(entry->entity_specs.function_code, 0)
                    );
        }
    }

    return result;
}

static void finish_pragma_declaration(
        decl_context_t decl_context,
        nodecl_t *nodecl_output,
        declaration_pragma_info_t* info)
{
    ERROR_CONDITION(info->num_pragma_lines == 0, "This cannot happen", 0);

    nodecl_t nodecl_pragma_declarations = nodecl_null();
    scope_entry_list_iterator_t* it = NULL;
    int i;
    for (it = entry_list_iterator_begin(*info->declared_symbols), i = 0;
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it), i++)
    {
        scope_entry_t* entry = entry_list_iterator_current(it);
        gather_decl_spec_t gather_info;
        copy_gather_info(&gather_info, &(info->gather_decl_spec_list->items[i]));

        nodecl_t nodecl_single_pragma_declaration = nodecl_null();
        int j;
        for (j = 0; j < info->num_pragma_lines; j++)
        {
            nodecl_single_pragma_declaration = nodecl_make_pragma_custom_declaration(
                    nodecl_shallow_copy(info->pragma_lines[j]),
                    nodecl_single_pragma_declaration,
                    nodecl_make_pragma_context(decl_context, entry->locus),
                    nodecl_make_pragma_context(
                        get_prototype_context_if_any(decl_context, entry, gather_info, info),
                        entry->locus),
                    entry,
                    info->pragma_texts[j],
                    nodecl_get_locus(info->pragma_lines[j]));
        }

        nodecl_pragma_declarations = nodecl_append_to_list(nodecl_pragma_declarations,
                nodecl_single_pragma_declaration);
    }

    entry_list_iterator_free(it);
    entry_list_free(*info->declared_symbols);

    xfree(info->pragma_texts);
    xfree(info->pragma_lines);

    *nodecl_output = nodecl_concat_lists(*nodecl_output,
            nodecl_pragma_declarations);
}

static void build_scope_pragma_custom_construct_statement(AST a,
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    pragma_block_level_info_t info;
    memset(&info, 0, sizeof(info));

    scope_entry_list_t* declared_symbols = NULL;
    gather_decl_spec_list_t gather_decl_spec_list;
    memset(&gather_decl_spec_list, 0, sizeof(gather_decl_spec_list));

    info.declaration_pragma.declared_symbols = &declared_symbols;
    info.declaration_pragma.gather_decl_spec_list = &gather_decl_spec_list;

    build_scope_pragma_custom_construct_statement_or_decl_rec(a, decl_context, nodecl_output, &info);

    if (info.is_declaration)
    {
        finish_pragma_declaration(decl_context, nodecl_output, &info.declaration_pragma);
    }
}

static void build_scope_pragma_custom_construct_declaration_rec(
        AST pragma,
        decl_context_t decl_context,
        nodecl_t *nodecl_output,
        declaration_pragma_info_t* info)
{
    ERROR_CONDITION(ASTType(pragma) != AST_PRAGMA_CUSTOM_CONSTRUCT, "Invalid node", 0);

    AST pragma_line = ASTSon0(pragma);
    AST pragma_decl = ASTSon1(pragma);
    AST end_clauses = ASTSon2(pragma);

    nodecl_t nodecl_pragma_line = nodecl_null();
    common_build_scope_pragma_custom_line(pragma_line, end_clauses, decl_context, &nodecl_pragma_line);

    if (ASTType(pragma_decl) == AST_AMBIGUITY)
        solve_ambiguous_declaration(pragma_decl, decl_context);

    switch (ASTType(pragma_decl))
    {
        case AST_AMBIGUITY:
            {
                internal_error("This should not happen", 0);
            }
        case AST_PRAGMA_CUSTOM_DIRECTIVE:
            {
                error_printf("%s: error: invalid nesting of #pragma\n",
                        ast_location(pragma_decl));
                return;
            }
        case AST_PRAGMA_CUSTOM_CONSTRUCT:
            {
                build_scope_pragma_custom_construct_declaration_rec(pragma_decl, decl_context, nodecl_output, info);
                break;
            }
        default:
            {
                // FIXME
                build_scope_declaration(pragma_decl, decl_context, nodecl_output,
                        info->declared_symbols,
                        info->gather_decl_spec_list);
                if (ASTType(pragma_decl) == AST_FUNCTION_DEFINITION)
                {
                    info->is_function_definition = 1;
                }
                break;
            }
    }

    P_LIST_ADD(info->pragma_lines,
            info->num_pragma_lines,
            nodecl_pragma_line);
    P_LIST_ADD(info->pragma_texts,
            info->num_pragma_texts,
            ASTText(pragma));
}

static void build_scope_pragma_custom_construct_declaration(AST a,
        decl_context_t decl_context,
        nodecl_t *nodecl_output)
{
    scope_entry_list_t* declared_symbols = NULL;
    gather_decl_spec_list_t gather_decl_spec_list;
    memset(&gather_decl_spec_list, 0, sizeof(gather_decl_spec_list));

    declaration_pragma_info_t info;
    memset(&info, 0, sizeof(info));

    info.declared_symbols = &declared_symbols;
    info.gather_decl_spec_list = &gather_decl_spec_list;

    build_scope_pragma_custom_construct_declaration_rec(a, decl_context, nodecl_output, &info);

    finish_pragma_declaration(decl_context, nodecl_output, &info);
}

typedef
struct member_declaration_pragma_info_tag
{
    type_t* class_info;
    access_specifier_t current_access;
    declaration_pragma_info_t declaration_pragma;
} member_declaration_pragma_info_t;

static void build_scope_pragma_custom_construct_member_declaration_rec(
        AST pragma,
        decl_context_t decl_context,
        nodecl_t *nodecl_output,
        member_declaration_pragma_info_t* info)
{
    ERROR_CONDITION(ASTType(pragma) != AST_PRAGMA_CUSTOM_CONSTRUCT, "Invalid node", 0);

    AST pragma_line = ASTSon0(pragma);
    AST pragma_decl = ASTSon1(pragma);
    AST end_clauses = ASTSon2(pragma);

    nodecl_t nodecl_pragma_line = nodecl_null();
    common_build_scope_pragma_custom_line(pragma_line, end_clauses, decl_context, &nodecl_pragma_line);

    if (ASTType(pragma_decl) == AST_AMBIGUITY)
        solve_ambiguous_declaration(pragma_decl, decl_context);

    switch (ASTType(pragma_decl))
    {
        case AST_AMBIGUITY:
            {
                internal_error("This should not happen", 0);
            }
        case AST_PRAGMA_CUSTOM_DIRECTIVE:
            {
                error_printf("%s: error: invalid nesting of #pragma %s\n",
                        ast_location(pragma_decl),
                        ast_get_text(pragma_decl));
                return;
            }
        case AST_PRAGMA_CUSTOM_CONSTRUCT:
            {
                build_scope_pragma_custom_construct_member_declaration_rec(pragma_decl, decl_context, nodecl_output, info);
                break;
            }
        default:
            {
                build_scope_member_declaration(decl_context, pragma_decl,
                        info->current_access,
                        info->class_info, 
                        nodecl_output,
                        info->declaration_pragma.declared_symbols, 
                        info->declaration_pragma.gather_decl_spec_list);
                break;
            }
    }

    P_LIST_ADD(info->declaration_pragma.pragma_lines,
            info->declaration_pragma.num_pragma_lines,
            nodecl_pragma_line);
    P_LIST_ADD(info->declaration_pragma.pragma_texts,
            info->declaration_pragma.num_pragma_texts,
            ASTText(pragma));
}

static void build_scope_pragma_custom_construct_member_declaration(AST a, 
        decl_context_t decl_context, 
        access_specifier_t current_access,
        type_t* class_info,
        nodecl_t* nodecl_output)
{
    scope_entry_list_t* declared_symbols = NULL;
    gather_decl_spec_list_t gather_decl_spec_list;
    memset(&gather_decl_spec_list, 0, sizeof(gather_decl_spec_list));

    member_declaration_pragma_info_t info;
    memset(&info, 0, sizeof(info));
    info.class_info = class_info;
    info.current_access = current_access;
    info.declaration_pragma.declared_symbols = &declared_symbols;
    info.declaration_pragma.gather_decl_spec_list = &gather_decl_spec_list;

    build_scope_pragma_custom_construct_member_declaration_rec(a, decl_context, nodecl_output, &info);

    finish_pragma_declaration(decl_context, nodecl_output, &info.declaration_pragma);
}


static void build_scope_upc_synch_statement(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{

    nodecl_t nodecl_expression = nodecl_null();
    if (ASTSon0(a) != NULL)
    {
        nodecl_t nodecl_expr = nodecl_null();
        check_expression(ASTSon0(a), decl_context, &nodecl_expr);
    }
    else
    {
        nodecl_expression = nodecl_make_integer_literal(get_signed_int_type(), 
                const_value_get_signed_int(0), ast_get_locus(a));
    }

    nodecl_expression = nodecl_make_list_1(nodecl_expression);


    const char* stmt_name = NULL;
#define START_CHECKS \
    if (0);
#define CHECK(x) \
    else if (ASTType(a) == AST_##x) { stmt_name = x##_STATEMENT; }
#define END_CHECKS \
    ERROR_CONDITION(stmt_name == NULL, "Invalid statmeent name", 0);

    START_CHECKS
       CHECK(UPC_NOTIFY)
       CHECK(UPC_WAIT)
       CHECK(UPC_BARRIER)
       CHECK(UPC_FENCE)
    END_CHECKS

    *nodecl_output = nodecl_make_upc_sync_statement(nodecl_expression, stmt_name, ast_get_locus(a));
}

static void build_scope_upc_forall_statement(AST a, 
        decl_context_t decl_context, 
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
                /* is_template */ 0, /* is_explicit_specialization */ 0,
                &nodecl_for_init,
                /* declared_symbols */ NULL, /* gather_decl_spec_list_t */ NULL);
    }
    else if (ASTType(for_init_statement) == AST_EXPRESSION_STATEMENT)
    {
        build_scope_expression_statement(for_init_statement, block_context, &nodecl_for_init);
    }

    nodecl_t nodecl_condition = nodecl_null();
    if (condition != NULL)
    {
        check_expression(condition, block_context, &nodecl_condition);
    }
    else
    {
        nodecl_condition = nodecl_make_integer_literal(get_signed_int_type(), 
                const_value_get_signed_int(0), ast_get_locus(a));
    }

    nodecl_t nodecl_iter = nodecl_null();
    if (expression != NULL)
    {
        check_expression(expression, block_context, &nodecl_iter);
    }
    else
    {
        nodecl_iter = nodecl_make_integer_literal(get_signed_int_type(), 
                const_value_get_signed_int(0), ast_get_locus(a));
    }

    nodecl_t nodecl_affinity = nodecl_null();
    if (affinity != NULL)
    {
        check_expression(affinity, block_context, &nodecl_affinity);
    }
    else
    {
        nodecl_affinity = nodecl_make_integer_literal(get_signed_int_type(), 
                const_value_get_signed_int(0), ast_get_locus(a));
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

    internal_error("Not yet implemented", 0);
}

static void build_scope_nodecl_literal(AST a, decl_context_t decl_context UNUSED_PARAMETER, nodecl_t* nodecl_output)
{
    *nodecl_output = nodecl_make_from_ast_nodecl_literal(a);
    if (!nodecl_is_list(*nodecl_output))
    {
        *nodecl_output = nodecl_make_list_1(*nodecl_output);
    }
}

static void build_scope_fortran_allocate_statement(AST a, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    // Mimick build_scope_allocate_stmt in fortran03-buildscope.c
    nodecl_t nodecl_opt_value = nodecl_null();

    AST expression = ASTSon0(a);

    nodecl_t nodecl_expr = nodecl_null();
    check_expression(expression, decl_context, &nodecl_expr);

    if (nodecl_is_err_expr(nodecl_expr))
    {
        *nodecl_output = nodecl_make_err_statement(ast_get_locus(a));
        return;
    }

    nodecl_t nodecl_allocate_list = nodecl_make_list_1(nodecl_expr);

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_fortran_allocate_statement(nodecl_allocate_list, 
                nodecl_opt_value,
                ast_get_locus(a)));
}

#define STMT_HANDLER(type, hndl) [type] = { .handler = (hndl) }

static stmt_scope_handler_map_t stmt_scope_handlers[] =
{
    STMT_HANDLER(AST_AMBIGUITY, build_scope_ambiguity_handler),
    STMT_HANDLER(AST_EXPRESSION_STATEMENT, build_scope_expression_statement),
    STMT_HANDLER(AST_DECLARATION_STATEMENT, build_scope_declaration_statement),
    STMT_HANDLER(AST_COMPOUND_STATEMENT, build_scope_compound_statement),
    STMT_HANDLER(AST_DO_STATEMENT, build_scope_do_statement),
    STMT_HANDLER(AST_WHILE_STATEMENT, build_scope_while_statement),
    STMT_HANDLER(AST_IF_ELSE_STATEMENT, build_scope_if_else_statement),
    STMT_HANDLER(AST_FOR_STATEMENT, build_scope_for_statement),
    STMT_HANDLER(AST_LABELED_STATEMENT, build_scope_labeled_statement),
    STMT_HANDLER(AST_DEFAULT_STATEMENT, build_scope_default_statement),
    STMT_HANDLER(AST_CASE_STATEMENT, build_scope_case_statement),
    STMT_HANDLER(AST_RETURN_STATEMENT, build_scope_return_statement),
    STMT_HANDLER(AST_TRY_BLOCK, build_scope_try_block),
    STMT_HANDLER(AST_SWITCH_STATEMENT, build_scope_switch_statement),
    STMT_HANDLER(AST_EMPTY_STATEMENT, build_scope_empty_statement),
    STMT_HANDLER(AST_BREAK_STATEMENT, build_scope_break),
    STMT_HANDLER(AST_CONTINUE_STATEMENT, build_scope_continue),
    STMT_HANDLER(AST_GOTO_STATEMENT, build_scope_goto_statement),
    // Pragma custom support
    STMT_HANDLER(AST_PRAGMA_CUSTOM_CONSTRUCT, build_scope_pragma_custom_construct_statement),
    STMT_HANDLER(AST_PRAGMA_CUSTOM_DIRECTIVE, build_scope_pragma_custom_directive),
    // UPC
    STMT_HANDLER(AST_UPC_NOTIFY, build_scope_upc_synch_statement),
    STMT_HANDLER(AST_UPC_WAIT, build_scope_upc_synch_statement),
    STMT_HANDLER(AST_UPC_BARRIER, build_scope_upc_synch_statement),
    STMT_HANDLER(AST_UPC_FENCE, build_scope_upc_synch_statement),
    STMT_HANDLER(AST_UPC_FORALL, build_scope_upc_forall_statement),
    // Special nodes that come only from TL::Source
    STMT_HANDLER(AST_NODE_LIST, build_scope_implicit_compound_statement),
    STMT_HANDLER(AST_STATEMENT_PLACEHOLDER, check_statement_placeholder),
    STMT_HANDLER(AST_NODECL_LITERAL, build_scope_nodecl_literal),
    STMT_HANDLER(AST_FORTRAN_ALLOCATE_STATEMENT, build_scope_fortran_allocate_statement),
};

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

static void build_scope_statement_(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "==== Statement line [%s] ====\n", ast_location(a));
    }

    stmt_scope_handler_t f = stmt_scope_handlers[ASTType(a)].handler;

    if (f != NULL)
    {
        f(a, decl_context, nodecl_output);
    }
    else
    {
        WARNING_MESSAGE("Statement node type '%s' does not have handler in %s", ast_print_node_type(ASTType(a)),
                ast_location(a));
    }
}

void build_scope_statement(AST a, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    diagnostic_context_push_buffered();
    build_scope_statement_(a, decl_context, nodecl_output);
    diagnostic_context_pop_and_commit();
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
        case AST_DECLARATOR_FUNC_TRAIL :
            {
                AST parameters_and_qualifiers = ASTSon1(funct_declarator);
                return ASTSon0(parameters_and_qualifiers);
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
        case AST_DECLARATOR :
        case AST_DECLARATOR_ID_PACK:
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
        case AST_DECLARATOR_FUNC_TRAIL :
            {
                return get_declarator_id_expression(ASTSon0(a), decl_context);
                break;
            }
        case AST_DECLARATOR_ID_EXPR :
            {
                return a;
                break;
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

const char* get_conversion_function_name(decl_context_t decl_context, 
        AST conversion_function_id, 
        type_t** result_conversion_type)
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

    nodecl_t dummy_nodecl_output = nodecl_null();
    build_scope_decl_specifier_seq(type_specifier, &gather_info, &simple_type_info,
            decl_context, &dummy_nodecl_output);

    type_t* type_info = NULL;
    compute_declarator_type(conversion_declarator, &gather_info, simple_type_info, &type_info,
            decl_context, &dummy_nodecl_output);

    if (result_conversion_type != NULL)
    {
        *result_conversion_type = type_info;
    }

    return UNIQUESTR_LITERAL("$.operator");
}

nodecl_t internal_expression_parse(const char *source, decl_context_t decl_context)
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

    nodecl_t nodecl_expr = nodecl_null();
    diagnostic_context_push_buffered();
    char c = check_expression(a, decl_context, &nodecl_expr);
    diagnostic_context_pop_and_discard();

    if (!c)
    {
        internal_error("Internally parsed expression '%s' could not be properly checked\n",
                prettyprint_in_buffer(a));
    }

    ast_free(a);

    return nodecl_expr;
}

scope_entry_t* entry_advance_aliases(scope_entry_t* entry)
{
    if (entry != NULL 
            && (entry->kind == SK_USING))
        return entry_advance_aliases(entry->entity_specs.alias_to);

    return entry;
}

// Instantiation of statements
#include "cxx-nodecl-visitor.h"

typedef
struct nodecl_instantiate_stmt_visitor_tag
{
    nodecl_external_visitor_t _base_visitor;

    decl_context_t orig_decl_context;
    decl_context_t new_decl_context;

    scope_entry_t* orig_function_instantiated;
    scope_entry_t* new_function_instantiated;

    instantiation_symbol_map_t *instantiation_symbol_map;

    // Instantiated tree
    nodecl_t nodecl_result;

} nodecl_instantiate_stmt_visitor_t;

typedef void (*nodecl_instantiate_stmt_visitor_fun_t)(nodecl_instantiate_stmt_visitor_t* visitor, nodecl_t node);
typedef void (*nodecl_visitor_fun_t)(nodecl_external_visitor_t* visitor, nodecl_t node);

static nodecl_t instantiate_stmt_walk(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    if (nodecl_is_null(node))
        return nodecl_null();

    v->nodecl_result = nodecl_null();
    if (nodecl_is_list(node))
    {
        nodecl_t result_list = nodecl_null();

        int i, n = 0;
        nodecl_t* list = nodecl_unpack_list(node, &n);
        for (i = 0; i < n; i++)
        {
            nodecl_t current_item = instantiate_stmt_walk(v, list[i]);

            ERROR_CONDITION(!nodecl_is_null(current_item)
                    && nodecl_is_list(current_item),
                    "Instantiated single tree as a list", 0);

            result_list = nodecl_append_to_list(
                    result_list,
                    current_item);
        }

        xfree(list);

        v->nodecl_result = result_list;
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: Instantiating statement '%s' at %s\n",
                    ast_print_node_type(nodecl_get_kind(node)),
                    nodecl_locus_to_str(node));
        }
        NODECL_WALK(v, node);
        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: Ended instantation of statement '%s' at %s\n",
                    ast_print_node_type(nodecl_get_kind(node)),
                    nodecl_locus_to_str(node));
        }
    }

    return v->nodecl_result;
}

static inline nodecl_visitor_fun_t instantiate_stmt_visitor_fun(nodecl_instantiate_stmt_visitor_fun_t p)
{
    return NODECL_VISITOR_FUN(p);
}

static void instantiate_stmt_not_implemented_yet(nodecl_instantiate_stmt_visitor_t* v UNUSED_PARAMETER,
        nodecl_t nodecl_stmt)
{
    internal_error("Instantiation of statement '%s' at '%s' not yet implemented\n",
            ast_print_node_type(nodecl_get_kind(nodecl_stmt)),
            nodecl_locus_to_str(nodecl_stmt));
}

// This function does not return a NODECL_TEMPLATE_FUNCTION_CODE but a NODECL_FUNCTION_CODE
static void instantiate_template_function_code(
        nodecl_instantiate_stmt_visitor_t* v,
        nodecl_t node)
{
    nodecl_t nodecl_context = nodecl_get_child(node, 0);
    nodecl_t nodecl_initializers = nodecl_get_child(node, 1);

    ERROR_CONDITION(nodecl_get_kind(nodecl_context) == NODECL_TRY_BLOCK, "Not yet implemented", 0);

    decl_context_t new_decl_context = new_block_context(v->new_decl_context);
    new_decl_context = new_function_context(new_decl_context);
    ERROR_CONDITION(v->new_function_instantiated == NULL, "Missing new function", 0);
    new_decl_context.current_scope->related_entry = v->new_function_instantiated;

    ERROR_CONDITION(v->orig_function_instantiated == NULL, "Missing orig function", 0);

    v->new_function_instantiated->entity_specs.num_related_symbols = 0;
    xfree(v->new_function_instantiated->entity_specs.related_symbols);
    v->new_function_instantiated->entity_specs.related_symbols = 0;

    v->instantiation_symbol_map = instantiation_symbol_map_push(v->instantiation_symbol_map);

    // Register every parameter in this context
    int i;
    for (i = 0; i < v->orig_function_instantiated->entity_specs.num_related_symbols; i++)
    {
        scope_entry_t* orig_parameter =
                v->orig_function_instantiated->entity_specs.related_symbols[i];
        scope_entry_t* new_parameter = new_symbol(new_decl_context,
                new_decl_context.current_scope,
                orig_parameter->symbol_name);

        new_parameter->kind = SK_VARIABLE;
        new_parameter->type_information = update_type_for_instantiation(
                orig_parameter->type_information,
                new_decl_context,
                nodecl_get_locus(node),
                v->instantiation_symbol_map,
                /* pack */ -1);

        new_parameter->value = instantiate_expression(orig_parameter->value,
                new_decl_context,
                v->instantiation_symbol_map,
                /* pack_index */ -1);

        // WARNING - This is a usual source of issues
        new_parameter->entity_specs = orig_parameter->entity_specs;
        // Clear these
        new_parameter->entity_specs.num_function_parameter_info = 0;
        new_parameter->entity_specs.function_parameter_info = 0;

        P_LIST_ADD(
                v->new_function_instantiated->entity_specs.related_symbols,
                v->new_function_instantiated->entity_specs.num_related_symbols,
                new_parameter);

        symbol_set_as_parameter_of_function(new_parameter, 
                v->new_function_instantiated,
                /* nesting */ 0, /* position */ i);

        instantiation_symbol_map_add(v->instantiation_symbol_map, orig_parameter, new_parameter);
    }

    // Create a new result symbol if any
    if (v->orig_function_instantiated->entity_specs.result_var != NULL)
    {
        scope_entry_t* orig_result_var =
                v->orig_function_instantiated->entity_specs.result_var;
        scope_entry_t* new_result_var = new_symbol(new_decl_context,
                new_decl_context.current_scope,
                orig_result_var->symbol_name);

        new_result_var->kind = SK_VARIABLE;
        new_result_var->type_information = update_type_for_instantiation(
                orig_result_var->type_information,
                new_decl_context,
                nodecl_get_locus(node),
                v->instantiation_symbol_map,
                /* pack */ -1);

        // WARNING - This is a usual source of issues
        new_result_var->entity_specs = orig_result_var->entity_specs;

        v->new_function_instantiated->entity_specs.result_var = new_result_var;

        instantiation_symbol_map_add(v->instantiation_symbol_map, orig_result_var, new_result_var);
    }

    // Register 'this'
    if (v->new_function_instantiated->entity_specs.is_member
            && !v->new_function_instantiated->entity_specs.is_static)
    {
        // The class we belong to
        type_t* pointed_this = v->new_function_instantiated->entity_specs.class_type;
        // Qualify likewise the function unless it is a destructor
        if (!v->new_function_instantiated->entity_specs.is_destructor)
        {
            pointed_this = get_cv_qualified_type(pointed_this,
                    get_cv_qualifier(v->new_function_instantiated->type_information));
        }

        type_t* this_type = get_pointer_type(pointed_this);
        // It is a constant pointer, so qualify like it is
        this_type = get_cv_qualified_type(this_type, CV_CONST);

        scope_entry_t* this_symbol = new_symbol(new_decl_context,
                new_decl_context.current_scope,
                "this");

        this_symbol->kind = SK_VARIABLE;
        this_symbol->type_information = this_type;
        this_symbol->defined = 1;
        this_symbol->do_not_print = 1;

        // Now map the orig this to the new this
        decl_context_t orig_decl_context = nodecl_get_decl_context(nodecl_context);

        scope_entry_list_t* entry_list = query_in_scope_str(orig_decl_context, UNIQUESTR_LITERAL("this"), NULL);
        ERROR_CONDITION(entry_list == NULL, "'this' not found", 0);

        scope_entry_t* orig_this_symbol = entry_list_head(entry_list);
        entry_list_free(entry_list);

        instantiation_symbol_map_add(v->instantiation_symbol_map, orig_this_symbol, this_symbol);
    }


    decl_context_t previous_orig_decl_context = v->orig_decl_context;
    decl_context_t previous_new_decl_context = v->new_decl_context;

    decl_context_t orig_decl_context = nodecl_get_decl_context(nodecl_context);

    v->orig_decl_context = orig_decl_context;
    v->new_decl_context = new_decl_context;

    nodecl_t nodecl_stmt_list = nodecl_get_child(nodecl_context, 0);

    nodecl_t new_nodecl_stmt_list = instantiate_stmt_walk(v, nodecl_stmt_list);
    nodecl_t new_nodecl_initializers = instantiate_stmt_walk(v, nodecl_initializers);

    v->nodecl_result =
        nodecl_make_function_code(
                nodecl_make_context(
                    new_nodecl_stmt_list,
                    new_decl_context,
                    v->new_function_instantiated->locus),
                new_nodecl_initializers,
                v->new_function_instantiated,
                v->new_function_instantiated->locus
                );

    v->orig_decl_context = previous_orig_decl_context;
    v->new_decl_context = previous_new_decl_context;

    v->instantiation_symbol_map = instantiation_symbol_map_pop(v->instantiation_symbol_map);
}

static void instantiate_compound_statement(
        nodecl_instantiate_stmt_visitor_t* v,
        nodecl_t node)
{
    nodecl_t orig_stmt_list = nodecl_get_child(node, 0);

    nodecl_t new_stmt_list = instantiate_stmt_walk(v, orig_stmt_list);

    nodecl_t new_nodecl_destructors = nodecl_null();
    call_destructors_of_classes(v->new_decl_context, nodecl_get_locus(node), &new_nodecl_destructors);

    v->nodecl_result =
        nodecl_make_compound_statement(
                new_stmt_list,
                new_nodecl_destructors,
                nodecl_get_locus(node)
                );
}

static void instantiate_expression_statement(
        nodecl_instantiate_stmt_visitor_t* v,
        nodecl_t node)
{
    nodecl_t expression = nodecl_get_child(node, 0);

    nodecl_t new_expression = instantiate_expression(expression, v->new_decl_context,
            v->instantiation_symbol_map, /* pack_index */ -1);

    v->nodecl_result =
        nodecl_make_expression_statement(
                new_expression,
                nodecl_get_locus(node));
}

static void instantiate_return_statement(
        nodecl_instantiate_stmt_visitor_t* v,
        nodecl_t node)
{
    nodecl_t expression = nodecl_get_child(node, 0);

    nodecl_t new_expression = instantiate_expression(expression, v->new_decl_context,
            v->instantiation_symbol_map, /* pack_index */ -1);

    v->nodecl_result =
        nodecl_make_return_statement(
                new_expression,
                nodecl_get_locus(node));
}

static scope_entry_t* instantiate_declaration_common(
        nodecl_instantiate_stmt_visitor_t* v,
        scope_entry_t* orig_entry,
        const locus_t* locus,
        char is_definition)
{
    scope_entry_t* new_entry = instantiation_symbol_do_map(v->instantiation_symbol_map, orig_entry);
    if (new_entry == NULL)
    {
        new_entry = new_symbol(v->new_decl_context, v->new_decl_context.current_scope, orig_entry->symbol_name);
        new_entry->kind = orig_entry->kind;
        new_entry->locus = orig_entry->locus;

        new_entry->defined = is_definition;

        instantiation_symbol_map_add(v->instantiation_symbol_map, orig_entry, new_entry);

        switch (orig_entry->kind)
        {
            case SK_VARIABLE:
                {
                    new_entry->type_information = update_type_for_instantiation(
                            orig_entry->type_information,
                            v->new_decl_context,
                            locus,
                            v->instantiation_symbol_map,
                            /* pack */ -1);
                    new_entry->entity_specs = orig_entry->entity_specs;
                    nodecl_t value = instantiate_expression(orig_entry->value,
                            v->new_decl_context,
                            v->instantiation_symbol_map,
                            /* pack_index */ -1);

                    nodecl_t nodecl_init = nodecl_null();
                    if (!nodecl_is_null(value))
                    {
                        if (nodecl_get_kind(value) == NODECL_CXX_EQUAL_INITIALIZER
                                || nodecl_get_kind(value) == NODECL_CXX_BRACED_INITIALIZER
                                || nodecl_get_kind(value) == NODECL_CXX_PARENTHESIZED_INITIALIZER)
                        {
                            check_nodecl_initialization(
                                    value,
                                    v->new_decl_context,
                                    new_entry,
                                    get_unqualified_type(new_entry->type_information),
                                    &nodecl_init,
                                    /* FIXME is_auto */ 0);
                        }
                        else
                        {
                            check_nodecl_expr_initializer(value, 
                                    v->new_decl_context,
                                    get_unqualified_type(new_entry->type_information),
                                    /* disallow_narrowing */ 0,
                                    &nodecl_init);
                        }
                    }

                    new_entry->value = nodecl_init;

                    break;
                }
            case SK_TYPEDEF:
                {
                    new_entry->type_information = update_type_for_instantiation(
                            orig_entry->type_information,
                            v->new_decl_context,
                            locus,
                            v->instantiation_symbol_map,
                            /* pack */ -1);
                    break;
                }
            default:
                internal_error("Not yet implemented for symbol kind %s\n", symbol_kind_name(orig_entry));
        }
    }

    return new_entry;
}

static void instantiate_cxx_def_or_decl(
        nodecl_instantiate_stmt_visitor_t* v,
        nodecl_t node,
        nodecl_t (*fun)(nodecl_t, scope_entry_t*, const locus_t*),
        char is_definition)
{
    scope_entry_t* new_entry = instantiate_declaration_common(v, nodecl_get_symbol(node), nodecl_get_locus(node), is_definition);

    nodecl_t orig_nodecl_context = nodecl_get_child(node, 0);
    nodecl_t new_nodecl_context = nodecl_null();

    if (!nodecl_is_null(orig_nodecl_context))
    {
        new_nodecl_context = nodecl_make_context(nodecl_null(), v->new_decl_context, nodecl_get_locus(node));
    }

    v->nodecl_result = fun(new_nodecl_context, new_entry, nodecl_get_locus(node));
}

static void instantiate_cxx_decl(
        nodecl_instantiate_stmt_visitor_t* v,
        nodecl_t node)
{
    instantiate_cxx_def_or_decl(v, node, nodecl_make_cxx_decl, /* is_definition */ 0);
}

static void instantiate_cxx_def(
        nodecl_instantiate_stmt_visitor_t* v,
        nodecl_t node)
{
    instantiate_cxx_def_or_decl(v, node, nodecl_make_cxx_def, /* is_definition */ 1);
}

static nodecl_t instantiate_object_init_node(
        nodecl_instantiate_stmt_visitor_t* v,
        nodecl_t node)
{
    scope_entry_t* new_entry = instantiate_declaration_common(v, nodecl_get_symbol(node), nodecl_get_locus(node), /* is_definition */ 1);

    return nodecl_make_object_init(new_entry, nodecl_get_locus(node));
}

static void instantiate_object_init(
        nodecl_instantiate_stmt_visitor_t* v,
        nodecl_t node)
{
    v->nodecl_result = instantiate_object_init_node(v, node);
}

static void instantiate_cxx_member_init(
        nodecl_instantiate_stmt_visitor_t* v,
        nodecl_t node)
{
    nodecl_t nodecl_cxx_dependent_name = nodecl_get_child(node, 0);

    nodecl_t nodecl_initialization_expression = nodecl_get_child(node, 1);

    nodecl_cxx_dependent_name = update_cxx_dep_qualified_name(nodecl_cxx_dependent_name,
            v->new_decl_context,
            v->instantiation_symbol_map,
            /* FIXME - pack_index */ -1);

    scope_entry_list_t *entry_list = query_nodecl_name_in_class(
            v->new_decl_context,
            v->new_decl_context.class_scope->related_entry,
            nodecl_cxx_dependent_name,
            NULL);

    if (entry_list == NULL)
    {
        v->nodecl_result = nodecl_null();
        return;
    }

    scope_entry_t* entry = entry_list_head(entry_list);
    entry_list_free(entry_list);

    if (entry->kind == SK_CLASS
            || entry->kind == SK_VARIABLE)
    {
        nodecl_initialization_expression = instantiate_expression(
                nodecl_initialization_expression,
                v->new_decl_context,
                v->instantiation_symbol_map,
                /* FIXME: pack_index */ -1);

        type_t* type_to_initialize = NULL;
        if (entry->kind == SK_VARIABLE)
            type_to_initialize = get_unqualified_type(entry->type_information);
        else
            type_to_initialize = get_user_defined_type(entry);

        nodecl_t nodecl_init = nodecl_null();
        check_nodecl_initialization(
                nodecl_initialization_expression,
                v->new_decl_context,
                entry,
                type_to_initialize,
                &nodecl_init,
                /* is_auto */ 0);

        v->nodecl_result = nodecl_make_member_init(
                nodecl_init,
                entry,
                nodecl_get_locus(node));
    }
    else
    {
        v->nodecl_result = nodecl_null();
        return;
    }
}

static void instantiate_do_statement(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_stmt = nodecl_get_child(node, 0);
    nodecl_stmt = instantiate_stmt_walk(v, nodecl_stmt);

    if (nodecl_is_err_stmt(nodecl_stmt))
    {
        v->nodecl_result = nodecl_stmt;
        return;
    }

    nodecl_t nodecl_expr = nodecl_get_child(node, 1);

    nodecl_expr = instantiate_expression(nodecl_expr,
            v->new_decl_context,
            v->instantiation_symbol_map,
            /* pack_index */ 1);

    v->nodecl_result = nodecl_make_do_statement(
            nodecl_stmt,
            nodecl_expr,
            nodecl_get_locus(node));
}

static void instantiate_context(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    decl_context_t existing_context = v->new_decl_context;
    v->new_decl_context = new_block_context(existing_context);

    nodecl_t nodecl_in_context = nodecl_get_child(node, 0);
    nodecl_in_context = instantiate_stmt_walk(v, nodecl_in_context);

    v->nodecl_result = nodecl_make_context(
            nodecl_in_context,
            v->new_decl_context,
            nodecl_get_locus(node));

    v->new_decl_context = existing_context;
}

static nodecl_t instantiate_condition(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    if (nodecl_get_kind(node) == NODECL_OBJECT_INIT)
    {
        return instantiate_object_init_node(v, node);
    }
    else
    {
        // This should be a regular expression
        return instantiate_expression(node,
                v->new_decl_context,
                v->instantiation_symbol_map,
                /* pack_index */ -1);
    }
}

static void instantiate_while_statement(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_condition = nodecl_get_child(node, 0);
    nodecl_t nodecl_statement = nodecl_get_child(node, 1);

    nodecl_condition = instantiate_condition(v, nodecl_condition);
    nodecl_statement = instantiate_stmt_walk(v, nodecl_statement);

    v->nodecl_result = nodecl_make_while_statement(
            nodecl_condition,
            nodecl_statement,
            /* loop_name */ nodecl_null(),
            nodecl_get_locus(node));
}

static void instantiate_if_else_statement(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_condition = nodecl_get_child(node, 0);
    nodecl_t nodecl_then = nodecl_get_child(node, 1);
    nodecl_t nodecl_else = nodecl_get_child(node, 2);

    nodecl_condition = instantiate_condition(v, nodecl_condition);
    nodecl_then = instantiate_stmt_walk(v, nodecl_then);
    nodecl_else = instantiate_stmt_walk(v, nodecl_else);

    v->nodecl_result = nodecl_make_if_else_statement(
            nodecl_condition,
            nodecl_then,
            nodecl_else,
            nodecl_get_locus(node));
}

static nodecl_t instantiate_loop_control(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    if (nodecl_get_kind(node) == NODECL_LOOP_CONTROL)
    {
        nodecl_t nodecl_init = nodecl_get_child(node, 0);
        nodecl_t cond = nodecl_get_child(node, 1);
        nodecl_t next = nodecl_get_child(node, 2);

        nodecl_t new_expr_list = nodecl_null();
        int i, n;
        nodecl_t* expr_list = nodecl_unpack_list(nodecl_init, &n);
        for (i = 0; i < n; i++)
        {
            // We can expect object-inits here, the name of the tree is misleading
            nodecl_t expr = expr_list[i];
            nodecl_t new_expr = nodecl_null();

            if (nodecl_get_kind(expr) == NODECL_OBJECT_INIT)
            {
                new_expr = instantiate_object_init_node(v, expr);
            }
            else
            {
                new_expr = instantiate_expression(expr,
                        v->new_decl_context,
                        v->instantiation_symbol_map,
                        /* pack_index */ -1);
            }

            new_expr_list = nodecl_append_to_list(new_expr_list, new_expr);
        }
        xfree(expr_list);

        cond = instantiate_expression(cond,
                v->new_decl_context,
                v->instantiation_symbol_map,
                /* pack_index */ -1);
        
        next = instantiate_expression(cond,
                v->new_decl_context,
                v->instantiation_symbol_map,
                /* pack_index */ -1);

        return nodecl_make_loop_control(new_expr_list, cond, next, nodecl_get_locus(node));
    }
    else if (nodecl_get_kind(node) == NODECL_ITERATOR_LOOP_CONTROL)
    {
        instantiate_stmt_not_implemented_yet(v, node);
    }
    else
    {
        internal_error("Unexpected loop control '%s' at '%s'\n", ast_print_node_type(nodecl_get_kind(node)), nodecl_locus_to_str(node));
    }

    return nodecl_null();
}

static void instantiate_for_statement(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_loop_control = nodecl_get_child(node, 0);
    nodecl_t nodecl_statement = nodecl_get_child(node, 1);

    nodecl_loop_control = instantiate_loop_control(v, nodecl_loop_control);
    nodecl_statement = instantiate_stmt_walk(v, nodecl_statement);

    v->nodecl_result = nodecl_make_for_statement(
            nodecl_loop_control,
            nodecl_statement,
            /* loop_name */ nodecl_null(),
            nodecl_get_locus(node));
}

static void instantiate_labeled_statement(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_statement = nodecl_get_child(node, 0);

    scope_entry_t* new_label = add_label_if_not_found(nodecl_get_symbol(node)->symbol_name,
            v->new_decl_context,
            nodecl_get_locus(node));

    nodecl_statement = instantiate_stmt_walk(v, nodecl_statement);

    v->nodecl_result = nodecl_make_labeled_statement(nodecl_statement, new_label, nodecl_get_locus(node));
}

static void instantiate_default_statement(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_statement = nodecl_get_child(node, 1);
    nodecl_statement = instantiate_stmt_walk(v, nodecl_statement);

    v->nodecl_result = nodecl_make_default_statement(nodecl_statement, nodecl_get_locus(node));
}

static void instantiate_case_statement(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    nodecl_t expr_list = nodecl_get_child(node, 0);
    nodecl_t nodecl_statement = nodecl_get_child(node, 1);

    nodecl_t new_expr_list = nodecl_null();
    int i, n;
    nodecl_t* list = nodecl_unpack_list(expr_list, &n);
    for (i = 0; i < n; i++)
    {
        nodecl_t expr = list[i];
        nodecl_t new_expr = instantiate_expression(expr,
                v->new_decl_context,
                v->instantiation_symbol_map,
                /* pack_index */ -1);

        new_expr_list = nodecl_append_to_list(new_expr_list, new_expr);
    }
    xfree(list);

    nodecl_statement = instantiate_stmt_walk(v, nodecl_statement);

    v->nodecl_result = nodecl_make_case_statement(
            new_expr_list,
            nodecl_statement,
            nodecl_get_locus(node));
}

static void instantiate_catch_handler(nodecl_instantiate_stmt_visitor_t *v, nodecl_t node)
{
    nodecl_t exception_name = nodecl_get_child(node, 0);
    nodecl_t stmt_list = nodecl_get_child(node, 1);
    type_t* catch_type = nodecl_get_type(node);

    {
        // This is a bit silly because exception name is in the context of statement
        // so we cannot use instantiate_context directly
        decl_context_t existing_context = v->new_decl_context;
        v->new_decl_context = new_block_context(existing_context);

        exception_name = instantiate_stmt_walk(v, exception_name);

        nodecl_t nodecl_statement = nodecl_list_head(stmt_list);
        nodecl_statement = instantiate_stmt_walk(v, nodecl_statement);

        stmt_list = nodecl_make_list_1(
                nodecl_make_context(
                    nodecl_statement,
                    v->new_decl_context,
                    nodecl_get_locus(stmt_list)));

        v->new_decl_context = existing_context;

    }

    catch_type = update_type_for_instantiation(
            catch_type,
            v->new_decl_context,
            nodecl_get_locus(node),
            v->instantiation_symbol_map,
            /* pack */ -1);

    v->nodecl_result = nodecl_make_catch_handler(
        exception_name,
        stmt_list,
        catch_type,
        nodecl_get_locus(node));
}

static void instantiate_try_block(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_statement = nodecl_get_child(node, 0);
    nodecl_t nodecl_catch_list = nodecl_get_child(node, 1);
    nodecl_t nodecl_catch_any = nodecl_get_child(node, 2);

    nodecl_statement = instantiate_stmt_walk(v, nodecl_statement);
    nodecl_catch_list = instantiate_stmt_walk(v, nodecl_catch_list);
    nodecl_catch_any = instantiate_stmt_walk(v, nodecl_catch_any);

    v->nodecl_result = nodecl_make_try_block(
            nodecl_statement,
            nodecl_catch_list,
            nodecl_catch_any,
            nodecl_get_locus(node));
}

static void instantiate_switch_statement(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_condition = nodecl_get_child(node, 0);
    nodecl_t nodecl_statement = nodecl_get_child(node, 1);

    nodecl_condition = instantiate_condition(v, nodecl_condition);
    nodecl_statement = instantiate_stmt_walk(v, nodecl_statement);

    v->nodecl_result = nodecl_make_switch_statement(
            nodecl_condition,
            nodecl_statement,
            nodecl_get_locus(node));
}

static void instantiate_empty_statement(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    v->nodecl_result = nodecl_make_empty_statement(nodecl_get_locus(node));
}

static void instantiate_break_statement(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    v->nodecl_result = nodecl_make_break_statement(/* construct-name */ nodecl_null(), nodecl_get_locus(node));
}

static void instantiate_continue_statement(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    v->nodecl_result = nodecl_make_continue_statement(/* construct-name */ nodecl_null(), nodecl_get_locus(node));
}

static void instantiate_goto_statement(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    scope_entry_t* label = add_label_if_not_found(nodecl_get_symbol(node)->symbol_name, v->new_decl_context, nodecl_get_locus(node));
    
    v->nodecl_result = nodecl_make_goto_statement(label, nodecl_get_locus(node));
}

static void instantiate_pragma_custom_statement(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    instantiate_stmt_not_implemented_yet(v, node);
}

static void instantiate_pragma_custom_declaration(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    instantiate_stmt_not_implemented_yet(v, node);
}


// Initialization
static void instantiate_stmt_init_visitor(nodecl_instantiate_stmt_visitor_t* v,
        decl_context_t orig_decl_context,
        decl_context_t new_decl_context,
        instantiation_symbol_map_t* instantiation_symbol_map,
        scope_entry_t* orig_function_instantiated,
        scope_entry_t* new_function_instantiated)
{
    memset(v, 0, sizeof(*v));

    nodecl_init_walker((nodecl_external_visitor_t*)v, instantiate_stmt_visitor_fun(instantiate_stmt_not_implemented_yet));

    v->orig_decl_context = orig_decl_context;
    v->new_decl_context = new_decl_context;

    v->instantiation_symbol_map = instantiation_symbol_map;

    v->orig_function_instantiated = orig_function_instantiated;
    v->new_function_instantiated = new_function_instantiated;

    NODECL_VISITOR(v)->visit_cxx_def = instantiate_stmt_visitor_fun(instantiate_cxx_def);
    NODECL_VISITOR(v)->visit_cxx_decl = instantiate_stmt_visitor_fun(instantiate_cxx_decl);
    NODECL_VISITOR(v)->visit_cxx_explicit_instantiation = NULL;
    NODECL_VISITOR(v)->visit_cxx_extern_explicit_instantiation = NULL;
    NODECL_VISITOR(v)->visit_cxx_using_namespace = NULL;
    NODECL_VISITOR(v)->visit_cxx_using_decl = NULL;

    NODECL_VISITOR(v)->visit_cxx_member_init = instantiate_stmt_visitor_fun(instantiate_cxx_member_init);

    NODECL_VISITOR(v)->visit_object_init = instantiate_stmt_visitor_fun(instantiate_object_init);

    NODECL_VISITOR(v)->visit_template_function_code = instantiate_stmt_visitor_fun(instantiate_template_function_code);
    NODECL_VISITOR(v)->visit_compound_statement = instantiate_stmt_visitor_fun(instantiate_compound_statement);
    NODECL_VISITOR(v)->visit_expression_statement = instantiate_stmt_visitor_fun(instantiate_expression_statement);
    NODECL_VISITOR(v)->visit_return_statement = instantiate_stmt_visitor_fun(instantiate_return_statement);
    NODECL_VISITOR(v)->visit_do_statement = instantiate_stmt_visitor_fun(instantiate_do_statement);
    NODECL_VISITOR(v)->visit_while_statement = instantiate_stmt_visitor_fun(instantiate_while_statement);
    NODECL_VISITOR(v)->visit_if_else_statement = instantiate_stmt_visitor_fun(instantiate_if_else_statement);
    NODECL_VISITOR(v)->visit_for_statement = instantiate_stmt_visitor_fun(instantiate_for_statement);
    NODECL_VISITOR(v)->visit_labeled_statement = instantiate_stmt_visitor_fun(instantiate_labeled_statement);
    NODECL_VISITOR(v)->visit_default_statement = instantiate_stmt_visitor_fun(instantiate_default_statement);
    NODECL_VISITOR(v)->visit_case_statement = instantiate_stmt_visitor_fun(instantiate_case_statement);
    NODECL_VISITOR(v)->visit_try_block = instantiate_stmt_visitor_fun(instantiate_try_block);
    NODECL_VISITOR(v)->visit_catch_handler = instantiate_stmt_visitor_fun(instantiate_catch_handler);
    NODECL_VISITOR(v)->visit_switch_statement = instantiate_stmt_visitor_fun(instantiate_switch_statement);
    NODECL_VISITOR(v)->visit_empty_statement = instantiate_stmt_visitor_fun(instantiate_empty_statement);
    NODECL_VISITOR(v)->visit_break_statement = instantiate_stmt_visitor_fun(instantiate_break_statement);
    NODECL_VISITOR(v)->visit_continue_statement = instantiate_stmt_visitor_fun(instantiate_continue_statement);
    NODECL_VISITOR(v)->visit_goto_statement = instantiate_stmt_visitor_fun(instantiate_goto_statement);

    NODECL_VISITOR(v)->visit_context = instantiate_stmt_visitor_fun(instantiate_context);

    NODECL_VISITOR(v)->visit_pragma_custom_statement = instantiate_stmt_visitor_fun(instantiate_pragma_custom_statement);
    NODECL_VISITOR(v)->visit_pragma_custom_declaration = instantiate_stmt_visitor_fun(instantiate_pragma_custom_declaration);
}

nodecl_t instantiate_statement(nodecl_t orig_tree,
        decl_context_t orig_decl_context,
        decl_context_t new_decl_context,
        instantiation_symbol_map_t* instantiation_symbol_map)
{
    nodecl_instantiate_stmt_visitor_t v;
    instantiate_stmt_init_visitor(&v,
            orig_decl_context,
            new_decl_context,
            instantiation_symbol_map,
            NULL, NULL);

    nodecl_t n = instantiate_stmt_walk(&v, orig_tree);

    return n;
}

nodecl_t instantiate_function_code(nodecl_t orig_tree,
        decl_context_t orig_decl_context,
        decl_context_t new_decl_context,
        scope_entry_t* orig_function_instantiated,
        scope_entry_t* new_function_instantiated,
        instantiation_symbol_map_t* instantiation_symbol_map)
{
    nodecl_instantiate_stmt_visitor_t v;
    instantiate_stmt_init_visitor(&v,
            orig_decl_context,
            new_decl_context,
            instantiation_symbol_map,
            orig_function_instantiated,
            new_function_instantiated
            );

    nodecl_t n = instantiate_stmt_walk(&v, orig_tree);

    return n;
}
