/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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
#include "cxx-koenig.h"
#include "cxx-codegen.h"
#include "cxx-placeholders.h"
#include "cxx-driver-utils.h"

#ifdef EXTRAE_ENABLED
#include "extrae_user_events.h"
#endif

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
        const decl_context_t* decl_context);

static void apply_attributes_to_type(type_t** type,
        AST attribute_seq,
        const decl_context_t* decl_context);

static void gather_virt_specifiers(AST a,
        gather_decl_spec_t* gather_info,
        const decl_context_t* decl_context);
static void gather_single_virt_specifier(AST item,
        gather_decl_spec_t* gather_info,
        const decl_context_t* decl_context);

static void build_scope_simple_declaration(AST a, const decl_context_t* decl_context, 
        char is_template, char is_explicit_specialization,
        nodecl_t *nodecl_output, 
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);

static scope_entry_t* build_scope_function_definition(
        AST function_definition,
        const decl_context_t* decl_context,
        char is_template,
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);

static void build_scope_namespace_alias(AST a, const decl_context_t* decl_context, nodecl_t *nodecl_output);
static void build_scope_namespace_definition(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output);
static void build_scope_declarator_with_parameter_context(AST a, 
        gather_decl_spec_t* gather_info, type_t* simple_type_info, type_t** declarator_type,
        const decl_context_t* decl_context, const decl_context_t* *prototype_context,
        nodecl_t* nodecl_output);

static void build_scope_member_specification(const decl_context_t* inner_decl_context, AST member_specification_tree, 
        access_specifier_t default_current_access, type_t* type_info, nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);

static void build_scope_member_declaration(const decl_context_t* inner_decl_context,
        AST a, access_specifier_t current_access, type_t* class_info,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);

static scope_entry_t* build_scope_member_function_definition(const decl_context_t* decl_context, AST a,
        access_specifier_t current_access,
        type_t* class_info,
        char is_template,
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);
static void build_scope_default_or_delete_member_function_definition(const decl_context_t* decl_context, 
        AST a, access_specifier_t current_access, type_t* class_info,
        char is_template,
        char is_explicit_specialization,
        nodecl_t* nodecl_output);
static void build_scope_member_simple_declaration(const decl_context_t* decl_context, AST a, 
        access_specifier_t current_access, type_t* class_info, 
        char is_template,
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);

static void common_gather_type_spec_from_simple_type_specifier(AST a, 
        const decl_context_t* decl_context, type_t** type_info,
        gather_decl_spec_t* gather_info, scope_entry_list_t* query_results);

static void gather_type_spec_from_simple_type_specifier(AST a, type_t** type_info,
        gather_decl_spec_t* gather_info, const decl_context_t* decl_context);

static void nodecl_gather_type_spec_from_simple_type_specifier(nodecl_t a, type_t** type_info,
        gather_decl_spec_t* gather_info, const decl_context_t* decl_context);

static void gather_type_spec_from_enum_specifier(AST a, type_t** type_info, 
        gather_decl_spec_t* gather_info,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output);
static void gather_type_spec_from_class_specifier(AST a, type_t** type_info,
        gather_decl_spec_t* gather_info,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output);
static void gather_type_spec_from_dependent_typename(AST a, 
        type_t** type_info,
        gather_decl_spec_t* gather_info,
        const decl_context_t* decl_context);

static void gather_type_spec_from_elaborated_friend_class_specifier(AST a,
        type_t** type_info,
        gather_decl_spec_t *gather_info,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output);

static void gather_type_spec_from_elaborated_class_specifier(AST a, 
        type_t** type_info,
        gather_decl_spec_t *gather_info,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output);
static void gather_type_spec_from_elaborated_enum_specifier(AST a, 
        type_t** type_info, 
        gather_decl_spec_t* gather_info, 
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output);

static void gather_extra_attributes_in_declarator(AST a, gather_decl_spec_t* gather_info, 
        const decl_context_t* declarator_context);
static void build_scope_declarator_rec(AST a,
                                       type_t **declarator_type,
                                       gather_decl_spec_t *gather_info,
                                       const decl_context_t *declarator_context,
                                       const decl_context_t *entity_context,
                                       const decl_context_t **prototype_context,
                                       char is_top_level_declarator,
                                       nodecl_t *nodecl_output);

static scope_entry_t* build_scope_declarator_name(AST declarator,
        type_t* type_specifier,
        type_t* declarator_type,
        gather_decl_spec_t* gather_info, const decl_context_t* decl_context);

static void build_scope_linkage_specifier(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output);
static void build_scope_linkage_specifier_declaration(AST a, 
        AST top_linkage_decl, 
        const decl_context_t* decl_context, 
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);

static void build_scope_template_declaration(AST a, 
        AST top_template_decl, 
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);
static void build_scope_explicit_template_specialization(AST a, 
        const decl_context_t* decl_context, 
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);

static void build_scope_statement_(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output);
static void build_scope_statement_seq(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output);

static void build_scope_template_parameter_list(AST a, 
        template_parameter_list_t* template_parameters,
        int nesting,
        const decl_context_t* template_context,
        nodecl_t* nodecl_output);
static void build_scope_template_parameter(AST a, 
        template_parameter_list_t* template_parameter_list, 
        int nesting,
        const decl_context_t* template_context,
        nodecl_t* nodecl_output);
static void build_scope_nontype_template_parameter(AST a,
        template_parameter_list_t* template_parameter_list,
        int nesting,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output);
static void build_scope_type_template_parameter(AST a,
        template_parameter_list_t* template_parameter_list,
        int nesting,
        char is_template_pack,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output);
static void build_scope_template_template_parameter(AST a,
        template_parameter_list_t* template_parameter_list,
        int nesting,
        char is_template_pack,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output);

static void build_scope_member_template_declaration(const decl_context_t* decl_context, AST a, 
        access_specifier_t current_access, type_t* class_info,
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);
static void build_scope_member_template_function_definition(const decl_context_t* decl_context,
        AST a, access_specifier_t current_access, type_t* class_info, 
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);

static void build_scope_member_template_simple_declaration(const decl_context_t* decl_context, AST a,
        access_specifier_t current_access, type_t* class_info,
        char is_explicit_specialization,
        nodecl_t* nodecl_output);

static void build_scope_static_assert(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_single_assert);

static void build_scope_simple_alias_declaration(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output);
static void build_scope_member_alias_declaration(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output,
        type_t* class_info, access_specifier_t access_specifier);

static void build_scope_template_alias_declaration(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output);
static void build_scope_member_template_alias_declaration(const decl_context_t* decl_context,
        AST a, access_specifier_t current_access, type_t* class_info,
        char is_explicit_specialization,
        nodecl_t* nodecl_output);
static void build_scope_default_or_delete_template_member_function_definition(
        const decl_context_t* decl_context, 
        AST a, access_specifier_t current_access, type_t* class_info,
        char is_explicit_specialization,
        nodecl_t* nodecl_output);

static void common_defaulted_or_deleted(AST a, const decl_context_t* decl_context, 
        void (*set)(scope_entry_t*, const decl_context_t*, const locus_t*),
        char is_template,
        char is_explicit_specialization,
        scope_entry_list_t** declared_symbols,
        nodecl_t* nodecl_output);

static void build_scope_defaulted_function_definition(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output);
static void build_scope_deleted_function_definition(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output);

static void build_scope_using_directive(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output);
static void build_scope_using_declaration(AST a, const decl_context_t* decl_context, access_specifier_t, 
        char is_typename,
        nodecl_t* nodecl_output);

static void build_scope_member_declaration_qualified(AST a, const decl_context_t* decl_context, access_specifier_t, nodecl_t* nodecl_output);

static void build_scope_template_function_definition(AST a, const decl_context_t* decl_context, 
        char is_explicit_specialization, 
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols, 
        gather_decl_spec_list_t* gather_decl_spec_list);

static void build_scope_template_deleted_function_definition(
        AST function_declaration,
        const decl_context_t* decl_context,
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);

static void build_scope_template_defaulted_function_definition(
        AST function_declaration,
        const decl_context_t* decl_context,
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);

static void build_scope_explicit_instantiation(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output);

static scope_entry_t* register_new_typedef_name(AST declarator_id, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, const decl_context_t* decl_context);
static scope_entry_t* register_new_var_or_fun_name(AST declarator_id, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, const decl_context_t* decl_context);
static scope_entry_t* register_function(AST declarator_id, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, const decl_context_t* decl_context);

static void build_scope_template_simple_declaration(AST a, const decl_context_t* decl_context, 
        char is_explicit_specialization, 
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);

static void build_scope_gcc_asm_definition(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output);
static void build_scope_asm_definition(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output);

static cv_qualifier_t compute_cv_qualifier(AST a);

static void build_exception_spec(type_t* function_type, AST a,
        gather_decl_spec_t *gather_info,
        const decl_context_t* decl_context,
        const decl_context_t* prototype_context,
        nodecl_t* nodecl_output);

static char find_function_declaration(AST declarator_id, 
        type_t* declarator_type, 
        gather_decl_spec_t* gather_info,
        const decl_context_t* decl_context,
        scope_entry_t** result_entry);

static void build_scope_pragma_custom_directive(AST a, const decl_context_t* decl_context, 
        nodecl_t* nodecl_output);
static void build_scope_pragma_custom_construct_declaration(AST a, 
        const decl_context_t* decl_context, 
        nodecl_t* nodecl_output);
static void build_scope_pragma_custom_construct_member_declaration(AST a, 
        const decl_context_t* decl_context, 
        access_specifier_t current_access,
        type_t* class_info,
        nodecl_t* nodecl_output);

static void call_destructors_of_classes(const decl_context_t* block_context, 
        const locus_t* locus,
        nodecl_t* nodecl_output);

typedef struct linkage_stack_tag { const char* name; char is_braced; } linkage_stack_t;

// Current linkage: NULL means the default linkage (if any) of the symbol
static linkage_stack_t _linkage_stack[MCXX_MAX_LINKAGE_NESTING] = { { NULL, 1 } };
static int _top_linkage_stack = 0;

// Extra declarations (VLA saved expressions mainly)
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
// --
static scope_entry_list_t* _instantiated_entries = NULL;

// This flag states if we are parsing a declaration of the form 'T D' where 'D'
// contains the form '... declarator-id' or is an abstract declarator that
// contains the form '...'
//
// This is needed for cases like
//     template <typename ...T> void g(T (...x)(int a));   [here D = (...x)(int a)]
//     template <typename ...T> void g(A<int(T* y)> ...x); [here D = ...x]
//
// Note that the following declaration involves three declarators ('g', 'p' and
// 'm'). This flag is only enabled for the declarator 'm'.
//
//     template <typename ...T> void g(void (*p)(T ...m))
static char _is_inside_pack_expansion = 0;

extern inline char get_is_inside_pack_expansion(void)
{
    return _is_inside_pack_expansion;
}

extern inline void set_is_inside_pack_expansion(char b)
{
    _is_inside_pack_expansion = b;
}

void push_instantiated_entity(scope_entry_t* entry)
{
    // if (!CURRENT_CONFIGURATION->explicit_instantiation)
    //     return;

    ERROR_CONDITION(entry->kind != SK_CLASS
            && entry->kind != SK_FUNCTION,
            "Invalid symbol", 0);

    _instantiated_entries = entry_list_add_once(
            _instantiated_entries,
            entry);
}

nodecl_t flush_instantiated_entities(void)
{
    // if (!CURRENT_CONFIGURATION->explicit_instantiation)
    //     return nodecl_null();

    nodecl_t nodecl_result = nodecl_null();

    scope_entry_list_iterator_t* it;
    for (it = entry_list_iterator_begin(_instantiated_entries);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        nodecl_t nodecl_instantiated = nodecl_make_cxx_implicit_instantiation(
                entry,
                // FIXME - Locus
                make_locus("", 0, 0));

        nodecl_result = nodecl_append_to_list(
                nodecl_result,
                nodecl_instantiated);
    }
    entry_list_iterator_free(it);
    entry_list_free(_instantiated_entries);
    _instantiated_entries = NULL;

    return nodecl_result;
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
        gather_decl_spec_t* gather_info, const decl_context_t* decl_context);

void initialize_translation_unit_scope(translation_unit_t* translation_unit, const decl_context_t** decl_context)
{
    *decl_context = new_global_context();

    // The global scope is created here
    translation_unit->global_decl_context = *decl_context;
}

void c_initialize_translation_unit_scope(translation_unit_t* translation_unit)
{
    const decl_context_t* decl_context;
    initialize_translation_unit_scope(translation_unit, &decl_context);
    c_initialize_builtin_symbols(decl_context);
}

#ifdef EXTRAE_ENABLED
enum { EXTRAE_DECLARATION_LOCUS = 6000019 + 100 };

static dhash_ptr_t* extrae_declaration_locus_value_set;

typedef
struct extrae_value_set_tag
{
    unsigned int num_values;

    const char** descriptions;
    extrae_value_t* values;
} extrae_value_set_t;

void extrae_declaration_locus_walk(const char* key,
        void *info UNUSED_PARAMETER,
        void *walk_info)
{
    extrae_value_set_t* extrae_value_set = (extrae_value_set_t*)walk_info;

    int n = extrae_value_set->num_values;
    P_LIST_ADD(extrae_value_set->descriptions, n, key);

    P_LIST_ADD(extrae_value_set->values, extrae_value_set->num_values, (extrae_value_t)key);
}
#endif

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
#ifdef EXTRAE_ENABLED
    extrae_declaration_locus_value_set = dhash_ptr_new(5);
#endif // EXTRAE_ENABLED
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

#ifdef EXTRAE_ENABLED
    extrae_value_set_t extrae_value_set;
    memset(&extrae_value_set, 0, sizeof(extrae_value_set));

    dhash_ptr_walk(extrae_declaration_locus_value_set, extrae_declaration_locus_walk, &extrae_value_set);
    dhash_ptr_destroy(extrae_declaration_locus_value_set);

    // void Extrae define event type (extrae type t *type, char *description, unsigned
    // *nvalues, extrae value t *values, char **description values)

    extrae_type_t v = EXTRAE_DECLARATION_LOCUS;
    const char* description = UNIQUESTR_LITERAL("Source declaration");
    Extrae_define_event_type(&v,
            (char*)description,
            &extrae_value_set.num_values,
            extrae_value_set.values,
            (char**)extrae_value_set.descriptions);

    DELETE(extrae_value_set.descriptions);
    DELETE(extrae_value_set.values);

#endif // EXTRAE_ENABLED
}

// Builds scope for the translation unit
nodecl_t build_scope_translation_unit(translation_unit_t* translation_unit)
{
    AST a = translation_unit->parsed_tree;
    const decl_context_t* decl_context = translation_unit->global_decl_context;

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

// This function initialize global symbols that exist in every translation unit
// prior to its translation
void c_initialize_builtin_symbols(const decl_context_t* decl_context)
{
    // __builtin_va_list is a very special type in GCC
    scope_entry_t* builtin_va_list;

    builtin_va_list = new_symbol(decl_context, decl_context->global_scope, uniquestr("__builtin_va_list"));
    builtin_va_list->kind = SK_TYPEDEF;
    builtin_va_list->defined = 1;
    builtin_va_list->type_information = get_gcc_builtin_va_list_type();
    builtin_va_list->do_not_print = 1;
    builtin_va_list->locus = make_locus("(global scope)", 0, 0);

    CXX_LANGUAGE()
    {
        {
            // Namespace std preexists
            scope_entry_t* namespace_std = new_symbol(decl_context, decl_context->global_scope, uniquestr("std"));
            namespace_std->kind = SK_NAMESPACE;
            symbol_entity_specs_set_is_user_declared(namespace_std, 1);

            const decl_context_t* namespace_std_context = new_namespace_context(decl_context, namespace_std);
            namespace_std->related_decl_context = namespace_std_context;
        }

        // There are two 'operator new' and two 'operator delete' at global scope
        {
            scope_entry_t* global_operator_new;
            global_operator_new = new_symbol(decl_context, decl_context->global_scope, uniquestr("operator new"));
            global_operator_new->kind = SK_FUNCTION;
            global_operator_new->do_not_print = 1;

            type_t* return_type = get_pointer_type(get_void_type());

            parameter_info_t parameter_info[1] = { 
                { .is_ellipsis = 0, .type_info = get_size_t_type() } 
            };

            global_operator_new->type_information = get_new_function_type(return_type, parameter_info, 1, REF_QUALIFIER_NONE);

            symbol_entity_specs_reserve_default_argument_info(global_operator_new, 1);

            global_operator_new->locus = make_locus("(global scope)", 0, 0);
        }
        // Version for arrays
        {
            scope_entry_t* global_operator_new;
            global_operator_new = new_symbol(decl_context, decl_context->global_scope, uniquestr("operator new[]"));
            global_operator_new->kind = SK_FUNCTION;
            global_operator_new->do_not_print = 1;

            type_t* return_type = get_pointer_type(get_void_type());

            parameter_info_t parameter_info[1] = { 
                { .is_ellipsis = 0, .type_info = get_size_t_type() } 
            };

            global_operator_new->type_information = get_new_function_type(return_type, parameter_info, 1, REF_QUALIFIER_NONE);
            symbol_entity_specs_reserve_default_argument_info(global_operator_new, 1);

            global_operator_new->locus = make_locus("(global scope)", 0, 0);
        }

        {
            scope_entry_t* global_operator_delete;
            global_operator_delete = new_symbol(decl_context, decl_context->global_scope, uniquestr("operator delete"));
            global_operator_delete->kind = SK_FUNCTION;
            global_operator_delete->do_not_print = 1;

            type_t* return_type = get_void_type();

            parameter_info_t parameter_info[1] = { 
                { .is_ellipsis = 0, .type_info = get_pointer_type(get_void_type()) } 
            };

            global_operator_delete->type_information = get_new_function_type(return_type, parameter_info, 1, REF_QUALIFIER_NONE);
            symbol_entity_specs_reserve_default_argument_info(global_operator_delete, 1);

            global_operator_delete->locus = make_locus("(global scope)", 0, 0);
        }
        {
            scope_entry_t* global_operator_delete;
            global_operator_delete = new_symbol(decl_context, decl_context->global_scope, uniquestr("operator delete[]"));
            global_operator_delete->kind = SK_FUNCTION;
            global_operator_delete->do_not_print = 1;

            type_t* return_type = get_void_type();

            parameter_info_t parameter_info[1] = { 
                { .is_ellipsis = 0, .type_info = get_pointer_type(get_void_type()) } 
            };

            global_operator_delete->type_information = get_new_function_type(return_type, parameter_info, 1, REF_QUALIFIER_NONE);
            symbol_entity_specs_reserve_default_argument_info(global_operator_delete, 1);

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
        scope_entry_t* __int128_t_type = new_symbol(decl_context, decl_context->global_scope, uniquestr("__int128_t"));
        __int128_t_type->kind = SK_TYPEDEF;
        __int128_t_type->type_information = get_signed_int128_type();
        __int128_t_type->locus = make_locus("(global scope)", 0, 0);
    }
    {
        scope_entry_t* __uint128_t_type = new_symbol(decl_context, decl_context->global_scope, uniquestr("__uint128_t"));
        __uint128_t_type->kind = SK_TYPEDEF;
        __uint128_t_type->type_information = get_unsigned_int128_type();
        __uint128_t_type->locus = make_locus("(global scope)", 0, 0);
    }
#endif
    // Mercurium basic types
    struct {
        const char* type_name;
        type_t* related_type;
    } mercurium_basic_types[] = {
        { "mercurium_size_t", get_size_t_type() },
        { "mercurium_ptrdiff_t", get_ptrdiff_t_type() },
        { NULL, NULL }
    };
    int i;
    for (i = 0; mercurium_basic_types[i].type_name != NULL; i++)
    {
        scope_entry_t* typedef_sym = new_symbol(decl_context, decl_context->global_scope,
                mercurium_basic_types[i].type_name);
        typedef_sym->kind = SK_TYPEDEF;
        typedef_sym->type_information = mercurium_basic_types[i].related_type;
        typedef_sym->locus = make_locus("(global scope)", 0, 0);
        symbol_entity_specs_set_is_user_declared(typedef_sym, 1);
    }

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

        scope_entry_t* max_sym = new_symbol(decl_context, decl_context->global_scope, 
                strappend(base_name, "_max"));
        max_sym->kind = SK_VARIABLE;
        max_sym->type_information = get_const_qualified_type(current_type);
        max_sym->locus = make_locus("(global scope)", 0, 0);
        max_sym->value = const_value_to_nodecl(value_max);
        symbol_entity_specs_set_is_user_declared(max_sym, 1);

        scope_entry_t* min_sym = new_symbol(decl_context, decl_context->global_scope, 
                strappend(base_name, "_min"));
        min_sym->kind = SK_VARIABLE;
        min_sym->type_information = get_const_qualified_type(current_type);
        min_sym->locus = make_locus("(global scope)", 0, 0);
        min_sym->value = const_value_to_nodecl(value_min);
        symbol_entity_specs_set_is_user_declared(min_sym, 1);
    }
}

void build_scope_declaration_sequence(AST list, 
        const decl_context_t* decl_context, 
        nodecl_t* nodecl_output_list)
{
    AST iter;
    for_each_element(list, iter)
    {
        nodecl_t current_nodecl_output_list = nodecl_null();
        build_scope_declaration(ASTSon1(iter), decl_context, &current_nodecl_output_list, 
                /* declared_symbols */ NULL, /* gather_decl_spec_list_t */ NULL);

        current_nodecl_output_list = nodecl_concat_lists(flush_instantiated_entities(),
                current_nodecl_output_list);

        *nodecl_output_list = nodecl_concat_lists(*nodecl_output_list, current_nodecl_output_list);
    }
}

// We need to keep some state here
static char gcc_extension = 0;

// Build scope for a declaration
void build_scope_declaration(AST a, const decl_context_t* decl_context, 
        nodecl_t* nodecl_output, 
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t *gather_decl_spec_list)
{
    // NOTE: if nodecl_output is not nodecl_null it should return a list
    DEBUG_CODE()
    {
        fprintf(stderr, "==== Declaration line [%s] ====\n", ast_location(a));
    }

#ifdef EXTRAE_ENABLED
    Extrae_user_function(1);
    Extrae_event (EXTRAE_DECLARATION_LOCUS,
            (extrae_value_t)ast_location(a));

    dhash_ptr_insert(extrae_declaration_locus_value_set, ast_location(a), (void*)ast_location(a));
#endif

    diagnostic_context_push_buffered();

    switch (ASTKind(a))
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
                if (ASTKind(a) == AST_USING_DECLARATION_TYPENAME)
                    error_printf_at(ast_get_locus(a), "'using typename' is only valid in member-declarations\n");

                build_scope_using_declaration(a, decl_context, AS_UNKNOWN,
                        /* is_typename */ ASTKind(a) == AST_USING_DECLARATION_TYPENAME,
                        nodecl_output);
                break;
            }
        case AST_STATIC_ASSERT:
            {
                nodecl_t nodecl_single_assert = nodecl_null();
                build_scope_static_assert(a, decl_context, &nodecl_single_assert);

                if (!nodecl_is_null(nodecl_single_assert)
                        && nodecl_get_kind(nodecl_single_assert) == NODECL_CXX_STATIC_ASSERT)
                {
                    *nodecl_output = nodecl_make_list_1(nodecl_single_assert);
                }
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
                if (ASTKind(a) == AST_AMBIGUITY)
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
                        ast_print_node_type(ASTKind(a)), ast_location(a));
                break;
            }
    }

    diagnostic_context_pop_and_commit();

#ifdef EXTRAE_ENABLED
    Extrae_event (EXTRAE_DECLARATION_LOCUS, 0);
    Extrae_user_function(0);
#endif
}

static void build_scope_asm_definition(AST a, 
        const decl_context_t* decl_context UNUSED_PARAMETER, 
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

static void build_scope_gcc_asm_definition(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output)
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

                if (ASTKind(asm_operand) == AST_GCC_ASM_OPERAND)
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
                    internal_error("Unexpected tree '%s'\n", ast_print_node_type(ASTKind(asm_operand)));
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

static void keep_std_attributes_in_symbol(scope_entry_t* entry,
        gather_decl_spec_t* gather_info)
{
    if (nodecl_is_null(gather_info->alignas_list))
        return;

    char is_dependent = 0;

    int n = 0;
    nodecl_t* list = nodecl_unpack_list(gather_info->alignas_list, &n);
    int i;
    for (i = 0; i < n && !is_dependent; i++)
    {
        if (nodecl_expr_is_value_dependent(list[i])
                || nodecl_expr_is_type_dependent(list[i]))
        {
            is_dependent = 1;
        }
    }

    if (!is_dependent)
    {
        // Compute the largest
        nodecl_t max_expr = list[0];
        const_value_t* max_value = nodecl_get_constant(list[0]);
        ERROR_CONDITION(max_value == NULL, "Expecting a constant here", 0);
        for (i = 1; i < n; i++)
        {
            const_value_t* current_value = nodecl_get_constant(list[i]);
            ERROR_CONDITION(current_value == NULL, "Expecting a constant here", 0);

            if (const_value_is_nonzero(const_value_gt(current_value, max_value)))
            {
                max_expr = list[i];
            }
        }

        symbol_entity_specs_set_alignas_value(entry, nodecl_shallow_copy(max_expr));
    }
    else
    {
        nodecl_t dep_alignas = 
            nodecl_make_cxx_alignas(
                    gather_info->alignas_list,
                    get_size_t_type(),
                    nodecl_get_locus(gather_info->alignas_list));
        nodecl_expr_set_is_value_dependent(dep_alignas, 1);

        symbol_entity_specs_set_alignas_value(entry, dep_alignas);
    }

    xfree(list);
}

static void keep_extra_attributes_in_symbol(scope_entry_t* entry, gather_decl_spec_t *gather_info)
{
    keep_std_attributes_in_symbol(entry, gather_info);
    keep_gcc_attributes_in_symbol(entry, gather_info);
    keep_ms_declspecs_in_symbol(entry, gather_info);

    if (gather_info->is_mcc_hidden)
    {
        entry->do_not_print = 1;
        symbol_entity_specs_set_is_user_declared(entry, 0);
    }
}

static void build_scope_explicit_instantiation(AST a,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    // There are two forms of explicit instantiation: an explicit instantiation
    // definition and an explicit instantiation declaration. An explicit
    // instantiation declaration begins with the 'extern' keyword.
    char is_expl_inst_decl = 0;
    AST class_or_function_specifier = ASTSon0(a);
    if (class_or_function_specifier != NULL)
    {
        if (ASTKind(class_or_function_specifier) == AST_EXTERN_SPEC)
        {
            is_expl_inst_decl = 1;
        }
        else
        {
            error_printf_at(ast_get_locus(a), "invalid specifier '%s' in an explicit instantiation\n", prettyprint_in_buffer(class_or_function_specifier));
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

    type_t* declarator_type = NULL;
    compute_declarator_type(declarator, &gather_info, simple_type_info,
            &declarator_type, decl_context, nodecl_output);

    nodecl_t declarator_name_opt = nodecl_null();
    scope_entry_t* entry = NULL;
    if (declarator != NULL)
    {
        entry = build_scope_declarator_name(declarator,
                simple_type_info,
                declarator_type,
                &gather_info, decl_context);

        keep_extra_attributes_in_symbol(entry, &gather_info);

        AST id_expr = get_declarator_id_expression(declarator, decl_context);
        compute_nodecl_name_from_id_expression(ASTSon0(id_expr), decl_context, &declarator_name_opt);
        // We do this to fix the declarator_name_opt
        query_nodecl_name_flags(decl_context, declarator_name_opt, NULL, DF_DEPENDENT_TYPENAME);

        if (entry == NULL
                || (entry->kind != SK_FUNCTION
                    && !(entry->kind == SK_VARIABLE
                        && symbol_entity_specs_get_is_member(entry)
                        && symbol_entity_specs_get_is_static(entry))))
        {
            error_printf_at(ast_get_locus(a), "invalid explicit instantiation of '%s %s'\n",
                    prettyprint_in_buffer(decl_specifier_seq),
                    prettyprint_in_buffer(declarator));
            return;
        }

        if (entry->kind == SK_FUNCTION
                && CURRENT_CONFIGURATION->explicit_instantiation)
        {
            symbol_entity_specs_set_is_instantiated(entry, 1);
        }
    }
    else
    {
        if (is_named_type(declarator_type)
                && named_type_get_symbol(declarator_type)->kind == SK_CLASS)
        {
            entry = named_type_get_symbol(declarator_type);

            if (CURRENT_CONFIGURATION->explicit_instantiation)
            {
                instantiate_template_class_if_needed(entry, decl_context, ast_get_locus(a));
                scope_entry_list_t* members = class_type_get_members(entry->type_information);

                if (is_expl_inst_decl)
                {
                    scope_entry_list_iterator_t* it;
                    for (it = entry_list_iterator_begin(members);
                            !entry_list_iterator_end(it);
                            entry_list_iterator_next(it))
                    {
                        scope_entry_t* current_member = entry_list_iterator_current(it);

                        if (current_member->kind == SK_FUNCTION)
                        {
                            symbol_entity_specs_set_is_instantiable(current_member, 0);
                        }
                    }
                }
            }
        }
        else
        {
            error_printf_at(ast_get_locus(a), "declaration should declare a class\n");
            return;
        }
    }

    if (CURRENT_CONFIGURATION->explicit_instantiation)
    {
        nodecl_t nodecl_list_of_instantiated = flush_instantiated_entities();
        nodecl_free(nodecl_list_of_instantiated);
    }

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
                nodecl_make_cxx_explicit_instantiation_decl(
                    declarator_name_opt,
                    nodecl_context,
                    entry,
                    ast_get_locus(a)))
        :
        nodecl_make_list_1(
                nodecl_make_cxx_explicit_instantiation_def(
                    declarator_name_opt,
                    nodecl_context,
                    entry,
                    ast_get_locus(a)));
}

static void check_nodecl_using_directive(nodecl_t nodecl_name,
        const decl_context_t* decl_context,
        char turn_into_inline,
        nodecl_t* nodecl_output)
{
    scope_entry_list_t* result_list = query_nodecl_name(decl_context, nodecl_name, NULL);

    if (result_list == NULL)
    {
        error_printf_at(nodecl_get_locus(nodecl_name), "unknown namespace '%s'\n", 
                codegen_to_str(nodecl_name, decl_context));
        return;
    }

    if (entry_list_size(result_list) > 1
            || entry_list_head(result_list)->kind != SK_NAMESPACE)
    {
        error_printf_at(nodecl_get_locus(nodecl_name), "'%s' does not name a namespace\n", 
                codegen_to_str(nodecl_name, decl_context));
        return;
    }

    scope_entry_t* entry = entry_list_head(result_list);

    entry_list_free(result_list);

    symbol_entity_specs_set_is_inline(entry, turn_into_inline);

    // Now add this namespace to the used namespaces of this scope
    scope_t* namespace_scope = decl_context->current_scope;

    ERROR_CONDITION(entry->related_decl_context->current_scope->kind != NAMESPACE_SCOPE,
            "Error, related scope is not namespace scope", 0);

    P_LIST_ADD_ONCE(namespace_scope->use_namespace,
            namespace_scope->num_used_namespaces,
            entry);

    nodecl_t cxx_using_namespace =
        nodecl_make_cxx_using_namespace(
                nodecl_make_context(
                    /* optional statement sequence */ nodecl_null(),
                    decl_context,
                    nodecl_get_locus(nodecl_name)),
                nodecl_name,
                entry,
                nodecl_get_locus(nodecl_name));

    *nodecl_output =
        nodecl_make_list_1(cxx_using_namespace);
}

static void build_scope_using_directive(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output UNUSED_PARAMETER)
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

    nodecl_t nodecl_name = nodecl_null();
    compute_nodecl_name_from_id_expression(id_expression, decl_context, &nodecl_name);
    if (nodecl_is_err_expr(nodecl_name))
        return;

    check_nodecl_using_directive(nodecl_name, decl_context, turn_into_inline, nodecl_output);
}

void introduce_using_entities_in_class(
        nodecl_t nodecl_name,
        scope_entry_list_t* used_entities,
        const decl_context_t* decl_context,
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

        already_using = entry_list_add_once(already_using, symbol_entity_specs_get_alias_to(current_using));
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
        if (symbol_entity_specs_get_is_injected_class_name(entry))
        {
            entry = named_type_get_symbol(symbol_entity_specs_get_class_type(entry));
        }

        symbol_name = entry->symbol_name;

        char is_hidden = 0;

        if (entry->kind == SK_CLASS
                && class_type_is_base_instantiating(
                    get_user_defined_type(entry),
                    get_user_defined_type(current_class),
                    locus))
        {
            // Inheriting constructors
            CXX03_LANGUAGE()
            {
                warn_printf_at(locus, "inheriting constructors is valid only in C++11\n");
            }
            class_type_add_inherited_constructor(current_class->type_information, entry);

            // We are done since finish_class_type_cxx will do the rest
            entry_list_iterator_free(it);
            entry_list_free(already_using);
            return;
        }
        else if (entry->kind == SK_DEPENDENT_ENTITY)
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
        else if (!symbol_entity_specs_get_is_member(entry))
        {
            error_printf_at(locus, "'%s' is not a member of a base class\n",
                    get_qualified_symbol_name(entry, 
                        decl_context));
        }
        else
        {
            if (!is_dependent_type(get_user_defined_type(current_class))
                && !class_type_is_base_instantiating(symbol_entity_specs_get_class_type(entry),
                    get_user_defined_type(current_class), locus))
            {
                error_printf_at(locus, "'%s' is not a member of a base class\n",
                        get_qualified_symbol_name(entry, 
                            decl_context));
            }
            else
            {
                // FIXME - We could check that there is at least one dependent base
                // If there are no dependent bases, then we should check with each
                // But let this fail at instantiation time, instead
            }

            // Usual case
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

        if (is_hidden)
            continue;

        // Do not add it twice in the scope
        if (entry_list_contains(already_using, entry))
            continue;

        scope_entry_t* used_name = new_symbol(decl_context, decl_context->current_scope, symbol_name);
        used_name->kind = !is_typename ? SK_USING : SK_USING_TYPENAME;
        used_name->locus = locus;
        symbol_entity_specs_set_alias_to(used_name, entry);

        symbol_entity_specs_set_is_member(used_name, 1);
        symbol_entity_specs_set_class_type(used_name, get_user_defined_type(current_class));
        symbol_entity_specs_set_access(used_name, current_access);

        insert_entry(decl_context->current_scope, used_name);
    }
    entry_list_iterator_free(it);
    entry_list_free(already_using);

    scope_entry_t* used_hub_symbol = NEW0(scope_entry_t);
    used_hub_symbol->kind = !is_typename ? SK_USING : SK_USING_TYPENAME;
    used_hub_symbol->type_information = get_unresolved_overloaded_type(used_entities, NULL);
    symbol_entity_specs_set_access(used_hub_symbol, current_access);
    used_hub_symbol->locus = locus;

    class_type_add_member(current_class->type_information,
            used_hub_symbol,
            decl_context,
            /* is_definition */ 1);
}

// This is for using found in non class scope
static void introduce_using_entities(
        scope_entry_list_t* used_entities,
        const decl_context_t* decl_context,
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

        already_using = entry_list_add_once(already_using, symbol_entity_specs_get_alias_to(current_using));
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
            original_entry = symbol_entity_specs_get_alias_to(original_entry);
        }

        // Do not add it twice in the scope
        if (entry_list_contains(already_using, original_entry))
            continue;

        scope_entry_t* used_name = new_symbol(decl_context, decl_context->current_scope, symbol_name);
        used_name->kind = !is_typename ? SK_USING : SK_USING_TYPENAME;
        used_name->locus = locus;
        symbol_entity_specs_set_alias_to(used_name, original_entry);

        insert_entry(decl_context->current_scope, used_name);
    }
    entry_list_iterator_free(it);

    entry_list_free(already_using);
}

static void introduce_using_entity_nodecl_name(nodecl_t nodecl_name,
        const decl_context_t* decl_context,
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
        error_printf_at(nodecl_get_locus(nodecl_name), "entity '%s' in using-declaration is unknown\n",
                codegen_to_str(nodecl_name, decl_context));
        return;
    }

    scope_entry_t* current_class = NULL;
    char is_class_scope = 0;
    if (decl_context->current_scope->kind == CLASS_SCOPE)
    {
        current_class = decl_context->current_scope->related_entry;
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
                        nodecl_name,
                        entry,
                        nodecl_get_locus(nodecl_name)));
    }

    entry_list_free(used_entities);
}

static void build_scope_using_declaration(AST a, const decl_context_t* decl_context,
        access_specifier_t current_access, char is_typename, nodecl_t* nodecl_output)
{
    AST id_expression = ASTSon0(a);

    if (decl_context->current_scope->kind != CLASS_SCOPE
            && decl_context->current_scope->kind != NAMESPACE_SCOPE
            && decl_context->current_scope->kind != BLOCK_SCOPE)
    {
        error_printf_at(ast_get_locus(a), "using-declaration not in a class, namespace or block scope\n");
        return;
    }

    nodecl_t nodecl_name = nodecl_null();
    compute_nodecl_name_from_id_expression(id_expression, decl_context, &nodecl_name);

    if (nodecl_is_err_expr(nodecl_name))
        return;

    introduce_using_entity_nodecl_name(nodecl_name, decl_context, current_access, is_typename, nodecl_output);
}

static void build_scope_member_declaration_qualified(AST a, const decl_context_t* decl_context,
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

void build_scope_nodecl_static_assert(nodecl_t nodecl_predicate,
        nodecl_t nodecl_message,
        const decl_context_t* decl_context,
        nodecl_t *nodecl_single_assert)
{
    check_contextual_conversion(
            nodecl_predicate,
            get_bool_type(),
            decl_context,
            &nodecl_predicate);
    if (nodecl_is_err_expr(nodecl_predicate))
    {
        *nodecl_single_assert = nodecl_make_err_statement(nodecl_get_locus(nodecl_predicate));
    }

    if (!nodecl_expr_is_value_dependent(nodecl_predicate))
    {
        if (!nodecl_is_constant(nodecl_predicate))
        {
            error_printf_at(nodecl_get_locus(nodecl_predicate), "static assertion expression is not constant\n");
        }
        else
        {
            const_value_t * val = nodecl_get_constant(nodecl_predicate);

            if (const_value_is_zero(val))
            {
                if (!nodecl_is_null(nodecl_message))
                    error_printf_at(nodecl_get_locus(nodecl_predicate), "static assertion failed: %s\n",
                            codegen_to_str(nodecl_message, decl_context));
                else
                    error_printf_at(nodecl_get_locus(nodecl_predicate), "static assertion failed\n");

                *nodecl_single_assert = nodecl_make_err_statement(nodecl_get_locus(nodecl_predicate));
            }
        }

        *nodecl_single_assert = nodecl_null();
    }
    else
    {
        *nodecl_single_assert = 
            nodecl_make_cxx_static_assert(
                    nodecl_predicate,
                    nodecl_message,
                    nodecl_get_locus(nodecl_predicate));
    }
}

static void build_scope_static_assert(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_single_assert)
{
    AST constant_expr = ASTSon0(a);
    AST message = ASTSon1(a);

    if (message == NULL)
    {
        if (!IS_CXX14_LANGUAGE)
        {
            warn_printf_at(ast_get_locus(a), "static_assert without message is a C++14 feature\n");
        }
    }

    nodecl_t nodecl_predicate = nodecl_null();
    nodecl_t nodecl_message = nodecl_null();
    if (!check_expression_must_be_constant(constant_expr, decl_context, &nodecl_predicate)
            || (message != NULL 
                && !check_expression(message, decl_context, &nodecl_message)))
    {
        error_printf_at(ast_get_locus(a), "static assertion expression is invalid\n");
        *nodecl_single_assert = nodecl_make_err_statement(ast_get_locus(a));
        return;
    }

    build_scope_nodecl_static_assert(nodecl_predicate, nodecl_message, decl_context, nodecl_single_assert);
}


static void build_scope_common_template_alias_declaration(AST a,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output,
        char is_member_declaration,
        type_t* class_info,
        access_specifier_t access_specifier,
        char is_explicit_specialization)
{
    ERROR_CONDITION(decl_context->template_parameters == NULL,
            "There must be template parameters", 0);

    if (IS_CXX03_LANGUAGE)
    {
        warn_printf_at(ast_get_locus(a), "template-alias are only valid in C++11\n");
    }

    if (is_explicit_specialization)
    {
        error_printf_at(ast_get_locus(a), "invalid alias-declaration in explicit template specialization\n");
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
            error_printf_at(ast_get_locus(identifier), "symbol '%s' has been redeclared as a different symbol kind\n",
                    ASTText(identifier));
        }
        else
        {
            error_printf_at(ast_get_locus(identifier), "alias template '%s' has already been defined\n",
                    ASTText(identifier));
        }

        info_printf_at(entry->locus, "previous declaration of '%s'\n",
                entry->symbol_name);
        return;
    }
    else
    {
        entry = new_symbol(decl_context, decl_context->current_scope, ASTText(identifier));
        entry->kind = SK_TEMPLATE;
        entry->type_information = get_new_template_alias_type(
                decl_context->template_parameters,
                aliased_type,
                ASTText(identifier),
                decl_context,
                ast_get_locus(identifier));

        symbol_entity_specs_set_is_user_declared(entry, 1);
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
        symbol_entity_specs_set_is_member(entry, 1);
        symbol_entity_specs_set_class_type(entry, class_info);
        symbol_entity_specs_set_access(entry, access_specifier);

        scope_entry_t* primary_symbol = named_type_get_symbol(template_type_get_primary_type(entry->type_information));
        symbol_entity_specs_set_is_member(primary_symbol, 1);
        symbol_entity_specs_set_class_type(primary_symbol, class_info);
        symbol_entity_specs_set_access(primary_symbol, access_specifier);

        class_type_add_member(class_info, primary_symbol, decl_context, /* is_definition */ 1);
    }
}

static void build_scope_template_alias_declaration(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    build_scope_common_template_alias_declaration(a, decl_context, nodecl_output,
            /* is_member_declaration */ 0, NULL, AS_UNKNOWN, 0);
}

static void build_scope_nontemplate_alias_declaration(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output,
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
        symbol_entity_specs_set_is_member(entry, 1);
        symbol_entity_specs_set_class_type(entry, class_info);
        symbol_entity_specs_set_access(entry, access_specifier);
        class_type_add_member(class_info, entry, decl_context, /* is_definition */ 1);
    }
}

static void build_scope_simple_alias_declaration(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    build_scope_nontemplate_alias_declaration(a, decl_context, nodecl_output,
            /* is_member_declaration */ 0, NULL, AS_UNKNOWN);
}

static void build_scope_member_alias_declaration(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output,
        type_t* class_info, access_specifier_t access_specifier)
{
    build_scope_nontemplate_alias_declaration(a, decl_context, nodecl_output,
            /* is_member_declaration */ 1, class_info, access_specifier);
}

static void copy_gather_info(gather_decl_spec_t* dest, gather_decl_spec_t* src)
{
    *dest = *src;

#define COPY_ARRAY(dest, orig, num_items) \
    (dest) = NEW_VEC(__typeof__(*(dest)), (num_items)); \
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
        if (symbol_entity_specs_get_is_saved_expression(extra_decl_symbol))
        {
            result = nodecl_append_to_list(
                    result,
                    nodecl_make_object_init(
                        extra_decl_symbol,
                        loc));
        }
        else if (IS_CXX_LANGUAGE
                && (extra_decl_symbol->kind == SK_CLASS
                    || extra_decl_symbol->kind == SK_ENUM))
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
        else if (extra_decl_symbol->kind == SK_FUNCTION)
        {
            nodecl_t function_code = symbol_entity_specs_get_function_code(extra_decl_symbol);
            if (!nodecl_is_null(function_code))
            {
                ERROR_CONDITION(!nodecl_is_null(nodecl_get_parent(function_code)),
                        "This function code seems rooted elsewhere", 0);
                result = nodecl_append_to_list(
                        result,
                        function_code);
            }
        }
        else
        {
            internal_error("Unhandled extra declared symbol '%s' %s",
                    extra_decl_symbol->symbol_name,
                    symbol_kind_name(extra_decl_symbol));
        }

        extra_decl_symbol = pop_extra_declaration_symbol();
    }

    return result;
}

// Builds scope for a simple declaration
static void build_scope_simple_declaration(AST a, const decl_context_t* decl_context,
        char is_template, char is_explicit_specialization,
        nodecl_t *nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list)
{
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
            warn_printf_at(ast_get_locus(a), "declaration does not have a decl-specifier, assuming 'int'\n");

            simple_type_info = get_signed_int_type();
        }
    }

    if (gather_info.is_friend)
    {
        error_printf_at(ast_get_locus(a), "friend specifier is not allowed here\n");
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

            if (ASTKind(init_declarator) == AST_AMBIGUITY)
            {
                solve_ambiguous_init_declarator(init_declarator, decl_context, &current_gather_info);
            }

            const decl_context_t* current_decl_context = decl_context;

            ERROR_CONDITION(ASTKind(init_declarator) != AST_INIT_DECLARATOR,
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

            scope_entry_t *entry = build_scope_declarator_name(declarator,
                    simple_type_info, declarator_type,
                    &current_gather_info, current_decl_context);

            // Something is wrong
            if (entry == NULL)
                continue;

            if (symbol_entity_specs_get_is_constructor(entry))
            {
                error_printf_at(ast_get_locus(a), "declaration of a constructor not valid in this scope\n");
                continue;
            }

            if (symbol_entity_specs_get_is_conversion(entry))
            {
                error_printf_at(ast_get_locus(a), "declaration of a conversion function not valid in this scope\n");
                continue;
            }

            if (symbol_entity_specs_get_is_destructor(entry))
            {
                error_printf_at(ast_get_locus(a), "declaration of a destructor not valid in this scope\n");
                continue;
            }

            // Note that in C99 we craft a signed int here internally, so this
            // can only happen in C++
            if (simple_type_info == NULL)
            {
                error_printf_at(ast_get_locus(a), "declaration of '%s' lacks a type-specifier\n",
                        get_qualified_symbol_name(entry, current_decl_context));
                continue;
            }

            if (symbol_entity_specs_get_is_member(entry))
            {
                if (entry->kind != SK_VARIABLE
                        || !symbol_entity_specs_get_is_static(entry))
                {
                    error_printf_at(ast_get_locus(a), "declaration of member '%s' not valid in this scope\n",
                            get_qualified_symbol_name(entry, current_decl_context));
                    continue;
                }
            }

            if (!symbol_entity_specs_get_is_member(entry)
                    && linkage_current_get_name() != NULL
                    && !linkage_current_is_braced())
            {
                // extern "C" int x;
                //   is like
                // extern "C" extern int x;
                current_gather_info.is_extern = 1;
            }

            if (current_gather_info.is_transparent_union)
            {
                set_is_transparent_union(entry->type_information, /* is_transparent_union */ 1);
            }

            keep_extra_attributes_in_symbol(entry, &current_gather_info);

            // Propagate the __extension__ attribute to the symbol
            symbol_entity_specs_set_gcc_extension(entry, gcc_extension);

            // Only variables can be initialized
            if (initializer != NULL)
            {
                if (entry->kind == SK_VARIABLE)
                {
                    if (current_gather_info.is_extern)
                    {
                        if (decl_context->current_scope->kind != NAMESPACE_SCOPE)
                        {
                            error_printf_at(ast_get_locus(a), "cannot initialize an 'extern' declaration\n");
                        }
                        else if (!IS_CXX_LANGUAGE
                                || !current_gather_info.is_const)
                        {
                            // In C++ initializing a const variable is OK
                            warn_printf_at(ast_get_locus(a), "initializing an 'extern' declaration\n");
                        }
                    }
                }
                else if (entry->kind == SK_TYPEDEF)
                {
                    error_printf_at(ast_get_locus(a), "cannot initialize a typedef\n");
                }
                else
                {
                    internal_error("Code unreachable", 0);
                }
            }

            if (initializer == NULL
                    && current_gather_info.is_auto_type)
            {
                error_printf_at(ast_get_locus(a), "declaration with auto type-specifier requires an initializer\n");
            }

            if (entry->kind == SK_FUNCTION)
            {
                if (current_decl_context->current_scope->kind == BLOCK_SCOPE
                        && !symbol_entity_specs_get_is_nested_function(entry))
                {
                    // Ensure that the symbol is marked as extern
                    symbol_entity_specs_set_is_extern(entry, 1);
                }

                CXX11_LANGUAGE()
                {
                    if ((function_type_get_ref_qualifier(entry->type_information) != REF_QUALIFIER_NONE)
                            && (!symbol_entity_specs_get_is_member(entry)
                                || symbol_entity_specs_get_is_static(entry)))
                    {
                        // No member function can reach here, so this is wrong
                        error_printf_at(ast_get_locus(a), "only nonstatic member functions may have ref-qualifier\n");
                    }
                }
            }

            if (entry->kind == SK_VARIABLE
                    || entry->kind == SK_TYPEDEF)
            {
                if (current_decl_context->current_scope->kind == BLOCK_SCOPE)
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
                    DELETE(current_gather_info.vla_dimension_symbols);
                    current_gather_info.vla_dimension_symbols = NULL;

                }
            }

            if (entry->kind == SK_VARIABLE)
            {
                if (entry->defined
                        && !BITMAP_TEST(current_decl_context->decl_flags, DF_ALLOW_REDEFINITION)
                        && !current_gather_info.is_extern
                        // In C, an entity may be redefined at file-scope
                        && !(IS_C_LANGUAGE
                            && (entry->decl_context->current_scope == entry->decl_context->global_scope)
                            && (nodecl_is_null(entry->value)
                                || initializer == NULL)))
                {
                    error_printf_at(ast_get_locus(declarator), "redefined entity '%s', first declared in '%s'\n",
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
                if (symbol_entity_specs_get_is_extern(entry)
                        && entry->decl_context->current_scope != entry->decl_context->global_scope
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
                            error_printf_at(ast_get_locus(declarator), "extern entity redeclared as a different entity kind\n");
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
                        fprintf(stderr, "BUILDSCOPE: Initializer: '%s'\n", ast_print_node_type(ASTKind(initializer)));
                    }

                    char init_check = check_initialization(initializer,
                            entry->decl_context,
                            entry,
                            get_unqualified_type(declarator_type),
                            &nodecl_initializer,
                            current_gather_info.is_auto_type,
                            current_gather_info.is_decltype_auto);

                    // Update unbounded arrays, bounded by their initialization
                    if (init_check)
                    {
                        type_t* initializer_type = nodecl_get_type(nodecl_initializer);
                        ERROR_CONDITION(initializer_type == NULL, "Missing type", 0);

                        if (is_array_type(declarator_type)
                                && !is_dependent_type(declarator_type)
                                && nodecl_is_null(array_type_get_array_size_expr(declarator_type)))
                        {
                            if (!is_dependent_type(initializer_type))
                            {
                                ERROR_CONDITION(is_braced_list_type(initializer_type),
                                        "Invalid type", 0);
                                cv_qualifier_t cv_qualif = get_cv_qualifier(entry->type_information);
                                entry->type_information = get_cv_qualified_type(
                                        clear_special_expr_type_variants(no_ref(initializer_type)),
                                        cv_qualif);
                            }
                            else
                            {
                                entry->type_information = get_array_type_unknown_size_dependent(
                                        array_type_get_element_type(entry->type_information));
                            }
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
                    if (!symbol_entity_specs_get_is_member(entry) // Not a member
                            || ( // Static member definition (outside of the class)
                                symbol_entity_specs_get_is_static(entry)  
                                && current_decl_context->current_scope->kind == NAMESPACE_SCOPE))
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
                            && current_decl_context->current_scope->kind == BLOCK_SCOPE
                            && is_variably_modified_type(entry->type_information))
                        || (IS_CXX_LANGUAGE
                            // - class variables in block scope
                            && current_decl_context->current_scope->kind == BLOCK_SCOPE
                            && (is_class_type(entry->type_information)
                                // - array of class type variables
                                || (is_array_type(entry->type_information) 
                                    && is_class_type(array_type_get_element_type(entry->type_information)))))
                        || (current_decl_context->current_scope->kind == NAMESPACE_SCOPE
                            // - namespace-scope declarations of non-member
                            // entities that are non-extern since they are
                            // definitions (global definitions are here)
                            && ((!symbol_entity_specs_get_is_member(entry) 
                                    && !symbol_entity_specs_get_is_extern(entry)
                                    && !current_gather_info.is_extern)
                                // - static member definitions (at
                                // namespace-scope these are definitions too)
                                || (symbol_entity_specs_get_is_member(entry) 
                                    && symbol_entity_specs_get_is_static(entry))))
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


            // (C and C++) Always declare typedefs of variably modified types
            if ((entry->kind == SK_TYPEDEF
                        && is_variably_modified_type(entry->type_information))
                    || (IS_C_LANGUAGE
                        // (Only C) Explicitly declare external variables in the global scope
                        && ((entry->kind == SK_VARIABLE
                                && symbol_entity_specs_get_is_extern(entry)
                                && entry->decl_context->current_scope == entry->decl_context->global_scope)
                            // (Only C) Explicitly declare functions that are aliases of other functions
                            || (entry->kind == SK_FUNCTION
                                && symbol_has_gcc_attribute(entry, "alias", /* gcc_attr */ NULL)))))
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
                symbol_entity_specs_set_asm_specification(entry, asm_spec);
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

            if (CURRENT_CONFIGURATION->xl_compatibility)
            {
                if (current_gather_info.num_xl_pragmas > 0)
                {
                    *nodecl_output = nodecl_append_to_list(
                            *nodecl_output,
                            nodecl_make_cxx_decl(
                                nodecl_make_context(nodecl_null(),
                                    current_decl_context,
                                    ast_get_locus(init_declarator)),
                                entry,
                                ast_get_locus(init_declarator)));
                    int i;
                    for (i = 0; i < current_gather_info.num_xl_pragmas; i++)
                    {
                        *nodecl_output = nodecl_append_to_list(
                                *nodecl_output,
                                nodecl_make_unknown_pragma(
                                    current_gather_info.xl_pragmas[i],
                                    ast_get_locus(init_declarator)));
                    }

                    DELETE(current_gather_info.xl_pragmas);
                }
            }
        }

    }
    else if (simple_type_info != NULL
            && declarator_list == NULL)
    {
        // Anonymous union special treatment
        if (is_named_type(simple_type_info)
                && is_class_type(simple_type_info)
                && symbol_entity_specs_get_is_anonymous_union(named_type_get_symbol(simple_type_info)))
        {
            scope_entry_t* named_type = named_type_get_symbol(simple_type_info);
            finish_anonymous_class(named_type, decl_context);
        }
        else if (decl_specifier_seq != NULL)
        {
            AST type_spec = ASTSon1(decl_specifier_seq);
            if (type_spec != NULL
                    && ASTKind(type_spec) != AST_CLASS_SPECIFIER
                    && ASTKind(type_spec) != AST_ELABORATED_TYPE_CLASS_SPEC
                    && ASTKind(type_spec) != AST_ENUM_SPECIFIER
                    && ASTKind(type_spec) != AST_ELABORATED_TYPE_ENUM_SPEC)
            {
                warn_printf_at(ast_get_locus(a), "declaration does not declare anything\n");
            }
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
        const decl_context_t* decl_context,
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
            if (ASTKind(spec) != AST_GCC_ATTRIBUTE)
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
                    && ASTKind(spec) == AST_GCC_ATTRIBUTE
                    && type_spec != NULL
                    && ASTKind(type_spec) == AST_CLASS_SPECIFIER)
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
                if (ASTKind(spec) == AST_GCC_ATTRIBUTE)
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
        gather_info->is_decltype_auto = local_gather_info.is_decltype_auto;

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
                error_printf_at(ast_get_locus(a), "a boolean type requires an integer type\n");
                *type_info = get_error_type();
                return;
            }
            *type_info = get_bool_of_integer_type(*type_info);
        }

        if (gather_info->is_mask_integer)
        {
            if (!is_integer_type(*type_info))
            {
                error_printf_at(ast_get_locus(a), "a mask type requires an integer type\n");
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

        if (gather_info->is_atomic)
        {
            if (is_array_type(*type_info)
                    || is_function_type(*type_info))
            {
                error_printf_at(ast_get_locus(a), "array or function types cannot be atomic\n");
                *type_info = get_error_type();
                return;
            }
            *type_info = get_variant_type_atomic(*type_info);
        }
    }
    else
    {
        C_LANGUAGE()
        {
            warn_printf_at(ast_get_locus(a), "declaration does not have a type-specifier, assuming 'int'\n");

            // Manually add the int tree to make things easier
            ast_set_child(a, 1, ASTLeaf(AST_IMPLICIT_INT_TYPE, ast_get_locus(a), NULL));
            *type_info = get_signed_int_type();
        }
    }
}

static void add_gcc_attribute_noreturn(
        AST attr_item UNUSED_PARAMETER,
        gather_decl_spec_t* gather_info,
        const decl_context_t* decl_context UNUSED_PARAMETER)
{
    gcc_attribute_t gcc_attr = { "noreturn", nodecl_null() };

    P_LIST_ADD(
            gather_info->gcc_attributes,
            gather_info->num_gcc_attributes,
            gcc_attr);
}

static void add_gcc_attribute_deprecated(
        AST attr_item UNUSED_PARAMETER,
        gather_decl_spec_t* gather_info,
        const decl_context_t* decl_context UNUSED_PARAMETER)
{
    // FIXME: deprecated (string-literal)
    gcc_attribute_t gcc_attr = { "deprecated", nodecl_null() };

    P_LIST_ADD(
            gather_info->gcc_attributes,
            gather_info->num_gcc_attributes,
            gcc_attr);
}

static void gather_std_attribute_spec(AST attribute_spec,
        gather_decl_spec_t* gather_info UNUSED_PARAMETER,
        const decl_context_t* decl_context UNUSED_PARAMETER)
{
    AST list = ASTSon0(attribute_spec);
    if (list == NULL)
        return;

    struct {
        const char* attr_name;
        char just_once;
        char seen;
        void (*fun)(AST, gather_decl_spec_t*, const decl_context_t*);
    } std_attributes[] =
    {
        // "name",              just-once,   0, fun
        { "noreturn",           1,           0, add_gcc_attribute_noreturn },
        { "gnu::noreturn",      1,           0, add_gcc_attribute_noreturn },
        { "deprecated",         1,           0, add_gcc_attribute_deprecated },

        // GCC does not implement this one
        // { "carries_dependency", 1,           0, NULL, },
    };

    AST iter;
    for_each_element(list, iter)
    {
        AST attr_item = ASTSon1(iter);

        AST attr_token = ASTSon0(attr_item);
        // AST attr_argument_clause = ASTSon1(attr_item);

        // We only handle the standard ones at the moment
        const char* attr_name = ast_get_text(attr_token);

        int N = STATIC_ARRAY_LENGTH(std_attributes);
        int i;
        for (i = 0; i < N; i++)
        {
            if (strcmp(attr_name, std_attributes[i].attr_name) == 0)
            {
                if (std_attributes[i].seen
                        && std_attributes[i].just_once)
                {
                    error_printf_at(ast_get_locus(attr_token),
                            "attribute '%s' can appear at most once in an attribute-list\n",
                            attr_name);
                }
                if (!std_attributes[i].seen
                        && std_attributes[i].fun != NULL)
                {
                    // Run attribute specific handler
                    (std_attributes[i].fun)(attr_item, gather_info, decl_context);
                }
                std_attributes[i].seen = 1;
                break;
            }
        }

        if (i >= N)
        {
            warn_printf_at(ast_get_locus(attr_token), "ignoring attribute '%s'\n",
                    ast_get_text(attr_token));
        }
    }
}

static void gather_alignas(AST a, gather_decl_spec_t* gather_info, const decl_context_t* decl_context)
{
    // FIXME - Ellipsis
    AST expr = ast_get_child(a, 0);
    if (ASTKind(a) == AST_ALIGNAS_TYPE)
    {
        // alignas(type-id) must be equivalent to alignas(alignof(type-id))
        expr = ASTMake1(AST_ALIGNOF_TYPE, ast_copy(expr), ast_get_locus(expr),  NULL);
    }

    nodecl_t nodecl_alignas_expr = nodecl_null();
    check_expression_non_executable(expr, decl_context, &nodecl_alignas_expr);

    if (nodecl_is_err_expr(nodecl_alignas_expr))
        return;

    type_t* alignas_type_expr = nodecl_get_type(nodecl_alignas_expr);
    if (!is_dependent_type(alignas_type_expr)
            && !is_integral_type(no_ref(alignas_type_expr)))
    {
        error_printf_at(nodecl_get_locus(nodecl_alignas_expr),
                "alignment-specifier expression does not have integral type");
        return;
    }

    nodecl_alignas_expr = nodecl_expression_make_rvalue(nodecl_alignas_expr, decl_context);
    if (!nodecl_expr_is_value_dependent(nodecl_alignas_expr)
            && !nodecl_is_constant(nodecl_alignas_expr))
    {
        error_printf_at(nodecl_get_locus(nodecl_alignas_expr),
                "alignment-specifier expression is not an integral constant expression\n");
        return;
    }

    gather_info->alignas_list = nodecl_append_to_list(
            gather_info->alignas_list,
            nodecl_alignas_expr);

    if (ASTKind(a) == AST_ALIGNAS_TYPE)
    {
        ast_free(expr);
    }
}

/*
 * This function gathers everything that is in a decl_spec and fills gather_info
 */
static void gather_decl_spec_information(AST a, gather_decl_spec_t* gather_info, const decl_context_t* decl_context)
{
    switch (ASTKind(a))
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
            // C11
        case AST_NORETURN_SPEC :
            gather_info->is_noreturn = 1;
            // We cannot do anything with it yet
            warn_printf_at(ast_get_locus(a), "ignoring _Noreturn function specifier\n");
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
            // C11
        case AST_ATOMIC_TYPE_QUALIFIER:
            gather_info->is_atomic = 1;
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
                                && ASTKind(layout_qualif_kind) != AST_UPC_LAYOUT_UNDEF)
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
        case AST_UNKNOWN_PRAGMA:
            // Do nothing at the moment
            break;
        case AST_MS_DECLSPEC:
            // __declspec(X)
            // __declspec(X(Y0, Y1, ...))
            gather_ms_declspec(a, gather_info, decl_context);
            break;
        case AST_ATTRIBUTE_SPECIFIER:
            gather_std_attribute_spec(a, gather_info, decl_context);
            break;
        case AST_ALIGNAS_TYPE:
        case AST_ALIGNAS:
            {
                gather_alignas(a, gather_info, decl_context);
                break;
            }
        case AST_AMBIGUITY:
            {
                solve_ambiguous_decl_specifier(a, decl_context);
                gather_decl_spec_information(a, gather_info, decl_context);
                break;
            }
        default:
            internal_error("Unknown node '%s' (%s)", ast_print_node_type(ASTKind(a)), ast_location(a));
            break;
    }
}


type_t* compute_type_of_decltype(AST a, const decl_context_t* decl_context)
{
    ERROR_CONDITION(ASTKind(a) != AST_DECLTYPE, "Invalid node", 0);

    AST expression = advance_expression_nest_flags(ASTSon0(a), /* advance_parentheses */ 0);

    // Compute the expression type and use it for the whole type
    nodecl_t nodecl_expr = nodecl_null();

    check_expression_non_executable(expression, decl_context, &nodecl_expr);
    if (!nodecl_is_err_expr(nodecl_expr)
            && ASTKind(expression) == AST_PARENTHESIZED_EXPRESSION)
    {
        nodecl_expr = cxx_nodecl_wrap_in_parentheses(nodecl_expr);
    }

    return compute_type_of_decltype_nodecl(nodecl_expr, decl_context);
}


/*
 * This function fills simple_type_info with type information.
 */
void gather_type_spec_information(AST a, type_t** simple_type_info,
        gather_decl_spec_t* gather_info,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    switch (ASTKind(a))
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
            // C11
        case AST_ATOMIC_TYPE_SPECIFIER:
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

                gather_info->is_atomic = 1;
                *simple_type_info = declarator_type;
            }
            break;
            // C++11
        case AST_AUTO_TYPE:
            *simple_type_info = get_auto_type();
            gather_info->is_auto_type = 1;
            break;
            // C++14
        case AST_DECLTYPE_AUTO:
            if (!IS_CXX14_LANGUAGE)
            {
                warn_printf_at(ast_get_locus(a), "'decltype(auto)' is a C++14 feature\n");
            }
            *simple_type_info = get_decltype_auto_type();
            gather_info->is_auto_type = 1;
            gather_info->is_decltype_auto = 1;
            break;
        case AST_GCC_COMPLEX_TYPE :
            *simple_type_info = get_signed_int_type();
            gather_info->is_complex = 1;
            break;
            // C++11
        case AST_DECLTYPE :
            {
                *simple_type_info = compute_type_of_decltype(a, decl_context);
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
                                error_printf_at(ast_get_locus(a), "'%s' yields an unresolved overload type\n",
                                        prettyprint_in_buffer(a));
                                *simple_type_info = get_error_type();
                                return;
                            }

                            scope_entry_t* entry = entry_list_head(entry_list);
                            entry_list_free(entry_list);

                            if (!symbol_entity_specs_get_is_member(entry)
                                    || symbol_entity_specs_get_is_static(entry))
                            {
                                computed_type = entry->type_information;
                            }
                            else
                            {
                                computed_type = get_pointer_to_member_type(
                                        entry->type_information,
                                        symbol_entity_specs_get_class_type(entry));
                            }
                        }
                        else if (nodecl_expr_is_type_dependent(nodecl_expr))
                        {
                            // The expression type is dependent, so we will wrap in an typeof expression
                            computed_type = get_typeof_expr_dependent_type(nodecl_expr, decl_context,
                                    /* is_decltype */ 0);
                        }
                    }

                    computed_type = clear_special_expr_type_variants(computed_type);

                    *simple_type_info = computed_type;
                }
                else
                {
                    error_printf_at(ast_get_locus(a), "could not solve type '%s'\n",
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
                error_printf_at(ast_get_locus(a), "__int128 support not available\n");
                *simple_type_info = get_error_type();
#endif
                break;
            }
        case AST_GCC_FLOAT128:
            {
#ifdef HAVE_QUADMATH_H
                *simple_type_info = get_float128_type();
#else
                error_printf_at(ast_get_locus(a), "__float128 support not available\n");
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
                    error_printf_at(ast_get_locus(a), "type-id of an __underlying_type must be an enum type\n");
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
                    error_printf_at(ast_get_locus(a), "__int64 not supported\n");
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
                internal_error("Unknown node '%s'", ast_print_node_type(ASTKind(a)));
            }
    }
}

static void register_dependent_friend_class(
        scope_entry_t* class_symbol,
        type_t* type_of_decl,
        const char* symbol_name,
        const decl_context_t* decl_context)
{
    scope_entry_t* dep_friend = NEW0(scope_entry_t);
    dep_friend->kind = SK_DEPENDENT_FRIEND_CLASS;
    dep_friend->decl_context = decl_context;
    dep_friend->type_information = type_of_decl;
    dep_friend->symbol_name = symbol_name;

    class_type_add_friend_symbol(class_symbol->type_information, dep_friend);
}

static char is_dependent_class_scope(const decl_context_t* decl_context)
{
    return (decl_context->class_scope != NULL 
            && is_dependent_type(decl_context->class_scope->related_entry->type_information));  
}

static void gather_type_spec_from_friend_elaborated_class_specifier_common(
        AST a,
        type_t** type_info,
        const char** declared_name,
        gather_decl_spec_t *gather_info,
        const decl_context_t* decl_context)
{
    char is_dependent_context = is_dependent_class_scope(decl_context);

    scope_entry_t* class_symbol = decl_context->current_scope->related_entry;
    ERROR_CONDITION(class_symbol->kind != SK_CLASS, "Invalid symbol", 0);

    *declared_name = NULL;

    AST class_key = ASTSon0(a);

    enum type_tag_t class_kind = TT_INVALID;
    decl_flags_t class_kind_flag = DF_NONE;
    switch (ASTKind(class_key))
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

    decl_flags_t decl_flags = DF_NONE;

    if (is_unqualified_id_expression(id_expression))
    {
        decl_flags |= class_kind_flag;
    }

    scope_entry_t* entry = NULL;
    if (is_qualified_id_expression(id_expression)
            || ASTKind(id_expression) == AST_TEMPLATE_ID)
    {
        scope_entry_list_t* entry_list = NULL;

        if (is_dependent_context)
        {
            // In dependent contexts we do not examine uninstantiated templates
            // because the template parameters (if any) are likely to be
            // wrongly nested
            decl_flags |= DF_DEPENDENT_TYPENAME;
        }
        
        entry_list = query_id_expression_flags(
                decl_context,
                id_expression, NULL, decl_flags);
        if (entry_list == NULL)
        {
            error_printf_at(ast_get_locus(id_expression), "class name '%s' not found\n",
                    prettyprint_in_buffer(id_expression));
            *type_info = get_error_type();
            return;
        }

        entry = entry_list_head(entry_list);
        entry_list_free(entry_list);

        entry = entry_advance_aliases(entry);

        if (entry->kind != SK_CLASS
                && (entry->kind != SK_TEMPLATE
                    || !is_class_type(template_type_get_primary_type(entry->type_information)))
                && entry->kind != SK_DEPENDENT_ENTITY)
        {
            error_printf_at(ast_get_locus(id_expression), "'%s' is not a class name\n",
                    prettyprint_in_buffer(id_expression));
            *type_info = get_error_type();
            return;
        }
    }

    if (entry == NULL)
    {
        // friend class X;

        // template <typename T>
        //   friend class Y;

        *declared_name = ASTText(id_expression);
        type_t* type_of_decl = get_new_class_type(
                decl_context,
                class_kind);

        if (gather_info->is_template)
        {
            scope_entry_t* fake_template = NEW0(scope_entry_t);
            fake_template->kind = SK_TEMPLATE;
            fake_template->decl_context = decl_context;
            fake_template->locus = ast_get_locus(id_expression);
            fake_template->type_information = get_new_template_type(
                    decl_context->template_parameters,
                    type_of_decl,
                    *declared_name,
                    decl_context,
                    ast_get_locus(id_expression));

            template_type_set_related_symbol(fake_template->type_information, fake_template);

            type_of_decl = fake_template->type_information;
        }

        *type_info = type_of_decl;
    }
    else if (entry->kind == SK_DEPENDENT_ENTITY)
    {
        // Let's copy this SK_DEPENDENT_ENTITY because we are
        // going to change its type
        scope_entry_t* new_dep = NEW0(scope_entry_t);
        *new_dep = *entry;
        new_dep->type_information = set_dependent_entry_kind(entry->type_information, class_kind);
        new_dep->decl_context = decl_context;

        *type_info = get_user_defined_type(new_dep);
    }
    else
    {
        // friend class A<S>
        // friend class N1::Class;
        // friend class N2::Class<S>;
        // friend class T::Foo; // T a template-parameter
        *type_info = get_user_defined_type(entry);
    }
}

#if 0
static void gather_type_spec_from_friend_simple_type_specifier_common(
        AST a,
        type_t** type_info,
        gather_decl_spec_t *gather_info,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output UNUSED_PARAMETER)
{
    scope_entry_t* class_symbol = decl_context->current_scope->related_entry;
    ERROR_CONDITION(class_symbol == NULL || class_symbol->kind != SK_CLASS, "Invalid symbol", 0);

    if (ASTKind(a) == AST_SIMPLE_TYPE_SPEC)
    {
        gather_type_spec_from_simple_type_specifier(a, type_info, gather_info, decl_context);
    }
    else if (ASTKind(a) == AST_ELABORATED_TYPENAME_SPEC)
    {
        gather_type_spec_from_dependent_typename(a, type_info, gather_info, decl_context);
    }
    else
    {
        internal_error("Code unreachable", 0);
    }

}
#endif

static const decl_context_t* get_innermost_enclosing_nonclass_context(const decl_context_t* decl_context)
{
    // This will be the innermost enclosing non-class scope
    scope_t* scope = decl_context->current_scope;
    while (scope->kind == CLASS_SCOPE)
    {
        scope = scope->contained_in;
    }

    if (scope->kind == NAMESPACE_SCOPE)
    {
        return scope->related_entry->related_decl_context;
    }
    else if (scope->kind == BLOCK_SCOPE)
    {
        decl_context_t* new_decl_context = decl_context_clone(decl_context);
        new_decl_context->current_scope = scope;

        return new_decl_context;
    }
    else
    {
        internal_error("Code unreachable", 0);
    }
}

static scope_entry_t* new_friend_declared_class(
        type_t* type_of_declaration,
        const decl_context_t* decl_context,
        const char* declared_name,
        const locus_t* locus)
{
    const decl_context_t* context_of_new_declared_class =
        get_innermost_enclosing_nonclass_context(decl_context);

    scope_entry_t* new_class = NULL;
    new_class = new_symbol(context_of_new_declared_class,
            context_of_new_declared_class->current_scope,
            declared_name);
    new_class->kind = SK_CLASS;
    enum type_tag_t class_kind = class_type_get_class_kind(type_of_declaration);
    new_class->type_information = get_new_class_type(
            context_of_new_declared_class,
            class_kind);

    new_class->locus = locus;
    symbol_entity_specs_set_is_friend_declared(new_class, 1);
    symbol_entity_specs_set_is_user_declared(new_class, 0);

    scope_entry_t* new_friend_class = NEW0(scope_entry_t);
    new_friend_class->kind = SK_FRIEND_CLASS;
    // Keep the context of the declaration, not where we sign in the new class
    new_friend_class->decl_context = decl_context;
    symbol_entity_specs_set_alias_to(new_friend_class, new_class);

    return new_friend_class;
}

static scope_entry_t* new_friend_declared_template_class(
        type_t* type_of_declaration,
        const decl_context_t* decl_context,
        const char* declared_name,
        const locus_t* locus)
{
    const decl_context_t* context_of_new_declared_class =
        get_innermost_enclosing_nonclass_context(decl_context);

    scope_entry_t* new_template = NULL;
    new_template = new_symbol(context_of_new_declared_class,
            context_of_new_declared_class->current_scope,
            declared_name);
    new_template->kind = SK_TEMPLATE;

    enum type_tag_t class_kind = class_type_get_class_kind(
            template_type_get_primary_type(type_of_declaration));
    new_template->type_information = get_new_template_type(
            decl_context->template_parameters,
            get_new_class_type(decl_context, class_kind),
            declared_name, context_of_new_declared_class,
            locus);
    template_type_set_related_symbol(new_template->type_information, new_template);

    new_template->locus = locus;
    symbol_entity_specs_set_is_friend_declared(new_template, 1);
    symbol_entity_specs_set_is_user_declared(new_template, 0);

    scope_entry_t* new_primary = named_type_get_symbol(
            template_type_get_primary_type(new_template->type_information));

    symbol_entity_specs_set_is_friend_declared(new_primary, 1);
    symbol_entity_specs_set_is_user_declared(new_primary, 0);

    scope_entry_t* new_friend_template = NEW0(scope_entry_t);
    new_friend_template->kind = SK_FRIEND_CLASS;
    // Keep the context of the declaration, not where we sign in the new class
    new_friend_template->decl_context = decl_context;
    symbol_entity_specs_set_alias_to(new_friend_template, new_template);

    return new_friend_template;
}

static char is_local_class_context(const decl_context_t* decl_context)
{
    scope_t* scope = decl_context->current_scope;
    while (scope->kind == CLASS_SCOPE)
    {
        scope = scope->contained_in;
    }

    return (scope->kind == BLOCK_SCOPE);
}

void build_scope_friend_class_declaration(
        type_t* type_of_declaration,
        const char* declared_name,
        const decl_context_t* decl_context,
        const locus_t* locus)
{
    scope_entry_t* class_symbol = decl_context->current_scope->related_entry;
    ERROR_CONDITION(class_symbol == NULL
            || class_symbol->kind != SK_CLASS, "Invalid symbol", 0);

    const decl_context_t* decl_context_query = decl_context;
    decl_flags_t decl_flags = DF_NONE;
    if (is_local_class_context(decl_context))
    {
        decl_context_query = get_innermost_enclosing_nonclass_context(decl_context_query);
        decl_flags |= DF_ONLY_CURRENT_SCOPE;
    }

    if (is_unnamed_class_type(type_of_declaration))
    {
        /*
         * struct A
         * {
         *   friend class B;
         * };
         */
        ERROR_CONDITION(declared_name == NULL, "Invalid name", 0);

        scope_entry_list_t* result_list = query_name_str_flags(decl_context_query,
                declared_name, NULL, decl_flags);

        enum cxx_symbol_kind filter_classes[] =
        {
            SK_CLASS,
        };

        scope_entry_list_t* entry_list = filter_symbol_kind_set(result_list,
                STATIC_ARRAY_LENGTH(filter_classes), filter_classes);

        entry_list_free(result_list);
        scope_entry_t* entry = (entry_list != NULL) ? entry_list_head(entry_list) : NULL;
        entry_list_free(entry_list);

        if (entry == NULL)
        {
            scope_entry_t* new_class =
                new_friend_declared_class(
                        type_of_declaration,
                        decl_context,
                        declared_name,
                        locus);

            class_type_add_friend_symbol(class_symbol->type_information,
                    new_class);
        }
        else
        {
            scope_entry_t* new_friend_class = NEW0(scope_entry_t);
            new_friend_class->kind = SK_FRIEND_CLASS;
            new_friend_class->decl_context = decl_context;
            symbol_entity_specs_set_alias_to(new_friend_class, entry);

            class_type_add_friend_symbol(class_symbol->type_information,
                    new_friend_class);
        }
    }
    else if (is_template_type(type_of_declaration))
    {
        /*
         * struct A
         * {
         *   template <typename T>
         *   friend class B;
         * };
         */
        ERROR_CONDITION(declared_name == NULL, "Invalid name", 0);

        scope_entry_list_t* result_list = query_name_str_flags(decl_context_query,
                declared_name, NULL, decl_flags);

        enum cxx_symbol_kind filter_classes[] =
        {
            SK_TEMPLATE,
        };

        scope_entry_list_t* entry_list = filter_symbol_kind_set(result_list,
                STATIC_ARRAY_LENGTH(filter_classes), filter_classes);

        if (result_list != NULL
                && entry_list == NULL)
        {
            scope_entry_t* entry = entry_list_head(result_list);
            error_printf_at(locus, "'%s' is not a template-name\n",
                    get_qualified_symbol_name(entry, entry->decl_context));
            entry_list_free(result_list);
            return;
        }

        entry_list_free(result_list);
        scope_entry_t* entry = (entry_list != NULL) ? entry_list_head(entry_list) : NULL;
        entry_list_free(entry_list);

        if (entry == NULL)
        {
            scope_entry_t* new_template = new_friend_declared_template_class(
                    type_of_declaration,
                    decl_context,
                    declared_name,
                    locus);

            class_type_add_friend_symbol(class_symbol->type_information,
                    new_template);
        }
        else
        {
            if (!is_class_type(template_type_get_primary_type(entry->type_information)))
            {
                error_printf_at(locus, "template name '%s' is not a class template\n",
                        get_qualified_symbol_name(entry, entry->decl_context));
                return;
            }

            scope_entry_t* new_friend_class = NEW0(scope_entry_t);
            new_friend_class->kind = SK_FRIEND_CLASS;
            new_friend_class->decl_context = decl_context;
            symbol_entity_specs_set_alias_to(new_friend_class, entry);

            class_type_add_friend_symbol(class_symbol->type_information,
                    new_friend_class);
        }
    }
    else if (is_named_type(type_of_declaration))
    {
        scope_entry_t* new_friend_class = NEW0(scope_entry_t);
        new_friend_class->kind = SK_FRIEND_CLASS;
        new_friend_class->decl_context = decl_context;
        symbol_entity_specs_set_alias_to(new_friend_class, named_type_get_symbol(type_of_declaration));

        class_type_add_friend_symbol(class_symbol->type_information,
                new_friend_class);
    }
}

static void gather_type_spec_from_elaborated_friend_class_specifier(AST a,
        type_t** type_info UNUSED_PARAMETER,
        gather_decl_spec_t *gather_info,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output UNUSED_PARAMETER)
{
    scope_entry_t* class_symbol = decl_context->current_scope->related_entry;
    ERROR_CONDITION(class_symbol == NULL
            || class_symbol->kind != SK_CLASS, "Invalid symbol", 0);

    const char* declared_name = NULL;
    gather_type_spec_from_friend_elaborated_class_specifier_common(
            a, type_info, &declared_name,
            gather_info, decl_context);
    if (is_error_type(*type_info))
        return;

    if (!is_dependent_class_scope(decl_context))
    {
        build_scope_friend_class_declaration(
                *type_info,
                declared_name,
                decl_context,
                ast_get_locus(a));
    }
    else
    {
        register_dependent_friend_class(
                class_symbol,
                *type_info,
                declared_name,
                decl_context);
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
            error_printf_at(locus, "a template-pack of a classe template must be the last template parameter\n");
            return 0;
        }
    }
    return 1;
}

static void gather_extra_attributes(AST a,
        gather_decl_spec_t* gather_info,
        const decl_context_t* decl_context)
{
    if (a == NULL)
        return;

    ERROR_CONDITION(ASTKind(a) != AST_NODE_LIST, "Invalid node", 0);

    AST it;
    for_each_element(a, it)
    {
        AST item = ASTSon1(it);
        switch (ASTKind(item))
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
            case AST_ALIGNAS:
            case AST_ALIGNAS_TYPE:
                {
                    gather_alignas(item, gather_info, decl_context);
                    break;
                }
            case AST_UNKNOWN_PRAGMA:
                {
                    if (CURRENT_CONFIGURATION->xl_compatibility)
                    {
                        P_LIST_ADD(gather_info->xl_pragmas, gather_info->num_xl_pragmas, ast_get_text(item));
                    }
                    break;
                }
            case AST_ATTRIBUTE_SPECIFIER:
                {
                    AST attr_list = ASTSon1(item);
                    if (attr_list != NULL)
                    {
                        warn_printf_at(ast_get_locus(attr_list), "ignoring attribute-specifier\n");
                    }
                    break;
                }
            default:
                {
                    internal_error("Unexpected node '%s'\n", ast_print_node_type(ASTKind(item)));
                    break;
                }
        }
    }
}

static void apply_attributes_to_type(type_t** type,
        AST a,
        const decl_context_t* decl_context)
{
    if (a == NULL)
        return;

    ERROR_CONDITION(ASTKind(a) != AST_NODE_LIST, "Invalid node", 0);

    AST it;
    for_each_element(a, it)
    {
        AST item = ASTSon1(it);
        switch (ASTKind(item))
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
                    internal_error("Unexpected node '%s'\n", ast_print_node_type(ASTKind(item)));
                    break;
                }
        }
    }
}

static void gather_type_spec_from_elaborated_class_specifier(AST a,
        type_t** type_info,
        gather_decl_spec_t *gather_info,
        const decl_context_t* decl_context,
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
    switch (ASTKind(class_key))
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
                (ASTKind(id_expression) == AST_TEMPLATE_ID || ASTKind(id_expression) == AST_QUALIFIED_ID)
         );

    CXX_LANGUAGE()
    {
        if (class_gather_info.no_declarators
                && !class_gather_info.parameter_declaration
                && ASTKind(id_expression) != AST_TEMPLATE_ID)
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
        if (decl_context->template_parameters->num_parameters
                != template_type_get_template_parameters(entry->type_information)->num_parameters)
        {
            error_printf_at(ast_get_locus(id_expression), "redeclaration with %d template parameters while previous declaration used %d\n",
                    decl_context->template_parameters->num_parameters,
                    template_type_get_template_parameters(entry->type_information)->num_parameters);
            *type_info = get_error_type();
            return;
        }

        template_type_update_template_parameters(entry->type_information,
                decl_context->template_parameters);

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
                decl_context_t *fix_decl_context = decl_context_clone(decl_context);

                // Note that in a parameter declaration no type can be defined actually
                if (class_gather_info.parameter_declaration)
                {
                    while (fix_decl_context->current_scope->kind == BLOCK_SCOPE)
                    {
                        fix_decl_context->current_scope = fix_decl_context->current_scope->contained_in;
                    }
                }
                // Look for the smallest enclosing non-function-prototype scope
                while (fix_decl_context->current_scope->kind == CLASS_SCOPE
                        || fix_decl_context->current_scope->kind == PROTOTYPE_SCOPE)
                {
                    fix_decl_context->current_scope = fix_decl_context->current_scope->contained_in;
                }

                decl_context = fix_decl_context;
            }

            const char* class_name = NULL;
            if (ASTKind(id_expression) == AST_SYMBOL)
            {
                class_name = ASTText(id_expression);
            }
            else if (ASTKind(id_expression) == AST_TEMPLATE_ID)
            {
                class_name = ASTText(ASTSon0(id_expression));
            }
            else
            {
                error_printf_at(ast_get_locus(id_expression), "invalid class specifier '%s'\n",
                        prettyprint_in_buffer(id_expression));
                *type_info = get_error_type();
                return;
            }

            C_LANGUAGE()
            {
                class_name = strappend(class_kind_name, strappend(" ", class_name));
            }

            scope_entry_t* new_class = NULL;
            new_class = new_symbol(decl_context, decl_context->current_scope, class_name);

            new_class->locus = ast_get_locus(id_expression);

            if ((!class_gather_info.is_template
                        || !class_gather_info.no_declarators)
                    && ASTKind(id_expression) != AST_TEMPLATE_ID)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "BUILDSCOPE: Type not found, creating a stub in scope %p for '%s' %p\n",
                            decl_context->current_scope,
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
                if (ASTKind(id_expression) != AST_TEMPLATE_ID)
                {
                    if (!check_class_template_parameters(ast_get_locus(a), decl_context->template_parameters))
                    {
                        *type_info = get_error_type();
                        return;
                    }

                    new_class->kind = SK_TEMPLATE;
                    new_class->type_information = get_new_template_type(decl_context->template_parameters,
                            get_new_class_type(decl_context, class_kind),
                            ASTText(id_expression), decl_context,
                            ast_get_locus(id_expression));
                    template_type_set_related_symbol(new_class->type_information, new_class);

                    new_class->locus = ast_get_locus(a);

                    if (decl_context->current_scope->kind == CLASS_SCOPE)
                    {
                        symbol_entity_specs_set_is_member(new_class, 1);
                        // FIXME!
                        // symbol_entity_specs_set_access(new_class, current_access);
                        symbol_entity_specs_set_class_type(new_class,
                            get_user_defined_type(decl_context->current_scope->related_entry));
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
                    error_printf_at(ast_get_locus(id_expression), "invalid template-name '%s'\n",
                            prettyprint_in_buffer(id_expression));
                    *type_info = get_error_type();
                    return;
                }
            }
        }
        else
        {
            error_printf_at(ast_get_locus(id_expression), "class name '%s' not found\n",
                    prettyprint_in_buffer(id_expression));
            *type_info = get_error_type();
            return;
        }

        // If the class is being declared in class-scope it means
        // it is a nested class
        if (decl_context->current_scope->kind == CLASS_SCOPE)
        {
            scope_entry_t* enclosing_class_symbol = decl_context->current_scope->related_entry;
            type_t* enclosing_class_type = enclosing_class_symbol->type_information;
            class_type_add_member(enclosing_class_type, class_entry,
                    decl_context, /* is_definition */ 0);

            CXX_LANGUAGE()
            {
                symbol_entity_specs_set_is_member(class_entry, 1);
                symbol_entity_specs_set_access(class_entry, class_gather_info.current_access);
                symbol_entity_specs_set_class_type(class_entry, get_user_defined_type(enclosing_class_symbol));
            }

            class_type_set_enclosing_class_type(class_type, get_user_defined_type(enclosing_class_symbol));

            // If the enclosing class is dependent, so is this one
            char c = is_dependent_type(class_entry->type_information);
            c = c || is_dependent_type(enclosing_class_type);
            set_is_dependent_type(class_entry->type_information, c);
        }
        else if (decl_context->current_scope->kind == BLOCK_SCOPE)
        {
            // This is a local class
            scope_entry_t* enclosing_function = decl_context->current_scope->related_entry;

            // A local class is dependent if enclosed in a template function or
            // a member function of a template class
            if (enclosing_function != NULL
                    && (is_dependent_type(enclosing_function->type_information)
                        || (symbol_entity_specs_get_is_member(enclosing_function)
                            && is_dependent_type(symbol_entity_specs_get_class_type(enclosing_function)))))
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
                error_printf_at(ast_get_locus(id_expression), "declaration '%s' does not declare anything\n",
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
                entry = NEW0(scope_entry_t);
                *entry = *old_entry;

                keep_extra_attributes_in_symbol(entry, &class_gather_info);
            }
            return;
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: Class type found already declared in %s, using it\n", locus_to_str(entry->locus));
        }

        ERROR_CONDITION(entry->kind != SK_CLASS, "This must be a class", 0);

        if (class_gather_info.no_declarators
                && (ASTKind(id_expression) == AST_TEMPLATE_ID
                    || ASTKind(id_expression) == AST_QUALIFIED_ID)
                && !class_gather_info.is_template
                && !class_gather_info.is_explicit_specialization
                && !class_gather_info.is_explicit_instantiation)
        {
            error_printf_at(ast_get_locus(id_expression), "declaration '%s' does not declare anything\n",
                    prettyprint_in_buffer(id_expression));
            *type_info = get_error_type();
            return;
        }

        class_entry = entry;

        if (!class_gather_info.is_friend
                && symbol_entity_specs_get_is_friend_declared(entry))
        {
            symbol_entity_specs_set_is_friend_declared(entry, 0);
        }

        // Check the enclosing namespace scope
        // This is only valid if the scope of the entry is an inlined namespace of the current one
        if (!class_gather_info.is_explicit_specialization
                && !class_gather_info.is_explicit_instantiation
                && is_template_specialized_type(class_entry->type_information)
                && (class_entry->decl_context->namespace_scope != decl_context->namespace_scope)
                && !is_inline_namespace_of(class_entry->decl_context, decl_context))
        {
            error_printf_at(ast_get_locus(id_expression), "specialization of '%s' in different namespace from definition\n",
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
                    decl_context->template_parameters);
            template_specialized_type_update_template_parameters(
                    class_symbol_get_canonical_symbol(class_entry)->type_information,
                    decl_context->template_parameters);

            // Update the template_scope
            DEBUG_CODE()
            {
                fprintf(stderr, "BUILDSCOPE: Updating template scope\n");
            }
            decl_context_t *adjusted_decl_context = decl_context_clone(decl_context);
            adjusted_decl_context->template_parameters = adjusted_decl_context->template_parameters;
            class_entry->decl_context = adjusted_decl_context;
            class_symbol_get_canonical_symbol(class_entry)->decl_context = adjusted_decl_context;
        }
    }

    ERROR_CONDITION(class_entry == NULL, "Invalid class entry", 0);

    if ((!is_template_specialized_type(class_entry->type_information) ||
            (class_gather_info.is_template && class_gather_info.no_declarators)))
    {
        // State this symbol has been created by the code and not by the type system
        symbol_entity_specs_set_is_user_declared(class_entry, 1);
        symbol_entity_specs_set_is_instantiable(class_entry, 1);

        symbol_entity_specs_set_is_user_declared(class_symbol_get_canonical_symbol(class_entry), 1);
        symbol_entity_specs_set_is_instantiable(class_symbol_get_canonical_symbol(class_entry), 1);
    }

    keep_extra_attributes_in_symbol(class_entry, &class_gather_info);

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
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    AST enum_key = ASTSon3(a);
    AST enum_attribute_specifier = ASTSon1(a);
    AST id_expression = ASTSon0(a);
    AST enum_base = ASTSon2(a);

    gather_decl_spec_t enum_gather_info;
    copy_gather_info(&enum_gather_info, gather_info);
    gather_extra_attributes(enum_attribute_specifier, &enum_gather_info, decl_context);

    char enum_is_scoped = ASTKind(enum_key) == AST_SCOPED_ENUM_KEY;

    if(IS_CXX03_LANGUAGE
            && enum_is_scoped)
    {
        warn_printf_at(ast_get_locus(enum_key), "scoped enumerators are only valid in C++11\n");
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
        if (current_entry->kind != SK_ENUM
                && current_entry->kind != SK_DEPENDENT_ENTITY)

        {
            error_printf_at(current_entry->locus, "'%s' is not an enum-name\n", current_entry->symbol_name);
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

    if (entry != NULL
            && entry->kind == SK_DEPENDENT_ENTITY)
    {
        *type_info = entry->type_information;
        return;
    }

    type_t* underlying_type = NULL;
    if (enum_base != NULL)
    {
        if (IS_CXX03_LANGUAGE)
        {
            warn_printf_at(ast_get_locus(a), "enum-base is only valid in C++11\n");
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

            if (ASTKind(id_expression) != AST_SYMBOL)
            {
                error_printf_at(ast_get_locus(id_expression), "invalid enum-name '%s'\n",
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
                warn_printf_at(ast_get_locus(id_expression), "previously undeclared '%s' is a GCC extension\n",
                        enum_name);
            }

            decl_context_t* new_decl_context = decl_context_clone(decl_context);

            if (!gather_info->no_declarators)
            {
                // If no declarators declare it in the first non-class enclosing namespace
                new_decl_context->current_scope = decl_context->namespace_scope;
            }

            scope_entry_t* new_enum = new_symbol(new_decl_context, new_decl_context->current_scope, enum_name);
            new_enum->locus = ast_get_locus(id_expression);
            new_enum->kind = SK_ENUM;
            new_enum->type_information = get_new_enum_type(decl_context, enum_is_scoped);

            symbol_entity_specs_set_is_user_declared(new_enum, 1);

            *type_info = get_user_defined_type(new_enum);

            if (new_decl_context->current_scope->kind == CLASS_SCOPE)
            {
                scope_entry_t* class_symbol = new_decl_context->current_scope->related_entry;
                type_t* class_type = class_symbol->type_information;
                class_type_add_member(get_actual_class_type(class_type), new_enum,
                        new_enum->decl_context, /* is_definition */ 0);

                CXX_LANGUAGE()
                {
                    symbol_entity_specs_set_is_member(new_enum, 1);
                    symbol_entity_specs_set_access(new_enum, gather_info->current_access);
                    symbol_entity_specs_set_class_type(new_enum, get_user_defined_type(class_symbol));
                }

                set_is_dependent_type(new_enum->type_information,
                        is_dependent_type(class_type));
            }

            entry = new_enum;
        }
        else
        {
            error_printf_at(ast_get_locus(a), "enum type '%s' not found\n", prettyprint_in_buffer(a));
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
            error_printf_at(ast_get_locus(a), "enumerator previously declared with different underlying-type\n");
        }
    }

    if (underlying_type != NULL)
    {
        enum_type_set_underlying_type(entry->type_information, underlying_type);
        enum_type_set_underlying_type_is_fixed(entry->type_information, 1);
    }

    keep_extra_attributes_in_symbol(entry, &enum_gather_info);

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
        const decl_context_t* decl_context)
{
     if (decl_context->class_scope == NULL)
         return 0;

     scope_entry_t* dependent_entry = NULL;
     nodecl_t nodecl_dependent_parts = nodecl_null();

     dependent_typename_get_components(dependent_typename, &dependent_entry, &nodecl_dependent_parts);

     if (dependent_entry->kind != SK_CLASS)
         return 0;

     scope_entry_t* class_in_scope = decl_context->class_scope->related_entry;

     return dependent_entry_is_same_class_base_or_nested(dependent_entry, class_in_scope);
}
#endif

static void gather_type_spec_from_dependent_typename(AST a, 
        type_t** type_info,
        gather_decl_spec_t* gather_info,
        const decl_context_t* decl_context)
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
        error_printf_at(ast_get_locus(id_expression), "typename '%s' not found\n",
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
        const decl_context_t* decl_context UNUSED_PARAMETER,
        type_t** type_info, gather_decl_spec_t* gather_info, scope_entry_list_t* query_results)
{
    if (query_results == NULL)
    {
        error_printf_at(ast_get_locus(a), "type name '%s' has not been found in the current scope\n", prettyprint_in_buffer(a));
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
            error_printf_at(ast_get_locus(a), "identifier '%s' does not name a type\n",
                    prettyprint_in_buffer(a));
            if (entry->kind == SK_DEPENDENT_ENTITY)
            {
                info_printf_at(ast_get_locus(a), "maybe you meant '%s'\n",
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
        ERROR_CONDITION(symbol_entity_specs_get_alias_to(entry)->kind != SK_DEPENDENT_ENTITY, "Expecting a dependent entity", 0);
        *type_info = symbol_entity_specs_get_alias_to(entry)->type_information;
        return;
    }
    // Chances are that through class-scope lookup we have found the injected name
    if (symbol_entity_specs_get_is_injected_class_name(entry))
    {
        entry = named_type_get_symbol(symbol_entity_specs_get_class_type(entry));
    }

    entry_list_free(query_results);

    CXX11_LANGUAGE()
    {
        if (!get_is_inside_pack_expansion()
                && (entry->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK
                    || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK))
        {
            error_printf_at(ast_get_locus(a), "invalid template %s parameter pack '%s' not inside a pack expansion\n",
                    entry->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK ? "type" : "template",
                    entry->symbol_name);
            *type_info = get_error_type();
            return;
        }
    }

    (*type_info) = get_user_defined_type(entry);
}

/*
 * This routine is called in gather_type_spec_information and its purpose is to
 * fill the simple_type with the proper reference of the user defined type.
 */
static void gather_type_spec_from_simple_type_specifier(AST a, type_t** type_info,
        gather_decl_spec_t* gather_info,
        const decl_context_t* decl_context)
{
    AST id_expression = ASTSon0(a);
    decl_flags_t flags = DF_IGNORE_FRIEND_DECL;
    scope_entry_list_t* entry_list = query_id_expression_flags(decl_context, id_expression, NULL, flags);

    common_gather_type_spec_from_simple_type_specifier(a, decl_context, type_info, gather_info, entry_list);
}

static void nodecl_gather_type_spec_from_simple_type_specifier(nodecl_t a, type_t** type_info,
        gather_decl_spec_t* gather_info, const decl_context_t* decl_context)
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
        type_t *unsigned_type;
    }
    checked_types[] =
    {
        { get_signed_char_type(),          get_unsigned_char_type() },
        { get_signed_short_int_type(),     get_unsigned_short_int_type() },
        { get_signed_int_type(),           get_unsigned_int_type() },
        { get_signed_long_int_type(),      get_unsigned_long_int_type() },
        { get_signed_long_long_int_type(), get_unsigned_long_long_int_type() },
        { NULL , NULL }
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

        // 1. Checking signed types
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

        if (B_(const_value_lte(min_int, min_value))
                && B_(const_value_lte(max_value, max_int)))
        {
            return result->signed_type;
        }

        // 2. Checking unsigned types
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

        if (B_(const_value_lte(min_int, min_value))
                && B_(const_value_lte(max_value, max_int)))
        {
            return result->unsigned_type;
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
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    AST enum_head = ASTSon0(a);
    AST enumeration_list = ASTSon1(a);

    AST enum_key = ASTSon0(enum_head);
    AST enum_attribute_specifier = ASTSon1(enum_head);
    AST enum_name = ASTSon2(enum_head);
    AST enum_base = ASTSon3(enum_head);

    char enum_is_scoped = ASTKind(enum_key) == AST_SCOPED_ENUM_KEY;

    if(IS_CXX03_LANGUAGE
            && enum_is_scoped)
    {
        warn_printf_at(ast_get_locus(enum_key), "scoped enumerators are only valid in C++11\n");
    }

    gather_decl_spec_t enum_gather_info;
    copy_gather_info(&enum_gather_info, gather_info);
    gather_extra_attributes(enum_attribute_specifier, &enum_gather_info, decl_context);

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
                fprintf(stderr, "BUILDSCOPE: Registering enum '%s' in '%p'\n", enum_name_str, decl_context->current_scope);
            }

            new_enum = new_symbol(decl_context, decl_context->current_scope, enum_name_str);
            new_enum->locus = ast_get_locus(enum_name);
            new_enum->kind = SK_ENUM;
            new_enum->type_information = get_new_enum_type(decl_context, enum_is_scoped);
            symbol_entity_specs_set_is_user_declared(new_enum, 1);
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
            new_enum = new_symbol(decl_context, decl_context->current_scope, symbol_name);
        }
        CXX_LANGUAGE()
        {
            uniquestr_sprintf(&symbol_name, "mcc_enum_anon_%d", anonymous_enums);
            new_enum = NEW0(scope_entry_t);
            new_enum->symbol_name = symbol_name;
            new_enum->decl_context = decl_context;
        }
        ERROR_CONDITION(new_enum == NULL, "Invalid enumerator", 0);

        anonymous_enums++;

        new_enum->locus = ast_get_locus(a);
        new_enum->kind = SK_ENUM;
        new_enum->type_information = get_new_enum_type(decl_context, enum_is_scoped);

        symbol_entity_specs_set_is_unnamed(new_enum, 1);
        symbol_entity_specs_set_is_user_declared(new_enum, 1);
    }

    if (decl_context->current_scope->kind == CLASS_SCOPE)
    {
        scope_entry_t* class_symbol = decl_context->current_scope->related_entry;
        type_t* class_type = class_symbol->type_information;
        class_type_add_member(get_actual_class_type(class_type), new_enum,
                decl_context, /* is_definition */ 1);
        CXX_LANGUAGE()
        {
            symbol_entity_specs_set_is_member(new_enum, 1);
            symbol_entity_specs_set_access(new_enum, gather_info->current_access);
            symbol_entity_specs_set_class_type(new_enum, get_user_defined_type(class_symbol));
            symbol_entity_specs_set_is_defined_inside_class_specifier(new_enum, 1);

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
            warn_printf_at(ast_get_locus(a), "enum-base is only valid in C++11\n");
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
            error_printf_at(ast_get_locus(a), "enumerator previously declared with different underlying type\n");
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
        decl_context_t* enumerators_context = decl_context_clone(decl_context);

        C_LANGUAGE()
        {
            if (enumerators_context->current_scope->kind == CLASS_SCOPE)
            {
                // Switch to the enclosing NAMESPACE scope
                enumerators_context->current_scope = enumerators_context->namespace_scope;
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
                    enumerators_context->current_scope, ASTText(enumeration_name));
            enumeration_item->locus = ast_get_locus(enumeration_name);
            enumeration_item->kind = SK_ENUMERATOR;
            enumeration_item->type_information = get_signed_int_type();

            CXX03_LANGUAGE()
            {
                if (decl_context->current_scope->kind == CLASS_SCOPE)
                {
                    scope_entry_t* enclosing_class_symbol = decl_context->current_scope->related_entry;
                    type_t* enclosing_class_type = enclosing_class_symbol->type_information;
                    class_type_add_member(get_actual_class_type(enclosing_class_type),
                            enumeration_item,
                            decl_context,
                            /* is_definition */ 1);

                    symbol_entity_specs_set_is_member(enumeration_item, 1);
                    symbol_entity_specs_set_access(enumeration_item, gather_info->current_access);
                    symbol_entity_specs_set_is_defined_inside_class_specifier(enumeration_item, 1);
                    symbol_entity_specs_set_class_type(enumeration_item, get_user_defined_type(enclosing_class_symbol));
                }
            }

            CXX11_LANGUAGE()
            {
                if (ASTKind(enum_key) == AST_UNSCOPED_ENUM_KEY)
                {
                    // Insert entry in the enclosing scope
                    // (note that in C and C++2003 we are already registering
                    // them in the enclosing scope)
                    insert_entry(decl_context->current_scope, enumeration_item);

                    if (decl_context->current_scope->kind == CLASS_SCOPE)
                    {
                        scope_entry_t* enclosing_class_symbol = decl_context->current_scope->related_entry;
                        type_t* enclosing_class_type = enclosing_class_symbol->type_information;
                        class_type_add_member(get_actual_class_type(enclosing_class_type),
                                enumeration_item,
                                decl_context,
                                /* is_definition */ 1);

                        symbol_entity_specs_set_is_member(enumeration_item, 1);
                        symbol_entity_specs_set_access(enumeration_item, gather_info->current_access);
                        symbol_entity_specs_set_is_defined_inside_class_specifier(enumeration_item, 1);
                        symbol_entity_specs_set_class_type(enumeration_item, get_user_defined_type(enclosing_class_symbol));
                    }
                }
            }

            if (enumeration_expr != NULL)
            {
                nodecl_t nodecl_expr = nodecl_null();
                if (!check_expression_must_be_constant(enumeration_expr, enumerators_context, &nodecl_expr))
                {
                    error_printf_at(ast_get_locus(enumeration_expr), "invalid enumerator expression '%s'\n",
                            prettyprint_in_buffer(enumeration_expr));
                    if (!underlying_type_is_fixed)
                        underlying_type = get_error_type();
                }
                else
                {
                    nodecl_expr = nodecl_expression_make_rvalue(nodecl_expr, decl_context);

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
                        error_printf_at(ast_get_locus(enumeration_expr), "enumerator expression '%s' is not constant\n",
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

    keep_extra_attributes_in_symbol(new_enum, &enum_gather_info);

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

static void build_scope_base_clause(AST base_clause, scope_entry_t* class_entry, const decl_context_t* decl_context)
{
    type_t* class_type = get_actual_class_type(class_entry->type_information);

    if (class_type_get_class_kind(class_type) == TT_UNION)
    {
        error_printf_at(ast_get_locus(base_clause), "a union cannot have bases\n");
        return;
    }

    AST list = ASTSon0(base_clause);
    AST iter;
    for_each_element(list, iter)
    {
        AST base_specifier = ASTSon1(iter);

        char is_expansion = 0;
        if (ASTKind(base_specifier) == AST_BASE_SPEC_PACK_EXPANSION)
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
            switch (ASTKind(access_spec_tree))
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
                        internal_error("Unexpected tree '%s'\n", ast_print_node_type(ASTKind(access_spec_tree)));
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
            error_printf_at(ast_get_locus(class_name), "base class '%s' not found\n",
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
                    || result->kind == SK_TYPEDEF
                    || result->kind == SK_TEMPLATE_ALIAS))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "BUILDSCOPE: Base class '%s' IS NOT a dependent type\n", prettyprint_in_buffer(base_specifier));
            }

            if (!is_class_type(result->type_information))
            {
                error_printf_at(ast_get_locus(class_name), "name '%s' is not a class-name\n",
                        get_qualified_symbol_name(result, result->decl_context));
                continue;
            }

            scope_entry_t* base_class_symbol = result;
            // Update symbol because it might have been a typedef
            if (base_class_symbol->kind == SK_TYPEDEF
                    || base_class_symbol->kind == SK_TEMPLATE_ALIAS)
            {
                base_class_symbol = named_type_get_symbol(
                        advance_over_typedefs(base_class_symbol->type_information)
                        );
            }

            // If the entity (being an independent one) has not been completed, then instantiate it
            class_type_complete_if_needed(base_class_symbol, decl_context, ast_get_locus(base_specifier));

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
            if (class_entry->decl_context->current_scope->kind == CLASS_SCOPE)
            {
                enclosing_class = class_entry->decl_context->current_scope->related_entry;
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
                scope_entry_t* new_sym = NEW0(scope_entry_t);
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
            error_printf_at(ast_get_locus(class_name), "name '%s' is not a class-name\n",
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

        if (symbol_entity_specs_get_is_member(class_sym_of_entry))
        {
            return is_nested_in_class(symbol_entity_specs_get_class_type(class_sym_of_entry), 
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
                || !symbol_entity_specs_get_is_user_declared(entry))
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

            if (symbol_entity_specs_get_is_member(entry))
            {
                return check_template_nesting_of_name(named_type_get_symbol(symbol_entity_specs_get_class_type(entry)),
                        template_parameters->enclosing);
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
        if (symbol_entity_specs_get_is_member(entry))
        {
            return check_template_nesting_of_name(named_type_get_symbol(symbol_entity_specs_get_class_type(entry)), template_parameters);
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
                    && entry->decl_context->current_scope->kind == BLOCK_SCOPE)
                return NESTING_CHECK_OK;

            return NESTING_CHECK_NOT_A_TEMPLATE;
        }
    }

    return NESTING_CHECK_OK;
}

void check_nodecl_member_initializer_list(
        nodecl_t nodecl_cxx_member_init_list,
        scope_entry_t* function_entry,
        const decl_context_t* decl_context,
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    scope_entry_t* class_sym = named_type_get_symbol(symbol_entity_specs_get_class_type(function_entry));

    scope_entry_list_t* virtual_bases =
        class_type_get_virtual_base_classes_canonical(class_sym->type_information);
    scope_entry_list_t* direct_base_classes =
        class_type_get_direct_base_classes_canonical(class_sym->type_information);
    scope_entry_list_t* nonstatic_data_members =
        class_type_get_nonstatic_data_members(class_sym->type_information);

    scope_entry_list_t* already_initialized = NULL;

    int num_initializers = 0;
    nodecl_t* list = nodecl_unpack_list(nodecl_cxx_member_init_list, &num_initializers);

    scope_entry_list_t* result_list = NULL;
    decl_context_t* class_context = decl_context_clone(class_type_get_inner_context(class_sym->type_information));
    class_context->template_parameters = decl_context->template_parameters;

    char is_delegating_constructor = 0;

    int i;
    for (i = 0; i < num_initializers; i++)
    {
        nodecl_t current_mem_initializer = list[i];

        ERROR_CONDITION(nodecl_get_kind(current_mem_initializer) != NODECL_CXX_MEMBER_INIT, "Invalid node", 0);

        nodecl_t nodecl_name = nodecl_get_child(current_mem_initializer, 0);
        nodecl_t nodecl_init = nodecl_get_child(current_mem_initializer, 1);

        result_list = query_nodecl_name(class_context, nodecl_name, NULL);

        if (result_list == NULL)
        {
            error_printf_at(nodecl_get_locus(nodecl_name), "initialized entity '%s' not found\n",
                    codegen_to_str(nodecl_name, class_context));
            continue;
        }

        scope_entry_t* entry = entry_list_head(result_list);
        entry_list_free(result_list);

        if (entry->kind == SK_TEMPLATE_TYPE_PARAMETER)
        {
            // FIXME - This is very infortunate and should be solved in a different way
            entry = lookup_of_template_parameter(decl_context,
                    symbol_entity_specs_get_template_parameter_nesting(entry),
                    symbol_entity_specs_get_template_parameter_position(entry));
        }

        if (entry->kind == SK_TYPEDEF)
        {
            if (is_named_type(advance_over_typedefs(entry->type_information)))
            {
                entry = named_type_get_symbol(advance_over_typedefs(entry->type_information));
            }
        }

        // Chances are that through class-scope lookup we have found the injected name
        if (entry->kind == SK_CLASS
                && symbol_entity_specs_get_is_injected_class_name(entry))
        {
            // The injected class name is a member
            entry = named_type_get_symbol(symbol_entity_specs_get_class_type(entry));
        }

        if (entry_list_contains(already_initialized, entry))
        {
            error_printf_at(nodecl_get_locus(nodecl_name), "'%s' initialized twice in member initializer list\n",
                    get_qualified_symbol_name(entry, entry->decl_context));
            continue;
        }

        if (entry->kind == SK_TYPEDEF
                || entry->kind == SK_TEMPLATE_ALIAS)
        {
            type_t* t = advance_over_typedefs(entry->type_information);
            if (is_named_class_type(t))
                entry = named_type_get_symbol(t);
        }

        if (entry->kind == SK_CLASS
                && entry == class_sym)
        {
            CXX03_LANGUAGE()
            {
                warn_printf_at(nodecl_get_locus(current_mem_initializer), "delegating constructors are only valid in C++11\n");
            }

            if (i != 0)
            {
                error_printf_at(nodecl_get_locus(current_mem_initializer), "a delegating constructor must be the only initializer\n");
                // Give up
                continue;
            }
            is_delegating_constructor = 1;

            check_nodecl_initialization(nodecl_init,
                    decl_context,
                    entry,
                    get_unqualified_type(get_user_defined_type(entry)),
                    &nodecl_init,
                    /* is_auto_type */ 0,
                    /* is_decltype_auto */ 0);

            if (nodecl_is_err_expr(nodecl_init))
            {
                continue;
            }

            // Now get the called symbol
            ERROR_CONDITION(nodecl_get_kind(nodecl_init) != NODECL_FUNCTION_CALL, "Invalid node", 0);
            nodecl_t nodecl_called = nodecl_get_child(nodecl_init, 0);
            scope_entry_t* target_constructor = nodecl_get_symbol(nodecl_called);
            ERROR_CONDITION(target_constructor == NULL
                    || !symbol_entity_specs_get_is_constructor(target_constructor),
                    "Invalid function called", 0);

            if (function_entry == target_constructor)
            {
                error_printf_at(nodecl_get_locus(current_mem_initializer), "the target constructor of a delegating constructor cannot be itself\n");
            }
        }
        // Stray initializer once we know this is a delegating constructor
        else if (is_delegating_constructor)
        {
            error_printf_at(nodecl_get_locus(current_mem_initializer), "invalid initializer in delegating constructor\n");
            // Give up
            continue;
        }
        // Non-static member
        else if (entry->kind == SK_VARIABLE)
        {
            if (!entry_list_contains(nonstatic_data_members, entry))
            {
                if (!symbol_entity_specs_get_is_member(entry)
                        || !is_nested_in_class(symbol_entity_specs_get_class_type(entry), symbol_entity_specs_get_class_type(function_entry)))
                {
                    error_printf_at(nodecl_get_locus(nodecl_name), "symbol '%s' is not a member of class %s\n",
                            get_qualified_symbol_name(entry, entry->decl_context),
                            get_qualified_symbol_name(class_sym,
                                function_entry->decl_context));
                    continue;
                }
                if (symbol_entity_specs_get_is_static(entry))
                {
                    error_printf_at(nodecl_get_locus(nodecl_name), "static data member '%s' cannot be initialized here\n",
                            get_qualified_symbol_name(entry, entry->decl_context));
                    continue;
                }
            }

            check_nodecl_initialization(
                    nodecl_init,
                    decl_context,
                    entry,
                    get_unqualified_type(entry->type_information),
                    &nodecl_init,
                    /* is_auto_type */ 0,
                    /* is_decltype_auto */ 0);
        }
        // Base class
        else if (entry->kind == SK_CLASS)
        {
            if (!entry_list_contains(direct_base_classes, class_symbol_get_canonical_symbol(entry))
                    && !entry_list_contains(virtual_bases, class_symbol_get_canonical_symbol(entry)))
            {
                error_printf_at(nodecl_get_locus(nodecl_name), "class '%s' is not a direct base or virtual base of class '%s'\n",
                        get_qualified_symbol_name(entry, entry->decl_context),
                        get_qualified_symbol_name(class_sym, class_sym->decl_context));
            }

            check_nodecl_initialization(nodecl_init,
                    decl_context,
                    entry,
                    get_unqualified_type(get_user_defined_type(entry)),
                    &nodecl_init,
                    /* is_auto_type */ 0,
                    /* is_decltype_auto */ 0);

        }
        else
        {
            error_printf_at(nodecl_get_locus(nodecl_name), "symbol '%s' cannot be initialized here\n",
                    get_qualified_symbol_name(entry, entry->decl_context));
            continue;
        }

        already_initialized = entry_list_add(already_initialized, entry);

        if (nodecl_is_err_expr(nodecl_init))
        {
            continue;
        }

        nodecl_t nodecl_member_init = nodecl_make_member_init(
                nodecl_init,
                entry,
                nodecl_get_locus(nodecl_name));

        *nodecl_output = nodecl_append_to_list(*nodecl_output, nodecl_member_init);
    }
    DELETE(list);

    // Now review the remaining objects not initialized yet unless this
    // constructor was a delegating one
    if (!is_delegating_constructor)
    {
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
                        symbol_entity_specs_get_class_type(constructor),
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
                        symbol_entity_specs_get_class_type(constructor),
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

            if (IS_CXX11_LANGUAGE
                    && !nodecl_is_null(entry->value))
            {
                nodecl_t nodecl_object_init = nodecl_make_implicit_member_init(
                        // FIXME: We may have to fix the 'this' symbol used in the value
                        nodecl_shallow_copy(entry->value),
                        entry,
                        locus);
                *nodecl_output = nodecl_append_to_list(*nodecl_output, nodecl_object_init);
                continue;
            }

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
                            symbol_entity_specs_get_class_type(constructor),
                            decl_context,
                            locus);

                    nodecl_t nodecl_object_init = nodecl_make_implicit_member_init(
                            nodecl_call_to_ctor,
                            entry,
                            locus);
                    *nodecl_output = nodecl_append_to_list(*nodecl_output, nodecl_object_init);
                }
                else if (is_error_type(t))
                {
                    // skip
                }
                else
                {
                    internal_error("Code unreachable", 0);
                }
            }
        }
        entry_list_iterator_free(it);
    }
}

static void build_scope_ctor_initializer_dependent(
        AST ctor_initializer, 
        scope_entry_t* function_entry UNUSED_PARAMETER,
        const decl_context_t* decl_context,
        const locus_t* locus UNUSED_PARAMETER,
        nodecl_t* nodecl_output)
{
    if (ctor_initializer != NULL)
    {
        ERROR_CONDITION(decl_context->current_scope->kind != BLOCK_SCOPE,
                "Block scope is not valid", 0);

        AST mem_initializer_list = ASTSon0(ctor_initializer);
        AST iter;

        for_each_element(mem_initializer_list, iter)
        {
            AST mem_initializer = ASTSon1(iter);

            ERROR_CONDITION(ASTKind(mem_initializer) != AST_MEM_INITIALIZER, "Invalid tree", 0);

            AST mem_initializer_id = ASTSon0(mem_initializer);
            AST id_expression = ASTSon0(mem_initializer_id);

            AST initializer = ASTSon1(mem_initializer);

            nodecl_t nodecl_name = nodecl_null();
            nodecl_t nodecl_init = nodecl_null();

            compute_nodecl_name_from_id_expression(id_expression, decl_context, &nodecl_name);
            check_initialization(initializer,
                    decl_context,
                    NULL, /* We do not really know what is being initialized */
                    get_unknown_dependent_type(),
                    &nodecl_init,
                    /* is_auto_type */ 0,
                    /* is_decltype_auto */ 0);

            nodecl_t nodecl_cxx_init = nodecl_make_cxx_member_init(
                    nodecl_name, nodecl_init,
                    get_unknown_dependent_type(),
                    ast_get_locus(mem_initializer_id));

            *nodecl_output = nodecl_append_to_list(*nodecl_output, nodecl_cxx_init);
        }
    }
}

static void build_scope_ctor_initializer(
        AST ctor_initializer, 
        scope_entry_t* function_entry,
        const decl_context_t* decl_context,
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    nodecl_t nodecl_cxx_member_init_list = nodecl_null();
    build_scope_ctor_initializer_dependent(
            ctor_initializer,
            function_entry,
            decl_context,
            locus,
            &nodecl_cxx_member_init_list);

    scope_entry_t* class_sym = named_type_get_symbol(symbol_entity_specs_get_class_type(function_entry));
    if (is_dependent_type(class_sym->type_information)
            || is_dependent_type(function_entry->type_information))
    {
        *nodecl_output = nodecl_cxx_member_init_list;
        return;
    }

    check_nodecl_member_initializer_list(
            nodecl_cxx_member_init_list,
            function_entry,
            decl_context,
            locus,
            nodecl_output);
}

static char name_is_accessible_from_context(scope_entry_t* entry UNUSED_PARAMETER,
        const decl_context_t* decl_context UNUSED_PARAMETER)
{
    // FIXME - Not properly implemented yet
    return 1;
}

static char one_function_is_usable(
        scope_entry_list_t* candidates,
        type_t* first_arg_type,
        type_t* second_arg_type,
        const decl_context_t* decl_context,
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
            locus, /* explicit_template_arguments */ NULL);

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

    scope_entry_t* overload_resolution = solve_overload(candidate_set,
            decl_context,
            locus);

    if (overload_resolution == NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: No function was found to be usable\n");
        }
        return 0;
    }

    if (symbol_entity_specs_get_is_deleted(overload_resolution))
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
        found = !symbol_entity_specs_get_is_trivial(entry);
    }
    entry_list_iterator_free(it);

    return found;
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
                        symbol_entity_specs_get_is_trivial(current_default_constructor));
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
        symbol_entity_specs_set_is_trivial(default_constructor, 1);
    }
}

static char is_union_type_or_thereof(type_t* t)
{
    if (is_array_type(t))
        t = array_type_get_element_type(t);

    if (!is_class_type(t))
        return 0;

    if (is_union_type(t))
        return 1;

    return 0;
}

static char is_union_type_or_thereof_with_one_initializer(type_t* t)
{
    ERROR_CONDITION(!is_union_type_or_thereof(t), "Invalid type", 0);

    if (is_array_type(t))
        t = array_type_get_element_type(t);

    scope_entry_list_t* nonstatic_data_members = class_type_get_nonstatic_data_members(t);

    int num_initializers = 0;

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(nonstatic_data_members);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t *data_member = entry_list_iterator_current(it);
        num_initializers += !nodecl_is_null(data_member->value);
    }
    entry_list_iterator_free(it);

    return (num_initializers == 1);
}

void register_symbol_this(const decl_context_t* decl_context,
        scope_entry_t* class_symbol,
        const locus_t* locus)
{
    ERROR_CONDITION(class_symbol == NULL || class_symbol->kind != SK_CLASS, "Invalid class", 0);

    // Early registration of 'this' to be used in the declarator Its exact type
    // will be updated prior analyzing the body of the function (e.g. when the
    // function is const)
    type_t* pointed_this = get_user_defined_type(class_symbol);
    type_t* this_type = get_pointer_type(pointed_this);

    scope_entry_t* this_symbol = new_symbol(decl_context, decl_context->current_scope, UNIQUESTR_LITERAL("this"));

    this_symbol->locus = locus;

    this_symbol->kind = SK_VARIABLE;
    this_symbol->type_information = this_type;
    this_symbol->defined = 1;
    this_symbol->do_not_print = 1;
}

void update_symbol_this(scope_entry_t* entry,
        const decl_context_t* block_context)
{
    // The class we belong to
    type_t* pointed_this = symbol_entity_specs_get_class_type(entry);
    // Qualify likewise the function unless it is a destructor
    if (!symbol_entity_specs_get_is_destructor(entry))
    {
        pointed_this = get_cv_qualified_type(pointed_this, get_cv_qualifier(entry->type_information));
    }
    type_t* this_type = get_pointer_type(pointed_this);

    scope_entry_list_t* entry_list = query_name_str(block_context, UNIQUESTR_LITERAL("this"), NULL);
    // If the function is defined inside the class specifier, build_scope_function_definition_declarator
    ERROR_CONDITION(entry_list == NULL, "Symbol 'this' somehow got lost in this context\n", 0);
    scope_entry_t *this_symbol = entry_list_head(entry_list);
    entry_list_free(entry_list);

    this_symbol->type_information = this_type;
}

void register_symbol_this_in_class_scope(scope_entry_t* class_entry)
{
    ERROR_CONDITION(class_entry == NULL
            || class_entry->kind != SK_CLASS, "Invalid class", 0);

    const decl_context_t* inner_decl_context =
        class_type_get_inner_context(class_entry->type_information);

    // Create a 'this' only used for class-scope lexical scopes
    type_t* pointed_this = get_user_defined_type(class_entry);
    type_t* this_type = get_pointer_type(pointed_this);
    this_type = get_cv_qualified_type(this_type, CV_CONST);

    // This symbol must be detached because we do not want it be found
    // through any sort of lookup. It will be accessible through
    // the related_symbols of the class symbol
    scope_entry_t* this_symbol = NEW0(scope_entry_t);
    this_symbol->symbol_name = UNIQUESTR_LITERAL("this");
    this_symbol->decl_context = inner_decl_context;
    this_symbol->locus = class_entry->locus;
    this_symbol->kind = SK_VARIABLE;
    this_symbol->type_information = this_type;
    this_symbol->defined = 1;
    this_symbol->do_not_print = 1;

    symbol_entity_specs_add_related_symbols(class_entry, this_symbol);
}

static void default_constructor_determine_if_constexpr(
        scope_entry_t* default_constructor,
        scope_entry_list_t* nonstatic_data_members,
        scope_entry_list_t* direct_base_classes,
        char has_virtual_bases,
        char has_virtual_functions,
        const locus_t* locus)
{
    if (has_virtual_bases
            /* This is unclear to me but seems to follow from the definition of literal type */
            || has_virtual_functions)
        return;

    scope_entry_list_iterator_t* it = NULL;
    char has_nonstatic_data_member_without_initializer = 0;
    for (it = entry_list_iterator_begin(nonstatic_data_members);
            !entry_list_iterator_end(it)
            && !has_nonstatic_data_member_without_initializer;
            entry_list_iterator_next(it))
    {
        scope_entry_t *data_member = entry_list_iterator_current(it);

        if (nodecl_is_null(data_member->value))
        {
            if (is_class_type_or_array_thereof(data_member->type_information))
            {
                if (is_union_type_or_thereof(data_member->type_information))
                {
                    has_nonstatic_data_member_without_initializer =
                        !is_union_type_or_thereof_with_one_initializer(data_member->type_information);
                }
                else
                {
                    scope_entry_t* constructor = NULL;

                    diagnostic_context_push_buffered();
                    char valid = check_default_initialization(data_member, data_member->decl_context, locus, &constructor);
                    diagnostic_context_pop_and_discard();

                    if (!valid)
                        has_nonstatic_data_member_without_initializer = 1;
                    else
                        has_nonstatic_data_member_without_initializer = !symbol_entity_specs_get_is_constexpr(constructor);
                }
            }
            else
            {
                // scope_entry_t* constructor = NULL;
                // diagnostic_context_push_buffered();
                // char valid = check_default_initialization(data_member, data_member->decl_context, locus, &constructor);
                // diagnostic_context_pop_and_discard();

                // if (!valid)
                    has_nonstatic_data_member_without_initializer = 1;
                // else
                //     has_nonstatic_data_member_without_initializer = !(constructor == NULL
                //             || symbol_entity_specs_get_is_constexpr(constructor));
            }
        }
    }
    entry_list_iterator_free(it);

    if (has_nonstatic_data_member_without_initializer)
        return;

    char has_base_without_constexpr_constructor = 0;
    for (it = entry_list_iterator_begin(direct_base_classes);
            !entry_list_iterator_end(it)
            && !has_base_without_constexpr_constructor;
            entry_list_iterator_next(it))
    {
        scope_entry_t *base_class = entry_list_iterator_current(it);

        scope_entry_t* constructor = NULL;
        diagnostic_context_push_buffered();
        char valid = check_default_initialization(base_class, base_class->decl_context, locus, &constructor);
        diagnostic_context_pop_and_discard();

        if (!valid)
            has_base_without_constexpr_constructor = 1;
        else
            has_base_without_constexpr_constructor = !symbol_entity_specs_get_is_constexpr(constructor);
    }
    entry_list_iterator_free(it);

    if (has_base_without_constexpr_constructor)
        return;

    default_constructor->defined = 1;
    symbol_entity_specs_set_is_constexpr(default_constructor, 1);
    symbol_entity_specs_set_is_instantiable(default_constructor, 0);
    symbol_entity_specs_set_emission_template(default_constructor, NULL);

#if 0
    make_empty_body_for_default_function(default_constructor,
            default_constructor->decl_context,
            locus);
#endif
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
                |= !symbol_entity_specs_get_is_trivial(current_copy_constructor);
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
        symbol_entity_specs_set_is_trivial(copy_constructor, 1);
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
        symbol_entity_specs_set_is_trivial(move_constructor, 1);
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
        symbol_entity_specs_set_is_trivial(copy_assignment_operator, 1);
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
        symbol_entity_specs_set_is_trivial(move_assignment_operator, 1);
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

        base_has_nontrivial_destructor |= !symbol_entity_specs_get_is_trivial(current_destructor);
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
                |= !symbol_entity_specs_get_is_trivial(current_destructor);
        }
    }
    entry_list_iterator_free(it);

    // It is a trivial destructor
    if (!base_has_nontrivial_destructor
            && !has_nonstatic_data_member_with_no_trivial_destructor)
    {
        symbol_entity_specs_set_is_trivial(destructor, 1);
    }
}

static void set_defaulted_outside_class_specifier(
        scope_entry_t* entry, 
        const decl_context_t* decl_context,
        const locus_t* locus);
static void build_noexcept_spec_delayed(scope_entry_t* entry);

static char same_template_parameter_list(
        template_parameter_list_t* template_parameter_list_1,
        template_parameter_list_t* template_parameter_list_2,
        const decl_context_t* decl_context);

static char constructors_have_same_characteristics_for_inheritance(
        scope_entry_t* constructor1,
        scope_entry_t* constructor2,
        type_t* constructor2_type,
        const decl_context_t* decl_context)
{
    if (is_template_specialized_type(constructor1->type_information)
            != is_template_specialized_type(constructor2_type))
        return 0;
    else if (is_template_specialized_type(constructor1->type_information))
    {
        if (!same_template_parameter_list(
                    template_specialized_type_get_template_parameters(constructor1->type_information),
                    template_specialized_type_get_template_parameters(constructor2_type),
                    decl_context))
            return 0;
    }

    if (!equivalent_types(constructor1->type_information,
                constructor2_type))
        return 0;

    if (symbol_entity_specs_get_is_explicit(constructor1) !=
            symbol_entity_specs_get_is_explicit(constructor2))
        return 0;

    if (symbol_entity_specs_get_is_constexpr(constructor1) !=
            symbol_entity_specs_get_is_constexpr(constructor2))
        return 0;

    return 1;
}

static char exists_constructor_with_same_characteristics(
        scope_entry_list_t* constructor_set,
        scope_entry_t* base_constructor,
        type_t* base_constructor_type,
        const decl_context_t* decl_context)
{
    scope_entry_list_iterator_t* it;
    for (it = entry_list_iterator_begin(constructor_set);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* current_constructor = entry_list_iterator_current(it);
        if (constructors_have_same_characteristics_for_inheritance(
                    current_constructor,
                    base_constructor,
                    base_constructor_type,
                    decl_context))
        {
            entry_list_iterator_free(it);
            return 1;
        }
    }

    entry_list_iterator_free(it);
    return 0;
}

static char function_is_move_constructor_types(type_t* function_type, type_t* class_type);
static char function_is_copy_constructor_types(type_t* function_type, type_t* class_type);

static void declare_constructors_for_candidate_constructor(
        type_t* candidate_constructor_type,
        scope_entry_t* inherited_constructor,
        /* out */ scope_entry_list_t** inherited_constructors,
        scope_entry_list_t* current_class_constructors,
        type_t* class_type,
        type_t* type_info,
        const decl_context_t* decl_context,
        const locus_t* locus)
{
    // For each non-template constructor in the candidate set of inherited
    // constructors other than a constructor having no parameters or a
    // copy/move constructor having a single parameter, a constructor is
    // implicitly declared with the same constructor characteristics unless
    // there is a user-declared constructor with the same signature in the
    // complete class where the using-declaration appears or the constructor
    // would be a default, copy, or move constructor for that class.
    int num_parameters = function_type_get_num_parameters(candidate_constructor_type);
    if (function_type_get_has_ellipsis(candidate_constructor_type))
        num_parameters--;

    if (!is_template_specialized_type(inherited_constructor->type_information))
    {
        if (function_type_get_num_parameters(candidate_constructor_type) == 0)
            return;

        if (num_parameters == 1
                && (function_is_move_constructor_types(candidate_constructor_type,
                        symbol_entity_specs_get_class_type(inherited_constructor))
                    || function_is_copy_constructor_types(candidate_constructor_type,
                        symbol_entity_specs_get_class_type(inherited_constructor))))
            return;
        if (num_parameters == 1
                && (function_is_move_constructor_types(candidate_constructor_type,
                        class_type)
                    || function_is_copy_constructor_types(candidate_constructor_type,
                        class_type)))
            return;
    }
    // Similarly, for each constructor template in the candidate set of
    // inherited constructors, a constructor template is implicitly declared
    // with the same constructor characteristics unless there is an equivalent
    // user-declared constructor template (14.5.6.1) in the complete class
    // where the using-declaration appears.
    //
    // (Checks both non-template and templates)
    if (exists_constructor_with_same_characteristics(
                current_class_constructors,
                inherited_constructor,
                candidate_constructor_type,
                decl_context))
        return;
    // [ Note: Default arguments are not
    // inherited. An exception-specification is implied as specified in 15.4. —
    // end note ] A constructor so declared has the same access as the
    // corresponding constructor in X. It is deleted if the corresponding
    // constructor in X is deleted (8.4).
    const char* constructor_name = NULL;
    if (is_named_class_type(type_info))
    {
        uniquestr_sprintf(&constructor_name, "constructor %s", named_type_get_symbol(type_info)->symbol_name);
    }
    else
    {
        uniquestr_sprintf(&constructor_name, "%s", "constructor ");
    }

    const decl_context_t* class_context = class_type_get_inner_context(class_type);
    scope_t* class_scope = class_context->current_scope;

    scope_entry_t* new_inherited_constructor = NULL;
    new_inherited_constructor = new_symbol(class_context, class_scope,
            constructor_name);
    if (is_template_specialized_type(inherited_constructor->type_information))
    {
        decl_context_t* templated_class_context = decl_context_clone(class_context);
        templated_class_context->template_parameters =
                template_specialized_type_get_template_parameters(inherited_constructor->type_information);
        type_t* template_type = get_new_template_type(
                template_specialized_type_get_template_parameters(inherited_constructor->type_information),
                candidate_constructor_type,
                constructor_name,
                templated_class_context,
                inherited_constructor->locus);

        new_inherited_constructor->kind = SK_TEMPLATE;
        new_inherited_constructor->type_information = template_type;
        symbol_entity_specs_set_is_member(new_inherited_constructor, 1);
        symbol_entity_specs_set_class_type(new_inherited_constructor, type_info);
        symbol_entity_specs_set_is_user_declared(new_inherited_constructor, 0);

        template_type_set_related_symbol(template_type, new_inherited_constructor);

        // Now work only with the specialization
        new_inherited_constructor = named_type_get_symbol(template_type_get_primary_type(template_type));
    }
    else
    {
        new_inherited_constructor->type_information = candidate_constructor_type;
    }

    new_inherited_constructor->kind = SK_FUNCTION;
    new_inherited_constructor->locus = locus;
    new_inherited_constructor->defined = 0;

    symbol_entity_specs_set_is_member(new_inherited_constructor, 1);
    symbol_entity_specs_reserve_default_argument_info(new_inherited_constructor,
            symbol_entity_specs_get_num_parameters(new_inherited_constructor));

    symbol_entity_specs_set_is_user_declared(new_inherited_constructor, 0);
    symbol_entity_specs_set_is_explicit(new_inherited_constructor, symbol_entity_specs_get_is_explicit(inherited_constructor));
    symbol_entity_specs_set_is_constructor(new_inherited_constructor, symbol_entity_specs_get_is_constructor(inherited_constructor));
    symbol_entity_specs_set_is_constexpr(new_inherited_constructor, symbol_entity_specs_get_is_constexpr(inherited_constructor));
    symbol_entity_specs_set_access(new_inherited_constructor, symbol_entity_specs_get_access(inherited_constructor));
    symbol_entity_specs_set_class_type(new_inherited_constructor, type_info);
    symbol_entity_specs_set_is_deleted(new_inherited_constructor, symbol_entity_specs_get_is_deleted(inherited_constructor));
    symbol_entity_specs_set_any_exception(new_inherited_constructor, symbol_entity_specs_get_any_exception(inherited_constructor));
    symbol_entity_specs_set_noexception(new_inherited_constructor, symbol_entity_specs_get_noexception(inherited_constructor));
    symbol_entity_specs_copy_exceptions_from(new_inherited_constructor, inherited_constructor);
    // Let's remember where we inherit from
    symbol_entity_specs_set_alias_to(new_inherited_constructor, inherited_constructor);

    class_type_add_member(class_type, new_inherited_constructor, class_context, /* is_definition */ 1);

    if (exists_constructor_with_same_characteristics(
                *inherited_constructors,
                new_inherited_constructor,
                new_inherited_constructor->type_information,
                decl_context))
    {
        error_printf_at(locus, "redeclaration of constructor '%s' due to inherited constructor '%s'\n",
                print_decl_type_str(new_inherited_constructor->type_information,
                    new_inherited_constructor->decl_context,
                    get_qualified_symbol_name(new_inherited_constructor,
                        new_inherited_constructor->decl_context)),
                print_decl_type_str(candidate_constructor_type,
                    inherited_constructor->decl_context,
                    get_qualified_symbol_name(inherited_constructor,
                        inherited_constructor->decl_context)));
    }

    *inherited_constructors = entry_list_add(*inherited_constructors, new_inherited_constructor);
}

static void declare_constructors_for_inherited_constructor(
        scope_entry_t* inherited_ctor,
        /* out */ scope_entry_list_t** inherited_constructors,
        scope_entry_list_t* current_class_constructors,
        type_t* class_type,
        type_t* type_info,
        const decl_context_t* decl_context,
        const locus_t* locus)
{
    // The constructor itself is always a candidate
    declare_constructors_for_candidate_constructor(
            inherited_ctor->type_information,
            inherited_ctor,
            inherited_constructors,
            current_class_constructors,
            class_type,
            type_info,
            decl_context,
            locus);

    if (symbol_entity_specs_get_num_parameters(inherited_ctor) > 0)
    {
        int num_parameters = function_type_get_num_parameters(inherited_ctor->type_information);
        // Number of real parameters, ellipsis are counted as parameters
        // but only in the type system
        if (function_type_get_has_ellipsis(inherited_ctor->type_information))
            num_parameters--;

        int i;
        for (i = num_parameters - 1; i >= 0; i--)
        {
            if (symbol_entity_specs_get_default_argument_info_num(inherited_ctor, i) != NULL)
            {
                // Found a default argument
                // Change the type
                int num_new_param_types = i;
                parameter_info_t param_info[num_new_param_types + 1];
                memset(param_info, 0, sizeof(param_info));
                int j;
                for (j = 0; j < i; j++)
                {
                    param_info[j].type_info = function_type_get_parameter_type_num(
                            inherited_ctor->type_information,
                            j);
                }

                type_t* candidate_constructor_type =
                    get_new_function_type(
                            /* return-type */ NULL,
                            param_info,
                            num_new_param_types,
                            REF_QUALIFIER_NONE);

                declare_constructors_for_candidate_constructor(
                        candidate_constructor_type,
                        inherited_ctor,
                        inherited_constructors,
                        current_class_constructors,
                        class_type,
                        type_info,
                        decl_context,
                        locus);
            }
        }
    }
}

static void declare_inherited_constructors(
        type_t* class_type,
        type_t* type_info,
        const decl_context_t* decl_context,
        const locus_t* locus)
{
    scope_entry_list_t* current_class_constructors = class_type_get_constructors(class_type);
    scope_entry_list_t* inheriting_bases = class_type_get_inherited_constructors(class_type);

    scope_entry_list_t* inherited_constructors = NULL;
    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(inheriting_bases);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* current_base = entry_list_iterator_current(it);
        scope_entry_list_t* base_constructors =
            class_type_get_constructors(current_base->type_information);

        scope_entry_list_iterator_t* it2 = NULL;
        for (it2 = entry_list_iterator_begin(base_constructors);
                !entry_list_iterator_end(it2);
                entry_list_iterator_next(it2))
        {
            scope_entry_t* base_constructor = entry_list_iterator_current(it2);
            declare_constructors_for_inherited_constructor(
                    base_constructor,
                    &inherited_constructors,
                    current_class_constructors,
                    class_type,
                    type_info,
                    decl_context,
                    locus);
        }
        entry_list_iterator_free(it2);
        entry_list_free(base_constructors);
    }
    entry_list_iterator_free(it);

    entry_list_free(inheriting_bases);
    entry_list_free(current_class_constructors);
}

// See gather_type_spec_from_class_specifier to know what are class_type and type_info
// This function is only for C++
//
// FIXME - This function is still HUGE
static void finish_class_type_cxx(type_t* class_type,
        type_t* type_info,
        const decl_context_t* decl_context,
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

                class_type_complete_if_needed(named_type_sym, decl_context, locus);
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

            if (symbol_entity_specs_get_is_static(entry))
                continue;

            if (symbol_entity_specs_get_is_virtual(entry)
                    && symbol_entity_specs_get_is_pure(entry))
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
                        if (symbol_entity_specs_get_is_final(current_virtual))
                        {
                            error_printf_at(entry->locus, "member function '%s' overrides final '%s'\n",
                                    print_decl_type_str(entry->type_information,
                                        entry->decl_context,
                                        get_qualified_symbol_name(entry, entry->decl_context)),
                                    print_decl_type_str(current_virtual->type_information,
                                        current_virtual->decl_context,
                                        get_qualified_symbol_name(current_virtual, current_virtual->decl_context)));
                        }

                        symbol_entity_specs_set_is_virtual(entry, 1);

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
                        && symbol_entity_specs_get_is_pure(current_virtual))
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

            if (symbol_entity_specs_get_is_final(entry) && !symbol_entity_specs_get_is_virtual(entry))
            {
                error_printf_at(entry->locus, "member function '%s' declared as final but it is not virtual\n",
                        print_decl_type_str(entry->type_information,
                            entry->decl_context,
                            get_qualified_symbol_name(entry, entry->decl_context)));
            }

            if (symbol_entity_specs_get_is_override(entry))
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
                    error_printf_at(entry->locus, "member function '%s' declared as override but it does not override\n",
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

            if (symbol_entity_specs_get_is_defaulted(current_member_function)
                    && symbol_entity_specs_get_is_defined_inside_class_specifier(current_member_function))
            {
                // Verify again that this function can be defaulted
                current_member_function->defined = 0;
                symbol_entity_specs_set_is_defaulted(current_member_function, 0);

                // Make sure the exception specifier has been fully parsed at this point
                if (!nodecl_is_null(symbol_entity_specs_get_noexception(current_member_function))
                        && nodecl_get_kind(symbol_entity_specs_get_noexception(current_member_function)) == NODECL_CXX_PARSE_LATER)
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

    const decl_context_t* class_context = class_type_get_inner_context(class_type);
    scope_t* class_scope = class_context->current_scope;

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
        symbol_entity_specs_set_is_member(implicit_default_constructor, 1);
        symbol_entity_specs_set_access(implicit_default_constructor, AS_PUBLIC);
        symbol_entity_specs_set_class_type(implicit_default_constructor, type_info);
        symbol_entity_specs_set_is_inline(implicit_default_constructor, 1);
        symbol_entity_specs_set_is_constructor(implicit_default_constructor, 1);
        symbol_entity_specs_set_is_default_constructor(implicit_default_constructor, 1);
        symbol_entity_specs_set_is_defaulted(implicit_default_constructor, 1);

        implicit_default_constructor->type_information = default_constructor_type;

        implicit_default_constructor->defined = 1;

        class_type_add_member(class_type, implicit_default_constructor, class_context, /* is_definition */ 0);
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
                && !symbol_entity_specs_get_is_anonymous_union(named_type_get_symbol(type_info)))
        {
            for (it = entry_list_iterator_begin(nonstatic_data_members);
                    !entry_list_iterator_end(it)
                    && !has_variant_member_with_nontrivial_default_ctor;
                    entry_list_iterator_next(it))
            {
                scope_entry_t *data_member = entry_list_iterator_current(it);
                has_variant_member_with_nontrivial_default_ctor = (is_class_type(data_member->type_information)
                        && class_type_get_default_constructor(data_member->type_information) != NULL
                        && !symbol_entity_specs_get_is_trivial(class_type_get_default_constructor(data_member->type_information)));
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
            if (symbol_entity_specs_get_is_member_of_anonymous(data_member))
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
                if (symbol_entity_specs_get_is_member_of_anonymous(data_member))
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
            symbol_entity_specs_set_is_deleted(implicit_default_constructor, 1);
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

        CXX11_LANGUAGE()
        {
            default_constructor_determine_if_constexpr(
                    implicit_default_constructor,
                    nonstatic_data_members,
                    direct_base_classes,
                    has_virtual_bases,
                    has_virtual_functions,
                    locus);
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

            if (symbol_entity_specs_get_is_default_constructor(current_constructor)
                    && symbol_entity_specs_get_is_defaulted(current_constructor))
            {
                default_constructor_determine_if_trivial(
                        current_constructor,
                        nonstatic_data_members,
                        direct_base_classes,
                        has_virtual_bases,
                        has_virtual_functions);

                CXX11_LANGUAGE()
                {
                    default_constructor_determine_if_constexpr(
                            current_constructor,
                            nonstatic_data_members,
                            direct_base_classes,
                            has_virtual_bases,
                            has_virtual_functions,
                            locus);
                }
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
        symbol_entity_specs_set_is_member(implicit_copy_constructor, 1);
        symbol_entity_specs_set_access(implicit_copy_constructor, AS_PUBLIC);
        symbol_entity_specs_set_class_type(implicit_copy_constructor, type_info);
        symbol_entity_specs_set_is_constructor(implicit_copy_constructor, 1);
        symbol_entity_specs_set_is_copy_constructor(implicit_copy_constructor, 1);
        symbol_entity_specs_set_is_conversor_constructor(implicit_copy_constructor, 1);
        symbol_entity_specs_set_is_inline(implicit_copy_constructor, 1);
        symbol_entity_specs_set_is_defaulted(implicit_copy_constructor, 1);

        implicit_copy_constructor->type_information = copy_constructor_type;

        implicit_copy_constructor->defined = 1;

        symbol_entity_specs_reserve_default_argument_info(implicit_copy_constructor, 1);

        class_type_add_member(class_type, implicit_copy_constructor, class_context, /* is_definition */ 1);

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
            if (symbol_entity_specs_get_is_defaulted(current_constructor))
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
        entry_list_iterator_free(it);

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
        entry_list_iterator_free(it);

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
            symbol_entity_specs_set_is_member(implicit_move_constructor, 1);
            symbol_entity_specs_set_access(implicit_move_constructor, AS_PUBLIC);
            symbol_entity_specs_set_class_type(implicit_move_constructor, type_info);
            symbol_entity_specs_set_is_constructor(implicit_move_constructor, 1);
            symbol_entity_specs_set_is_move_constructor(implicit_move_constructor, 1);
            symbol_entity_specs_set_is_conversor_constructor(implicit_move_constructor, 1);
            symbol_entity_specs_set_is_inline(implicit_move_constructor, 1);
            symbol_entity_specs_set_is_defaulted(implicit_move_constructor, 1);

            implicit_move_constructor->type_information = move_constructor_type;

            implicit_move_constructor->defined = 1;

            symbol_entity_specs_reserve_default_argument_info(implicit_move_constructor, 1);

            class_type_add_member(class_type, implicit_move_constructor, class_context, /* is_definition */ 1);

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
            if (symbol_entity_specs_get_is_defaulted(current_constructor))
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
        symbol_entity_specs_set_is_member(implicit_copy_assignment_function, 1);
        symbol_entity_specs_set_access(implicit_copy_assignment_function, AS_PUBLIC);
        symbol_entity_specs_set_class_type(implicit_copy_assignment_function, type_info);
        symbol_entity_specs_set_is_inline(implicit_copy_assignment_function, 1);
        symbol_entity_specs_set_is_defaulted(implicit_copy_assignment_function, 1);

        implicit_copy_assignment_function->type_information = copy_assignment_type;

        implicit_copy_assignment_function->defined = 1;

        symbol_entity_specs_reserve_default_argument_info(implicit_copy_assignment_function, 1);

        symbol_entity_specs_set_is_copy_assignment_operator(implicit_copy_assignment_function, 1);

        class_type_add_member(class_type, implicit_copy_assignment_function, class_context, /* is_definition */ 1);

        char union_has_member_with_nontrivial_copy_assignment = 0;
        if (is_union_type(class_type)
                // This does not apply to anonymous unions
                && !symbol_entity_specs_get_is_anonymous_union(named_type_get_symbol(type_info)))
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
            entry_list_iterator_free(it);
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
        entry_list_iterator_free(it);

        char has_nonstatic_data_member_reference = 0;
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it) && !has_nonstatic_data_member_reference;
                entry_list_iterator_next(it))
        {
            scope_entry_t* data_member = entry_list_iterator_current(it);
            has_nonstatic_data_member_reference = is_any_reference_type(data_member->type_information);
        }
        entry_list_iterator_free(it);

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
        entry_list_iterator_free(it);

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
        entry_list_iterator_free(it);

        if (union_has_member_with_nontrivial_copy_assignment
                || has_nonstatic_data_member_const_of_non_class_type
                || has_nonstatic_data_member_reference
                || has_non_assignment_operator_copiable_data_member
                || has_non_assignment_operator_copiable_base)
        {
            symbol_entity_specs_set_is_deleted(implicit_copy_assignment_function, 1);
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
            if (symbol_entity_specs_get_is_defaulted(current_assignment_operator))
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
                && !symbol_entity_specs_get_is_anonymous_union(named_type_get_symbol(type_info)))
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
            entry_list_iterator_free(it);
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
        entry_list_iterator_free(it);

        char has_nonstatic_data_member_reference = 0;
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it) && !has_nonstatic_data_member_reference;
                entry_list_iterator_next(it))
        {
            scope_entry_t* data_member = entry_list_iterator_current(it);
            has_nonstatic_data_member_reference = is_any_reference_type(data_member->type_information);
        }
        entry_list_iterator_free(it);

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
        entry_list_iterator_free(it);

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
        entry_list_iterator_free(it);

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
            symbol_entity_specs_set_is_member(implicit_move_assignment_function, 1);
            symbol_entity_specs_set_access(implicit_move_assignment_function, AS_PUBLIC);
            symbol_entity_specs_set_class_type(implicit_move_assignment_function, type_info);
            symbol_entity_specs_set_is_inline(implicit_move_assignment_function, 1);
            symbol_entity_specs_set_is_defaulted(implicit_move_assignment_function, 1);

            implicit_move_assignment_function->type_information = move_assignment_type;

            implicit_move_assignment_function->defined = 1;

            symbol_entity_specs_reserve_default_argument_info(implicit_move_assignment_function, 1);

            symbol_entity_specs_set_is_move_assignment_operator(implicit_move_assignment_function, 1);

            class_type_add_member(class_type, implicit_move_assignment_function, class_context, /* is_definition */ 1);

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
            if (symbol_entity_specs_get_is_defaulted(current_assignment_operator))
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
        symbol_entity_specs_set_is_member(implicit_destructor, 1);
        symbol_entity_specs_set_access(implicit_destructor, AS_PUBLIC);
        symbol_entity_specs_set_is_destructor(implicit_destructor, 1);
        symbol_entity_specs_set_class_type(implicit_destructor, type_info);
        symbol_entity_specs_set_is_inline(implicit_destructor, 1);
        implicit_destructor->defined = 1;
        symbol_entity_specs_set_is_defaulted(implicit_destructor, 1);

        class_type_add_member(class_type, implicit_destructor, class_context, /* is_definition */ 1);
        class_type_set_destructor(class_type, implicit_destructor);
        if (is_virtual_destructor(class_type))
        {
            symbol_entity_specs_set_is_virtual(implicit_destructor, 1);
        }

        destructor_determine_if_trivial(
                implicit_destructor,
                all_bases,
                nonstatic_data_members);
    }
    else
    {
        if (symbol_entity_specs_get_is_defaulted(user_declared_destructor))
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

    declare_inherited_constructors(class_type, type_info, decl_context, locus);

    DEBUG_CODE()
    {
        fprintf(stderr, "BUILDSCOPE: Ended class finalization\n");
    }
}

void finish_class_type(type_t* class_type, type_t* type_info, const decl_context_t* decl_context,
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
                /* is_auto_type */ 0,
                /* is_decltype_auto */ 0);

        entry->value = nodecl_init;
    }
    build_scope_delayed_member_declarator_initializer_clear_pending();
}

// Delayed member function declarations (due to default arguments)
struct delayed_function_decl_tag
{
    scope_entry_t* entry;
    const decl_context_t* decl_context;
};

static int _next_delayed_function_decl = 0;
static struct delayed_function_decl_tag _delayed_functions_decl_list[MCXX_MAX_FUNCTIONS_PER_CLASS];

static void build_scope_delayed_add_function_declaration(scope_entry_t* entry, const decl_context_t* decl_context)
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
        AST a, const decl_context_t* decl_context,
        nodecl_t* nodecl_output);

static void build_noexcept_spec_delayed(scope_entry_t* entry)
{
    AST tree = nodecl_get_ast(nodecl_get_child(symbol_entity_specs_get_noexception(entry), 0));
    ERROR_CONDITION(tree == NULL, "Invalid tree", 0);

    DEBUG_CODE()
    {
        fprintf(stderr, "=== Delayed default argument parsing at '%s' ===\n",
                ast_location(tree));
    }

    const decl_context_t* relevant_context = entry->decl_context;

    if (symbol_entity_specs_get_num_related_symbols(entry) > 0)
    {
        // Use the context of the parameters if possible
        relevant_context = symbol_entity_specs_get_related_symbols_num(entry, 0)->decl_context;
    }

    nodecl_t nodecl_output = nodecl_null();
    build_noexcept_spec(entry->type_information,
            tree,
            relevant_context,
            &nodecl_output);
    symbol_entity_specs_set_noexception(entry, nodecl_output);
}

static void build_scope_delayed_function_decl(void)
{
    int i;
    for (i = 0; i < _next_delayed_function_decl; i++)
    {
        scope_entry_t* entry = _delayed_functions_decl_list[i].entry;
        const decl_context_t* decl_context = _delayed_functions_decl_list[i].decl_context;

        int num_parameters = function_type_get_num_parameters(entry->type_information);
        if (function_type_get_has_ellipsis(entry->type_information)) 
            num_parameters--;
        int j;
        for (j = 0; j < num_parameters; j++)
        {
            default_argument_info_t* default_arg = symbol_entity_specs_get_default_argument_info_num(entry, j);
            if (default_arg != NULL
                    && nodecl_get_kind(default_arg->argument) == NODECL_CXX_PARSE_LATER)
            {
                // Let's parse it now
                AST tree = nodecl_get_ast(nodecl_get_child(default_arg->argument, 0));
                ERROR_CONDITION(tree == NULL, "Invalid tree", 0);

                DEBUG_CODE()
                {
                    fprintf(stderr, "=== Delayed default argument parsing at '%s' ===\n",
                            ast_location(tree));
                }

                check_expression(tree, decl_context, &default_arg->argument);
            }
        }

        if (!nodecl_is_null(symbol_entity_specs_get_noexception(entry))
                && nodecl_get_kind(symbol_entity_specs_get_noexception(entry)) == NODECL_CXX_PARSE_LATER)
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
    const decl_context_t* block_context;
    gather_decl_spec_t* gather_info;
};

static int _next_delayed_function_def = 0;
static struct delayed_function_def_tag _delayed_functions_def_list[MCXX_MAX_FUNCTIONS_PER_CLASS];

static void build_scope_delayed_add_delayed_function_def(AST function_definition,
        scope_entry_t* entry,
        const decl_context_t* block_context,
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
        const decl_context_t* block_context,
        gather_decl_spec_t* gather_info,
        nodecl_t *nodecl_output);

static void build_scope_delayed_function_def(nodecl_t* nodecl_output)
{
    int i;
    for (i = 0;  i < _next_delayed_function_def; i++)
    {
        struct delayed_function_def_tag current = _delayed_functions_def_list[i];

        AST function_definition = current.function_definition;
        const decl_context_t* block_context = current.block_context;
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

        DELETE(gather_info);

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

static void insert_symbols_in_enclosing_context(const decl_context_t* enclosing_context,
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
        if (!symbol_entity_specs_get_is_member_of_anonymous(member))
        {
            symbol_entity_specs_set_is_member_of_anonymous(member, 1);
            symbol_entity_specs_set_anonymous_accessor(member,
                nodecl_make_symbol(accessor_symbol, accessor_symbol->locus));
        }
        else
        {
            symbol_entity_specs_set_is_member_of_anonymous(member, 1);

            nodecl_t nodecl_symbol = nodecl_make_symbol(accessor_symbol,
                             accessor_symbol->locus);
            nodecl_set_type(nodecl_symbol, lvalue_ref(accessor_symbol->type_information));

            nodecl_t nodecl_accessor = cxx_integrate_field_accesses(nodecl_symbol,
                    symbol_entity_specs_get_anonymous_accessor(member));
            nodecl_set_type(nodecl_accessor, lvalue_ref(member->type_information));
            nodecl_set_locus(nodecl_accessor, accessor_symbol->locus);

            symbol_entity_specs_set_anonymous_accessor(member, nodecl_accessor);
        }
        insert_entry(enclosing_context->current_scope, member);

        // If the members happen to be one of those faked members to access an
        // anonymous union then recursively add its members to the current scope
        if (member->kind == SK_VARIABLE
                && is_named_class_type(member->type_information)
                && symbol_entity_specs_get_is_anonymous_union(named_type_get_symbol(member->type_information)))
        {
            insert_symbols_in_enclosing_context(enclosing_context,
                    named_type_get_symbol(member->type_information),
                    accessor_symbol);
        }
    }
    entry_list_iterator_free(it);
    entry_list_free(members);
}

scope_entry_t* finish_anonymous_class(scope_entry_t* class_symbol, const decl_context_t* decl_context)
{
    ERROR_CONDITION(!symbol_entity_specs_get_is_anonymous_union(class_symbol), "This class is not anonymous", 0);

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
            class_symbol->decl_context->current_scope,
            accessing_name);

    accessor_symbol->kind = SK_VARIABLE;
    accessor_symbol->locus = class_symbol->locus;
    accessor_symbol->type_information = get_user_defined_type(class_symbol);

    symbol_entity_specs_set_anonymous_accessor(class_symbol,
        nodecl_make_symbol(accessor_symbol, class_symbol->locus));

    // Sign in members in the appropiate enclosing scope
    insert_symbols_in_enclosing_context(decl_context, class_symbol, accessor_symbol);

    return accessor_symbol;
}

/*
 * This function is called for class specifiers
 */
void gather_type_spec_from_class_specifier(AST a, type_t** type_info,
        gather_decl_spec_t* gather_info,
        const decl_context_t* decl_context,
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
        if (decl_context->current_scope->kind == CLASS_SCOPE)
        {
            decl_context = decl_context->current_scope->related_entry->decl_context;
        }
    }

    if (gather_info->is_friend
            && gather_info->no_declarators)
    {
        error_printf_at(ast_get_locus(a), "friend applied to class definition\n");
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

    switch (ASTKind(class_key))
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

    decl_context_t* inner_decl_context;

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
                    && ASTKind(class_id_expression) != AST_TEMPLATE_ID)
            {
                scope_entry_t* template_sym = class_entry;
                if (decl_context->template_parameters == NULL)
                {
                    error_printf_at(ast_get_locus(class_id_expression), "template parameters required for declaration of '%s'\n",
                            get_qualified_symbol_name(template_sym, decl_context));
                    *type_info = get_error_type();
                    return;
                }

                if (decl_context->template_parameters->num_parameters
                        != template_type_get_template_parameters(template_sym->type_information)->num_parameters)
                {
                    error_printf_at(ast_get_locus(class_id_expression), "redeclaration with %d template parameters while previous declaration used %d\n",
                            decl_context->template_parameters->num_parameters,
                            template_type_get_template_parameters(template_sym->type_information)->num_parameters);
                    *type_info = get_error_type();
                    return;
                }

                template_type_update_template_parameters(template_sym->type_information,
                        decl_context->template_parameters);

                // If it was friend-declared, it is not anymore
                if (symbol_entity_specs_get_is_friend_declared(template_sym))
                    symbol_entity_specs_set_is_friend_declared(template_sym, 0);

                // This is a named type
                type_t* primary_type = template_type_get_primary_type(template_sym->type_information);

                class_entry = named_type_get_symbol(primary_type);
                class_type = class_entry->type_information;

                if (is_template_explicit_specialization(decl_context->template_parameters))
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
                            class_entry->decl_context->template_parameters,
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
                    error_printf_at(ast_get_locus(class_id_expression), "invalid template-name redeclaration\n");
                    *type_info = get_error_type();
                    return;
                }

                ERROR_CONDITION(!is_class_type(class_entry->type_information), "This must be a class type", 0);
            }

            nesting_check_t nest_check = check_template_nesting_of_name(class_entry, decl_context->template_parameters);

            if (nest_check != NESTING_CHECK_OK)
            {
                if (nest_check == NESTING_CHECK_NOT_A_TEMPLATE)
                {
                    error_printf_at(ast_get_locus(class_id_expression), "'%s' is not a template type\n",
                            get_qualified_symbol_name(class_entry, decl_context));
                }
                else if (nest_check == NESTING_CHECK_INVALID)
                {
                    error_printf_at(ast_get_locus(class_id_expression), "invalid nesting of template parameters in template declaration\n");
                    error_printf_at(ast_get_locus(class_id_expression), "there are %d levels of template parameters but the symbol required exactly %d levels\n",
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
            if (symbol_entity_specs_get_is_friend_declared(class_entry))
                symbol_entity_specs_set_is_friend_declared(class_entry, 0);

            DEBUG_CODE()
            {
                fprintf(stderr, "BUILDSCOPE: Class '%s' already declared as %p in scope %p (%s)\n", 
                        prettyprint_in_buffer(class_id_expression),
                        class_entry, 
                        class_entry->decl_context->current_scope,
                        locus_to_str(class_entry->locus));
            }

            if (class_entry->defined
                    || (symbol_entity_specs_get_alias_to(class_entry) != NULL
                        && symbol_entity_specs_get_alias_to(class_entry)->defined))
            {
                error_printf_at(ast_get_locus(class_id_expression), "class '%s' already defined\n",
                        get_qualified_symbol_name(class_entry, class_entry->decl_context));
                info_printf_at(class_symbol_get_canonical_symbol(class_entry)->locus, "location of previous definition\n");
                *type_info = get_error_type();
                return;
            }

            if (is_template_specialized_type(class_entry->type_information))
            {
                // Check the enclosing namespace scope
                // This is only valid if the scope of the entry is an inlined namespace of the current one
                if ((class_entry->decl_context->namespace_scope != decl_context->namespace_scope)
                        // The primary type can be defined anywhere since it is not a specialization per se
                        && (named_type_get_symbol(
                                template_type_get_primary_type(
                                template_specialized_type_get_related_template_type(
                                    class_entry->type_information))) != class_entry)
                        && !is_inline_namespace_of(class_entry->decl_context, decl_context))
                {
                    error_printf_at(ast_get_locus(class_id_expression), "specialization of '%s' in different namespace from definition\n",
                            prettyprint_in_buffer(class_id_expression));
                }

                if (!gather_info->is_explicit_specialization)
                {
                    template_specialized_type_update_template_parameters(class_entry->type_information,
                            decl_context->template_parameters);
                    template_specialized_type_update_template_parameters(
                            class_symbol_get_canonical_symbol(class_entry)->type_information,
                            decl_context->template_parameters);
                }
            }

            // Update the template_scope
            DEBUG_CODE()
            {
                fprintf(stderr, "BUILDSCOPE: Updating template scope\n");
            }
            decl_context_t *updated_decl_context = decl_context_clone(class_entry->decl_context);
            updated_decl_context->template_parameters = decl_context->template_parameters;
            class_entry->decl_context = updated_decl_context;

            inner_decl_context = new_class_context(class_entry->decl_context, class_entry);

            // Remove empty template headers if they are produced by an explicit template specialization
            while (inner_decl_context->template_parameters != NULL &&
                    inner_decl_context->template_parameters->is_explicit_specialization)
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

                inner_decl_context->template_parameters = inner_decl_context->template_parameters->enclosing;
            }

            class_type_set_inner_context(class_type, inner_decl_context);
        }
        else if (filtered_class_entry_list == NULL
                && is_unqualified_id_expression(class_id_expression))
        {
            // If no class found and no nested name, the symbol must be created
            // here
            if (ASTKind(class_id_expression) == AST_SYMBOL)
            {
                C_LANGUAGE()
                {
                    const char* class_name = ASTText(class_id_expression);
                    class_name = strappend(class_kind_name, strappend(" ", class_name));

                    class_entry = new_symbol(decl_context, 
                            decl_context->current_scope, class_name);
                }

                CXX_LANGUAGE()
                {
                    class_entry = new_symbol(decl_context, 
                            decl_context->current_scope, 
                            ASTText(class_id_expression));
                }
            }
            else if (ASTKind(class_id_expression) == AST_TEMPLATE_ID)
            {
                error_printf_at(ast_get_locus(class_id_expression), "template class-name '%s' not found in the current scope\n",
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
                        prettyprint_in_buffer(class_id_expression), class_entry, decl_context->current_scope);
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
                if (ASTKind(class_id_expression) != AST_TEMPLATE_ID)
                {
                    if (!check_class_template_parameters(ast_get_locus(a), decl_context->template_parameters))
                    {
                        *type_info = get_error_type();
                        return;
                    }

                    class_entry->kind = SK_TEMPLATE;
                    class_entry->type_information = get_new_template_type(decl_context->template_parameters,
                            get_new_class_type(decl_context, class_kind),
                            ASTText(class_id_expression), decl_context,
                            ast_get_locus(class_id_expression));
                    template_type_set_related_symbol(class_entry->type_information, class_entry);

                    class_entry->locus = ast_get_locus(class_id_expression);

                    // Set it as a member if needed
                    if (decl_context->current_scope->kind == CLASS_SCOPE)
                    {
                        symbol_entity_specs_set_is_member(class_entry, 1);
                        // FIXME
                        // symbol_entity_specs_set_access(class_entry, current_access);
                        symbol_entity_specs_set_class_type(class_entry,
                            get_user_defined_type(decl_context->current_scope->related_entry));
                    }

                    // Now update class_entry to be a real class
                    class_entry = named_type_get_symbol(
                            template_type_get_primary_type(class_entry->type_information)
                            );

                    class_type = class_entry->type_information;
                }
                else
                {
                    error_printf_at(ast_get_locus(class_id_expression), "invalid template-name '%s'\n",
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
            error_printf_at(ast_get_locus(a), "class '%s' not found\n", prettyprint_in_buffer(class_id_expression));
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
            class_entry = new_symbol(decl_context, decl_context->current_scope, symbol_name);
        }
        CXX_LANGUAGE()
        {
            uniquestr_sprintf(&symbol_name, "mcc_%s_anon_%d", class_kind_name, anonymous_classes);
            class_entry = NEW0(scope_entry_t);
            class_entry->symbol_name = symbol_name;
            class_entry->decl_context = decl_context;
        }
        ERROR_CONDITION(class_entry == NULL, "Invalid symbol", 0);

        anonymous_classes++;

        class_entry->kind = SK_CLASS;
        class_entry->type_information = get_new_class_type(decl_context, class_kind);
        class_type = class_entry->type_information;

        class_entry->locus = ast_get_locus(a);

        symbol_entity_specs_set_is_unnamed(class_entry, 1);

        inner_decl_context = new_class_context(decl_context, class_entry);
        class_type_set_inner_context(class_type, inner_decl_context);

        symbol_entity_specs_set_is_anonymous_union(class_entry, gather_info->no_declarators);
    }

    ERROR_CONDITION(inner_decl_context->current_scope == NULL,
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

    // This may overwrite the class kind, but this is OK
    class_type_set_class_kind(class_type, class_kind);

    // Compute *type_info as it is needed by build_scope_member_specification
    *type_info = get_user_defined_type(class_entry);

    // Save the class symbol

    // If the class is being declared in class-scope it means
    // it is a nested class
    if (decl_context->current_scope->kind == CLASS_SCOPE)
    {
        scope_entry_t* enclosing_class_symbol = decl_context->current_scope->related_entry;
        type_t* enclosing_class_type = enclosing_class_symbol->type_information;

        class_type_add_member(enclosing_class_type, class_entry, decl_context, /* is_definition */ 1);

        CXX_LANGUAGE()
        {
            symbol_entity_specs_set_is_member(class_entry, 1);
            symbol_entity_specs_set_access(class_entry, gather_info->current_access);
            symbol_entity_specs_set_class_type(class_entry, get_user_defined_type(enclosing_class_symbol));
            symbol_entity_specs_set_is_defined_inside_class_specifier(class_entry, 1);
        }
        class_type_set_enclosing_class_type(class_type, get_user_defined_type(enclosing_class_symbol));

        // If the enclosing class is dependent, so is this one
        char c = is_dependent_type(class_entry->type_information);
        c = c || is_dependent_type(enclosing_class_type);
        set_is_dependent_type(class_entry->type_information, c);
    }
    else if (decl_context->current_scope->kind == BLOCK_SCOPE)
    {
        // This is a local class
        scope_entry_t* enclosing_function = decl_context->current_scope->related_entry;
        if (enclosing_function != NULL
                && (is_dependent_type(enclosing_function->type_information)
                    || (symbol_entity_specs_get_is_member(enclosing_function)
                        && is_dependent_type(symbol_entity_specs_get_class_type(enclosing_function)))))
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
        if (!symbol_entity_specs_get_is_unnamed(class_entry))
        {
            scope_entry_t* injected_symbol = new_symbol(inner_decl_context,
                    inner_decl_context->current_scope,
                    class_entry->symbol_name);

            *injected_symbol = *class_entry;
            // the injected class name is logically in the class-scope
            injected_symbol->decl_context = inner_decl_context;
            injected_symbol->do_not_print = 1;

            symbol_entity_specs_set_is_member(injected_symbol, 1);
            symbol_entity_specs_set_access(injected_symbol, AS_PUBLIC);
            symbol_entity_specs_set_class_type(injected_symbol, get_user_defined_type(class_entry));

            symbol_entity_specs_set_is_injected_class_name(injected_symbol, 1);
        }

        register_symbol_this_in_class_scope(class_entry);
    }

    access_specifier_t current_access;
    // classes have a private by default
    if (ASTKind(class_key) == AST_CLASS_KEY_CLASS)
    {
        current_access = AS_PRIVATE;
    }
    // otherwise this is public (for union and structs)
    else
    {
        current_access = AS_PUBLIC;
    }

    symbol_entity_specs_set_is_user_declared(class_entry, 1);
    if (class_type_is_incomplete_independent(class_entry->type_information))
    {
        symbol_entity_specs_set_is_instantiated(class_entry, 1);
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

    symbol_entity_specs_set_is_instantiable(class_entry, 1);
    symbol_entity_specs_set_is_instantiable(class_symbol_get_canonical_symbol(class_entry), 1);

    keep_extra_attributes_in_symbol(class_entry, gather_info);

    // Keep class-virt-specifiers
    symbol_entity_specs_set_is_explicit(class_entry, gather_info->is_explicit);
    symbol_entity_specs_set_is_final(class_entry, gather_info->is_final);

    // Propagate the __extension__ attribute to the symbol
    symbol_entity_specs_set_gcc_extension(class_entry, gcc_extension);

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

void build_scope_member_specification_first_step(const decl_context_t* inner_decl_context,
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
        if (ASTKind(member_specification) == AST_MEMBER_ACCESS_SPEC)
        {
            switch (ASTKind(ASTSon0(member_specification)))
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
                    internal_error("Unknown node type '%s'\n", ast_print_node_type(ASTKind(ASTSon0(member_specification))));
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

static void build_scope_member_specification(const decl_context_t* inner_decl_context, AST member_specification_tree, 
        access_specifier_t default_current_access, type_t* type_info, nodecl_t *nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list)
{
    ERROR_CONDITION(inner_decl_context->current_scope->kind != CLASS_SCOPE,
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
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    build_scope_declarator_with_parameter_context(a,
            gather_info, type_info, declarator_type, decl_context,
            /* prototype_context */ NULL, nodecl_output);
}

static template_parameter_list_t* duplicate_template_parameter_list(
        template_parameter_list_t* orig_tpl_list)
{
    template_parameter_list_t *current_tpl_list = NEW(template_parameter_list_t);
    *current_tpl_list = *orig_tpl_list;
    current_tpl_list->parameters = NEW_VEC(template_parameter_t*, current_tpl_list->num_parameters);
    memcpy(current_tpl_list->parameters,
            orig_tpl_list->parameters,
            sizeof(*orig_tpl_list->parameters) * orig_tpl_list->num_parameters);

    return current_tpl_list;
}

static template_parameter_list_t* hide_template_parameters_because_of_members(
        const decl_context_t* decl_context,
        scope_entry_t* member,
        template_parameter_list_t* orig_tpl_list)
{
    if (orig_tpl_list == NULL)
        return NULL;

    template_parameter_list_t* enclosing = hide_template_parameters_because_of_members(
            decl_context,
            member,
            orig_tpl_list->enclosing);

    template_parameter_list_t* current_tpl_list = orig_tpl_list;

    if (enclosing != orig_tpl_list->enclosing)
    {
        // Duplicate the list if the enclosing list has changed
        current_tpl_list = duplicate_template_parameter_list(orig_tpl_list);
        current_tpl_list->enclosing = enclosing;
    }

    int i;
    for (i = 0; i < current_tpl_list->num_parameters; i++)
    {
        template_parameter_t* current_tpl = current_tpl_list->parameters[i];

        if (current_tpl == NULL
                || current_tpl->entry == NULL
                || current_tpl->entry->symbol_name == NULL)
            continue;


        nodecl_t nodecl_name = nodecl_make_cxx_dep_name_simple(
                current_tpl->entry->symbol_name,
                current_tpl->entry->locus);

        scope_entry_list_t *entry_list = query_nodecl_name_in_class(
                decl_context,
                member,
                nodecl_name,
                /* field_path */ NULL);

        nodecl_free(nodecl_name);

        char hidden_by_class = (entry_list != NULL);
        entry_list_free(entry_list);

        if (hidden_by_class)
        {
            if (current_tpl_list == orig_tpl_list)
            {
                // Duplicate the list if it has to change
                current_tpl_list = duplicate_template_parameter_list(orig_tpl_list);
            }

            DEBUG_CODE()
            {
                fprintf(stderr, "BUILDSCOPE: Hiding template parameter '%s' (%d, %d)\n",
                        current_tpl->entry->symbol_name,
                        symbol_entity_specs_get_template_parameter_nesting(current_tpl->entry),
                        symbol_entity_specs_get_template_parameter_position(current_tpl->entry));
            }

            template_parameter_t* new_tpl = NEW(template_parameter_t);
            *new_tpl = *current_tpl;

            // Make a clone here
            new_tpl->entry = NEW0(scope_entry_t);
            *new_tpl->entry = *current_tpl->entry;
            symbol_entity_specs_copy_from(new_tpl->entry, current_tpl->entry);
            uniquestr_sprintf(&new_tpl->entry->symbol_name, "__hidden_tpl__param_%d_%d__",
                    symbol_entity_specs_get_template_parameter_nesting(current_tpl->entry),
                    symbol_entity_specs_get_template_parameter_position(current_tpl->entry));

            current_tpl_list->parameters[i] = new_tpl;
        }
    }

    return current_tpl_list;
}

/*
 * This is the actual implementation of 'compute_declarator_type'
 */
static void build_scope_declarator_with_parameter_context(AST declarator, 
        gather_decl_spec_t* gather_info, type_t* type_info, type_t** declarator_type,
        const decl_context_t* decl_context, const decl_context_t* *prototype_context,
        nodecl_t* nodecl_output)
{
    *declarator_type = type_info;

    if (declarator != NULL)
    {
        gather_extra_attributes_in_declarator(declarator, gather_info, decl_context);
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
                *declarator_type = get_vector_type_by_bytes(gather_info->mode_type, 
                        gather_info->vector_size);
            }
        }
        else
        {
            // We do not want declarator 'vector 16 to volatile float' but declarator 
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
                        get_vector_type_by_bytes(base_vector_type,
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
            error_printf_at(ast_get_locus(declarator), "'mode' attribute is only valid for integral or floating types\n");
        }
        else
        {
            *declarator_type = gather_info->mode_type;
        }
    }

    if (declarator != NULL)
    {
        AST declarator_name = get_declarator_name(declarator, decl_context);

        const decl_context_t* entity_context = decl_context;
        // Adjust context if the name is qualified and this is not declarator friend
        if (declarator_name != NULL
                && ASTKind(declarator_name) == AST_QUALIFIED_ID
                // friends do not adjust their context, otherwise they would not be
                // parsable correctly
                && !gather_info->is_friend)
        {
            // If it is qualified it must be declared previously
            // We are not interested in anything but the context of any of the symbols

            AST global_op = ASTSon0(declarator_name);
            AST nested_name = ASTSon1(declarator_name);
            AST name = ASTSon2(declarator_name);

            // Extra check for this case X::A::A<int>
            if (ASTKind(name) == AST_TEMPLATE_ID
                    // The last component of the nested-name-spec
                    && ASTKind(ASTSon0(name)) == AST_SYMBOL
                    && ASTKind(ASTSon1(nested_name)) == AST_SYMBOL
                    && strcmp(ASTText(ASTSon1(nested_name)),
                        ASTText(ASTSon0(name))) == 0)
            {
                // Converts the lookup below into X::A::A
                name = ASTSon0(name);
            }

            scope_entry_list_t* symbols = query_nested_name(decl_context,
                    global_op, nested_name, name, NULL);

            if (symbols == NULL)
            {
                error_printf_at(ast_get_locus(declarator_name), "qualified name '%s' not found\n",
                        prettyprint_in_buffer(declarator_name));
                *declarator_type = get_error_type();
                return;
            }

            scope_entry_t* first_symbol = entry_list_head(symbols);
            entry_list_free(symbols);

            // Update the entity context, inheriting the template_scope
            decl_context_t* updated_entity_context = decl_context_clone(first_symbol->decl_context);
            if (symbol_entity_specs_get_is_member(first_symbol))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "BUILDSCOPE: The qualified symbol is a member, checking if we have to hide template parameters\n");
                }
                updated_entity_context->template_parameters =
                    hide_template_parameters_because_of_members(decl_context,
                            named_type_get_symbol(symbol_entity_specs_get_class_type(first_symbol)),
                            decl_context->template_parameters);
                DEBUG_CODE()
                {
                    if (updated_entity_context->template_parameters == decl_context->template_parameters)
                    {
                        // fprintf(stderr, "BUILDSCOPE: No template parameter was hidden\n");
                    }
                    else
                    {
                        fprintf(stderr, "BUILDSCOPE: %s Some template parameters were hidden\n",
                                ast_location(declarator_name));
                    }
                }
            }
            else
            {
                updated_entity_context->template_parameters = decl_context->template_parameters;
            }

            entity_context = updated_entity_context;

            if (prototype_context != NULL)
            {
                decl_context_t* updated_prototype_context = decl_context_clone(*prototype_context);
                updated_prototype_context->template_parameters = entity_context->template_parameters;
                updated_prototype_context->current_scope->contained_in = first_symbol->decl_context->current_scope;
                updated_prototype_context->namespace_scope = first_symbol->decl_context->namespace_scope;
                updated_prototype_context->class_scope = first_symbol->decl_context->class_scope;

                *prototype_context = updated_prototype_context;
            }
        }

        // Register 'this' for declarator successful parsing of the declarator
        if (prototype_context != NULL
                && (*prototype_context)->current_scope->kind == BLOCK_SCOPE
                && entity_context->current_scope->kind == CLASS_SCOPE)
        {
            register_symbol_this(*prototype_context,
                    entity_context->current_scope->related_entry,
                    ast_get_locus(declarator));
        }

        // Second traversal, here we build the type
        build_scope_declarator_rec(declarator,
                                   declarator_type,
                                   gather_info,
                                   decl_context,
                                   entity_context,
                                   prototype_context,
                                   /* is_top_level_declarator */ 1,
                                   nodecl_output);

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
                    if (ASTKind(id_expression) == AST_QUALIFIED_ID)
                    {
                        if (ASTKind(ASTSon2(id_expression)) == AST_CONVERSION_FUNCTION_ID)
                        {
                            conversion_function_id = ASTSon2(id_expression);
                        }
                    }

                    if (ASTKind(id_expression) == AST_CONVERSION_FUNCTION_ID)
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
                else if (ASTKind(declarator_name) == AST_DESTRUCTOR_ID
                        || ASTKind(declarator_name) == AST_DESTRUCTOR_TEMPLATE_ID)
                {
                    // Patch the type of the function of declarator destructor so it
                    // works for const objects as well
                    *declarator_type = get_const_qualified_type(*declarator_type);
                }
            }
        }

        DEBUG_CODE()
        {
            // fprintf(stderr, "BUILDSCOPE: Computed type of '%s' is  '%s'\n", 
            //         prettyprint_in_buffer(declarator),
            //         print_declarator(*declarator_type));
        }
    }
}

/*
 * This functions converts a type "T" to a "pointer to T"
 */
static void set_pointer_type(type_t** declarator_type, AST pointer_tree, 
        const decl_context_t* decl_context)
{
    type_t* pointee_type = *declarator_type;

    switch (ASTKind(pointer_tree))
    {
        case AST_POINTER_SPEC :
            {
                if (ASTSon0(pointer_tree) == NULL)
                {
                    if (!is_dependent_type(pointee_type)
                            && is_any_reference_type(pointee_type))
                    {
                        error_printf_at(ast_get_locus(pointer_tree), "attempt to create a pointer to reference\n");
                        *declarator_type = get_error_type();
                        return;
                    }
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

                        scope_entry_t* checked_symbol = entry;

                        if (checked_symbol->kind == SK_TYPEDEF)
                        {
                            type_t* t = advance_over_typedefs(checked_symbol->type_information);
                            if (is_named_type(t))
                            {
                                checked_symbol = named_type_get_symbol(t);
                            }
                        }

                        if (checked_symbol->kind != SK_CLASS
                                && checked_symbol->kind != SK_DEPENDENT_ENTITY
                                && checked_symbol->kind != SK_TEMPLATE_TYPE_PARAMETER
                                && checked_symbol->kind != SK_TEMPLATE_TYPE_PARAMETER_PACK
                                && checked_symbol->kind != SK_TEMPLATE_ALIAS)
                        {
                            error_printf_at(ast_get_locus(id_type_expr), "'%s' is not valid as the class-name of a pointer to member\n",
                                    entry->symbol_name);
                            *declarator_type = get_error_type();
                            return;
                        }

                        if (entry->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK
                                && !get_is_inside_pack_expansion())
                        {
                            error_printf_at(ast_get_locus(id_type_expr), "invalid template parameter pack '%s' not inside a pack expansion\n",
                                    entry->symbol_name);
                            *declarator_type = get_error_type();
                            return;
                        }

                        if (symbol_entity_specs_get_is_injected_class_name(entry))
                        {
                            // Advance this case as it will lead to a simpler type-id
                            entry = named_type_get_symbol(symbol_entity_specs_get_class_type(entry));
                        }

                        *declarator_type = get_pointer_to_member_type(pointee_type, get_user_defined_type(entry));
                    }
                    else
                    {
                        error_printf_at(ast_get_locus(id_type_expr), "class-name '%s' not found\n",
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
            internal_error("Unhandled node type '%s'\n", ast_print_node_type(ASTKind(pointer_tree)));
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
static void set_array_type(type_t **declarator_type,
                           AST constant_expr,
                           AST static_qualifier,
                           AST cv_qualifier_seq UNUSED_PARAMETER,
                           gather_decl_spec_t *gather_info,
                           const decl_context_t *decl_context,
                           const locus_t *locus,
                           char is_top_level_declarator)
{
    type_t* element_type = *declarator_type;

    if (element_type == NULL)
    {
        error_printf_at(locus, "array declaration without a type-specifier\n");
        *declarator_type = get_error_type();
        return;
    }

    if (static_qualifier != NULL)
    {
        if (gather_info->parameter_declaration && is_top_level_declarator)
            warn_printf_at(ast_get_locus(static_qualifier),
                           "'static' qualifier ignored in array declarator\n");
        else
            error_printf_at(ast_get_locus(static_qualifier),
                            "invalid 'static' qualifier in array declarator\n");
    }

    nodecl_t nodecl_expr = nodecl_null();
    if (constant_expr != NULL)
    {
        if (!check_expression(constant_expr, decl_context, &nodecl_expr))
        {
            error_printf_at(ast_get_locus(constant_expr), "could not check array size expression '%s'\n",
                    prettyprint_in_buffer(constant_expr));

            *declarator_type = get_error_type();
            return;
        }

        nodecl_expr = nodecl_expression_make_rvalue(nodecl_expr, decl_context);

        if (!nodecl_expr_is_value_dependent(nodecl_expr)
                && !nodecl_is_constant(nodecl_expr))
        {
            if (decl_context->current_scope->kind == NAMESPACE_SCOPE
                    || decl_context->current_scope->kind == CLASS_SCOPE)
            {
                error_printf_at(ast_get_locus(constant_expr), "declaring a variable sized object in a scope not allowing them\n");
                *declarator_type = get_error_type();
                return;
            }
            // // Maybe we should check for decl_context->block_scope != NULL
            else if (decl_context->current_scope->kind == BLOCK_SCOPE
                    && !gather_info->is_cxx_new_declarator)
            {
                const char* vla_name = NULL;
                uniquestr_sprintf(&vla_name, "mcc_vla_%d", get_vla_counter());

                scope_entry_t* new_vla_dim = new_symbol(decl_context, decl_context->current_scope, vla_name);

                new_vla_dim->kind = SK_VARIABLE;
                new_vla_dim->locus = ast_get_locus(constant_expr);

                if (!equivalent_types(
                            get_unqualified_type(no_ref(nodecl_get_type(nodecl_expr))),
                            get_ptrdiff_t_type()))
                {
                    nodecl_expr = nodecl_make_conversion(nodecl_expr,
                            get_ptrdiff_t_type(),
                            nodecl_get_locus(nodecl_expr));
                }

                new_vla_dim->value = nodecl_expr;
                new_vla_dim->type_information = get_const_qualified_type(get_ptrdiff_t_type());

                // It's not user declared code, but we must generate it.
                // For this reason, we do this trick
                symbol_entity_specs_set_is_user_declared(new_vla_dim, 1);
                symbol_entity_specs_set_is_saved_expression(new_vla_dim, 1);

                P_LIST_ADD(gather_info->vla_dimension_symbols,
                        gather_info->num_vla_dimension_symbols,
                        new_vla_dim);

                nodecl_expr = nodecl_make_symbol(new_vla_dim, new_vla_dim->locus);
                nodecl_set_type(nodecl_expr, new_vla_dim->type_information);
            }
            else if (decl_context->current_scope->kind == PROTOTYPE_SCOPE)
            {
                // Do nothing, keep the expression as is, this is what we want, actually
            }
        }
    }

    if (is_void_type(element_type))
    {
        error_printf_at(locus, "attempt to create an array of void type\n");
        *declarator_type = get_error_type();
        return;
    }
    else if (!is_dependent_type(element_type))
    {
        if (is_any_reference_type(element_type))
        {
            error_printf_at(locus, "attempt to create an array of reference type\n");
            *declarator_type = get_error_type();
            return;
        }
        else if (is_function_type(element_type))
        {
            error_printf_at(locus, "attempt to create an array of function type\n");
            *declarator_type = get_error_type();
            return;
        }
    }

    if (nodecl_is_constant(nodecl_expr)
            && const_value_is_zero(
                const_value_gte(
                    nodecl_get_constant(nodecl_expr),
                    const_value_get_zero(/*bytes*/ 4, /* sign*/ 1))))
    {
        error_printf_at(locus, "attempt to create an array of negative size\n");
        *declarator_type = get_error_type();
        return;
    }

    C_LANGUAGE()
    {
        if (is_incomplete_type(element_type))
        {
            error_printf_at(locus, "invalid array of incomplete type '%s'\n",
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
            error_printf_at(locus, "declaration of array type of an unbounded array type '%s'\n",
                    print_type_str(element_type, decl_context));
            *declarator_type = get_error_type();
            return;
        }
    }
    *declarator_type = get_array_type(element_type, nodecl_expr, decl_context);

    if (cv_qualifier_seq != NULL)
    {
        if (gather_info->parameter_declaration && is_top_level_declarator)
        {
            cv_qualifier_t cv_qualif = compute_cv_qualifier(cv_qualifier_seq);
            *declarator_type
                = get_cv_qualified_array_type(*declarator_type, cv_qualif);
        }
        else
        {
            error_printf_at(ast_get_locus(cv_qualifier_seq),
                            "invalid type qualifier in array declarator\n");
        }
    }
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
        _decl_proxy = NEW0(scope_entry_t);
        _decl_proxy->symbol_name = UNIQUESTR_LITERAL("._function_declarator_");
        _decl_proxy->kind = SK_FUNCTION;
    }

    return _decl_proxy;
}

char type_does_not_contain_any_template_parameter_pack(type_t* t, const locus_t* locus)
{
    scope_entry_t** packs = NULL;
    int num_packs = 0;

    get_packs_in_type(t, &packs, &num_packs);
    DELETE(packs);

    if (num_packs == 0)
    {
        error_printf_at(locus, "pack expansion does not contain any template-parameter pack\n");
    }

    return num_packs == 0;
}

/*
 * This function fetches information for every declarator in the
 * parameter_declaration_clause of a functional declarator
 */
static void set_function_parameter_clause(type_t** function_type, 
        AST parameters, AST ref_qualifier_opt,
        const decl_context_t* decl_context,
        gather_decl_spec_t* gather_info,
        nodecl_t* nodecl_output)
{
    parameter_info_t parameter_info[MCXX_MAX_FUNCTION_PARAMETERS];
    memset(parameter_info, 0, sizeof(parameter_info));
    int num_parameters = 0;

    if (ASTKind(parameters) == AST_AMBIGUITY)
    {
        solve_ambiguous_parameter_clause(parameters, decl_context);
    }

    ref_qualifier_t ref_qualifier = REF_QUALIFIER_NONE;
    if (ref_qualifier_opt != NULL)
    {
        CXX03_LANGUAGE()
        {
            error_printf_at(ast_get_locus(ref_qualifier_opt), "ref-qualifier is only valid in C++2011\n");
        }
        switch (ASTKind(ref_qualifier_opt))
        {
            case AST_REFERENCE_SPEC:
                ref_qualifier = REF_QUALIFIER_LVALUE;
                break;
            case AST_RVALUE_REFERENCE_SPEC:
                ref_qualifier = REF_QUALIFIER_RVALUE;
                break;
            default:
                internal_error("Invalid ref-qualifier '%s'\n", ast_print_node_type(ASTKind(ref_qualifier_opt)));
        }
    }

    if (ASTKind(parameters) == AST_EMPTY_PARAMETER_DECLARATION_CLAUSE)
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
    if (ASTKind(parameters) == AST_KR_PARAMETER_LIST)
    {
        list = ASTSon0(parameters);

        if (list != NULL)
        {
            for_each_element(list, iter)
            {
                if (num_parameters > MCXX_MAX_FUNCTION_PARAMETERS)
                {
                    error_printf_at(ast_get_locus(parameters), "too many parameters (more than %d) in function declaration\n",
                            num_parameters);
                }

                // Clear this parameter_info 
                memset(&(parameter_info[num_parameters]), 0, sizeof(parameter_info[num_parameters]));
                AST kr_id = ASTSon1(iter);

                const decl_context_t* param_decl_context = decl_context;
                scope_entry_t* new_parameter = new_symbol(param_decl_context, 
                        param_decl_context->current_scope,
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
                error_printf_at(ast_get_locus(parameters), "too many parameters (more than %d) in function declaration\n",
                        num_parameters);
            }

            // Clear this parameter_info 
            memset(&(parameter_info[num_parameters]), 0, sizeof(parameter_info[num_parameters]));

            AST parameter_declaration = ASTSon1(iter);

            if (ASTKind(parameter_declaration) == AST_AMBIGUITY)
            {
                solve_ambiguous_parameter_decl(parameter_declaration, decl_context);
                ERROR_CONDITION((ASTKind(parameter_declaration) == AST_AMBIGUITY), "Ambiguity not solved %s", 
                        ast_location(parameter_declaration));
            }

            if (ASTKind(parameter_declaration) == AST_VARIADIC_ARG)
            {
                // Nothing more to do
                parameter_info[num_parameters].is_ellipsis = 1;
                parameter_info[num_parameters].type_info = get_ellipsis_type();
                num_parameters++;
                continue;
            }

            ERROR_CONDITION(ASTKind(parameter_declaration) != AST_PARAMETER_DECL,
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
                        error_printf_at(ast_get_locus(default_argument), "could not check default argument expression '%s'\n",
                                prettyprint_in_buffer(default_argument));

                        *function_type = get_error_type();
                        return;
                    }
                }
            }

            gather_decl_spec_t param_decl_gather_info;
            memset(&param_decl_gather_info, 0, sizeof(param_decl_gather_info));

            type_t* simple_type_info;

            const decl_context_t* param_decl_context = decl_context;
            param_decl_gather_info.parameter_declaration = 1;

            char keep_is_inside_pack_expansion = get_is_inside_pack_expansion();
            char this_is_a_pack = 0;
            if (get_declarator_id_pack(parameter_declarator, decl_context) != NULL)
            {
                set_is_inside_pack_expansion(1);
                this_is_a_pack = 1;
            }

            build_scope_decl_specifier_seq(parameter_decl_spec_seq,
                    &param_decl_gather_info, &simple_type_info,
                    param_decl_context, nodecl_output);

            // Note that this can only happen in C++ since in C we inject an implicit int
            if (simple_type_info == NULL)
            {
                error_printf_at(ast_get_locus(parameter_decl_spec_seq), "missing type-specifier in parameter declaration\n");
                simple_type_info = get_error_type();
            }

            if (is_error_type(simple_type_info))
            {
                set_is_inside_pack_expansion(keep_is_inside_pack_expansion);

                *function_type = get_error_type();
                return;
            }

            AST attribute_list = ASTSon3(parameter_declaration);
            gather_extra_attributes(attribute_list, &param_decl_gather_info, param_decl_context);

            if (param_decl_gather_info.is_extern)
            {
                error_printf_at(ast_get_locus(parameter_decl_spec_seq), "parameter declared as 'extern'\n");

                set_is_inside_pack_expansion(keep_is_inside_pack_expansion);

                *function_type = get_error_type();
                return;
            }
            if (param_decl_gather_info.is_static)
            {
                error_printf_at(ast_get_locus(parameter_decl_spec_seq), "parameter declared as 'static'\n");

                set_is_inside_pack_expansion(keep_is_inside_pack_expansion);

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
                set_is_inside_pack_expansion(keep_is_inside_pack_expansion);

                *function_type = get_error_type();
                return;
            }

            if (parameter_declarator != NULL)
            {
                entry = build_scope_declarator_name(parameter_declarator,
                        simple_type_info, type_info,
                        &param_decl_gather_info, param_decl_context);
            }

            if (is_void_type(type_info))
            {
                if (entry != NULL)
                {
                    set_is_inside_pack_expansion(keep_is_inside_pack_expansion);

                    error_printf_at(ast_get_locus(parameter_decl_spec_seq), "parameter '%s' declared as void\n",
                            entry->symbol_name);
                    *function_type = get_error_type();
                    return;
                }
                else if (ASTSon0(iter) != NULL
                        || (num_parameters != 0))
                {
                    set_is_inside_pack_expansion(keep_is_inside_pack_expansion);

                    error_printf_at(ast_get_locus(parameter_decl_spec_seq), "parameter declared as void\n");
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
                entry = register_new_var_or_fun_name(declarator_id, type_info, &param_decl_gather_info, param_decl_context);
                entry->do_not_print = 1;
            }

            keep_extra_attributes_in_symbol(entry, &param_decl_gather_info);

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
                type_t *orig_array_type = type_info;
                type_info = array_type_get_element_type(type_info);
                type_info = get_pointer_type(type_info);

                // Thi is only for C99
                type_info = get_cv_qualified_type(
                    type_info, array_type_get_cv_qualifier(orig_array_type));
            }

            if (this_is_a_pack)
            {
                // If this parameter declaration explicitly introduces a pack,
                // make sure it has a pack type somewhere
                if (type_does_not_contain_any_template_parameter_pack(
                            type_info,
                            ast_get_locus(parameter_declaration)))
                {
                    set_is_inside_pack_expansion(keep_is_inside_pack_expansion);
                    *function_type = get_error_type();
                    return;
                }

                type_info = get_pack_type(type_info);
                original_type = get_pack_type(original_type);

                entry->type_information = original_type;
            }

            if (entry != NULL)
            {
                // A parameter is always a variable entity
                entry->kind = SK_VARIABLE;
                // it was a parameter pack
                if (this_is_a_pack)
                {
                    entry->kind = SK_VARIABLE_PACK;
                }

                // Update the type info but try to to preserve the original if
                // possible
                if (!equivalent_types(type_info, original_type))
                    entry->type_information = type_info;

                entry->defined = 1;
            }

            set_is_inside_pack_expansion(keep_is_inside_pack_expansion);

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

            DELETE(param_decl_gather_info.vla_dimension_symbols);

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
        const decl_context_t* decl_context,
        const decl_context_t* *p_prototype_context,
        const decl_context_t* *out_prototype_context,
        nodecl_t* nodecl_output)
{
    const decl_context_t* prototype_context;

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
        const decl_context_t* decl_context,
        const decl_context_t* *lambda_block_context,
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
static void gather_extra_attributes_in_declarator(AST a, gather_decl_spec_t* gather_info, const decl_context_t* declarator_context)
{
    // FIXME - This function is a no-op currently

    if (a == NULL)
        return;

    switch(ASTKind(a))
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
                internal_error("Unknown node '%s'\n", ast_print_node_type(ASTKind(a)));
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
    type_t **declarator_type,
    gather_decl_spec_t *gather_info,
    // This one contains the context of the occurring declarator
    // e.g void A::f(T) will contain the context of 'void A::f(T)'
    const decl_context_t *declarator_context,
    // This context contains the real context for the entity named
    // e.g void A::f(T) will contain the context of 'A::f'
    const decl_context_t *entity_context,
    // This one is used to sign in parameters, this is a block context
    // in function definitions and a prototype context for function
    // declarations or functional types
    const decl_context_t **prototype_context,
    // States if this is a top level declarator
    char is_top_level_declarator,
    nodecl_t *nodecl_output)
{
    if (a == NULL)
        return;

    switch(ASTKind(a))
    {
        case AST_DECLARATOR :
        case AST_PARENTHESIZED_DECLARATOR :
            {
                AST attributes = ASTSon1(a);
                apply_attributes_to_type(declarator_type, attributes, declarator_context);

                build_scope_declarator_rec(ASTSon0(a),
                                           declarator_type,
                                           gather_info,
                                           declarator_context,
                                           entity_context,
                                           prototype_context,
                                           is_top_level_declarator,
                                           nodecl_output);
                break;
            }
        case AST_POINTER_DECLARATOR :
            {
                if (gather_info->is_decltype_auto)
                {
                    *declarator_type = get_error_type();
                    error_printf_at(ast_get_locus(a), "invalid %s declarator for 'decltype(auto)'\n",
                            ASTKind(ASTSon0(a)) == AST_POINTER_SPEC ? "pointer" : "reference");
                    return;
                }
                AST attributes = ASTSon2(a);
                apply_attributes_to_type(declarator_type, attributes, declarator_context);

                set_pointer_type(declarator_type, ASTSon0(a), declarator_context);
                if (is_error_type(*declarator_type))
                {
                    return;
                }
                build_scope_declarator_rec(ASTSon1(a),
                                           declarator_type,
                                           gather_info,
                                           declarator_context,
                                           entity_context,
                                           prototype_context,
                                           /* is_top_level_declarator */ 0,
                                           nodecl_output);
                break;
            }
        case AST_DECLARATOR_ARRAY :
            {
                if (gather_info->is_decltype_auto)
                {
                    *declarator_type = get_error_type();
                    error_printf_at(ast_get_locus(a), "invalid array declarator for 'decltype(auto)'\n");
                    return;
                }
                set_array_type(declarator_type,
                               /* expr */ ASTSon1(a),
                               /* (C99)static_qualif */ ASTSon3(a),
                               /* (C99)cv_qualifier_seq */ ASTSon2(a),
                               gather_info,
                               entity_context,
                               ast_get_locus(a),
                               is_top_level_declarator);
                if (is_error_type(*declarator_type))
                {
                    return;
                }
                build_scope_declarator_rec(ASTSon0(a),
                                           declarator_type,
                                           gather_info,
                                           declarator_context,
                                           entity_context,
                                           prototype_context,
                                           /* is_top_level_declarator */ 0,
                                           nodecl_output);
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

                build_scope_declarator_rec(ASTSon0(a),
                                           declarator_type,
                                           gather_info,
                                           declarator_context,
                                           entity_context,
                                           prototype_context,
                                           /* is_top_level_declarator */ 0,
                                           nodecl_output);
                break;
            }
        case AST_DECLARATOR_FUNC_TRAIL:
            {
                if (gather_info->is_decltype_auto)
                {
                    *declarator_type = get_error_type();
                    error_printf_at(ast_get_locus(a), "invalid function declarator for 'decltype(auto)'\n");
                    return;
                }
                CXX03_LANGUAGE()
                {
                    // Try to be helpful
                    if (gather_info->is_auto_storage)
                    {
                        error_printf_at(ast_get_locus(a), "a trailing return using an 'auto' type-specifier is only valid in C++11\n");
                    }
                    else
                    {
                        error_printf_at(ast_get_locus(a), "a trailing return is only valid in C++11\n");
                    }

                    *declarator_type = get_error_type();
                }
                CXX11_LANGUAGE()
                {
                    if (!gather_info->is_auto_type
                            || gather_info->is_decltype_auto)
                    {
                        error_printf_at(ast_get_locus(a), "a trailing return requires an 'auto' type-specifier\n");
                    }
                }

                // This auto has already been used
                gather_info->is_auto_type = 0;

                AST declarator = ASTSon0(a);
                AST parameters_and_qualifiers = ASTSon1(a);
                AST trailing_return = ASTSon2(a);

                const decl_context_t* out_prototype_context;
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
                build_scope_declarator_rec(declarator,
                                           declarator_type,
                                           gather_info,
                                           declarator_context,
                                           entity_context,
                                           prototype_context,
                                           /* is_top_level_declarator */ 0,
                                           nodecl_output);
                break;
            }
        case AST_DECLARATOR_ID_EXPR :
            {
                // Do nothing
                break;
            }
        case AST_DECLARATOR_ID_PACK :
            {
                if (!gather_info->parameter_declaration)
                {
                    error_printf_at(ast_get_locus(a), "invalid template-pack in non parameter declaration\n");
                    *declarator_type = get_error_type();
                }
                break;
            }
        case AST_AMBIGUITY :
            {
                solve_ambiguous_declarator(a, declarator_context);
                // Restart function
                build_scope_declarator_rec(a,
                                           declarator_type,
                                           gather_info,
                                           declarator_context,
                                           entity_context,
                                           prototype_context,
                                           /* is_top_level_declarator */ 0,
                                           nodecl_output);
                break;
            }
        default:
            {
                internal_error("Unknown node '%s'\n", ast_print_node_type(ASTKind(a)));
            }
    }
}

#if 0
static char is_constructor_declarator_rec(AST a, char seen_decl_func)
{
    ERROR_CONDITION((a == NULL), "This function does not admit NULL trees", 0);

    switch(ASTKind(a))
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
                    switch (ASTKind(ASTSon0(a)))
                    {
                        case AST_SYMBOL :
                        case AST_TEMPLATE_ID :
                            return 1;
                        case AST_QUALIFIED_ID :
                            {
                                AST qualif = ASTSon0(a);
                                AST unqualif = ASTSon2(qualif);

                                return ASTKind(unqualif) == AST_TEMPLATE_ID
                                    || ASTKind(unqualif) == AST_SYMBOL;
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
                internal_error("Unknown node '%s'\n", ast_print_node_type(ASTKind(a)));
            }
    }
}

static char is_constructor_declarator(AST a)
{
    return is_constructor_declarator_rec(a, 0);
}
#endif

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
        if (symbol_entity_specs_get_num_parameters(function_symbol) == 0)
        {
            symbol_entity_specs_reserve_default_argument_info(function_symbol, gather_info->num_arguments_info);
        }

        ERROR_CONDITION(gather_info->num_arguments_info != symbol_entity_specs_get_num_parameters(function_symbol),
                "These two should be the same and they are %d != %d", 
                gather_info->num_arguments_info, 
                symbol_entity_specs_get_num_parameters(function_symbol));

        int i;
        for (i = 0; i < gather_info->num_arguments_info; i++)
        {
            default_argument_info_t* default_arg = symbol_entity_specs_get_default_argument_info_num(function_symbol, i);
            if (default_arg == NULL
                    && !nodecl_is_null(gather_info->arguments_info[i].argument))
            {
                default_arg = NEW0(default_argument_info_t);
                default_arg->argument = gather_info->arguments_info[i].argument;
                default_arg->context = gather_info->arguments_info[i].context;

                symbol_entity_specs_set_default_argument_info_num(function_symbol, i, default_arg); 
            }
        }
    }
}

static char is_gnu_inline_attr_name(const char* str)
{
    return (strcmp(str, "gnu_inline") == 0)
        || (strcmp(str, "__gnu_inline__") == 0);

}

static void update_function_specifiers(scope_entry_t* entry,
        gather_decl_spec_t* gather_info,
        type_t* declarator_type,
        const locus_t* locus)
{
    ERROR_CONDITION(entry->kind != SK_FUNCTION, "Invalid symbol", 0);
    symbol_entity_specs_set_is_user_declared(entry, 1);

    // Previous declaration
    char is_gnu_inline_previous = 0;
    int i, num_gcc_attributes = symbol_entity_specs_get_num_gcc_attributes(entry);
    for (i = 0; i < num_gcc_attributes && !is_gnu_inline_previous; i++)
    {
        gcc_attribute_t gcc_attr = symbol_entity_specs_get_gcc_attributes_num(entry, i);
        is_gnu_inline_previous = is_gnu_inline_attr_name(gcc_attr.attribute_name);
    }

    // Current declaration
    char is_gnu_inline_current = 0;
    num_gcc_attributes = gather_info->num_gcc_attributes;
    for (i = 0; i < num_gcc_attributes && !is_gnu_inline_current; i++)
    {
        gcc_attribute_t gcc_attr = gather_info->gcc_attributes[i];
        is_gnu_inline_current = is_gnu_inline_attr_name(gcc_attr.attribute_name);
    }

    char is_gnu_inline = is_gnu_inline_current || is_gnu_inline_previous;

    C_LANGUAGE()
    {
        if (entry->decl_context->current_scope
                == entry->decl_context->global_scope)
        {
            // If this function is global, and previously declared not static,
            // extern or inline and now is going to be inline, make it extern
            // otherwise the function will not be emitted in C99
            //
            // So, the input source (case A)
            //
            //   void f();
            //   inline void f() { }
            //
            // must be emitted as
            //
            //   extern inline void f() { }
            //
            // Note that, the input source
            //
            //   inline void f();
            //   inline void f() { }
            //
            // must NOT add extern: a definition of 'f' does not have to be emitted
            // in this case (the use may provide it elsewhere by using extern, or
            // not using inline)
            //
            // The dual case (case B)
            //
            //   inline void f();
            //   void f() { }
            //
            // must be emitted also as
            //
            //   extern inline void f()
            //
            // Note that in general we do not force extern to functions, this is a
            // special case required by the subtle C99 semantics regarding inline

            if (!is_gnu_inline
                    && !symbol_entity_specs_get_is_extern(entry)
                    && !symbol_entity_specs_get_is_static(entry)
                    // This covers cases A and B shown above
                    && (symbol_entity_specs_get_is_inline(entry)
                        != gather_info->is_inline))
            {
                symbol_entity_specs_set_is_extern(entry, 1);
            }
        }
    }

    symbol_entity_specs_set_is_constexpr(entry,
            symbol_entity_specs_get_is_constexpr(entry)
            || gather_info->is_constexpr);

    // Merge inline attribute
    symbol_entity_specs_set_is_inline(entry,
            symbol_entity_specs_get_is_inline(entry)
            || gather_info->is_inline
            || gather_info->is_constexpr);

    // Merge extern attribute
    if (!is_gnu_inline)
    {
        symbol_entity_specs_set_is_extern(entry,
                (symbol_entity_specs_get_is_extern(entry)
                 || gather_info->is_extern)
                && !symbol_entity_specs_get_is_static(entry));
    }
    else
    {
        // extern void bar();
        // __attribute__((gnu_inline)) void bar();
        if ((symbol_entity_specs_get_is_extern(entry)
                    && is_gnu_inline_current
                    && !gather_info->is_extern)
                // __attribute__((gnu_inline)) void bar();
                // extern void bar();
                || (is_gnu_inline_previous
                    && !is_gnu_inline_current
                    && gather_info->is_extern))
        {
            // Remove the extern, otherwise an extra extern will be emitted
            // along with __attribute__((gnu_inline)) which causes a behaviour
            // like that of the bare "inline" in C99
            symbol_entity_specs_set_is_extern(entry, 0);
        }
        // extern __attribute__((gnu_inline)) void bar();
        else if (gather_info->is_extern
                && is_gnu_inline_current)
        {
            // Usual case as above
            symbol_entity_specs_set_is_extern(entry,
                    (symbol_entity_specs_get_is_extern(entry)
                     || gather_info->is_extern)
                    && !symbol_entity_specs_get_is_static(entry));
        }
    }

    // Remove the friend-declared attribute if we find the function but
    // this is not a friend declaration
    if (!gather_info->is_friend)
    {
        symbol_entity_specs_set_is_friend_declared(entry, 0);
        if (is_template_specialized_type(entry->type_information))
        {
            // Propagate it to the template name as well if this is the primary
            if (named_type_get_symbol(
                        template_type_get_primary_type(
                            template_specialized_type_get_related_template_type(entry->type_information))) == entry)
            {
                symbol_entity_specs_set_is_friend_declared(
                        template_type_get_related_symbol(
                            template_specialized_type_get_related_template_type(entry->type_information)),
                        0);
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


static void copy_related_symbols(scope_entry_t* dest, scope_entry_t* orig)
{
    symbol_entity_specs_copy_related_symbols_from(dest, orig);
}


static scope_entry_t* build_scope_user_defined_literal_declarator(
        AST declarator_id,
        type_t* declarator_type,
        gather_decl_spec_t* gather_info,
        const decl_context_t* decl_context)
{
    ERROR_CONDITION(ASTKind(declarator_id) != AST_LITERAL_OPERATOR_ID,
            "Invalid node '%s'\n", ast_print_node_type(ASTKind(declarator_id)));

    AST symbol = ASTSon0(declarator_id);
    const char* ud_suffix = ast_get_text(symbol);

    if (ud_suffix != NULL
            && *ud_suffix != '_')
    {
        // This warning is commented since some standard literal operators do
        // not begin with '_' and we are not able to distinguish them
        //
        // warn_printf_at(ast_get_locus(declarator_id),
        //         "literal operator suffixes must begin with '_'\n");
    }

    ERROR_CONDITION(!is_function_type(declarator_type), "Invalid type", 0);
    int num_parameters = function_type_get_num_parameters(declarator_type);

    struct {
        int num_types;
        type_t* types[2];
    } valid_param_types[] = {
        { 1, { get_pointer_type(get_const_qualified_type(get_char_type())), NULL } },
        { 1, { get_unsigned_long_long_int_type(), NULL } },
        { 1, { get_long_double_type(), NULL } },
        { 1, { get_char_type(),     NULL } },
        { 1, { get_wchar_t_type(),  NULL } },
        { 1, { get_char16_t_type(), NULL } },
        { 1, { get_char32_t_type(), NULL } },
        { 2, { get_pointer_type(get_const_qualified_type(get_char_type())),     get_size_t_type() } },
        { 2, { get_pointer_type(get_const_qualified_type(get_wchar_t_type())),  get_size_t_type() } },
        { 2, { get_pointer_type(get_const_qualified_type(get_char16_t_type())), get_size_t_type() } },
        { 2, { get_pointer_type(get_const_qualified_type(get_char32_t_type())), get_size_t_type() } }
    };

    char ok = false;
    int i, num_valid_param_types = STATIC_ARRAY_LENGTH(valid_param_types);
    for (i = 0; i < num_valid_param_types && !ok; ++i)
    {
        if (valid_param_types[i].num_types != num_parameters)
            continue;

        char valid_candidate = true;
        int j;
        for (j = 0; j < num_parameters && valid_candidate; ++j) {
             valid_candidate = equivalent_types(
                     valid_param_types[i].types[j],
                     function_type_get_parameter_type_num(declarator_type, j));
        }

        ok = valid_candidate;
    }

    if (!ok)
    {
        error_printf_at(ast_get_locus(declarator_id),
                "'%s' is not a valid literal operator\n",
                print_type_str(declarator_type, decl_context));
    }

    const char* literal_operator_name =
        get_literal_operator_name(ud_suffix);

    AST literal_operator_id = ASTLeaf(AST_SYMBOL,
            ast_get_locus(declarator_id),
            literal_operator_name);

    // Keep the parent of the original declarator
    ast_set_parent(literal_operator_id, ast_get_parent(declarator_id));

    return register_new_var_or_fun_name(literal_operator_id, declarator_type, gather_info, decl_context);
}


/**
 *  In C++11, a constexpr speficier for a non-static member function that is
 *  not a constructor declares that member function as const. This restriction
 *  was removed in C++14
 */
static void adjust_constexpr_function_type_if_needed(
        char is_constexpr,
        char is_static,
        char is_member,
        char is_constructor,
        // Out
        type_t** declarator_type)
{
    ERROR_CONDITION(!is_function_type(*declarator_type),  "Unexpected non-function type", 0);

    if (IS_CXX11_LANGUAGE
            && !IS_CXX14_LANGUAGE
            && is_constexpr
            && !is_constructor
            && is_member
            && !is_static)
    {
        *declarator_type = get_const_qualified_type(*declarator_type);
    }
}

/*
 * This function fills the symbol table with the information of this declarator
 */
static scope_entry_t* build_scope_declarator_name(AST declarator,
        type_t* type_specifier, // used here only for proper diagnostics
        type_t* declarator_type,
        gather_decl_spec_t* gather_info,
        const decl_context_t* decl_context)
{
    AST declarator_id_expr = get_declarator_id_expression(declarator, decl_context);

    if (declarator_id_expr == NULL)
        return NULL;

    ERROR_CONDITION(ASTKind(declarator_id_expr) != AST_DECLARATOR_ID_EXPR,
            "Invalid node '%s'\n", ast_print_node_type(ASTKind(declarator_id_expr)));

    AST declarator_id = ASTSon0(declarator_id_expr);

    switch (ASTKind(declarator_id))
    {
        // Unqualified ones
        case AST_SYMBOL :
            {
                // A simply unqualified symbol "name"

                // We are not declaring a variable but a type
                if (gather_info->is_typedef)
                {
                    if (type_specifier == NULL)
                    {
                        error_printf_at(ast_get_locus(declarator), "typedef declaration lacks a type-specifier\n");
                        declarator_type = get_error_type();
                    }
                    return register_new_typedef_name(declarator_id, declarator_type, gather_info, decl_context);
                }
                else
                {
                    char name_of_a_constructor =
                        is_function_type(declarator_type)
                        && decl_context->current_scope->kind == CLASS_SCOPE
                        && decl_context->current_scope->related_entry != NULL
                        && decl_context->current_scope->related_entry->symbol_name != NULL
                        && strcmp(decl_context->current_scope->related_entry->symbol_name,
                                ASTText(declarator_id)) == 0;

                    if (is_function_type(declarator_type))
                    {
                        adjust_constexpr_function_type_if_needed(
                                /* is_constexpr */ gather_info->is_constexpr,
                                /* is_static */ gather_info->is_static,
                                /* is_member */ decl_context->current_scope->kind == CLASS_SCOPE,
                                /* is_constructor */ name_of_a_constructor,
                                &declarator_type);
                    }

                    if (type_specifier == NULL)
                    {
                        if (name_of_a_constructor)
                        {
                            // this names a constructor
                            decl_context_t* updated_decl_context = decl_context_clone(decl_context);
                            updated_decl_context->decl_flags |= DF_CONSTRUCTOR;

                            decl_context = updated_decl_context;
                        }
                        else
                        {
                            error_printf_at(ast_get_locus(declarator), "declaration lacks a type-specifier\n");
                        }
                    }
                    else
                    {
                        if (name_of_a_constructor)
                        {
                            error_printf_at(ast_get_locus(declarator), "constructor declaration cannot have a type-specifier\n");
                        }
                    }
                    return register_new_var_or_fun_name(declarator_id, declarator_type, gather_info, decl_context);
                }
                break;
            }
        case AST_DESTRUCTOR_TEMPLATE_ID : // This can appear here // FIXME - Template arguments are not checked
        case AST_DESTRUCTOR_ID :
            {
                adjust_constexpr_function_type_if_needed(
                        /* is_constexpr */ gather_info->is_constexpr,
                        /* is_static */ gather_info->is_static,
                        /* is_member */ decl_context->current_scope->kind == CLASS_SCOPE,
                        /* is_constructor */ 0,
                        &declarator_type);

                if (type_specifier != NULL)
                {
                    error_printf_at(ast_get_locus(declarator), "destructor declarator cannot have a type-specifier\n");
                }

                // An unqualified destructor name "~name"
                // 'name' should be a class in this scope
                AST destructor_id = ASTSon0(declarator_id);
                if (ASTKind(declarator_id) == AST_DESTRUCTOR_TEMPLATE_ID)
                {
                    destructor_id = declarator_id;
                }
                // Adjust to 'function () returning void'
                declarator_type = get_const_qualified_type(get_new_function_type(get_void_type(), NULL, 0, REF_QUALIFIER_NONE));
                return register_new_var_or_fun_name(destructor_id, declarator_type, gather_info, decl_context);
                break;
            }
        case AST_TEMPLATE_ID :
            {
                if (!is_function_type(declarator_type))
                {
                    if (type_specifier == NULL)
                    {
                        error_printf_at(ast_get_locus(declarator), "declaration lacks a type-specifier\n");
                    }

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
                    // This is for the weird cases like this
                    //
                    // template <typename T>
                    // struct C { };
                    //
                    // template <>
                    // struct C<int> { C<int>() { } };

                    AST template_name = ASTSon0(declarator_id);
                    char name_of_a_constructor =
                        (ASTKind(template_name) == AST_SYMBOL)
                        && is_function_type(declarator_type)
                        && decl_context->current_scope->kind == CLASS_SCOPE
                        && decl_context->current_scope->related_entry != NULL
                        && decl_context->current_scope->related_entry->symbol_name != NULL
                        && strcmp(decl_context->current_scope->related_entry->symbol_name,
                                ASTText(template_name)) == 0;

                    if (is_function_type(declarator_type))
                    {
                        adjust_constexpr_function_type_if_needed(
                                /* is_constexpr */ gather_info->is_constexpr,
                                /* is_static */ gather_info->is_static,
                                /* is_member */ decl_context->current_scope->kind == CLASS_SCOPE,
                                /* is_constructor */ name_of_a_constructor,
                                &declarator_type);
                    }

                    if (type_specifier == NULL)
                    {
                        if (name_of_a_constructor)
                        {
                            // Check the specialization actually mentions the current class
                            scope_entry_list_t* entry_list = query_nested_name(decl_context,
                                    NULL, NULL,
                                    declarator_id,
                                    NULL);

                            if (entry_list == NULL
                                    || entry_list_head(entry_list)->kind != SK_CLASS
                                    || (class_symbol_get_canonical_symbol(entry_list_head(entry_list))
                                        != decl_context->current_scope->related_entry))
                            {
                                error_printf_at(ast_get_locus(declarator), "invalid constructor declaration '%s'\n",
                                        prettyprint_in_buffer(declarator));
                                return NULL;
                            }

                            // this names a constructor
                            decl_context_t* updated_decl_context = decl_context_clone(decl_context);
                            updated_decl_context->decl_flags |= DF_CONSTRUCTOR;

                            decl_context = updated_decl_context;

                            // now drop the template-id and work only with the name
                            declarator_id = template_name;
                        }
                        else
                        {
                            error_printf_at(ast_get_locus(declarator), "declaration lacks a type-specifier\n");
                        }
                    }
                    else
                    {
                        if (name_of_a_constructor)
                        {
                            error_printf_at(ast_get_locus(declarator), "constructor declaration cannot have a type-specifier\n");
                        }
                    }

                    return register_new_var_or_fun_name(declarator_id, declarator_type, gather_info, decl_context);
                }

                break;
            }
        case AST_OPERATOR_FUNCTION_ID:
        case AST_OPERATOR_FUNCTION_ID_TEMPLATE:
            {
                adjust_constexpr_function_type_if_needed(
                        /* is_constexpr */ gather_info->is_constexpr,
                        /* is_static */ gather_info->is_static,
                        /* is_member */ decl_context->current_scope->kind == CLASS_SCOPE,
                        /* is_constructor */ 0,
                        &declarator_type);

                if (type_specifier == NULL)
                {
                    error_printf_at(ast_get_locus(declarator), "declaration lacks a type-specifier\n");
                }

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

                if (ASTKind(declarator_id) == AST_OPERATOR_FUNCTION_ID)
                {
                    return register_new_var_or_fun_name(operator_id, declarator_type, gather_info, decl_context);
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
        case AST_LITERAL_OPERATOR_ID:
            {
                if (!IS_CXX11_LANGUAGE)
                {
                    warn_printf_at(ast_get_locus(declarator),
                            "literal operators are only valid in C++11\n");
                }

                adjust_constexpr_function_type_if_needed(
                        /* is_constexpr */ gather_info->is_constexpr,
                        /* is_static */ gather_info->is_static,
                        /* is_member */ decl_context->current_scope->kind == CLASS_SCOPE,
                        /* is_constructor */ 0,
                        &declarator_type);

                if (type_specifier == NULL)
                {
                    error_printf_at(ast_get_locus(declarator),
                            "literal operator lacks a type-specifier\n");
                }

                return build_scope_user_defined_literal_declarator(declarator_id, declarator_type, gather_info, decl_context);

                break;
            };
        case AST_CONVERSION_FUNCTION_ID :
            {
                adjust_constexpr_function_type_if_needed(
                        /* is_constexpr */ gather_info->is_constexpr,
                        /* is_static */ gather_info->is_static,
                        /* is_member */ decl_context->current_scope->kind == CLASS_SCOPE,
                        /* is_constructor */ 0,
                        &declarator_type);

                if (type_specifier != NULL)
                {
                    error_printf_at(ast_get_locus(declarator), "conversion function declaration cannot have any type-specifier\n");
                }

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
                return register_new_var_or_fun_name(conversion_id, declarator_type, gather_info, decl_context);
                break;
            }
            // Qualified ones
        case AST_QUALIFIED_ID :
            {
                // A qualified id "a::b::c"
                if (!is_function_type(declarator_type))
                {
                    if (type_specifier == NULL)
                    {
                        error_printf_at(ast_get_locus(declarator), "declaration lacks a type-specifier\n");
                    }

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
                    char is_static = gather_info->is_static;
                    char is_member = decl_context->current_scope->kind == CLASS_SCOPE;
                    char name_of_a_constructor = 0;

                    scope_entry_t *entry = NULL;

                    AST unqualified_part = ASTSon2(declarator_id);
                    switch (ASTKind(unqualified_part))
                    {
                        case AST_TEMPLATE_ID:
                        case AST_SYMBOL:
                            {
                                // A::B::X, check that A::B designates a
                                // class, if X == B then this designates a
                                // constructor

                                AST lookup_id = declarator_id;

                                if (ASTKind(unqualified_part) == AST_TEMPLATE_ID
                                        && ASTKind(ASTSon0(unqualified_part)) == AST_SYMBOL)
                                {
                                    lookup_id = ast_copy(declarator_id);
                                    // Transform A::B::B<int> into A::B::B
                                    // so we can make a lookup that finds
                                    // the injected class-name
                                    ast_set_child(
                                            lookup_id, 2,
                                            ASTSon0(ASTSon2(lookup_id)));
                                }

                                scope_entry_list_t * entry_list = query_id_expression(
                                        decl_context,
                                        lookup_id,
                                        /* field_path */ NULL);

                                if (ASTKind(unqualified_part) == AST_TEMPLATE_ID)
                                {
                                    ast_free(lookup_id);
                                }

                                if (entry_list != NULL)
                                {
                                    entry = entry_list_head(entry_list);
                                    is_member = symbol_entity_specs_get_is_member(entry);

                                    if (entry->kind == SK_CLASS
                                            // this only happens when A::B::B under normal lookups
                                            && symbol_entity_specs_get_is_injected_class_name(entry)
                                            /*
                                               && tail->symbol_name != NULL
                                               && is_function_type(declarator_type)
                                               && strcmp(ASTText(unqualified_part), tail->symbol_name) == 0
                                            */
                                            )
                                    {
                                        name_of_a_constructor = 1;
                                    }
                                }

                                entry_list_free(entry_list);


                                if (type_specifier == NULL)
                                {
                                    if (name_of_a_constructor)
                                    {
                                        // this names a constructor
                                        decl_context_t* updated_decl_context = decl_context_clone(decl_context);
                                        updated_decl_context->decl_flags |= DF_CONSTRUCTOR;

                                        decl_context = updated_decl_context;
                                    }
                                    else
                                    {
                                        error_printf_at(ast_get_locus(declarator), "declaration lacks a type-specifier\n");
                                    }
                                }
                                else
                                {
                                    if (name_of_a_constructor)
                                    {
                                        error_printf_at(ast_get_locus(declarator), "constructor declaration cannot have a type-specifier\n");
                                    }
                                }

                                break;
                            }
                        case AST_CONVERSION_FUNCTION_ID:
                            {
                                if (type_specifier != NULL)
                                {
                                    error_printf_at(ast_get_locus(declarator), "conversion function declaration cannot have a type-specifier\n");
                                }
                                break;
                            }
                        case AST_DESTRUCTOR_ID:
                        case AST_DESTRUCTOR_TEMPLATE_ID:
                            {
                                if (type_specifier != NULL)
                                {
                                    error_printf_at(ast_get_locus(declarator), "destructor declarator cannot have any type-specifier\n");
                                }
                                // Adjust the type to 'const function () returning void'
                                declarator_type = get_const_qualified_type(
                                        get_new_function_type(get_void_type(), NULL, 0, REF_QUALIFIER_NONE));
                                break;
                            }
                        case AST_OPERATOR_FUNCTION_ID:
                        case AST_OPERATOR_FUNCTION_ID_TEMPLATE:
                            {
                                if (type_specifier == NULL)
                                {
                                    error_printf_at(ast_get_locus(declarator), "declaration lacks a type-specifier\n");
                                }
                                break;
                            }
                        default: { }
                    }

                    adjust_constexpr_function_type_if_needed(
                            /* is_constexpr */ gather_info->is_constexpr,
                            is_static,
                            is_member,
                            name_of_a_constructor,
                            &declarator_type);

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
                internal_error("Unknown node '%s'\n", ast_print_node_type(ASTKind(declarator_id)));
                break;
            }
    }

    return NULL;
}



static char dependent_typename_entry_aliases_member(type_t* dependent_typename, scope_entry_t* member)
{
    ERROR_CONDITION(!is_dependent_typename_type(dependent_typename), "Invalid type", 0);
    ERROR_CONDITION(!symbol_entity_specs_get_is_member(member), "Invalid symbol", 0);

    scope_entry_t* current_class = named_type_get_symbol(
            symbol_entity_specs_get_class_type(member)
            );

    scope_entry_t* dependent_entry = NULL;
    nodecl_t nodecl_dependent_parts = nodecl_null();

    dependent_typename_get_components(dependent_typename, &dependent_entry, &nodecl_dependent_parts);
    if ((current_class == dependent_entry)
            && nodecl_get_kind(nodecl_dependent_parts) == NODECL_CXX_DEP_NAME_NESTED)
    {
        nodecl_t list = nodecl_get_child(nodecl_dependent_parts, 0);
        if (nodecl_list_length(list) == 1)
        {
            nodecl_t nodecl_name = nodecl_list_head(list);
            if (nodecl_get_kind(nodecl_name) == NODECL_CXX_DEP_NAME_SIMPLE
                    && strcmp(nodecl_get_text(nodecl_name), member->symbol_name) == 0)
            {
                return 1;
            }
        }
    }

    return 0;
}

/*
 * This function registers a new typedef name.
 */
static scope_entry_t* register_new_typedef_name(AST declarator_id, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, const decl_context_t* decl_context)
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

            // Using symbols have to be advanced
            if (entry->kind == SK_USING)
                entry = entry_advance_aliases(entry);

            if (entry->kind != SK_ENUM
                    && entry->kind != SK_CLASS
                    && entry->kind != SK_TYPEDEF)
            {
                error_printf_at(ast_get_locus(declarator_id), "symbol '%s' has been redeclared as a different symbol kind\n", 
                        prettyprint_in_buffer(declarator_id));
                info_printf_at(entry->locus, "previous declaration of '%s'\n",
                        entry->symbol_name);
                return NULL;
            }
        }
        entry_list_iterator_free(it);

        scope_entry_t* entry = entry_list_head(list);

        // Using symbols have to be advanced
        if (entry->kind == SK_USING)
            entry = entry_advance_aliases(entry);

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
        if ((is_named_type(declarator_type)
                && named_type_get_symbol(declarator_type) == entry)
                || (symbol_entity_specs_get_is_member(entry)
                    && is_dependent_typename_type(declarator_type)
                    && dependent_typename_entry_aliases_member(declarator_type, entry)))
        {
            // In this special case, "A" will not be redefined, lets undefine
            // here and let it be redefined again later
            entry->defined = 0;
        }
        else
        {
            if (!equivalent_types(entry->type_information, declarator_type))
            {
                error_printf_at(ast_get_locus(declarator_id), "symbol '%s' has been redeclared as a different symbol kind\n", 
                        prettyprint_in_buffer(declarator_id));
                info_printf_at(ast_get_locus(declarator_id), "current declaration of '%s' (with type '%s')\n", 
                        prettyprint_in_buffer(declarator_id),
                        print_type_str(declarator_type, decl_context));
                info_printf_at(entry->locus, "previous declaration of '%s' (with type '%s')\n",
                        entry->symbol_name,
                        print_type_str(
                            entry->kind == SK_TYPEDEF
                                ? entry->type_information
                                : get_user_defined_type(entry),
                            entry->decl_context)
                        );
                return NULL;
            }

            if (is_function_type(declarator_type)
                    && !is_named_type(declarator_type))
            {
                // If the declarator is a functional one, we have to mix the arguments here
                int i;
                for (i = 0; i < gather_info->num_arguments_info; i++)
                {
                    default_argument_info_t* default_arg = symbol_entity_specs_get_default_argument_info_num(entry, i);
                    if (default_arg == NULL
                            && !nodecl_is_null(gather_info->arguments_info[i].argument))
                    {
                        default_arg = NEW0(default_argument_info_t);

                        default_arg->argument = gather_info->arguments_info[i].argument;
                        default_arg->context = gather_info->arguments_info[i].context;

                        symbol_entity_specs_set_default_argument_info_num(entry, i, default_arg);
                    }
                }
            }
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
                && symbol_entity_specs_get_is_unnamed(named_type_get_symbol(declarator_type)))
        {
            scope_entry_t* unnamed_symbol = named_type_get_symbol(declarator_type);

            unnamed_symbol->symbol_name = ASTText(declarator_id);
            symbol_entity_specs_set_is_unnamed(unnamed_symbol, 0);

            insert_entry(unnamed_symbol->decl_context->current_scope, unnamed_symbol);

            return unnamed_symbol;
        }
    }

    scope_entry_t* entry = new_symbol(decl_context, decl_context->current_scope, ASTText(declarator_id));

    DEBUG_CODE()
    {
        fprintf(stderr, "BUILDSCOPE: Registering typedef '%s'\n", ASTText(declarator_id));
    }

    entry->locus = ast_get_locus(declarator_id);
    symbol_entity_specs_set_is_user_declared(entry, 1);

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

        symbol_entity_specs_reserve_default_argument_info(entry, gather_info->num_arguments_info);

        int i;
        for (i = 0; i < gather_info->num_arguments_info; i++)
        {
            if (!nodecl_is_null(gather_info->arguments_info[i].argument))
            {
                default_argument_info_t* default_arg = NEW0(default_argument_info_t);

                default_arg->argument = gather_info->arguments_info[i].argument;
                default_arg->context = gather_info->arguments_info[i].context;

                symbol_entity_specs_set_default_argument_info_num(entry, i, default_arg);
            }
        }

        // Copy exception info as well
        symbol_entity_specs_set_any_exception(entry, gather_info->any_exception);
        for (i = 0; i < gather_info->num_exceptions; i++)
        {
            symbol_entity_specs_add_exceptions(entry, gather_info->exceptions[i]);
        }
        symbol_entity_specs_set_noexception(entry, gather_info->noexception);

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
                fprintf(stderr, "BUILDSCOPE: Number of parameters %d\n", symbol_entity_specs_get_num_parameters(named_type));
            }

            // Case 1 above will have copied such information in the symbol
            symbol_entity_specs_copy_default_argument_info_from(entry, named_type);

            // Copy exception info as well
            symbol_entity_specs_set_any_exception(entry, gather_info->any_exception);
            int i;
            for (i = 0; i < gather_info->num_exceptions; i++)
            {
                symbol_entity_specs_add_exceptions(entry, gather_info->exceptions[i]);
            }
            symbol_entity_specs_set_noexception(entry, symbol_entity_specs_get_noexception(named_type));

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
static scope_entry_t* register_new_var_or_fun_name(AST declarator_id, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, const decl_context_t* decl_context)
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
            if (symbol_entity_specs_get_is_extern(entry))
            {
                symbol_entity_specs_set_is_extern(entry, gather_info->is_extern);
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
            error_printf_at(ast_get_locus(declarator_id), "incompatible redeclaration of '%s' (look at '%s')\n",
                    prettyprint_in_buffer(declarator_id),
                    locus_to_str(entry->locus));
            return NULL;
        }

        entry_list_free(check_list);

        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: Registering variable '%s' in %p\n", ASTText(declarator_id), decl_context->current_scope);
        }

        scope_entry_t* entry = NULL;
        entry = new_symbol(decl_context, decl_context->current_scope, ASTText(declarator_id));

        entry->locus = ast_get_locus(declarator_id);
        entry->kind = SK_VARIABLE;
        entry->type_information = declarator_type;

        symbol_entity_specs_set_is_user_declared(entry, 1);
        symbol_entity_specs_set_is_static(entry, gather_info->is_static);
        symbol_entity_specs_set_is_mutable(entry, gather_info->is_mutable);
        symbol_entity_specs_set_is_extern(entry, gather_info->is_extern);
        symbol_entity_specs_set_is_register(entry, gather_info->is_register);
        symbol_entity_specs_set_is_thread(entry, gather_info->is_thread);
        symbol_entity_specs_set_is_thread_local(entry, gather_info->is_thread_local);
        symbol_entity_specs_set_is_constexpr(entry, gather_info->is_constexpr);

        if (symbol_entity_specs_get_is_constexpr(entry)
                && !is_any_reference_type(entry->type_information))
        {
            entry->type_information = get_const_qualified_type(entry->type_information);
        }

        symbol_entity_specs_set_linkage_spec(entry, linkage_current_get_name());

        return entry;
    }
    else
    {
        return register_function(declarator_id, declarator_type, gather_info, decl_context);
    }
}

static scope_entry_t* register_function(AST declarator_id, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, const decl_context_t* decl_context)
{
    scope_entry_t* entry = NULL;
    char ok = find_function_declaration(declarator_id, declarator_type, gather_info, decl_context, &entry);

    if (!ok)
        return NULL;

    if (entry == NULL)
    {
        // No existing function was found
        const char* function_name = ASTText(declarator_id);

        char declares_a_constructor = BITMAP_TEST(decl_context->decl_flags, DF_CONSTRUCTOR);

        decl_context_t* updated_decl_context = decl_context_clone(decl_context);
        updated_decl_context->decl_flags &= ~DF_CONSTRUCTOR;
        decl_context = updated_decl_context;

        if (declares_a_constructor)
        {
            function_name = strprepend(function_name, "constructor ");
        }

        scope_entry_t* new_entry = NULL;

        if (!gather_info->is_template)
        {
            // Create the symbol as a normal function type

            if (!gather_info->is_friend)
            {
                new_entry = new_symbol(decl_context, decl_context->current_scope, function_name);
            }
            // If it is friend it must be signed in in the enclosing namespace
            // and the current_scope must be the namespace_scope
            else
            {
                decl_context_t* updated_decl_context_2 = decl_context_clone(decl_context);
                updated_decl_context_2->current_scope = decl_context->namespace_scope;

                new_entry = new_symbol(updated_decl_context_2, updated_decl_context_2->namespace_scope, function_name);
            }

            new_entry->type_information = declarator_type;

            new_entry->kind = SK_FUNCTION;
            new_entry->locus = ast_get_locus(declarator_id);

            symbol_entity_specs_set_linkage_spec(new_entry, linkage_current_get_name());
            symbol_entity_specs_set_is_explicit(new_entry, gather_info->is_explicit);
            symbol_entity_specs_set_is_friend_declared(new_entry, gather_info->is_friend);

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
            if (decl_context->template_parameters == NULL)
            {
                error_printf_at(ast_get_locus(declarator_id), "explicit specialization '%s' does not match any template of '%s'\n",
                        print_decl_type_str(declarator_type, decl_context, function_name),
                        function_name);
                return NULL;
            }

            ERROR_CONDITION(decl_context->template_parameters == NULL,
                    "Error, there must be template parameters", 0);

            const decl_context_t* template_context = decl_context;
            if (!gather_info->is_friend)
            {
                new_entry = new_symbol(decl_context, decl_context->current_scope, function_name);
            }
            // If it is friend it must be signed in in the enclosing namespace
            // and the current_scope must be the namespace_scope
            else
            {
                decl_context_t* updated_decl_context_2 = decl_context_clone(decl_context);
                updated_decl_context_2->current_scope = decl_context->namespace_scope;
                new_entry = new_symbol(updated_decl_context_2, updated_decl_context_2->namespace_scope, function_name);

                decl_context_t* updated_decl_context_3 = decl_context_clone(template_context);
                updated_decl_context_3->current_scope = decl_context->namespace_scope;
                template_context = updated_decl_context_3;
            }

            // If this function is template we have to create a template type
            // in the right context (It may be friend declared)
            type_t* template_type = get_new_template_type(decl_context->template_parameters,
                    declarator_type,
                    function_name,
                    template_context,
                    ast_get_locus(declarator_id));

            new_entry->type_information = template_type;

            // This is a template, not a plain function
            new_entry->kind = SK_TEMPLATE;
            new_entry->locus = ast_get_locus(declarator_id);

            symbol_entity_specs_set_is_friend_declared(new_entry, 0);

            if (decl_context->current_scope->kind == CLASS_SCOPE
                    && !symbol_entity_specs_get_is_friend_declared(new_entry))
            {
                symbol_entity_specs_set_is_member(new_entry, 1);

                symbol_entity_specs_set_class_type(new_entry,
                    get_user_defined_type(decl_context->current_scope->related_entry));
            }

            template_type_set_related_symbol(template_type, new_entry);

            // Now update the symbol, we are not working anymore on the
            // template type itself but on its main specialization (primary
            // template type)
            new_entry = named_type_get_symbol(
                    template_type_get_primary_type(template_type));

            // Update info
            new_entry->locus = ast_get_locus(declarator_id);

            symbol_entity_specs_set_is_explicit(new_entry, gather_info->is_explicit);

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
                    new_entry->decl_context->current_scope,
                    (new_entry->kind == SK_TEMPLATE)
                   );
        }

        if (declares_a_constructor)
        {
            symbol_entity_specs_set_is_constructor(new_entry, 1);
        }

        symbol_entity_specs_set_is_user_declared(new_entry, 1);

        symbol_entity_specs_set_is_static(new_entry, gather_info->is_static);
        symbol_entity_specs_set_is_extern(new_entry, gather_info->is_extern);
        symbol_entity_specs_set_is_constexpr(new_entry, gather_info->is_constexpr);
        symbol_entity_specs_set_is_virtual(new_entry, gather_info->is_virtual);
        symbol_entity_specs_set_is_inline(new_entry, gather_info->is_inline
            || gather_info->is_constexpr);

        if (gather_info->is_static && gather_info->is_extern
                && !gather_info->is_auto_storage)
        {
            error_printf_at(ast_get_locus(declarator_id), "conflicting linkage specifiers extern and static\n");
        }

        if (gather_info->is_virtual && gather_info->is_extern)
        {
            error_printf_at(ast_get_locus(declarator_id), "a virtual function is member, so it cannot have extern linkage\n");
        }

        if (gather_info->is_virtual && gather_info->is_static)
        {
            error_printf_at(ast_get_locus(declarator_id), "a virtual function must be a nonstatic member\n");
        }

        if (gather_info->is_auto_storage)
        {
            if (!gather_info->is_static
                    && !gather_info->is_extern)
            {
                if (decl_context->current_scope->kind != BLOCK_SCOPE)
                {
                    error_printf_at(ast_get_locus(declarator_id), "invalid auto linkage specifier for functions in non block scope\n");
                }
                // This is the gcc way to declare (not define) a nested function
                symbol_entity_specs_set_is_nested_function(new_entry, 1);
            }
            else
            {
                error_printf_at(ast_get_locus(declarator_id), "conflicting linkage specifiers auto %s specified\n",
                        (gather_info->is_static && gather_info->is_extern)
                        ?  ", extern and static"
                        : (gather_info->is_static ? "and static" : "and extern"));
            }
        }

        if (decl_context->current_scope->kind == BLOCK_SCOPE)
        {
            if (gather_info->is_static)
            {
                error_printf_at(ast_get_locus(declarator_id), "invalid static linkage specifier for a function declared in block scope\n");
            }
        }

        if (decl_context->current_scope->kind != BLOCK_SCOPE)
        {
            symbol_entity_specs_set_linkage_spec(new_entry, linkage_current_get_name());
        }

        // "is_pure" of a function is computed in "build_scope_member_simple_declaration"

        symbol_entity_specs_set_any_exception(new_entry, gather_info->any_exception);
        int i;
        for (i = 0; i < gather_info->num_exceptions; i++)
        {
            symbol_entity_specs_add_exceptions(new_entry, gather_info->exceptions[i]);
        }
        symbol_entity_specs_set_noexception(new_entry, gather_info->noexception);

        char do_delay_function = 0;
        if (!nodecl_is_null(symbol_entity_specs_get_noexception(new_entry))
                && nodecl_get_kind(symbol_entity_specs_get_noexception(new_entry)) == NODECL_CXX_PARSE_LATER)
        {
            do_delay_function = 1;
        }

        symbol_entity_specs_reserve_default_argument_info(new_entry, gather_info->num_arguments_info);

        symbol_entity_specs_set_is_friend_declared(new_entry, gather_info->is_friend);

        // If the declaration context is CLASS_SCOPE and the function definition is friend,
        // It is not a member class
        if (decl_context->current_scope->kind == CLASS_SCOPE
            && !symbol_entity_specs_get_is_friend_declared(new_entry))
        {
            symbol_entity_specs_set_is_member(new_entry, 1);
            symbol_entity_specs_set_class_type(new_entry,
                    get_user_defined_type(decl_context->current_scope->related_entry));
        }

        for (i = 0; i < gather_info->num_arguments_info; i++)
        {
            if (!nodecl_is_null(gather_info->arguments_info[i].argument))
            {
                if (gather_info->is_explicit_specialization)
                {
                    error_printf_at(ast_get_locus(declarator_id), "default template arguments in explicit specialization function declaration\n");
                }
                else if (gather_info->is_explicit_instantiation)
                {
                    error_printf_at(ast_get_locus(declarator_id), "default template arguments in explicit instantiation function declaration\n");
                }

                default_argument_info_t* default_argument = NEW0(default_argument_info_t);
                default_argument->argument = 
                    gather_info->arguments_info[i].argument;
                default_argument->context = 
                    gather_info->arguments_info[i].context;

                symbol_entity_specs_set_default_argument_info_num(new_entry, i, default_argument);

                if (nodecl_get_kind(default_argument->argument) == NODECL_CXX_PARSE_LATER)
                {
                    do_delay_function = 1;
                }
            }
        }

        if (do_delay_function)
        {
            ERROR_CONDITION(decl_context->current_scope->kind != CLASS_SCOPE,
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
                fprintf(stderr , "BUILDSCOPE: Num parameters %d\n", symbol_entity_specs_get_num_parameters(named_function_type));
            }

            // Adjust the parameter info
            int j;
            for (j = 0; j < symbol_entity_specs_get_num_parameters(named_function_type); j++)
            {
                symbol_entity_specs_add_default_argument_info(
                        new_entry,
                        symbol_entity_specs_get_default_argument_info_num(named_function_type, j));
            }

            // Copy exception info as well
            symbol_entity_specs_copy_exceptions_from(new_entry, named_function_type);
            symbol_entity_specs_set_noexception(new_entry, symbol_entity_specs_get_noexception(named_function_type));
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
        const decl_context_t* decl_context,
        scope_entry_t** result_entry)
{
    ERROR_CONDITION(!(gather_info->is_friend
                && is_dependent_class_scope(decl_context)),
            "this is not a depedent friend function", 0);

    // We should create a new dependent friend function, but first we need to
    // check some constraints

    char is_template_function = gather_info->is_template;
    char is_qualified = is_qualified_id_expression(declarator_id);

    AST declarator_id_without_template_id = NULL;
    char is_template_id = 0;

    if (ASTKind(declarator_id) == AST_TEMPLATE_ID)
    {
        is_template_id = 1;
        declarator_id_without_template_id = ASTSon0(declarator_id);
    }
    else if (ASTKind(declarator_id) == AST_OPERATOR_FUNCTION_ID_TEMPLATE)
    {
        is_template_id = 1;
        declarator_id_without_template_id = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ast_copy(ASTSon0(declarator_id)),
                ast_get_locus(declarator_id),
                NULL);
    }
    else if (ASTKind(declarator_id) == AST_QUALIFIED_ID
            && ASTKind(ASTSon2(declarator_id)) == AST_TEMPLATE_ID)
    {
        AST unqualified_part = ASTSon2(declarator_id);
        AST name = ASTSon0(unqualified_part);

        is_template_id = 1;
        declarator_id_without_template_id =
            ASTMake3(AST_QUALIFIED_ID,
                    ast_copy(ASTSon0(declarator_id)),
                    ast_copy(ASTSon1(declarator_id)),
                    ast_copy(name),
                    ast_get_locus(declarator_id),
                    NULL);
    }
    else if (ASTKind(declarator_id) == AST_QUALIFIED_ID
            && ASTKind(ASTSon2(declarator_id)) == AST_OPERATOR_FUNCTION_ID_TEMPLATE)
    {
        is_template_id = 1;
        AST unqualified_part = ASTSon2(declarator_id);

        AST new_op = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ast_copy(ASTSon0(unqualified_part)),
                ast_get_locus(unqualified_part),
                NULL);

        declarator_id_without_template_id =
            ASTMake3(AST_QUALIFIED_ID,
                    ast_copy(ASTSon0(declarator_id)),
                    ast_copy(ASTSon1(declarator_id)),
                    new_op,
                    ast_get_locus(declarator_id),
                    NULL);
    }
    else if (ASTKind(declarator_id) == AST_QUALIFIED_ID
            && ASTKind(ASTSon2(declarator_id)) == AST_DESTRUCTOR_TEMPLATE_ID)
    {
        is_template_id = 1;
        AST unqualified_part = ASTSon2(declarator_id);

        AST name = ASTLeaf(AST_SYMBOL,
                ast_get_locus(unqualified_part),
                ast_get_text(unqualified_part));

        AST destructor_id = ASTMake1(AST_DESTRUCTOR_ID,
                name,
                ast_get_locus(unqualified_part),
                NULL);

        declarator_id_without_template_id =
            ASTMake3(AST_QUALIFIED_ID,
                    ast_copy(ASTSon0(declarator_id)),
                    ast_copy(ASTSon1(declarator_id)),
                    destructor_id,
                    ast_get_locus(declarator_id),
                    NULL);
    }
    else
    {
        is_template_id = 0;
        declarator_id_without_template_id = declarator_id;
    }

    decl_flags_t decl_flags = DF_DEPENDENT_TYPENAME;
    decl_context_t* lookup_context = decl_context_clone(decl_context);
    if (!is_qualified)
    {
        decl_flags |= DF_ONLY_CURRENT_SCOPE;
    }

    lookup_context->current_scope = lookup_context->namespace_scope;

    scope_entry_list_t* entry_list
        = query_id_expression_flags(lookup_context, declarator_id_without_template_id, NULL, decl_flags);

    // Summary:
    //  1. The declaration is not a template function
    //      1.1 It's a qualified or unqualified template-id -> refers to a specialization of a function template
    //      1.2 It's a qualified name -> refers to:
    //          *   A nontemplate function, otherwise
    //          *   A matching specialization of a template function
    //      1.3 It's an unqualified name -> declares an nontemplate function
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
        entry_list_iterator_free(it);
    }

    if (!is_template_function)
    {
        if (is_template_id) // 1.1
        {
            if (!found_candidate)
            {
                error_printf_at(ast_get_locus(declarator_id), "template-id '%s' does not refer to a specialization of a function template\n", prettyprint_in_buffer(declarator_id));
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
            entry_list_iterator_free(it);

            if (!found_candidate)
            {
                error_printf_at(ast_get_locus(declarator_id), "name '%s' does not match with any nontemplate function or specialization of a function template\n", prettyprint_in_buffer(declarator_id));
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
                error_printf_at(ast_get_locus(declarator_id), "qualified id '%s' name not found\n", prettyprint_in_buffer(declarator_id));
                return 0;
            }
        }

        // We should create a new SK_TEMPLATE
        const char* declarator_name = NULL;
        switch (ASTKind(declarator_id))
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

        func_templ = NEW0(scope_entry_t);
        func_templ->symbol_name = declarator_name;

        func_templ->kind = SK_TEMPLATE;
        func_templ->locus = ast_get_locus(declarator_id);
        symbol_entity_specs_set_is_friend_declared(func_templ, 1);

        decl_context_t *updated_decl_context = decl_context_clone(decl_context);
        updated_decl_context->current_scope = decl_context->namespace_scope;
        func_templ->decl_context = updated_decl_context;

        func_templ->type_information =
            get_new_template_type(decl_context->template_parameters, declarator_type,
                    ASTText(declarator_id), decl_context, ast_get_locus(declarator_id));

        // Perhaps we may need to update some entity specs of the primary symbol
        type_t* primary_type = template_type_get_primary_type(func_templ->type_information);
        scope_entry_t* primary_symbol = named_type_get_symbol(primary_type);
        symbol_entity_specs_set_any_exception(primary_symbol, gather_info->any_exception);

        template_type_set_related_symbol(func_templ->type_information, func_templ);

        // We should update the declarator type with the primary type
        declarator_type = primary_symbol->type_information;
    }

    //We create a new symbol always
    scope_entry_t* new_entry = NEW0(scope_entry_t);

    new_entry->kind = SK_DEPENDENT_FRIEND_FUNCTION;
    new_entry->locus = ast_get_locus(declarator_id);

    new_entry->type_information = declarator_type;

    decl_context_t *updated_decl_context = decl_context_clone(decl_context);
    updated_decl_context->current_scope = decl_context->namespace_scope;
    new_entry->decl_context = updated_decl_context;

    nodecl_t nodecl_name = nodecl_null();
    compute_nodecl_name_from_id_expression(declarator_id, decl_context, &nodecl_name);
    new_entry->value = nodecl_name;
    //The symbol name has been computed by Codegen!!
    new_entry->symbol_name = uniquestr(codegen_to_str(nodecl_name, decl_context));

    symbol_entity_specs_set_is_friend_declared(new_entry, 1);
    symbol_entity_specs_set_any_exception(new_entry, gather_info->any_exception);

    symbol_entity_specs_reserve_default_argument_info(new_entry,
            gather_info->num_arguments_info);
    int i;
    for (i = 0; i < gather_info->num_arguments_info; i++)
    {
        if (!nodecl_is_null(gather_info->arguments_info[i].argument))
        {
            default_argument_info_t* default_argument = NEW0(default_argument_info_t);
            default_argument->argument = gather_info->arguments_info[i].argument;
            default_argument->context = gather_info->arguments_info[i].context;

            symbol_entity_specs_set_default_argument_info_num(new_entry, i,
                    default_argument);
        }
    }

    if (is_template_id)
    {
        // We should store the candidates list because It will be used during
        // the instantiation of the current class
        scope_entry_t** array = NULL;
        int num_items = 0;
        entry_list_to_symbol_array(filtered_entry_list,
                &array,
                &num_items);

        for (i = 0; i < num_items; i++)
        {
            symbol_entity_specs_add_friend_candidates(new_entry, array[i]);
        }
        DELETE(array);
    }

    *result_entry = new_entry;
    entry_list_free(filtered_entry_list);
    entry_list_free(entry_list);
    return 1;
}

static char same_template_parameter_list(
        template_parameter_list_t* template_parameter_list_1,
        template_parameter_list_t* template_parameter_list_2,
        const decl_context_t* decl_context)
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
            if (!equivalent_types(
                        template_parameter_list_1->parameters[i]->entry->type_information,
                        template_parameter_list_2->parameters[i]->entry->type_information))
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
        const decl_context_t* decl_context,
        scope_entry_t** result_entry)
{
    *result_entry = NULL;

    AST considered_tree = declarator_id;
    if (ASTKind(declarator_id) == AST_QUALIFIED_ID)
    {
        considered_tree = ASTSon2(declarator_id);
    }

    char declarator_is_template_id = (ASTKind(considered_tree) == AST_TEMPLATE_ID
            || ASTKind(considered_tree) == AST_OPERATOR_FUNCTION_ID_TEMPLATE);

    // Template function declarations that are friend declared cannot be partially specialized
    if (gather_info->is_friend
            && gather_info->is_template
            && declarator_is_template_id)
    {
        error_printf_at(ast_get_locus(declarator_id), "invalid use of a template-id '%s' in a template friend function declaration\n",
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
    if (BITMAP_TEST(decl_context->decl_flags, DF_CONSTRUCTOR))
    {
        decl_flags |= DF_CONSTRUCTOR;
    }

    // Restrict ourselves to the current scope
    // if the declarator-id is unqualified
    // and we are not naming a friend
    decl_context_t* lookup_context = decl_context_clone(decl_context);
    if (!gather_info->is_friend)
    {
        switch (ASTKind(declarator_id))
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
        if (ASTKind(declarator_id) != AST_QUALIFIED_ID)
        {
            lookup_context->current_scope = lookup_context->namespace_scope;
        }

        // The class or function is not a template class or template function
        if (!gather_info->is_template
                // The 'declarator_id' is not a template-id
                && ASTKind(declarator_id) != AST_TEMPLATE_ID)
        {
            switch (ASTKind(declarator_id))
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
        scope_entry_t* result = NEW0(scope_entry_t);
        result->kind = SK_DEPENDENT_FRIEND_FUNCTION;
        result->locus = ast_get_locus(declarator_id);

        if (ASTKind(declarator_id) == AST_TEMPLATE_ID)
        {
            result->symbol_name = ASTText(ASTSon0(declarator_id));
        }
        else
        {
            result->symbol_name = ASTText(declarator_id);
        }

        symbol_entity_specs_set_any_exception(result, gather_info->any_exception);
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
            error_printf_at(ast_get_locus(declarator_id), "name '%s' has already been declared as a different entity kind\n",
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
            nesting_check_t nest_check = check_template_nesting_of_name(considered_symbol, decl_context->template_parameters);

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

            type_t* considered_type = considered_symbol->type_information;
            type_t* considered_type_advanced_to_context = considered_type;
            type_t* function_type_being_declared_advanced_to_context = function_type_being_declared;

            if (IS_CXX_LANGUAGE
                    && symbol_entity_specs_get_is_member(considered_symbol))
            {
                /*
                   Make sure the two types look like the same

                   template <typename T>
                   struct B { };
                   template <typename T>
                   struct A
                   {
                      typename B<T>::S foo(); // (1)
                      typedef B<T>::S Q;
                   };

                   template <typename T>
                   typename A<T>::Q A<T>::foo() { } // (2)

                   Note that in (2), 'typename A<T>::Q' is actually 'typename
                   B<T>::S', but the syntactic nature of a dependent typename
                   hinders us from claiming that they are the same.

                   We can only discover that this is so if we contextually
                   advance the types of the two declarations (note that it is
                   not always that (2) must be advanced, sometimes (1) must be
                   advanced too). This means that we can examine uninstantiated
                   templates only if they belong to the class of the member
                   function being declared (i.e. we can examine inside A<T>
                   because we are declaring A<T>::foo).

                   This process is performed in fix_dependent_typenames_in_context,
                   this way we avoid passing a context to equivalent_types
               */

                // fprintf(stderr, "%s: CONSIDERED FUNCTION TYPE [before] -> %s\n",
                //         ast_location(declarator_id),
                //         print_declarator(considered_type));
                considered_type_advanced_to_context =
                    fix_dependent_typenames_in_context(considered_type,
                            entry->decl_context,
                            ast_get_locus(declarator_id));
                // fprintf(stderr, "%s: CONSIDERED FUNCTION TYPE [after] -> %s\n",
                //         ast_location(declarator_id),
                //         print_declarator(fixed_considered_type));

                // fprintf(stderr, "%s: DECLARED FUNCTION TYPE [before] -> %s\n",
                //         ast_location(declarator_id),
                //         print_declarator(function_type_being_declared_advanced_to_context));
                function_type_being_declared_advanced_to_context =
                    fix_dependent_typenames_in_context(function_type_being_declared,
                            entry->decl_context,
                            ast_get_locus(declarator_id));
                // fprintf(stderr, "%s: DECLARED FUNCTION TYPE [after] -> %s\n",
                //         ast_location(declarator_id),
                //         print_declarator(function_type_being_declared_advanced_to_context));
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
                fprintf(stderr, "BUILDSCOPE: Types used for comparison will be\n"
                                "BUILDSCOPE:    existing '%s'\n"
                                "BUILDSCOPE:    current  '%s'\n",
                        print_declarator(considered_type_advanced_to_context),
                        print_declarator(function_type_being_declared_advanced_to_context));
            }

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
                if (equivalent_types(function_type_being_declared_advanced_to_context, considered_type_advanced_to_context))
                {
                    template_parameter_list_t* decl_template_parameters = decl_context->template_parameters;

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
                        function_type_being_declared_advanced_to_context,
                        considered_type_advanced_to_context);

                CXX11_LANGUAGE()
                {
                    if ((function_type_get_ref_qualifier(function_type_being_declared) != REF_QUALIFIER_NONE)
                            != (function_type_get_ref_qualifier(considered_type) != REF_QUALIFIER_NONE))
                    {
                        error_printf_at(ast_get_locus(declarator_id), "declaration cannot overload '%s'\n",
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
                        error_printf_at(ast_get_locus(declarator_id),
                                "function '%s' has been declared with different prototype\n",
                                ASTText(declarator_id));
                        info_printf_at(entry->locus,
                                "previous declaration is '%s'\n",
                                print_decl_type_str(considered_type, decl_context, entry->symbol_name));
                        info_printf_at(ast_get_locus(declarator_id),
                                "current declaration is '%s'\n",
                                print_decl_type_str(function_type_being_declared, decl_context, entry->symbol_name));
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

        template_parameter_list_t *explicit_template_arguments = NULL;
        if (declarator_is_template_id)
        {
            explicit_template_arguments =
                get_template_arguments_from_syntax(ASTSon1(considered_tree), decl_context);
        }

        // This function ignores non-templates
        scope_entry_list_t* solved_templates = solve_template_function_in_declaration(
                candidates,
                explicit_template_arguments,
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

            error_printf_at(ast_get_locus(declarator_id), "ambiguous template specialization '%s'\n",
                    print_decl_type_str(function_type_being_declared, decl_context, full_name));

            for (it = entry_list_iterator_begin(result_function_list);
                    !entry_list_iterator_end(it);
                    entry_list_iterator_next(it))
            {
                scope_entry_t* current_entry = entry_list_iterator_current(it);

                info_printf_at(current_entry->locus, "%s\n",
                        print_decl_type_str(current_entry->type_information, current_entry->decl_context, 
                            get_qualified_symbol_name(current_entry, current_entry->decl_context)));
            }
            entry_list_iterator_free(it);
            entry_list_free(result_function_list);

            // Error due to ambiguity
            return 0;
        }
    }
    else if (gather_info->is_explicit_specialization
            || gather_info->is_explicit_instantiation)
    {
        // We should have found something
        return 0;
    }

    // No error
    return 1;
}

/*
 * This function saves the current linkage, sets the new and restores it back.
 */
static void build_scope_linkage_specifier(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output)
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
        const decl_context_t* decl_context,
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

    if (ASTKind(declaration)  == AST_LINKAGE_SPEC_DECL)
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
        const decl_context_t* decl_context,
        const locus_t* locus)
{
    char can_delete = 1;
    if (entry->defined)
    {
        const char* qualified_name = get_qualified_symbol_name(entry, decl_context);

        error_printf_at(locus, "cannot delete function '%s' already defined\n",
                print_decl_type_str(entry->type_information,
                    decl_context,
                    qualified_name));

        can_delete = 0;
    }

    if (can_delete)
    {
        symbol_entity_specs_set_is_deleted(entry, 1);
        entry->defined = 1;
    }
}

static void build_scope_deleted_function_definition(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output)
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
        if (symbol_entity_specs_get_default_argument_info_num(entry, j) != NULL)
            return 1;
    }
    return 0;
}

static char function_has_exception_specification_valid_for_defaulted(scope_entry_t* entry)
{
    if (nodecl_is_null(symbol_entity_specs_get_noexception(entry)))
    {
        return symbol_entity_specs_get_any_exception(entry);
    }
    else
    {
        if (nodecl_expr_is_value_dependent(symbol_entity_specs_get_noexception(entry)))
            return 1;

        // Must be true
        return (nodecl_is_constant(symbol_entity_specs_get_noexception(entry))
                && const_value_is_nonzero(
                    nodecl_get_constant(symbol_entity_specs_get_noexception(entry))));
    }
}

static void set_defaulted_outside_class_specifier(
        scope_entry_t* entry,
        const decl_context_t* decl_context,
        const locus_t* locus)
{
    char can_default = 1;

    if (entry->defined)
    {
        const char* qualified_name = get_qualified_symbol_name(entry, decl_context);

        error_printf_at(locus, "cannot default function '%s' already defined\n",
                print_decl_type_str(entry->type_information,
                    decl_context,
                    qualified_name));

        can_default = 0;
    }

    // Must be a special member
    if ((!symbol_entity_specs_get_is_default_constructor(entry)
            && !symbol_entity_specs_get_is_copy_constructor(entry)
            && !symbol_entity_specs_get_is_move_constructor(entry)
            && !symbol_entity_specs_get_is_copy_assignment_operator(entry)
            && !symbol_entity_specs_get_is_move_assignment_operator(entry)
            && !symbol_entity_specs_get_is_destructor(entry))
            // Without default arguments
            || function_has_default_arguments(entry)
            // Without exception specification
            || !function_has_exception_specification_valid_for_defaulted(entry))
    {
        const char* qualified_name = get_qualified_symbol_name(entry, decl_context);

        error_printf_at(locus, "function '%s' cannot be defaulted\n",
                print_decl_type_str(entry->type_information,
                    decl_context,
                    qualified_name));
        can_default = 0;
    }

    if (can_default)
    {
        symbol_entity_specs_set_is_defaulted(entry, 1);
        entry->defined = 1;
    }
}

static void set_defaulted_inside_class_specifier(
        scope_entry_t* entry,
        const decl_context_t* decl_context,
        const locus_t* locus)
{
    char can_default = 1;

    if (entry->defined)
    {
        const char* qualified_name = get_qualified_symbol_name(entry, decl_context);

        error_printf_at(locus, "cannot default function '%s' already defined\n",
                print_decl_type_str(entry->type_information,
                    decl_context,
                    qualified_name));

        can_default = 0;
    }

    if (can_default)
    {
        entry->defined = 1;
        symbol_entity_specs_set_is_defaulted(entry, 1);
        symbol_entity_specs_set_is_defined_inside_class_specifier(entry, 1);

#if 0
        make_empty_body_for_default_function(entry, decl_context, locus);
#endif
    }
}

static void build_scope_defaulted_function_definition(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output)
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
        const decl_context_t* decl_context,
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
    decl_context_t* template_context;
    build_scope_template_header(ASTSon0(a), decl_context, &template_context, nodecl_output);

    AST templated_decl = ASTSon1(a);
    if (ASTKind(templated_decl) == AST_AMBIGUITY)
    {
        solve_ambiguous_declaration(templated_decl, template_context);
    }

    switch (ASTKind(templated_decl))
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
            internal_error("Unknown node type '%s' (line=%s)\n", ast_print_node_type(ASTKind(templated_decl)),
                    ast_location(templated_decl));
    }

}

static void push_new_template_header_level(const decl_context_t* decl_context,
        decl_context_t* *template_context,
        char is_explicit_specialization)
{
    (*template_context) = decl_context_clone(decl_context);
    // A new level of template nesting
    (*template_context)->template_parameters = NEW0(template_parameter_list_t);

    (*template_context)->template_parameters->is_explicit_specialization = is_explicit_specialization;
    (*template_context)->template_parameters->enclosing = decl_context->template_parameters;
}

void build_scope_template_header(AST template_parameter_list, 
        const decl_context_t* decl_context, 
        decl_context_t* *template_context,
        nodecl_t* nodecl_output)
{
    push_new_template_header_level(decl_context, template_context, /* is explicit specialization */ 0);

    int nesting = get_template_nesting_of_context(decl_context) + 1;

    build_scope_template_parameter_list(template_parameter_list, 
            (*template_context)->template_parameters, 
            nesting,
            (*template_context), nodecl_output);
}

/*
 * This function registers an explicit template specialization
 */
static void build_scope_explicit_template_specialization(AST a, 
        const decl_context_t* decl_context, 
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list)
{
    decl_context_t* template_context;
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

    if (ASTKind(ASTSon0(a)) == AST_AMBIGUITY)
    {
        solve_ambiguous_declaration(ASTSon0(a), template_context);
    }

    switch (ASTKind(ASTSon0(a)))
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
                error_printf_at(ast_get_locus(ASTSon0(a)), "invalid alias-declaration in explicit template specialization\n");
                break;
            }
        default :
            {
                internal_error("Unknown node type '%s'\n", ast_print_node_type(ASTKind(ASTSon0(a))));
            }
    }
}

static void build_scope_template_function_definition(
        AST function_definition,
        const decl_context_t* decl_context,
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
        const decl_context_t* decl_context,
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
        const decl_context_t* decl_context,
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

static void build_scope_template_simple_declaration(AST a, const decl_context_t* decl_context,
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
                    && ASTKind(type_specifier) == AST_CLASS_SPECIFIER)
            {
                error_printf_at(ast_get_locus(init_declarator_list), "invalid declarator in class template definition\n");
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

        if (ASTKind(init_declarator) == AST_AMBIGUITY)
        {
            solve_ambiguous_init_declarator(init_declarator, decl_context, &gather_info);
        }

        if (ASTSon0(init_declarator_list) != NULL)
        {
            error_printf_at(ast_get_locus(init_declarator), "too many declarators in template declaration\n");
        }

        AST declarator = ASTSon0(init_declarator);
        AST initializer = ASTSon1(init_declarator);

        // Note that the scope where this declarator will be declared includes
        // the template parameters, since the symbol will have to be qualified
        // it will not create a symbol in "st" but will fetch the previously
        // declared one within the class.
        type_t* declarator_type = NULL;

        const decl_context_t* new_decl_context = decl_context;

        compute_declarator_type(declarator,
                &gather_info, simple_type_info, &declarator_type,
                new_decl_context, nodecl_output);
        scope_entry_t *entry = build_scope_declarator_name(declarator,
                simple_type_info, declarator_type,
                &gather_info, new_decl_context);

        char ok = 1;
        if (entry == NULL)
        {
            ok = 0;
        }
        else if (entry->kind == SK_VARIABLE)
        {
            if (!symbol_entity_specs_get_is_member(entry)
                    || !symbol_entity_specs_get_is_static(entry))
            {
                error_printf_at(ast_get_locus(a), "entity '%s' must be a static data member\n",
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
                error_printf_at(ast_get_locus(a), "cannot initialize non-data member '%s\n",
                        get_qualified_symbol_name(entry, decl_context));
                ok = 0;
            }
        }
        else
        {
            error_printf_at(ast_get_locus(a), "invalid declaration of entity '%s'\n",
                    get_qualified_symbol_name(entry, decl_context));
            ok = 0;
        }

        if (!ok)
            return;

        keep_extra_attributes_in_symbol(entry, &gather_info);

        // Propagate the __extension__ attribute to the symbol
        symbol_entity_specs_set_gcc_extension(entry, gcc_extension);

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
                decl_context_t* initializer_context = decl_context_clone(entry->decl_context);
                initializer_context->template_parameters = new_decl_context->template_parameters;

                nodecl_t nodecl_expr = nodecl_null();
                check_initialization(initializer,
                        initializer_context,
                        entry,
                        get_unqualified_type(declarator_type),
                        &nodecl_expr,
                        /* is_auto_type */ 0,
                        /* is_decltype_auto */ 0);
                entry->value = nodecl_expr;

                entry->defined = 1;
            }
            else if (is_explicit_specialization)
            {
                diagnostic_context_push_buffered();
                scope_entry_t* constructor = NULL;
                char valid = check_default_initialization(entry, entry->decl_context, ast_get_locus(a), &constructor);
                diagnostic_context_pop_and_discard();

                if (valid && is_class_type_or_array_thereof(entry->type_information))
                {
                    check_default_initialization_and_destruction_declarator(entry, decl_context, ast_get_locus(a));
                }
            }
            else
            {
                // We may have to update the outermost array type
                if (is_array_type(declarator_type)
                        && is_array_type(entry->type_information)
                        && !nodecl_is_null(array_type_get_array_size_expr(declarator_type))
                        && nodecl_is_null(array_type_get_array_size_expr(entry->type_information)))
                {
                    // template <typename T>
                    // struct A
                    // {
                    //   static int c[];
                    // };
                    //
                    // template <typename T>
                    // int A::c[10];         <-- We are in this declaration
                    entry->type_information = declarator_type;
                }

                entry->defined = 1;
            }
        }

        // Mark this as user declared from now
        symbol_entity_specs_set_is_user_declared(entry, 1);

        nodecl_t (*make_cxx_decl_or_def)(nodecl_t, scope_entry_t*, const locus_t*) =
            // Only variables are actually defined, everything else is a declaration
            (entry->kind == SK_VARIABLE
             && entry->defined) ? nodecl_make_cxx_def : nodecl_make_cxx_decl;

        decl_context_t* updated_decl_context = decl_context_clone(decl_context);
        updated_decl_context->template_parameters = entry->decl_context->template_parameters;
        decl_context = updated_decl_context;

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
        const decl_context_t* template_context,
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
        const decl_context_t* template_context,
        nodecl_t* nodecl_output)
{
    switch (ASTKind(a))
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
                warn_printf_at(ast_get_locus(a), "template packs are only valid in C++11\n");
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
                warn_printf_at(ast_get_locus(a), "template packs are only valid in C++11\n");
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
            internal_error("Unknown node type '%s'", ast_print_node_type(ASTKind(a)));
    }
}

static void build_scope_template_template_parameter(AST a,
        template_parameter_list_t* template_parameters,
        int nesting,
        char is_template_pack,
        const decl_context_t* template_context,
        nodecl_t* nodecl_output)
{
    // These parameters have the form

    //    TEMPLATE < template_param_list > CLASS [...] [identifier] [= id_expr]
    //
    // "identifier" is then a template-name
    //
    // Construct parameter information
    decl_context_t* template_params_context = decl_context_clone(template_context);
    template_params_context->template_parameters = NEW0(template_parameter_list_t);

    build_scope_template_parameter_list(ASTSon0(a),
            template_params_context->template_parameters,
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

    scope_entry_t* new_entry = NEW0(scope_entry_t);
    new_entry->symbol_name = template_parameter_name;
    new_entry->decl_context = template_context;

    new_entry->locus = ast_get_locus(a);

    if (!is_template_pack)
        new_entry->kind = SK_TEMPLATE_TEMPLATE_PARAMETER;
    else
        new_entry->kind = SK_TEMPLATE_TEMPLATE_PARAMETER_PACK;

    symbol_entity_specs_set_is_template_parameter(new_entry, 1);
    symbol_entity_specs_set_template_parameter_nesting(new_entry, nesting);
    symbol_entity_specs_set_template_parameter_position(new_entry, template_parameters->num_parameters);

    // This is a faked class type
    type_t* primary_type = get_new_class_type(template_context, TT_CLASS);

    new_entry->type_information = get_new_template_type(template_params_context->template_parameters, 
            /* primary_type = */ primary_type, template_parameter_name, template_context,
            new_entry->locus);
    set_is_dependent_type(new_entry->type_information, 1);

    template_type_set_related_symbol(new_entry->type_information, new_entry);

    template_parameter_t *template_parameter = NEW0(template_parameter_t);
    template_parameter->entry = new_entry;

    template_parameter_value_t* default_argument = NULL;

    AST id_expr = ASTSon2(a);
    if (id_expr != NULL)
    {
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
            error_printf_at(ast_get_locus(id_expr), "'%s' does not name a template class\n",
                    prettyprint_in_buffer(id_expr));
            return;
        }

        scope_entry_t* entry = entry_list_head(filtered_entry_list);
        entry_list_free(filtered_entry_list);

        if (entry->kind == SK_TEMPLATE
                && named_type_get_symbol(template_type_get_primary_type(entry->type_information))->kind != SK_CLASS)
        {
            error_printf_at(ast_get_locus(id_expr), "'%s' does not name a template class\n",
                    prettyprint_in_buffer(id_expr));
            return;
        }

        default_argument = NEW0(template_parameter_value_t);
        // We need a named type
        default_argument->type = get_user_defined_type(entry);
        default_argument->is_default = 1;
        default_argument->kind = TPK_TEMPLATE;

        if (is_template_pack)
        {
            error_printf_at(ast_get_locus(id_expr), "a template-template pack cannot have a default argument\n");
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
        const decl_context_t* template_context,
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

    unsigned int line;
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

    scope_entry_t* new_entry = NEW0(scope_entry_t);
    new_entry->decl_context = template_context;
    new_entry->symbol_name = template_parameter_name;


    new_entry->locus = make_locus(file, line, 0);
    if (!is_template_pack)
        new_entry->kind = SK_TEMPLATE_TYPE_PARAMETER;
    else
        new_entry->kind = SK_TEMPLATE_TYPE_PARAMETER_PACK;

    symbol_entity_specs_set_is_template_parameter(new_entry, 1);
    symbol_entity_specs_set_template_parameter_nesting(new_entry, nesting);
    symbol_entity_specs_set_template_parameter_position(new_entry, template_parameters->num_parameters);

    template_parameter_t* template_parameter = NEW0(template_parameter_t);
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

        default_argument = NEW0(template_parameter_value_t);
        default_argument->type = declarator_type;
        default_argument->is_default = 1;
        default_argument->kind = TPK_TYPE;

        if (is_template_pack)
        {
            error_printf_at(ast_get_locus(type_id), "a type-template parameter pack cannot have a default argument\n");
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
        const decl_context_t* template_context,
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

    char keep_is_inside_pack_expansion = get_is_inside_pack_expansion();
    char this_is_a_pack = 0;
    if (get_declarator_id_pack(parameter_declarator, template_context) != NULL)
    {
        set_is_inside_pack_expansion(1);
        this_is_a_pack = 1;
    }

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
            fprintf(stderr, "BUILDSCOPE: Registering '%s' as a non-type template parameter with nesting %d and position %d\n",
                    template_parameter_name,
                    nesting,
                    template_parameters->num_parameters);
        }
    }
    else
    {
        uniquestr_sprintf(&template_parameter_name, "__nontype_tpl_param_%d_%d__", 
                nesting, 
                template_parameters->num_parameters);
    }
    entry = NEW0(scope_entry_t);
    entry->symbol_name = template_parameter_name;
    entry->decl_context = template_context;

    if (!IS_CXX11_LANGUAGE && this_is_a_pack)
    {
        warn_printf_at(ast_get_locus(a), "template-packs are only valid in C++11\n");
    }

    // This is not a variable, but a template parameter
    if (!this_is_a_pack)
        entry->kind = SK_TEMPLATE_NONTYPE_PARAMETER;
    else
        entry->kind = SK_TEMPLATE_NONTYPE_PARAMETER_PACK;

    entry->type_information = declarator_type;
    symbol_entity_specs_set_is_template_parameter(entry, 1);
    symbol_entity_specs_set_template_parameter_nesting(entry, nesting);
    symbol_entity_specs_set_template_parameter_position(entry, template_parameters->num_parameters);

    // Save its symbol
    template_parameter_t* template_parameter = NEW0(template_parameter_t);
    template_parameter->entry = entry;
    template_parameter_value_t* default_argument = NULL;
    if (default_expression != NULL)
    {
        nodecl_t nodecl_expr;
        if (!check_nontype_template_argument_expression(default_expression, template_context, &nodecl_expr))
        {
            error_printf_at(ast_get_locus(default_expression), "could not check default argument of template parameter '%s'\n",
                    prettyprint_in_buffer(default_expression));
        }

        default_argument = NEW0(template_parameter_value_t);
        default_argument->value = nodecl_expr;
        default_argument->type = declarator_type;
        default_argument->is_default = 1;
        default_argument->kind = TPK_NONTYPE;

        if (this_is_a_pack)
        {
            error_printf_at(ast_get_locus(default_expression), "a nontype-template pack cannot have a default argument\n");
        }
    }

    if (!this_is_a_pack)
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

    set_is_inside_pack_expansion(keep_is_inside_pack_expansion);
}

static void build_scope_namespace_alias(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    if (decl_context->current_scope->kind != NAMESPACE_SCOPE)
    {
        error_printf_at(ast_get_locus(a), "namespace alias in a non namespace scope\n");
        return;
    }
    // namespace alias_ident = id_expression;
    AST alias_ident = ASTSon0(a);
    AST id_expression = ASTSon1(a);

    scope_entry_list_t* entry_list = query_id_expression(decl_context, id_expression, NULL);
    if (entry_list == NULL
            || entry_list_head(entry_list)->kind != SK_NAMESPACE)
    {
        error_printf_at(ast_get_locus(id_expression), "'%s' does not name any namespace\n",
                prettyprint_in_buffer(id_expression));
        return;
    }

    scope_entry_t* entry = entry_list_head(entry_list);
    entry_list_free(entry_list);

    char create_new_alias = 1;
    const char* alias_name = ASTText(alias_ident);
    entry_list = query_in_scope_str(decl_context, alias_name, NULL);
    scope_entry_t* alias_entry = NULL;
    if (entry_list != NULL)
    {
        alias_entry = entry_list_head(entry_list);
        if (alias_entry->kind != SK_NAMESPACE)
        {
            error_printf_at(ast_get_locus(alias_ident), "'%s' does not name any namespace\n",
                    alias_name);
            return;
        }

        if (alias_entry->related_decl_context->current_scope->related_entry == entry)
        {
            // we don't need to create a new alias, reuse the current one
            create_new_alias = 0;
        }
    }
    entry_list_free(entry_list);

    if (create_new_alias)
    {
        alias_entry = new_symbol(decl_context, decl_context->current_scope, alias_name);

        alias_entry->locus = ast_get_locus(alias_ident);
        alias_entry->kind = SK_NAMESPACE;
        alias_entry->related_decl_context = entry->related_decl_context;
        alias_entry->defined = 1;
        symbol_entity_specs_set_is_user_declared(alias_entry, 1);
    }

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
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    AST namespace_name = ASTSon0(a);

    char is_inline = 0;
    AST namespace_inline = ASTSon3(a);

    if (namespace_inline != NULL)
    {
        ERROR_CONDITION(ASTKind(namespace_inline) != AST_INLINE_SPEC,
                "Invalid inline specifier tree", 0);

        is_inline = 1;
    }

    if (namespace_name != NULL)
    {
        ERROR_CONDITION((decl_context->current_scope->kind != NAMESPACE_SCOPE),
                "Incorrect scope, it should be a namespace scope", 0);

        // Register this namespace if it does not exist in this scope
        scope_entry_list_t* list = query_in_scope_str_flags(decl_context, ASTText(namespace_name), NULL, DF_ONLY_CURRENT_SCOPE);

        scope_entry_list_t* check_list = filter_symbol_non_kind(list, SK_NAMESPACE);

        if (check_list != NULL)
        {
            error_printf_at(ast_get_locus(namespace_name), "'%s' has already been declared as another entity kind\n",
                    prettyprint_in_buffer(namespace_name));
            return;
        }
        entry_list_free(check_list);

        scope_entry_t* entry = NULL;
        const decl_context_t* namespace_context;
        if (list != NULL &&
                entry_list_head(list)->kind == SK_NAMESPACE)
        {
            entry = entry_list_head(list);
            namespace_context = entry->related_decl_context;

            if (is_inline
                    && !symbol_entity_specs_get_is_inline(entry))
            {
                error_printf_at(ast_get_locus(a), "inline namespace extension of a non-inlined namespace\n");
                return;
            }
        }
        else
        {
            entry = new_symbol(decl_context, decl_context->current_scope, ASTText(namespace_name));
            namespace_context = new_namespace_context(decl_context, entry);

            entry->locus = ast_get_locus(namespace_name);
            entry->kind = SK_NAMESPACE;
            entry->related_decl_context = namespace_context;
            symbol_entity_specs_set_is_user_declared(entry, 1);

            // Link the scope of this newly created namespace
            if (is_inline)
            {
                symbol_entity_specs_set_is_inline(entry, 1);

                // An inline namespace is an associated namespace of the current namespace
                scope_t* namespace_scope = decl_context->current_scope;

                P_LIST_ADD_ONCE(namespace_scope->use_namespace, namespace_scope->num_used_namespaces,
                        entry);
            }
        }

        // Anonymous namespace cannot have gcc attributes
        AST attributes = ASTSon2(a);
        gather_decl_spec_t gather_info;
        memset(&gather_info, 0, sizeof(gather_info));
        gather_extra_attributes(attributes, &gather_info, decl_context);

        keep_extra_attributes_in_symbol(entry, &gather_info);

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

        const decl_context_t* namespace_context;
        if (list != NULL &&
                entry_list_head(list)->kind == SK_NAMESPACE)
        {
            scope_entry_t* entry = entry_list_head(list);

            entry_list_free(list);

            namespace_context = entry->related_decl_context;

            if (is_inline
                    && !symbol_entity_specs_get_is_inline(entry))
            {
                error_printf_at(ast_get_locus(a), "inline namespace extension of a non-inlined namespace\n");
                return;
            }
        }
        else
        {
            scope_entry_t* entry = new_symbol(decl_context, decl_context->current_scope, unnamed_namespace);
            namespace_context = new_namespace_context(decl_context, entry);

            entry->locus = ast_get_locus(a);
            entry->kind = SK_NAMESPACE;
            entry->related_decl_context = namespace_context;

            // Link the scope of this newly created namespace
            // And associate it to the current namespace
            scope_t* namespace_scope = decl_context->current_scope;

            // Anonymous namespace is implemented as an associated namespace of the current scope
            P_LIST_ADD_ONCE(namespace_scope->use_namespace, namespace_scope->num_used_namespaces,
                    entry);
        }

        if (ASTSon1(a) != NULL)
        {
            build_scope_declaration_sequence(
                ASTSon1(a), namespace_context, nodecl_output);
        }
    }
}


// This function is only intended for C99
void build_scope_kr_parameter_declaration(scope_entry_t* function_entry,
        AST kr_parameter_declaration, 
        AST kr_parameters UNUSED_PARAMETER,
        const decl_context_t* decl_context,
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

                scope_entry_t *entry = build_scope_declarator_name(declarator,
                        simple_type_info, declarator_type,
                        &current_gather_info, decl_context);

                if (!symbol_is_parameter_of_function(entry, function_entry))
                {
                    error_printf_at(ast_get_locus(init_declarator), "'%s' is not a parameter\n",
                            entry->symbol_name);
                    continue;
                }

                if (initializer != NULL)
                {
                    error_printf_at(ast_get_locus(initializer), "initializer given to a parameter\n");
                }

                if (current_gather_info.is_static)
                {
                    error_printf_at(ast_get_locus(init_declarator), "parameter '%s' defined to be static\n",
                            entry->symbol_name);
                }

                if (current_gather_info.is_extern)
                {
                    error_printf_at(ast_get_locus(init_declarator), "parameter '%s' defined to be extern\n",
                            entry->symbol_name);
                }

                entry->type_information = declarator_type;

                keep_extra_attributes_in_symbol(entry, &current_gather_info);

                int parameter_position = -1;

                int j;
                for (j = 0; j < symbol_entity_specs_get_num_related_symbols(function_entry) && parameter_position == -1; j++)
                {
                    if (symbol_entity_specs_get_related_symbols_num(function_entry, j) == entry)
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
                error_printf_at(ast_get_locus(simple_decl), "declaration does not declare anything\n");
            }
        }
    }

    // Update the type
    function_entry->type_information = get_new_function_type(
            function_type_get_return_type(function_entry->type_information),
            parameter_info,
            real_num_parameters, REF_QUALIFIER_NONE);
}

static void common_defaulted_or_deleted(AST a, const decl_context_t* decl_context, 
        void (*set)(scope_entry_t*, const decl_context_t*, const locus_t* locus),
        char is_template,
        char is_explicit_specialization,
        scope_entry_list_t** declared_symbols,
        nodecl_t* nodecl_output)
{
    CXX03_LANGUAGE()
    {
        warn_printf_at(ast_get_locus(a), "default/delete functions are a C++11 feature\n");
    }

    AST function_header = ASTSon0(a);

    if (ASTKind(function_header) == AST_AMBIGUITY)
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

    if (decl_spec_seq != NULL)
    {
        build_scope_decl_specifier_seq(decl_spec_seq, &gather_info,
                &type_info, decl_context, nodecl_output);
    }

    // function_declarator
    type_t* declarator_type = NULL;
    scope_entry_t* entry = NULL;

    const decl_context_t* new_decl_context = decl_context;

    compute_declarator_type(function_declarator, &gather_info, type_info,
            &declarator_type, new_decl_context, nodecl_output);
    entry = build_scope_declarator_name(function_declarator,
            type_info, declarator_type,
            &gather_info, new_decl_context);

    ERROR_CONDITION(entry == NULL, "Invalid symbol", 0);

    set(entry, decl_context, ast_get_locus(a));

    keep_extra_attributes_in_symbol(entry, &gather_info);

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
    if (symbol_entity_specs_get_num_related_symbols(entry) == 0)
    {
        symbol_entity_specs_reserve_related_symbols(entry, gather_info->num_arguments_info);
    }
    else
    {
        if (symbol_entity_specs_get_num_related_symbols(entry) != gather_info->num_arguments_info)
        {
            // Mismatching number of parameters. Reserve again
            symbol_entity_specs_free_related_symbols(entry);
            symbol_entity_specs_reserve_related_symbols(entry, gather_info->num_arguments_info);
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
                error_printf_at(locus, "parameter %d does not have a name\n", i + 1);
            }
        }

        scope_entry_t* current_param = gather_info->arguments_info[i].entry;
        if (current_param != NULL)
        {
            if (current_param->decl_context->current_scope->related_entry == NULL
                    && current_param->decl_context->current_scope->kind == PROTOTYPE_SCOPE)
            {
                // Make sure this prototype scope knows what function it refers
                // (A prototype scope may not have related entry because there
                // may be none. For instance in a nested function declarator)
                current_param->decl_context->current_scope->related_entry = entry;
            }

            // Remember this symbol as a parameter of entry
            symbol_set_as_parameter_of_function(current_param, entry, /* nesting */ 0, /* position */ i);
        }

        // We keep the first parameter declaration or the definition (ignoring any other declaration)
        if (is_definition
                || symbol_entity_specs_get_related_symbols_num(entry, i) == NULL)
        {
            symbol_entity_specs_set_related_symbols_num(entry, i, current_param);
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

    if (nodecl_get_kind(node) == NODECL_OBJECT_INIT)
        return mercurium_pretty_function_has_been_used(
                mercurium_pretty_function,
                nodecl_get_symbol(node)->value);

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
        char diagnose,
        char emit_error)
{
    if (symbol_entity_specs_get_is_virtual(entry))
    {
        if (diagnose)
        {
            warn_or_error_printf_at(locus, emit_error,
                    "a constexpr function cannot be virtual\n");
        }
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
            if (diagnose)
            {
                warn_or_error_printf_at(
                        locus,
                        emit_error,
                        "parameter types of a constexpr function must be a literal type or "
                        "reference to literal type\n");
            }
            return 0;
        }
    }

    if (!symbol_entity_specs_get_is_constructor(entry))
    {
        type_t* return_type = function_type_get_return_type(entry->type_information);

        if (!is_dependent_type(return_type)
                && !is_literal_type(no_ref(return_type)))
        {
            if (diagnose)
            {
                warn_or_error_printf_at(
                        locus,
                        emit_error,
                        "the return type of a constexpr function must be a literal type or reference to literal type\n");
            }
            return 0;
        }
    }

    return 1;
}

static void check_constexpr_function_statement_list(nodecl_t statement_list,
        int *num_seen_returns,
        int *num_seen_other_statements,
        int *num_asm_definitions,
        int *num_try_blocks,
        int *num_invalid_initializations)
{
    int num_items = 0;
    nodecl_t* l = nodecl_unpack_list(statement_list, &num_items);

    int i;
    for (i = 0; i < num_items; i++)
    {
        nodecl_t nodecl = l[i];
        node_t kind = nodecl_get_kind(nodecl);
        if (kind == NODECL_CXX_DECL
                || kind == NODECL_CXX_DEF
                || kind == NODECL_CXX_USING_DECL
                || kind == NODECL_CXX_USING_NAMESPACE)
        {
            if (kind == NODECL_CXX_DECL
                    || kind == NODECL_CXX_DEF
                    || kind == NODECL_OBJECT_INIT)
            {
                scope_entry_t* sym = nodecl_get_symbol(nodecl);
                if (sym->kind == SK_VARIABLE)
                {
                    (*num_seen_other_statements)++;
                    if (symbol_entity_specs_get_is_static(sym)
                            || symbol_entity_specs_get_is_thread_local(sym)
                            || !is_literal_type(sym->type_information)
                            || nodecl_is_null(sym->value))
                    {
                        (*num_invalid_initializations)++;
                    }
                }
            }
        }
        else if (kind == NODECL_RETURN_STATEMENT)
        {
            (*num_seen_returns)++;
        }
        else if (kind == NODECL_COMPOUND_STATEMENT
                || kind == NODECL_CONTEXT
                || kind == NODECL_DEFAULT_STATEMENT
                || kind == NODECL_DO_STATEMENT)
        {
            (*num_seen_other_statements)++;
            check_constexpr_function_statement_list(
                    nodecl_get_child(nodecl, 0),
                    num_seen_returns,
                    num_seen_other_statements,
                    num_asm_definitions,
                    num_try_blocks,
                    num_invalid_initializations);
        }
        else if (kind == NODECL_WHILE_STATEMENT
                || kind == NODECL_CASE_STATEMENT
                || kind == NODECL_SWITCH_STATEMENT)
        {
            (*num_seen_other_statements)++;
            check_constexpr_function_statement_list(
                    nodecl_get_child(nodecl, 1),
                    num_seen_returns,
                    num_seen_other_statements,
                    num_asm_definitions,
                    num_try_blocks,
                    num_invalid_initializations);
        }
        else if (kind == NODECL_IF_ELSE_STATEMENT)
        {
            (*num_seen_other_statements)++;
            check_constexpr_function_statement_list(
                    nodecl_get_child(nodecl, 1),
                    num_seen_returns,
                    num_seen_other_statements,
                    num_asm_definitions,
                    num_try_blocks,
                    num_invalid_initializations);
            check_constexpr_function_statement_list(
                    nodecl_get_child(nodecl, 2),
                    num_seen_returns,
                    num_seen_other_statements,
                    num_asm_definitions,
                    num_try_blocks,
                    num_invalid_initializations);
        }
        else if (kind == NODECL_ASM_DEFINITION)
        {
            (*num_seen_other_statements)++;
            (*num_asm_definitions)++;
        }
        else if (kind == NODECL_TRY_BLOCK)
        {
            (*num_seen_other_statements)++;
            (*num_try_blocks)++;
        }
        else if (kind == NODECL_EXPRESSION_STATEMENT)
        {
            (*num_seen_other_statements)++;
        }
        else if (kind == NODECL_CXX_STATIC_ASSERT)
        {
            // do not count these
        }
        else
        {
            internal_error("Code unreachable: %s\n", ast_print_node_type(kind));
        }
    }
}

char check_constexpr_constructor(scope_entry_t* entry,
        const locus_t* locus,
        nodecl_t nodecl_initializer_list,
        char diagnose,
        char emit_error)
{
    scope_entry_t* class_symbol = named_type_get_symbol(symbol_entity_specs_get_class_type(entry));

    // We assume it could be constexpr
    if (is_dependent_type(class_symbol->type_information)
            || is_dependent_type(entry->type_information))
        return 1;

    scope_entry_list_t* virtual_base_classes = class_type_get_virtual_base_classes(class_symbol->type_information);

    if (virtual_base_classes != NULL)
    {
        if (diagnose)
        {
            warn_or_error_printf_at(locus, emit_error,
                    "a constructor of a class with virtual base classes cannot be constexpr\n");
        }
        entry_list_free(virtual_base_classes);
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
            if (diagnose)
            {
                warn_or_error_printf_at(
                        locus,
                        emit_error,
                        "parameter types of a constexpr constructor must be a literal type or "
                        "reference to literal type\n");
            }
            return 0;
        }
    }

    if (!nodecl_is_null(symbol_entity_specs_get_function_code(entry)))
    {
        nodecl_t nodecl_function_code = symbol_entity_specs_get_function_code(entry);
        nodecl_t nodecl_context = nodecl_get_child(nodecl_function_code, 0);

        nodecl_t nodecl_list = nodecl_get_child(nodecl_context, 0);
        ERROR_CONDITION(nodecl_list_length(nodecl_list) != 1, "Invalid function code", 0);

        nodecl_t nodecl_body = nodecl_list_head(nodecl_list);

        if (nodecl_get_kind(nodecl_body) == AST_TRY_BLOCK)
        {
            if (diagnose)
            {
                warn_or_error_printf_at(
                        locus,
                        emit_error,
                        "the body of a constexpr construct cannot be a try block\n");
            }
            return 0;
        }

        ERROR_CONDITION(nodecl_get_kind(nodecl_body) != NODECL_COMPOUND_STATEMENT, "Invalid node", 0);

        nodecl_t statement_list = nodecl_get_child(nodecl_body, 0);

        int num_seen_other_statements = 0,
            num_seen_returns = 0,
            num_asm_definitions = 0,
            num_try_blocks = 0,
            num_invalid_initializations = 0;

        check_constexpr_function_statement_list(statement_list,
                &num_seen_returns,
                &num_seen_other_statements,
                &num_asm_definitions,
                &num_try_blocks,
                &num_invalid_initializations);
    
        if (IS_CXX14_LANGUAGE)
        {
            if (num_asm_definitions > 0)
            {
                if (diagnose)
                {
                    warn_or_error_printf_at(
                            nodecl_get_locus(nodecl_body),
                            emit_error,
                            "the body of a constexpr function cannot contain asm-blocks\n");
                }
                return 0;
            }
            if (num_try_blocks > 0)
            {
                if (diagnose)
                {
                    warn_or_error_printf_at(
                            nodecl_get_locus(nodecl_body),
                            emit_error,
                            "the body of a constexpr function cannot contain try-blocks\n");
                }
                return 0;
            }
            if (num_invalid_initializations > 0)
            {
                if (diagnose)
                {
                    warn_or_error_printf_at(
                            nodecl_get_locus(nodecl_body),
                            emit_error,
                            "the body of a constexpr function cannot contain a "
                            "non-initialized variable, thread_local, static or of non-literal type\n");
                }
                return 0;
            }
        }
        else if (IS_CXX11_LANGUAGE)
        {
            if (num_seen_returns != 0
                    || num_seen_other_statements != 0)
            {
                if (diagnose)
                {
                    warn_or_error_printf_at(
                            locus,
                            emit_error,
                            "the body of a constexpr construct must be empty\n");
                }
                return 0;
            }
        }
    }

    // Maybe this is a delegating constructor
    if (nodecl_list_length(nodecl_initializer_list) == 1)
    {
        nodecl_t first = nodecl_list_head(nodecl_initializer_list);

        ERROR_CONDITION(nodecl_get_kind(first) != NODECL_MEMBER_INIT
                && nodecl_get_kind(first) != NODECL_IMPLICIT_MEMBER_INIT,
                "Invalid node", 0);
        scope_entry_t* sym = nodecl_get_symbol(first);

        if (sym == class_symbol)
        {
            // This is a forwarding constructor, check that the target constructor
            // is a constexpr constructor
            nodecl_t initializer = nodecl_get_child(first, 0);
            ERROR_CONDITION(nodecl_get_kind(initializer) != NODECL_FUNCTION_CALL,
                    "Invalid node", 0);

            nodecl_t called_function = nodecl_get_child(initializer, 0);
            scope_entry_t* target_constructor = nodecl_get_symbol(called_function);

            if (!symbol_entity_specs_get_is_constexpr(target_constructor))
            {
                error_printf_at(nodecl_get_locus(initializer), "a constexpr delegating constructor must target a constexpr constructor\n");
                return 0;
            }
            else
            {
                // This is enough
                return 1;
            }
        }
    }

    scope_entry_t** all_members = NULL;
    int num_all_members = 0;
    {
        scope_entry_list_t* nonstatic_data_members = class_type_get_nonstatic_data_members(class_symbol->type_information);
        scope_entry_list_t* base_classes = class_type_get_direct_base_classes_canonical(class_symbol->type_information);

        scope_entry_list_t* all_members_list = entry_list_concat(base_classes, nonstatic_data_members);
        entry_list_free(nonstatic_data_members);
        entry_list_free(base_classes);

        entry_list_to_symbol_array(all_members_list, &all_members, &num_all_members);
        entry_list_free(all_members_list);
    }

    char initialized[num_all_members + 1];
    memset(initialized, 0, sizeof(initialized));

    char is_constexpr_initialized[num_all_members + 1];
    memset(is_constexpr_initialized, 0, sizeof(is_constexpr_initialized));

    int num_items_initializer_list = 0;
    nodecl_t* list = nodecl_unpack_list(nodecl_initializer_list, &num_items_initializer_list);
    for (i = 0; i < num_items_initializer_list; i++)
    {
        ERROR_CONDITION(nodecl_get_kind(list[i]) != NODECL_MEMBER_INIT
                && nodecl_get_kind(list[i]) != NODECL_IMPLICIT_MEMBER_INIT,
                "Invalid node", 0);

        scope_entry_t* sym = nodecl_get_symbol(list[i]);

        int j;
        for (j = 0; j < num_all_members; j++)
        {
            if (sym == all_members[j])
            {
                initialized[i] = 1;
                is_constexpr_initialized[i] = 1;

                if (sym->kind == SK_CLASS
                        || (sym->kind == SK_VARIABLE
                            && is_class_type(sym->type_information)))
                {
                    nodecl_t initializer = nodecl_get_child(list[i], 0);

                    if (nodecl_get_kind(initializer) == NODECL_FUNCTION_CALL)
                    {
                        nodecl_t called = nodecl_get_child(initializer, 0);
                        if (nodecl_get_symbol(called) != NULL)
                        {
                            scope_entry_t* called_func = nodecl_get_symbol(called);
                            if (symbol_entity_specs_get_is_constructor(called_func))
                            {
                                is_constexpr_initialized[i] = symbol_entity_specs_get_is_constexpr(called_func);
                            }
                            else
                            {
                                internal_error("Expecting a call to a consructor here", 0);
                            }
                        }
                    }
                }
                break;
            }
        }
    }
    DELETE(list);

    for (i = 0; i < num_all_members; i++)
    {
        if (!initialized[i])
        {
            if (diagnose)
            {
                warn_or_error_printf_at(
                        locus,
                        emit_error,
                        "constructor cannot be constexpr because entity '%s' is not initialized\n",
                        get_qualified_symbol_name(all_members[i], all_members[i]->decl_context));
            }
            return 0;
        }
        else if (!is_constexpr_initialized[i])
        {
            if (diagnose)
            {
                warn_or_error_printf_at(
                        locus,
                        emit_error,
                        "constructor cannot be constexpr because entity '%s' is not "
                        "initialized using a constexpr constructor\n",
                        get_qualified_symbol_name(all_members[i], all_members[i]->decl_context));
            }
            return 0;
        }
    }

    return 1;
}

static char check_constexpr_function_body(scope_entry_t* entry, nodecl_t nodecl_body,
        char diagnose, char emit_error)
{
    if (nodecl_get_kind(nodecl_body) != NODECL_COMPOUND_STATEMENT)
    {
        if (diagnose)
        {
            warn_or_error_printf_at(
                    nodecl_get_locus(nodecl_body),
                    emit_error,
                    "the body of a constexpr function or constructor must be a compound-statement\n");
        }
        return 0;
    }

    nodecl_t statement_list = nodecl_get_child(nodecl_body, 0);

    int num_seen_other_statements = 0,
        num_seen_returns = 0,
        num_asm_definitions = 0,
        num_try_blocks = 0,
        num_invalid_initializations = 0;
    check_constexpr_function_statement_list(statement_list,
            &num_seen_returns,
            &num_seen_other_statements,
            &num_asm_definitions,
            &num_try_blocks,
            &num_invalid_initializations);

    if (IS_CXX14_LANGUAGE)
    {
        if (!symbol_entity_specs_get_is_constructor(entry))
        {
            if (num_seen_returns == 0)
            {
                if (diagnose)
                {
                    warn_or_error_printf_at(
                            nodecl_get_locus(nodecl_body),
                            emit_error,
                            "the body of a constexpr function should contain at least one return-statement\n");
                }
                return 0;
            }
        }
        if (num_asm_definitions > 0)
        {
            if (diagnose)
            {
                warn_or_error_printf_at(
                        nodecl_get_locus(nodecl_body),
                        emit_error,
                        "the body of a constexpr function cannot contain asm-blocks\n");
            }
            return 0;
        }
        if (num_try_blocks > 0)
        {
            if (diagnose)
            {
                warn_or_error_printf_at(
                            nodecl_get_locus(nodecl_body),
                        emit_error,
                        "the body of a constexpr function cannot contain try-blocks\n");
            }
            return 0;
        }
        if (num_invalid_initializations > 0)
        {
            if (diagnose)
            {
                warn_or_error_printf_at(
                            nodecl_get_locus(nodecl_body),
                        emit_error,
                        "the body of a constexpr function cannot contain a "
                        "non-initialized variable, thread_local, static or of non-literal type\n");
            }
            return 0;
        }
    }
    else if (IS_CXX11_LANGUAGE)
    {
        if (!symbol_entity_specs_get_is_constructor(entry))
        {
            if (num_seen_other_statements != 0
                    || num_seen_returns != 1)
            {
                if (diagnose)
                {
                    warn_or_error_printf_at(
                            nodecl_get_locus(nodecl_body),
                            emit_error,
                            "the body of a constexpr function must contain a single return-statement\n");
                }
                return 0;
            }
        }
        else
        {
            if (num_seen_other_statements != 0
                    || num_seen_returns != 0)
            {
                if (diagnose)
                {
                    warn_or_error_printf_at(
                            nodecl_get_locus(nodecl_body),
                            emit_error,
                            "the body of a constexpr construction must be empty\n");
                }
                return 0;
            }
        }
    }
    else
    {
        internal_error("Code unreachable", 0);
    }

    return 1;
}

char check_constexpr_function_code(scope_entry_t* entry, nodecl_t nodecl_function_code,
        char diagnose,
        char emit_error)
{
    nodecl_t nodecl_context = nodecl_get_child(nodecl_function_code, 0);

    nodecl_t nodecl_list = nodecl_get_child(nodecl_context, 0);
    ERROR_CONDITION(nodecl_list_length(nodecl_list) != 1, "Invalid function code", 0);

    nodecl_t nodecl_body = nodecl_list_head(nodecl_list);

    return check_constexpr_function_body(entry, nodecl_body, diagnose, emit_error);
}

static scope_entry_t* build_scope_function_definition_declarator(
        AST function_definition,
        const decl_context_t* decl_context,
        char is_template,
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list,

        gather_decl_spec_t * gather_info,
        const decl_context_t** block_context
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

    if (ASTKind(function_header) == AST_AMBIGUITY)
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
        C_LANGUAGE()
        {
            // There is no decl specifier sequence at all
            if (decl_spec_seq == NULL
                    // Or its type is null
                    || ASTSon1(decl_spec_seq) == NULL)
            {
                if (decl_spec_seq == NULL)
                {
                    warn_printf_at(ast_get_locus(function_definition), "function definition does not have a decl-specifier, assuming 'int'\n");
                }
                else
                {
                    warn_printf_at(ast_get_locus(function_definition), "function definition does not have a type-specifier, assuming 'int'\n");
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

    const decl_context_t* new_decl_context = decl_context;

    // block-context will be updated for qualified-id to reflect the exact context
    build_scope_declarator_with_parameter_context(function_declarator, gather_info, type_info, &declarator_type,
            new_decl_context, block_context, nodecl_output);

    if (is_error_type(declarator_type))
    {
        error_printf_at(ast_get_locus(function_header),
                "discarding function definition due to errors in the declarator\n");
        return NULL;
    }

    entry = build_scope_declarator_name(function_declarator,
            type_info, declarator_type,
            gather_info, new_decl_context);

    if (entry == NULL)
    {
        if (!is_error_type(declarator_type))
        {
            error_printf_at(ast_get_locus(function_header), "function '%s' was not found in the current scope\n",
                    print_decl_type_str(declarator_type, new_decl_context,
                        prettyprint_in_buffer(get_declarator_name(function_declarator, new_decl_context))));
        }
        else
        {
            // If no type was synthesized at all use the declarator instead (less nice, though)
            error_printf_at(ast_get_locus(function_header), "function '%s' was not found in the current scope\n",
                    prettyprint_in_buffer(function_declarator));
        }
        return NULL;
    }

    keep_extra_attributes_in_symbol(entry, gather_info);

    // Propagate the __extension__ attribute to the symbol
    symbol_entity_specs_set_gcc_extension(entry, gcc_extension);

    if (declared_symbols != NULL)
    {
        *declared_symbols = entry_list_new(entry);
        P_LIST_ADD(gather_decl_spec_list->items, gather_decl_spec_list->num_items, *gather_info);
    }

    // Set the related entry
    (*block_context)->current_scope->related_entry = entry;

    if (decl_context->current_scope->kind == BLOCK_SCOPE)
    {
        if (symbol_entity_specs_get_is_extern(entry))
        {
            error_printf_at(ast_get_locus(function_header), "definition of a nested function already declared as an extern\n");
        }
        symbol_entity_specs_set_is_nested_function(entry, 1);
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
        error_printf_at(ast_get_locus(function_definition), "function '%s' already defined\n",
                funct_name);
        info_printf_at(entry->locus, "location of previous definition\n");
        return NULL;
    }

    symbol_entity_specs_set_is_constexpr(entry,
            symbol_entity_specs_get_is_constexpr(entry)
            || gather_info->is_constexpr);

    symbol_entity_specs_set_is_inline(entry,
            symbol_entity_specs_get_is_inline(entry)
            || gather_info->is_inline
            || gather_info->is_constexpr);

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
                || ASTKind(kr_parameter_list) == AST_KR_PARAMETER_LIST)
        {
            build_scope_kr_parameter_declaration(entry, kr_parameter_declaration, 
                    kr_parameter_list, *block_context, nodecl_output);
        }
    }

    return entry;
}

scope_entry_t* register_mercurium_pretty_print(scope_entry_t* entry, const decl_context_t* block_context)
{
    const char* pretty_function_str = UNIQUESTR_LITERAL("__PRETTY_FUNCTION__");
    const char* mercurium_pretty_function_str = UNIQUESTR_LITERAL("__MERCURIUM_PRETTY_FUNCTION__");

    const char* nice_name =
        print_decl_type_str(entry->type_information,
                entry->decl_context, get_qualified_symbol_name(entry, entry->decl_context));
    const_value_t* nice_name_value = const_value_make_string_null_ended(nice_name, strlen(nice_name));
    nodecl_t nice_name_tree = const_value_to_nodecl(nice_name_value);

    // Adjust type to include room for the final \0
    nodecl_set_type(nice_name_tree,
            get_array_type(
                get_char_type(),
                nodecl_make_integer_literal(get_signed_int_type(),
                    const_value_get_signed_int(strlen(nice_name) + 1),
                    make_locus("", 0, 0)),
                block_context));

    // __PRETTY_FUNCTION__ is very compiler specific, so we will sign in a
    // __MERCURIUM_PRETTY_FUNCTION__ and make __PRETTY_FUNCTION__ an alias
    // to it
    //
    // Sign in __MERCURIUM_PRETTY_FUNCTION__
    scope_entry_t* mercurium_pretty_function = new_symbol(block_context,
            block_context->current_scope,
            mercurium_pretty_function_str);
    mercurium_pretty_function->kind = SK_VARIABLE;
    mercurium_pretty_function->type_information =
        get_const_qualified_type(no_ref(nodecl_get_type(nice_name_tree)));
    mercurium_pretty_function->value = nice_name_tree;
    symbol_entity_specs_set_is_user_declared(mercurium_pretty_function, 1);
    symbol_entity_specs_set_is_static(mercurium_pretty_function, 1);
    mercurium_pretty_function->locus = entry->locus;

    // Register __PRETTY_FUNCTION__ as an alias to __MERCURIUM_PRETTY_FUNCTION__
    insert_alias(block_context->current_scope, mercurium_pretty_function, pretty_function_str);

    return mercurium_pretty_function;
}

static void emit_mercurium_pretty_function(nodecl_t body_nodecl, scope_entry_t* mercurium_pretty_function)
{
    ERROR_CONDITION(nodecl_get_kind(body_nodecl) != NODECL_COMPOUND_STATEMENT, "Invalid node", 0);

    // Emit __MERCURIUM_PRETTY_FUNCTION__ if needed, otherwise do not emit it
    if (mercurium_pretty_function != NULL
            && mercurium_pretty_function_has_been_used(mercurium_pretty_function, body_nodecl))
    {
        nodecl_t emit_mercurium_pretty_function_tree = nodecl_null();
        CXX_LANGUAGE()
        {
            emit_mercurium_pretty_function_tree = nodecl_append_to_list(
                    emit_mercurium_pretty_function_tree,
                    nodecl_make_cxx_def(
                        nodecl_null(),
                        mercurium_pretty_function,
                        mercurium_pretty_function->locus));
        }
        emit_mercurium_pretty_function_tree = nodecl_append_to_list(
                emit_mercurium_pretty_function_tree,
                nodecl_make_object_init(mercurium_pretty_function,
                    mercurium_pretty_function->locus));

        nodecl_t statements_list = nodecl_get_child(body_nodecl, 0);
        statements_list = nodecl_concat_lists(emit_mercurium_pretty_function_tree, statements_list);
        nodecl_set_child(body_nodecl, 0, statements_list);
    }
}

static nodecl_t generate_compound_statement_for_try_block(
        nodecl_t body_nodecl,
        scope_entry_t* entry)
{
    if (symbol_entity_specs_get_is_constructor(entry)
            || symbol_entity_specs_get_is_destructor(entry))
    {
        // Emit a rethrow inside every handler
        nodecl_t try_block = nodecl_list_head(body_nodecl);
        nodecl_t catch_handler_list = nodecl_get_child(try_block, 1);

        int n;
        nodecl_t* list = nodecl_unpack_list(catch_handler_list, &n);

        int i;
        for (i = 0; i < n; i++)
        {
            nodecl_t context_of_catch_handler = list[i];
            nodecl_t catch_handler = nodecl_list_head(nodecl_get_child(context_of_catch_handler, 0));
            nodecl_t context_of_compound = nodecl_list_head(nodecl_get_child(catch_handler, 1));
            nodecl_t compound_statement = nodecl_list_head(nodecl_get_child(context_of_compound, 0));

            nodecl_t statement_list = nodecl_get_child(compound_statement, 0);

            nodecl_t rethrow_stmt =
                nodecl_make_expression_statement(
                        nodecl_make_throw(nodecl_null(),
                            get_throw_expr_type(),
                            nodecl_get_locus(body_nodecl)),
                        nodecl_get_locus(body_nodecl));

            statement_list = nodecl_append_to_list(statement_list, rethrow_stmt);
            nodecl_set_child(compound_statement, 0, statement_list);
        }

        DELETE(list);
    }

    nodecl_t result =
        nodecl_make_compound_statement(body_nodecl,
                nodecl_null(),
                nodecl_get_locus(body_nodecl));

    return result;
}

static void build_scope_function_definition_body(
        AST function_definition,
        scope_entry_t* entry,
        const decl_context_t* block_context,
        gather_decl_spec_t* gather_info,
        nodecl_t *nodecl_output)
{
    // Function_body
    AST function_body = ASTSon2(function_definition);
    AST statement = ASTSon0(function_body);

    // Here we update the type of 'this'
    if (symbol_entity_specs_get_is_member(entry)
            && !symbol_entity_specs_get_is_static(entry))
    {
        update_symbol_this(entry, block_context);
    }

    CXX11_LANGUAGE()
    {
        if (function_type_get_ref_qualifier(entry->type_information) != REF_QUALIFIER_NONE
                && (!symbol_entity_specs_get_is_member(entry)
                    || symbol_entity_specs_get_is_static(entry)))
        {
            error_printf_at(ast_get_locus(function_definition), "only nonstatic member functions may have ref-qualifier\n");
        }
    }

    // Sign in __func__ (C99/C++11) and GCC's __FUNCTION__ and
    // __PRETTY_FUNCTION__
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

        const char *special__func__ = UNIQUESTR_LITERAL("__func__");
        const char *special__FUNCTION__ = UNIQUESTR_LITERAL("__FUNCTION__");
        const char* func_names[] =
        {
            special__func__,
            special__FUNCTION__,
        };

        unsigned int j;
        for (j = 0; j < STATIC_ARRAY_LENGTH(func_names); j++)
        {
            scope_entry_t* func_var = new_symbol(block_context, block_context->current_scope, func_names[j]);
            func_var->kind = SK_VARIABLE;
            func_var->type_information = no_ref(nodecl_get_type(nodecl_expr));
            func_var->value = nodecl_expr;
            symbol_entity_specs_set_is_builtin(func_var, 1);
        }

        const char* pretty_function_str = UNIQUESTR_LITERAL("__PRETTY_FUNCTION__");

        if (is_dependent_function(entry))
        {
            // Insert a dependent __PRETTY_FUNCTION__
            scope_entry_t* pretty_function = new_symbol(block_context,
                    block_context->current_scope,
                    pretty_function_str);
            pretty_function->kind = SK_VARIABLE;
            pretty_function->type_information = get_unknown_dependent_type();
            symbol_entity_specs_set_is_builtin(pretty_function, 1);
        }
        else
        {
            mercurium_pretty_function = register_mercurium_pretty_print(entry, block_context);
        }
    }

    nodecl_t nodecl_initializers = nodecl_null();
    CXX_LANGUAGE()
    {
        AST ctor_initializer = ASTSon1(function_definition);
        if (symbol_entity_specs_get_is_member(entry)
            && symbol_entity_specs_get_is_constructor(entry))
        {
            AST location = ctor_initializer;
            if (ctor_initializer == NULL)
                location = function_definition;
            build_scope_ctor_initializer(ctor_initializer,
                                         entry,
                                         block_context,
                                         ast_get_locus(location),
                                         &nodecl_initializers);
        }
        else
        {
            if (ctor_initializer != NULL)
            {
                error_printf_at(ast_get_locus(function_definition),
                                "member-initializer-lists are only valid in "
                                "constructors\n");
            }
        }
    }

    // FIXME - Think how to make this better maintained
    if (CURRENT_CONFIGURATION->enable_cuda
        && (gather_info->cuda.is_global || gather_info->cuda.is_device))
    {
        cuda_kernel_symbols_for_function_body(
            function_body, gather_info, entry->decl_context, block_context);
    }


    // Result symbol only if the function returns something
    if (function_type_get_return_type(entry->type_information) != NULL
            && !is_void_type(function_type_get_return_type(entry->type_information)))
    {
        scope_entry_t* result_sym = new_symbol(block_context,
                block_context->current_scope,
                ".result"); // This name is currently not user accessible
        result_sym->kind = SK_VARIABLE;
        symbol_entity_specs_set_is_result_var(result_sym, 1);
        result_sym->type_information = get_unqualified_type(function_type_get_return_type(entry->type_information));

        symbol_entity_specs_set_result_var(entry, result_sym);
    }

    linkage_push(NULL, /* is_braced */ 1);

    nodecl_t body_nodecl = nodecl_null();
    if (ASTKind(statement) == AST_COMPOUND_STATEMENT)
    {
        // We want to inherit the block context to this compound statement
        // so build_scope_statement cannot be used, because it would create
        // one for the compound statement
        AST list = ASTSon0(statement);
        if (list != NULL)
        {
            build_scope_statement_seq(list, block_context, &body_nodecl);
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
            DELETE(gather_info->vla_dimension_symbols);
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
    else if (ASTKind(statement) == AST_TRY_BLOCK)
    {
        build_scope_statement(statement, block_context, &body_nodecl);

        body_nodecl = generate_compound_statement_for_try_block(
                body_nodecl,
                entry);
    }
    else
    {
        internal_error("Unreachable code", 0);
    }

    linkage_pop();

    if (symbol_entity_specs_get_is_constexpr(entry))
    {
        if (symbol_entity_specs_get_is_member(entry)
                && symbol_entity_specs_get_is_constructor(entry))
        {
            check_constexpr_constructor(entry, nodecl_get_locus(body_nodecl), nodecl_initializers,
                    /* diagnose */ 1, /* emit_error */ 1);
        }
        else
        {
            check_constexpr_function(entry, nodecl_get_locus(body_nodecl), /* diagnose */ 1, /* emit_error */ 1);
        }
        check_constexpr_function_body(entry, body_nodecl, /* diagnose */ 1, /* emit_error */ 1);
    }

    if (is_dependent_function(entry))
    {
        symbol_entity_specs_set_is_instantiable(entry, 1);
        // The emission template is itself
        symbol_entity_specs_set_emission_template(entry, entry);
    }
    else if (type_is_derived_from_auto(function_type_get_return_type(entry->type_information))
            || is_decltype_auto_type(function_type_get_return_type(entry->type_information)))
    {
        scope_entry_t* result_var = symbol_entity_specs_get_result_var(entry);
        ERROR_CONDITION(result_var == NULL, "Missing symbol", 0);

        type_t* deduced_return_type = result_var->type_information;
        if (type_is_derived_from_auto(deduced_return_type)
                || is_decltype_auto_type(deduced_return_type))
        {
            deduced_return_type = get_void_type();
        }

        if (function_type_get_has_trailing_return(entry->type_information))
        {
            entry->type_information =
                function_type_replace_return_type_with_trailing_return(
                        entry->type_information,
                        deduced_return_type);
        }
        else
        {
            entry->type_information =
                function_type_replace_return_type(
                        entry->type_information,
                        deduced_return_type);
        }
    }

    emit_mercurium_pretty_function(body_nodecl, mercurium_pretty_function);

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
    symbol_entity_specs_set_function_code(entry, nodecl_function_def);
}


/*
 * This function builds symbol table information for a function definition
 *
 * If previous_symbol != NULL, the found symbol should match
 */
static scope_entry_t* build_scope_function_definition(
        AST function_definition,
        const decl_context_t* decl_context,
        char is_template,
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list)
{
    const decl_context_t* block_context;

    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));

    scope_entry_t* entry = build_scope_function_definition_declarator(
            function_definition,
            decl_context,
            is_template,
            is_explicit_specialization,
            nodecl_output,
            declared_symbols,
            gather_decl_spec_list,

            &gather_info,
            &block_context);

    if (entry == NULL)
        return NULL;

    build_scope_function_definition_body(
            function_definition,
            entry,
            block_context,
            &gather_info,
            nodecl_output);

    // This field may have been set during instantiation
    symbol_entity_specs_set_is_defined_inside_class_specifier(entry, 0);

    return entry;
}

static void build_scope_member_declaration(const decl_context_t* inner_decl_context,
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
    switch (ASTKind(a))
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
                        /* is_typename */ ASTKind(a) == AST_USING_DECLARATION_TYPENAME,
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
                nodecl_t nodecl_single_assert = nodecl_null();
                build_scope_static_assert(a, inner_decl_context, &nodecl_single_assert);

                if (!nodecl_is_null(nodecl_single_assert)
                        && nodecl_get_kind(nodecl_single_assert) == NODECL_CXX_STATIC_ASSERT)
                {
                    scope_entry_t* member_static_assert = new_symbol(
                            inner_decl_context,
                            inner_decl_context->current_scope,
                            ".static_assert");
                    member_static_assert->kind = SK_MEMBER_STATIC_ASSERT;
                    member_static_assert->value = nodecl_single_assert;
                    symbol_entity_specs_set_access(member_static_assert, current_access);
                    class_type_add_member(get_actual_class_type(class_info), member_static_assert,
                            inner_decl_context, /* is_definition */ 1);
                }
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
                solve_ambiguous_member_declaration(a, inner_decl_context);
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
                internal_error("Unsupported node '%s' (%s)\n", ast_print_node_type(ASTKind(a)),
                        ast_location(a));
                break;
            }
    }
}

/*
 * This function registers a member template declaration
 */
static void build_scope_member_template_declaration(const decl_context_t* decl_context, AST a, 
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
    decl_context_t* template_context;
    build_scope_template_header(ASTSon0(a), decl_context, &template_context, nodecl_output);

    AST templated_decl = ASTSon1(a);
    if (ASTKind(templated_decl) == AST_AMBIGUITY)
    {
        solve_ambiguous_declaration(templated_decl, template_context);
    }

    switch (ASTKind(templated_decl))
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
            internal_error("Unknown node type '%s' at %s\n", ast_print_node_type(ASTKind(templated_decl)), ast_location(templated_decl));
    }

}

static void build_scope_member_template_function_definition(const decl_context_t* decl_context,
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

static void build_scope_member_template_simple_declaration(const decl_context_t* decl_context,
        AST a, access_specifier_t current_access, type_t* class_info,
        char is_explicit_specialization,
        nodecl_t* nodecl_output)
{
    build_scope_member_simple_declaration(decl_context, a, current_access, class_info, 
            /* is_template */ 1, is_explicit_specialization,
            nodecl_output, /* declared_symbols */ NULL, /* gather_decl_spec_t */ NULL);
}

static void build_scope_member_template_alias_declaration(const decl_context_t* decl_context,
        AST a, access_specifier_t current_access, type_t* class_info,
        char is_explicit_specialization,
        nodecl_t* nodecl_output)
{
    build_scope_common_template_alias_declaration(a, decl_context, nodecl_output,
            /* is_member_declaration */ 1, class_info, current_access, is_explicit_specialization);
}

static void build_scope_default_or_delete_template_member_function_definition(
        const decl_context_t* decl_context, 
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
                    && equivalent_types(class_type,
                        get_unqualified_type(reference_type_get_referenced_type(first_parameter))))
                || equivalent_types(class_type, first_parameter))
        {
            return 1;
        }
    }
    return 0;
}

char function_is_move_assignment_operator(scope_entry_t* entry, type_t* class_type)
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
                && equivalent_types(class_type,
                    get_unqualified_type(reference_type_get_referenced_type(first_parameter))))
        {
            return 1;
        }
    }
    return 0;
}

static char function_is_copy_constructor_types(type_t* function_type, type_t* class_type)
{
    // The caller should have checked that this function can be called with one parameter
    // If the function is not to have default arguments, it should have checked the number
    // of parameters

    // It might be callable with one parameter because of A(...) 
    // [but note that A(const A&, ...) is a valid copy constructor]
    int num_types = function_type_get_num_parameters(function_type);
    if (function_type_get_has_ellipsis(function_type))
        num_types--;

    if (num_types > 0)
    {
        type_t* first_parameter = function_type_get_parameter_type_num(function_type, 0);
        // Check that its form is either
        //
        // A(const A&, X = x);
        // A(A&, X = x);

        if (is_lvalue_reference_type(first_parameter)
                && equivalent_types(class_type,
                    get_unqualified_type(reference_type_get_referenced_type(first_parameter))))
        {
            return 1;
        }
    }
    return 0;
}

char function_is_copy_constructor(scope_entry_t* entry, type_t* class_type)
{
    return (symbol_entity_specs_get_is_constructor(entry)
            && can_be_called_with_number_of_arguments(entry, 1)
            && function_is_copy_constructor_types(entry->type_information, class_type));
}

static char function_is_move_constructor_types(type_t* function_type, type_t* class_type)
{
    // The caller should have checked that this function can be called with one parameter
    // If the function is not to have default arguments, it should have checked the number
    // of parameters

    // It might be callable with one parameter because of A(...) 
    // [but note that A(const A&, ...) is a valid copy constructor]
    int num_types = function_type_get_num_parameters(function_type);
    if (function_type_get_has_ellipsis(function_type))
        num_types--;

    if (num_types > 0)
    {
        type_t* first_parameter = function_type_get_parameter_type_num(function_type, 0);
        // Check that its form is either
        //
        // A(const A&&, X = x);
        // A(A&&, X = x);

        if (is_rvalue_reference_type(first_parameter)
                && equivalent_types(class_type,
                    get_unqualified_type(reference_type_get_referenced_type(first_parameter))))
        {
            return 1;
        }
    }
    return 0;
}

char function_is_move_constructor(scope_entry_t* entry, type_t* class_type)
{
    return (symbol_entity_specs_get_is_constructor(entry)
            && can_be_called_with_number_of_arguments(entry, 1)
            && function_is_move_constructor_types(entry->type_information, class_type));
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

        ERROR_CONDITION(destructor == NULL, "Invalid class '%s' lacking destructor",
                get_qualified_symbol_name(base_class, base_class->decl_context));

        if (symbol_entity_specs_get_is_virtual(destructor))
            return 1;
    }

    return 0;
}

static void update_member_function_info(
        AST declarator_name,
        scope_entry_t* entry,
        type_t* class_type)
{
    // Update information in the class about this member function
    symbol_entity_specs_set_is_user_declared(entry, 1);
    switch (ASTKind(declarator_name))
    {
        case AST_SYMBOL :
        case AST_TEMPLATE_ID :
            {
                if (symbol_entity_specs_get_is_constructor(entry))
                {
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
                        symbol_entity_specs_set_is_conversor_constructor(entry, 1);
                    }

                    if (can_be_called_with_number_of_arguments(entry, 0))
                    {
                        symbol_entity_specs_set_is_default_constructor(entry, 1);
                        class_type_set_default_constructor(class_type, entry);
                    }

                    symbol_entity_specs_set_is_copy_constructor(entry,
                        function_is_copy_constructor(entry, class_type));

                    CXX11_LANGUAGE()
                    {
                        symbol_entity_specs_set_is_move_constructor(entry,
                                function_is_move_constructor(entry, class_type));
                    }
                }
                break;
            }
            // Special members
        case AST_DESTRUCTOR_TEMPLATE_ID : // FIXME - Template arguments are not checked
        case AST_DESTRUCTOR_ID :
            {
                // This is the destructor
                if (symbol_entity_specs_get_is_virtual(entry)
                        || is_virtual_destructor(class_type))
                {
                    symbol_entity_specs_set_is_virtual(entry, 1);
                }
                symbol_entity_specs_set_is_destructor(entry, 1);
                class_type_set_destructor(get_actual_class_type(class_type), entry);
                break;
            }
        case AST_OPERATOR_FUNCTION_ID :
        case AST_OPERATOR_FUNCTION_ID_TEMPLATE :
            {
                symbol_entity_specs_set_is_copy_assignment_operator(entry,
                    function_is_copy_assignment_operator(entry, class_type));

                CXX11_LANGUAGE()
                {
                    if (function_is_move_assignment_operator(entry, class_type))
                    {
                        symbol_entity_specs_set_is_move_assignment_operator(entry, 1);
                    }
                }

                // These are always static
                if (ASTKind(ASTSon0(declarator_name)) == AST_NEW_OPERATOR
                        || ASTKind(ASTSon0(declarator_name)) == AST_DELETE_OPERATOR)
                {
                    symbol_entity_specs_set_is_static(entry, 1);
                }
                break;
            }
        case AST_CONVERSION_FUNCTION_ID :
            {
                symbol_entity_specs_set_is_conversion(entry, 1);
                break;
            }
        case AST_QUALIFIED_ID :
            {
                internal_error("Unreachable code", 0);
                break;
            }
        default :
            {
                internal_error("Unknown node '%s'\n", ast_print_node_type(ASTKind(declarator_name)));
                break;
            }
    }
}

void hide_using_declarations(type_t* class_info, scope_entry_t* currently_declared)
{
    const decl_context_t* class_context = class_type_get_inner_context(class_info);

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
        remove_entry(class_context->current_scope, hidden);
    }
}

/*
 * This is a function definition inlined in a class
 */
static scope_entry_t* build_scope_member_function_definition(
        const decl_context_t* decl_context,
        AST function_definition,
        access_specifier_t current_access,
        type_t* class_info,
        char is_template,
        char is_explicit_specialization,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list)
{
    const decl_context_t* block_context;

    gather_decl_spec_t *gather_info = NEW0(gather_decl_spec_t);
    gather_info->inside_class_specifier = 1;

    scope_entry_t* entry = build_scope_function_definition_declarator(
            function_definition,
            decl_context,
            is_template,
            is_explicit_specialization,
            nodecl_output,
            declared_symbols,
            gather_decl_spec_list,

            gather_info,
            &block_context);

    if (entry == NULL)
        return NULL;

    AST function_header = ASTSon0(function_definition);
    AST function_declarator = ASTSon1(function_header);
    AST declarator_name = get_declarator_name(function_declarator, decl_context);

    // Propagate 'do_not_print' attribute to the current member
    entry->do_not_print = named_type_get_symbol(class_info)->do_not_print;

    symbol_entity_specs_set_access(entry, current_access);
    symbol_entity_specs_set_is_defined_inside_class_specifier(entry, 1);
    symbol_entity_specs_set_is_inline(entry, 1);
    symbol_entity_specs_set_access(entry, current_access);
    symbol_entity_specs_set_class_type(entry, class_info);

    if (gather_info->is_friend
            && is_template_specialized_type(entry->type_information)
            && !gather_info->is_template)
    {
        error_printf_at(ast_get_locus(declarator_name), "defining explicit specialization '%s' in friend declaration\n",
                prettyprint_in_buffer(declarator_name));
        return NULL;
    }

    update_member_function_info(declarator_name, entry, class_info);

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
        scope_entry_t* friend_function = entry;

        if (friend_function->kind != SK_DEPENDENT_FRIEND_FUNCTION)
        {
            friend_function = NEW0(scope_entry_t);
            friend_function->kind = SK_FRIEND_FUNCTION;
            friend_function->decl_context = decl_context;
            symbol_entity_specs_set_alias_to(friend_function, entry);
        }

        class_type_add_friend_symbol(class_info, friend_function);
    }
    else
    {
        // Otherwise, we add this symbol as a member of the class
        class_type_add_member(get_actual_class_type(class_info), entry, decl_context, /* is_definition */ 1);
    }

    build_scope_delayed_add_delayed_function_def(function_definition, entry, block_context, gather_info);

    return entry;
}

static void build_scope_default_or_delete_member_function_definition(
        const decl_context_t* decl_context,
        AST a,
        access_specifier_t current_access,
        type_t* class_info,
        char is_template,
        char is_explicit_specialization,
        nodecl_t* nodecl_output)
{
    CXX03_LANGUAGE()
    {
        warn_printf_at(ast_get_locus(a), "default/delete functions are a C++11 feature\n");
    }

    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));

    gather_info.inside_class_specifier = 1;
    gather_info.is_template = is_template;
    gather_info.is_explicit_specialization = is_explicit_specialization;

    AST function_header = ASTSon0(a);

    if (ASTKind(function_header) == AST_AMBIGUITY)
    {
        solve_ambiguous_function_header(function_header, decl_context);
    }

    AST decl_spec_seq = ASTSon0(function_header);
    AST declarator = ASTSon1(function_header);

    type_t* member_type = NULL;

    if (decl_spec_seq != NULL)
    {
        build_scope_decl_specifier_seq(decl_spec_seq, &gather_info,
                &member_type, decl_context, nodecl_output);
    }

    AST declarator_name = get_declarator_name(declarator, decl_context);

    type_t* declarator_type = NULL;

    compute_declarator_type(declarator, &gather_info,
            member_type, &declarator_type,
            decl_context, nodecl_output);
    scope_entry_t *entry =
        build_scope_declarator_name(declarator,
                member_type, declarator_type,
                &gather_info, decl_context);

    ERROR_CONDITION(entry == NULL, "Invalid entry computed", 0);

    symbol_entity_specs_set_access(entry, current_access);

    ERROR_CONDITION(entry->kind != SK_FUNCTION, "Invalid symbol for default/delete", 0);

    update_member_function_info(declarator_name, entry, class_info);

    switch (ASTKind(a))
    {
        case AST_DEFAULTED_FUNCTION_DEFINITION :
            {
                set_defaulted_inside_class_specifier(entry, decl_context, ast_get_locus(a));
                break;
            }
        case AST_DELETED_FUNCTION_DEFINITION :
            {
                symbol_entity_specs_set_is_defined_inside_class_specifier(entry, 1);
                set_deleted(entry, decl_context, ast_get_locus(a));
                break;
            }
        default:
            {
                internal_error("Code unreachable", 0);
            }
    }

    keep_extra_attributes_in_symbol(entry, &gather_info);

    // Propagate the __extension__ attribute to the symbol
    symbol_entity_specs_set_gcc_extension(entry, gcc_extension);

    // Add definition as a member
    class_type_add_member(get_actual_class_type(class_info), entry, decl_context, /* is_definition */ 1);
}

void build_scope_friend_declarator(const decl_context_t* decl_context, 
        gather_decl_spec_t *gather_info,
        type_t* class_type,
        type_t* member_type, 
        AST declarator)
{
    nodecl_t nodecl_output = nodecl_null();

    type_t* declarator_type = NULL;
    compute_declarator_type(declarator, gather_info, 
            member_type, &declarator_type, 
            decl_context, &nodecl_output);

    if (is_error_type(declarator_type))
        return;

    scope_entry_t *entry =
        build_scope_declarator_name(declarator,
                member_type, declarator_type,
                gather_info, decl_context);

    if (entry == NULL
            || (entry->kind != SK_FUNCTION
                && entry->kind != SK_DEPENDENT_FRIEND_FUNCTION
                && entry->kind != SK_DEPENDENT_ENTITY))
    {
        error_printf_at(ast_get_locus(declarator), "friend declaration '%s' does not name a function\n",
                prettyprint_in_buffer(declarator));
        return;
    }

    if (entry->kind == SK_DEPENDENT_ENTITY)
    {
        // Fix the dependent entity here to be a dependent friend
        entry->kind = SK_DEPENDENT_FRIEND_FUNCTION;
        internal_error("Not yet implemented", 0);
    }

    scope_entry_t* friend_function = entry;
    if (entry->kind != SK_DEPENDENT_FRIEND_FUNCTION)
    {
        friend_function = NEW0(scope_entry_t);
        friend_function->kind = SK_FRIEND_FUNCTION;
        friend_function->decl_context = decl_context;
        symbol_entity_specs_set_alias_to(friend_function, entry);
    }

    class_type_add_friend_symbol(class_type, friend_function);
}

static void gather_single_virt_specifier(AST item,
        gather_decl_spec_t* gather_info,
        const decl_context_t* decl_context UNUSED_PARAMETER)
{
    switch (ASTKind(item))
    {
        case AST_CLASS_VIRT_SPEC:
        case AST_MEMBER_VIRT_SPEC:
            {
                ERROR_CONDITION( (ASTText(item) == NULL), "Invalid node", 0);
                const char* spec = ASTText(item);

                if (IS_CXX03_LANGUAGE)
                {
                    warn_printf_at(ast_get_locus(item), "virt-specifiers are a C+11 feature\n");
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
                internal_error("Invalid node '%s'\n", ast_print_node_type(ASTKind(item)));
                break;
            }
    }
}

static void gather_virt_specifiers(AST a,
        gather_decl_spec_t* gather_info,
        const decl_context_t* decl_context UNUSED_PARAMETER)
{
    if (a == NULL)
        return;
    ERROR_CONDITION(ASTKind(a) != AST_NODE_LIST, "Invalid node", 0);

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
static void build_scope_member_simple_declaration(const decl_context_t* decl_context, AST a, 
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
        const decl_context_t* new_decl_context = decl_context;
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
                        && (ASTKind(type_specifier) == AST_CLASS_SPECIFIER // class A { } [x];
                            // class A; (no declarator)
                            || ((ASTKind(type_specifier) == AST_ELABORATED_TYPE_CLASS_SPEC)
                                && (member_init_declarator_list == NULL))
                            // enum E { } [x];
                            || ASTKind(type_specifier) == AST_ENUM_SPECIFIER
                            // enum E; (no declarator)
                            || ((ASTKind(type_specifier) == AST_ELABORATED_TYPE_ENUM_SPEC)
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

            gather_decl_spec_t current_gather_info;
            copy_gather_info(&current_gather_info, &gather_info);

            switch (ASTKind(declarator))
            {
                case AST_AMBIGUITY:
                    {
                        solve_ambiguous_init_declarator(declarator, decl_context, &current_gather_info);
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
                            error_printf_at(ast_get_locus(declarator), "a bit-field cannot be declared as friend\n");
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
                            bitfield_symbol = build_scope_declarator_name(identifier,
                                    member_type, declarator_type,
                                    &current_gather_info, decl_context);
                        }
                        else
                        {
                            // Invent some name to sign it up because we will
                            // need it when computing the size of a class
                            bitfield_symbol = new_symbol(decl_context, decl_context->current_scope, 
                                    get_unique_name());
                            bitfield_symbol->kind = SK_VARIABLE;
                            bitfield_symbol->type_information = declarator_type;
                            symbol_entity_specs_set_is_user_declared(bitfield_symbol, 1);
                            // Remember that is unnamed, this is relevant for the size
                            symbol_entity_specs_set_is_unnamed_bitfield(bitfield_symbol, 1);
                        }

                        symbol_entity_specs_set_access(bitfield_symbol, current_access);
                        symbol_entity_specs_set_is_member(bitfield_symbol, 1);
                        symbol_entity_specs_set_class_type(bitfield_symbol, class_info);
                        class_type_add_member(get_actual_class_type(class_type), bitfield_symbol, decl_context, /* is_definition */ 1);

                        if (current_gather_info.is_static)
                        {
                            error_printf_at(ast_get_locus(declarator), "a bitfield declaration cannot be static\n");
                            return;
                        }

                        AST expression = ASTSon1(declarator);
                        nodecl_t nodecl_bit_size = nodecl_null();
                        if (!check_expression_must_be_constant(expression, decl_context, &nodecl_bit_size))
                        {
                            error_printf_at(ast_get_locus(expression), "invalid bitfield size '%s'\n",
                                    prettyprint_in_buffer(expression));
                        }

                        nodecl_bit_size = nodecl_expression_make_rvalue(nodecl_bit_size, decl_context);

                        if (!nodecl_is_constant(nodecl_bit_size))
                        {
                            error_printf_at(ast_get_locus(expression), "bitfield size is not constant '%s'\n",
                                    prettyprint_in_buffer(expression));
                            nodecl_bit_size = const_value_to_nodecl(const_value_get_one( /* bytes */ 4, /* signed */ 1));
                        }

                        symbol_entity_specs_set_is_bitfield(bitfield_symbol, 1);
                        symbol_entity_specs_set_bitfield_size(bitfield_symbol, nodecl_bit_size);
                        bitfield_symbol->related_decl_context = decl_context;

                        bitfield_symbol->defined = 1;

                        if (declared_symbols != NULL)
                        {
                            *declared_symbols = entry_list_add(*declared_symbols, bitfield_symbol);
                            P_LIST_ADD(gather_decl_spec_list->items, gather_decl_spec_list->num_items, current_gather_info);
                        }

                        break;
                    }
                case AST_MEMBER_DECLARATOR :
                case AST_INIT_DECLARATOR : // may appear here because of templates
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

                        if (ASTKind(too_much_qualified_declarator_name) == AST_QUALIFIED_ID)
                        {
                            error_printf_at(ast_get_locus(too_much_qualified_declarator_name),
                                    "extra qualification of member declaration is not allowed: '%s'. "
                                    "Did you mean '%s'?\n",
                                    prettyprint_in_buffer(declarator),
                                    prettyprint_in_buffer(ASTSon2(too_much_qualified_declarator_name))
                                    );
                            return;
                        }

                        AST declarator_name = get_declarator_name(declarator, decl_context);
                        AST initializer = ASTSon1(declarator);

                        const decl_context_t* new_decl_context = decl_context;

                        type_t* declarator_type = NULL;

                        compute_declarator_type(ASTSon0(declarator), &current_gather_info,
                                member_type, &declarator_type,
                                new_decl_context, nodecl_output);
                        scope_entry_t *entry =
                            build_scope_declarator_name(ASTSon0(declarator),
                                    member_type, declarator_type,
                                    &current_gather_info, new_decl_context);

                        if (entry == NULL)
                            continue;

                        DEBUG_CODE()
                        {
                            fprintf(stderr, "BUILDSCOPE: Setting symbol '%s' as a member of class '%s'\n",
                                    entry->symbol_name, class_name);
                        }

                        // Propagate 'do_not_print' attribute to the current member
                        entry->do_not_print = named_type_get_symbol(class_info)->do_not_print;

                        symbol_entity_specs_set_is_member(entry, 1);
                        symbol_entity_specs_set_access(entry, current_access);
                        symbol_entity_specs_set_class_type(entry, class_info);

                        // Copy some extra attributes
                        symbol_entity_specs_set_is_override(entry, current_gather_info.is_override);
                        symbol_entity_specs_set_is_hides_member(entry, current_gather_info.is_hides_member);
                        symbol_entity_specs_set_is_final(entry, current_gather_info.is_final);

                        if (entry->kind == SK_FUNCTION)
                        {
                            update_member_function_info(declarator_name, entry, class_info);

                            // This function might be hiding using declarations, remove those
                            hide_using_declarations(class_type, entry);

                            CXX11_LANGUAGE()
                            {
                                if (function_type_get_ref_qualifier(entry->type_information) != REF_QUALIFIER_NONE
                                        && symbol_entity_specs_get_is_static(entry))
                                {
                                    error_printf_at(ast_get_locus(declarator_name), "only nonstatic member functions may have ref-qualifier\n");
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
                        class_type_add_member(get_actual_class_type(class_type), entry, decl_context, entry->defined);

                        if (!current_gather_info.is_static
                                && current_gather_info.is_auto_type)
                        {
                            error_printf_at(ast_get_locus(declarator_name), "nonstatic member declared as %s\n",
                                    current_gather_info.is_decltype_auto ? "decltype(auto)" : "auto");
                        }

                        if (initializer != NULL)
                        {
                            if (entry->kind == SK_VARIABLE)
                            {
                                if (!current_gather_info.is_static
                                        && IS_CXX03_LANGUAGE)
                                {
                                    warn_printf_at(ast_get_locus(initializer), "initialization of nonstatic data members is only valid in C++11\n");
                                }

                                if (current_gather_info.is_static)
                                {
                                    nodecl_t nodecl_expr = nodecl_null();
                                    check_initialization(initializer,
                                            entry->decl_context,
                                            entry,
                                            get_unqualified_type(entry->type_information),
                                            &nodecl_expr,
                                            current_gather_info.is_auto_type,
                                            current_gather_info.is_decltype_auto);
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
                                    // But some parts of the code check this tree, create a fake "parse later"
                                    entry->value = nodecl_make_cxx_parse_later(ast_get_locus(initializer));
                                }
                                symbol_entity_specs_set_is_defined_inside_class_specifier(entry, 1);
                            }

                            // Special initializer for functions
                            else if (entry->kind == SK_FUNCTION)
                            {
                                // Check that it is '= 0'
                                char wrong_initializer = 1;
                                if (symbol_entity_specs_get_is_virtual(entry))
                                {
                                    AST equal_initializer = initializer;
                                    if (ASTKind(equal_initializer) == AST_EQUAL_INITIALIZER)
                                    {
                                        AST octal_literal = ASTSon0(equal_initializer);
                                        if (ASTKind(octal_literal) == AST_OCTAL_LITERAL)
                                        {
                                            if (strcmp(ASTText(octal_literal), "0") == 0)
                                            {
                                                // It is pure and the initializer was fine
                                                symbol_entity_specs_set_is_pure(entry, 1);
                                                wrong_initializer = 0;
                                            }
                                        }
                                    }
                                }

                                if (wrong_initializer)
                                {
                                    error_printf_at(
                                            ast_get_locus(declarator),
                                            "function declaration '%s' has an invalid initializer '%s'"
                                            " or has not been declared as a virtual function\n",
                                            prettyprint_in_buffer(declarator),
                                            prettyprint_in_buffer(initializer));
                                    return;
                                }
                            }
                            else
                            {
                                error_printf_at(ast_get_locus(initializer), "no initializer allowed in current member declaration\n");
                                return;
                            }
                        }
                        if (declared_symbols != NULL)
                        {
                            *declared_symbols = entry_list_add(*declared_symbols, entry);
                            P_LIST_ADD(gather_decl_spec_list->items, gather_decl_spec_list->num_items, current_gather_info);
                        }

                        keep_extra_attributes_in_symbol(entry, &current_gather_info);

                        // Propagate the __extension__ attribute to the symbol
                        symbol_entity_specs_set_gcc_extension(entry, gcc_extension);

                        break;
                    }
                default :
                    {
                        internal_error("Unhandled node '%s' (%s)", ast_print_node_type(ASTKind(declarator)), ast_location(declarator));
                        break;
                    }
            }
        }
    }
    else
    {
        if (is_named_type(original_member_type)
                && is_class_type(original_member_type)
                && symbol_entity_specs_get_is_anonymous_union(named_type_get_symbol(original_member_type)))
        {
            scope_entry_t* named_type = named_type_get_symbol(original_member_type);

            // Anonymous unions are members even in C
            C_LANGUAGE()
            {
                symbol_entity_specs_set_is_member(named_type, 1);
                symbol_entity_specs_set_access(named_type, current_access);
                symbol_entity_specs_set_is_defined_inside_class_specifier(named_type, 1);
                symbol_entity_specs_set_class_type(named_type, class_info);
            }

            scope_entry_t* new_member = finish_anonymous_class(named_type, decl_context);
            new_member->type_information = original_member_type;

            // Add this member to the current class
            symbol_entity_specs_set_is_member(new_member, 1);
            symbol_entity_specs_set_access(new_member, current_access);
            symbol_entity_specs_set_class_type(new_member, class_info);

            class_type_add_member(class_type, new_member, decl_context, /* is_definition */ 1);
        }
        else if (gather_info.is_friend
                && (ASTKind(type_specifier) != AST_ELABORATED_TYPE_CLASS_SPEC))
        {
            if (!is_dependent_class_scope(decl_context))
            {
                build_scope_friend_class_declaration(
                        original_member_type,
                        /* declared_name */ NULL,
                        decl_context,
                        ast_get_locus(a));
            }
            else
            {
                register_dependent_friend_class(
                        named_type_get_symbol(class_info),
                        original_member_type,
                        /* declared_name */ NULL,
                        decl_context);
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

    ERROR_CONDITION((ASTKind(a) != AST_NODE_LIST), "This function expects a list", 0);

    AST list, iter;
    list = a;

    for_each_element(list, iter)
    {
        AST cv_qualifier = ASTSon1(iter);

        switch (ASTKind(cv_qualifier))
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
                internal_error("Unknown node type '%s'", ast_print_node_type(ASTKind(cv_qualifier)));
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
        const decl_context_t* decl_context,
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

        char keep_is_inside_pack_expansion = get_is_inside_pack_expansion();
        char this_is_a_pack = 0;
        if (get_declarator_id_pack(abstract_decl, decl_context) != NULL)
        {
            this_is_a_pack = 1;
            set_is_inside_pack_expansion(1);
        }

        build_scope_decl_specifier_seq(type_specifier_seq, &inner_gather_info, &type_info,
                decl_context, nodecl_output);

        if (is_error_type(type_info))
        {
                    set_is_inside_pack_expansion(
                            keep_is_inside_pack_expansion);
            continue;
        }

        type_t* declarator_type = type_info;
        compute_declarator_type(abstract_decl, &inner_gather_info, type_info, &declarator_type,
                decl_context, nodecl_output);

        if (is_error_type(declarator_type))
        {
            set_is_inside_pack_expansion(
                    keep_is_inside_pack_expansion);
            continue;
        }

        if (this_is_a_pack)
        {
            // If this parameter declaration explicitly introduces a pack,
            // make sure it has a pack type somewhere
            if (type_does_not_contain_any_template_parameter_pack(
                        type_info,
                        ast_get_locus(type_id)))
            {
                set_is_inside_pack_expansion(
                        keep_is_inside_pack_expansion);
                continue;
            }

            declarator_type = get_pack_type(declarator_type);
        }

        set_is_inside_pack_expansion(keep_is_inside_pack_expansion);

        P_LIST_ADD_ONCE(gather_info->exceptions, gather_info->num_exceptions, declarator_type);
    }
}

static void check_nodecl_noexcept_spec(nodecl_t nodecl_expr,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    if (nodecl_is_err_expr(nodecl_expr))
    {
        *nodecl_output = nodecl_expr;
        return;
    }

    nodecl_expr = nodecl_expression_make_rvalue(nodecl_expr, decl_context);

    if (!nodecl_is_constant(nodecl_expr)
            && !nodecl_expr_is_value_dependent(nodecl_expr)
            && !nodecl_expr_is_type_dependent(nodecl_expr))
    {
        error_printf_at(nodecl_get_locus(nodecl_expr), "noexcept must specify a constant expression\n");
        *nodecl_output = nodecl_make_err_expr(nodecl_get_locus(nodecl_expr));
    }

    if (nodecl_expr_is_type_dependent(nodecl_expr))
    {
        *nodecl_output = nodecl_expr;
        return;
    }

    check_contextual_conversion(
            nodecl_expr,
            get_bool_type(),
            decl_context,
            nodecl_output);
}

static void build_noexcept_spec(type_t* function_type UNUSED_PARAMETER,
        AST a, const decl_context_t* decl_context,
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
        nodecl_t nodecl_expr = nodecl_null();
        check_expression_must_be_constant(const_expr, decl_context, &nodecl_expr);
        check_nodecl_noexcept_spec(nodecl_expr, decl_context, nodecl_output);
    }
}

static void build_exception_spec(type_t* function_type UNUSED_PARAMETER, 
        AST a, gather_decl_spec_t *gather_info, 
        const decl_context_t* decl_context,
        const decl_context_t* prototype_context,
        nodecl_t* nodecl_output)
{
    // No exception specifier at all
    if (a == NULL)
    {
        gather_info->any_exception = 1;
        return;
    }

    if (ASTKind(a) == AST_EXCEPTION_SPECIFICATION)
    {
        build_dynamic_exception_spec(function_type, a, gather_info, decl_context, nodecl_output);
    }
    else if (ASTKind(a) == AST_NOEXCEPT_SPECIFICATION)
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
        internal_error("Unexpected tree '%s'\n", ast_print_node_type(ASTKind(a)));
    }
}

// Gives a name to an operator function name
// 'operator +'
const char* get_operator_function_name(AST declarator_id)
{
    ERROR_CONDITION((ASTKind(declarator_id) != AST_OPERATOR_FUNCTION_ID
                && ASTKind(declarator_id) != AST_OPERATOR_FUNCTION_ID_TEMPLATE), 
            "This node is not valid here '%s'", ast_print_node_type(ASTKind(declarator_id)));

    AST operator  = ASTSon0(declarator_id);

#define RETURN_UNIQUESTR_NAME(x) \
    { \
        static const char* c = NULL; \
        if (c != NULL) return c; \
        return (c = uniquestr(x)); \
    } 

    switch (ASTKind(operator))
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
            internal_error("Invalid node type '%s'\n", ast_print_node_type(ASTKind(declarator_id)));
    }

#undef RETURN_UNIQUESTR_NAME
}

// This function computes the name of a literal operator: operator "" NAME
const char* get_literal_operator_name(const char* symbol_name)
{
    return strappend(uniquestr(STR_LITERAL_OPERATOR), symbol_name);
}

typedef
struct call_to_destructor_data_tag
{
    nodecl_t* nodecl_output;
    scope_t* scope;
    const locus_t* locus;
    const decl_context_t* decl_context;
} call_to_destructor_data_t;

static void call_to_destructor(scope_entry_list_t* entry_list, void *data)
{
    call_to_destructor_data_t* destructor_data = (call_to_destructor_data_t*)data;

    scope_entry_t* entry = entry_list_head(entry_list);

    if (entry->kind == SK_VARIABLE
            && is_class_type(entry->type_information)
            && is_complete_type(entry->type_information)
            && !is_dependent_type(entry->type_information)
            && !symbol_entity_specs_get_is_static(entry)
            && !symbol_entity_specs_get_is_extern(entry))
    {
        class_type_complete_if_needed(named_type_get_symbol(entry->type_information), 
                entry->decl_context, destructor_data->locus);

        nodecl_t sym_ref = nodecl_make_symbol(entry, make_locus("", 0, 0));
        type_t* t = entry->type_information;
        if (!is_any_reference_type(t))
            t = get_lvalue_reference_type(t);
        nodecl_set_type(sym_ref, t);

        scope_entry_t* destructor = class_type_get_destructor(entry->type_information);

        if (!symbol_entity_specs_get_is_trivial(destructor))
        {
            // Only call non-trivial destructors
            nodecl_t nodecl_call_to_destructor = 
                nodecl_make_expression_statement(
                        cxx_nodecl_make_function_call(
                            nodecl_make_symbol(class_type_get_destructor(entry->type_information), destructor_data->locus),
                            /* called name */ nodecl_null(),
                            nodecl_make_list_1(sym_ref),
                            /* function_form */ nodecl_null(),
                            get_void_type(),
                            destructor_data->decl_context,
                            destructor_data->locus),
                        destructor_data->locus);

            *(destructor_data->nodecl_output) = nodecl_append_to_list(
                    *(destructor_data->nodecl_output), 
                    nodecl_call_to_destructor);
        }
    }

    entry_list_free(entry_list);
}

static void call_destructors_of_classes(const decl_context_t* block_context, 
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    call_to_destructor_data_t call_to_destructor_data = { 
        .nodecl_output = nodecl_output,
        .scope = block_context->current_scope,
        .decl_context = block_context,
        .locus = locus,
    };

    scope_for_each_entity(block_context->current_scope, &call_to_destructor_data, call_to_destructor);
}

/*
 * Building scope for statements
 */

typedef void (*stmt_scope_handler_t)(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output);
typedef 
struct stmt_scope_handler_map_tag
{
    stmt_scope_handler_t handler;
} stmt_scope_handler_map_t;

void build_scope_nodecl_compound_statement(
        nodecl_t nodecl_statement_list,
        const decl_context_t* decl_context,
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    nodecl_t nodecl_destructors = nodecl_null();
    CXX_LANGUAGE()
    {
        call_destructors_of_classes(decl_context, locus, &nodecl_destructors);
    }

    *nodecl_output =
        nodecl_make_list_1(
                nodecl_make_compound_statement(
                    nodecl_statement_list,
                    nodecl_destructors,
                    locus
                    )
                );
}

static void build_scope_compound_statement(AST a, 
        const decl_context_t* decl_context, 
        nodecl_t* nodecl_output)
{
    const decl_context_t* block_context = new_block_context(decl_context);

    nodecl_t nodecl_statement_list = nodecl_null();
    AST list = ASTSon0(a);
    if (list != NULL)
    {
        build_scope_statement_seq(list, block_context, &nodecl_statement_list);
    }

    build_scope_nodecl_compound_statement(
            nodecl_statement_list,
            block_context,
            ast_get_locus(a),
            nodecl_output);

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_context(
                *nodecl_output,
                block_context,
                ast_get_locus(a)
                )
            );
}

static void build_scope_implicit_compound_statement(AST list, 
        const decl_context_t* decl_context, 
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

static void build_scope_nodecl_condition(nodecl_t nodecl_condition,
        const decl_context_t* decl_context,
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    nodecl_t nodecl_expr = nodecl_null();
    type_t* orig_type = NULL;

    if (nodecl_is_null(nodecl_condition))
    {
        *nodecl_output = nodecl_null();
        return;
    }
    if (nodecl_is_err_expr(nodecl_condition))
    {
        *nodecl_output = nodecl_condition;
        return;
    }
    else if (nodecl_get_kind(nodecl_condition) == NODECL_OBJECT_INIT)
    {
        orig_type = lvalue_ref(
                nodecl_get_symbol(nodecl_condition)->type_information
                );
        nodecl_expr = nodecl_get_symbol(nodecl_condition)->value;
    }
    else
    {
        orig_type = nodecl_get_type(nodecl_condition);
        nodecl_expr = nodecl_condition;
    }

    C_LANGUAGE()
    {
        standard_conversion_t scs;
        if (!standard_conversion_between_types(&scs,
                    orig_type,
                    get_signed_int_type(),
                    locus))
        {
            error_printf_at(locus, "expression of type '%s' is not valid in this context\n",
                    print_type_str(orig_type, decl_context));
            *nodecl_output = nodecl_make_err_expr(locus);
            return;
        }

        if (!equivalent_types(orig_type, get_signed_int_type()))
        {
            nodecl_expr = cxx_nodecl_make_conversion_to_logical(
                    nodecl_expr,
                    get_signed_int_type(),
                    decl_context,
                    locus);
        }
    }

    CXX_LANGUAGE()
    {
        if (!nodecl_expr_is_type_dependent(nodecl_expr))
        {
            check_contextual_conversion(
                    nodecl_expr,
                    get_bool_type(),
                    decl_context,
                    &nodecl_expr);

            if (nodecl_is_err_expr(nodecl_expr))
                return;
        }
    }

    if (nodecl_get_kind(nodecl_condition) == NODECL_OBJECT_INIT)
    {
        nodecl_get_symbol(nodecl_condition)->value = nodecl_expr;
    }
    else
    {
        *nodecl_output = nodecl_expr;
    }
}

static void build_scope_nodecl_condition_for_switch(nodecl_t nodecl_condition,
        const decl_context_t* decl_context,
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    nodecl_t nodecl_expr = nodecl_null();
    type_t* orig_type = NULL;

    if (nodecl_is_null(nodecl_condition))
    {
        *nodecl_output = nodecl_null();
        return;
    }

    if (nodecl_is_err_expr(nodecl_condition))
    {
        *nodecl_output = nodecl_condition;
        return;
    }


    if (nodecl_get_kind(nodecl_condition) == NODECL_OBJECT_INIT)
    {
        orig_type = lvalue_ref(
                nodecl_get_symbol(nodecl_condition)->type_information
                );
        nodecl_expr = nodecl_get_symbol(nodecl_condition)->value;
    }
    else
    {
        orig_type = nodecl_get_type(nodecl_condition);
        nodecl_expr = nodecl_condition;
    }

    C_LANGUAGE()
    {
        standard_conversion_t scs;
        if (!standard_conversion_between_types(&scs,
                    orig_type,
                    get_signed_int_type(),
                    locus))
        {
            error_printf_at(locus, "expression of type '%s' is not valid in this context\n",
                    print_type_str(orig_type, decl_context));
            *nodecl_output = nodecl_make_err_expr(locus);
            return;
        }

        if (!equivalent_types(orig_type, get_signed_int_type()))
        {
            nodecl_expr = cxx_nodecl_make_conversion(
                    nodecl_expr,
                    get_signed_int_type(),
                    decl_context,
                    locus);
        }
    }

    CXX_LANGUAGE()
    {
        if (!nodecl_expr_is_type_dependent(nodecl_expr))
        {
            type_t* dest_type = NULL;
            if (is_scoped_enum_type(no_ref(orig_type)))
                dest_type = no_ref(orig_type);
            else
                dest_type = get_signed_int_type();

            check_contextual_conversion(
                    nodecl_expr,
                    dest_type,
                    decl_context,
                    &nodecl_expr);

            if (nodecl_is_err_expr(nodecl_expr))
                return;
        }
    }

    if (nodecl_get_kind(nodecl_condition) == NODECL_OBJECT_INIT)
    {
        nodecl_get_symbol(nodecl_condition)->value = nodecl_expr;
    }
    else
    {
        *nodecl_output = nodecl_expr;
    }
}

static void build_scope_condition(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{

    if (ASTKind(a) == AST_AMBIGUITY)
    {
        solve_ambiguous_condition(a, decl_context);
    }

    if (ASTSon0(a) != NULL
            && ASTSon1(a) != NULL)
    {
        // This condition declares something in this scope
        AST type_specifier_seq = ASTSon0(a);
        AST declarator = ASTSon1(a);

        ERROR_CONDITION((ASTKind(declarator) == AST_AMBIGUITY), "Unexpected ambiguity", 0);

        // A type_specifier_seq is essentially a subset of a
        // declarator_specifier_seq so we can reuse existing functions
        type_t* type_info = NULL;
        gather_decl_spec_t gather_info;
        memset(&gather_info, 0, sizeof(gather_info));

        build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &type_info,
                decl_context, nodecl_output);

        type_t* declarator_type = NULL;

        compute_declarator_type(declarator, &gather_info,
                type_info, &declarator_type,
                decl_context, nodecl_output);
        scope_entry_t* entry = build_scope_declarator_name(declarator,
                type_info, declarator_type,
                &gather_info, decl_context);

        // FIXME: Handle VLAs here
        ERROR_CONDITION(gather_info.num_vla_dimension_symbols > 0, "Unsupported VLAs at the declaration", 0);

        AST initializer = ASTSon2(a);

        nodecl_t nodecl_expr = nodecl_null();
        if (!check_initialization(initializer,
                    decl_context,
                    entry,
                    get_unqualified_type(entry->type_information),
                    &nodecl_expr,
                    gather_info.is_auto_type,
                    gather_info.is_decltype_auto))
        {
            *nodecl_output = nodecl_expr;
            return;
        }

        entry->value = nodecl_expr;

        *nodecl_output = nodecl_make_object_init(entry, ast_get_locus(initializer));

        keep_extra_attributes_in_symbol(entry, &gather_info);
    }
    else
    {
        check_expression(ASTSon2(a), decl_context, nodecl_output);
        // FIXME: Handle VLAs here
        ERROR_CONDITION (pop_extra_declaration_symbol() != NULL,
                "Unsupported extra declarations at the initialization expression", 0);
    }
}

static void build_scope_normalized_statement(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    if (ASTKind(a) == AST_COMPOUND_STATEMENT)
    {
        build_scope_statement(a, decl_context, nodecl_output);
    }
    else
    {
        // Mimick the behaviour of a compound statement
        const decl_context_t* block_context = new_block_context(decl_context);

        nodecl_t nodecl_output_list = nodecl_null();

        build_scope_statement(a, block_context, &nodecl_output_list);

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

static void build_scope_nodecl_while_statement(
        nodecl_t nodecl_condition,
        nodecl_t nodecl_statement,
        const decl_context_t* decl_context UNUSED_PARAMETER,
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    build_scope_nodecl_condition(
            nodecl_condition,
            decl_context,
            locus,
            &nodecl_condition);
    if (nodecl_is_err_expr(nodecl_condition))
    {
        *nodecl_output = nodecl_make_list_1(
                nodecl_make_err_statement(locus));
        return;
    }

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_while_statement(
                nodecl_condition,
                nodecl_statement,
                /* loop_name */ nodecl_null(),
                locus));
}

static void build_scope_while_statement(AST a, 
        const decl_context_t* decl_context, 
        nodecl_t* nodecl_output)
{
    const decl_context_t* block_context = new_block_context(decl_context);

    nodecl_t nodecl_condition = nodecl_null();
    build_scope_condition(ASTSon0(a), block_context, &nodecl_condition);

    nodecl_t nodecl_statement = nodecl_null();
    if (ASTSon1(a) != NULL)
    {
        build_scope_normalized_statement(ASTSon1(a), block_context, &nodecl_statement);
    }

    build_scope_nodecl_while_statement(
            nodecl_condition,
            nodecl_statement,
            block_context,
            ast_get_locus(a),
            nodecl_output);

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_context(
                *nodecl_output,
                block_context,
                ast_get_locus(a)));
}

static void build_scope_ambiguity_handler(AST a, 
        const decl_context_t* decl_context, 
        nodecl_t* nodecl_output)
{
    solve_ambiguous_statement(a, decl_context);
    nodecl_t n = flush_extra_declared_symbols(ast_get_locus(a));
    nodecl_free(n);

    // Restart
    build_scope_statement(a, decl_context, nodecl_output);
}

static void build_scope_declaration_statement(AST a, 
        const decl_context_t* decl_context, 
        nodecl_t* nodecl_output)
{
    AST declaration = ASTSon0(a);

    build_scope_declaration(declaration, decl_context, nodecl_output, 
            /* declared_symbols */ NULL, /* gather_decl_spec_t */ NULL);
}

static void build_scope_nodecl_expression_statement(nodecl_t nodecl_expr,
        const decl_context_t* decl_context,
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    if (!nodecl_is_err_expr(nodecl_expr))
    {
        if (is_unresolved_overloaded_type(nodecl_get_type(nodecl_expr)))
        {
            const char* message = NULL;
            uniquestr_sprintf(&message,
                    "invalid unresolved overloaded expression '%s'\n",
                    codegen_to_str(nodecl_expr, decl_context));
            scope_entry_list_t* candidates = unresolved_overloaded_type_get_overload_set(nodecl_get_type(nodecl_expr));

            diagnostic_candidates(candidates, &message, locus);
            error_printf_at(locus, "%s", message);
        }
    }

    nodecl_t nodecl_expr_stmt = nodecl_make_expression_statement(
            nodecl_expr, locus);

    *nodecl_output = flush_extra_declared_symbols(locus);
    *nodecl_output = nodecl_append_to_list(
            *nodecl_output,
            nodecl_expr_stmt);
}

static void build_scope_expression_statement(AST a,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    AST expr = ASTSon0(a);
    nodecl_t nodecl_expr = nodecl_null();
    check_expression(expr, decl_context, &nodecl_expr);

    if (CURRENT_CONFIGURATION->strict_typecheck
            && nodecl_is_err_expr(nodecl_expr))
    {
        internal_error("Could not check expression '%s' at '%s'\n",
                prettyprint_in_buffer(ASTSon0(a)),
                ast_location(ASTSon0(a)));
    }

    build_scope_nodecl_expression_statement(nodecl_expr,
            decl_context,
            ast_get_locus(a),
            nodecl_output);
}

static void build_scope_nodecl_if_else_statement(
        nodecl_t nodecl_condition,
        nodecl_t nodecl_then_statement,
        nodecl_t nodecl_else_statement,
        const decl_context_t* decl_context UNUSED_PARAMETER,
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    build_scope_nodecl_condition(
            nodecl_condition,
            decl_context,
            locus,
            &nodecl_condition);
    if (nodecl_is_err_expr(nodecl_condition))
    {
        *nodecl_output = nodecl_make_list_1(
                nodecl_make_err_statement(locus));
        return;
    }

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_if_else_statement(
                nodecl_condition,
                nodecl_then_statement,
                nodecl_else_statement,
                locus)
            );
}

static void build_scope_if_else_statement(AST a,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    const decl_context_t* block_context = new_block_context(decl_context);

    AST condition = ASTSon0(a);
    nodecl_t nodecl_condition = nodecl_null();
    build_scope_condition(condition, block_context, &nodecl_condition);

    AST then_branch = ASTSon1(a);
    nodecl_t nodecl_then = nodecl_null();
    build_scope_normalized_statement(then_branch, block_context, &nodecl_then);

    nodecl_t nodecl_else = nodecl_null();
    AST else_branch = ASTSon2(a);
    if (else_branch != NULL)
    {
        build_scope_normalized_statement(else_branch, block_context, &nodecl_else);
    }

    build_scope_nodecl_if_else_statement(
            nodecl_condition,
            nodecl_then,
            nodecl_else,
            block_context,
            ast_get_locus(a),
            nodecl_output);

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_context(
                *nodecl_output,
                block_context,
                ast_get_locus(a)
                )
            );
}

static void solve_literal_symbol_scope(AST a, const decl_context_t* decl_context UNUSED_PARAMETER,
        nodecl_t* nodecl_output)
{
    ERROR_CONDITION(ASTKind(a) != AST_SYMBOL_LITERAL_REF, "Invalid node", 0);

    const char *tmp = ASTText(ASTSon0(a));

    const char * prefix = NULL;
    void *p = NULL;
    unpack_pointer(tmp, &prefix, &p);

    ERROR_CONDITION(prefix == NULL || p == NULL || strcmp(prefix, "symbol") != 0,
            "Failure during unpack of symbol", 0);

    scope_entry_t* entry = (scope_entry_t*)p;

    *nodecl_output = nodecl_make_symbol(entry, ast_get_locus(a));
}

static void build_scope_nodecl_for_statement_nonrange(
        nodecl_t nodecl_loop_init,
        nodecl_t nodecl_loop_condition,
        nodecl_t nodecl_loop_iter,
        nodecl_t nodecl_loop_name,
        nodecl_t nodecl_statement,
        const decl_context_t* decl_context UNUSED_PARAMETER,
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    build_scope_nodecl_condition(
            nodecl_loop_condition,
            decl_context,
            locus,
            &nodecl_loop_condition);
    if (!nodecl_is_null(nodecl_loop_condition)
            && nodecl_is_err_expr(nodecl_loop_condition))
    {
        *nodecl_output = nodecl_make_list_1(
                nodecl_make_err_statement(locus));
        return;
    }

    nodecl_t nodecl_loop_control =
        nodecl_make_loop_control(
                nodecl_loop_init,
                nodecl_loop_condition,
                nodecl_loop_iter,
                locus);

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_for_statement(
                nodecl_loop_control,
                nodecl_statement,
                nodecl_loop_name,
                locus)
            );
}

static void build_scope_for_statement_nonrange(AST a,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    AST loop_control = ASTSon0(a);

    AST for_init_statement = ASTSon0(loop_control);
    AST condition = ASTSon1(loop_control);
    AST expression = ASTSon2(loop_control);

    AST statement = ASTSon1(a);

    // AST end_loop_statement = ASTSon2(a); // Fortran only
    AST synthesized_loop_name = ASTSon3(a); // Mercurium internal parsing

    if (ASTKind(for_init_statement) == AST_AMBIGUITY)
    {
        solve_ambiguous_for_init_statement(for_init_statement, decl_context);
    }

    const decl_context_t* block_context = new_block_context(decl_context);

    nodecl_t nodecl_loop_init = nodecl_null();
    if (ASTKind(for_init_statement) == AST_SIMPLE_DECLARATION)
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

        DELETE(gather_decl_spec_list.items);
    }
    else if (ASTKind(for_init_statement) == AST_EXPRESSION_STATEMENT)
    {
        build_scope_expression_statement(for_init_statement, block_context, &nodecl_loop_init);
        // This tree contains VLA object inits
        ERROR_CONDITION(!nodecl_is_null(nodecl_loop_init)
                && (nodecl_list_length(nodecl_loop_init) > 1), "Unsupported VLAs at this expression statement", 0);

        nodecl_loop_init = nodecl_list_head(nodecl_loop_init);
        // Get the expression itself instead of an expression statement
        nodecl_loop_init = nodecl_make_list_1(nodecl_get_child(nodecl_loop_init, 0));
    }
    else if (ASTKind(for_init_statement) == AST_EMPTY_STATEMENT)
    {
        build_scope_statement(for_init_statement, block_context, &nodecl_loop_init);
        // Make it empty
        nodecl_loop_init = nodecl_null();
    }
    else
    {
        internal_error("unexpected node '%s'", ast_print_node_type(ASTKind(for_init_statement)));
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

    nodecl_t nodecl_loop_name = nodecl_null();
    if (synthesized_loop_name != NULL)
    {
        solve_literal_symbol_scope(synthesized_loop_name, decl_context, &nodecl_loop_name);
    }

    build_scope_nodecl_for_statement_nonrange(
            nodecl_loop_init,
            nodecl_loop_condition,
            nodecl_loop_iter,
            nodecl_loop_name,
            nodecl_statement,
            block_context,
            ast_get_locus(a),
            nodecl_output);

    *nodecl_output =
        nodecl_make_list_1(
                nodecl_make_context(
                    *nodecl_output,
                    block_context,
                    ast_get_locus(a))
                );

}

char class_lookup_begin_or_end(const decl_context_t* decl_context,
        scope_entry_t* class_symbol,
        const locus_t* locus)
{
    nodecl_t begin_name = nodecl_make_cxx_dep_name_simple(
            UNIQUESTR_LITERAL("begin"),
            locus);
    nodecl_t end_name = nodecl_make_cxx_dep_name_simple(
            UNIQUESTR_LITERAL("end"),
            locus);

    scope_entry_list_t* begin_query = query_nodecl_name_in_class(
            decl_context,
            class_symbol,
            begin_name,
            NULL);
    scope_entry_list_t* end_query = query_nodecl_name_in_class(
            decl_context,
            class_symbol,
            end_name,
            NULL);

    char result = (begin_query != NULL) || (end_query != NULL);

    entry_list_free(begin_query);
    entry_list_free(end_query);

    nodecl_free(begin_name);
    nodecl_free(end_name);

    return result;
}

static void build_scope_nodecl_for_statement_range_nondependent(
        scope_entry_t* iterator_symbol,
        scope_entry_t* range_symbol,
        nodecl_t nodecl_range_initializer,
        nodecl_t nodecl_statement,
        const decl_context_t* decl_context,
        const decl_context_t* block_context,
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    nodecl_t nodecl_initializer_tmp = nodecl_null();

    // Craft begin_expr and end_expr
    nodecl_t nodecl_begin_init = nodecl_null(),
             nodecl_end_init = nodecl_null();

    if (is_array_type(no_ref(range_symbol->type_information)))
    {
        nodecl_t nodecl_begin_symbol = nodecl_make_symbol(range_symbol, locus);
        nodecl_set_type(nodecl_begin_symbol, lvalue_ref(range_symbol->type_information));

        nodecl_begin_init = nodecl_make_cxx_equal_initializer(
                nodecl_make_cxx_initializer(
                    nodecl_begin_symbol,
                    nodecl_get_type(nodecl_begin_symbol),
                    locus),
                nodecl_get_type(nodecl_begin_symbol),
                locus);

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
                                locus),
                            nodecl_array_size,
                            pointer_type,
                            locus),
                        pointer_type,
                        locus),
                    pointer_type,
                    locus);
    }
    else
    {
        if (is_class_type(no_ref(range_symbol->type_information))
                && class_lookup_begin_or_end(
                    decl_context,
                    named_type_get_symbol(
                        advance_over_typedefs(
                            no_ref(range_symbol->type_information))),
                    locus))
        {
            AST begin_init_tree = ASTMake2(AST_FUNCTION_CALL,
                    ASTMake2(AST_CLASS_MEMBER_ACCESS,
                        ASTLeaf(AST_SYMBOL, locus, UNIQUESTR_LITERAL(".__range")),
                        ASTLeaf(AST_SYMBOL, locus, UNIQUESTR_LITERAL("begin")),
                        locus,
                        NULL),
                    NULL,
                    locus,
                    NULL);

            AST end_init_tree = ASTMake2(AST_FUNCTION_CALL,
                    ASTMake2(AST_CLASS_MEMBER_ACCESS,
                        ASTLeaf(AST_SYMBOL, locus, UNIQUESTR_LITERAL(".__range")),
                        ASTLeaf(AST_SYMBOL, locus, UNIQUESTR_LITERAL("end")),
                        locus,
                        NULL),
                    NULL,
                    locus,
                    NULL);

            check_expression(begin_init_tree, block_context, &nodecl_begin_init);
            if (nodecl_is_err_expr(nodecl_begin_init))
            {
                *nodecl_output = nodecl_make_list_1(
                        nodecl_make_err_statement(locus)
                        );
                return;
            }

            check_expression(end_init_tree, block_context, &nodecl_end_init);
            if (nodecl_is_err_expr(nodecl_end_init))
            {
                *nodecl_output = nodecl_make_list_1(
                        nodecl_make_err_statement(locus)
                        );
                return;
            }
        }
        else
        {
            scope_entry_list_t* begin_lookup = NULL;
            scope_entry_list_t* end_lookup = NULL;
            if (is_class_type(no_ref(range_symbol->type_information)))
            {
                nodecl_t begin_name = nodecl_make_cxx_dep_name_simple(
                        UNIQUESTR_LITERAL("begin"),
                        locus);
                nodecl_t end_name = nodecl_make_cxx_dep_name_simple(
                        UNIQUESTR_LITERAL("end"),
                        locus);

                type_t* t  = no_ref(range_symbol->type_information);
                begin_lookup = koenig_lookup(1, &t,
                        block_context,
                        begin_name,
                        locus);
                end_lookup = koenig_lookup(1, &t,
                        block_context,
                        end_name,
                        locus);

                nodecl_free(begin_name);
                nodecl_free(end_name);
            }

            if (begin_lookup == NULL)
            {
                error_printf_at(locus, "invalid type '%s' in range-based for-statement, no suitable 'begin' found\n",
                        print_declarator(range_symbol->type_information));
                *nodecl_output = nodecl_make_list_1(
                        nodecl_make_err_statement(locus)
                        );
                return;
            }

            if (end_lookup == NULL)
            {
                error_printf_at(locus, "invalid type '%s' in range-based for-statement, no suitable 'end' found\n",
                        print_declarator(range_symbol->type_information));
                *nodecl_output = nodecl_make_list_1(
                        nodecl_make_err_statement(locus)
                        );
                return;
            }

            nodecl_t begin_set = nodecl_make_symbol(entry_list_head(begin_lookup), locus);
            nodecl_set_type(begin_set, get_unresolved_overloaded_type(begin_lookup, NULL));

            nodecl_t nodecl_range_sym = nodecl_make_symbol(range_symbol, locus);
            nodecl_set_type(nodecl_range_sym, lvalue_ref(range_symbol->type_information));

            check_nodecl_function_call(begin_set, 
                    nodecl_make_list_1(nodecl_range_sym),
                    decl_context,
                    &nodecl_begin_init);

            entry_list_free(begin_lookup);

            if (nodecl_is_err_expr(nodecl_begin_init))
            {
                *nodecl_output = nodecl_make_list_1(
                        nodecl_make_err_statement(locus)
                        );
                return;
            }

            nodecl_t end_set = nodecl_make_symbol(entry_list_head(end_lookup), locus);
            nodecl_set_type(end_set, get_unresolved_overloaded_type(end_lookup, NULL));

            nodecl_range_sym = nodecl_make_symbol(range_symbol, locus);
            nodecl_set_type(nodecl_range_sym, lvalue_ref(range_symbol->type_information));

            check_nodecl_function_call(end_set, 
                    nodecl_make_list_1(nodecl_range_sym),
                    decl_context,
                    &nodecl_end_init);

            entry_list_free(end_lookup);

            if (nodecl_is_err_expr(nodecl_end_init))
            {
                *nodecl_output = nodecl_make_list_1(
                        nodecl_make_err_statement(locus)
                        );
                return;
            }
        }

        nodecl_begin_init = nodecl_make_cxx_equal_initializer(
                nodecl_make_cxx_initializer(
                    nodecl_begin_init,
                    nodecl_get_type(nodecl_begin_init),
                    locus),
                nodecl_get_type(nodecl_begin_init),
                locus);

        nodecl_end_init = nodecl_make_cxx_equal_initializer(
                nodecl_make_cxx_initializer(
                    nodecl_end_init,
                    nodecl_get_type(nodecl_end_init),
                    locus),
                nodecl_get_type(nodecl_end_init),
                locus);
    }

    // Create __begin and __end
    scope_entry_t* begin_symbol = new_symbol(block_context, block_context->current_scope, ".__begin");
    begin_symbol->symbol_name = UNIQUESTR_LITERAL("__begin");
    begin_symbol->kind = SK_VARIABLE;
    begin_symbol->type_information = get_auto_type();
    begin_symbol->locus = locus;

    nodecl_initializer_tmp = nodecl_null();
    check_nodecl_initialization(
            nodecl_begin_init,
            block_context,
            begin_symbol,
            begin_symbol->type_information,
            &nodecl_initializer_tmp,
            /* is_auto_type */ 1,
            /* is_decltype_auto */ 0);

    if (nodecl_is_err_expr(nodecl_initializer_tmp))
    {
        *nodecl_output = nodecl_make_list_1(
                nodecl_make_err_statement(locus)
                );
        return;
    }

    scope_entry_t* end_symbol = new_symbol(block_context, block_context->current_scope, ".__end");
    end_symbol->symbol_name = UNIQUESTR_LITERAL("__end");
    end_symbol->kind = SK_VARIABLE;
    end_symbol->type_information = get_auto_type();
    end_symbol->locus = locus;

    nodecl_initializer_tmp = nodecl_null();
    check_nodecl_initialization(nodecl_end_init,
            block_context,
            end_symbol,
            end_symbol->type_information,
            &nodecl_initializer_tmp,
            /* is_auto_type */ 1,
            /* is_decltype_auto */ 0);

    if (nodecl_is_err_expr(nodecl_initializer_tmp))
    {
        *nodecl_output = nodecl_make_list_1(
                nodecl_make_err_statement(locus)
                );
        return;
    }

    AST initialize_iterator =
        ASTMake1(AST_EQUAL_INITIALIZER,
                ASTMake1(AST_DERREFERENCE,
                    ASTLeaf(AST_SYMBOL, locus, UNIQUESTR_LITERAL(".__begin")),
                    locus, NULL),
                locus, NULL);

    bool is_auto_type = type_is_derived_from_auto(iterator_symbol->type_information);
    bool is_decltype_auto = is_decltype_auto_type(iterator_symbol->type_information);

    nodecl_initializer_tmp = nodecl_null();
    check_initialization(initialize_iterator,
            block_context,
            iterator_symbol,
            iterator_symbol->type_information,
            &nodecl_initializer_tmp,
            is_auto_type,
            is_decltype_auto);

    if (nodecl_is_err_expr(nodecl_initializer_tmp))
    {
        *nodecl_output = nodecl_make_list_1(
                nodecl_make_err_statement(locus)
                );
        return;
    }

    nodecl_t nodecl_loop_control = nodecl_make_iterator_loop_control(
            nodecl_make_symbol(iterator_symbol, locus),
            nodecl_range_initializer,
            locus);

    *nodecl_output =
        nodecl_make_list_1(
                nodecl_make_for_statement(
                    nodecl_loop_control,
                    nodecl_statement,
                    /* loop_name */ nodecl_null(),
                    locus)
                );

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_context(
                *nodecl_output,
                block_context,
                locus
                )
            );
}

static void build_scope_nodecl_for_statement_range(
        scope_entry_t* iterator_symbol,
        nodecl_t nodecl_range_initializer,
        nodecl_t nodecl_statement,
        const decl_context_t* decl_context,
        const decl_context_t* block_context,
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    // The iterator type is dependent
    if (is_dependent_type(iterator_symbol->type_information)
            || nodecl_expr_is_type_dependent(nodecl_range_initializer))
    {

        *nodecl_output = nodecl_make_list_1(
                nodecl_make_cxx_for_ranged(
                    nodecl_range_initializer,
                    nodecl_statement,
                    iterator_symbol,
                    locus)
                );
        return;
    }

    // Create an inaccessible .__range
    scope_entry_t* range_symbol = new_symbol(block_context, block_context->current_scope, ".__range");
    // But rename it to __range for nice diagnostics
    range_symbol->symbol_name = UNIQUESTR_LITERAL("__range");
    range_symbol->kind = SK_VARIABLE;
    range_symbol->type_information = get_rvalue_reference_type(get_auto_type());
    range_symbol->locus = locus;

    nodecl_t nodecl_orig_range_initializer = nodecl_range_initializer;
    check_nodecl_initialization(
            nodecl_shallow_copy(nodecl_orig_range_initializer),
            block_context,
            range_symbol,
            range_symbol->type_information,
            &nodecl_range_initializer,
            /* is_auto_type */ 1,
            /* is_decltype_auto */ 0);

    if (nodecl_is_err_expr(nodecl_range_initializer))
    {
        *nodecl_output = nodecl_make_list_1(
                nodecl_make_err_statement(locus)
                );
        return;
    }

    // The initializer expression is dependent
    // FIXME: can we deduce this fact earlier and not after computing a whole
    // initialization for .__range?
    if (is_dependent_type(range_symbol->type_information))
    {
        nodecl_free(nodecl_range_initializer);

        *nodecl_output = nodecl_make_list_1(
                nodecl_make_cxx_for_ranged(
                    nodecl_orig_range_initializer,
                    nodecl_statement,
                    iterator_symbol,
                    locus)
                );
        return;
    }
    else
    {
        nodecl_free(nodecl_orig_range_initializer);
        nodecl_orig_range_initializer = nodecl_null();
    }

    build_scope_nodecl_for_statement_range_nondependent(
        iterator_symbol,
        range_symbol,
        nodecl_range_initializer,
        nodecl_statement,
        decl_context,
        block_context,
        locus,
        nodecl_output);
}

static void build_scope_for_statement_range(AST a,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    AST loop_control = ASTSon0(a);
    AST statement = ASTSon1(a);

    ast_set_child(a, 0, NULL);
    ast_set_child(a, 1, NULL);

    CXX03_LANGUAGE()
    {
        warn_printf_at(ast_get_locus(a), "range-based for is a C++11 feature\n");
    }

    const decl_context_t* block_context = new_block_context(decl_context);

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

    scope_entry_t* iterator_symbol = build_scope_declarator_name(declarator,
            type_info, declarator_type,
            &gather_info, block_context);

    ERROR_CONDITION(gather_info.num_vla_dimension_symbols > 0, "Unsupported VLAs at the declaration", 0);

    // Wrap this inside an equal initializer to verify the initialization
    expr_or_init_braced = ASTMake1(AST_EQUAL_INITIALIZER,
            expr_or_init_braced,
            ast_get_locus(expr_or_init_braced), NULL);

    nodecl_t nodecl_range_initializer = nodecl_null();
    compute_nodecl_initialization(expr_or_init_braced, decl_context,
            /* preserve_top_level_parentheses */ gather_info.is_decltype_auto,
            &nodecl_range_initializer);

    nodecl_t nodecl_statement = nodecl_null();
    build_scope_normalized_statement(statement, block_context, &nodecl_statement);

    build_scope_nodecl_for_statement_range(
            iterator_symbol,
            nodecl_range_initializer,
            nodecl_statement,
            decl_context,
            block_context,
            ast_get_locus(a),
            nodecl_output);
}

static void build_scope_for_statement(AST a,
        const decl_context_t* decl_context,
        nodecl_t *nodecl_output)
{
    AST loop_control = ASTSon0(a);

    if (ASTKind(loop_control) == AST_LOOP_CONTROL)
        return build_scope_for_statement_nonrange(a,
                decl_context,
                nodecl_output);
    else if (IS_CXX_LANGUAGE
            && ASTKind(loop_control) == AST_RANGE_LOOP_CONTROL)
        // C++2011
        // for (T t : e) S;
        return build_scope_for_statement_range(a,
                decl_context,
                nodecl_output);
    else
        internal_error("Code unreachable", 0);
}

static void build_scope_nodecl_switch_statement(
        nodecl_t nodecl_condition,
        nodecl_t nodecl_statement,
        const decl_context_t* decl_context UNUSED_PARAMETER,
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    build_scope_nodecl_condition_for_switch(
            nodecl_condition,
            decl_context,
            locus,
            &nodecl_condition);

    if (nodecl_is_err_expr(nodecl_condition))
    {
        *nodecl_output = nodecl_make_list_1(
                nodecl_make_err_statement(locus));
        return;
    }

    *nodecl_output =  nodecl_make_list_1(
            nodecl_make_switch_statement(nodecl_condition,
                nodecl_statement,
                locus)
            );
}

static void build_scope_switch_statement(AST a,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    const decl_context_t* block_context = new_block_context(decl_context);

    AST condition = ASTSon0(a);
    AST statement = ASTSon1(a);

    nodecl_t nodecl_condition = nodecl_null();
    build_scope_condition(condition, block_context, &nodecl_condition);

    nodecl_t nodecl_statement = nodecl_null();
    build_scope_normalized_statement(statement, block_context, &nodecl_statement);

    build_scope_nodecl_switch_statement(
            nodecl_condition,
            nodecl_statement,
            block_context,
            ast_get_locus(a),
            nodecl_output);

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_context(
                *nodecl_output,
                block_context,
                ast_get_locus(a))
            );
}

scope_entry_t* add_label_if_not_found(const char* label_text, const decl_context_t* decl_context, const locus_t* locus)
{
    scope_entry_list_t* entry_list = query_name_str_flags(decl_context, label_text, NULL, DF_LABEL);

    scope_entry_t* sym_label = NULL;
    if (entry_list == NULL)
    {
        sym_label = new_symbol(decl_context, decl_context->function_scope, label_text);
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

static void build_scope_nodecl_goto_statement(scope_entry_t* sym_label,
        const decl_context_t* decl_context UNUSED_PARAMETER,
        const locus_t* locus,
        nodecl_t *nodecl_output)
{
    *nodecl_output = nodecl_make_list_1(
            nodecl_make_goto_statement(sym_label, locus));
}

static void build_scope_goto_statement(AST a,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    AST label = ASTSon0(a);
    scope_entry_t* sym_label = add_label_if_not_found(ASTText(label), decl_context, ast_get_locus(label));

    build_scope_nodecl_goto_statement(
            sym_label,
            decl_context,
            ast_get_locus(a),
            nodecl_output);
}

static void build_scope_nodecl_labeled_statement(
        scope_entry_t* sym_label,
        nodecl_t nodecl_statement,
        const decl_context_t* decl_context UNUSED_PARAMETER,
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    *nodecl_output =
        nodecl_make_list_1(
                nodecl_make_labeled_statement(
                    nodecl_statement,
                    sym_label,
                    locus));
}

static void build_scope_labeled_statement(AST a,
        const decl_context_t* decl_context,
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

    build_scope_nodecl_labeled_statement(
            sym_label,
            nodecl_statement,
            decl_context,
            ast_get_locus(a),
            nodecl_output);
}

static void build_scope_nodecl_default_statement(
        nodecl_t nodecl_statement,
        const decl_context_t* decl_context UNUSED_PARAMETER,
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    if (nodecl_get_kind(nodecl_list_head(nodecl_statement)) == NODECL_CASE_STATEMENT)
    {
        // If we find 'default: case X: S;' we will generate 'default: ; case X: S;'
        *nodecl_output = nodecl_concat_lists(
                nodecl_make_list_1(
                    nodecl_make_default_statement(
                        nodecl_make_list_1(
                            nodecl_make_empty_statement(locus)),
                        locus)),
                nodecl_statement);
    }
    else
    {
        *nodecl_output = nodecl_make_list_1(
                nodecl_make_default_statement(nodecl_statement, locus));
    }
}

static void build_scope_default_statement(AST a,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    AST statement = ASTSon0(a);
    nodecl_t nodecl_statement = nodecl_null();
    build_scope_statement(statement, decl_context, &nodecl_statement);

    build_scope_nodecl_default_statement(
            nodecl_statement,
            decl_context,
            ast_get_locus(a),
            nodecl_output);
}

static void build_scope_nodecl_case_statement(nodecl_t nodecl_case_expression_list,
        nodecl_t nodecl_statement,
        const decl_context_t* decl_context UNUSED_PARAMETER,
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    if (nodecl_is_null(nodecl_statement))
    {
        // This case may happen when the case statement is just a declaration
        // case 3: int x;
        *nodecl_output = nodecl_make_list_1(
                nodecl_make_case_statement(
                    nodecl_case_expression_list,
                    nodecl_make_list_1(
                        nodecl_make_empty_statement(locus)),
                    locus));
    }
    else if (nodecl_get_kind(nodecl_list_head(nodecl_statement)) == NODECL_CASE_STATEMENT)
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
                            nodecl_case_expression_list,
                            nodecl_make_list_1(
                                nodecl_make_empty_statement(locus)),
                            locus)),
                    nodecl_statement);
    }
    else
    {
        *nodecl_output = nodecl_make_list_1(
                nodecl_make_case_statement(
                    nodecl_case_expression_list,
                    nodecl_statement,
                    locus));
    }
}

static void build_scope_case_statement(AST a, 
        const decl_context_t* decl_context, 
        nodecl_t* nodecl_output)
{
    AST constant_expression = ASTSon0(a);
    AST statement = ASTSon1(a);

    nodecl_t nodecl_expr = nodecl_null();
    check_expression_must_be_constant(constant_expression, decl_context, &nodecl_expr);

    if (nodecl_is_err_expr(nodecl_expr))
    {
        *nodecl_output = nodecl_make_list_1(
                nodecl_make_err_statement(ast_get_locus(a)));
        return;
    }

    nodecl_expr = nodecl_expression_make_rvalue(nodecl_expr, decl_context);

    if (!nodecl_expr_is_value_dependent(nodecl_expr)
            && !nodecl_is_constant(nodecl_expr))
    {
        error_printf_at(ast_get_locus(a), "case expression '%s' is not constant\n",
                codegen_to_str(nodecl_expr, decl_context));

        *nodecl_output = nodecl_make_list_1(
                nodecl_make_err_statement(ast_get_locus(a)));
        return;
    }

    nodecl_t nodecl_expr_list = nodecl_expr_list = nodecl_make_list_1(nodecl_expr);

    nodecl_t nodecl_statement = nodecl_null();
    build_scope_statement(statement, decl_context, &nodecl_statement);

    build_scope_nodecl_case_statement(
            nodecl_expr_list,
            nodecl_statement,
            decl_context,
            ast_get_locus(a),
            nodecl_output);
}

static void build_scope_nodecl_return_statement(
        type_t* return_type,
        nodecl_t nodecl_return_expression,
        const decl_context_t* decl_context,
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    scope_entry_t* function = decl_context->current_scope->related_entry;
    ERROR_CONDITION(function == NULL
            || (function->kind != SK_FUNCTION
                && function->kind != SK_DEPENDENT_FRIEND_FUNCTION
                && function->kind != SK_LAMBDA),
            "Invalid related entry!", 0);

    scope_entry_t* result_var = symbol_entity_specs_get_result_var(function);

    if (is_dependent_function(function)
            || (!nodecl_is_null(nodecl_return_expression)
                && nodecl_expr_is_type_dependent(nodecl_return_expression)))
    {
        // Do nothing if the function is dependent
        // or the expression is type dependent
    }
    else if (!nodecl_is_null(nodecl_return_expression))
    {
        // Case for 'return e;' or 'return { expr-list };'
        char valid_expr = !nodecl_is_err_expr(nodecl_return_expression);

        if (is_void_type(return_type))
        {
            if ((!nodecl_expr_is_type_dependent(nodecl_return_expression)
                        && !is_void_type(nodecl_get_type(nodecl_return_expression))))
            {
                valid_expr = 0;
                error_printf_at(nodecl_get_locus(nodecl_return_expression), "return with non-void expression in a void function\n");
            }
        }

        if (!valid_expr)
        {
            *nodecl_output = nodecl_make_list_1(
                    nodecl_make_err_statement(
                        locus));
            return;
        }

        if (is_decltype_auto_type(return_type)
                || type_is_derived_from_auto(return_type))
        {
            char verify_deduction = 1;
            type_t* deduced_type = NULL;
            if (is_decltype_auto_type(return_type))
            {
                deduced_type = deduce_decltype_auto_initializer(
                        nodecl_return_expression,
                        return_type,
                        decl_context);
                if (is_error_type(deduced_type))
                {
                    *nodecl_output = nodecl_make_list_1(
                            nodecl_make_err_statement(
                                locus));
                    return;
                }

                if (is_decltype_auto_type(result_var->type_information))
                {
                    result_var->type_information = deduced_type;
                    verify_deduction = 0;
                }
            }
            else
            {
                deduced_type = deduce_auto_initializer(
                        nodecl_return_expression,
                        return_type,
                        decl_context);
                if (is_error_type(deduced_type))
                {
                    *nodecl_output = nodecl_make_list_1(
                            nodecl_make_err_statement(
                                locus));
                    return;
                }

                if (type_is_derived_from_auto(result_var->type_information))
                {
                    result_var->type_information = deduced_type;
                    verify_deduction = 0;
                }
            }

            if (verify_deduction)
            {
                if (!equivalent_types(result_var->type_information,
                            deduced_type))
                {
                    error_printf_at(
                            nodecl_get_locus(nodecl_return_expression),
                            "deduced return type '%s' that is "
                            "different from a previous deduced type '%s'\n",
                            print_type_str(deduced_type, decl_context),
                            print_type_str(result_var->type_information, decl_context));
                    *nodecl_output = nodecl_make_list_1(
                            nodecl_make_err_statement(
                                locus));
                    return;
                }
            }

            return_type = deduced_type;
        }

        if (nodecl_get_kind(nodecl_return_expression) == NODECL_CXX_BRACED_INITIALIZER)
        {
            check_nodecl_braced_initializer(
                    nodecl_return_expression,
                    decl_context,
                    return_type,
                    /* is_explicit_type_cast */ 0,
                    /* allow_excess_of_initializers */ 0,
                    IK_COPY_INITIALIZATION,
                    &nodecl_return_expression);

            if (nodecl_is_err_expr(nodecl_return_expression))
            {
                *nodecl_output = nodecl_make_list_1(
                        nodecl_make_err_statement(
                            locus));
                return;
            }
        }
        else
        {
            type_t* return_expr_type = nodecl_get_type(nodecl_return_expression);

            // In some situations, which are described in section 12.8, when we
            // are checking whether an expression of a certain type can be used
            // to initialize a variable of another type we may need to perform
            // two attempts

            diagnostic_context_t* diagnostics[2] = {NULL, NULL};
            nodecl_t expr_initializer = nodecl_null();

            // 1st attempt: interpret the lvalue expression as an rvalue
            // expression if some conditions are met(C++11/C++14: 12.8)
            if (nodecl_get_kind(nodecl_return_expression) == NODECL_SYMBOL)
            {
                scope_entry_t* sym = nodecl_get_symbol(nodecl_return_expression);
                if (sym->kind == SK_VARIABLE
                        && sym->decl_context->current_scope->kind == BLOCK_SCOPE
                        && !is_any_reference_type(sym->type_information)
                        && !symbol_entity_specs_get_is_static(sym))
                {
                    diagnostics[0] = diagnostic_context_push_buffered();

                    nodecl_set_type(nodecl_return_expression, no_ref(return_expr_type));
                    check_nodecl_expr_initializer(
                            nodecl_return_expression,
                            decl_context,
                            return_type,
                            /* disallow_narrowing */ 0,
                            IK_COPY_INITIALIZATION,
                            &expr_initializer);

                    diagnostic_context_pop();

                    if (nodecl_is_err_expr(expr_initializer))
                    {
                        nodecl_set_type(nodecl_return_expression, return_expr_type);
                        expr_initializer = nodecl_null();
                    }
                }
            }

            // 2nd attempt: leave the expression as it is
            if (nodecl_is_null(expr_initializer))
            {
                diagnostics[1] = diagnostic_context_push_buffered();

                check_nodecl_expr_initializer(
                        nodecl_return_expression,
                        decl_context,
                        return_type,
                        /* disallow_narrowing */ 0,
                        IK_COPY_INITIALIZATION,
                        &expr_initializer);

                diagnostic_context_pop();
            }

            nodecl_return_expression = expr_initializer;
            if (nodecl_is_err_expr(nodecl_return_expression))
            {
                diagnostic_context_t* combine_diagnostics = diagnostic_context_push_buffered();
                diagnostic_context_commit(diagnostics[0]);
                diagnostic_context_commit(diagnostics[1]);
                diagnostic_context_pop();
                diagnostic_context_commit(combine_diagnostics);

                *nodecl_output = nodecl_make_list_1(
                        nodecl_make_err_statement(
                            locus));
                return;
            }

        }
    }
    else
    {
        // Case for 'return;'
        if (type_is_derived_from_auto(return_type)
                || is_decltype_auto_type(return_type))
        {
        }
        else if (return_type != NULL
                && !is_dependent_type(return_type)
                && !is_error_type(return_type)
                && !is_void_type(return_type))
        {
            error_printf_at(locus, "return with no expression in a non-void function\n");
        }
    }

    *nodecl_output = flush_extra_declared_symbols(locus);
    *nodecl_output =
        nodecl_append_to_list(
                *nodecl_output,
                nodecl_make_return_statement(nodecl_return_expression, locus));
}

static void build_scope_return_statement(AST a,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    scope_entry_t* function = decl_context->current_scope->related_entry;
    ERROR_CONDITION(function == NULL
            || (function->kind != SK_FUNCTION
                && function->kind != SK_DEPENDENT_FRIEND_FUNCTION
                && function->kind != SK_LAMBDA),
            "Invalid related entry!", 0);


    type_t* return_type = function_type_get_return_type(function->type_information);
    if (return_type == NULL)
        return_type = get_void_type();

    AST expression = advance_expression_nest_flags(
            ASTSon0(a),
            /* preserve_top_level_parentheses */ is_decltype_auto_type(return_type));

    nodecl_t nodecl_return_expression = nodecl_null();
    if (expression != NULL)
    {
        if (ASTKind(expression) == AST_INITIALIZER_BRACES)
        {
            compute_nodecl_initialization(expression,
                    decl_context,
                    /* preserve_top_level_parentheses */ 0,
                    &nodecl_return_expression);
        }
        else
        {
            check_expression(expression,
                    decl_context,
                    &nodecl_return_expression);
            // FIXME - overlapped logic with preserve_top_level_parentheses
            if (!nodecl_is_err_expr(nodecl_return_expression)
                    && is_decltype_auto_type(return_type)
                    && ASTKind(expression) == AST_PARENTHESIZED_EXPRESSION)
            {
                nodecl_return_expression = cxx_nodecl_wrap_in_parentheses(nodecl_return_expression);
            }
        }
    }

    build_scope_nodecl_return_statement(
            return_type,
            nodecl_return_expression,
            decl_context,
            ast_get_locus(a),
            nodecl_output);
}

static void build_scope_nodecl_try_block(
        nodecl_t nodecl_statement,
        nodecl_t nodecl_catch_list,
        nodecl_t nodecl_catch_any,
        const decl_context_t* decl_context UNUSED_PARAMETER,
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    *nodecl_output = nodecl_make_list_1(
            nodecl_make_try_block(nodecl_statement, nodecl_catch_list, nodecl_catch_any, locus));
}

static void build_scope_nodecl_catch_handler(
        nodecl_t exception_name,
        nodecl_t nodecl_catch_statement,
        type_t* declarator_type,
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    *nodecl_output = nodecl_make_list_1(
            nodecl_make_catch_handler(
                exception_name,
                nodecl_catch_statement,
                declarator_type,
                locus)
            );
}

static void build_scope_try_block(AST a, 
        const decl_context_t* decl_context, 
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

        const decl_context_t* catch_handler_context = new_block_context(decl_context);
        if (ASTKind(exception_declaration) == AST_AMBIGUITY)
        {
            solve_ambiguous_exception_decl(exception_declaration, catch_handler_context);
        }

        if (ASTKind(exception_declaration) != AST_ANY_EXCEPTION)
        {
            AST type_specifier_seq = ASTSon0(exception_declaration);
            // This declarator can be null
            AST declarator = ASTSon1(exception_declaration);

            type_t* type_info = NULL;
            gather_decl_spec_t gather_info;
            memset(&gather_info, 0, sizeof(gather_info));

            nodecl_t dummy = nodecl_null();
            build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &type_info,
                    catch_handler_context, &dummy);

            type_t* declarator_type = type_info;

            nodecl_t exception_name = nodecl_null();
            if (declarator != NULL)
            {
                dummy = nodecl_null();
                compute_declarator_type(declarator, &gather_info, type_info, &declarator_type,
                        catch_handler_context, &dummy);

                scope_entry_t* entry = build_scope_declarator_name(declarator,
                        type_info, declarator_type,
                        &gather_info, catch_handler_context);

                if (entry != NULL)
                {
                    exception_name = nodecl_make_object_init(entry, ast_get_locus(declarator));

                    keep_extra_attributes_in_symbol(entry, &gather_info);
                }
            }

            nodecl_t nodecl_catch_statement = nodecl_null();
            build_scope_statement(handler_compound_statement,
                    catch_handler_context,
                    &nodecl_catch_statement);

            nodecl_t nodecl_current_catch = nodecl_null();
            build_scope_nodecl_catch_handler(
                    exception_name,
                    nodecl_catch_statement,
                    declarator_type,
                    ast_get_locus(exception_declaration),
                    &nodecl_current_catch);

            nodecl_current_catch = 
                nodecl_make_context(
                        nodecl_current_catch,
                        catch_handler_context,
                        ast_get_locus(exception_declaration));

            nodecl_catch_list = nodecl_append_to_list(
                    nodecl_catch_list,
                    nodecl_current_catch);
        }
        else
        {
            if (seen_any_case)
            {
                error_printf_at(ast_get_locus(exception_declaration), "more than one 'catch(...)' handler in try-block\n");
                *nodecl_output = nodecl_make_list_1(
                        nodecl_make_err_statement(
                            ast_get_locus(a)
                            )
                        );
                return;
            }
            seen_any_case = 1;
            build_scope_statement(handler_compound_statement, catch_handler_context, &nodecl_catch_any);
        }
    }

    build_scope_nodecl_try_block(
            nodecl_statement,
            nodecl_catch_list,
            nodecl_catch_any,
            decl_context,
            ast_get_locus(a),
            nodecl_output);
}

static void build_scope_nodecl_do_statement(
        nodecl_t nodecl_statement,
        nodecl_t nodecl_expr,
        const decl_context_t* decl_context UNUSED_PARAMETER,
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    if (nodecl_is_err_expr(nodecl_expr))
    {
        *nodecl_output = nodecl_make_list_1(
                nodecl_make_err_statement(locus)
                );
        return;
    }

    C_LANGUAGE()
    {
        type_t* t = nodecl_get_type(nodecl_expr);
        standard_conversion_t dummy;
        if (!standard_conversion_between_types(&dummy, t, get_bool_type(), locus))
        {
            error_printf_at(locus, "expression of type '%s' is not valid in this context\n",
                    print_type_str(t, decl_context));

            *nodecl_output = nodecl_make_err_statement(locus);
            return;
        }

        nodecl_expr =  cxx_nodecl_make_conversion_to_logical(
                nodecl_expr,
                get_signed_int_type(),
                decl_context,
                locus);
    }
    CXX_LANGUAGE()
    {
        if (!nodecl_expr_is_type_dependent(nodecl_expr))
        {
            check_contextual_conversion(
                    nodecl_expr,
                    get_bool_type(),
                    decl_context,
                    &nodecl_expr);
        }

        if (nodecl_is_err_expr(nodecl_expr))
        {
            *nodecl_output = nodecl_make_err_statement(locus);
            return;
        }
    }

    *nodecl_output = nodecl_make_list_1(
            nodecl_make_do_statement(
                nodecl_statement,
                nodecl_expr,
                locus));
}

static void build_scope_do_statement(AST a,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    AST statement = ASTSon0(a);
    AST expression = ASTSon1(a);

    nodecl_t nodecl_statement = nodecl_null();
    build_scope_normalized_statement(statement, decl_context, &nodecl_statement);

    nodecl_t nodecl_expression = nodecl_null();
    check_expression(expression, decl_context, &nodecl_expression);

    build_scope_nodecl_do_statement(nodecl_statement,
            nodecl_expression,
            decl_context,
            ast_get_locus(a),
            nodecl_output);
}

static void build_scope_nodecl_empty_statement(
        const decl_context_t* decl_context UNUSED_PARAMETER,
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    *nodecl_output = nodecl_make_list_1(
            nodecl_make_empty_statement(locus));
}

static void build_scope_empty_statement(AST a,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    build_scope_nodecl_empty_statement(
            decl_context,
            ast_get_locus(a),
            nodecl_output);
}

static void build_scope_nodecl_break_statement(
        const decl_context_t* decl_context UNUSED_PARAMETER,
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    *nodecl_output = nodecl_make_list_1(
            nodecl_make_break_statement(
                /* construct_name */ nodecl_null(),
                locus));
}

static void build_scope_break_statement(AST a,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    build_scope_nodecl_break_statement(
            decl_context,
            ast_get_locus(a),
            nodecl_output);
}

static void build_scope_nodecl_continue_statement(
        const decl_context_t* decl_context UNUSED_PARAMETER,
        const locus_t* locus,
        nodecl_t* nodecl_output)
{
    *nodecl_output = nodecl_make_list_1(
            nodecl_make_continue_statement(
                /* construct_name */ nodecl_null(),
                locus));
}

static void build_scope_continue_statement(AST a,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    build_scope_nodecl_continue_statement(
            decl_context,
            ast_get_locus(a),
            nodecl_output);
}

static void build_scope_pragma_custom_directive(AST a, 
        const decl_context_t* decl_context, 
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
        const decl_context_t* decl_context, 
        const decl_context_t* top_level_decl_context, 
        nodecl_t* nodecl_output, 
        pragma_block_level_info_t* info)
{
    ERROR_CONDITION(ASTKind(pragma) != AST_PRAGMA_CUSTOM_CONSTRUCT, "Invalid node", 0);

    AST pragma_line = ASTSon0(pragma);
    AST pragma_stmt = ASTSon1(pragma);
    AST end_clauses = ASTSon2(pragma);

    nodecl_t nodecl_pragma_line = nodecl_null();
    common_build_scope_pragma_custom_line(pragma_line, end_clauses, decl_context, &nodecl_pragma_line);

    if (ASTKind(pragma_stmt) == AST_AMBIGUITY)
        solve_ambiguous_statement(pragma_stmt, decl_context);

    nodecl_t nodecl_statement = nodecl_null();

    switch (ASTKind(pragma_stmt))
    {
        case AST_AMBIGUITY:
            {
                internal_error("This should not happen", 0);
            }
        case AST_PRAGMA_CUSTOM_DIRECTIVE:
            {
                error_printf_at(ast_get_locus(pragma_stmt), "invalid nesting of #pragma\n");

                *nodecl_output = nodecl_make_list_1(
                        nodecl_make_err_statement(ast_get_locus(pragma_stmt)));
                return;
            }
        case AST_PRAGMA_CUSTOM_CONSTRUCT:
            {
                const decl_context_t* block_context = new_block_context(decl_context);
                build_scope_statement(pragma_stmt, block_context, &nodecl_statement);

                build_scope_pragma_custom_construct_statement_or_decl_rec(pragma_stmt,
                        block_context,
                        top_level_decl_context,
                        &nodecl_statement, info);

                nodecl_statement =
                    nodecl_make_list_1(
                            nodecl_make_context(
                                nodecl_statement,
                                block_context,
                                nodecl_get_locus(nodecl_statement)));
                break;
            }
        case AST_DECLARATION_STATEMENT:
            {
                // FIXME
                info->is_declaration = 1;

                AST declaration = ASTSon0(pragma_stmt);
                // Note that here we use nodecl_output instead of nodecl_statement

                // Note also that here we use top_level_decl_context rather
                // than any computed decl_context (which may be nested when
                // there are nested pragmas) because we want the side-effect
                // (i.e. registering new entities) to happen in the outermost
                // decl-context (i.e. the context where we found the outermost
                // pragma)
                build_scope_declaration(declaration, top_level_decl_context, nodecl_output,
                        info->declaration_pragma.declared_symbols,
                        info->declaration_pragma.gather_decl_spec_list);
                break;
            }
        case AST_EXPRESSION_STATEMENT:
            {
                // Special case: wrap expressions inside a context when they
                // are the child of a pragma
                const decl_context_t* block_context = new_block_context(decl_context);
                build_scope_statement(pragma_stmt, block_context, &nodecl_statement);

                nodecl_statement =
                    nodecl_make_list_1(
                            nodecl_make_context(
                                nodecl_statement,
                                block_context,
                                nodecl_get_locus(nodecl_statement)));
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
static const decl_context_t* get_prototype_context_if_any(const decl_context_t* decl_context,
        scope_entry_t* entry,
        gather_decl_spec_t gather_info,
        declaration_pragma_info_t* info)
{
    const decl_context_t* result = decl_context;

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
                    nodecl_get_child(symbol_entity_specs_get_function_code(entry), 0)
                    );
        }
    }

    return result;
}

static void finish_pragma_declaration(
        const decl_context_t* decl_context,
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

    DELETE(info->pragma_texts);
    DELETE(info->pragma_lines);

    *nodecl_output = nodecl_concat_lists(*nodecl_output,
            nodecl_pragma_declarations);
}

static void build_scope_pragma_custom_construct_statement(AST a,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    pragma_block_level_info_t info;
    memset(&info, 0, sizeof(info));

    scope_entry_list_t* declared_symbols = NULL;
    gather_decl_spec_list_t gather_decl_spec_list;
    memset(&gather_decl_spec_list, 0, sizeof(gather_decl_spec_list));

    info.declaration_pragma.declared_symbols = &declared_symbols;
    info.declaration_pragma.gather_decl_spec_list = &gather_decl_spec_list;

    nodecl_t nodecl_pragma_body = nodecl_null();
    build_scope_pragma_custom_construct_statement_or_decl_rec(a, decl_context, decl_context, &nodecl_pragma_body, &info);

    if (info.is_declaration)
    {
        nodecl_t nodecl_pragma_finish = nodecl_null();
        finish_pragma_declaration(decl_context, &nodecl_pragma_finish, &info.declaration_pragma);
        *nodecl_output = nodecl_concat_lists(nodecl_pragma_finish, nodecl_pragma_body);
    }
    else
    {
        *nodecl_output = nodecl_pragma_body;
    }
}

static void build_scope_pragma_custom_construct_declaration_rec(
        AST pragma,
        const decl_context_t* decl_context,
        nodecl_t *nodecl_output,
        declaration_pragma_info_t* info)
{
    ERROR_CONDITION(ASTKind(pragma) != AST_PRAGMA_CUSTOM_CONSTRUCT, "Invalid node", 0);

    AST pragma_line = ASTSon0(pragma);
    AST pragma_decl = ASTSon1(pragma);
    AST end_clauses = ASTSon2(pragma);

    nodecl_t nodecl_pragma_line = nodecl_null();
    common_build_scope_pragma_custom_line(pragma_line, end_clauses, decl_context, &nodecl_pragma_line);

    if (ASTKind(pragma_decl) == AST_AMBIGUITY)
        solve_ambiguous_declaration(pragma_decl, decl_context);

    switch (ASTKind(pragma_decl))
    {
        case AST_AMBIGUITY:
            {
                internal_error("This should not happen", 0);
            }
        case AST_PRAGMA_CUSTOM_DIRECTIVE:
            {
                error_printf_at(ast_get_locus(pragma_decl), "invalid nesting of #pragma\n");
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
                if (ASTKind(pragma_decl) == AST_FUNCTION_DEFINITION)
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
        const decl_context_t* decl_context,
        nodecl_t *nodecl_output)
{
    scope_entry_list_t* declared_symbols = NULL;
    gather_decl_spec_list_t gather_decl_spec_list;
    memset(&gather_decl_spec_list, 0, sizeof(gather_decl_spec_list));

    declaration_pragma_info_t info;
    memset(&info, 0, sizeof(info));

    info.declared_symbols = &declared_symbols;
    info.gather_decl_spec_list = &gather_decl_spec_list;

    nodecl_t nodecl_pragma_body = nodecl_null();
    build_scope_pragma_custom_construct_declaration_rec(a, decl_context, &nodecl_pragma_body, &info);

    nodecl_t nodecl_pragma_finish = nodecl_null();
    finish_pragma_declaration(decl_context, &nodecl_pragma_finish, &info);

    *nodecl_output = nodecl_concat_lists(
        nodecl_pragma_finish,
        nodecl_pragma_body);
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
        const decl_context_t* decl_context,
        nodecl_t *nodecl_output,
        member_declaration_pragma_info_t* info)
{
    ERROR_CONDITION(ASTKind(pragma) != AST_PRAGMA_CUSTOM_CONSTRUCT, "Invalid node", 0);

    AST pragma_line = ASTSon0(pragma);
    AST pragma_decl = ASTSon1(pragma);
    AST end_clauses = ASTSon2(pragma);

    nodecl_t nodecl_pragma_line = nodecl_null();
    common_build_scope_pragma_custom_line(pragma_line, end_clauses, decl_context, &nodecl_pragma_line);

    if (ASTKind(pragma_decl) == AST_AMBIGUITY)
        solve_ambiguous_declaration(pragma_decl, decl_context);

    switch (ASTKind(pragma_decl))
    {
        case AST_AMBIGUITY:
            {
                internal_error("This should not happen", 0);
            }
        case AST_PRAGMA_CUSTOM_DIRECTIVE:
            {
                error_printf_at(ast_get_locus(pragma_decl), "invalid nesting of #pragma %s\n",
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
        const decl_context_t* decl_context, 
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
        const decl_context_t* decl_context, 
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
    else if (ASTKind(a) == AST_##x) { stmt_name = x##_STATEMENT; }
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
        const decl_context_t* decl_context, 
        nodecl_t* nodecl_output)
{
    AST forall_header = ASTSon0(a);
    AST statement = ASTSon1(a);

    AST for_init_statement = ASTSon0(forall_header);
    AST condition = ASTSon1(forall_header);
    AST expression = ASTSon2(forall_header);
    AST affinity = ASTSon3(forall_header);

    if (ASTKind(for_init_statement) == AST_AMBIGUITY)
    {
        solve_ambiguous_for_init_statement(for_init_statement, decl_context);
    }

    const decl_context_t* block_context = new_block_context(decl_context);

    nodecl_t nodecl_for_init = nodecl_null();
    if (ASTKind(for_init_statement) == AST_SIMPLE_DECLARATION)
    {
        build_scope_simple_declaration(for_init_statement, block_context, 
                /* is_template */ 0, /* is_explicit_specialization */ 0,
                &nodecl_for_init,
                /* declared_symbols */ NULL, /* gather_decl_spec_list_t */ NULL);
    }
    else if (ASTKind(for_init_statement) == AST_EXPRESSION_STATEMENT)
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

static void build_scope_nodecl_literal(AST a, const decl_context_t* decl_context UNUSED_PARAMETER, nodecl_t* nodecl_output)
{
    *nodecl_output = nodecl_make_from_ast_nodecl_literal(a);
    if (!nodecl_is_list(*nodecl_output))
    {
        *nodecl_output = nodecl_make_list_1(*nodecl_output);
    }
}

static void build_scope_fortran_allocate_statement(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output)
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
    STMT_HANDLER(AST_BREAK_STATEMENT, build_scope_break_statement),
    STMT_HANDLER(AST_CONTINUE_STATEMENT, build_scope_continue_statement),
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

static void build_scope_statement_seq(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output)
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
        const decl_context_t* decl_context, 
        nodecl_t* nodecl_output)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "==== Statement line [%s] ====\n", ast_location(a));
    }

    stmt_scope_handler_t f = stmt_scope_handlers[ASTKind(a)].handler;

    if (f != NULL)
    {
        f(a, decl_context, nodecl_output);
    }
    else
    {
        WARNING_MESSAGE("Statement node type '%s' does not have handler in %s", ast_print_node_type(ASTKind(a)),
                ast_location(a));
    }
}

void build_scope_statement(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    diagnostic_context_push_buffered();
    build_scope_statement_(a, decl_context, nodecl_output);
    diagnostic_context_pop_and_commit();
}

AST get_function_declarator_parameter_list(AST funct_declarator, const decl_context_t* decl_context)
{
    ERROR_CONDITION((funct_declarator == NULL), "This function does not admit NULL trees", 0);

    switch (ASTKind(funct_declarator))
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
                internal_error("Unknown node '%s' at '%s'\n", ast_print_node_type(ASTKind(funct_declarator)),
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
AST get_declarator_name(AST a, const decl_context_t* decl_context)
{
    AST declarator_name = NULL;

    AST declarator_id_expr = get_declarator_id_expression(a, decl_context);

    if (declarator_id_expr != NULL)
    {
        declarator_name = ASTSon0(declarator_id_expr);
    }

    return declarator_name;
}

AST get_declarator_id_expression(AST a, const decl_context_t* decl_context)
{
    if (a == NULL)
        return NULL;

    switch(ASTKind(a))
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
                internal_error("Unknown node '%s'\n", ast_print_node_type(ASTKind(a)));
            }
    }
}

AST get_declarator_id_pack(AST a, const decl_context_t* decl_context)
{
    if (a == NULL)
        return NULL;

    switch(ASTKind(a))
    {
        case AST_INIT_DECLARATOR :
        case AST_MEMBER_DECLARATOR :
        case AST_DECLARATOR :
        case AST_PARENTHESIZED_DECLARATOR :
            {
                return get_declarator_id_pack(ASTSon0(a), decl_context); 
                break;
            }
        case AST_POINTER_DECLARATOR :
            {
                return get_declarator_id_pack(ASTSon1(a), decl_context);
                break;
            }
        case AST_DECLARATOR_ARRAY :
            {
                return get_declarator_id_pack(ASTSon0(a), decl_context);
                break;
            }
        case AST_DECLARATOR_FUNC :
        case AST_DECLARATOR_FUNC_TRAIL :
            {
                return get_declarator_id_pack(ASTSon0(a), decl_context);
                break;
            }
        case AST_DECLARATOR_ID_EXPR :
            {
                return NULL;
                break;
            }
        case AST_DECLARATOR_ID_PACK:
            {
                return a;
                break;
            }
        case AST_AMBIGUITY :
            {
                solve_ambiguous_declarator(a, decl_context);

                // Restart function
                return get_declarator_id_pack(a, decl_context);
            }
        default:
            {
                internal_error("Unknown node '%s'\n", ast_print_node_type(ASTKind(a)));
            }
    }
}

const char* get_conversion_function_name(const decl_context_t* decl_context, 
        AST conversion_function_id, 
        type_t** result_conversion_type)
{
    if (ASTKind(conversion_function_id) != AST_CONVERSION_FUNCTION_ID)
    {
        internal_error("This node '%s' is not valid for this function", 
                ast_print_node_type(ASTKind(conversion_function_id)));
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

nodecl_t internal_expression_parse(const char *source, const decl_context_t* decl_context)
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
        return entry_advance_aliases(symbol_entity_specs_get_alias_to(entry));

    return entry;
}

// Instantiation of statements
#include "cxx-nodecl-visitor.h"

typedef
struct nodecl_instantiate_stmt_visitor_tag
{
    nodecl_external_visitor_t _base_visitor;

    const decl_context_t* orig_decl_context;
    const decl_context_t* new_decl_context;

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

            result_list = nodecl_concat_lists(
                    result_list,
                    current_item);
        }

        DELETE(list);

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
        ERROR_CONDITION(!nodecl_is_null(v->nodecl_result)
                && !nodecl_is_list(v->nodecl_result),
                "Instantiated tree must be a list", 0);
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

static void instantiate_unknown_pragma(nodecl_instantiate_stmt_visitor_t* v,
        nodecl_t nodecl_stmt)
{
    v->nodecl_result = nodecl_make_list_1(nodecl_shallow_copy(nodecl_stmt));
}

// This function does not return a NODECL_TEMPLATE_FUNCTION_CODE but a NODECL_FUNCTION_CODE
static void instantiate_template_function_code(
        nodecl_instantiate_stmt_visitor_t* v,
        nodecl_t node)
{
    nodecl_t nodecl_context = nodecl_get_child(node, 0);
    nodecl_t nodecl_initializers = nodecl_get_child(node, 1);

    ERROR_CONDITION(nodecl_get_kind(nodecl_context) == NODECL_TRY_BLOCK, "Not yet implemented", 0);

    const decl_context_t* new_decl_context = new_block_context(v->new_decl_context);
    new_decl_context = new_function_context(new_decl_context);
    ERROR_CONDITION(v->new_function_instantiated == NULL, "Missing new function", 0);
    new_decl_context->current_scope->related_entry = v->new_function_instantiated;

    ERROR_CONDITION(v->orig_function_instantiated == NULL, "Missing orig function", 0);

    symbol_entity_specs_free_related_symbols(v->new_function_instantiated);

    v->instantiation_symbol_map = instantiation_symbol_map_push(v->instantiation_symbol_map);

    // Register every parameter in this context
    int num_new_parameter = 0;
    int num_parameter;

    // Register function parameters
    for (num_parameter = 0;
            num_parameter < symbol_entity_specs_get_num_related_symbols(v->orig_function_instantiated);
            num_parameter++)
    {
        scope_entry_t* orig_parameter =
                symbol_entity_specs_get_related_symbols_num(v->orig_function_instantiated, num_parameter);
        scope_entry_t* new_parameter = new_symbol(new_decl_context,
                new_decl_context->current_scope,
                orig_parameter->symbol_name);

        new_parameter->kind = orig_parameter->kind;
        new_parameter->type_information = update_type_for_instantiation(
                orig_parameter->type_information,
                new_decl_context,
                nodecl_get_locus(node),
                v->instantiation_symbol_map,
                /* pack */ -1);

        // WARNING - This is a usual source of issues
        symbol_entity_specs_copy_from(new_parameter, orig_parameter);
        symbol_entity_specs_free_function_parameter_info(new_parameter);

        if (orig_parameter->kind == SK_VARIABLE)
        {
            new_parameter->value = instantiate_expression(orig_parameter->value,
                    new_decl_context,
                    v->instantiation_symbol_map,
                    /* pack_index */ -1);

            symbol_entity_specs_add_related_symbols(v->new_function_instantiated,
                    new_parameter);

            symbol_set_as_parameter_of_function(new_parameter, 
                    v->new_function_instantiated,
                    /* nesting */ 0, /* position */ num_new_parameter);
            num_new_parameter++;
        }
        else if (orig_parameter->kind == SK_VARIABLE_PACK)
        {
            int num_types = sequence_of_types_get_num_types(new_parameter->type_information);

            nodecl_t nodecl_sym_list = nodecl_null();

            int num_sub_parameter;
            for (num_sub_parameter = 0; num_sub_parameter < num_types; num_sub_parameter++)
            {
                type_t* t = sequence_of_types_get_type_num(new_parameter->type_information,
                        num_sub_parameter);

                const char* c = NULL;
                uniquestr_sprintf(
                        &c, "_%s__%d",
                        orig_parameter->symbol_name,
                        num_sub_parameter);

                scope_entry_t* new_sub_parameter = new_symbol(new_decl_context,
                        new_decl_context->current_scope,
                        c);

                new_sub_parameter->kind = SK_VARIABLE;
                new_sub_parameter->type_information = t;

                // WARNING - This is a usual source of issues
                symbol_entity_specs_copy_from(new_sub_parameter, orig_parameter);
                // Clear these
                symbol_entity_specs_free_function_parameter_info(new_sub_parameter);

                symbol_entity_specs_add_related_symbols(v->new_function_instantiated, new_sub_parameter);

                symbol_set_as_parameter_of_function(new_sub_parameter, 
                        v->new_function_instantiated,
                        /* nesting */ 0, /* position */ num_new_parameter);
                num_new_parameter++;

                nodecl_t nodecl_sub_symbol = nodecl_make_symbol(
                        new_sub_parameter,
                        nodecl_get_locus(node));
                nodecl_set_type(nodecl_sub_symbol, lvalue_ref(new_sub_parameter->type_information));
                nodecl_sym_list = nodecl_append_to_list(nodecl_sym_list,
                        nodecl_sub_symbol);
            }

            new_parameter->value = nodecl_sym_list;
        }

        instantiation_symbol_map_add(v->instantiation_symbol_map, orig_parameter, new_parameter);
    }

    // Update exceptions as well
#if 0
    if (!nodecl_is_null(symbol_entity_specs_get_noexception(v->orig_function_instantiated)))
    {
        nodecl_t nodecl_expr = instantiate_expression(
                symbol_entity_specs_get_noexception(v->orig_function_instantiated),
                new_decl_context,
                v->instantiation_symbol_map, /* pack_index */ -1);

        nodecl_t nodecl_noexcept = nodecl_null();
        check_nodecl_noexcept_spec(nodecl_expr, new_decl_context, &nodecl_noexcept);

        symbol_entity_specs_set_noexception(v->new_function_instantiated, nodecl_noexcept);
    }
    else
    if (symbol_entity_specs_get_num_exceptions(v->orig_function_instantiated) > 0)
    {
        symbol_entity_specs_get_num_exceptions(v->new_function_instantiated) =
            symbol_entity_specs_get_num_exceptions(v->orig_function_instantiated);
        symbol_entity_specs_get_exceptions(v->new_function_instantiated) =
            xcalloc(symbol_entity_specs_get_num_exceptions(v->orig_function_instantiated),
                    sizeof(*(symbol_entity_specs_get_exceptions(v->new_function_instantiated))));

        memcpy(symbol_entity_specs_get_exceptions(v->new_function_instantiated),
                symbol_entity_specs_get_exceptions(v->orig_function_instantiated),
                symbol_entity_specs_get_num_exceptions(v->orig_function_instantiated) *
                sizeof(*(symbol_entity_specs_get_exceptions(v->new_function_instantiated))));

        int i;
        for (i = 0; i < symbol_entity_specs_get_num_exceptions(v->new_function_instantiated); i++)
        {
            symbol_entity_specs_get_exceptions(v->new_function_instantiated)[i] =
                update_type_for_instantiation(
                        symbol_entity_specs_get_exceptions(v->new_function_instantiated)[i],
                        new_decl_context,
                        nodecl_get_locus(node),
                        v->instantiation_symbol_map,
                        /* pack */ -1);
        }
    }
#endif

    // Create a new result symbol if any
    if (symbol_entity_specs_get_result_var(v->orig_function_instantiated) != NULL)
    {
        scope_entry_t* orig_result_var =
                symbol_entity_specs_get_result_var(v->orig_function_instantiated);
        scope_entry_t* new_result_var = new_symbol(new_decl_context,
                new_decl_context->current_scope,
                orig_result_var->symbol_name);

        new_result_var->kind = SK_VARIABLE;
        new_result_var->type_information = update_type_for_instantiation(
                orig_result_var->type_information,
                new_decl_context,
                nodecl_get_locus(node),
                v->instantiation_symbol_map,
                /* pack */ -1);

        // WARNING - This is a usual source of issues
        symbol_entity_specs_copy_from(new_result_var, orig_result_var);

        symbol_entity_specs_set_result_var(v->new_function_instantiated, new_result_var);

        instantiation_symbol_map_add(v->instantiation_symbol_map, orig_result_var, new_result_var);
    }

    // Register 'this'
    if (symbol_entity_specs_get_is_member(v->new_function_instantiated)
            && !symbol_entity_specs_get_is_static(v->new_function_instantiated))
    {
        // The class we belong to
        type_t* pointed_this = symbol_entity_specs_get_class_type(v->new_function_instantiated);
        // Qualify likewise the function unless it is a destructor
        if (!symbol_entity_specs_get_is_destructor(v->new_function_instantiated))
        {
            pointed_this = get_cv_qualified_type(pointed_this,
                    get_cv_qualifier(v->new_function_instantiated->type_information));
        }

        type_t* this_type = get_pointer_type(pointed_this);
        // It is a constant pointer, so qualify like it is
        this_type = get_cv_qualified_type(this_type, CV_CONST);

        scope_entry_t* this_symbol = new_symbol(new_decl_context,
                new_decl_context->current_scope,
                "this");

        this_symbol->kind = SK_VARIABLE;
        this_symbol->type_information = this_type;
        this_symbol->defined = 1;
        this_symbol->do_not_print = 1;

        // Now map the orig this to the new this
        const decl_context_t* orig_decl_context = nodecl_get_decl_context(nodecl_context);

        scope_entry_list_t* entry_list = query_in_scope_str(orig_decl_context, UNIQUESTR_LITERAL("this"), NULL);
        ERROR_CONDITION(entry_list == NULL, "'this' not found", 0);

        scope_entry_t* orig_this_symbol = entry_list_head(entry_list);
        entry_list_free(entry_list);

        instantiation_symbol_map_add(v->instantiation_symbol_map, orig_this_symbol, this_symbol);
    }

    // Register __MERCURIUM_PRETTY_FUNCTION__
    scope_entry_t* mercurium_pretty_function = NULL;
    {
        const char* pretty_function_str = UNIQUESTR_LITERAL("__PRETTY_FUNCTION__");
        mercurium_pretty_function = register_mercurium_pretty_print(v->new_function_instantiated, new_decl_context);

        // Now map the orig __PRETTY_FUNCTION__ to __MERCURIUM_PRETTY_FUNCTION__
        const decl_context_t* orig_decl_context = nodecl_get_decl_context(nodecl_context);

        scope_entry_list_t* entry_list = query_in_scope_str(orig_decl_context, pretty_function_str, NULL);
        ERROR_CONDITION(entry_list == NULL, "__PRETTY_FUNCTION__ not found", 0);

        scope_entry_t* orig_pretty_function = entry_list_head(entry_list);
        entry_list_free(entry_list);

        instantiation_symbol_map_add(v->instantiation_symbol_map, orig_pretty_function, mercurium_pretty_function);
    }


    const decl_context_t* previous_orig_decl_context = v->orig_decl_context;
    const decl_context_t* previous_new_decl_context = v->new_decl_context;

    const decl_context_t* orig_decl_context = nodecl_get_decl_context(nodecl_context);

    v->orig_decl_context = orig_decl_context;
    v->new_decl_context = new_decl_context;

    nodecl_t nodecl_stmt_list = nodecl_get_child(nodecl_context, 0);

    nodecl_t new_nodecl_stmt_list = instantiate_stmt_walk(v, nodecl_stmt_list);

    {
        // Emit __MERCURIUM_PRETTY_FUNCTION__
        nodecl_t new_compound_stmt = nodecl_list_head(new_nodecl_stmt_list);
        emit_mercurium_pretty_function(new_compound_stmt, mercurium_pretty_function);
    }

    nodecl_t instantiated_nodecl_initializers = instantiate_stmt_walk(v, nodecl_initializers);
    nodecl_t new_nodecl_initializers = nodecl_null();
    if (symbol_entity_specs_get_is_constructor(v->new_function_instantiated))
    {
        check_nodecl_member_initializer_list(instantiated_nodecl_initializers,
                v->new_function_instantiated,
                new_decl_context,
                v->new_function_instantiated->locus,
                &new_nodecl_initializers);
    }

    if (symbol_entity_specs_get_is_destructor(v->new_function_instantiated))
    {
        call_destructor_for_data_layout_members(
                v->new_function_instantiated,
                v->new_decl_context,
                v->new_function_instantiated->locus);
    }

    v->nodecl_result =
        nodecl_make_list_1(
                nodecl_make_function_code(
                    nodecl_make_context(
                        new_nodecl_stmt_list,
                        new_decl_context,
                        v->new_function_instantiated->locus),
                    new_nodecl_initializers,
                    v->new_function_instantiated,
                    v->new_function_instantiated->locus
                    )
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

    build_scope_nodecl_compound_statement(
            new_stmt_list,
            v->new_decl_context,
            nodecl_get_locus(node),
            &v->nodecl_result);
}

static void instantiate_expression_statement(
        nodecl_instantiate_stmt_visitor_t* v,
        nodecl_t node)
{
    nodecl_t expression = nodecl_get_child(node, 0);

    nodecl_t new_expression = instantiate_expression(expression, v->new_decl_context,
            v->instantiation_symbol_map, /* pack_index */ -1);

    build_scope_nodecl_expression_statement(
            new_expression,
            v->new_decl_context,
            nodecl_get_locus(node),
            &v->nodecl_result);
}

static void instantiate_return_statement(
        nodecl_instantiate_stmt_visitor_t* v,
        nodecl_t node)
{
    nodecl_t expression = nodecl_get_child(node, 0);

    nodecl_t nodecl_return_expression = instantiate_expression(expression, v->new_decl_context,
            v->instantiation_symbol_map, /* pack_index */ -1);

    scope_entry_t* function = v->new_decl_context->current_scope->related_entry;
    ERROR_CONDITION(function == NULL
            || (function->kind != SK_FUNCTION
                && function->kind != SK_LAMBDA),
            "Invalid related entry!", 0);

    type_t* return_type = function_type_get_return_type(function->type_information);
    if (return_type == NULL)
        return_type = get_void_type();

    build_scope_nodecl_return_statement(
            return_type,
            nodecl_return_expression,
            v->new_decl_context,
            nodecl_get_locus(node),
            &v->nodecl_result);
}

static scope_entry_t* instantiate_declaration_common(
        nodecl_instantiate_stmt_visitor_t* v,
        scope_entry_t* orig_entry,
        const locus_t* locus,
        char is_definition,
        char check_initializer)
{
    scope_entry_t* new_entry = instantiation_symbol_do_map(v->instantiation_symbol_map, orig_entry);
    if (new_entry == NULL)
    {
        new_entry = new_symbol(v->new_decl_context, v->new_decl_context->current_scope, orig_entry->symbol_name);
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
                    // Warning: this is a common source of issues
                    symbol_entity_specs_copy_from(new_entry, orig_entry);

                    if (check_initializer)
                    {
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
                                        type_is_derived_from_auto(new_entry->type_information)
                                        || is_decltype_auto_type(new_entry->type_information),
                                        is_decltype_auto_type(new_entry->type_information));

                                type_t* initializer_type = nodecl_get_type(nodecl_init);
                                ERROR_CONDITION(initializer_type == NULL, "Missing type", 0);

                                if (is_array_type(new_entry->type_information)
                                        && nodecl_is_null(array_type_get_array_size_expr(new_entry->type_information)))
                                {
                                    ERROR_CONDITION (is_dependent_type(initializer_type), "Invalid type", 0);

                                    cv_qualifier_t cv_qualif = get_cv_qualifier(new_entry->type_information);
                                    new_entry->type_information = get_cv_qualified_type(no_ref(initializer_type), cv_qualif);
                                }
                            }
                            else
                            {
                                check_nodecl_expr_initializer(value,
                                        v->new_decl_context,
                                        get_unqualified_type(new_entry->type_information),
                                        /* disallow_narrowing */ 0,
                                        IK_DIRECT_INITIALIZATION,
                                        &nodecl_init);
                            }

                            new_entry->value = nodecl_init;
                        }
                        else
                        {
                            check_default_initialization_and_destruction_declarator(
                                    new_entry,
                                    v->new_decl_context,
                                    locus);
                        }
                    }
                    else
                    {
                        new_entry->value = nodecl_null();
                    }

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
                internal_error("%s: Not yet implemented for symbol '%s' of kind %s (is_def=%d)\n",
                        locus_to_str(locus),
                        orig_entry->symbol_name,
                        symbol_kind_name(orig_entry),
                        is_definition);
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
    scope_entry_t* new_entry =
        instantiate_declaration_common(
                v,
                nodecl_get_symbol(node),
                nodecl_get_locus(node),
                is_definition,
                /* check_initializer */ 1);

    nodecl_t orig_nodecl_context = nodecl_get_child(node, 0);
    nodecl_t new_nodecl_context = nodecl_null();

    if (!nodecl_is_null(orig_nodecl_context))
    {
        new_nodecl_context = nodecl_make_context(nodecl_null(), v->new_decl_context, nodecl_get_locus(node));
    }

    v->nodecl_result = flush_extra_declared_symbols(nodecl_get_locus(node));
    v->nodecl_result =
        nodecl_append_to_list(
                v->nodecl_result,
                fun(new_nodecl_context, new_entry, nodecl_get_locus(node))
                );
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
    scope_entry_t* new_entry = instantiate_declaration_common(
            v,
            nodecl_get_symbol(node),
            nodecl_get_locus(node),
            /* is_definition */ 1,
            /* check_initializer */ 1);

    return nodecl_make_object_init(new_entry, nodecl_get_locus(node));
}

static void instantiate_object_init(
        nodecl_instantiate_stmt_visitor_t* v,
        nodecl_t node)
{
    nodecl_t n = instantiate_object_init_node(v, node);
    v->nodecl_result =
        flush_extra_declared_symbols(nodecl_get_locus(node));
    v->nodecl_result =
        nodecl_append_to_list(v->nodecl_result, n);
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

    nodecl_initialization_expression = instantiate_expression(
            nodecl_initialization_expression,
            v->new_decl_context,
            v->instantiation_symbol_map,
            /* FIXME: pack_index */ -1);

    v->nodecl_result = 
        nodecl_make_list_1(
                nodecl_make_cxx_member_init(
                    nodecl_cxx_dependent_name,
                    nodecl_initialization_expression,
                    get_unknown_dependent_type(),
                    nodecl_get_locus(node))
                );
}

static void instantiate_cxx_using_namespace(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    nodecl_t orig_used_expression = nodecl_get_child(node, 1);
    nodecl_t used_expression = update_cxx_dep_qualified_name(orig_used_expression,
            v->new_decl_context,
            v->instantiation_symbol_map,
            /* pack_index */ -1);

    check_nodecl_using_directive(used_expression,
            v->new_decl_context,
            // FIXME - Devise a way to remember this
            /* turn_into_inline */ 0,
            &v->nodecl_result);
}

static void instantiate_cxx_using_decl(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    nodecl_t orig_used_expression = nodecl_get_child(node, 1);
    nodecl_t used_expression = update_cxx_dep_qualified_name(orig_used_expression,
            v->new_decl_context,
            v->instantiation_symbol_map,
            /* pack_index */ -1);

    introduce_using_entity_nodecl_name(used_expression, v->new_decl_context,
            /* current_access */ AS_UNKNOWN,
            /* is_typename */ 0,
            &v->nodecl_result);
}

static void instantiate_cxx_for_ranged(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    scope_entry_t* iterator_symbol = nodecl_get_symbol(node);
    nodecl_t nodecl_range_initializer = nodecl_get_child(node, 0);
    nodecl_t nodecl_statement = nodecl_get_child(node, 1);

    const decl_context_t *current_decl_context = v->new_decl_context;
    const decl_context_t *block_context = new_block_context(current_decl_context);

    nodecl_t new_nodecl_range_initializer
        = instantiate_expression(
                nodecl_range_initializer,
                v->new_decl_context,
                v->instantiation_symbol_map,
                /* pack_index */ 1);
    nodecl_t new_nodecl_statement = instantiate_stmt_walk(v, nodecl_statement);

    // Temporarily move inside the block context
    v->new_decl_context = block_context;

    scope_entry_t* new_iterator_symbol = instantiate_declaration_common(
            v,
            iterator_symbol,
            iterator_symbol->locus,
            /* is_definition */ 1,
            /* check_initializer */ 0);

    v->new_decl_context = current_decl_context;
    // Go back to the enclosing context

    build_scope_nodecl_for_statement_range(
            new_iterator_symbol,
            new_nodecl_range_initializer,
            new_nodecl_statement,
            current_decl_context,
            block_context,
            nodecl_get_locus(node),
            &v->nodecl_result);
}

static void instantiate_cxx_static_assert(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_predicate = nodecl_get_child(node, 0);
    nodecl_t nodecl_message = nodecl_get_child(node, 1);

    nodecl_t new_nodecl_predicate
        = instantiate_expression(
                nodecl_predicate,
                v->new_decl_context,
                v->instantiation_symbol_map,
                /* pack_index */ 1);

    nodecl_t nodecl_single_assert = nodecl_null();
    build_scope_nodecl_static_assert(new_nodecl_predicate, nodecl_message, v->new_decl_context, &nodecl_single_assert);

    ERROR_CONDITION(!nodecl_is_null(nodecl_single_assert)
            && !nodecl_is_err_stmt(nodecl_single_assert), "Invalid node", 0);

    v->nodecl_result = nodecl_null();
}

static void instantiate_do_statement(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_stmt = nodecl_get_child(node, 0);
    nodecl_stmt = instantiate_stmt_walk(v, nodecl_stmt);

    nodecl_t nodecl_expr = nodecl_get_child(node, 1);

    nodecl_expr = instantiate_expression(nodecl_expr,
            v->new_decl_context,
            v->instantiation_symbol_map,
            /* pack_index */ 1);

    build_scope_nodecl_do_statement(
            nodecl_stmt,
            nodecl_expr,
            v->new_decl_context,
            nodecl_get_locus(node),
            &v->nodecl_result);
}

static void instantiate_context(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    const decl_context_t* existing_context = v->new_decl_context;
    v->new_decl_context = new_block_context(existing_context);

    nodecl_t nodecl_in_context = nodecl_get_child(node, 0);
    nodecl_in_context = instantiate_stmt_walk(v, nodecl_in_context);

    v->nodecl_result =
        nodecl_make_list_1(
                nodecl_make_context(
                    nodecl_in_context,
                    v->new_decl_context,
                    nodecl_get_locus(node))
                );

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

    build_scope_nodecl_while_statement(
            nodecl_condition,
            nodecl_statement,
            v->new_decl_context,
            nodecl_get_locus(node),
            &v->nodecl_result);
}

static void instantiate_if_else_statement(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_condition = nodecl_get_child(node, 0);
    nodecl_t nodecl_then = nodecl_get_child(node, 1);
    nodecl_t nodecl_else = nodecl_get_child(node, 2);

    nodecl_condition = instantiate_condition(v, nodecl_condition);
    nodecl_then = instantiate_stmt_walk(v, nodecl_then);
    nodecl_else = instantiate_stmt_walk(v, nodecl_else);

    build_scope_nodecl_if_else_statement(
            nodecl_condition,
            nodecl_then,
            nodecl_else,
            v->new_decl_context,
            nodecl_get_locus(node),
            &v->nodecl_result);
}

static nodecl_t instantiate_loop_init(nodecl_instantiate_stmt_visitor_t* v, nodecl_t nodecl_init)
{
    nodecl_t new_expr_list = nodecl_null();
    int i, n;
    nodecl_t* expr_list = nodecl_unpack_list(nodecl_init, &n);
    for (i = 0; i < n; i++)
    {
        // We can expect object-inits here, the name of the tree is misleading
        nodecl_t expr = expr_list[i];
        nodecl_t new_expr;

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
    DELETE(expr_list);

    return new_expr_list;
}

static void instantiate_for_statement(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_loop_control = nodecl_get_child(node, 0);
    nodecl_t nodecl_statement = nodecl_get_child(node, 1);

    if (nodecl_get_kind(nodecl_loop_control) == NODECL_LOOP_CONTROL)
    {
        nodecl_t nodecl_loop_init = nodecl_get_child(nodecl_loop_control, 0);
        nodecl_t nodecl_loop_condition = nodecl_get_child(nodecl_loop_control, 1);
        nodecl_t nodecl_loop_iter = nodecl_get_child(nodecl_loop_control, 2);

        nodecl_loop_init = instantiate_loop_init(v, nodecl_loop_init);
        if (!nodecl_is_null(nodecl_loop_condition))
            nodecl_loop_condition = instantiate_condition(v, nodecl_loop_condition);
        nodecl_loop_iter = instantiate_expression(nodecl_loop_iter,
                v->new_decl_context,
                v->instantiation_symbol_map,
                /* pack_index */ -1);

        // body of the loop
        nodecl_statement = instantiate_stmt_walk(v, nodecl_statement);

        build_scope_nodecl_for_statement_nonrange(
                nodecl_loop_init,
                nodecl_loop_condition,
                nodecl_loop_iter,
                /* nodecl_loop_name */ nodecl_null(),
                nodecl_statement,
                v->new_decl_context,
                nodecl_get_locus(node),
                &v->nodecl_result);
    }
    else if (nodecl_get_kind(nodecl_loop_control) == NODECL_ITERATOR_LOOP_CONTROL)
    {
        // Here we are instantiating a non-dependent ranged loop
        scope_entry_t* iterator_symbol = nodecl_get_symbol(nodecl_get_child(nodecl_loop_control, 0));

        scope_entry_t* new_iterator_symbol = new_symbol(v->new_decl_context,
                v->new_decl_context->current_scope,
                iterator_symbol->symbol_name);
        new_iterator_symbol->kind = SK_VARIABLE;
        new_iterator_symbol->type_information = iterator_symbol->type_information;
        symbol_entity_specs_copy_from(new_iterator_symbol, iterator_symbol);

        nodecl_t nodecl_range_initializer = nodecl_get_child(nodecl_loop_control, 1);
        nodecl_t new_nodecl_range_initializer = instantiate_expression(
                nodecl_range_initializer,
                v->new_decl_context,
                v->instantiation_symbol_map,
                /* pack_index */ -1);

        nodecl_t new_nodecl_statement = instantiate_stmt_walk(v, nodecl_statement);

        scope_entry_list_t* entry_list = query_in_scope_str(
                nodecl_retrieve_context(node),
                UNIQUESTR_LITERAL(".__range"),
                NULL
                );
        ERROR_CONDITION(entry_list == NULL, "Range symbol not found", 0);
        scope_entry_t* range_symbol = entry_list_head(entry_list);
        entry_list_free(entry_list);

        // Create inaccessible .__range symbol
        scope_entry_t* new_range_symbol = new_symbol(v->new_decl_context,
                v->new_decl_context->current_scope,
                ".__range");
        // Rename it to __range for nice diagnostics
        new_range_symbol->symbol_name = UNIQUESTR_LITERAL("__range");
        new_range_symbol->kind = SK_VARIABLE;
        new_range_symbol->type_information = range_symbol->type_information;
        symbol_entity_specs_copy_from(new_range_symbol, range_symbol);

        // FIXME: this should be done better
        decl_context_t* enclosing_decl_context = decl_context_clone(v->new_decl_context);
        enclosing_decl_context->current_scope =
            enclosing_decl_context->current_scope->contained_in;

        build_scope_nodecl_for_statement_range_nondependent(
                new_iterator_symbol,
                new_range_symbol,
                new_nodecl_range_initializer,
                new_nodecl_statement,
                enclosing_decl_context,
                v->new_decl_context,
                nodecl_get_locus(node),
                &v->nodecl_result);
    }
    else
    {
        internal_error("Code unreachable", 0);
    }
}

static void instantiate_labeled_statement(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_statement = nodecl_get_child(node, 0);

    scope_entry_t* new_label = add_label_if_not_found(nodecl_get_symbol(node)->symbol_name,
            v->new_decl_context,
            nodecl_get_locus(node));

    nodecl_statement = instantiate_stmt_walk(v, nodecl_statement);

    build_scope_nodecl_labeled_statement(
            new_label,
            nodecl_statement,
            v->new_decl_context,
            nodecl_get_locus(node),
            &v->nodecl_result);
}

static void instantiate_default_statement(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_statement = nodecl_get_child(node, 1);
    nodecl_statement = instantiate_stmt_walk(v, nodecl_statement);

    build_scope_nodecl_default_statement(
            nodecl_statement,
            v->new_decl_context,
            nodecl_get_locus(node),
            &v->nodecl_result);
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
    DELETE(list);

    nodecl_statement = instantiate_stmt_walk(v, nodecl_statement);

    v->nodecl_result = nodecl_make_case_statement(
            new_expr_list,
            nodecl_statement,
            nodecl_get_locus(node));

    build_scope_nodecl_case_statement(
            new_expr_list,
            nodecl_statement,
            v->new_decl_context,
            nodecl_get_locus(node),
            &v->nodecl_result);
}

static void instantiate_catch_handler(nodecl_instantiate_stmt_visitor_t *v, nodecl_t node)
{
    nodecl_t exception_name = nodecl_get_child(node, 0);
    nodecl_t stmt_list = nodecl_get_child(node, 1);
    type_t* catch_type = nodecl_get_type(node);

    exception_name = instantiate_stmt_walk(v, exception_name);
    stmt_list = instantiate_stmt_walk(v, stmt_list);
    catch_type = update_type_for_instantiation(
            catch_type,
            v->new_decl_context,
            nodecl_get_locus(node),
            v->instantiation_symbol_map,
            /* pack */ -1);

    build_scope_nodecl_catch_handler(
            exception_name,
            stmt_list,
            catch_type,
            nodecl_get_locus(node),
            &v->nodecl_result);
}

static void instantiate_try_block(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_statement = nodecl_get_child(node, 0);
    nodecl_t nodecl_catch_list = nodecl_get_child(node, 1);
    nodecl_t nodecl_catch_any = nodecl_get_child(node, 2);

    nodecl_statement = instantiate_stmt_walk(v, nodecl_statement);
    nodecl_catch_list = instantiate_stmt_walk(v, nodecl_catch_list);
    nodecl_catch_any = instantiate_stmt_walk(v, nodecl_catch_any);

    build_scope_nodecl_try_block(
            nodecl_statement,
            nodecl_catch_list,
            nodecl_catch_any,
            v->new_decl_context,
            nodecl_get_locus(node),
            &v->nodecl_result);
}

static void instantiate_switch_statement(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_condition = nodecl_get_child(node, 0);
    nodecl_t nodecl_statement = nodecl_get_child(node, 1);

    nodecl_condition = instantiate_condition(v, nodecl_condition);
    nodecl_statement = instantiate_stmt_walk(v, nodecl_statement);

    build_scope_nodecl_switch_statement(
            nodecl_condition,
            nodecl_statement,
            v->new_decl_context,
            nodecl_get_locus(node),
            &v->nodecl_result);
}

static void instantiate_empty_statement(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    build_scope_nodecl_empty_statement(
            v->new_decl_context,
            nodecl_get_locus(node),
            &v->nodecl_result);
}

static void instantiate_break_statement(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    build_scope_nodecl_break_statement(v->new_decl_context, nodecl_get_locus(node), &v->nodecl_result);
}

static void instantiate_continue_statement(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    build_scope_nodecl_continue_statement(v->new_decl_context, nodecl_get_locus(node), &v->nodecl_result);
}

static void instantiate_goto_statement(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    scope_entry_t* label = add_label_if_not_found(nodecl_get_symbol(node)->symbol_name, v->new_decl_context, nodecl_get_locus(node));

    build_scope_nodecl_goto_statement(label,
            v->new_decl_context,
            nodecl_get_locus(node),
            &v->nodecl_result);
}

static void instantiate_pragma_custom_statement(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_pragma_line = nodecl_get_child(node, 0);
    nodecl_t statements = nodecl_get_child(node, 1);

    nodecl_pragma_line = nodecl_shallow_copy(nodecl_pragma_line);
    statements = instantiate_stmt_walk(v, statements);

    v->nodecl_result =
        nodecl_make_list_1(
                nodecl_make_pragma_custom_statement(
                    nodecl_pragma_line,
                    statements,
                    nodecl_get_text(node),
                    nodecl_get_locus(node)));
}

static void instantiate_pragma_custom_directive(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_pragma_line = nodecl_get_child(node, 0);

    nodecl_pragma_line = nodecl_shallow_copy(nodecl_pragma_line);
    nodecl_t nodecl_pragma_context = nodecl_make_pragma_context(v->new_decl_context, nodecl_get_locus(node));

    v->nodecl_result =
        nodecl_make_list_1(
                nodecl_make_pragma_custom_directive(
                    nodecl_pragma_line,
                    nodecl_pragma_context,
                    nodecl_get_text(node),
                    nodecl_get_locus(node)));
}

static void instantiate_pragma_custom_declaration(nodecl_instantiate_stmt_visitor_t* v, nodecl_t node)
{
    instantiate_stmt_not_implemented_yet(v, node);
}

// Initialization
static void instantiate_stmt_init_visitor(nodecl_instantiate_stmt_visitor_t* v,
        const decl_context_t* orig_decl_context,
        const decl_context_t* new_decl_context,
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

    NODECL_VISITOR(v)->visit_cxx_def = instantiate_stmt_visitor_fun(instantiate_cxx_def); // --
    NODECL_VISITOR(v)->visit_cxx_decl = instantiate_stmt_visitor_fun(instantiate_cxx_decl); // --
    NODECL_VISITOR(v)->visit_cxx_explicit_instantiation_def = NULL;
    NODECL_VISITOR(v)->visit_cxx_explicit_instantiation_decl = NULL;
    NODECL_VISITOR(v)->visit_cxx_using_namespace = instantiate_stmt_visitor_fun(instantiate_cxx_using_namespace);
    NODECL_VISITOR(v)->visit_cxx_using_decl = instantiate_stmt_visitor_fun(instantiate_cxx_using_decl);

    NODECL_VISITOR(v)->visit_cxx_member_init = instantiate_stmt_visitor_fun(instantiate_cxx_member_init);

    NODECL_VISITOR(v)->visit_object_init = instantiate_stmt_visitor_fun(instantiate_object_init); // --

    NODECL_VISITOR(v)->visit_template_function_code = instantiate_stmt_visitor_fun(instantiate_template_function_code); // --
    NODECL_VISITOR(v)->visit_compound_statement = instantiate_stmt_visitor_fun(instantiate_compound_statement); // --
    NODECL_VISITOR(v)->visit_expression_statement = instantiate_stmt_visitor_fun(instantiate_expression_statement); // --
    NODECL_VISITOR(v)->visit_return_statement = instantiate_stmt_visitor_fun(instantiate_return_statement); // --
    NODECL_VISITOR(v)->visit_do_statement = instantiate_stmt_visitor_fun(instantiate_do_statement); // --
    NODECL_VISITOR(v)->visit_while_statement = instantiate_stmt_visitor_fun(instantiate_while_statement); // --
    NODECL_VISITOR(v)->visit_if_else_statement = instantiate_stmt_visitor_fun(instantiate_if_else_statement); // --
    NODECL_VISITOR(v)->visit_for_statement = instantiate_stmt_visitor_fun(instantiate_for_statement); // --
    NODECL_VISITOR(v)->visit_labeled_statement = instantiate_stmt_visitor_fun(instantiate_labeled_statement); // --
    NODECL_VISITOR(v)->visit_default_statement = instantiate_stmt_visitor_fun(instantiate_default_statement); // --
    NODECL_VISITOR(v)->visit_case_statement = instantiate_stmt_visitor_fun(instantiate_case_statement); // --
    NODECL_VISITOR(v)->visit_try_block = instantiate_stmt_visitor_fun(instantiate_try_block); // --
    NODECL_VISITOR(v)->visit_catch_handler = instantiate_stmt_visitor_fun(instantiate_catch_handler); // --
    NODECL_VISITOR(v)->visit_switch_statement = instantiate_stmt_visitor_fun(instantiate_switch_statement); // --
    NODECL_VISITOR(v)->visit_empty_statement = instantiate_stmt_visitor_fun(instantiate_empty_statement); // --
    NODECL_VISITOR(v)->visit_break_statement = instantiate_stmt_visitor_fun(instantiate_break_statement); // --
    NODECL_VISITOR(v)->visit_continue_statement = instantiate_stmt_visitor_fun(instantiate_continue_statement); // --
    NODECL_VISITOR(v)->visit_goto_statement = instantiate_stmt_visitor_fun(instantiate_goto_statement); // --

    NODECL_VISITOR(v)->visit_cxx_for_ranged = instantiate_stmt_visitor_fun(instantiate_cxx_for_ranged);
    NODECL_VISITOR(v)->visit_cxx_static_assert = instantiate_stmt_visitor_fun(instantiate_cxx_static_assert);

    NODECL_VISITOR(v)->visit_context = instantiate_stmt_visitor_fun(instantiate_context); // --

    NODECL_VISITOR(v)->visit_unknown_pragma = instantiate_stmt_visitor_fun(instantiate_unknown_pragma);
    NODECL_VISITOR(v)->visit_pragma_custom_statement = instantiate_stmt_visitor_fun(instantiate_pragma_custom_statement);
    NODECL_VISITOR(v)->visit_pragma_custom_directive = instantiate_stmt_visitor_fun(instantiate_pragma_custom_directive);
    NODECL_VISITOR(v)->visit_pragma_custom_declaration = instantiate_stmt_visitor_fun(instantiate_pragma_custom_declaration);
}

nodecl_t instantiate_statement(nodecl_t orig_tree,
        const decl_context_t* orig_decl_context,
        const decl_context_t* new_decl_context,
        instantiation_symbol_map_t* instantiation_symbol_map)
{
    nodecl_instantiate_stmt_visitor_t v;
    instantiate_stmt_init_visitor(&v,
            orig_decl_context,
            new_decl_context,
            instantiation_symbol_map,
            NULL, NULL);

    nodecl_t n = instantiate_stmt_walk(&v, orig_tree);

    ERROR_CONDITION(!nodecl_is_list(n)
            || nodecl_list_length(n) != 1,
            "Invalid instantiated node", 0);

    return nodecl_list_head(n);
}

nodecl_t instantiate_function_code(nodecl_t orig_tree,
        const decl_context_t* orig_decl_context,
        const decl_context_t* new_decl_context,
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

    ERROR_CONDITION(!nodecl_is_list(n)
            || nodecl_list_length(n) != 1,
            "Invalid instantiated node", 0);

    return nodecl_list_head(n);
}

