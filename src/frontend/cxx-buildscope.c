/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#include <string.h>
#include <stdio.h>
#include <signal.h>
#include "extstruct.h"
#include "cxx-driver.h"
#include "cxx-buildscope.h"
#include "cxx-scope.h"
#include "cxx-prettyprint.h"
#include "cxx-typeutils.h"
#include "cxx-utils.h"
#include "cxx-cexpr.h"
#include "cxx-ambiguity.h"
#include "cxx-printscope.h"
#include "cxx-solvetemplate.h"
#include "cxx-instantiation.h"
#include "cxx-tltype.h"
#include "cxx-scopelink.h"
#include "cxx-attrnames.h"
#include "hash_iterator.h"

/*
 * This file builds symbol table. If ambiguous nodes are found disambiguating
 * routines will be called prior to filling symbolic inormation. Note that
 * disambiguating routines will use the currently built symbol table.
 *
 * Note that some "semantic checks" performed here are intended only to verify
 * that lookup and symbol registration are performed correctly. By no means
 * this is a full type checking phase
 */


static void build_scope_declaration(AST a, decl_context_t decl_context);
static void build_scope_declaration_sequence(AST a, decl_context_t decl_context);
static void build_scope_simple_declaration(AST a, decl_context_t decl_context);

static void build_scope_namespace_alias(AST a, decl_context_t decl_context);
static void build_scope_namespace_definition(AST a, decl_context_t decl_context);
static scope_entry_t* build_scope_function_definition(AST a, decl_context_t decl_context);
static void build_scope_declarator_with_parameter_context(AST a, 
        gather_decl_spec_t* gather_info, type_t* simple_type_info, type_t** declarator_type,
        decl_context_t decl_context, decl_context_t *prototype_context);

static void build_scope_member_declaration(decl_context_t inner_decl_context,
        AST a, access_specifier_t current_access, type_t* class_info,
        int step);
static scope_entry_t* build_scope_member_function_definition(decl_context_t decl_context,
        AST a, access_specifier_t current_access, type_t* class_info, int step);
static void build_scope_simple_member_declaration(decl_context_t decl_context, AST a, 
        access_specifier_t current_access, type_t* class_info);

static void gather_type_spec_from_simple_type_specifier(AST a, type_t** type_info,
        decl_context_t decl_context);
static void gather_type_spec_from_enum_specifier(AST a, type_t** type_info, 
        decl_context_t decl_context);
static void gather_type_spec_from_class_specifier(AST a, type_t** type_info,
        decl_context_t decl_context);
static void gather_type_spec_from_dependent_typename(AST a, type_t** simple_type_info,
        decl_context_t decl_context);

static void gather_type_spec_from_elaborated_class_specifier(AST a, type_t** type_info,
        decl_context_t decl_context);
static void gather_type_spec_from_elaborated_enum_specifier(AST a, type_t** type_info,
        decl_context_t decl_context);

static void build_scope_declarator_rec(AST a, type_t** declarator_type, 
        gather_decl_spec_t* gather_info,
        decl_context_t declarator_context,
        decl_context_t entity_context,
        decl_context_t prototype_context);

static scope_entry_t* build_scope_declarator_name(AST declarator_name, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, decl_context_t decl_context);
static scope_entry_t* build_scope_declarator_id_expr(AST declarator_name, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, decl_context_t decl_context);

static void build_scope_linkage_specifier(AST a, decl_context_t decl_context);
static void build_scope_linkage_specifier_declaration(AST a, decl_context_t decl_context);

static void build_scope_template_arguments_for_primary_template(decl_context_t decl_context,
        template_parameter_t** template_parameter_info, int num_template_parameters, 
        template_argument_list_t** template_arguments);

static void build_scope_template_declaration(AST a, AST top_template_decl, decl_context_t decl_context);
static void build_scope_explicit_template_specialization(AST a, decl_context_t decl_context);

static void build_scope_statement_seq(AST a, decl_context_t decl_context);

static void build_scope_template_parameter_list(AST a, 
        template_parameter_t*** template_parameters, int* num_parameters,
        decl_context_t decl_context);
static void build_scope_template_parameter(AST a, 
        template_parameter_t* template_parameters, int num_parameter,
        decl_context_t decl_context);
static void build_scope_nontype_template_parameter(AST a,
        template_parameter_t* template_parameters, int num_parameter,
        decl_context_t decl_context);
static void build_scope_type_template_parameter(AST a,
        template_parameter_t* template_parameters, int num_parameter,
        decl_context_t decl_context);
static void build_scope_template_template_parameter(AST a,
        template_parameter_t* template_parameters, int num_parameter, 
        decl_context_t decl_context);

static void build_scope_member_template_declaration(decl_context_t decl_context, AST a, 
        access_specifier_t current_access, type_t* class_info, int step);
static void build_scope_member_template_function_definition(decl_context_t decl_context,
        AST a, access_specifier_t current_access, type_t* class_info, int step);
        
static void build_scope_member_template_simple_declaration(decl_context_t decl_context, AST a,
        access_specifier_t current_access, type_t* class_info);

static void build_scope_using_directive(AST a, decl_context_t decl_context);
static void build_scope_using_declaration(AST a, decl_context_t decl_context);

static void build_scope_explicit_instantiation(AST a, decl_context_t decl_context);

static scope_entry_t* register_new_typedef_name(AST declarator_id, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, decl_context_t decl_context);
static scope_entry_t* register_new_variable_name(AST declarator_id, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, decl_context_t decl_context);
static scope_entry_t* register_function(AST declarator_id, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, decl_context_t decl_context);

static void build_scope_template_function_definition(AST a, decl_context_t template_context);
static void build_scope_template_simple_declaration(AST a, decl_context_t template_context);

static void build_scope_gcc_asm_definition(AST a, decl_context_t decl_context);

static cv_qualifier_t compute_cv_qualifier(AST a);

static void build_exception_spec(type_t* function_type, AST a, decl_context_t decl_context);

static char is_constructor_declarator(AST a);

static scope_entry_t* find_function_declaration(AST declarator_id, 
        type_t* declarator_type, char* is_overload, decl_context_t decl_context);

static AST get_enclosing_declaration(AST point_of_declarator);

// OpenMP functions needed here
static void build_scope_omp_custom_directive(AST a, decl_context_t decl_context, char* attr_name);
static void build_scope_omp_threadprivate(AST a, decl_context_t decl_context, char* attr_name);
static void build_scope_omp_custom_construct_declaration(AST a, decl_context_t decl_context);

static void build_scope_pragma_custom_directive(AST a, decl_context_t decl_context, char* _dummy);
static void build_scope_pragma_custom_construct_declaration(AST a, decl_context_t decl_context, char* attr_name);

static void mix_template_parameters(decl_context_t decl_context, scope_entry_t* newly_declarated_class, AST a);

// Current linkage, by default C++
static char* current_linkage = "\"C++\"";

static void initialize_builtin_symbols(decl_context_t decl_context);

static AST advance_over_declarator_nests(AST a, decl_context_t decl_context);

void build_scope_dynamic_initializer(void)
{
    // Defined in cxx-attrnames.c
    register_ast_extended_attributes();
}

// Builds scope for the translation unit
void build_scope_translation_unit(translation_unit_t* translation_unit)
{
    AST a = translation_unit->parsed_tree;

    AST list = ASTSon0(a);

    if (list == NULL)
        return;

    decl_context_t decl_context = new_global_context();

    // The global scope is created here
    translation_unit->global_decl_context = decl_context;
    translation_unit->scope_link = scope_link_new(decl_context);

    // Link the AST root node with the global scope
    scope_link_set(translation_unit->scope_link, a, decl_context);

    initialize_builtin_symbols(decl_context);

    // Refactor this and "build_scope_translation_unit_tree_with_global_scope" one day
    build_scope_declaration_sequence(list, decl_context);
}

void build_scope_translation_unit_tree_with_global_scope(AST tree, scope_link_t* scope_link, decl_context_t decl_context)
{
    if (ASTType(tree) != AST_TRANSLATION_UNIT)
    {
        internal_error("This function expects a translation unit tree but '%s'", 
                ast_print_node_type(ASTType(tree)));
    }

    AST list = ASTSon0(tree);
    // The scope will have been already populated with basic things
    build_scope_declaration_sequence(list, decl_context);
}

// This function initialize global symbols that exist in every translation unit
// prior to its translation
static void initialize_builtin_symbols(decl_context_t decl_context)
{
    // __builtin_va_list is a very special type in GCC
    scope_entry_t* builtin_va_list;

    builtin_va_list = new_symbol(decl_context, decl_context.global_scope, "__builtin_va_list");
    builtin_va_list->kind = SK_GCC_BUILTIN_TYPE;
    builtin_va_list->defined = 1;
    builtin_va_list->type_information = get_gcc_builtin_va_list_type();

    CXX_LANGUAGE()
    {
        // __null is a magic NULL in g++
        scope_entry_t* null_keyword;

        null_keyword = new_symbol(decl_context, decl_context.global_scope, "__null");
        null_keyword->kind = SK_VARIABLE;
        null_keyword->type_information = get_signed_int_type();
        null_keyword->expression_value = ASTLeaf(AST_OCTAL_LITERAL, 0, "0");
        null_keyword->defined = 1;
    }
}

static void build_scope_declaration_sequence(AST list, decl_context_t decl_context)
{
    AST iter;
    for_each_element(list, iter)
    {
        build_scope_declaration(ASTSon1(iter), decl_context);
    }
}

static int max_line = 0;

// Build scope for a declaration
static void build_scope_declaration(AST a, decl_context_t decl_context)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "==== Declaration line [%s] ====\n", node_information(a));
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
                build_scope_simple_declaration(a, decl_context);
                break;
            }
        case AST_NAMESPACE_DEFINITION :
            {
                // Namespace definitions are of the form
                //   namespace [name]
                //   {
                //      ...
                //   }
                build_scope_namespace_definition(a, decl_context);
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
                build_scope_function_definition(a, decl_context);
                break;
            }
        case AST_LINKAGE_SPEC :
            {
                // extern "C" { ... }
                build_scope_linkage_specifier(a, decl_context);
                break;
            }
        case AST_LINKAGE_SPEC_DECL :
            {
                // extern "C" int a;
                build_scope_linkage_specifier_declaration(a, decl_context);
                break;
            }
        case AST_EXPORT_TEMPLATE_DECLARATION :
        case AST_TEMPLATE_DECLARATION :
            {
                // [export] template<typename _T> struct A;
                // [export] template<typename _T> struct A { };
                // [export] template<typename _T> void f(_T t);
                build_scope_template_declaration(a, a, decl_context);
                break;
            }
        case AST_EXPLICIT_INSTANTIATION :
            {
                // template A<int>;
                build_scope_explicit_instantiation(a, decl_context);
                break;
            }
        case AST_EXPLICIT_SPECIALIZATION :
            {
                // template<> struct A<int> { };
                build_scope_explicit_template_specialization(a, decl_context);
                break;
            }
        case AST_USING_DIRECTIVE :
            {
                // using namespace std;
                build_scope_using_directive(a, decl_context);
                break;
            }
        case AST_USING_DECL :
        case AST_USING_DECL_TYPENAME :
            {
                // using A::b;
                build_scope_using_declaration(a, decl_context);
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
                build_scope_declaration(a, decl_context);
                break;
            }
        case AST_ASM_DEFINITION :
        case AST_EMPTY_DECL :
        case AST_UNKNOWN_PRAGMA :
            {
                // Do nothing
                break;
            }
        case AST_PRAGMA_CUSTOM_DIRECTIVE :
            {
                build_scope_pragma_custom_directive(a, decl_context, NULL);
                break;
            }
        case AST_PRAGMA_CUSTOM_CONSTRUCT: 
            {
                build_scope_pragma_custom_construct_declaration(a, decl_context, NULL);
                break;
            }
            // OpenMP 2.5 constructs
        case AST_OMP_THREADPRIVATE_DIRECTIVE :
            {
                build_scope_omp_threadprivate(a, decl_context, OMP_IS_THREADPRIVATE_DIRECTIVE);
                break;
            }
        case AST_OMP_CUSTOM_DIRECTIVE :
            {
                build_scope_omp_custom_directive(a, decl_context, NULL);
                break;
            }
        case AST_OMP_CUSTOM_CONSTRUCT :
            {
                build_scope_omp_custom_construct_declaration(a, decl_context);
                break;
            }
            // GCC Extensions
        case AST_GCC_EXTENSION :
            {
                build_scope_declaration(ASTSon0(a), decl_context);
                break;
            }
        case AST_GCC_ASM_DEFINITION :
            {
                build_scope_gcc_asm_definition(a, decl_context);
                break;
            }
        case AST_GCC_USING_DIRECTIVE :
            {
                build_scope_using_directive(a, decl_context);
                break;
            }
        case AST_GCC_NAMESPACE_DEFINITION :
            {
                build_scope_namespace_definition(a, decl_context);
                break;
            }
        case AST_PP_COMMENT :
        case AST_PP_TOKEN :
            {
                // Ignore this, it is a prettyprinted comment or token
                break;
            }
        default :
            {
                internal_error("A declaration of kind '%s' is still unsupported (%s)\n", 
                        ast_print_node_type(ASTType(a)), node_information(a));
                break;
            }
    }
}

// It simply disambiguates
static void build_scope_gcc_asm_definition(AST a, decl_context_t decl_context)
{
    AST asm_parms = ASTSon1(a);

    int i;
    // first one is always an AST_STRING_LITERAL
    for (i = 1; i < ASTNumChildren(asm_parms); i++)
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
                    AST expression = ASTSon2(asm_operand);
                    solve_possibly_ambiguous_expression(expression, decl_context);
                }
            }
        }
    }
}

// It simply disambiguates
static void build_scope_explicit_instantiation(AST a, decl_context_t decl_context)
{
    AST decl_specifier_seq = ASTSon1(a);
    AST declarator = ASTSon2(a);

    type_t* simple_type_info = NULL;
    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));

    if (decl_specifier_seq != NULL)
    {
        build_scope_decl_specifier_seq(decl_specifier_seq, &gather_info, &simple_type_info, decl_context);
    }

    if (declarator != NULL)
    {
        type_t* declarator_type = NULL;
        compute_declarator_type(declarator, &gather_info, simple_type_info, &declarator_type, decl_context);
        build_scope_declarator_name(declarator, declarator_type, &gather_info, decl_context);
    }
}

static void build_scope_using_directive(AST a, decl_context_t decl_context)
{
    // First get the involved namespace
    AST global_op = ASTSon0(a);
    AST nested_name = ASTSon1(a);
    AST name = ASTSon2(a);

    scope_entry_list_t* result_list = query_nested_name(decl_context, 
            global_op, nested_name, name);
        
    ERROR_CONDITION((result_list == NULL), "Namespace '%s' not found\n", ASTText(name));

    ERROR_CONDITION((result_list->next != NULL || result_list->entry->kind != SK_NAMESPACE),
            "Symbol '%s' is not a namespace\n", ASTText(name));

    scope_entry_t* entry = result_list->entry;

    // Now add this namespace to the used namespaces of this scope
    scope_t* namespace_scope = decl_context.current_scope;

    ERROR_CONDITION(namespace_scope->kind != NAMESPACE_SCOPE,
            "Error, using directive not in namespace scope", 0);
    ERROR_CONDITION(entry->related_decl_context.current_scope->kind != NAMESPACE_SCOPE,
            "Error, related scope is not namespace scope", 0);

    P_LIST_ADD_ONCE(namespace_scope->use_namespace, namespace_scope->num_used_namespaces, 
            entry->related_decl_context.current_scope);
}

static void build_scope_using_declaration(AST a, decl_context_t decl_context)
{
    AST global_op = ASTSon0(a);
    AST nested_name_specifier = ASTSon1(a);
    AST unqualified_id = ASTSon2(a);

    ERROR_CONDITION(decl_context.current_scope->kind != CLASS_SCOPE
            && decl_context.current_scope->kind != NAMESPACE_SCOPE
            && decl_context.current_scope->kind != BLOCK_SCOPE,
            "Using declaration not in a class, namespace or block scope", 0);

    scope_entry_list_t* used_entity = query_nested_name(decl_context, 
            global_op, 
            nested_name_specifier, 
            unqualified_id);

    ERROR_CONDITION((used_entity == NULL), 
            "Entity '%s' not found (%s)\n", 
            prettyprint_in_buffer(a), 
            node_information(a));

    while (used_entity != NULL)
    {
        // Now add all the used entities to the current scope
        scope_entry_t* entry = used_entity->entry;

        if (entry->kind != SK_DEPENDENT_ENTITY)
        {
            insert_entry(decl_context.current_scope, entry);
        }

        used_entity = used_entity->next;
    }
}

// Builds scope for a simple declaration
static void build_scope_simple_declaration(AST a, decl_context_t decl_context)
{
    // Empty declarations are meaningless for the symbol table
    // They are of the form
    //    ;
    if (ASTType(a) == AST_EMPTY_DECL)
        return;

    type_t* simple_type_info = NULL;
    gather_decl_spec_t gather_info;
    // Clear stack debris
    memset(&gather_info, 0, sizeof(gather_info));

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
        decl_context_t new_decl_context = decl_context;
        if (ASTSon1(a) == NULL)
        {
            new_decl_context.decl_flags |= DF_NO_DECLARATORS;
        }
        else
        {
            new_decl_context.decl_flags &= (~DF_NO_DECLARATORS);
        }
        build_scope_decl_specifier_seq(ASTSon0(a), &gather_info, &simple_type_info,
                new_decl_context);
    }
    else
    {
        C_LANGUAGE()
        {
            fprintf(stderr, "Warning: Declaration at '%s' does not have decl-specifier, assuming 'int'\n",
                    node_information(a));

            simple_type_info = get_signed_int_type();
        }
    }

    ASTAttrSetValueType(a, LANG_IS_DECLARATION, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_DECLARATION_SPECIFIERS, tl_type_t, tl_ast(ASTSon0(a)));
    ASTAttrSetValueType(a, LANG_DECLARATION_DECLARATORS, tl_type_t, tl_ast(ASTSon1(a)));

    // A type has been specified and there are declarators ahead
    if (simple_type_info != NULL && (ASTSon1(a) != NULL))
    {
        AST list, iter;
        list = ASTSon1(a);

        // For every declarator create its full type based on the type
        // specified in the decl_specifier_seq
        for_each_element(list, iter)
        {
            AST init_declarator = ASTSon1(iter);

            if (ASTType(init_declarator) == AST_AMBIGUITY)
            {
                solve_ambiguous_init_declarator(init_declarator, decl_context);
            }

            AST declarator = ASTSon0(init_declarator);
            AST initializer = ASTSon1(init_declarator);

            type_t* declarator_type;

            // This will create the symbol if it is unqualified
            compute_declarator_type(declarator, &gather_info, 
                    simple_type_info, &declarator_type, decl_context);
            build_scope_declarator_name(declarator, declarator_type, &gather_info, decl_context);

            // This is a simple declaration, thus if it does not declare an
            // extern variable or function, the symbol is already defined here
            if (!gather_info.is_extern
                    && !is_function_type(declarator_type))
            {
                AST declarator_name = get_declarator_name(declarator, decl_context);
                ERROR_CONDITION(declarator_name == NULL, "This cannot be null!", 0);
                scope_entry_list_t* entry_list = query_id_expression(decl_context, declarator_name);

                ERROR_CONDITION((entry_list == NULL), "Symbol '%s' just declared has not been found in the scope",
                        prettyprint_in_buffer(declarator_name));

                CXX_LANGUAGE()
                {
                    ERROR_CONDITION((entry_list->entry->defined 
                                && entry_list->entry->kind != SK_TYPEDEF
                                && !BITMAP_TEST(decl_context.decl_flags, DF_ALLOW_REDEFINITION)),
                            "Symbol '%s' in %s has already been defined", prettyprint_in_buffer(declarator_name),
                            node_information(declarator_name));
                }

                CXX_LANGUAGE()
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Defining symbol '");
                        prettyprint(stderr, declarator_name);
                        fprintf(stderr, "'\n");
                    }
                    entry_list->entry->defined = 1;
                }

                if (initializer != NULL)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Initializer: '%s'\n", ast_print_node_type(ASTType(initializer)));
                    }

                    if (!check_for_initialization(initializer, entry_list->entry->decl_context))
                    {
                        internal_error("Initializer '%s' in %s could not be disambiguated!\n",
                                prettyprint_in_buffer(initializer),
                                node_information(initializer));
                    }

                    entry_list->entry->expression_value = initializer;

                    {
                        AST non_nested_declarator = advance_over_declarator_nests(declarator, decl_context);
                        ASTAttrSetValueType(non_nested_declarator, LANG_INITIALIZER, tl_type_t, tl_ast(initializer));
                    }
                }
            }
            else
            {
                ERROR_CONDITION((initializer != NULL), "An extern symbol cannot be initialized", 0);
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
void build_scope_decl_specifier_seq(AST a, gather_decl_spec_t* gather_info, 
        type_t **type_info, decl_context_t decl_context)
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
            gather_decl_spec_information(spec, gather_info);
        }
    }

    // Gather decl specifier sequence information after type_spec
    list = ASTSon2(a);
    if (list != NULL)
    {
        for_each_element(list, iter)
        {
            AST spec = ASTSon1(iter);
            gather_decl_spec_information(spec, gather_info);
        }
    }

    // Now gather information of the type_spec
    if (ASTSon1(a) != NULL) 
    {
        decl_context_t new_decl_context = decl_context;

        if (gather_info->is_friend)
        {
            new_decl_context.decl_flags |= DF_FRIEND;
        }
        else
        {
            new_decl_context.decl_flags &= (~DF_FRIEND);
        }

        gather_type_spec_information(ASTSon1(a), type_info, new_decl_context);
        
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
        }
        else if (gather_info->is_long == 2)
        {
            if (*type_info == get_signed_int_type())
            {
                *type_info = get_signed_long_long_int_type();
            }
            else if (*type_info == get_double_type())
            {
                *type_info = get_long_double_type();
            }
        }
        // Second signed/usigned
        if (gather_info->is_unsigned)
        {
            if (*type_info == get_signed_char_type())
            {
                *type_info = get_unsigned_char_type();
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
            if (*type_info == get_unsigned_char_type())
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
            fprintf(stderr, "Warning: Declaration at '%s' does not have a type-specifier, assuming 'int'\n",
                    node_information(a));

            *type_info = get_signed_int_type();
        }
    }
}

/*
 * This function gathers everything that is in a decl_spec and fills gather_info
 */
void gather_decl_spec_information(AST a, gather_decl_spec_t* gather_info)
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
            // Happily ignore them :)
            break;
        case AST_GCC_COMPLEX_TYPE :
            gather_info->is_complex = 1;
            break;
            // Unknown node
        default:
            internal_error("Unknown node '%s' (%s)", ast_print_node_type(ASTType(a)), node_information(a));
            break;
    }
}


/*
 * This function fills simple_type_info with type information.
 *
 * scope_t* sc is unused here
 */
void gather_type_spec_information(AST a, type_t** simple_type_info,
        decl_context_t decl_context)
{
    switch (ASTType(a))
    {
        case AST_SIMPLE_TYPE_SPECIFIER :
            gather_type_spec_from_simple_type_specifier(a, simple_type_info, decl_context);
            break;
        case AST_ELABORATED_TYPENAME : 
        case AST_ELABORATED_TYPENAME_TEMPLATE : 
            gather_type_spec_from_dependent_typename(a, simple_type_info, decl_context);
            break;
        case AST_ENUM_SPECIFIER :
            gather_type_spec_from_enum_specifier(a, simple_type_info, decl_context);
            break;
        case AST_CLASS_SPECIFIER :
            gather_type_spec_from_class_specifier(a, simple_type_info, decl_context);
            break;
        case AST_ELABORATED_TYPE_ENUM :
            gather_type_spec_from_elaborated_enum_specifier(a, simple_type_info, decl_context);
            break;
        case AST_GCC_ELABORATED_TYPE_ENUM :
            gather_type_spec_from_elaborated_enum_specifier(a, simple_type_info, decl_context);
            break;
        case AST_ELABORATED_TYPE_CLASS :
            gather_type_spec_from_elaborated_class_specifier(a, simple_type_info, decl_context);
            break;
        case AST_GCC_ELABORATED_TYPE_CLASS :
            gather_type_spec_from_elaborated_class_specifier(ASTSon1(a), simple_type_info, decl_context);
            break;
        case AST_ELABORATED_TYPE_TEMPLATE_TEMPLATE_CLASS :
        case AST_ELABORATED_TYPE_TEMPLATE_CLASS :
            gather_type_spec_from_elaborated_class_specifier(a, simple_type_info, decl_context);
            break;
        case AST_GCC_ELABORATED_TYPE_TEMPLATE_CLASS :
        case AST_GCC_ELABORATED_TYPE_TEMPLATE_TEMPLATE_CLASS :
            gather_type_spec_from_elaborated_class_specifier(ASTSon1(a), simple_type_info, decl_context);
            break;
        case AST_CHAR_TYPE :
            *simple_type_info = get_char_type();
            break;
        case AST_WCHAR_TYPE :
            *simple_type_info = get_wchar_t_type();
            break;
        case AST_BOOL_TYPE :
            *simple_type_info = get_bool_type();
            break;
        case AST_SHORT_TYPE :
            *simple_type_info = get_signed_short_int_type();
            break;
        case AST_INT_TYPE :
            *simple_type_info = get_signed_int_type();
            break;
        case AST_LONG_TYPE :
            *simple_type_info = get_signed_long_int_type();
            break;
        case AST_SIGNED_TYPE :
            *simple_type_info = get_signed_int_type();
            break;
        case AST_UNSIGNED_TYPE :
            *simple_type_info = get_unsigned_int_type();
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
        case AST_AMBIGUITY :
            solve_ambiguous_type_specifier(a, decl_context);
            // Restart function
            gather_type_spec_information(a, simple_type_info, decl_context);
            break;
            // GCC Extensions
        case AST_GCC_TYPEOF :
            *simple_type_info = get_gcc_typeof_type(ASTSon0(a), decl_context);
            break;
        case AST_GCC_TYPEOF_EXPR :
            *simple_type_info = get_gcc_typeof_expr_type(ASTSon0(a), decl_context);
            break;
        default:
            internal_error("Unknown node '%s'", ast_print_node_type(ASTType(a)));
    }
}

static void gather_type_spec_from_elaborated_class_specifier(AST a, type_t** type_info,
        decl_context_t decl_context)
{
    AST global_scope = ASTSon1(a);
    AST nested_name_specifier = ASTSon2(a);
    AST class_symbol = ASTSon3(a);

    scope_entry_list_t* result_list = NULL;

    decl_flags_t decl_flags = DF_NONE;

    if (global_scope == NULL
            && nested_name_specifier == NULL)
    {
        decl_flags |= DF_ELABORATED_NAME;
    }

    CXX_LANGUAGE()
    {
        result_list = query_nested_name_flags(decl_context, global_scope, nested_name_specifier, 
                class_symbol, DF_ALWAYS_CREATE_SPECIALIZATION | decl_flags);
    }

    C_LANGUAGE()
    {
        char* class_name = ASTText(class_symbol);
        class_name = strappend("struct ", class_name);

        result_list = query_unqualified_name_str(decl_context, class_name);
    }

    // Now look for a type
    enum cxx_symbol_kind filter_classes[3] = {SK_CLASS, SK_TEMPLATE_PRIMARY_CLASS, SK_TEMPLATE_SPECIALIZED_CLASS};

    scope_entry_list_t* entry_list = filter_symbol_kind_set(result_list, 3, filter_classes);

    scope_entry_t* entry = (entry_list != NULL) ? entry_list->entry : NULL;

    scope_entry_t* new_class = NULL;
    if (entry == NULL)
    {
        // Create a stub but only if it is unqualified, otherwise it should exist elsewhere
        if (nested_name_specifier == NULL
                && global_scope == NULL)
        {
            // If the declaration has declarators
            if (!BITMAP_TEST(decl_context.decl_flags, DF_NO_DECLARATORS))
            {
                // Look for the smallest enclosing non-prototype
                // non-function-prototype scope
                //
                // Note that in our implementation a function scope is not
                // nested and template scopes live in another world, so they
                // should not appear here
                ERROR_CONDITION(decl_context.current_scope->kind == FUNCTION_SCOPE
                        || decl_context.current_scope->kind == TEMPLATE_SCOPE,
                        "Error, invalid scope", 0);

                while (decl_context.current_scope->kind == CLASS_SCOPE
                        || decl_context.current_scope->kind == PROTOTYPE_SCOPE)
                {
                    decl_context.current_scope = decl_context.current_scope->contained_in;
                }
            }
            if (BITMAP_TEST(decl_context.decl_flags, DF_FRIEND))
            {
                decl_context.current_scope = decl_context.namespace_scope;
            }

            char* class_name = NULL;
            if (ASTType(class_symbol) == AST_SYMBOL)
            {
                class_name = ASTText(class_symbol);
            }
            else // AST_TEMPLATE_ID
            {
                class_name = ASTText(ASTSon0(class_symbol));
            }

            C_LANGUAGE()
            {
                class_name = strappend("struct ", class_name);
            }

            new_class = new_symbol(decl_context, decl_context.current_scope, class_name);

            DEBUG_CODE()
            {
                fprintf(stderr, "Type not found, creating a stub in scope %p for '%s' %p\n", 
                        decl_context.current_scope,
                        class_name,
                        new_class);
            }

            new_class->line = ASTLine(class_symbol);
            new_class->file = ASTFileName(class_symbol);
            new_class->point_of_declaration = get_enclosing_declaration(class_symbol);

            new_class->type_information = get_new_class_type(decl_context);

            *type_info = get_user_defined_type(new_class);

            if (!BITMAP_TEST(decl_context.decl_flags, DF_TEMPLATE) 
                    && ASTType(class_symbol) != AST_TEMPLATE_ID)
            {
                new_class->kind = SK_CLASS;
            }
            else // BITMAP_TEST(decl_context.decl_flags, DF_TEMPLATE) || ASTType(class_symbol) == AST_TEMPLATE_ID
            {

                template_argument_list_t* template_argument_list = NULL;
                if (ASTType(class_symbol) != AST_TEMPLATE_ID)
                {
                    // If this is new, there is no mix to be performed
                    new_class->template_parameter_info = decl_context.template_parameters;
                    new_class->num_template_parameters = decl_context.num_template_parameters;

                    new_class->kind = SK_TEMPLATE_PRIMARY_CLASS;
                    build_scope_template_arguments_for_primary_template(decl_context,
                            new_class->template_parameter_info,
                            new_class->num_template_parameters, 
                            &template_argument_list);

                }
                else
                {
                    new_class->kind = SK_TEMPLATE_SPECIALIZED_CLASS;
                    // Both lookup and decl_context are the same
                    build_scope_template_arguments(decl_context, decl_context, class_symbol, 
                            &template_argument_list);
                }
                template_type_set_template_arguments(new_class->type_information, template_argument_list);

                class_type_set_incomplete_dependent(new_class->type_information);
            }
        }
        else
        {
            running_error("Class name '%s' at '%s' not found", prettyprint_in_buffer(a), node_information(a));
        }
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Class type found already declared in %s:%d, using it\n", entry->file, entry->line);
        }

        *type_info = get_user_defined_type(entry);
        new_class = entry;

        if (BITMAP_TEST(decl_context.decl_flags, DF_EXPLICIT_SPECIALIZATION))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Symbol '%s' does not come from instantiation\n", 
                        entry->symbol_name);
            }
            // It is a complete type that does not depend on anything
            class_type_set_complete_independent(entry->type_information);
        }
    }

    // There are template parameters in this context
    if (decl_context.template_parameters != NULL)
    {
        // Mix parameter templates, but only for primary templates
        if (new_class->template_parameter_info != NULL
            && new_class->kind == SK_TEMPLATE_PRIMARY_CLASS)
        {
            mix_template_parameters(decl_context, new_class, a);
        }
        // Just overwrite since they are in sync now
        new_class->template_parameter_info = decl_context.template_parameters;
        new_class->num_template_parameters = decl_context.num_template_parameters;
    }
}

static void gather_type_spec_from_elaborated_enum_specifier(AST a, type_t** type_info, decl_context_t decl_context)
{
    AST global_scope = ASTSon0(a);
    AST nested_name_specifier = ASTSon1(a);
    AST symbol = ASTSon2(a);

    scope_entry_list_t* result_list = NULL;

    decl_flags_t decl_flags = DF_NONE;

    if (global_scope == NULL && nested_name_specifier == NULL)
    {
        decl_flags |= DF_ELABORATED_NAME;
    }

    CXX_LANGUAGE()
    {
        result_list = query_nested_name_flags(decl_context, global_scope, 
                nested_name_specifier, symbol, decl_flags);
    }

    C_LANGUAGE()
    {
        char* enum_name = ASTText(symbol);

        enum_name = strappend("enum ", enum_name);
        result_list = query_unqualified_name_str(decl_context, enum_name);
    }

    // Look for an enum name
    scope_entry_t* entry = NULL;

    while (result_list != NULL)
    {
        scope_entry_t *current_entry = result_list->entry;
        if (current_entry->kind != SK_ENUM)
        {
            running_error("Type found at %s:%d is not an enum\n", entry->file, entry->line);
        }
        else
        {
            entry = current_entry;
        }
        result_list = result_list->next;
    }

    if (entry == NULL)
    {
        // Create a stub but only if it is unqualified, otherwise it should exist elsewhere
        if (nested_name_specifier == NULL
                && global_scope == NULL
                // If does not exist and there are no declarators
                && BITMAP_TEST(decl_context.decl_flags, DF_NO_DECLARATORS))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Enum type not found, creating a stub for this scope\n");
            }

            char* enum_name = ASTText(symbol);

            C_LANGUAGE()
            {
                enum_name = strappend("enum ", enum_name);
            }

            decl_context_t new_decl_context = decl_context;

            if (!BITMAP_TEST(decl_context.decl_flags, DF_NO_DECLARATORS))
            {
                // If no declarators declare it in the first non-class enclosing namespace
                new_decl_context.current_scope = decl_context.namespace_scope;
            }

            scope_entry_t* new_enum = new_symbol(new_decl_context, new_decl_context.current_scope, enum_name);
            new_enum->line = ASTLine(symbol);
            new_enum->file = ASTFileName(symbol);
            new_enum->point_of_declaration = get_enclosing_declaration(symbol);
            new_enum->kind = SK_ENUM;
            new_enum->type_information = get_new_enum_type(decl_context);

            *type_info = get_user_defined_type(new_enum);
        }
        else
        {
            running_error("Enum type '%s' in %s not found\n", prettyprint_in_buffer(a), node_information(a));
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
    AST global_scope = ASTSon0(a);
    AST nested_name_spec = ASTSon1(a);
    AST name = ASTSon2(a);

    // Remove additional ambiguities that might appear in things of the form 
    // T::template A<B>
    while (nested_name_spec != NULL)
    {
        AST class_name = ASTSon0(nested_name_spec);

        if (ASTType(class_name) == AST_TEMPLATE_ID)
        {
            solve_possibly_ambiguous_template_id(class_name, decl_context);
        }

        nested_name_spec = ASTSon1(nested_name_spec);
    }

    if (ASTType(name) == AST_TEMPLATE_ID)
    {
        solve_possibly_ambiguous_template_id(name, decl_context);
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "Trying to look up a dependent typename '%s'\n",
                prettyprint_in_buffer(a));
    }

    global_scope = ASTSon0(a);
    nested_name_spec = ASTSon1(a);
    name = ASTSon2(a);

    decl_flags_t decl_flags = DF_NO_FAIL;
    if (decl_context.template_nesting != 0)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "We will not instantiate when solving this dependent typename because we are under template hood\n");
        }
        decl_flags |= DF_NO_INSTANTIATE;
    }

    scope_entry_list_t* result = query_nested_name_flags(decl_context, global_scope, nested_name_spec, name, decl_flags);

    char is_dependent = 0;
    if (result != NULL
            && result->entry->kind != SK_TEMPLATE_TYPE_PARAMETER
            && result->entry->kind != SK_TEMPLATE_TEMPLATE_PARAMETER
            && result->entry->kind != SK_DEPENDENT_ENTITY
            && !(is_dependent = is_dependent_type(result->entry->type_information, decl_context)))
    {
        scope_entry_t* entry = result->entry;

        if (entry->kind != SK_TYPEDEF)
        {
            *type_info = get_user_defined_type(result->entry);
        }
        else
        {
            *type_info = advance_over_typedefs(entry->type_information);
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "Dependent typename refers to an existing type\n");
        }

        return;
    }

    DEBUG_CODE()
    {
        if (result == NULL)
        {
            fprintf(stderr, "Typename not found -> returning a dependent type\n");
        }
        else if (result->entry->kind == SK_DEPENDENT_ENTITY)
        {
            fprintf(stderr, "Typename is a dependent entity -> returning a dependent type\n");
        }
        else if (is_dependent)
        {
            fprintf(stderr, "Typename refers to a type that is dependent -> returning a dependent type\n");
        }
    }

    if (decl_context.template_nesting == 0
            && !BITMAP_TEST(decl_context.decl_flags, DF_NO_FAIL))
    {
        internal_error("Dependent typename '%s' not resolved outside of template scope (%s)\n", 
                prettyprint_in_buffer(a), node_information(a));
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "Returning dependent type for '%s'\n", prettyprint_in_buffer(a));
    }

    *type_info = get_template_dependent_type(a, decl_context);
}

/*
 * This routine is called in gather_type_spec_information and its purpose is to fill the simple_type
 * with the proper reference of the user defined type.
 */
static void gather_type_spec_from_simple_type_specifier(AST a, type_t** type_info,
        decl_context_t decl_context)
{
    AST global_op = ASTSon0(a);
    AST nested_name_spec = ASTSon1(a);
    AST type_name = (ASTSon2(a) != NULL) ? ASTSon2(a) : ASTSon3(a);

    if (ASTType(type_name) == AST_TEMPLATE_ID)
    {
        solve_possibly_ambiguous_template_id(type_name, decl_context);
    }
    
    scope_entry_list_t* entry_list = query_nested_name(decl_context, global_op, nested_name_spec, 
            type_name);

    if (entry_list == NULL)
    {
        if (!BITMAP_TEST(decl_context.decl_flags, DF_NO_FAIL))
        {
            running_error("The type name '%s' has not been found in the scope of %s. Did you forget to declare it ?\n",
                    prettyprint_in_buffer(a), node_information(a));
        }
        else
        {
            // Do nothing at the moment
            return;
        }
    }

    {
        scope_entry_list_t* it = entry_list;
        while (it != NULL)
        {
            scope_entry_t* entry = it->entry;
            if (entry->kind != SK_ENUM 
                    && entry->kind != SK_CLASS 
                    && entry->kind != SK_TYPEDEF 
                    && entry->kind != SK_TEMPLATE_TYPE_PARAMETER
                    && entry->kind != SK_TEMPLATE_TEMPLATE_PARAMETER
                    && entry->kind != SK_TEMPLATE_PRIMARY_CLASS
                    && entry->kind != SK_TEMPLATE_SPECIALIZED_CLASS
                    && entry->kind != SK_GCC_BUILTIN_TYPE)
            {
                running_error("Identifier '%s' in %s does not name a type", 
                        prettyprint_in_buffer(a), node_information(a));
            }

            it = it->next;
        }
    }

    scope_entry_t* simple_type_entry = entry_list->entry;

    // Fix the entry if we are asking for a template-name (without template-id)
    // we always adjust to the primary template, unless the given symbol is the
    // injected one (because it refers to the current specialization)
    if (ASTType(type_name) != AST_TEMPLATE_ID
            && simple_type_entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS
            && !simple_type_entry->injected_class_name)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Adjusting template-name '%s' in '%s'\n", 
                    prettyprint_in_buffer(a),
                    node_information(a));
        }
        simple_type_entry = NULL;
        scope_entry_list_t* it = entry_list;
        while (it != NULL)
        {
            scope_entry_t* entry = it->entry;
            if (entry->kind == SK_TEMPLATE_PRIMARY_CLASS)
            {
                simple_type_entry = entry;
            }
            it = it->next;
        }
        ERROR_CONDITION(simple_type_entry == NULL, 
                "While adjusting template-name, the primary template was not found", 0);
    }
    
    (*type_info) = get_user_defined_type(simple_type_entry);
}

/*
 * This function is called for enum specifiers. It saves all enumerated values
 * and if it has been given a name, it is registered in the scope.
 */
void gather_type_spec_from_enum_specifier(AST a, type_t** type_info,
        decl_context_t decl_context)
{
    type_t* enum_type = NULL;

    AST enum_name = ASTSon0(a);

    // If it has name, we register this type name in the symbol table
    // but only if it has not been declared previously
    if (enum_name != NULL)
    {
        char* enum_name_str = ASTText(enum_name);

        C_LANGUAGE()
        {
            enum_name_str = strappend("enum ", enum_name_str);
        }

        scope_entry_list_t* enum_entry_list = query_unqualified_name_str(decl_context, enum_name_str);

        scope_entry_t* new_entry;
            
        if (enum_entry_list != NULL 
                && enum_entry_list->entry->kind == SK_ENUM 
                && enum_entry_list->next == NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Enum '%s' already declared\n", enum_name_str);
            }

            new_entry = enum_entry_list->entry;
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
            new_entry->point_of_declaration = enum_name;
            new_entry->kind = SK_ENUM;
            new_entry->type_information = get_new_enum_type(decl_context);
        }

        enum_type = new_entry->type_information;
        new_entry->defined = 1;
        // Since this type is not anonymous we'll want that type_info
        // refers to this newly created type
        *type_info = get_user_defined_type(new_entry);
    }
    else
    {
        // This is anonymous, both resulting type and enum are the same
        enum_type = *type_info = get_new_enum_type(decl_context);
    }

    AST list, iter;
    list = ASTSon1(a);
    
    if (list != NULL)
    {
        // For every enumeration, sign them up in the symbol table
        for_each_element(list, iter)
        {
            AST enumeration = ASTSon1(iter);
            AST enumeration_name = ASTSon0(enumeration);
            AST enumeration_expr = ASTSon1(enumeration);

            // Note that enums do not define an additional scope
            DEBUG_CODE()
            {
                fprintf(stderr, "Registering enumerator '%s'\n", ASTText(enumeration_name));
            }

            scope_entry_t* enumeration_item = new_symbol(decl_context, decl_context.current_scope, ASTText(enumeration_name));
            enumeration_item->line = ASTLine(enumeration_name);
            enumeration_item->file = ASTFileName(enumeration_name);
            enumeration_item->point_of_declaration = get_enclosing_declaration(enumeration_name);
            enumeration_item->kind = SK_ENUMERATOR;
            enumeration_item->type_information = enum_type;

            if (enumeration_expr != NULL)
            {
                solve_possibly_ambiguous_expression(enumeration_expr, decl_context);

                AST duplicate_expr = duplicate_ast(enumeration_expr);

                AST fake_initializer = ASTMake1(AST_CONSTANT_INITIALIZER, duplicate_expr, ASTLine(duplicate_expr), NULL);
                enumeration_item->expression_value = fake_initializer;

            }

            enum_type_add_enumerator(enum_type, enumeration_item);
        }

    }
}

void build_scope_base_clause(AST base_clause, type_t* class_type, decl_context_t decl_context)
{
    AST list = ASTSon0(base_clause);
    AST iter;
    for_each_element(list, iter)
    {
        AST base_specifier = ASTSon1(iter);

        AST access_spec = NULL;
        AST global_op; 
        AST nested_name_specifier; 
        AST name;

        char is_virtual = 0;
        char is_template_qualified = 0;

        int base_specifier_kind = ASTType(base_specifier);
        switch (base_specifier_kind)
        {
            case AST_BASE_SPECIFIER :
            case AST_BASE_SPECIFIER_TEMPLATE :
                {
                    global_op = ASTSon0(base_specifier);
                    nested_name_specifier = ASTSon1(base_specifier);
                    name = ASTSon2(base_specifier);

                    break;
                }
            case AST_BASE_SPECIFIER_VIRTUAL :
            case AST_BASE_SPECIFIER_ACCESS_VIRTUAL :
            case AST_BASE_SPECIFIER_VIRTUAL_TEMPLATE :
            case AST_BASE_SPECIFIER_ACCESS_VIRTUAL_TEMPLATE :
                {
                    is_virtual = 1;
                    /* Fall through */
                }
            case AST_BASE_SPECIFIER_ACCESS :
            case AST_BASE_SPECIFIER_ACCESS_TEMPLATE :
                {
                    access_spec = ASTSon0(base_specifier);
                    global_op = ASTSon1(base_specifier);
                    nested_name_specifier = ASTSon2(base_specifier);
                    name = ASTSon3(base_specifier);
                    break;
                }
            default :
                internal_error("Unexpected node '%s'\n", ast_print_node_type(ASTType(base_specifier)));
        }

        decl_flags_t decl_flags = DF_NONE;
        switch (base_specifier_kind)
        {
            case AST_BASE_SPECIFIER_TEMPLATE :
            case AST_BASE_SPECIFIER_VIRTUAL_TEMPLATE :
            case AST_BASE_SPECIFIER_ACCESS_TEMPLATE :
            case AST_BASE_SPECIFIER_ACCESS_VIRTUAL_TEMPLATE :
                {
                    is_template_qualified = 1;
                    decl_flags |= DF_NO_FAIL;
                    break;
                }
            default:
                break;
        }

        enum cxx_symbol_kind filter[7] =
        {
            SK_CLASS,
            SK_TEMPLATE_PRIMARY_CLASS,
            SK_TEMPLATE_SPECIALIZED_CLASS, 
            SK_TEMPLATE_TYPE_PARAMETER, 
            SK_TEMPLATE_TEMPLATE_PARAMETER, 
            SK_TYPEDEF, 
            SK_DEPENDENT_ENTITY
        };

        scope_entry_list_t* result_list = query_nested_name_flags(decl_context, 
                global_op, nested_name_specifier, name, decl_flags);

        result_list = filter_symbol_kind_set(result_list, 7, filter);

        scope_entry_t _dep_result = { .kind = SK_DEPENDENT_ENTITY, };
        scope_entry_list_t _dep_result_list = { .entry = &_dep_result, .next = NULL, };
        if (result_list == NULL)
        {
            // We did not find the exact type and it was marked as potentially dependent
            if (decl_context.template_nesting == 0
                    && !BITMAP_TEST(decl_context.decl_flags, DF_NO_FAIL))
            {
                internal_error("Dependent typename '%s' not resolved outside of template scope (%s)\n", 
                        prettyprint_in_buffer(base_specifier), node_information(base_specifier));
            }

            // Return a fake dependent entity
            result_list = &_dep_result_list;
        }

        ERROR_CONDITION((result_list == NULL), "Base class '%s' not found in '%s'!\n", prettyprint_in_buffer(base_specifier),
                node_information(base_specifier));
        scope_entry_t* result = result_list->entry;
        result = give_real_entry(result);

        if (is_class_type(result->type_information))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Base class '%s' IS NOT a dependent type\n", prettyprint_in_buffer(base_specifier));
            }
            if (result->kind == SK_TEMPLATE_SPECIALIZED_CLASS)
            {
                // If the entity (being an independent one) has not been completed, then instantiate it
                if (class_type_is_incomplete_independent(result->type_information))
                {
                    instantiate_template(result, decl_context);
                }
            }

            if (result->related_decl_context.current_scope != NULL)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Adding scope %p as base number %d\n", 
                            result->related_decl_context.current_scope, decl_context.current_scope->num_base_scopes);
                }

                scope_t* base_scope = result->related_decl_context.current_scope;

                ERROR_CONDITION(base_scope->kind != CLASS_SCOPE, 
                        "Base scope is not a class scope", 0);
                ERROR_CONDITION(decl_context.current_scope->kind != CLASS_SCOPE, 
                        "Current scope is not a class scope", 0);

                // Add the base-scope to the class-scope
                P_LIST_ADD(decl_context.current_scope->base_scope, 
                        decl_context.current_scope->num_base_scopes, 
                        base_scope);
            }

            // Inherit the virtual bases of this base (if any)
            {
                int i;
                for (i = 0; i < class_type_get_num_bases(result->type_information); i++)
                {
                    char is_virtual = 0;
                    scope_entry_t* base_class = class_type_get_base_num(result->type_information, i, &is_virtual);
                    if (is_virtual)
                    {
                        class_type_add_base_class(class_type, base_class, is_virtual);
                    }
                }
            }

            // Add the base to the class type
            class_type_add_base_class(class_type, result, is_virtual);
        }
        else if (result->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                || result->kind == SK_TEMPLATE_TYPE_PARAMETER
                || is_template_dependent_type(result->type_information))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Base class '%s' found IS a dependent type\n", prettyprint_in_buffer(base_specifier));
            }
        }
    }
}

static void mix_template_parameters(decl_context_t decl_context, scope_entry_t* newly_declarated_class, AST a)
{
    // Mix parameter templates of a previous primary template declaration
    if (newly_declarated_class->kind == SK_TEMPLATE_PRIMARY_CLASS
            && newly_declarated_class->template_parameter_info != NULL)
    {
        ERROR_CONDITION((newly_declarated_class->num_template_parameters != decl_context.num_template_parameters), 
                "The number of template parameters declared in %s is %d and does not match with a previous " 
                "declaration in %s:%d of %d parameters\n", 
                node_information(a), 
                decl_context.num_template_parameters,
                newly_declarated_class->file,
                newly_declarated_class->line,
                newly_declarated_class->num_template_parameters);

        int i;
        for (i = 0; i < decl_context.num_template_parameters; i++)
        {
            switch (newly_declarated_class->template_parameter_info[i]->kind)
            {
                case TPK_TYPE :
                case TPK_TEMPLATE :
                case TPK_NONTYPE :
                    {
                        // Mix the information
                        if (newly_declarated_class->template_parameter_info[i]->default_argument != NULL
                                && decl_context.template_parameters[i]->default_argument == NULL)
                        {
                            decl_context.template_parameters[i]->default_argument = 
                                newly_declarated_class->template_parameter_info[i]->default_argument;
                            decl_context.template_parameters[i]->default_argument_context = 
                                newly_declarated_class->template_parameter_info[i]->default_argument_context;
                            decl_context.template_parameters[i]->has_default_argument = 1;
                        }
                        else if (newly_declarated_class->template_parameter_info[i]->default_argument == NULL
                                && decl_context.template_parameters[i]->default_argument != NULL)
                        {
                            newly_declarated_class->template_parameter_info[i]->default_argument = 
                                decl_context.template_parameters[i]->default_argument;
                            newly_declarated_class->template_parameter_info[i]->default_argument_context = 
                                decl_context.template_parameters[i]->default_argument_context;
                            newly_declarated_class->template_parameter_info[i]->has_default_argument = 1;
                        }
                        break;
                    }
                default :
                    {
                        internal_error("Unexpected template parameter type %d\n", 
                                newly_declarated_class->template_parameter_info[i]->kind);
                    }
            }
        }
    }
}

/*
 * This function is called for class specifiers
 */
void gather_type_spec_from_class_specifier(AST a, type_t** type_info,
        decl_context_t decl_context)
{
    AST class_head = ASTSon0(a);

    if (ASTType(class_head) == AST_GCC_CLASS_HEAD)
    {
        class_head = ASTSon1(class_head);
    }

    AST class_key = ASTSon0(class_head);
    AST base_clause = ASTSon3(class_head);

    AST class_head_nested_name = ASTSon1(class_head);
    AST class_head_identifier = ASTSon2(class_head);


    char* qualification_name = NULL;
    if (class_head_identifier != NULL)
    {
        qualification_name = prettyprint_in_buffer(class_head_identifier);
    }

    decl_context_t inner_decl_context = new_class_context(decl_context, qualification_name);

    // No class entry
    scope_entry_t* class_entry = NULL;
    
    if (class_head_identifier != NULL)
    {
        // If the class has name, register it in the symbol table but only if
        // it does not exist
        if (ASTType(class_head_identifier) == AST_SYMBOL
                || ASTType(class_head_identifier) == AST_TEMPLATE_ID)
        {
            scope_entry_list_t* class_entry_list = NULL;
            
            // Look up the symbol
            CXX_LANGUAGE()
            {
                if (class_head_nested_name == NULL)
                {
                    class_entry_list = query_in_scope_flags(decl_context,
                            class_head_identifier, DF_ALWAYS_CREATE_SPECIALIZATION);
                }
                else
                {
                    class_entry_list = query_nested_name_flags(decl_context,
                            NULL,
                            class_head_nested_name, 
                            class_head_identifier,
                            DF_ALWAYS_CREATE_SPECIALIZATION);
                }
            }

            C_LANGUAGE()
            {
                // This can only be an AST_SYMBOL in C
                char* class_name = ASTText(class_head_identifier);
                class_name = strappend("struct ", class_name);

                class_entry_list = query_unqualified_name_str(decl_context, class_name);
            }

            enum cxx_symbol_kind filter_classes[3] = 
            {
                SK_CLASS, 
                SK_TEMPLATE_PRIMARY_CLASS, 
                SK_TEMPLATE_SPECIALIZED_CLASS
            };

            class_entry_list = filter_symbol_kind_set(class_entry_list, 3, filter_classes);

            if (class_entry_list != NULL)
            {
                // If a valid class was found
                // Get the class entry
                class_entry = class_entry_list->entry;

                DEBUG_CODE()
                {
                    fprintf(stderr, "Class '%s%s' already declared as %p in scope %p (%s:%d)\n", 
                            prettyprint_in_buffer(class_head_nested_name),
                            prettyprint_in_buffer(class_head_identifier),
                            class_entry, 
                            class_entry->decl_context.current_scope,
                            class_entry->file,
                            class_entry->line);
                }

                // Update the template_scope
                DEBUG_CODE()
                {
                    fprintf(stderr, "Updating template scope\n");
                }
                class_entry->decl_context.template_scope = decl_context.template_scope;

                // Now the scope is the one where the class was defined
                inner_decl_context = new_class_context(class_entry->decl_context,
                        qualification_name);

                // Get its simple type info and adjust its scope
                *type_info = class_entry->type_information;
                class_type_set_inner_context((*type_info), inner_decl_context);
            }
            else if (class_entry_list == NULL
                    && class_head_nested_name == NULL)
            {
                // If no class found and no nested name, the symbol must be created
                // here
                if (ASTType(class_head_identifier) == AST_SYMBOL)
                {
                    C_LANGUAGE()
                    {
                        char* class_name = ASTText(class_head_identifier);
                        class_name = strappend("struct ", class_name);

                        class_entry = new_symbol(decl_context, 
                                decl_context.current_scope, class_name);
                    }

                    CXX_LANGUAGE()
                    {
                        class_entry = new_symbol(decl_context, 
                                decl_context.current_scope, 
                                ASTText(class_head_identifier));
                    }
                }
                else
                {
                    class_entry = new_symbol(decl_context, 
                            decl_context.current_scope, 
                            ASTText(ASTSon0(class_head_identifier)));
                }

                DEBUG_CODE()
                {
                    fprintf(stderr, "Registering class '");
                    prettyprint(stderr, class_head_nested_name);
                    prettyprint(stderr, class_head_identifier);
                    fprintf(stderr, "' (%p) in scope %p\n", class_entry, decl_context.current_scope);
                }

                class_entry->line = ASTLine(class_head_identifier);
                class_entry->file = ASTFileName(class_head_identifier);
                class_entry->point_of_declaration = get_enclosing_declaration(class_head_identifier);

                // Create the class type for this newly created class
                class_entry->type_information = get_new_class_type(decl_context);
                *type_info = class_entry->type_information;

                if (!BITMAP_TEST(decl_context.decl_flags, DF_TEMPLATE))
                {
                    class_entry->kind = SK_CLASS;
                }
                else // (BITMAP_TEST(decl_context.decl_flags, DF_TEMPLATE))
                {
                    if (ASTType(class_head_identifier) == AST_SYMBOL)
                    {
                        class_entry->kind = SK_TEMPLATE_PRIMARY_CLASS;
                    }
                    else if (ASTType(class_head_identifier) == AST_TEMPLATE_ID)
                    {
                        // This path has already been handled when we looked up
                        // the specialized template. Since we used
                        // DF_ALWAYS_CREATE_SPECIALIZATION the specialization
                        // should already exist after the lookup
                        internal_error("Code unreachable", 0);
                    }
                }
            }
            else
            {
                // Other cases are error, not finding a nested class means that
                // something was amiss
                //
                // struct A::B <-- if 'A::B' is not found it means that there is an error
                // {
                // };
                internal_error("Unreachable code caused by '%s'", node_information(a));
            }

            // This code is completely ugly
            // What we do here is mixing template parameters with those already declarated
            //
            // Believe or not, this is valid
            //
            //    template <class _T, class _S = int>;
            //    struct A;
            //
            //    template <class _T = float, class _S>
            //    struct A;
            //
            // and it is equivalent to
            //
            //    template <class _T = float, class _S = int>
            //    struct A;
            //

            // If we are in template "hood"
            if (decl_context.template_parameters != NULL)
            {
                // If the class already had template information and
                // it is a primary template
                if (class_entry->template_parameter_info != NULL
                        && class_entry->kind == SK_TEMPLATE_PRIMARY_CLASS)
                {
                    // Mix template parameters
                    mix_template_parameters(decl_context, class_entry, a);
                }
                // Just overwrite since now are in sync
                class_entry->template_parameter_info = decl_context.template_parameters;
                class_entry->num_template_parameters = decl_context.num_template_parameters;
            }

            // Gather template argument information
            template_argument_list_t *template_argument_list = NULL;
            switch (class_entry->kind)
            {
                case SK_TEMPLATE_PRIMARY_CLASS :
                    {
                        build_scope_template_arguments_for_primary_template(
                                decl_context,
                                class_entry->template_parameter_info, 
                                class_entry->num_template_parameters,
                                &template_argument_list);

                        template_type_set_template_arguments((*type_info), template_argument_list);

                        break;
                    }
                case SK_TEMPLATE_SPECIALIZED_CLASS :
                    {
                        // The new template scope has already been inherited
                        decl_context_t new_decl_context = class_entry->decl_context;
                        new_decl_context.decl_flags |= DF_ALWAYS_CREATE_SPECIALIZATION;

                        build_scope_template_arguments(new_decl_context, new_decl_context, 
                                class_head_identifier, &template_argument_list);

                        template_type_set_template_arguments((*type_info), template_argument_list);
                        break;
                    }
                case SK_CLASS :
                    {
                        break;
                    }
                default :
                    {
                        internal_error("Unexpected symbol kind for class %d\n", class_entry->kind);
                    }
            }
            
            // Save the decl that will be instantiated
            class_type_set_instantiation_trees((*type_info), /*body*/ASTSon1(a), base_clause);

            // Copy the type because we are creating it and we would clobber it
            // otherwise
            class_entry->related_decl_context = inner_decl_context;

            // Clear this flag
            DEBUG_CODE()
            {
                    fprintf(stderr, "Symbol '%s' does not come from instantiation\n",
                            class_entry->symbol_name);
            }
            // It is a complete but dependent symbol
            class_type_set_complete_dependent(class_entry->type_information);

            if (BITMAP_TEST(decl_context.decl_flags, DF_EXPLICIT_SPECIALIZATION))
            {
                // unless it is an explicit specialization
                DEBUG_CODE()
                {
                    fprintf(stderr, "Symbol '%s' explicitly instantiated\n",
                            class_entry->symbol_name);
                }
                // in this case it is complete independent
                class_type_set_complete_independent(class_entry->type_information);
            }

            // Since this type is not anonymous we'll want that type_info
            // refers to this newly created type
            *type_info = get_user_defined_type(class_entry);

            if (class_entry->kind == SK_TEMPLATE_PRIMARY_CLASS)
            {
                // Adjust properly the qualification name because now is just the template-name
                // and we want the full template-id
                inner_decl_context.current_scope->qualification_name = 
                    strappend(inner_decl_context.current_scope->qualification_name, 
                            get_unqualified_template_symbol_name(class_entry, decl_context));
            }
        }
        else
        {
            internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(class_head_identifier)));
        }
    }
    else
    {
        // Unnamed class
        *type_info = get_new_class_type(decl_context);
        // Save the inner scope in the class type
        // (it is used when checking member acesses)
        class_type_set_inner_context((*type_info), inner_decl_context);
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

    // The inner scope is properly adjusted here thus we can link it with the AST
    scope_link_set(CURRENT_COMPILED_FILE(scope_link), a, inner_decl_context);
    
    // Now add the bases
    if (base_clause != NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Adding the bases of this class\n");
        }

        build_scope_base_clause(base_clause, 
                class_entry->type_information, 
                inner_decl_context);

        DEBUG_CODE()
        {
            fprintf(stderr, "Bases added\n");
        }
    }

    // Inject the class symbol in the scope
    CXX_LANGUAGE()
    {
        if (class_entry != NULL 
                && class_entry->symbol_name != NULL)
        {
            scope_entry_t* injected_symbol = new_symbol(inner_decl_context, 
                    inner_decl_context.current_scope, 
                    class_entry->symbol_name);

            *injected_symbol = *class_entry;
            injected_symbol->do_not_print = 1;

            injected_symbol->injected_class_name = 1;
            injected_symbol->injected_class_referred_symbol = class_entry;
        }
    }

    AST member_specification = ASTSon1(a);
    build_scope_member_specification(inner_decl_context, member_specification, 
            current_access, *type_info);
    
    if (class_entry != NULL)
    {
        // If the class had a name, it is completely defined here
        class_entry->defined = 1;
    }
}


void build_scope_member_specification(decl_context_t inner_decl_context, AST member_specification_tree, 
        access_specifier_t default_current_access, type_t* type_info)
{
    ERROR_CONDITION(inner_decl_context.current_scope->kind != CLASS_SCOPE,
            "Error, current scope should be a class scope", 0);
    // Member specification
    access_specifier_t current_access = default_current_access;
    decl_context_t new_inner_decl_context = inner_decl_context;

    new_inner_decl_context.decl_flags &= ~DF_TEMPLATE;
    new_inner_decl_context.decl_flags &= ~DF_EXPLICIT_SPECIALIZATION;

    // // Start afresh, no template parameters here (but template_scope is still available)
    new_inner_decl_context.num_template_parameters = 0;
    new_inner_decl_context.template_parameters = NULL;

    AST iter;

    if (member_specification_tree == NULL)
    {
        return;
    }

    // First pass, sign up only prototypes and simple declarations
    for_each_element(member_specification_tree, iter)
    {
        AST member_specification = ASTSon1(iter);

        if (max_line < ASTLine(member_specification))
        {
            max_line = ASTLine(member_specification);
        }

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
            build_scope_member_declaration(new_inner_decl_context, member_specification,
                    current_access, type_info, /* step= */ 0);
        }
    }

    // Second step, sign up everything
    current_access = default_current_access;
    for_each_element(member_specification_tree, iter)
    {
        AST member_specification = ASTSon1(iter);

        if (max_line < ASTLine(member_specification))
        {
            max_line = ASTLine(member_specification);
        }

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
            build_scope_member_declaration(new_inner_decl_context, member_specification,
                    current_access, type_info, /* step= */ 1);
        }
    }
}


/*
 * This function computes the type of a declarator
 *
 * If the declarator is not abstract, therefore it has a name,
 * build_scope_declarator_name is called to sign it up in the symbol table.
 */
scope_entry_t* build_scope_declarator(AST a, 
        gather_decl_spec_t* gather_info, type_t* type_info, type_t** declarator_type,
        decl_context_t decl_context)
{
    /* To be removed soon */
    static char _warning_deprecated = 0;
    if (!_warning_deprecated)
    {
        fprintf(stderr, "%s",
                "Please do not use function 'build_scope_declarator'.\n"
                "If you only need to compute the type use 'compute_declarator_type'.\n"
                "If you need both the computed type of a declarator and register a symbol\n"
                "use first 'compute_declarator_type' and then 'build_scope_declarator_name'\n");
        _warning_deprecated = 1;
    }
    /* end - To be removed soon */

    scope_entry_t* entry = NULL;

    compute_declarator_type(a, gather_info, type_info, declarator_type, decl_context);

    entry = build_scope_declarator_name(a, *declarator_type, gather_info, decl_context);

    return entry;
}

/*
 * This function just computes the type of a declarator
 * it does not sign in any entry
 */
void compute_declarator_type(AST a, gather_decl_spec_t* gather_info,
        type_t* type_info, type_t** declarator_type, decl_context_t decl_context)
{
    // Create a prototype context for parameters. For function definitions, this one
    // is already given and it is the block scope of the function body
    decl_context_t prototype_context = new_prototype_context(decl_context);

    build_scope_declarator_with_parameter_context(a,
            gather_info, type_info, declarator_type, decl_context, &prototype_context);
}

/*
 * This is the actual implementation of 'build_scope_declarator'
 */
static void build_scope_declarator_with_parameter_context(AST a, 
        gather_decl_spec_t* gather_info, type_t* type_info, type_t** declarator_type,
        decl_context_t decl_context, decl_context_t *prototype_context)
{
    // Set base type, it should be set in build_scope_declarator_rec
    // but anyway, here is fine
    *declarator_type = type_info;

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
                fprintf(stderr, "Warning, context of the qualified declarator '%s' in %s not found,"
                        " falling back to the current one\n", prettyprint_in_buffer(declarator_name),
                        node_information(declarator_name));
            }
            else
            {
                // Update the entity context, inheriting the template_scope
                scope_t* template_scope = decl_context.template_scope;
                entity_context = symbols->entry->decl_context;
                entity_context.template_scope = template_scope;

                prototype_context->current_scope->contained_in = symbols->entry->decl_context.current_scope;
                prototype_context->namespace_scope = symbols->entry->decl_context.namespace_scope;
                prototype_context->class_scope = symbols->entry->decl_context.class_scope;
            }
        }

        {
            AST non_nested_declarator = advance_over_declarator_nests(a, decl_context);
            ASTAttrSetValueType(non_nested_declarator, LANG_IS_DECLARED_NAME, tl_type_t, tl_bool(1));
            ASTAttrSetValueType(non_nested_declarator, LANG_DECLARED_NAME, tl_type_t, tl_ast(declarator_name));
        }

        scope_link_set(CURRENT_COMPILED_FILE(scope_link), a, entity_context);
    }

    build_scope_declarator_rec(a, declarator_type, 
            gather_info, decl_context, entity_context, *prototype_context);
    
    if (declarator_name != NULL)
    {
        // Special case for conversion function ids
        // We fix the return type according to the standard
        if (is_function_type(*declarator_type)
                && function_type_get_return_type(*declarator_type) == NULL)
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
                type_t* conversion_function_type;
                get_conversion_function_name(decl_context, conversion_function_id, &conversion_function_type);

                *declarator_type = get_conversion_type(conversion_function_type);
            }
        }
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "Computed type of '%s' is  '%s'\n", 
                prettyprint_in_buffer(a),
                print_declarator(*declarator_type, decl_context));
    }
}

static void convert_tree_from_nested_name_to_qualified_id(AST tree, 
        AST* nested_name_spec, 
        AST* unqualified_id)
{
    *nested_name_spec = duplicate_ast(tree);

    AST iter = *nested_name_spec;
    while (ASTSon1(iter) != NULL)
    {
        iter = ASTSon1(iter);
    }

    if (iter == *nested_name_spec)
    {
        *nested_name_spec = NULL;
    }
    else
    {
        AST previous_nest = ASTParent(iter);
        ASTSon1(previous_nest) = NULL;
    }
    *unqualified_id = ASTSon0(iter);
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
            if (ASTSon0(pointer_tree) == NULL
                    && ASTSon1(pointer_tree) == NULL)
            {
                *declarator_type = get_pointer_type(pointee_type);
            }
            else
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "---> POINTER TO MEMBER <---\n");
                }

                scope_entry_list_t* entry_list = NULL;

                AST global_op = ASTSon0(pointer_tree);
                AST nested_name_spec = NULL;
                AST unqualified_id = NULL;
                convert_tree_from_nested_name_to_qualified_id(ASTSon1(pointer_tree), &nested_name_spec, &unqualified_id);

                entry_list = query_nested_name(decl_context, 
                        global_op, 
                        nested_name_spec,
                        unqualified_id);

                if (entry_list != NULL)
                {
                    *declarator_type = get_pointer_to_member_type(pointee_type, entry_list->entry);
                }
                else
                {
                    running_error("Class name '%s%s%s' not found\n", 
                            prettyprint_in_buffer(global_op), 
                            prettyprint_in_buffer(nested_name_spec),
                            prettyprint_in_buffer(unqualified_id));
                }
            }
            *declarator_type = get_cv_qualified_type(*declarator_type, 
                    compute_cv_qualifier(ASTSon2(pointer_tree)));
            break;
        case AST_REFERENCE_SPEC :
            *declarator_type = get_reference_type(pointee_type);
            break;
        case AST_GCC_REFERENCE_SPEC :
            *declarator_type = get_reference_type(pointee_type);
            *declarator_type = get_cv_qualified_type(*declarator_type, 
                    compute_cv_qualifier(ASTSon0(pointer_tree)));
            break;
        default :
            internal_error("Unhandled node type '%s'\n", ast_print_node_type(ASTType(pointer_tree)));
            break;
    }
}

/*
 * This function converts a type "T" to a "array x of T"
 */
static void set_array_type(type_t** declarator_type, 
        AST constant_expr, AST static_qualifier, AST cv_qualifier_seq,
        decl_context_t decl_context)
{
    type_t* element_type = *declarator_type;

    if (constant_expr != NULL)
    {
        solve_possibly_ambiguous_expression(constant_expr, decl_context);
    }

    *declarator_type = get_array_type(element_type, constant_expr, decl_context);

    C_LANGUAGE()
    {
        /* C99 static qualifier for arrays is ignored */
        /* C99 cv_qualifier_seq */
        *declarator_type = get_cv_qualified_type(*declarator_type, 
                compute_cv_qualifier(cv_qualifier_seq));
    }
}

/*
 * This function fetches information for every declarator in the
 * parameter_declaration_clause of a functional declarator
 */
static void set_function_parameter_clause(type_t* function_type, 
        AST parameters, decl_context_t decl_context)
{
    if (ASTType(parameters) == AST_EMPTY_PARAMETER_DECLARATION_CLAUSE)
    {
        C_LANGUAGE()
        {
            // In C this is a function definition lacking a prototype
            function_type_set_lacking_prototype(function_type, 1);
        }
        // An empty parameter declaration clause is like (void) in C++
        return;
    }

    AST iter, list;
    list = parameters;
    
    // Do not contaminate the current symbol table if we are in
    // a function declaration, otherwise this should already be 
    // a block scope

    C_LANGUAGE()
    {
        // Nothing to do here with K&R parameters
        if (ASTType(parameters) == AST_KR_PARAMETER_LIST)
        {
            // It will lack a prototype
            function_type_set_lacking_prototype(function_type, 1);
            return;
        }
    }

    // Link the scope of the parameters
    scope_link_set(CURRENT_COMPILED_FILE(scope_link), parameters, decl_context);

    int parameter_position = 0;
    for_each_element(list, iter)
    {
        AST parameter_declaration = ASTSon1(iter);

        if (ASTType(parameter_declaration) == AST_AMBIGUITY)
        {
            solve_ambiguous_parameter_decl(parameter_declaration, decl_context);
            ERROR_CONDITION((ASTType(parameter_declaration) == AST_AMBIGUITY), "Ambiguity not solved %s", 
                    node_information(parameter_declaration));
        }

        if (ASTType(parameter_declaration) == AST_VARIADIC_ARG)
        {
            function_type_set_has_ellipsis(function_type);
            continue;
        }

        ASTAttrSetValueType(parameter_declaration, LANG_IS_PARAMETER_DECLARATION, tl_type_t, tl_bool(1));
        // This is never null
        AST parameter_decl_spec_seq = ASTSon0(parameter_declaration);
        // Declarator can be null
        AST parameter_declarator = ASTSon1(parameter_declaration);
        // Default value can be null
        // The scope of this parameter declaration should be "st" and not parameters_scope
        AST default_argument = ASTSon2(parameter_declaration);

        if (default_argument != NULL)
        {
            solve_possibly_ambiguous_expression(default_argument, decl_context);
        }

        gather_decl_spec_t gather_info;
        memset(&gather_info, 0, sizeof(gather_info));
        
        type_t* simple_type_info;

        decl_context_t param_decl_context = decl_context;
        param_decl_context.decl_flags |= DF_PARAMETER_DECLARATION;

        build_scope_decl_specifier_seq(parameter_decl_spec_seq, &gather_info, &simple_type_info,
                decl_context);

        // It is valid in a function declaration not having a declarator at all
        // (note this is different from having an abstract declarator).
        //
        // int f(int, int*);
        //
        // The first "int" does not contain any declarator while the second has
        // an abstract one

        scope_entry_t* entry = NULL;
        type_t* type_info = NULL;

        // If we have a declarator compute its type
        if (parameter_declarator != NULL)
        {
            compute_declarator_type(parameter_declarator, 
                    &gather_info, simple_type_info, &type_info, param_decl_context);

            entry = build_scope_declarator_name(parameter_declarator, type_info, &gather_info, param_decl_context);

            AST declarator_name = get_declarator_name(parameter_declarator, param_decl_context);
            if (declarator_name != NULL)
            {
                ASTAttrSetValueType(parameter_declaration, LANG_IS_NAMED_PARAMETER_DECLARATION, 
						tl_type_t, tl_bool(1));
                ASTAttrSetValueType(parameter_declaration, LANG_PARAMETER_DECLARATION_NAME, 
						tl_type_t, tl_ast(declarator_name));
            }
        }
        // If we don't have a declarator just save the base type
        else
        {
            type_info = simple_type_info;
        }

        // Now normalize the types

        // First save the original type for the entry itself (but not for the function prototype)
        type_t* original_type = type_info;
        // If the original type is a typedef then we want to ignore
        // all the indirections
        if (is_typedef_type(type_info))
        {
            type_info = advance_over_typedefs(type_info);
        }

        type_info = get_unqualified_type(type_info);

        // function to pointer-to-function standard conversion
        if (is_function_type(type_info))
        {
            type_info = get_pointer_type(type_info);
        }
        // Array to pointer standard conversion
        else if (is_array_type(type_info))
        {
            cv_qualifier_t cv_qualif = get_cv_qualifier(original_type);

            type_info = array_type_get_element_type(type_info);
            /* C99 - cv-qualification is retained in this conversion */
            type_info = get_cv_qualified_type(type_info, cv_qualif);

            type_info = get_pointer_type(type_info);
        }

        if (entry != NULL)
        {
            // A parameter is always a variable entity
            entry->kind = SK_VARIABLE;
            entry->is_parameter = 1;
            entry->parameter_position = parameter_position;

            // Update the type info
            entry->type_information = original_type;
        }

        function_type_add_parameter(function_type, type_info, original_type, default_argument, param_decl_context);
        parameter_position++;
    }

    if ((function_type_get_num_parameters(function_type) == 1)
            && (!function_type_get_has_ellipsis(function_type)))
    {
        type_t* parameter_type = function_type_get_parameter_type_num(function_type, 0);

        if (is_void_type(parameter_type))
        {
            // This list was really empty
            function_type_set_no_parameters(function_type);
        }
    }
}

/*
 * This function converts a type "T" into a "function (...) returning T" type
 */
static void set_function_type(type_t** declarator_type,  
        gather_decl_spec_t* gather_info, AST parameter, AST cv_qualif_tree, AST except_spec, 
        decl_context_t decl_context, decl_context_t prototype_context)
{
    /*
     * FIXME - Many things saved in the type actually belong to the symbol thus
     * hindering type information sharing accross symbols
     */

    type_t* returning_type = *declarator_type;

    *declarator_type = get_function_type(returning_type);

    set_function_parameter_clause(*declarator_type, parameter, prototype_context);

    cv_qualifier_t cv_qualif = compute_cv_qualifier(cv_qualif_tree);

    *declarator_type = get_cv_qualified_type(*declarator_type, cv_qualif);

    build_exception_spec(*declarator_type, except_spec, decl_context);

    function_type_set_static(*declarator_type, gather_info->is_static);
    function_type_set_inline(*declarator_type, gather_info->is_inline);
    function_type_set_virtual(*declarator_type, gather_info->is_virtual);
    function_type_set_explicit(*declarator_type, gather_info->is_explicit);
}

/*
 * This function builds the full type a declarator is representing.  For
 * instance
 *
 *   int (*f)[3];
 *
 * Starts with a base type of "int" and ends being a "pointer to array 3 of int"
 */
static void build_scope_declarator_rec(AST a, type_t** declarator_type, 
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
        decl_context_t prototype_context)
{
    ERROR_CONDITION((a == NULL), "This function does not admit NULL trees", 0);

    switch(ASTType(a))
    {
        case AST_DECLARATOR :
        case AST_PARENTHESIZED_ABSTRACT_DECLARATOR :
        case AST_PARENTHESIZED_DECLARATOR :
            {
                build_scope_declarator_rec(ASTSon0(a), declarator_type, 
                        gather_info, declarator_context, entity_context, prototype_context); 
                break;
            }
        case AST_CONVERSION_DECLARATOR :
        case AST_ABSTRACT_DECLARATOR :
            {
                set_pointer_type(declarator_type, ASTSon0(a), declarator_context);
                if (ASTSon1(a) != NULL)
                {
                    build_scope_declarator_rec(ASTSon1(a), declarator_type, 
                            gather_info, declarator_context, entity_context, prototype_context);
                }
                break;
            }
        case AST_POINTER_DECL :
            {
                set_pointer_type(declarator_type, ASTSon0(a), declarator_context);
                build_scope_declarator_rec(ASTSon1(a), declarator_type, 
                        gather_info, declarator_context, entity_context, prototype_context);
                break;
            }
        case AST_ABSTRACT_ARRAY :
            {
                set_array_type(declarator_type, 
                        /* expr */ASTSon1(a), 
                        /* (C99)static_qualif */ ASTSon3(a),
                        /* (C99)cv_qualifier_seq */ ASTSon2(a),
                        entity_context);
                if (ASTSon0(a) != NULL)
                {
                    build_scope_declarator_rec(ASTSon0(a), declarator_type, 
                            gather_info, declarator_context, entity_context, prototype_context);
                }
                break;
            }
        case AST_DIRECT_NEW_DECLARATOR :
            {
                set_array_type(declarator_type, 
                        /* expr */ ASTSon1(a),
                        /* N/A */ NULL,
                        /* N/A */ NULL, 
                        entity_context);
                if (ASTSon0(a) != NULL)
                {
                    build_scope_declarator_rec(ASTSon0(a), declarator_type, 
                            gather_info, declarator_context, entity_context, prototype_context);
                }
                break;
            }
        case AST_NEW_DECLARATOR :
            {
                set_pointer_type(declarator_type, ASTSon0(a), entity_context);
                if (ASTSon1(a) != NULL)
                {
                    build_scope_declarator_rec(ASTSon1(a), declarator_type, 
                            gather_info, declarator_context, entity_context, prototype_context);
                }
                break;
            }
        case AST_DECLARATOR_ARRAY :
            {
                set_array_type(declarator_type, 
                        /* expr */ASTSon1(a), 
                        /* (C99)static_qualif */ ASTSon3(a),
                        /* (C99)cv_qualifier_seq */ ASTSon2(a),
                        entity_context);
                build_scope_declarator_rec(ASTSon0(a), declarator_type, 
                        gather_info, declarator_context, entity_context, prototype_context);
                break;
            }
        case AST_ABSTRACT_DECLARATOR_FUNC :
            {
                set_function_type(declarator_type, gather_info, ASTSon1(a), 
                        ASTSon2(a), ASTSon3(a), entity_context, prototype_context);
                if (ASTSon0(a) != NULL)
                {
                    build_scope_declarator_rec(ASTSon0(a), declarator_type, 
                            gather_info, declarator_context, entity_context, prototype_context);
                }
                break;
            }
        case AST_DECLARATOR_FUNC :
            {
                ASTAttrSetValueType(a, LANG_IS_FUNCTIONAL_DECLARATOR, tl_type_t, tl_bool(1));
                set_function_type(declarator_type, gather_info, ASTSon1(a), 
                        ASTSon2(a), ASTSon3(a), entity_context, prototype_context);

                C_LANGUAGE()
                {
                    if (function_type_get_lacking_prototype(*declarator_type))
                    {
                        if (ASTSon1(a) == NULL
                                || ASTType(ASTSon1(a)) == AST_EMPTY_PARAMETER_DECLARATION_CLAUSE)
                        {
                            char *funct_decl_name = prettyprint_in_buffer(ASTSon0(a));
                            fprintf(stderr, "Warning: Function '%s' in '%s' lacks a prototype. "
                                    "Did you mean '%s(void)' instead of '%s()'?\n",
                                    prettyprint_in_buffer(a), node_information(a), funct_decl_name, funct_decl_name);
                        }
                    }
                }

                build_scope_declarator_rec(ASTSon0(a), declarator_type, 
                        gather_info, declarator_context, entity_context, prototype_context);
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
                build_scope_declarator_rec(ASTSon1(a), declarator_type, 
                        gather_info, declarator_context, entity_context, prototype_context); 
                break;
            }
            // attribute * declarator
            // attribute & declarator
        case AST_GCC_POINTER_DECL :
            {
                set_pointer_type(declarator_type, ASTSon1(a), declarator_context);
                build_scope_declarator_rec(ASTSon2(a), declarator_type, 
                        gather_info, declarator_context, entity_context, prototype_context);
                break;
            }
            // attribute abstract-declarator
        case AST_GCC_ABSTRACT_DECLARATOR :
            {
                build_scope_declarator_rec(ASTSon1(a), declarator_type, 
                        gather_info, declarator_context, entity_context, prototype_context); 
                break;
            }
            // attribute * abstract-declarator
            // attribute & abstract-declarator
        case AST_GCC_PTR_ABSTRACT_DECLARATOR :
            {
                set_pointer_type(declarator_type, ASTSon1(a), declarator_context);
                if (ASTSon2(a) != NULL)
                {
                    build_scope_declarator_rec(ASTSon2(a), declarator_type, 
                            gather_info, declarator_context, entity_context, prototype_context);
                }
                break;
            }
            // functional-declarator attribute
        case AST_GCC_FUNCTIONAL_DECLARATOR :
            {
                build_scope_declarator_rec(ASTSon0(a), declarator_type,
                        gather_info, declarator_context, entity_context, prototype_context);
                break;
            }
        case AST_AMBIGUITY :
            {
                solve_ambiguous_declarator(a, declarator_context);
                // Restart function
                build_scope_declarator_rec(a, declarator_type, 
                        gather_info, declarator_context, entity_context, prototype_context);
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
                        case AST_QUALIFIED_ID :
                        case AST_SYMBOL :
                            return 1;
                        default :
                            return 0;
                    }
                }
            }
        case AST_POINTER_DECL :
        case AST_DECLARATOR_ARRAY :
            {
                return 0;
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
static scope_entry_t* build_scope_declarator_name(AST declarator_name, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, decl_context_t decl_context)
{
    declarator_name = get_declarator_id_expression(declarator_name, decl_context);

    if (declarator_name == NULL)
        return NULL;

    ERROR_CONDITION(ASTType(declarator_name) != AST_DECLARATOR_ID_EXPR,
            "Invalid node '%s'\n", ast_print_node_type(ASTType(declarator_name)));

    return build_scope_declarator_id_expr(declarator_name, declarator_type, gather_info, 
            decl_context);
}

/*
 * This function fills information for a declarator_id_expr. Actually only
 * unqualified names can be signed up since qualified names should have been
 * declared elsewhere.
 */
static scope_entry_t* build_scope_declarator_id_expr(AST declarator_name, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, decl_context_t decl_context)
{
    AST declarator_id = ASTSon0(declarator_name);

    decl_flags_t decl_flags = DF_NONE;
    if (BITMAP_TEST(decl_context.decl_flags, DF_FRIEND))
    {
        decl_flags |= DF_FRIEND;
    }

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
                return register_new_variable_name(destructor_id, declarator_type, gather_info, decl_context);
                break;
            }
        case AST_TEMPLATE_ID :
            {
                // This can only happen in an explicit template function instantiation.
                // WARNING_MESSAGE("Template id not supported. Skipping it", 0);
                scope_entry_list_t* entry_list = query_nested_name_flags(decl_context, 
                        NULL,
                        NULL,
                        declarator_id, decl_flags);

                ERROR_CONDITION((entry_list == NULL), "Template-id not found", 0);
                return entry_list->entry;
                break;
            }
        case AST_OPERATOR_FUNCTION_ID :
        case AST_OPERATOR_FUNCTION_ID_TEMPLATE :
            {
                // An unqualified operator_function_id "operator +"
                char* operator_function_name = get_operator_function_name(declarator_id);
                AST operator_id = ASTLeaf(AST_SYMBOL, ASTLine(declarator_id), operator_function_name);

                if (ASTType(declarator_id) == AST_OPERATOR_FUNCTION_ID_TEMPLATE)
                {
                    solve_possibly_ambiguous_template_id(declarator_id, decl_context);
                }

                return register_new_variable_name(operator_id, declarator_type, gather_info, decl_context);
                break;
            }
        case AST_CONVERSION_FUNCTION_ID :
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Registering a conversion function in %s\n", node_information(declarator_id));
                }
                // Ok, according to the standard, this function returns the
                // type defined in the conversion function id
                type_t* conversion_type_info = NULL;

                // Get the type and its name
                char* conversion_function_name = get_conversion_function_name(decl_context, declarator_id, 
                        &conversion_type_info);
                AST conversion_id = ASTLeaf(AST_SYMBOL, ASTLine(declarator_id), conversion_function_name);
                return register_new_variable_name(conversion_id, declarator_type, gather_info, decl_context);
                break;
            }
        // Qualified ones
        case AST_QUALIFIED_ID :
            {
                // A qualified id "a::b::c"
                if (!is_function_type(declarator_type))
                {
                    scope_entry_list_t* entry_list = query_nested_name_flags(decl_context,
                            ASTSon0(declarator_id),
                            ASTSon1(declarator_id),
                            ASTSon2(declarator_id),
                            decl_flags);

                    ERROR_CONDITION((entry_list == NULL), "Qualified id '%s' name not found (%s)", 
                            prettyprint_in_buffer(declarator_id), node_information(declarator_id));

                    return entry_list->entry;
                }
                else
                {
                    char is_overload;
                    scope_entry_t* entry = find_function_declaration(declarator_id, declarator_type, &is_overload,
                            decl_context);
                    return entry;
                }
                break;
            }
        case AST_QUALIFIED_TEMPLATE :
            {
                // A qualified template "a::b::template c<id>"
                return NULL;
                break;
            }
        case AST_QUALIFIED_OPERATOR_FUNCTION_ID :
            {
                // A qualified operator function_id "a::b::operator +"
                return NULL;
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
        {
            // Check that the symbol is eligible for "typedeffing"
            scope_entry_list_t* iter = list;
            while (iter != NULL)
            {
                scope_entry_t* entry = iter->entry;
                if (entry->kind != SK_ENUM 
                        && entry->kind != SK_CLASS)
                {
                    ERROR_CONDITION((entry == NULL), 
                            "Symbol '%s' in %s has been redeclared as a different symbol kind (look at %s:%d).", 
                            ASTText(declarator_id), 
                            node_information(declarator_id), 
                            entry->file,
                            entry->line);
                }
                iter = iter->next;
            }
        }
        scope_entry_t* entry = list->entry;

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
            ERROR_CONDITION((!equivalent_types(entry->type_information, declarator_type, CVE_CONSIDER,
                            decl_context)), 
                    "Symbol '%s' in line %s has been redeclared as a different symbol kind (look at %s:%d).", 
                    ASTText(declarator_id), 
                    node_information(declarator_id), 
                    entry->file,
                    entry->line);
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
    // Save aliased type under the type of this declaration
    entry->kind = SK_TYPEDEF;

    // We saved decl_context before, but now we are not
    entry->type_information = get_new_typedef(declarator_type);

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
            || BITMAP_TEST(decl_context.decl_flags, DF_PARAMETER_DECLARATION))
    {
        decl_flags_t decl_flags = DF_NONE;
        if (BITMAP_TEST(decl_context.decl_flags, DF_FRIEND))
        {
            decl_flags |= DF_FRIEND;
        }
        // Check for existence of this symbol in this scope
        scope_entry_list_t* entry_list = query_in_scope_flags(decl_context, declarator_id, decl_flags);

        scope_entry_list_t* check_list = filter_symbol_kind(entry_list, SK_VARIABLE);

        if (check_list != NULL)
        {
            scope_entry_t* entry = check_list->entry;
            
            // Always use the latest one, unfortunately a variable can be
            // declared several times by means of "extern" keyword
            entry->point_of_declaration = get_enclosing_declaration(declarator_id);
            return entry;
        }

        enum cxx_symbol_kind valid_symbols[2] = {SK_CLASS, SK_ENUM};
        check_list = filter_symbol_non_kind_set(entry_list, 2, valid_symbols);

        ERROR_CONDITION((check_list != NULL), "The symbol has already been defined as another symbol kind %d", check_list->entry->kind);

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
    scope_entry_t* entry;

    char is_overload;
    entry = find_function_declaration(declarator_id, declarator_type, &is_overload, decl_context);

    if (entry == NULL)
    {
        char* function_name = ASTText(declarator_id);

        if (BITMAP_TEST(decl_context.decl_flags, DF_CONSTRUCTOR))
        {
            function_name = strprepend(function_name, "constructor ");
        }
        scope_entry_t* new_entry = NULL;
        if (BITMAP_TEST(decl_context.decl_flags, DF_FRIEND))
        {
            // If friend, must be signed up in namespace scope
            new_entry = new_symbol(decl_context, decl_context.namespace_scope, function_name);
        }
        else
        {
            new_entry = new_symbol(decl_context, decl_context.current_scope, function_name);
        }

        new_entry->line = ASTLine(declarator_id);
        new_entry->file = ASTFileName(declarator_id);
        new_entry->point_of_declaration = get_enclosing_declaration(declarator_id);

        if (!BITMAP_TEST(decl_context.decl_flags, DF_TEMPLATE))
        {
            new_entry->kind = SK_FUNCTION;
        }
        else
        {
            new_entry->kind = SK_TEMPLATE_FUNCTION;
        }

        new_entry->type_information = declarator_type;

        DEBUG_CODE()
        {
            if (!is_overload)
            {
                fprintf(stderr, "Registering function '%s' %p in (%s)\n", 
                        function_name, 
                        new_entry,
                        node_information(declarator_id)
                        );
            }
            else
            {
                fprintf(stderr, "Registering overload function for '%s' %p in (%s)\n", 
                        function_name, 
                        new_entry,
                        node_information(declarator_id)
                        );
            }
        }

        return new_entry;
    }
    else
    {
        return entry;
    }
}

static scope_entry_t* find_function_declaration(AST declarator_id, type_t* declarator_type, 
        char* is_overload, decl_context_t decl_context)
{
    decl_flags_t decl_flags = DF_NONE;
    if (BITMAP_TEST(decl_context.decl_flags, DF_CONSTRUCTOR))
    {
        // Another trick would be creating a fake tree with an "A::constructor A" instead of "A::A"
        // (likewise for 'A' -> 'constructor A'
        decl_flags |= DF_CONSTRUCTOR;
    }
    if (BITMAP_TEST(decl_context.decl_flags, DF_FRIEND))
    {
        decl_flags |= DF_FRIEND;
    }
    else
    {
        // Restrict ourselves to the current scope
        // if the declarator-id is unqualified
        // and we are not naming a friend
        switch (ASTType(declarator_id))
        {
            case AST_SYMBOL :
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

    scope_entry_list_t* entry_list;
    entry_list = query_id_expression_flags(decl_context, declarator_id, decl_flags);

    type_t* function_being_declared = declarator_type;

    function_type_set_template_information(function_being_declared, decl_context.template_nesting,
            decl_context.num_template_parameters, decl_context.template_parameters);

    scope_entry_t* equal_entry = NULL;

    char found_equal = 0;
    *is_overload = 0;

    while (entry_list != NULL && !found_equal)
    {
        scope_entry_t* entry = entry_list->entry;

        if (entry->kind != SK_FUNCTION
                && entry->kind != SK_TEMPLATE_FUNCTION)
        {
            ERROR_CONDITION((entry->kind != SK_ENUM && entry->kind != SK_CLASS), 
                    "Symbol '%s' already declared as a different symbol type", 
                    prettyprint_in_buffer(declarator_id), 
                    entry->kind);

            entry_list = entry_list->next;
            continue;
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "Checking function declaration of '%s' in %s against the declaration in %s:%d\n",
                    prettyprint_in_buffer(declarator_id), 
                    node_information(declarator_id),
                    entry->file, 
                    entry->line);
        }

        type_t* current_function = entry->type_information;

        found_equal = !overloaded_function(function_being_declared, current_function, decl_context);
        if (found_equal)
        {
            equal_entry = entry;
        }
        else
        {
            CXX_LANGUAGE()
            {
                *is_overload = 1;
            }

            C_LANGUAGE()
            {
                if (!function_type_get_lacking_prototype(function_being_declared)
                         && !function_type_get_lacking_prototype(current_function))
                {
                    internal_error("Function '%s' has been declared with different prototype", 
                            ASTText(declarator_id));
                }
                equal_entry = entry;
                found_equal = 1;
            }
        }

        entry_list = entry_list->next;
    }

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
static void build_scope_linkage_specifier(AST a, decl_context_t decl_context)
{
    AST declaration_sequence = ASTSon1(a);

    if (declaration_sequence == NULL)
        return;

    char* previous_linkage = current_linkage;

    AST linkage_spec = ASTSon0(a);
    current_linkage = ASTText(linkage_spec);

    build_scope_declaration_sequence(declaration_sequence, decl_context);

    current_linkage = previous_linkage;
}

/*
 * Similar to build_scope_linkage_specifier but for just one declaration
 */
static void build_scope_linkage_specifier_declaration(AST a, decl_context_t decl_context)
{
    AST declaration = ASTSon1(a);

    char* previous_linkage = current_linkage;

    AST linkage_spec = ASTSon0(a);
    current_linkage = ASTText(linkage_spec);

    build_scope_declaration(declaration, decl_context);

    current_linkage = previous_linkage;
}

/*
 * This function registers a template declaration
 */
static void build_scope_template_declaration(AST a, AST top_template_decl, decl_context_t decl_context)
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
    decl_context_t template_context = new_template_context(decl_context);
    template_context.decl_flags |= DF_TEMPLATE;
    // A new level of template nesting
    template_context.template_nesting++;
    template_context.template_parameters = NULL;
    template_context.num_template_parameters = 0;

    build_scope_template_parameter_list(ASTSon0(a), &template_context.template_parameters, 
            &template_context.num_template_parameters, template_context);
    
    AST templated_decl = ASTSon1(a);
    if (ASTType(templated_decl) == AST_AMBIGUITY)
    {
        solve_ambiguous_declaration(templated_decl, template_context);
    }

    // Link the AST with the scope
    scope_link_set(CURRENT_COMPILED_FILE(scope_link), a, template_context);

    switch (ASTType(templated_decl))
    {
        case AST_FUNCTION_DEFINITION :
            {
                build_scope_template_function_definition(templated_decl, template_context);

                ASTAttrSetValueType(templated_decl, LANG_IS_TEMPLATED_FUNCTION_DEFINITION, tl_type_t, tl_bool(1));
                break;
            }
        case AST_SIMPLE_DECLARATION :
            {
                build_scope_template_simple_declaration(templated_decl, template_context);

                ASTAttrSetValueType(templated_decl, LANG_IS_TEMPLATED_DECLARATION, tl_type_t, tl_bool(1));
                break;
            }
        case AST_TEMPLATE_DECLARATION :
            {
                build_scope_template_declaration(templated_decl, top_template_decl, template_context);
                break;
            }
        default :
            internal_error("Unknown node type '%s' (line=%s)\n", ast_print_node_type(ASTType(templated_decl)), 
                    node_information(templated_decl));
    }

    ASTAttrSetValueType(ASTSon0(a), LANG_IS_TEMPLATE_HEADER, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(templated_decl, LANG_TEMPLATE_HEADER, tl_type_t, tl_ast(top_template_decl));
}

/*
 * This function registers an explicit template specialization
 */
static void build_scope_explicit_template_specialization(AST a, decl_context_t decl_context)
{
    decl_context_t template_context = new_template_context(decl_context);
    template_context.decl_flags |= DF_TEMPLATE;
    template_context.decl_flags |= DF_EXPLICIT_SPECIALIZATION;

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
                build_scope_template_function_definition(ASTSon0(a), template_context);
                break;
            }
        case AST_SIMPLE_DECLARATION :
            {
                build_scope_template_simple_declaration(ASTSon0(a), template_context);
                break;
            }
        case AST_EXPLICIT_SPECIALIZATION :
            {
                build_scope_explicit_template_specialization(ASTSon0(a), template_context);
                break;
            }
        default :
            {
                internal_error("Unknown node type '%s'\n", ast_print_node_type(ASTType(ASTSon0(a))));
            }
    }
}

static void build_scope_template_function_definition(AST a, decl_context_t decl_context)
{
    /* scope_entry_t* entry = */ build_scope_function_definition(a, decl_context);
}

static void build_scope_template_simple_declaration(AST a, decl_context_t decl_context)
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

    char is_constructor = 0;
    
    if (decl_specifier_seq != NULL)
    {
        // If a class specifier appears here it will be properly declarated in the scope (not within
        // in the template one)
        decl_context_t new_decl_context = decl_context;
        if (init_declarator_list == NULL)
        {
            new_decl_context.decl_flags |= DF_NO_DECLARATORS;
        }

        build_scope_decl_specifier_seq(decl_specifier_seq, &gather_info, &simple_type_info, new_decl_context);
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
                new_decl_context);
        build_scope_declarator_name(declarator, declarator_type, &gather_info, new_decl_context);
        
        // This is a simple declaration, thus if it does not declare an
        // extern variable or function, the symbol is already defined here
        if (!gather_info.is_extern
                && !is_function_type(declarator_type))
        {
            AST declarator_name = get_declarator_name(declarator, decl_context);
            scope_entry_list_t* entry_list = query_id_expression(decl_context, declarator_name);

            ERROR_CONDITION((entry_list == NULL), "Symbol just declared has not been found in the scope!", 0);

            // The last entry will hold our symbol, no need to look for it in the list
            ERROR_CONDITION((entry_list->entry->defined), "Symbol '%s' in '%s' has already been defined", 
                    prettyprint_in_buffer(declarator_name), node_information(declarator_name));

            DEBUG_CODE()
            {
                fprintf(stderr, "Defining symbol '%s'\n", prettyprint_in_buffer(declarator_name));
            }

            entry_list->entry->defined = 1;

            if (initializer != NULL)
            {
                ERROR_CONDITION(!check_for_initialization(initializer,
                            entry_list->entry->decl_context),
                        "Initializer '%s' in %s could not be disambiguated!\n",
                        prettyprint_in_buffer(initializer),
                        node_information(initializer));
                entry_list->entry->expression_value = initializer;
            }
        }
        else if (is_function_type(declarator_type))
        {
            // ???
        }
    }
}

/*
 * This function registers templates parameters in a given scope
 */
static void build_scope_template_parameter_list(AST a, 
        template_parameter_t*** template_parameters, int* num_parameters,
        decl_context_t template_context)
{
    AST iter;
    AST list = a;

    for_each_element(list, iter)
    {
        AST template_parameter = ASTSon1(iter);

        template_parameter_t* new_template_param = calloc(1, sizeof(*new_template_param));

        DEBUG_CODE()
        {
            fprintf(stderr, "New template parameter -> %p\n", new_template_param);
        }

        build_scope_template_parameter(template_parameter, new_template_param, *num_parameters, template_context);
        P_LIST_ADD(*template_parameters, *num_parameters, new_template_param);
    }
}

/*
 * This function registers one template parameter in a given scope
 */
static void build_scope_template_parameter(AST a, 
        template_parameter_t* template_parameters, int num_parameter,
        decl_context_t template_context)
{
    switch (ASTType(a))
    {
        case AST_GCC_PARAMETER_DECL :
        case AST_PARAMETER_DECL :
            build_scope_nontype_template_parameter(a, template_parameters, num_parameter, template_context);
            break;
        case AST_TYPE_PARAMETER_CLASS :
        case AST_TYPE_PARAMETER_TYPENAME :
            build_scope_type_template_parameter(a, template_parameters, num_parameter, template_context);
            break;
        case AST_TYPE_PARAMETER_TEMPLATE :
            build_scope_template_template_parameter(a, template_parameters, num_parameter, template_context);
            break;
        case AST_AMBIGUITY :
            // The ambiguity here is parameter_class vs parameter_decl
            solve_parameter_declaration_vs_type_parameter_class(a, template_context);
            // Restart this routine
            build_scope_template_parameter(a, template_parameters, num_parameter, template_context);
            break;
        default :
            internal_error("Unknown node type '%s'", ast_print_node_type(ASTType(a)));
    }
}

static void build_scope_template_template_parameter(AST a,
        template_parameter_t* template_parameters, int num_parameter,
        decl_context_t template_context)
{
    // These parameters have the form
    //
    //    TEMPLATE < template_param_list > CLASS [identifier] [= id_expr]
    //
    // "identifier" is then a template-name
    //
    // Construct parameter information
    decl_context_t template_params_context = new_template_context(template_context);

    build_scope_template_parameter_list(ASTSon0(a), &template_params_context.template_parameters,
            &template_params_context.num_template_parameters,
            template_params_context);

    if (ASTSon1(a) != NULL)
    {
        AST symbol = ASTSon1(a);
        char* name = ASTText(symbol);

        ASTAttrSetValueType(symbol, LANG_IS_TEMPLATE_PARAMETER, tl_type_t, tl_bool(1));

        // Note that we sign up the symbol in the template_scope !
        // This is a named type parameter. Register it in the symbol table
        DEBUG_CODE()
        {
            fprintf(stderr, "[%d] Registering template template-parameter '%s' in scope %p\n",
                    num_parameter,
                    name, 
                    template_context.template_scope);
        }
        scope_entry_t* new_entry = new_symbol(template_context, template_context.template_scope, name);
        new_entry->line = ASTLine(symbol);
        new_entry->file = ASTFileName(symbol);
        new_entry->point_of_declaration = symbol;

        new_entry->kind = SK_TEMPLATE_TEMPLATE_PARAMETER;

        new_entry->is_template_parameter = 1;
        new_entry->template_parameter_nesting = template_context.template_nesting;
        new_entry->template_parameter_position = num_parameter;

        new_entry->template_parameter_info = template_params_context.template_parameters;
        new_entry->num_template_parameters = template_params_context.num_template_parameters;

        template_parameters->entry = new_entry;
    }
    else
    {
        char* template_param_name = calloc(256, sizeof(char));

        sprintf(template_param_name, " <template-template-param-%d-%d> ", template_context.template_nesting, num_parameter+1);

        // Note that we sign up the symbol in the template_scope !
        scope_entry_t* new_entry = new_symbol(template_context, template_context.template_scope, template_param_name);
        new_entry->line = ASTLine(a);
        new_entry->file = ASTFileName(a);
        new_entry->point_of_declaration = a;

        new_entry->kind = SK_TEMPLATE_TEMPLATE_PARAMETER;

        new_entry->is_template_parameter = 1;
        new_entry->template_parameter_nesting = template_context.template_nesting;
        new_entry->template_parameter_position = num_parameter;

        new_entry->template_parameter_info = template_params_context.template_parameters;
        new_entry->num_template_parameters = template_params_context.num_template_parameters;

        template_parameters->entry = new_entry;
    }

    AST id_expr = ASTSon2(a);
    if (id_expr != NULL)
    {
        // This might be ambiguous
        // solve_possibly_ambiguous_expression(id_expr, template_context);

        // scope_entry_list_t* entry_list = query_id_expression(template_context, id_expr);

        // ERROR_CONDITION((entry_list == NULL), "Default argument expression id not found\n", 0);

        // enum cxx_symbol_kind valid_templates_arguments[2] = 
        // { 
        //     SK_TEMPLATE_PRIMARY_CLASS, 
        //     SK_TEMPLATE_TEMPLATE_PARAMETER,
        // };
        // // specializations are not necessary here
        // entry_list = filter_symbol_kind_set(entry_list, 2, valid_templates_arguments);

        // ERROR_CONDITION((entry_list == NULL), "No primary template or template template parameter found", 0);

        // Even if we looked up something, just save the tree
        template_parameters->default_argument = id_expr;
        template_parameters->default_argument_context = template_context;
        template_parameters->has_default_argument = 1;
    }

    template_parameters->kind = TPK_TEMPLATE;
}

static void build_scope_type_template_parameter(AST a,
        template_parameter_t* template_parameters, int num_parameter,
        decl_context_t template_context)
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
    
    if (name != NULL)
    {
        // This is a named type parameter. Register it in the symbol table
        DEBUG_CODE()
        {
            fprintf(stderr, "[%d] Registering type template-parameter '%s' in scope %p\n",
                    num_parameter,
                    ASTText(name), 
                    template_context.template_scope);
        }
        // Note that we sign it in the template_scope !
        scope_entry_t* new_entry = new_symbol(template_context, template_context.template_scope, ASTText(name));
        new_entry->line = ASTLine(name);
        new_entry->file = ASTFileName(name);
        new_entry->point_of_declaration = name;
        new_entry->kind = SK_TEMPLATE_TYPE_PARAMETER;

        new_entry->is_template_parameter = 1;
        new_entry->template_parameter_nesting = template_context.template_nesting;
        new_entry->template_parameter_position = num_parameter;

        ASTAttrSetValueType(name, LANG_IS_TEMPLATE_PARAMETER, tl_type_t, tl_bool(1));

        template_parameters->entry = new_entry;
    }
    else
    {
        char* template_param_name = calloc(256, sizeof(char));

        sprintf(template_param_name, " <type-template-param-%d-%d> ", template_context.template_nesting, num_parameter+1);

        // Note that we sign it in the template_scope !
        scope_entry_t* new_entry = new_symbol(template_context, template_context.template_scope, template_param_name);
        new_entry->line = ASTLine(a);
        new_entry->file = ASTFileName(a);
        new_entry->point_of_declaration = a;
        new_entry->kind = SK_TEMPLATE_TYPE_PARAMETER;

        new_entry->is_template_parameter = 1;
        new_entry->template_parameter_nesting = template_context.template_nesting;
        new_entry->template_parameter_position = num_parameter;

        template_parameters->entry = new_entry;
    }

    if (type_id != NULL)
    {
        // This might be ambiguous, disambiguate
        AST type_specifier_seq = ASTSon0(type_id);
        AST abstract_decl = ASTSon1(type_id);

        type_t *type_info = NULL;

        gather_decl_spec_t gather_info;
        memset(&gather_info, 0, sizeof(gather_info));

        build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &type_info, template_context);

        type_t* declarator_type = type_info;
        if (abstract_decl != NULL)
        {
            compute_declarator_type(abstract_decl, 
                    &gather_info, type_info, &declarator_type,
                    template_context);
        }

        template_parameters->default_argument = type_id;
        template_parameters->default_argument_context = template_context;
        template_parameters->has_default_argument = 1;
    }

    template_parameters->kind = TPK_TYPE;
}

static void build_scope_nontype_template_parameter(AST a,
        template_parameter_t* template_parameters, int num_parameter,
        decl_context_t template_context)
{
    // As usual there are three parts
    //     decl_specifier_seq [declarator] [ = expression ]
    // AST decl_specifier_seq = ASTSon0(a);
    AST parameter_declarator = ASTSon1(a);
    AST default_expression = ASTSon2(a);

    decl_context_t adjusted_template_context = template_context;
    adjusted_template_context.current_scope = adjusted_template_context.template_scope;

    scope_entry_t* entry = NULL;
    if (parameter_declarator != NULL)
    {
        AST declarator_name = get_declarator_name(parameter_declarator, template_context);

        if (declarator_name != NULL)
        {
            char *declarator_name_str = prettyprint_in_buffer(declarator_name);
            DEBUG_CODE()
            {
                fprintf(stderr, "[%d] Remembering '%s' as a non-type template parameter in %p\n", 
                        num_parameter,
                        declarator_name_str, 
                        template_context.template_scope);
            }
            // This is not a variable, but a template parameter
            entry = new_symbol(adjusted_template_context, adjusted_template_context.current_scope, declarator_name_str);
            entry->kind = SK_TEMPLATE_PARAMETER;

            entry->is_template_parameter = 1;
            entry->template_parameter_nesting = template_context.template_nesting;
            entry->template_parameter_position = num_parameter;
        }
        else
        {
            char* template_param_name = calloc(256, sizeof(char));

            sprintf(template_param_name, " <nontype-template-param-%d-%d> ", template_context.template_nesting, num_parameter+1);

            entry = new_symbol(adjusted_template_context, adjusted_template_context.current_scope, template_param_name);

            entry->kind = SK_TEMPLATE_PARAMETER;

            entry->is_template_parameter = 1;
            entry->template_parameter_nesting = template_context.template_nesting;
            entry->template_parameter_position = num_parameter;
        }
        // Save its symbol
        template_parameters->entry = entry;

        ASTAttrSetValueType(declarator_name, LANG_IS_TEMPLATE_PARAMETER, tl_type_t, tl_bool(1));
    }
    else
    {
        char* template_param_name = calloc(256, sizeof(char));

        sprintf(template_param_name, " <nontype-template-param-%d-%d> ", template_context.template_nesting, num_parameter+1);

        scope_entry_t* entry = 
            new_symbol(adjusted_template_context, adjusted_template_context.current_scope, template_param_name);

        entry->kind = SK_TEMPLATE_PARAMETER;

        entry->is_template_parameter = 1;
        entry->template_parameter_nesting = template_context.template_nesting;
        entry->template_parameter_position = num_parameter;

        // Save its symbol
        template_parameters->entry = entry;
    }

    if (default_expression != NULL)
    {
        solve_possibly_ambiguous_expression(default_expression, template_context);

        template_parameters->default_argument_context = template_context;
        template_parameters->default_argument = default_expression;
        template_parameters->has_default_argument = 1;

    }

    // Save the typename
    template_parameters->nontype_param_typename_context = template_context;
    template_parameters->nontype_param_typename = a;
    template_parameters->kind = TPK_NONTYPE;
}

static void build_scope_namespace_alias(AST a, decl_context_t decl_context)
{
    AST alias_ident = ASTSon0(a);
    AST qualified_name = ASTSon1(a);

    AST global_op = ASTSon0(qualified_name);
    AST nested_name_spec = ASTSon1(qualified_name);
    AST name = ASTSon2(qualified_name);

    scope_entry_list_t* entry_list = query_nested_name(decl_context, global_op, nested_name_spec, 
            name);

    ERROR_CONDITION((entry_list == NULL), "Namespace not found\n", 0);

    scope_entry_t* entry = entry_list->entry;
    
    ERROR_CONDITION((entry->kind != SK_NAMESPACE), "The referred symbol is not a namespace\n", 0);

    ERROR_CONDITION((decl_context.current_scope->kind != NAMESPACE_SCOPE),
            "Current scope is not namespace scope", 0);

    char* alias_name = ASTText(alias_ident);

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
static void build_scope_namespace_definition(AST a, decl_context_t decl_context)
{
    AST namespace_name = ASTSon0(a);

    if (namespace_name != NULL)
    {
        ERROR_CONDITION((decl_context.current_scope->kind != NAMESPACE_SCOPE), 
                "Incorrect scope, it should be a namespace scope", 0);

        // Register this namespace if it does not exist in this scope
        scope_entry_list_t* list = query_in_scope_str(decl_context, ASTText(namespace_name));

        scope_entry_list_t* check_list = filter_symbol_non_kind(list, SK_NAMESPACE);
        ERROR_CONDITION((check_list != NULL), "Identifier '%s' has already been declared as another symbol kind\n", ASTText(namespace_name));

        scope_entry_t* entry;
        decl_context_t namespace_context;
        if (list != NULL && list->entry->kind == SK_NAMESPACE)
        {
            entry = list->entry;
            namespace_context = entry->related_decl_context;
        }
        else
        {
            // We register a symbol of type namespace and link to a newly created scope.
            char* qualification_name = prettyprint_in_buffer(namespace_name);

            namespace_context = new_namespace_context(decl_context, qualification_name);

            entry = new_symbol(decl_context, decl_context.current_scope, ASTText(namespace_name));
            entry->line = ASTLine(namespace_name);
            entry->file = ASTFileName(namespace_name);
            entry->point_of_declaration = namespace_name;
            entry->kind = SK_NAMESPACE;
            entry->related_decl_context = namespace_context;

            // Link the scope of this newly created namespace
            scope_link_set(CURRENT_COMPILED_FILE(scope_link), a, namespace_context);
        }

        if (ASTSon1(a) != NULL)
        {
            build_scope_declaration_sequence(ASTSon1(a), namespace_context);
        }
    }
    else
    {
        WARNING_MESSAGE("Unnamed namespace support is missing", 0);
        build_scope_declaration_sequence(ASTSon1(a), decl_context);
        // #warning Unnamed namespace support is missing
    }
}

static void build_scope_ctor_initializer(AST ctor_initializer, 
        scope_entry_t* function_entry, 
        decl_context_t block_context)
{
    ERROR_CONDITION(block_context.current_scope->kind != BLOCK_SCOPE,
            "Block scope is not valid", 0);
    // Get the class symbol
    char* constructor_name = function_entry->symbol_name;
    // Jump over "constructor " to get the real name of the class
    // since the constructor function is called 'constructor A'
    constructor_name += strlen("constructor ");

    scope_entry_list_t* class_entry_list = 
        query_in_scope_str(block_context, constructor_name);

    ERROR_CONDITION(class_entry_list == NULL,
            "Class of constructor '%s' not found!", function_entry->symbol_name);

    scope_entry_t* class_entry = class_entry_list->entry;

    ERROR_CONDITION(class_entry->kind != SK_CLASS
            && class_entry->kind != SK_TEMPLATE_PRIMARY_CLASS
            && class_entry->kind != SK_TEMPLATE_SPECIALIZED_CLASS,
            "Symbol '%s' is not a class", class_entry->symbol_name);

    DEBUG_CODE()
    {
        fprintf(stderr, "Class '%s' is symbol %p\n", constructor_name, class_entry);
    }

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
                    AST expression_list = ASTSon1(mem_initializer);

                    AST global_op = ASTSon0(mem_initializer_id);
                    AST nested_name_spec = ASTSon1(mem_initializer_id);
                    AST symbol = ASTSon2(mem_initializer_id);

                    scope_entry_list_t* result_list = NULL;
                    result_list = query_nested_name(block_context, global_op, nested_name_spec, symbol);

                    ERROR_CONDITION((result_list == NULL), "Initialized entity in constructor initializer not found (%s)", 
                            node_information(symbol));

                    scope_entry_t* entry = result_list->entry;
                    // This checking code is only partially correct
                    // and covers very obvious cases
                    if (entry->kind != SK_TEMPLATE_TYPE_PARAMETER
                            && entry->kind != SK_TEMPLATE_TEMPLATE_PARAMETER
                            && !is_dependent_type(entry->type_information, block_context))
                    {
                        if (entry->kind == SK_VARIABLE)
                        {
                            ERROR_CONDITION(!same_scope(entry->decl_context.current_scope, block_context.class_scope),
                                    "This symbol does not belong to this class (%s)", 
                                    node_information(symbol));
                        }
                        else if (entry->kind == SK_CLASS
                                || entry->kind == SK_TEMPLATE_PRIMARY_CLASS
                                || entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS
                                || entry->kind == SK_TYPEDEF)
                        {
                            // scope_entry_t* original_entry = entry;
                            entry = give_real_entry(entry);

                            // It must be a direct base class
                            char found = 0;
                            int i;
                            for (i = 0; i < class_type_get_num_bases(class_entry->type_information); i++)
                            {

                                if (class_type_get_base_num(class_entry->type_information, 
                                            i, /*is_virtual=*/NULL) == entry)
                                {
                                    found = 1;
                                    break;
                                }
                            }
                            ERROR_CONDITION(!found,
                                    "Symbol '%s' is not a direct base of this class (%s)", 
                                    prettyprint_in_buffer(mem_initializer_id),
                                    node_information(symbol));
                        }
                        else 
                        {
                            internal_error("Unexpected symbol '%s' of kind %d in %s", 
                                    prettyprint_in_buffer(mem_initializer_id),
                                    entry->kind,
                                    node_information(symbol));
                        }
                    }

                    if (expression_list != NULL)
                    {
                        if (ASTType(expression_list) == AST_AMBIGUITY)
                        {
                            solve_ambiguous_expression_list(expression_list, block_context);
                        }

                        AST iter;
                        for_each_element(expression_list, iter)
                        {
                            AST expression = ASTSon1(iter);

                            solve_possibly_ambiguous_expression(expression, block_context);
                        }
                    }
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

// This function is only intended for C99
void build_scope_kr_parameter_declaration(scope_entry_t* function_entry,
        AST kr_parameter_declaration, 
        AST kr_parameters,
        decl_context_t decl_context)
{
    CXX_LANGUAGE()
    {
        internal_error("This function is intended only for C99", 0);
    }
    AST kr_parameter_list = NULL;
    
    if (kr_parameters != NULL 
            && ASTSon0(kr_parameters) != NULL)
    {
        kr_parameter_list = ASTSon0(kr_parameters);
    }

    AST declaration_list = kr_parameter_declaration;
    AST iter;

    type_t* declarator_type = function_entry->type_information;

    // This can be empty, but undefined parameters must be signed up
    if (kr_parameter_declaration != NULL)
    {
        for_each_element(declaration_list, iter)
        {
            AST simple_decl = ASTSon1(iter);

            build_scope_simple_declaration(simple_decl, decl_context);
        }
    }

    // Now for every symbol in kr_parameter_list get its type and sign it up
    // in the parameters of the type
    //
    // Clear previous prototype
    function_type_set_no_parameters(declarator_type);

    if (kr_parameter_list != NULL)
    {
        for_each_element(kr_parameter_list, iter)
        {
            AST symbol = ASTSon1(iter);

            scope_entry_list_t* entry_list = query_in_scope_str(decl_context, ASTText(symbol));
            scope_entry_t* entry;

            if (entry_list == NULL)
            {
                fprintf(stderr, "Warning: Parameter '%s' of K&R-style function '%s' in '%s' not defined, defaulting to 'int'\n",
                        ASTText(symbol), function_entry->symbol_name, node_information(kr_parameters));

                scope_entry_t* new_entry = new_symbol(decl_context, decl_context.current_scope, ASTText(symbol));

                new_entry->kind = SK_VARIABLE;
                new_entry->line = ASTLine(symbol);
                new_entry->file = ASTFileName(symbol);
                new_entry->type_information = get_signed_int_type();

                entry = new_entry;
            }
            else
            {
                entry = entry_list->entry;
            }

            // FIXME - We have to adjust the types, like we did for prototyped ones.
            function_type_add_parameter(declarator_type, entry->type_information, entry->type_information, NULL, decl_context);
        }
    }

    // Should not lack prototype no more
    if (kr_parameter_list != NULL)
    {
        function_type_set_lacking_prototype(declarator_type, 0);
    }
}

/*
 * This function builds symbol table information for a function definition
 */
static scope_entry_t* build_scope_function_definition(AST a, decl_context_t decl_context)
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
    type_t* type_info = NULL;

    AST decl_spec_seq = ASTSon0(a);
    char is_constructor = 0;
    if (decl_spec_seq != NULL 
            && ((ASTType(decl_spec_seq) != AST_AMBIGUITY && ASTSon1(decl_spec_seq) != NULL)
             || (ASTType(decl_spec_seq) == AST_AMBIGUITY)))
    {
        build_scope_decl_specifier_seq(decl_spec_seq, &gather_info, &type_info, decl_context);
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
                    fprintf(stderr, "Warning: Function definition at '%s' does not have decl-specifier, assuming 'int'\n",
                            node_information(a));
                }
                else
                {
                    fprintf(stderr, "Warning: Function definition at '%s' does not have type-specifier, assuming 'int'\n",
                            node_information(a));
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

    if (gather_info.is_friend)
    {
        new_decl_context.decl_flags |= DF_FRIEND;
    }
    else
    {
        new_decl_context.decl_flags &= (~DF_FRIEND);
    }

    decl_context_t block_context = new_block_context(decl_context);
    
    // This does not modify block_context.current_scope, it simply adds a function_scope to the context
    block_context = new_function_context(block_context);

    // block-context will be updated for qualified-id to reflect the exact context
    build_scope_declarator_with_parameter_context(ASTSon1(a), &gather_info, type_info, &declarator_type, 
            new_decl_context, &block_context);
    entry = build_scope_declarator_name(ASTSon1(a), declarator_type, &gather_info, new_decl_context);

    ERROR_CONDITION((entry == NULL), "Function '%s' does not exist! %s", prettyprint_in_buffer(ASTSon1(a)), node_information(a));
    
    {
        // Function declaration name
        AST declarator_name = get_declarator_name(ASTSon1(a), decl_context);
        ASTAttrSetValueType(a, LANG_FUNCTION_NAME, tl_type_t, tl_ast(declarator_name));

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
                ASTAttrSetValueType(declarator_name, LANG_NESTED_NAME_SPECIFIER, tl_type_t, tl_ast(nested_name_spec));
            }

            ASTAttrSetValueType(declarator_name, LANG_UNQUALIFIED_ID, tl_type_t, tl_ast(unqualified_id));

            if (ASTType(unqualified_id) == AST_TEMPLATE_ID)
            {
                ASTAttrSetValueType(unqualified_id, LANG_IS_TEMPLATE_ID, tl_type_t, tl_bool(1));
                ASTAttrSetValueType(unqualified_id, LANG_TEMPLATE_NAME, tl_type_t, tl_ast(ASTSon0(unqualified_id)));
                ASTAttrSetValueType(unqualified_id, LANG_TEMPLATE_ARGS, tl_type_t, tl_ast(ASTSon1(unqualified_id)));
            }
        }
        else if (ASTType(declarator_name) == AST_SYMBOL)
        {
            ASTAttrSetValueType(declarator_name, LANG_IS_UNQUALIFIED_ID, tl_type_t, tl_bool(1));
            ASTAttrSetValueType(declarator_name, LANG_UNQUALIFIED_ID, tl_type_t, tl_ast(declarator_name));
        }
        else if (ASTType(declarator_name) == AST_TEMPLATE_ID)
        {
            ASTAttrSetValueType(declarator_name, LANG_IS_TEMPLATE_ID, tl_type_t, tl_bool(1));
            ASTAttrSetValueType(declarator_name, LANG_TEMPLATE_NAME, tl_type_t, tl_ast(ASTSon0(declarator_name)));
            ASTAttrSetValueType(declarator_name, LANG_TEMPLATE_ARGS, tl_type_t, tl_ast(ASTSon1(declarator_name)));
        }
    }

    // The scope seen by this function definition
    scope_link_set(CURRENT_COMPILED_FILE(scope_link), a, decl_context);

    ERROR_CONDITION((entry->kind != SK_FUNCTION && entry->kind != SK_TEMPLATE_FUNCTION), 
            "This is not a function!!!", 0);

    ASTAttrSetValueType(a, LANG_IS_DECLARATION, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_DECLARATION_SPECIFIERS, tl_type_t, tl_ast(ASTSon0(a)));
    ASTAttrSetValueType(a, LANG_FUNCTION_SYMBOL, tl_type_t, tl_symbol(entry));

    // Function_body
    AST function_body = ASTSon3(a);
    AST statement = ASTSon0(function_body);

    CXX_LANGUAGE()
    {
        AST ctor_initializer = ASTSon2(a);
        if (ctor_initializer != NULL)
        {
            build_scope_ctor_initializer(ctor_initializer, entry, block_context);
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
                    kr_parameter_list, block_context);
        }
    }

    if (entry->is_member)
    {
        // If is a member function sign up additional information
        if (!function_type_get_static(entry->type_information)
                && !function_type_get_is_constructor(entry->type_information))
        {
            // type_t* this_type = calloc(1, sizeof(*this_type));
            // this_type->kind = TK_POINTER;
            // this_type->pointer = calloc(1, sizeof(*(this_type->pointer)));
            // // This cannot be assigned, so it can be understood as "T* const this"
            // this_type->cv_qualifier = CV_CONST;
            // this_type->pointer->pointee = copy_type(entry->class_type);

            // // "this" pseudovariable has the same cv-qualification of this member
            // this_type->pointer->pointee->cv_qualifier = entry->type_information->cv_qualifier;
            
            // The class we belong to
            type_t* pointed_this = entry->class_type;
            // Qualify likewise the function
            pointed_this = get_cv_qualified_type(pointed_this, get_cv_qualifier(entry->type_information));

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

    // Fix inherited template context
    block_context.decl_flags &= ~DF_TEMPLATE;
    block_context.decl_flags &= ~DF_EXPLICIT_SPECIALIZATION;
    block_context.template_parameters = NULL;
    block_context.num_template_parameters = 0;

    if (ASTType(statement) == AST_COMPOUND_STATEMENT)
    {
        // We want to inherit the block context to this compound statement
        // so build_scope_statement cannot be used, because it would create
        // one for the compound statement
        AST list = ASTSon0(statement);
        if (list != NULL)
        {
            scope_link_set(CURRENT_COMPILED_FILE(scope_link), list, block_context);

            build_scope_statement_seq(list, block_context);
        }
    }
    else
    {
        // This only can be a try-except, but a normal context is created
        // for this one
        build_scope_statement(statement, block_context);
    }

    scope_link_set(CURRENT_COMPILED_FILE(scope_link), statement, block_context);

    ASTAttrSetValueType(a, LANG_IS_FUNCTION_DEFINITION, tl_type_t, tl_bool(1));
    {
        AST non_nested_declarator = advance_over_declarator_nests(ASTSon1(a), decl_context);
        ASTAttrSetValueType(a, LANG_FUNCTION_DECLARATOR, tl_type_t, tl_ast(non_nested_declarator));
    }
    ASTAttrSetValueType(a, LANG_FUNCTION_BODY, tl_type_t, tl_ast(statement));

    DEBUG_CODE()
    {
        fprintf(stderr, "Function '%s' is defined\n", entry->symbol_name);
    }

    entry->defined = 1;

    if (BITMAP_TEST(decl_context.decl_flags, DF_TEMPLATE))
    {
        function_type_set_template_body(entry->type_information, function_body);
        entry->defined = 1;
    }

    entry->related_decl_context = block_context;

    return entry;
}

static void build_scope_member_declaration(decl_context_t inner_decl_context,
        AST a, access_specifier_t current_access, type_t* class_info,
        int step)
{
    DEBUG_CODE()
    {
    fprintf(stderr, "==== Member declaration line: [%s] (step=%d) ====\n",
            node_information(a), step);
    }
    switch (ASTType(a))
    {
        case AST_MEMBER_DECLARATION :
            {
                if (step == 0)
                {
                    build_scope_simple_member_declaration(inner_decl_context, a, current_access, class_info);
                }
                break;
            }
        case AST_FUNCTION_DEFINITION :
            {
                build_scope_member_function_definition(inner_decl_context, a, current_access, class_info, step);
                break;
            }
        case AST_GCC_EXTENSION :
            {
                build_scope_member_declaration(inner_decl_context, ASTSon0(a), current_access, class_info, step);
                break;
            }
        case AST_TEMPLATE_DECLARATION :
            {
                build_scope_member_template_declaration(inner_decl_context, a, current_access, class_info, step);
                break;
            }
        case AST_USING_DECL :
            {
                build_scope_using_declaration(a, inner_decl_context);
                break;
            }
        case AST_AMBIGUITY :
            {
                solve_ambiguous_declaration(a, inner_decl_context);
                // Restart
                build_scope_member_declaration(inner_decl_context, a, current_access, class_info, step);
                break;
            }
        case AST_EMPTY_DECL :
            {
                break;
            }
        case AST_UNKNOWN_PRAGMA :
            {
                break;
            }
        default:
            {
                internal_error("Unsupported node '%s' (%s)\n", ast_print_node_type(ASTType(a)),
                        node_information(a));
                break;
            }
    }
}

/*
 * This function registers a member template declaration
 */
static void build_scope_member_template_declaration(decl_context_t decl_context, AST a, 
        access_specifier_t current_access, type_t* class_info, int step)
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
    decl_context_t template_context = new_template_context(decl_context);
    template_context.decl_flags |= DF_TEMPLATE;
    // A new level of template nesting
    template_context.template_nesting++;
    template_context.num_template_parameters = 0;
    template_context.template_parameters = NULL;

    build_scope_template_parameter_list(ASTSon0(a), &template_context.template_parameters, 
            &template_context.num_template_parameters, template_context);
    
    AST templated_decl = ASTSon1(a);
    if (ASTType(templated_decl) == AST_AMBIGUITY)
    {
        solve_ambiguous_declaration(templated_decl, template_context);
    }

    // Link the AST with the scope
    scope_link_set(CURRENT_COMPILED_FILE(scope_link), a, template_context);

    switch (ASTType(templated_decl))
    {
        case AST_FUNCTION_DEFINITION :
            {
                build_scope_member_template_function_definition(template_context, ASTSon1(a), current_access, class_info, step);
                ASTAttrSetValueType(ASTSon1(a), LANG_IS_TEMPLATED_FUNCTION_DEFINITION, tl_type_t, tl_bool(1));
            }
            break;
        case AST_SIMPLE_DECLARATION :
            {
                if (step == 0)
                {
                    build_scope_member_template_simple_declaration(template_context, ASTSon1(a), current_access, class_info);
                    ASTAttrSetValueType(ASTSon1(a), LANG_IS_TEMPLATED_DECLARATION, tl_type_t, tl_bool(1));
                }
                break;
            }
            //      I think this is not possible
#if 0
        case AST_TEMPLATE_DECLARATION :
            build_scope_member_template_declaration(ASTSon1(a), st, current_access, class_info, step, decl_context);
            break;
#endif
        default :
            internal_error("Unknown node type '%s'\n", ast_print_node_type(ASTType(a)));
    }

    ASTAttrSetValueType(ASTSon1(a), LANG_TEMPLATE_HEADER, tl_type_t, tl_ast(ASTSon0(a)));
    ASTAttrSetValueType(ASTSon0(a), LANG_IS_TEMPLATE_HEADER, tl_type_t, tl_bool(true));
}

static void build_scope_member_template_function_definition(decl_context_t decl_context,
        AST a, access_specifier_t current_access, type_t* class_info, int step)
{
    decl_context_t new_decl_context = decl_context;
    new_decl_context.decl_flags |= DF_TEMPLATE;

    // Define the function within st scope but being visible template_scope
    /* scope_entry_t* entry = */ build_scope_member_function_definition(new_decl_context, a, current_access, class_info, step);
}

static void build_scope_member_template_simple_declaration(decl_context_t decl_context,
        AST a, access_specifier_t current_access, type_t* class_info)
{
    decl_context_t new_decl_context = decl_context;
    new_decl_context.decl_flags |= DF_TEMPLATE;

    build_scope_simple_member_declaration(new_decl_context, a, current_access, class_info);
}

/*
 * This is a function definition inlined in a class
 */
static scope_entry_t* build_scope_member_function_definition(decl_context_t decl_context, AST a, 
        access_specifier_t current_access, type_t* class_info,
        int step)
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
    if (step == 0)
    {
        // Handle this as if it was a plain declaration
        // decl_specifier_seq [optional]
        // If there is no decl_specifier_seq this has to be a destructor, constructor or conversion function
        gather_decl_spec_t gather_info;
        memset(&gather_info, 0, sizeof(gather_info));
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
                    decl_context);

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

        if (gather_info.is_friend)
        {
            decl_context.decl_flags |= DF_FRIEND;
        }
        else
        {
            decl_context.decl_flags &= (~DF_FRIEND);
        }

        compute_declarator_type(ASTSon1(a), &gather_info, type_info, &declarator_type, decl_context);
        entry = build_scope_declarator_name(ASTSon1(a), declarator_type, &gather_info, decl_context);

        switch (ASTType(declarator_name))
        {
            case AST_SYMBOL :
                {
                    if (is_constructor)
                    {
                        // This is a constructor
                        class_type_add_constructor(get_actual_class_type(class_type), entry);
                        function_type_set_is_constructor(entry->type_information, 1);
                    }
                    break;
                }
                // This should not appear here
                // case AST_DESTRUCTOR_TEMPLATE_ID : 
            case AST_DESTRUCTOR_ID :
                {
                    class_type_set_destructor(get_actual_class_type(class_type), entry);
                    break;
                }
            case AST_OPERATOR_FUNCTION_ID :
                {
                    class_type_add_operator_function(get_actual_class_type(class_type), entry);
                    break;
                }
            case AST_CONVERSION_FUNCTION_ID :
                {
                    class_type_add_conversion_function(get_actual_class_type(class_type), entry);
                    break;
                }
            default :
                {
                    internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(declarator_name)));
                    break;
                }
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "Setting member function definition of '%s' as a member\n", entry->symbol_name); 
        }
        entry->is_member = 1;
        entry->class_type = class_info;
    }
    else
    {
        // Build the function definition
        entry = build_scope_function_definition(a, decl_context);
    }

    return entry;
}

/*
 * This is a member declaration inlined in a class, not a function definition
 */
static void build_scope_simple_member_declaration(decl_context_t decl_context, AST a, 
        access_specifier_t current_access, type_t* class_info)
{
    gather_decl_spec_t gather_info;

    memset(&gather_info, 0, sizeof(gather_info));

    type_t* class_type = NULL;
    char* class_name = "";
    if (is_named_type(class_info))
    {
        class_type = named_type_get_symbol(class_info)->type_information;
        class_name = named_type_get_symbol(class_info)->symbol_name;
    }
    else if (is_unnamed_class_type(class_info))
    {
        class_type = class_info;
        // class_name remains as empty 
    }
    else
    {
        internal_error("Type is not a class type", 0);
    }


    type_t* member_type = NULL;

    if (ASTSon0(a) != NULL)
    {
        decl_context_t new_decl_context = decl_context;
        if (ASTSon1(a) == NULL)
        {
            new_decl_context.decl_flags |= DF_NO_DECLARATORS;
        }
        else
        {
            new_decl_context.decl_flags &= (~DF_NO_DECLARATORS);
        }

        build_scope_decl_specifier_seq(ASTSon0(a), &gather_info,
                &member_type, new_decl_context);

        if (member_type != NULL
                && is_named_type(member_type)
                && (is_enumerated_type(member_type)
                    || is_class_type(member_type)))
                
        {
            scope_entry_t* type_symbol = named_type_get_symbol(member_type);
            if (same_scope(type_symbol->decl_context.current_scope,
                        decl_context.current_scope))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Setting type '%s' as member\n", 
                            type_symbol->symbol_name);
                }

                type_symbol->is_member = 1;
                type_symbol->class_type = class_info;
            }
        }

    }

    if (ASTSon1(a) != NULL)
    {
        ASTAttrSetValueType(a, LANG_IS_DECLARATION, tl_type_t, tl_bool(1));
        ASTAttrSetValueType(a, LANG_DECLARATION_SPECIFIERS, tl_type_t, tl_ast(ASTSon0(a)));
        ASTAttrSetValueType(a, LANG_DECLARATION_DECLARATORS, tl_type_t, tl_ast(ASTSon1(a)));

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
                        build_scope_simple_member_declaration(decl_context, a, current_access, class_info);
                        return;
                        break;
                    }
                case AST_GCC_BITFIELD_DECLARATOR :
                case AST_BITFIELD_DECLARATOR :
                    {
                        AST identifier = ASTSon0(declarator);
                        if (identifier != NULL)
                        {
                            type_t* declarator_type = NULL;
                            compute_declarator_type(identifier, &gather_info, 
                                     member_type, &declarator_type, 
                                     decl_context);
                            build_scope_declarator_name(identifier, declarator_type, &gather_info, decl_context);
                        }

                        AST expression = ASTSon1(declarator);
                        solve_possibly_ambiguous_expression(expression, decl_context);
                        break;
                    }
                    // init declarator may appear here because of templates
                case AST_INIT_DECLARATOR :
                case AST_MEMBER_DECLARATOR :
                case AST_GCC_MEMBER_DECLARATOR :
                    {
                        AST declarator_name = get_declarator_name(declarator, decl_context);
                        AST initializer = ASTSon1(declarator);

                        {
                            AST non_nested_declarator = advance_over_declarator_nests(declarator, decl_context);
                            ASTAttrSetValueType(non_nested_declarator, LANG_IS_DECLARED_NAME, tl_type_t, tl_bool(1));
                            ASTAttrSetValueType(non_nested_declarator, LANG_DECLARED_NAME, tl_type_t, tl_ast(declarator_name));
                            ASTAttrSetValueType(non_nested_declarator, LANG_INITIALIZER, tl_type_t, tl_ast(initializer));
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

                        if (gather_info.is_friend)
                        {
                            new_decl_context.decl_flags |= DF_FRIEND;
                        }
                        else
                        {
                            new_decl_context.decl_flags &= (~DF_FRIEND);

                            AST declarator_name = get_declarator_name(declarator, decl_context);

                            if (ASTType(declarator_name) == AST_QUALIFIED_ID
                                || ASTType(declarator_name) == AST_QUALIFIED_TEMPLATE
                                || ASTType(declarator_name) == AST_QUALIFIED_OPERATOR_FUNCTION_ID)
                            {
                                // Let's fix the tree for the user message
                                AST fixed_declarator = duplicate_ast(declarator);
                                AST fixed_declarator_name = get_declarator_name(fixed_declarator, decl_context);

                                AST fixed_unqualified_id = ASTSon2(fixed_declarator_name);

                                *fixed_declarator_name = *fixed_unqualified_id;

                                running_error("Extra qualification of member declaration is not allowed: '%s' in '%s'. Did you mean '%s'?", 
                                        prettyprint_in_buffer(declarator),
                                        node_information(declarator_name),
                                        prettyprint_in_buffer(fixed_declarator)
                                        );
                            }
                        }

                        type_t* declarator_type = NULL;

                        compute_declarator_type(ASTSon0(declarator), &gather_info, 
                                member_type, &declarator_type, 
                                new_decl_context);
                        scope_entry_t *entry =
                            build_scope_declarator_name(ASTSon0(declarator),
                                    declarator_type, &gather_info,
                                    new_decl_context);


                        DEBUG_CODE()
                        {
                            fprintf(stderr, "Setting symbol '%s' as a member of class '%s'\n", entry->symbol_name, 
                                    class_name);
                        }
                        entry->is_member = 1;
                        entry->class_type = class_info;

                        // Function declarations (these are not members)
                        if (entry->kind == SK_FUNCTION
                                || entry->kind == SK_TEMPLATE_FUNCTION)
                        {
                            // Update information in the class about this member function
                            switch (ASTType(declarator_name))
                            {
                                case AST_SYMBOL :
                                    {
                                        if (is_constructor)
                                        {
                                            // This is a constructor
                                            class_type_add_constructor(class_type, entry);
                                            function_type_set_is_constructor(entry->type_information, 1);
                                        }
                                        break;
                                    }
                                case AST_DESTRUCTOR_TEMPLATE_ID : // This can appear here
                                case AST_DESTRUCTOR_ID :
                                    {
                                        // This is the destructor
                                        class_type_set_destructor(get_actual_class_type(class_type), entry);
                                        break;
                                    }
                                case AST_OPERATOR_FUNCTION_ID :
                                case AST_OPERATOR_FUNCTION_ID_TEMPLATE :
                                    {
                                        class_type_add_operator_function(get_actual_class_type(class_type), entry);
                                        break;
                                    }
                                case AST_CONVERSION_FUNCTION_ID :
                                    {
                                        class_type_add_conversion_function(get_actual_class_type(class_type), entry);
                                        break;
                                    }
                                case AST_QUALIFIED_ID :
                                    {
                                        // Do nothing with them
                                        // In particular if they come from a friend context
                                        ERROR_CONDITION((!gather_info.is_friend), "I was not expecting a qualified-id" 
                                                "in something that is not a friend declaration", 0);
                                        break;
                                    }
                                case AST_TEMPLATE_ID :
                                    {
                                        // Do nothing with this
                                        break;
                                    }
                                default :
                                    {
                                        internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(declarator_name)));
                                        break;
                                    }
                            }
                        }
                        else
                        {
                            if (!gather_info.is_static)
                            {
                                DEBUG_CODE()
                                {
                                    fprintf(stderr, "Registering a nonstatic data member '%s' of class '%s'\n",
                                            entry->symbol_name, class_name);
                                }
                                // This is a nonstatic data member
                                class_type_add_nonstatic_data_member(get_actual_class_type(class_type), entry);
                            }
                            else
                            {
                                DEBUG_CODE()
                                {
                                    fprintf(stderr, "Registering a static data member '%s' of class '%s'\n",
                                            entry->symbol_name, class_name);
                                }
                                // This is a static data member
                                class_type_add_static_data_member(get_actual_class_type(class_type), entry);
                            }
                            if (initializer != NULL)
                            {
                                ERROR_CONDITION(!check_for_initialization(initializer,
                                            entry->decl_context),
                                        "Initializer '%s' in %s could not be disambiguated!\n",
                                        prettyprint_in_buffer(initializer),
                                        node_information(initializer));
                                entry->expression_value = initializer;
                            }
                        }
                        break;
                    }
                default :
                    {
                        internal_error("Unhandled node '%s' (%s)", ast_print_node_type(ASTType(declarator)), node_information(declarator));
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
static void build_exception_spec(type_t* function_type, AST a, decl_context_t decl_context)
{
    // No exception specifier at all
    if (a == NULL)
        return;

    function_type_set_exception_spec(function_type);

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
        gather_decl_spec_t gather_info;
        memset(&gather_info, 0, sizeof(gather_info));
    
        build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &type_info,
                decl_context);

        type_t* declarator_type = type_info;
        if (abstract_decl != NULL)
        {
            compute_declarator_type(abstract_decl, &gather_info, type_info, &declarator_type,
                    decl_context);
        }
        function_type_add_exception_spec(function_type, declarator_type);
    }
}

// This function builds template arguments with no specialization,
// the idea is that a primary template like this one
//
// template <typename _T, typename _Q>
// struct A { };
//
// can be understood as if it was a specialization
//
//    template <typename _T, typename _Q>
//    struct A<_T, _Q> { };
//
// but the code above is not valid, as no specialization is actually done
static void build_scope_template_arguments_for_primary_template(decl_context_t decl_context,
        template_parameter_t** template_parameter_info, 
        int num_template_parameters, 
        template_argument_list_t** template_arguments)
{
    *template_arguments = calloc(sizeof(1), sizeof(*(*template_arguments)));
    (*template_arguments)->num_arguments = 0;

    int i;
    for (i = 0; i < num_template_parameters; i++)
    {
        template_parameter_t* template_parameter = template_parameter_info[i];

        if (template_parameter->entry->template_parameter_nesting != decl_context.template_nesting)
        {
            // This is a template parameter from an outer template declaration
            // ignore it
            DEBUG_CODE()
            {
                fprintf(stderr, "Ignoring template parameter '%s' coming from outer template declaration\n",
                        template_parameter->entry->symbol_name);
            }
            continue;
        }

        switch (template_parameter->kind)
        {
            case TPK_TYPE :
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Creating unspecialized template argument for type template parameter '%s'\n",
                                template_parameter->entry->symbol_name);
                    }
                    template_argument_t* new_template_argument = calloc(1, sizeof(*new_template_argument));

                    new_template_argument->kind = TAK_TYPE;
                    new_template_argument->type = get_user_defined_type(template_parameter->entry);

                    P_LIST_ADD((*template_arguments)->argument_list, (*template_arguments)->num_arguments, new_template_argument);
                    break;
                }
            case TPK_TEMPLATE :
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Creating unspecialized template argument for template template parameter '%s'\n",
                                template_parameter->entry->symbol_name);
                    }
                    template_argument_t* new_template_argument = calloc(1, sizeof(*new_template_argument));

                    new_template_argument->kind = TAK_TEMPLATE;
                    new_template_argument->type = get_user_defined_type(template_parameter->entry);

                    P_LIST_ADD((*template_arguments)->argument_list, (*template_arguments)->num_arguments, new_template_argument);

                    break;
                }
            case TPK_NONTYPE :
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Creating unspecialized template argument for nontype template parameter '%s'\n",
                                template_parameter->entry->symbol_name);
                    }

                    template_argument_t* new_template_argument = calloc(1, sizeof(*new_template_argument));

                    new_template_argument->kind = TAK_NONTYPE;
                    //
                    // We have to rebuild the actual type of this nontype template argument because
                    // we did not save it
                    type_t* declarator_type = NULL;
                    {
                        type_t* type_info = NULL;

                        DEBUG_CODE()
                        {
                            fprintf(stderr, "Rebuilding type for '%s'\n", 
                                    template_parameter->entry->symbol_name);
                        }

                        AST nontype_template_parameter_tree = 
                            template_parameter->nontype_param_typename;
                        AST decl_specifier_seq = ASTSon0(nontype_template_parameter_tree);
                        AST parameter_declarator = ASTSon1(nontype_template_parameter_tree);
                        // AST default_expression = ASTSon2(nontype_template_parameter_tree);

                        gather_decl_spec_t gather_info;
                        memset(&gather_info, 0, sizeof(gather_info));

                        // Create fake context to avoid polluting the current one
                        build_scope_decl_specifier_seq(decl_specifier_seq, &gather_info, &type_info, 
                                template_parameter->nontype_param_typename_context);

                        declarator_type = type_info;
                        if (parameter_declarator != NULL)
                        {
                            compute_declarator_type(parameter_declarator, 
                                    &gather_info, type_info, &declarator_type,
                                    template_parameter->nontype_param_typename_context);
                        }
                    }
                    new_template_argument->type = declarator_type;

                    // Fake an expression
                    new_template_argument->expression = ASTLeaf(AST_SYMBOL, 
                            template_parameter->entry->line,
                            template_parameter->entry->symbol_name);
                    new_template_argument->expression_context = decl_context;

                    P_LIST_ADD((*template_arguments)->argument_list, (*template_arguments)->num_arguments, new_template_argument);
                    break;
                }
            default :
                {
                    internal_error("Invalid template parameter kind %d\n", template_parameter->kind);
                }
        }
    }
}

// Replaces symbols of template parameters with template arguments seen so far
//
decl_context_t replace_template_parameters_with_template_arguments(
        template_argument_list_t* template_arguments,
        decl_context_t context_to_replace)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "Replacing template parameters up to parameter %d\n", template_arguments->num_arguments - 1);
    }
    
    // Replace types in the parameter scope
    decl_context_t fake_context = new_template_context(context_to_replace);

    // Now iterate in the template_scope of the current template parameter (_Q3
    // or _S2 in the example above). Here template_scope is used because it is the one
    Iterator* it = (Iterator*) hash_iterator_create(
            context_to_replace.template_scope->hash);
    for (iterator_first(it); !iterator_finished(it); iterator_next(it))
    {
        scope_entry_list_t* entry_list = (scope_entry_list_t*) iterator_item(it);

        scope_entry_t* entry = entry_list->entry;

        DEBUG_CODE()
        {
            fprintf(stderr, "Considering template parameter '%s'\n", entry->symbol_name);
        }

        if (entry->template_parameter_position >= template_arguments->num_arguments)
        {
            // Ignore it, it has not been binded yet
            continue;
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "Creating interception symbol '%s' in fake context\n", entry->symbol_name);
        }

        // Create the new symbol in the fake context (e.g entry->symbol_name can be "_T3", in the example above)
        scope_entry_t* new_entry = new_symbol(fake_context, fake_context.template_scope, entry->symbol_name);

        // type template parameter
        if (entry->kind == SK_TEMPLATE_TYPE_PARAMETER)
        {
            // Create a typedef, 
            new_entry->kind = SK_TYPEDEF;

            type_t* argument_type = 
                template_arguments->argument_list[entry->template_parameter_position]->type;

            new_entry->type_information = get_new_typedef(argument_type);

            DEBUG_CODE()
            {
                fprintf(stderr, "Making type template parameter '%s' of type '%s'\n", 
                        new_entry->symbol_name,
                        print_declarator(new_entry->type_information, new_entry->decl_context));
            }
        }
        // non-type template parameter
        else if (entry->kind == SK_TEMPLATE_PARAMETER)
        {
            // This is no more a SK_TEMPLATE_PARAMETER
            new_entry->kind = SK_VARIABLE;
            AST expression = duplicate_ast(
                    template_arguments->argument_list[entry->template_parameter_position]->expression);
            AST constant_initializer = ASTMake1(AST_CONSTANT_INITIALIZER, expression, ASTLine(expression), 
                    NULL);

            new_entry->expression_value = constant_initializer;

            new_entry->type_information = template_arguments->argument_list[entry->template_parameter_position]->type;

            DEBUG_CODE()
            {
                fprintf(stderr, "Making non-type template parameter '%s' to have value %s\n", 
                        new_entry->symbol_name,
                        prettyprint_in_buffer(constant_initializer));
            }
        }
        // template template parameter
        else if (entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
        {
            new_entry->kind = SK_TEMPLATE_ALIAS;
            type_t* template_argument_type = 
                template_arguments->argument_list[entry->template_parameter_position]->type;

            new_entry->template_alias_type = template_argument_type;

            DEBUG_CODE()
            {
                fprintf(stderr, "Making template template parameter '%s' to be an alias to %s\n", 
                        new_entry->symbol_name,
                        print_declarator(template_argument_type, new_entry->decl_context));
            }
        }
        else
        {
            internal_error("Unexpected symbol kind '%d'\n", entry->kind);
        }
    }

    return fake_context;
}

void build_scope_template_arguments(
        decl_context_t lookup_context, // The context of A::B
        decl_context_t argument_context, // The context of <T, e>
        AST class_head_id, 
        template_argument_list_t** template_arguments)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "Building scope information for '");
        prettyprint(stderr, class_head_id);
        fprintf(stderr, "' template\n");
    }

    AST list, iter;
    *template_arguments = calloc(sizeof(1), sizeof(*(*template_arguments)));

    (*template_arguments)->num_arguments = 0;

    int num_arguments = 0;

    list = ASTSon1(class_head_id);
    // Count the arguments of the template-id, these may be less than the
    // template parameters. So we will look for the primary template, which is
    // the only template definition that can be given default template
    // arguments for template parameters, specializations cannot (for our
    // mental health :)
    if (list != NULL)
    {
        for_each_element(list, iter)
        {
            num_arguments++;
        }
    }

    solve_possibly_ambiguous_template_id(class_head_id, argument_context);
    
    // In order to complete template arguments with default template arguments we need the
    // primary template. So, search it first.
    AST template_name = ASTSon0(class_head_id);

    DEBUG_CODE()
    {
        fprintf(stderr, "Looking for primary template '%s' in '%p'\n", ASTText(template_name), lookup_context.current_scope);
    }

    scope_entry_list_t* templates_list = query_unqualified_name_str(lookup_context, ASTText(template_name));

    // Sometimes we end with the injected symbol, that is fairly useless here
    // so readjust the lookup_context and lookup again
    if (templates_list != NULL
           && templates_list->entry->injected_class_name)
    {
        scope_entry_t* injected_symbol = templates_list->entry;
        lookup_context = injected_symbol->injected_class_referred_symbol->decl_context;
        templates_list = query_unqualified_name_str(lookup_context, ASTText(template_name));
    }
    
    enum cxx_symbol_kind filter_template_classes[4] = {
        SK_TEMPLATE_PRIMARY_CLASS, 
        SK_TEMPLATE_SPECIALIZED_CLASS, 
        SK_TEMPLATE_TEMPLATE_PARAMETER,
        SK_TEMPLATE_ALIAS
    };

    templates_list = filter_symbol_kind_set(templates_list, 4, filter_template_classes);

    enum cxx_symbol_kind filter_primary_classes[3] = {
        SK_TEMPLATE_PRIMARY_CLASS,
        SK_TEMPLATE_TEMPLATE_PARAMETER,
        SK_TEMPLATE_ALIAS
    };

    scope_entry_list_t* primary_template_list = filter_symbol_kind_set(templates_list, 3, filter_primary_classes);

    ERROR_CONDITION((primary_template_list == NULL), "Primary template for '%s' not found", ASTText(template_name));

    // If what is found is an alias, repeat the lookup with the aliased name
    // Template alias are template names that are linked to template template
    // arguments
    if (primary_template_list->entry->kind == SK_TEMPLATE_ALIAS)
    {
        type_t* alias_type_info = primary_template_list->entry->template_alias_type;

        if (!is_named_type(alias_type_info))
        {
            internal_error("Expected a template name type\n", 0);
        }

        primary_template_list = create_list_from_entry(named_type_get_symbol(alias_type_info));
    }

    scope_entry_t* primary_template = NULL;
    primary_template = primary_template_list->entry;

    list = ASTSon1(class_head_id);
    if (list != NULL)
    {
        // For every explicitly given template argument compute it
        for_each_element(list, iter)
        {
            AST template_argument = ASTSon1(iter);

            switch (ASTType(template_argument))
            {
                // A<int> - type template arguments
                // B<A> - template template arguments
                case AST_TEMPLATE_TYPE_ARGUMENT:
                    {
                        if ((*template_arguments)->num_arguments >= 
                                primary_template->num_template_parameters)
                        {
                            // Do nothing, we may be checking for ambiguities
                            break;
                        }
                        template_argument_t* new_template_argument = calloc(1, sizeof(*new_template_argument));

                        template_parameter_t* curr_template_parameter = 
                            primary_template->template_parameter_info[(*template_arguments)->num_arguments];

                        if (curr_template_parameter->kind == TPK_TEMPLATE)
                        {
                            new_template_argument->kind = TAK_TEMPLATE;
                        }
                        else if (curr_template_parameter->kind == TPK_TYPE)
                        {
                            new_template_argument->kind = TAK_TYPE;
                        }
                        else if (curr_template_parameter->kind == TPK_NONTYPE)
                        {
                            // Some ambiguity is sliping here
                            break;
                        }
                        else
                        {
                            internal_error("Unknown template parameter kind=%d\n", 
                                    curr_template_parameter->kind);
                        }
                        // Create the type_spec
                        // A type_id is a type_specifier_seq followed by an optional abstract
                        // declarator
                        AST type_template_argument = ASTSon0(template_argument);
                        AST type_specifier_seq = ASTSon0(type_template_argument);
                        AST abstract_decl = ASTSon1(type_template_argument);

                        // A type_specifier_seq is essentially a subset of a
                        // declarator_specifier_seq so we can reuse existing functions
                        type_t* type_info;
                        gather_decl_spec_t gather_info;
                        memset(&gather_info, 0, sizeof(gather_info));

                        build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &type_info,
                                argument_context);

                        type_t* declarator_type;
                        if (abstract_decl != NULL)
                        {
                            compute_declarator_type(abstract_decl, &gather_info, type_info, &declarator_type,
                                    argument_context);
                        }
                        else
                        {
                            declarator_type = type_info;
                        }

                        new_template_argument->type = declarator_type;

                        // Finally add to the template argument list
                        P_LIST_ADD((*template_arguments)->argument_list, (*template_arguments)->num_arguments, new_template_argument);
                        break;
                    }
                    // A<3> nontype template arguments
                case AST_TEMPLATE_EXPRESSION_ARGUMENT :
                    {
                        if ((*template_arguments)->num_arguments >= 
                                primary_template->num_template_parameters)
                        {
                            // Do nothing, we may be checking for ambiguities
                            break;
                        }

                        template_parameter_t* curr_template_parameter = 
                            primary_template->template_parameter_info[(*template_arguments)->num_arguments];

                        // We have to rebuild the actual type of this nontype template argument because
                        // we did not save it
                        // Because of cases like these
                        //
                        // template <typename _T, _T _N>
                        // struct A
                        // {
                        // };
                        //
                        // A<int, 3> b;
                        //
                        // template <typename _T, typename _T::M _N>
                        // struct B
                        // {
                        // };
                        //
                        // struct C { typedef int M; };
                        //
                        // B<C, 5> c;
                        //
                        //
                        type_t* declarator_type = NULL;
                        {
                            DEBUG_CODE()
                            {
                                fprintf(stderr, "Rebuilding type for nontype template argument '%s'\n", 
                                        curr_template_parameter->entry->symbol_name);
                            }

                            type_t* type_info = NULL;
                            AST nontype_template_parameter_tree = 
                                curr_template_parameter->nontype_param_typename;
                            AST decl_specifier_seq = ASTSon0(nontype_template_parameter_tree);
                            AST parameter_declarator = ASTSon1(nontype_template_parameter_tree);
                            // AST default_expression = ASTSon2(nontype_template_parameter_tree);

                            gather_decl_spec_t gather_info;
                            memset(&gather_info, 0, sizeof(gather_info));

                            // Create fake context to avoid polluting the current one
                            decl_context_t fake_context = 
                                replace_template_parameters_with_template_arguments(
                                        (*template_arguments),
                                        curr_template_parameter->nontype_param_typename_context
                                        );
                            build_scope_decl_specifier_seq(decl_specifier_seq, &gather_info, &type_info, fake_context);

                            declarator_type = type_info;
                            if (parameter_declarator != NULL)
                            {
                                compute_declarator_type(parameter_declarator, 
                                        &gather_info, type_info, &declarator_type,
                                        fake_context);
                            }
                        }

                        // This expression is of limited nature
                        template_argument_t* new_template_argument = calloc(1, sizeof(*new_template_argument));
                        new_template_argument->kind = TAK_NONTYPE;

                        AST expr_template_argument = ASTSon0(template_argument);

                        new_template_argument->type = declarator_type;
                        new_template_argument->expression = expr_template_argument;
                        new_template_argument->expression_context = argument_context;

                        // Finally add to the template argument list
                        P_LIST_ADD((*template_arguments)->argument_list, (*template_arguments)->num_arguments, new_template_argument);

                        break;
                    }
                case AST_AMBIGUITY :
                    {
                        internal_error("Ambiguous node\n", 0);
                        break;
                    }
                default :
                    internal_error("Unexpected node '%s' (%s)\n", ast_print_node_type(ASTType(template_argument)),
                            node_information(template_argument));
                    break;
            }
        }
    }

    // Check number of parameter templates
    {
        int mandatory_num_arguments = 0;
        mandatory_num_arguments = primary_template->num_template_parameters;

        while (mandatory_num_arguments >= 1)
        {
            template_parameter_t* curr_template_parameter = 
                primary_template->template_parameter_info[mandatory_num_arguments - 1];

            if (curr_template_parameter->has_default_argument)
            {
                mandatory_num_arguments--;
            }
            else
            {
                break;
            }
        }

        if (num_arguments < mandatory_num_arguments)
        {
            running_error("The number of arguments required by the template '%s' is at least %d but only %d were given in %s",
                    prettyprint_in_buffer(class_head_id), mandatory_num_arguments, num_arguments, node_information(class_head_id));
        }
    }

    if (primary_template->num_template_parameters > num_arguments)
    {
        // We have to complete with default template arguments, as defined
        // in the primary template definition
        DEBUG_CODE()
        {
            fprintf(stderr, "Completing template arguments of '%s' with default arguments\n",
                   prettyprint_in_buffer(class_head_id));
        }

        int k;
        for (k = num_arguments; 
                k < (primary_template->num_template_parameters);
                k++)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Completing template argument #%d of '%s' with default arguments\n",
                        k, prettyprint_in_buffer(class_head_id));
            }

            template_parameter_t* curr_template_parameter = primary_template->template_parameter_info[k];
            template_argument_t* curr_template_arg = calloc(1, sizeof(*curr_template_arg));

            switch (curr_template_parameter->kind)
            {
                // Default argument for a type template parameter
                case TPK_TYPE :
                    {
                        // Update the type to cover this case
                        //
                        // template <typename _T, typename _Q = _T>
                        // struct A { };
                        type_t* updated_type = NULL;
                        // Build the whole type info in the faked context
                        {
                            AST type_id = curr_template_parameter->default_argument;

                            decl_context_t faked_context = replace_template_parameters_with_template_arguments(
                                    (*template_arguments),
                                    curr_template_parameter->default_argument_context);

                            AST type_specifier_seq = ASTSon0(type_id);
                            AST abstract_decl = ASTSon1(type_id);

                            type_t *type_info = NULL;

                            gather_decl_spec_t gather_info;
                            memset(&gather_info, 0, sizeof(gather_info));

                            build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &type_info, faked_context);

                            updated_type = type_info;
                            if (abstract_decl != NULL)
                            {
                                compute_declarator_type(abstract_decl, 
                                        &gather_info, type_info, &updated_type,
                                        faked_context);
                            }
                        }

                        DEBUG_CODE()
                        {
                            fprintf(stderr, "Completed template argument %d with type '%s'\n", k,
                                    print_declarator(updated_type, argument_context));
                        }

                        curr_template_arg->kind = TAK_TYPE;
                        curr_template_arg->type = updated_type;
                        break;
                    }
                // Default argument for a template template parameter,
                // this should be a simple name
                case TPK_TEMPLATE :
                    {
                        decl_context_t faked_context = replace_template_parameters_with_template_arguments(
                                (*template_arguments),
                                curr_template_parameter->default_argument_context);
                        
                        type_t* updated_type = NULL;
                        {
                            // Lookup the name
                            AST id_expr = curr_template_parameter->default_argument;

                            scope_entry_list_t* entry_list = query_id_expression(faked_context, id_expr);

                            ERROR_CONDITION((entry_list == NULL), "Default argument expression id not found\n", 0);

                            enum cxx_symbol_kind valid_templates_arguments[3] = 
                            { 
                                SK_TEMPLATE_PRIMARY_CLASS, 
                                SK_TEMPLATE_TEMPLATE_PARAMETER,
                                SK_TEMPLATE_ALIAS,
                            };
                            // specializations are not necessary here
                            entry_list = filter_symbol_kind_set(entry_list, 3, valid_templates_arguments);

                            ERROR_CONDITION((entry_list == NULL), "No primary template or template template parameter found", 0);

                            scope_entry_t* entry = entry_list->entry;

                            // Flatten the template alias to avoid chains of template-alias
                            if (entry->kind == SK_TEMPLATE_ALIAS)
                            {
                                type_t* template_alias_type = entry->template_alias_type;
                                // Check things are OK here
                                ERROR_CONDITION(!is_named_type(template_alias_type),
                                        "Template alias should be named entities", 0);

                                scope_entry_t* aliased_entry = named_type_get_symbol(template_alias_type);

                                ERROR_CONDITION( (aliased_entry->kind != SK_TEMPLATE_PRIMARY_CLASS
                                        && aliased_entry->kind != SK_TEMPLATE_TEMPLATE_PARAMETER),
                                        "This template alias is ill-formed", 0);

                                updated_type = entry->template_alias_type;
                            }
                            else
                            {
                                updated_type = get_user_defined_type(entry);
                            }

                        }

                        curr_template_arg->kind = TAK_TEMPLATE;
                        curr_template_arg->type = updated_type;
                        break;
                    }
                // Default argument for a nontype parameter
                case TPK_NONTYPE :
                    {
                        curr_template_arg->kind = TAK_NONTYPE;

                        // Update the type to cover this case
                        //
                        // template <typename _T, _T _N = 3>
                        // struct A { };
                        //
                        // It is rather odd but valid anyway
                        //
                        type_t* declarator_type = NULL;
                        {
                            DEBUG_CODE()
                            {
                                fprintf(stderr, "Rebuilding type for default nontype template argument '%s'\n", 
                                        curr_template_parameter->entry->symbol_name);
                            }

                            type_t* type_info = NULL;
                            AST nontype_template_parameter_tree = 
                                curr_template_parameter->nontype_param_typename;
                            AST decl_specifier_seq = ASTSon0(nontype_template_parameter_tree);
                            AST parameter_declarator = ASTSon1(nontype_template_parameter_tree);
                            // AST default_expression = ASTSon2(nontype_template_parameter_tree);

                            gather_decl_spec_t gather_info;
                            memset(&gather_info, 0, sizeof(gather_info));

                            // Create fake context to avoid polluting the current one
                            decl_context_t fake_context = 
                                replace_template_parameters_with_template_arguments(
                                        (*template_arguments),
                                        curr_template_parameter->nontype_param_typename_context
                                        );
                            build_scope_decl_specifier_seq(decl_specifier_seq, &gather_info, &type_info, fake_context);

                            declarator_type = type_info;
                            if (parameter_declarator != NULL)
                            {
                                compute_declarator_type(parameter_declarator, 
                                        &gather_info, type_info, &declarator_type,
                                        fake_context);
                            }
                        }
                        //     update_template_argument_type_rec(curr_template_parameter->entry->type_information,
                        //             (*template_arguments),
                        //             argument_context,
                        //             ASTLine(class_head_id),
                        //             ASTFileName(class_head_id));

                        // Create a fake context for this expression to cover this case
                        // template <int _N, int _M = _N + 1>
                        // struct A { };
                        //
                        curr_template_arg->expression = curr_template_parameter->default_argument;
                        curr_template_arg->expression_context = 
                            replace_template_parameters_with_template_arguments(
                                    (*template_arguments),
                                    curr_template_parameter->default_argument_context);

                        curr_template_arg->type = declarator_type;
                        break;
                    }
                default:
                    internal_error("Unknown template parameter kind %d\n", curr_template_parameter->kind);
            }

            // Was given implicitly
            curr_template_arg->implicit = 1;

            DEBUG_CODE()
            {
                // fprintf(stderr, "Adding '%s' as an implicit template argument of '%s'\n", 
                //         prettyprint_in_buffer(curr_template_arg->argument_tree),
                //         prettyprint_in_buffer(class_head_id));
            }
            // Finally add to the template argument list
            P_LIST_ADD((*template_arguments)->argument_list, (*template_arguments)->num_arguments, curr_template_arg);
        }
    }
}

// Gives a name to an operator
char* get_operator_function_name(AST declarator_id)
{
    ERROR_CONDITION((ASTType(declarator_id) != AST_OPERATOR_FUNCTION_ID
                && ASTType(declarator_id) != AST_OPERATOR_FUNCTION_ID_TEMPLATE), 
            "This node is not valid here '%s'", ast_print_node_type(ASTType(declarator_id)));

    AST operator  = ASTSon0(declarator_id);

    switch (ASTType(operator))
    {
        case AST_NEW_OPERATOR :
            return "operator new";
        case AST_DELETE_OPERATOR :
            return "operator delete";
        case AST_NEW_ARRAY_OPERATOR :
            return "operator new[]";
        case AST_DELETE_ARRAY_OPERATOR :
            return "operator delete[]";
        case AST_ADD_OPERATOR :
            return "operator +";
        case AST_MINUS_OPERATOR :
            return "operator -";
        case AST_MULT_OPERATOR :
            return "operator *";
        case AST_DIV_OPERATOR :
            return "operator /";
        case AST_MOD_OPERATOR :
            return "operator %";
        case AST_BITWISE_XOR_OPERATOR :
            return "operator ^";
        case AST_BITWISE_AND_OPERATOR :
            return "operator &";
        case AST_BITWISE_OR_OPERATOR :
            return "operator |";
        case AST_BITWISE_NEG_OPERATOR :
            return "operator ~";
        case AST_LOGICAL_NOT_OPERATOR :
            return "operator !";
        case AST_ASSIGNMENT_OPERATOR :
            return "operator =";
        case AST_LOWER_OPERATOR :
            return "operator <";
        case AST_GREATER_OPERATOR :
            return "operator >";
        case AST_ADD_ASSIGN_OPERATOR :
            return "operator +=";
        case AST_SUB_ASSIGN_OPERATOR :
            return "operator -=";
        case AST_MUL_ASSIGN_OPERATOR :
            return "operator *=";
        case AST_DIV_ASSIGN_OPERATOR :
            return "operator /=";
        case AST_MOD_ASSIGN_OPERATOR :
            return "operator %=";
        case AST_XOR_ASSIGN_OPERATOR :
            return "operator ^=";
        case AST_AND_ASSIGN_OPERATOR :
            return "operator &=";
        case AST_OR_ASSIGN_OPERATOR :
            return "operator |=";
        case AST_LEFT_OPERATOR :
            return "operator <<";
        case AST_RIGHT_OPERATOR :
            return "operator >>";
        case AST_LEFT_ASSIGN_OPERATOR :
            return "operator <<=";
        case AST_RIGHT_ASSIGN_OPERATOR :
            return "operator >>=";
        case AST_EQUAL_OPERATOR :
            return "operator ==";
        case AST_DIFFERENT_OPERATOR :
            return "operator !=";
        case AST_LESS_OR_EQUAL_OPERATOR :
            return "operator <=";
        case AST_GREATER_OR_EQUAL_OPERATOR :
            return "operator >=";
        case AST_LOGICAL_AND_OPERATOR :
            return "operator &&";
        case AST_LOGICAL_OR_OPERATOR :
            return "operator ||";
        case AST_INCREMENT_OPERATOR :
            return "operator ++";
        case AST_DECREMENT_OPERATOR :
            return "operator --";
        case AST_COMMA_OPERATOR :
            return "operator ,";
        case AST_POINTER_OPERATOR :
            return "operator ->";
        case AST_POINTER_DERREF_OPERATOR :
            return "operator ->*";
        case AST_FUNCTION_CALL_OPERATOR :
            return "operator ()";
        case AST_SUBSCRIPT_OPERATOR :
            return "operator []";
        default :
            internal_error("Invalid node type '%s'\n", ast_print_node_type(ASTType(declarator_id)));
    }
}


/*
 * Building scope for statements
 */

typedef void (*stmt_scope_handler_t)(AST a, decl_context_t decl_context, char* attrib_to_set);
typedef 
struct stmt_scope_handler_map_tag
{
    void (*handler)(AST a, decl_context_t decl_context, char* attrib_to_set);
    char* attr_name;
} stmt_scope_handler_map_t;

static void build_scope_compound_statement(AST a, decl_context_t decl_context, char* attr_name)
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

    AST list = ASTSon0(a);
    if (list != NULL)
    {
        scope_link_set(CURRENT_COMPILED_FILE(scope_link), list, block_context);

        build_scope_statement_seq(list, block_context);
    }

    ASTAttrSetValueType(a, LANG_IS_COMPOUND_STATEMENT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_COMPOUND_STATEMENT_LIST, tl_type_t, tl_ast(ASTSon0(a)));
}

static void build_scope_condition(AST a, decl_context_t decl_context)
{
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
                decl_context);

        type_t* declarator_type = NULL;

        compute_declarator_type(declarator, &gather_info, type_info, &declarator_type,
                decl_context);
        scope_entry_t* entry = build_scope_declarator_name(declarator, declarator_type, &gather_info, decl_context);

        solve_possibly_ambiguous_expression(ASTSon2(a), decl_context);
        
        entry->expression_value = ASTSon2(a);
    }
    else
    {
        solve_possibly_ambiguous_expression(ASTSon2(a), decl_context);
        ASTAttrSetValueType(a, LANG_IS_EXPRESSION_NEST, tl_type_t, tl_bool(1));
        ASTAttrSetValueType(a, LANG_EXPRESSION_NESTED, tl_type_t, tl_ast(ASTSon2(a)));
    }
}

static void build_scope_while_statement(AST a, decl_context_t decl_context, char* attr_name)
{
    decl_context_t block_context = new_block_context(decl_context);

    build_scope_condition(ASTSon0(a), block_context);
    scope_link_set(CURRENT_COMPILED_FILE(scope_link), ASTSon0(a), block_context);

    if (ASTSon1(a) != NULL)
    {
        build_scope_statement(ASTSon1(a), block_context);
        scope_link_set(CURRENT_COMPILED_FILE(scope_link), ASTSon1(a), block_context);
    }

    ASTAttrSetValueType(a, LANG_IS_WHILE_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_ambiguity_handler(AST a, decl_context_t decl_context, 
        char* attrib_to_set)
{
    solve_ambiguous_statement(a, decl_context);
    // Restart
    build_scope_statement(a, decl_context);
}

static void build_scope_declaration_statement(AST a, decl_context_t decl_context, char* attr_name)
{
    AST declaration = ASTSon0(a);

    build_scope_declaration(declaration, decl_context);

    ASTAttrSetValueType(a, LANG_IS_DECLARATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void solve_expression_ambiguities(AST a, decl_context_t decl_context, char* attr_name)
{
    solve_possibly_ambiguous_expression(ASTSon0(a), decl_context);

    ASTAttrSetValueType(a, LANG_IS_EXPRESSION_NEST, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_EXPRESSION_NESTED, tl_type_t, tl_ast(ASTSon0(a)));
}

static void build_scope_if_else_statement(AST a, decl_context_t decl_context, char* attr_name)
{
    decl_context_t block_context = new_block_context(decl_context);

    AST condition = ASTSon0(a);
    build_scope_condition(condition, block_context);
    scope_link_set(CURRENT_COMPILED_FILE(scope_link), condition, block_context);

    AST then_branch = ASTSon1(a);
    build_scope_statement(then_branch, block_context);
    scope_link_set(CURRENT_COMPILED_FILE(scope_link), then_branch, block_context);

    AST else_branch = ASTSon2(a);
    if (else_branch != NULL)
    {
        build_scope_statement(else_branch, block_context);
        scope_link_set(CURRENT_COMPILED_FILE(scope_link), else_branch, block_context);
    }

    ASTAttrSetValueType(a, LANG_IS_IF_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_for_statement(AST a, decl_context_t decl_context, char* attr_name)
{
    AST for_init_statement = ASTSon0(a);
    AST condition = ASTSon1(a);
    AST expression = ASTSon2(a);
    AST statement = ASTSon3(a);

    if (ASTType(for_init_statement) == AST_AMBIGUITY)
    {
        solve_ambiguous_for_init_statement(for_init_statement, decl_context);
    }

    decl_context_t block_context = new_block_context(decl_context);

    if (ASTType(for_init_statement) == AST_SIMPLE_DECLARATION)
    {
        build_scope_simple_declaration(for_init_statement, block_context);
    }
    else if (ASTType(for_init_statement) == AST_EXPRESSION_STATEMENT)
    {
        solve_expression_ambiguities(for_init_statement, block_context, LANG_IS_EXPRESSION_STATEMENT);
    }
    scope_link_set(CURRENT_COMPILED_FILE(scope_link), for_init_statement, block_context);

    if (condition != NULL)
    {
        build_scope_condition(condition, block_context);
        scope_link_set(CURRENT_COMPILED_FILE(scope_link), condition, block_context);
    }

    if (expression != NULL)
    {
        solve_possibly_ambiguous_expression(expression, block_context);
        scope_link_set(CURRENT_COMPILED_FILE(scope_link), expression, block_context);
    }
    
    build_scope_statement(statement, block_context);
    scope_link_set(CURRENT_COMPILED_FILE(scope_link), statement, block_context);

    ASTAttrSetValueType(a, LANG_IS_FOR_STATEMENT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_FOR_INIT_CONSTRUCT, tl_type_t, tl_ast(for_init_statement));
    ASTAttrSetValueType(a, LANG_FOR_CONDITION, tl_type_t, tl_ast(condition));
    ASTAttrSetValueType(a, LANG_FOR_ITERATION_EXPRESSION, tl_type_t, tl_ast(expression));
    ASTAttrSetValueType(a, LANG_FOR_BODY_STATEMENT, tl_type_t, tl_ast(statement));
}

static void build_scope_switch_statement(AST a, decl_context_t decl_context, char* attr_name)
{
    decl_context_t block_context = new_block_context(decl_context);

    AST condition = ASTSon0(a);
    AST statement = ASTSon1(a);

    build_scope_condition(condition, block_context);
    scope_link_set(CURRENT_COMPILED_FILE(scope_link), condition, block_context);

    build_scope_statement(statement, block_context);
    scope_link_set(CURRENT_COMPILED_FILE(scope_link), statement, block_context);

    ASTAttrSetValueType(a, LANG_IS_SWITCH_STATEMENT, tl_type_t, tl_bool(1));
}

static void add_label_if_not_found(AST label, decl_context_t decl_context)
{
    // A curiosity :)
    char* label_text = ASTText(label);
    scope_entry_list_t* entry_list = query_unqualified_name_str_flags(decl_context, label_text, DF_LABEL);

    if (entry_list == NULL)
    {
        scope_entry_t* new_label = new_symbol(decl_context, decl_context.function_scope, ASTText(label));
        new_label->kind = SK_LABEL;
        new_label->line = ASTLine(label);
        new_label->file = ASTFileName(label);
    }
}
static void build_scope_goto_statement(AST a, decl_context_t decl_context, char* attr_name)
{
    AST label = ASTSon0(a);
    add_label_if_not_found(label, decl_context);

    ASTAttrSetValueType(a, LANG_IS_GOTO_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_labeled_statement(AST a, decl_context_t decl_context, char* attr_name)
{
    AST label = ASTSon0(a);
    add_label_if_not_found(label, decl_context);

    AST statement = ASTSon1(a);

    build_scope_statement(statement, decl_context);

    ASTAttrSetValueType(a, LANG_IS_LABELED_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_default_statement(AST a, decl_context_t decl_context, char* attr_name)
{
    AST statement = ASTSon0(a);
    build_scope_statement(statement, decl_context);

    ASTAttrSetValueType(a, LANG_IS_DEFAULT_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_case_statement(AST a, decl_context_t decl_context, char* attr_name)
{
    AST constant_expression = ASTSon0(a);
    AST statement = ASTSon1(a);
    solve_possibly_ambiguous_expression(constant_expression, decl_context);

    build_scope_statement(statement, decl_context);

    ASTAttrSetValueType(a, LANG_IS_CASE_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_return_statement(AST a, decl_context_t decl_context, char* attr_name)
{
    AST expression = ASTSon0(a);
    if (expression != NULL)
    {
        solve_possibly_ambiguous_expression(expression, decl_context);
    }

    ASTAttrSetValueType(a, LANG_IS_RETURN_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_try_block(AST a, decl_context_t decl_context, char* attr_name)
{
    AST compound_statement = ASTSon0(a);

    build_scope_statement(compound_statement, decl_context);

    AST handler_seq = ASTSon1(a);
    AST iter;

    for_each_element(handler_seq, iter)
    {
        AST handler = ASTSon1(iter);

        AST exception_declaration = ASTSon0(handler);
        AST compound_statement = ASTSon1(handler);

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

            build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &type_info,
                    block_context);

            scope_link_set(CURRENT_COMPILED_FILE(scope_link), exception_declaration, block_context);

            if (declarator != NULL)
            {
                type_t* declarator_type = NULL;
                compute_declarator_type(declarator, &gather_info, type_info, &declarator_type,
                        block_context);
            }
        }

        build_scope_statement(compound_statement, block_context);
    }

    ASTAttrSetValueType(a, LANG_IS_TRY_BLOCK, tl_type_t, tl_bool(1));
}

static void build_scope_do_statement(AST a, decl_context_t decl_context, char* attr_name) 
{
    AST statement = ASTSon0(a);
    AST expression = ASTSon1(a);

    build_scope_statement(statement, decl_context);
    solve_possibly_ambiguous_expression(expression, decl_context);

    ASTAttrSetValueType(a, LANG_IS_DO_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_null(AST a, decl_context_t decl_context, char* attr_name)
{
    // Do nothing
}

/* OpenMP 2.5 handlers */
static void build_scope_omp_data_clause(AST a, decl_context_t decl_context)
{
    AST list = a;
    AST iter;

    for_each_element(list, iter)
    {
        AST variable = ASTSon1(iter);

        // This is not technically an expression (just a name for an object),
        // but this is a strict subset of expressions, so we use the same code
        solve_possibly_ambiguous_expression(variable, decl_context);
    }
}


static void build_scope_omp_directive(AST a, decl_context_t decl_context, char* attr_name) 
{
    if (attr_name != NULL)
    {
        ASTAttrSetValueType(a, attr_name, tl_type_t, tl_bool(1));
    }

    // Semantic fix of expressions in clauses
    if (ASTSon0(a) != NULL)
    {
        AST list = ASTSon0(a);
        AST iter;

        for_each_element(list, iter)
        {
            AST clause = ASTSon1(iter);

            switch (ASTType(clause))
            {
                case AST_OMP_IF_CLAUSE :
                    {
                        AST expression = ASTSon0(clause);
                        solve_possibly_ambiguous_expression(expression, decl_context);
                        ASTAttrSetValueType(clause, OMP_IS_IF_CLAUSE, tl_type_t, tl_bool(1));
                        break;
                    }
                case AST_OMP_NUM_THREADS_CLAUSE :
                    {
                        AST expression = ASTSon0(clause);
                        solve_possibly_ambiguous_expression(expression, decl_context);
                        ASTAttrSetValueType(clause, OMP_IS_NUM_THREADS_CLAUSE, tl_type_t, tl_bool(1));
                        break;
                    }
                case AST_OMP_SCHEDULE_CLAUSE :
                    {
                        ASTAttrSetValueType(clause, OMP_IS_SCHEDULE_CLAUSE, tl_type_t, tl_bool(1));
                        AST schedule_type = ASTSon0(clause);

                        switch (ASTType(schedule_type))
                        {

                            case AST_OMP_STATIC_SCHEDULE :
                                {
                                    ASTAttrSetValueType(clause, OMP_SCHEDULE_KIND, tl_type_t, tl_integer(1));
                                    break;
                                }
                            case AST_OMP_DYNAMIC_SCHEDULE :
                                {
                                    ASTAttrSetValueType(clause, OMP_SCHEDULE_KIND, tl_type_t, tl_integer(2));
                                    break;
                                }
                            case AST_OMP_GUIDED_SCHEDULE :
                                {
                                    ASTAttrSetValueType(clause, OMP_SCHEDULE_KIND, tl_type_t, tl_integer(4));
                                    break;
                                }
                            case AST_OMP_RUNTIME_SCHEDULE :
                                {
                                    ASTAttrSetValueType(clause, OMP_SCHEDULE_KIND, tl_type_t, tl_integer(8));
                                    break;
                                }
                            case AST_OMP_CUSTOM_SCHEDULE :
                                {
                                    ASTAttrSetValueType(clause, OMP_SCHEDULE_KIND, tl_type_t, tl_integer(0));
                                    ASTAttrSetValueType(clause, OMP_SCHEDULE_CUSTOM_NAME, tl_type_t, 
                                            tl_string(ASTText(schedule_type)));
                                    break;
                                }
                            default:
                                {
                                    internal_error("Unknown schedule type %s\n", ast_print_node_type(ASTType(schedule_type)));
                                    break;
                                }
                        }

                        AST chunk = ASTSon1(clause);
                        if (chunk != NULL)
                        {
                            ASTAttrSetValueType(clause, OMP_SCHEDULE_CHUNK, tl_type_t, tl_ast(chunk));
                        }

                        break;
                    }
                case AST_OMP_ORDERED_CLAUSE :
                    {
                        ASTAttrSetValueType(clause, OMP_IS_ORDERED_CLAUSE, tl_type_t, tl_bool(1));
                        break;
                    }
                case AST_OMP_NOWAIT_CLAUSE :
                    {
                        ASTAttrSetValueType(clause, OMP_IS_NOWAIT_CLAUSE, tl_type_t, tl_bool(1));
                        break;
                    }
                case AST_OMP_SHARED_CLAUSE :
                    {
                        ASTAttrSetValueType(clause, OMP_IS_SHARED_CLAUSE, tl_type_t, tl_bool(1));
                        build_scope_omp_data_clause(ASTSon0(clause), decl_context);
                        break;
                    }
                case AST_OMP_PRIVATE_CLAUSE :
                    {
                        ASTAttrSetValueType(clause, OMP_IS_PRIVATE_CLAUSE, tl_type_t, tl_bool(1));
                        build_scope_omp_data_clause(ASTSon0(clause), decl_context);
                        break;
                    }
                case AST_OMP_FIRSTPRIVATE_CLAUSE :
                    {
                        ASTAttrSetValueType(clause, OMP_IS_FIRSTPRIVATE_CLAUSE, tl_type_t, tl_bool(1));
                        build_scope_omp_data_clause(ASTSon0(clause), decl_context);
                        break;
                    }
                case AST_OMP_LASTPRIVATE_CLAUSE :
                    {
                        ASTAttrSetValueType(clause, OMP_IS_LASTPRIVATE_CLAUSE, tl_type_t, tl_bool(1));
                        build_scope_omp_data_clause(ASTSon0(clause), decl_context);
                        break;
                    }
                case AST_OMP_COPYPRIVATE_CLAUSE :
                    {
                        ASTAttrSetValueType(clause, OMP_IS_COPYPRIVATE_CLAUSE, tl_type_t, tl_bool(1));
                        build_scope_omp_data_clause(ASTSon0(clause), decl_context);
                        break;
                    }
                case AST_OMP_COPYIN_CLAUSE :
                    {
                        ASTAttrSetValueType(clause, OMP_IS_COPYIN_CLAUSE, tl_type_t, tl_bool(1));
                        build_scope_omp_data_clause(ASTSon0(clause), decl_context);
                        break;
                    }
                case AST_OMP_DEFAULT_NONE_CLAUSE :
                    {
                        ASTAttrSetValueType(clause, OMP_IS_DEFAULT_NONE_CLAUSE, tl_type_t, tl_bool(1));
                        break;
                    }
                case AST_OMP_DEFAULT_SHARED_CLAUSE :
                    {
                        ASTAttrSetValueType(clause, OMP_IS_DEFAULT_SHARED_CLAUSE, tl_type_t, tl_bool(1));
                        break;
                    }
                case AST_OMP_DEFAULT_CUSTOM_CLAUSE :
                    {
                        ASTAttrSetValueType(clause, OMP_IS_DEFAULT_CUSTOM_CLAUSE, tl_type_t, tl_bool(1));
                        ASTAttrSetValueType(clause, OMP_DEFAULT_CUSTOM_CLAUSE, tl_type_t, tl_string(ASTText(clause)));
                        break;
                    }
                case AST_OMP_REDUCTION_CLAUSE :
                    {
                        build_scope_omp_data_clause(ASTSon1(clause), decl_context);

                        // Compute here the neuter for future use
                        AST neuter = NULL;
                        switch (ASTType(ASTSon0(clause)))
                        {
                            case AST_ADD_OPERATOR :
                            case AST_MINUS_OPERATOR : 
                            case AST_BITWISE_OR_OPERATOR :
                            case AST_BITWISE_XOR_OPERATOR :
                            case AST_LOGICAL_OR_OPERATOR :
                                {
                                    neuter = ASTMake1(AST_EXPRESSION, 
                                            ASTLeaf(AST_OCTAL_LITERAL, ASTLine(clause), "0"),
                                            ASTLine(clause), NULL);
                                    break;
                                }
                            case AST_MULT_OPERATOR :
                            case AST_LOGICAL_AND_OPERATOR :
                                {
                                    neuter = ASTMake1(AST_EXPRESSION, 
                                            ASTLeaf(AST_DECIMAL_LITERAL, ASTLine(clause), "1"),
                                            ASTLine(clause), NULL);
                                    break;
                                }
                            case AST_BITWISE_AND_OPERATOR :
                                {
                                    AST zero = ASTLeaf(AST_OCTAL_LITERAL, ASTLine(clause), "0");

                                    neuter = ASTMake1(AST_EXPRESSION,
                                            ASTMake1(AST_COMPLEMENT_OP, zero, ASTLine(clause), NULL),
                                            ASTLine(clause), NULL);

                                    break;
                                }
                            default:
                                {
                                    internal_error("Unknown node' %s'\n", 
                                            ast_print_node_type(ASTType(ASTSon0(clause))));
                                }

                        }

                        // Save the neuter
                        ASTSon2(clause) = neuter;
                        ASTParent(neuter) = clause;

                        ASTAttrSetValueType(clause, OMP_IS_REDUCTION_CLAUSE, tl_type_t, tl_bool(1));
                        ASTAttrSetValueType(clause, OMP_IS_USER_DEFINED_REDUCTION, tl_type_t, tl_bool(0));
                        ASTAttrSetValueType(clause, OMP_REDUCTION_OPERATOR, tl_type_t, tl_ast(ASTSon0(clause)));
                        ASTAttrSetValueType(clause, OMP_REDUCTION_VARIABLES, tl_type_t, tl_ast(ASTSon1(clause)));
                        ASTAttrSetValueType(clause, OMP_REDUCTION_NEUTER, tl_type_t, tl_ast(ASTSon2(clause)));
                        break;
                    }
                case AST_OMP_USER_DEFINED_REDUCTION_CLAUSE :
                    {
                        // Check the id-expression
                        solve_possibly_ambiguous_expression(ASTSon0(clause), decl_context);

                        // Check the neuter expression
                        solve_possibly_ambiguous_expression(ASTSon1(clause), decl_context);

                        // Build scope for var-lists
                        build_scope_omp_data_clause(ASTSon2(clause), decl_context);

                        ASTAttrSetValueType(clause, OMP_IS_REDUCTION_CLAUSE, tl_type_t, tl_bool(1));
                        ASTAttrSetValueType(clause, OMP_IS_USER_DEFINED_REDUCTION, tl_type_t, tl_bool(1));
                        ASTAttrSetValueType(clause, OMP_REDUCTION_FUNCTION, tl_type_t, tl_ast(ASTSon0(clause)));
                        ASTAttrSetValueType(clause, OMP_REDUCTION_NEUTER, tl_type_t, tl_ast(ASTSon1(clause)));
                        ASTAttrSetValueType(clause, OMP_REDUCTION_VARIABLES, tl_type_t, tl_ast(ASTSon2(clause)));

                        break;
                    }
                case AST_OMP_CUSTOM_PARAMETER_CLAUSE :
                case AST_OMP_CUSTOM_CLAUSE :
                    {
                        if (ASTType(clause) == AST_OMP_CUSTOM_CLAUSE)
                        {
                            ASTAttrSetValueType(clause, OMP_IS_CUSTOM_CLAUSE, tl_type_t, tl_bool(1));
                            DEBUG_CODE()
                            {
                                fprintf(stderr, "Custom clause name -> '%s'\n", ASTText(clause));
                            }
                            ASTAttrSetValueType(clause, OMP_CUSTOM_CLAUSE_NAME, tl_type_t, tl_string(ASTText(clause)));
                        }
                        else if (ASTType(clause) == AST_OMP_CUSTOM_PARAMETER_CLAUSE)
                        {
                            ASTAttrSetValueType(clause, OMP_IS_PARAMETER_CLAUSE, tl_type_t, tl_bool(1));
                        }

                        AST expression_list = ASTSon0(clause);
                        if (expression_list != NULL)
                        {
                            AST iter;

                            for_each_element(expression_list, iter)
                            {
                                AST expression = ASTSon1(iter);

                                solve_possibly_ambiguous_expression(expression, decl_context);
                            }
                        }

                        break;
                    }
                default:
                    {
                        internal_error("Unknown node '%s' in %s\n", ast_print_node_type(ASTType(clause)),
                                node_information(clause));
                    }
            }
        }
    }
}

static void build_scope_omp_custom_directive(AST a, decl_context_t decl_context, char* attr_name)
{
    if (ASTType(a) == AST_OMP_CUSTOM_DIRECTIVE)
    {
        ASTAttrSetValueType(a, OMP_IS_CUSTOM_DIRECTIVE, tl_type_t, tl_bool(1));
    }
    else if (ASTType(a) == AST_OMP_CUSTOM_CONSTRUCT_DIRECTIVE)
    {
        ASTAttrSetValueType(a, OMP_IS_CUSTOM_CONSTRUCT_DIRECTIVE, tl_type_t, tl_bool(1));
    }

    ASTAttrSetValueType(a, OMP_CUSTOM_DIRECTIVE_NAME, tl_type_t, tl_string(ASTText(a)));

    build_scope_omp_directive(a, decl_context, attr_name);
}

static void build_scope_omp_construct(AST a, decl_context_t decl_context, char* attr_name)
{
    ASTAttrSetValueType(a, OMP_IS_OMP_CONSTRUCT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, attr_name, tl_type_t, tl_bool(1));

    ASTAttrSetValueType(a, OMP_CONSTRUCT_DIRECTIVE, tl_type_t, tl_ast(ASTSon0(a)));

    build_scope_omp_directive(ASTSon0(a), decl_context, NULL);
    if (ASTSon1(a) != NULL)
    {
        ASTAttrSetValueType(a, OMP_CONSTRUCT_BODY, tl_type_t, tl_ast(ASTSon1(a)));
        build_scope_statement(ASTSon1(a), decl_context);
    }
}

static void build_scope_omp_custom_construct_declaration(AST a, decl_context_t decl_context)
{
    ASTAttrSetValueType(a, OMP_IS_OMP_CONSTRUCT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, OMP_IS_CUSTOM_CONSTRUCT, tl_type_t, tl_bool(1));

    ASTAttrSetValueType(a, OMP_CONSTRUCT_DIRECTIVE, tl_type_t, tl_ast(ASTSon0(a)));

    build_scope_omp_custom_directive(ASTSon0(a), decl_context, NULL);
    if (ASTSon1(a) != NULL)
    {
        build_scope_declaration(ASTSon1(a), decl_context);
        ASTAttrSetValueType(a, OMP_CONSTRUCT_DECLARATION, tl_type_t, tl_ast(ASTSon1(a)));
    }
}

static void build_scope_omp_custom_construct_statement(AST a, decl_context_t decl_context, char* attr_name)
{
    ASTAttrSetValueType(a, OMP_IS_OMP_CONSTRUCT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, attr_name, tl_type_t, tl_bool(1));

    ASTAttrSetValueType(a, OMP_CONSTRUCT_DIRECTIVE, tl_type_t, tl_ast(ASTSon0(a)));

    build_scope_omp_custom_directive(ASTSon0(a), decl_context, NULL);
    if (ASTSon1(a) != NULL)
    {
        build_scope_statement(ASTSon1(a), decl_context);
        ASTAttrSetValueType(a, OMP_CONSTRUCT_BODY, tl_type_t, tl_ast(ASTSon1(a)));
    }
}

static void build_scope_omp_threadprivate(AST a, decl_context_t decl_context, char* attr_name)
{
    ASTAttrSetValueType(a, OMP_IS_THREADPRIVATE_DIRECTIVE, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, OMP_CONSTRUCT_DIRECTIVE, tl_type_t, tl_ast(a));
    ASTAttrSetValueType(ASTSon0(a), OMP_IS_PARAMETER_CLAUSE, tl_type_t, tl_bool(1));
    build_scope_omp_data_clause(ASTSon0(a), decl_context);
}

static void build_scope_omp_flush_directive(AST a, decl_context_t decl_context, char* attr_name)
{
    // At the moment do nothing
    ASTAttrSetValueType(a, OMP_IS_FLUSH_DIRECTIVE, tl_type_t, tl_bool(1));
}

static void build_scope_omp_sections_construct(AST a, decl_context_t decl_context, char* attr_name)
{
    ASTAttrSetValueType(a, OMP_CONSTRUCT_DIRECTIVE, tl_type_t, tl_ast(ASTSon0(a)));
    build_scope_omp_directive(ASTSon0(a), decl_context, NULL);

    AST section_sequence = ASTSon1(a);

    if (section_sequence != NULL)
    {
        AST list = section_sequence;
        AST iter;
        
        for_each_element(list, iter)
        {
            AST omp_section = ASTSon1(iter);

            // Nothing is going to be done to the "#pragma omp section"
            //
            // ASTAttrSetValueType(omp_section, OMP_CONSTRUCT_DIRECTIVE, tl_type_t, ASTSon0(omp_section));
            // build_scope_omp_directive(ASTSon0(omp_section), decl_context)

            AST omp_section_body = ASTSon1(omp_section);
            build_scope_statement(omp_section_body, decl_context);

            ASTAttrSetValueType(omp_section, OMP_IS_SECTION_CONSTRUCT, tl_type_t, tl_bool(1));
            ASTAttrSetValueType(omp_section, OMP_CONSTRUCT_BODY, tl_type_t, tl_ast(omp_section_body));
        }

        ASTAttrSetValueType(a, OMP_CONSTRUCT_BODY, tl_type_t, tl_ast(section_sequence));
    }

    ASTAttrSetValueType(a, attr_name, tl_type_t, tl_bool(1));
}

static void build_scope_omp_critical_construct(AST a, decl_context_t decl_context, char* attr_name)
{
    AST critical_directive = ASTSon0(a);
    ASTAttrSetValueType(a, OMP_CONSTRUCT_DIRECTIVE, tl_type_t, tl_ast(critical_directive));

    AST body_construct = ASTSon1(a);
    if (body_construct != NULL)
    {
        ASTAttrSetValueType(a, OMP_CONSTRUCT_BODY, tl_type_t, tl_ast(body_construct));
        build_scope_statement(body_construct, decl_context);
    }

    AST region_phrase = ASTSon0(critical_directive);
    if (region_phrase != NULL)
    {
        solve_possibly_ambiguous_expression(region_phrase, decl_context);

        ASTAttrSetValueType(region_phrase, OMP_IS_PARAMETER_CLAUSE, tl_type_t, tl_bool(1));
    }

    ASTAttrSetValueType(a, OMP_IS_CRITICAL_CONSTRUCT, tl_type_t, tl_bool(1));
}

static void build_scope_pragma_custom_clause_argument(AST a, decl_context_t decl_context)
{
    AST list, iter;
    list = a;

    for_each_element(list, iter)
    {
        ASTAttrSetValueType(ASTSon1(iter), LANG_IS_PRAGMA_CUSTOM_CLAUSE_ARGUMENT, tl_type_t, tl_bool(1));
    }
}

static void build_scope_pragma_custom_clause(AST a, decl_context_t decl_context)
{
    build_scope_pragma_custom_clause_argument(ASTSon0(a), decl_context);

    ASTAttrSetValueType(a, LANG_IS_PRAGMA_CUSTOM_CLAUSE, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_PRAGMA_CUSTOM_CLAUSE, tl_type_t, tl_string(ASTText(a)));
}

static void build_scope_pragma_custom_line(AST a, decl_context_t decl_context, char* attr_name)
{
    if (ASTSon0(a) != NULL)
    {
        AST list, iter;
        list = ASTSon0(a);

        for_each_element(list, iter)
        {
            AST pragma_clause = ASTSon1(iter);

            build_scope_pragma_custom_clause(pragma_clause, decl_context);
        }
    }

    if (ASTSon1(a) != NULL)
    {
        build_scope_pragma_custom_clause_argument(ASTSon1(a), decl_context);

        ASTAttrSetValueType(a, LANG_PRAGMA_CUSTOM_LINE_PARAMETER, tl_type_t, tl_ast(ASTSon1(a)));
    }

    ASTAttrSetValueType(a, LANG_PRAGMA_CUSTOM_LINE_IS_PARAMETERIZED, tl_type_t, 
            tl_bool(ASTSon1(a) != NULL));

    ASTAttrSetValueType(a, LANG_IS_PRAGMA_CUSTOM_LINE, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_PRAGMA_CUSTOM_DIRECTIVE, tl_type_t, tl_string(ASTText(a)));
}

static void build_scope_pragma_custom_directive(AST a, decl_context_t decl_context, char* _dummy)
{
    build_scope_pragma_custom_line(ASTSon0(a), decl_context, LANG_IS_PRAGMA_CUSTOM_LINE);

    ASTAttrSetValueType(a, LANG_IS_PRAGMA_CUSTOM_DIRECTIVE, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_PRAGMA_CUSTOM, tl_type_t, tl_string(ASTText(a)));
    ASTAttrSetValueType(a, LANG_PRAGMA_CUSTOM_LINE, tl_type_t, tl_ast(ASTSon0(a)));
}

static void build_scope_pragma_custom_construct_statement(AST a, decl_context_t decl_context, char* attr_name)
{
    build_scope_pragma_custom_line(ASTSon0(a), decl_context, LANG_IS_PRAGMA_CUSTOM_LINE);
    build_scope_statement(ASTSon1(a), decl_context);

    ASTAttrSetValueType(a, LANG_IS_PRAGMA_CUSTOM_CONSTRUCT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_PRAGMA_CUSTOM, tl_type_t, tl_string(ASTText(a)));
    ASTAttrSetValueType(a, LANG_PRAGMA_CUSTOM_LINE, tl_type_t, tl_ast(ASTSon0(a)));
    ASTAttrSetValueType(a, LANG_PRAGMA_CUSTOM_STATEMENT, tl_type_t, tl_ast(ASTSon1(a)));
}

static void build_scope_pragma_custom_construct_declaration(AST a, decl_context_t decl_context, char* attr_name)
{
    build_scope_pragma_custom_line(ASTSon0(a), decl_context, LANG_IS_PRAGMA_CUSTOM_LINE);
    build_scope_declaration(ASTSon1(a), decl_context);

    ASTAttrSetValueType(a, LANG_IS_PRAGMA_CUSTOM_CONSTRUCT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_PRAGMA_CUSTOM, tl_type_t, tl_string(ASTText(a)));
    ASTAttrSetValueType(a, LANG_PRAGMA_CUSTOM_LINE, tl_type_t, tl_ast(ASTSon0(a)));
    ASTAttrSetValueType(a, LANG_PRAGMA_CUSTOM_DECLARATION, tl_type_t, tl_ast(ASTSon1(a)));
}

static void build_scope_custom_construct_statement(AST a, decl_context_t decl_context, char *attr_name)
{
    // Header
    AST custom_construct_header = ASTSon0(a);

    AST parameters_seq = ASTSon0(custom_construct_header);
    if (parameters_seq != NULL)
    {
        AST iter;
        for_each_element(parameters_seq, iter)
        {
            AST parameter = ASTSon1(iter);

            // AST symbol = ASTSon0(parameter);
            AST expression = ASTSon1(parameter);
            
            solve_possibly_ambiguous_expression(expression, decl_context);
        }
    }

    // Statement
    build_scope_statement(ASTSon1(a), decl_context);

    // TODO - Fill attributes
}


#define STMT_HANDLER(type, hndl, attr_name_v) [type] = { .handler = (hndl), .attr_name = (attr_name_v) }

static stmt_scope_handler_map_t stmt_scope_handlers[] =
{
    STMT_HANDLER(AST_AMBIGUITY, build_scope_ambiguity_handler, NULL),
    STMT_HANDLER(AST_EXPRESSION_STATEMENT, solve_expression_ambiguities, LANG_IS_EXPRESSION_STATEMENT),
    STMT_HANDLER(AST_DECLARATION_STATEMENT, build_scope_declaration_statement, LANG_IS_DECLARATION_STATEMENT),
    STMT_HANDLER(AST_COMPOUND_STATEMENT, build_scope_compound_statement, LANG_IS_COMPOUND_STATEMENT),
    STMT_HANDLER(AST_DO_STATEMENT, build_scope_do_statement, LANG_IS_DO_STATEMENT),
    STMT_HANDLER(AST_WHILE_STATEMENT, build_scope_while_statement, LANG_IS_WHILE_STATEMENT),
    STMT_HANDLER(AST_IF_ELSE_STATEMENT, build_scope_if_else_statement, LANG_IS_IF_STATEMENT),
    STMT_HANDLER(AST_FOR_STATEMENT, build_scope_for_statement, LANG_IS_FOR_STATEMENT),
    STMT_HANDLER(AST_LABELED_STATEMENT, build_scope_labeled_statement, LANG_IS_LABELED_STATEMENT),
    STMT_HANDLER(AST_DEFAULT_STATEMENT, build_scope_default_statement, LANG_IS_DEFAULT_STATEMENT),
    STMT_HANDLER(AST_CASE_STATEMENT, build_scope_case_statement, LANG_IS_CASE_STATEMENT),
    STMT_HANDLER(AST_RETURN_STATEMENT, build_scope_return_statement, LANG_IS_RETURN_STATEMENT),
    STMT_HANDLER(AST_TRY_BLOCK, build_scope_try_block, LANG_IS_TRY_BLOCK),
    STMT_HANDLER(AST_SWITCH_STATEMENT, build_scope_switch_statement, LANG_IS_SWITCH_STATEMENT),
    STMT_HANDLER(AST_EMPTY_STATEMENT, build_scope_null, LANG_IS_EMPTY_STATEMENT),
    STMT_HANDLER(AST_BREAK_STATEMENT, build_scope_null, LANG_IS_BREAK_STATEMENT),
    STMT_HANDLER(AST_CONTINUE_STATEMENT, build_scope_null, LANG_IS_CONTINUE_STATEMENT),
    STMT_HANDLER(AST_GOTO_STATEMENT, build_scope_goto_statement, LANG_IS_GOTO_STATEMENT),
    // Pragma custom support
    STMT_HANDLER(AST_PRAGMA_CUSTOM_CONSTRUCT, build_scope_pragma_custom_construct_statement, LANG_IS_PRAGMA_CUSTOM_CONSTRUCT),
    STMT_HANDLER(AST_PRAGMA_CUSTOM_DIRECTIVE, build_scope_pragma_custom_directive, LANG_IS_PRAGMA_CUSTOM_DIRECTIVE),
    // Custom construct
    STMT_HANDLER(AST_CUSTOM_CONSTRUCT_STATEMENT, build_scope_custom_construct_statement, /*LANG_IS_CUSTOM_CONSTRUCT*/ ""),
    // OpenMP 2.5 constructs
    STMT_HANDLER(AST_OMP_PARALLEL_CONSTRUCT, build_scope_omp_construct, OMP_IS_PARALLEL_CONSTRUCT),
    STMT_HANDLER(AST_OMP_FOR_CONSTRUCT, build_scope_omp_construct, OMP_IS_FOR_CONSTRUCT),
    STMT_HANDLER(AST_OMP_PARALLEL_FOR_CONSTRUCT, build_scope_omp_construct, OMP_IS_PARALLEL_FOR_CONSTRUCT),
    STMT_HANDLER(AST_OMP_PARALLEL_SINGLE_CONSTRUCT, build_scope_omp_construct, OMP_IS_PARALLEL_SINGLE_CONSTRUCT),
    STMT_HANDLER(AST_OMP_SECTIONS_CONSTRUCT, build_scope_omp_sections_construct, OMP_IS_SECTIONS_CONSTRUCT),
    STMT_HANDLER(AST_OMP_PARALLEL_SECTIONS_CONSTRUCT, build_scope_omp_sections_construct, OMP_IS_PARALLEL_SECTIONS_CONSTRUCT),
    STMT_HANDLER(AST_OMP_SINGLE_CONSTRUCT, build_scope_omp_construct, OMP_IS_SINGLE_CONSTRUCT),
    STMT_HANDLER(AST_OMP_MASTER_CONSTRUCT, build_scope_omp_construct, OMP_IS_MASTER_CONSTRUCT),
    STMT_HANDLER(AST_OMP_ATOMIC_CONSTRUCT, build_scope_omp_construct, OMP_IS_ATOMIC_CONSTRUCT),
    STMT_HANDLER(AST_OMP_ORDERED_CONSTRUCT, build_scope_omp_construct, OMP_IS_ORDERED_CONTRUCT),
    STMT_HANDLER(AST_OMP_CRITICAL_CONSTRUCT, build_scope_omp_critical_construct, OMP_IS_CRITICAL_CONSTRUCT),
    STMT_HANDLER(AST_OMP_FLUSH_DIRECTIVE, build_scope_omp_flush_directive, OMP_IS_FLUSH_DIRECTIVE),
    STMT_HANDLER(AST_OMP_BARRIER_DIRECTIVE, build_scope_omp_directive, OMP_IS_BARRIER_DIRECTIVE),
    STMT_HANDLER(AST_OMP_THREADPRIVATE_DIRECTIVE, build_scope_omp_threadprivate, OMP_IS_THREADPRIVATE_DIRECTIVE),
    STMT_HANDLER(AST_OMP_CUSTOM_CONSTRUCT, build_scope_omp_custom_construct_statement, OMP_IS_CUSTOM_CONSTRUCT),
    STMT_HANDLER(AST_OMP_CUSTOM_DIRECTIVE, build_scope_omp_custom_directive, NULL)
};

void build_scope_member_specification_with_scope_link(
        decl_context_t class_context,
        scope_link_t* scope_link,
        AST member_specification_tree, 
        access_specifier_t current_access,
        type_t* simple_type_info)
{
    scope_link_t* old_scope_link = CURRENT_COMPILED_FILE(scope_link);
    CURRENT_COMPILED_FILE(scope_link) = scope_link;

    build_scope_member_specification(class_context, member_specification_tree, 
            current_access, simple_type_info);

    CURRENT_COMPILED_FILE(scope_link) = old_scope_link;
}

static void build_scope_statement_seq(AST a, decl_context_t decl_context)
{
    AST list = a;
    if (list != NULL)
    {
        AST iter;
        for_each_element(list, iter)
        {
            build_scope_statement(ASTSon1(iter), decl_context);
        }
    }
}

void build_scope_statement_seq_with_scope_link(AST a, decl_context_t decl_context, scope_link_t* scope_link)
{
    scope_link_t* old_scope_link = CURRENT_COMPILED_FILE(scope_link);
    CURRENT_COMPILED_FILE(scope_link) = scope_link;

    build_scope_statement_seq(a, decl_context);

    CURRENT_COMPILED_FILE(scope_link) = old_scope_link;
}

void build_scope_declaration_sequence_with_scope_link(AST a, decl_context_t decl_context, scope_link_t* scope_link)
{
    scope_link_t* old_scope_link = CURRENT_COMPILED_FILE(scope_link);
    CURRENT_COMPILED_FILE(scope_link) = scope_link;

    build_scope_declaration_sequence(a, decl_context);

    CURRENT_COMPILED_FILE(scope_link) = old_scope_link;
}

void build_scope_statement(AST a, decl_context_t decl_context)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "==== Statement line [%s] ====\n", node_information(a));
    }

    stmt_scope_handler_t f = stmt_scope_handlers[ASTType(a)].handler;
    char* attr_name = stmt_scope_handlers[ASTType(a)].attr_name;

    if (f != NULL)
    {
        f(a, decl_context, attr_name);
    }
    else
    {
        WARNING_MESSAGE("Statement node type '%s' does not have handler in %s", ast_print_node_type(ASTType(a)),
                node_information(a));
    }

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
        case AST_POINTER_DECL :
            {
                return get_function_declarator_parameter_list(ASTSon1(funct_declarator), decl_context);
                break;
            }
        case AST_DECLARATOR_FUNC :
            {
                return ASTSon1(funct_declarator);
                break;
            }
        case AST_GCC_DECLARATOR :
            {
                return get_function_declarator_parameter_list(ASTSon1(funct_declarator), decl_context);
                break;
            }
        case AST_GCC_POINTER_DECL :
            {
                return get_function_declarator_parameter_list(ASTSon2(funct_declarator), decl_context);
                break;
            }
        default:
            {
                internal_error("Unknown node '%s' at '%s'\n", ast_print_node_type(ASTType(funct_declarator)),
                        node_information(funct_declarator));
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
    ERROR_CONDITION((a == NULL), "This function does not admit NULL trees", 0);

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
        case AST_POINTER_DECL :
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
        case AST_GCC_FUNCTIONAL_DECLARATOR :
            {
                return get_declarator_id_expression(ASTSon0(a), decl_context);
            }
        case AST_NEW_DECLARATOR :
        case AST_DIRECT_NEW_DECLARATOR :
        case AST_CONVERSION_DECLARATOR :
        case AST_ABSTRACT_DECLARATOR :
        case AST_GCC_ABSTRACT_DECLARATOR :
        case AST_PARENTHESIZED_ABSTRACT_DECLARATOR:
        case AST_ABSTRACT_DECLARATOR_FUNC:
        case AST_ABSTRACT_ARRAY :
            {
                return NULL;
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

static AST advance_over_declarator_nests(AST a, decl_context_t decl_context)
{
    ERROR_CONDITION(a == NULL, "This node cannot be null", 0);
    ERROR_CONDITION(ASTType(a) == AST_AMBIGUITY, "This node should not be ambiguous here", 0);
    ERROR_CONDITION(get_declarator_name(a, decl_context) == NULL, "This should be a non-abstract declarator", 0);

    switch(ASTType(a))
    {
        case AST_INIT_DECLARATOR :
        case AST_MEMBER_DECLARATOR :
        case AST_GCC_MEMBER_DECLARATOR :
        case AST_DECLARATOR :
        case AST_PARENTHESIZED_DECLARATOR :
            {
                return advance_over_declarator_nests(ASTSon0(a), decl_context); 
                break;
            }
        case AST_GCC_DECLARATOR :
            {
                return advance_over_declarator_nests(ASTSon1(a), decl_context);
            }
        default:
            return a;
    }
}

// Returns non-null tree with the leftmost declarator
// that is not preceded by any other sign 
//
//   int f(); // Returns the tree holding 'f'
//   int (f()); // Returns null
//   int a[10]; // Returns the tree holding 'a'
AST get_leftmost_declarator_name(AST a, decl_context_t decl_context)
{
    ERROR_CONDITION((a == NULL), "This function does not admit NULL trees", 0);

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
        case AST_POINTER_DECL :
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
        case AST_ABSTRACT_DECLARATOR :
        case AST_GCC_ABSTRACT_DECLARATOR :
        case AST_PARENTHESIZED_ABSTRACT_DECLARATOR:
        case AST_ABSTRACT_DECLARATOR_FUNC:
        case AST_ABSTRACT_ARRAY :
            {
                return NULL;
            }
        case AST_AMBIGUITY :
            {
                // A scope null is valid here since this is purely syntactic
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

// This function used to do very strange things
char* get_conversion_function_name(decl_context_t decl_context, 
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

    build_scope_decl_specifier_seq(type_specifier, &gather_info, &simple_type_info,
            decl_context);

    type_t* type_info = NULL;

    if (conversion_declarator != NULL)
    {
        compute_declarator_type(conversion_declarator, &gather_info, simple_type_info, &type_info,
                decl_context);
    }
    else
    {
        type_info = simple_type_info;
    }

    if (result_conversion_type != NULL)
    {
        *result_conversion_type = type_info;
    }

    return "$.operator";
}

static AST get_enclosing_declaration(AST point_of_declarator)
{
    while (point_of_declarator != NULL
            && ASTType(point_of_declarator) != AST_SIMPLE_DECLARATION
            && ASTType(point_of_declarator) != AST_MEMBER_DECLARATION
            && ASTType(point_of_declarator) != AST_FUNCTION_DEFINITION
            && ASTType(point_of_declarator) != AST_PARAMETER_DECL)
    {
        point_of_declarator = ASTParent(point_of_declarator);
    }

    return point_of_declarator;
}
