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


static void build_scope_declaration(AST a, scope_t* st, decl_context_t decl_context);
static void build_scope_declaration_sequence(AST a, scope_t* st, decl_context_t decl_context);
static void build_scope_simple_declaration(AST a, scope_t* st, decl_context_t decl_context);

static void build_scope_namespace_alias(AST a, scope_t* st, decl_context_t decl_context);
static void build_scope_namespace_definition(AST a, scope_t* st, decl_context_t decl_context);
static scope_entry_t* build_scope_function_definition(AST a, scope_t* st, decl_context_t decl_context);
static scope_entry_t* build_scope_declarator_with_parameter_scope(AST a, scope_t* st, scope_t** parameters_scope, 
        gather_decl_spec_t* gather_info, type_t* simple_type_info, type_t** declarator_type,
        decl_context_t decl_context);

static void build_scope_member_declaration(AST a, scope_t*  st, 
        access_specifier_t current_access, type_t* simple_type_info,
        int step, decl_context_t decl_context);
static void build_scope_simple_member_declaration(AST a, scope_t*  st, 
        access_specifier_t current_access, type_t* simple_type_info, decl_context_t decl_context);
static scope_entry_t* build_scope_member_function_definition(AST a, scope_t*  st, 
        access_specifier_t current_access, type_t* class_info, int step, decl_context_t decl_context);

static void build_scope_statement(AST statement, scope_t* st, decl_context_t decl_context);

static void gather_type_spec_from_simple_type_specifier(AST a, scope_t* st, type_t* type_info,
        decl_context_t decl_context);
static void gather_type_spec_from_enum_specifier(AST a, scope_t* st, type_t* type_info, 
        decl_context_t decl_context);
static void gather_type_spec_from_class_specifier(AST a, scope_t* st, type_t* type_info,
        decl_context_t decl_context);
static void gather_type_spec_from_dependent_typename(AST a, scope_t* st, type_t* simple_type_info,
        decl_context_t decl_context);

static void gather_type_spec_from_elaborated_class_specifier(AST a, scope_t* st, type_t* type_info,
        decl_context_t decl_context);
static void gather_type_spec_from_elaborated_enum_specifier(AST a, scope_t* st, type_t* type_info,
        decl_context_t decl_context);

static void build_scope_declarator_rec(AST a, scope_t* st, scope_t** parameters_scope, type_t** declarator_type, 
        gather_decl_spec_t* gather_info, AST* declarator_name, decl_context_t decl_context);


static scope_entry_t* build_scope_declarator_name(AST declarator_name, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, scope_t* st, decl_context_t decl_context);
static scope_entry_t* build_scope_declarator_id_expr(AST declarator_name, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, scope_t* st, decl_context_t decl_context);

static void build_scope_linkage_specifier(AST a, scope_t* st, decl_context_t decl_context);
static void build_scope_linkage_specifier_declaration(AST a, scope_t* st, decl_context_t decl_context);

static void build_scope_template_arguments_for_primary_template(scope_t* st, 
        scope_t* template_scope,
        template_parameter_t** template_parameter_info, int num_template_parameters, 
        template_argument_list_t** template_arguments);

static void build_scope_template_declaration(AST a, scope_t* st, decl_context_t decl_context);
static void build_scope_explicit_template_specialization(AST a, scope_t* st, decl_context_t decl_context);

static void build_scope_statement_seq(AST a, scope_t* st, decl_context_t decl_context);

static void build_scope_template_parameter_list(AST a, scope_t* st, 
        template_parameter_t*** template_parameters, int* num_parameters,
        decl_context_t decl_context);
static void build_scope_template_parameter(AST a, scope_t* st, 
        template_parameter_t* template_parameters, int num_parameter,
        decl_context_t decl_context);
static void build_scope_nontype_template_parameter(AST a, scope_t* st,
        template_parameter_t* template_parameters, int num_parameter,
        decl_context_t decl_context);
static void build_scope_type_template_parameter(AST a, scope_t* st,
        template_parameter_t* template_parameters, int num_parameter,
        decl_context_t decl_context);
static void build_scope_template_template_parameter(AST a, scope_t* st,
        template_parameter_t* template_parameters, int num_parameter, 
        decl_context_t decl_context);

static void build_scope_member_template_declaration(AST a, scope_t* st, 
        access_specifier_t current_access, type_t* class_info, int step, 
        decl_context_t decl_context);
static void build_scope_member_template_function_definition(AST a, scope_t* st, scope_t* template_scope, 
        int num_parameters, template_parameter_t** template_parameters,
        access_specifier_t current_access, type_t* class_info, int step,
        decl_context_t decl_context);
static void build_scope_member_template_simple_declaration(AST a, scope_t* st, scope_t* template_scope, 
        int num_parameters, template_parameter_t** template_parameters,
        access_specifier_t current_access, type_t* class_info, decl_context_t decl_context);

static void build_scope_using_directive(AST a, scope_t* st, decl_context_t decl_context);
static void build_scope_using_declaration(AST a, scope_t* st, decl_context_t decl_context);

static void build_scope_explicit_instantiation(AST a, scope_t* st, decl_context_t decl_context);

static scope_entry_t* register_new_typedef_name(AST declarator_id, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, scope_t* st, decl_context_t decl_context);
static scope_entry_t* register_new_variable_name(AST declarator_id, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, scope_t* st, decl_context_t decl_context);
static scope_entry_t* register_function(AST declarator_id, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, scope_t* st, decl_context_t decl_context);

static void build_scope_template_function_definition(AST a, scope_t* st, scope_t* template_scope, 
        int num_parameters, template_parameter_t** template_parameters, decl_context_t decl_context);
static void build_scope_template_simple_declaration(AST a, scope_t* st, scope_t* template_scope, 
        int num_parameters, template_parameter_t** template_parameters, decl_context_t decl_context);

static void build_scope_gcc_asm_definition(AST a, scope_t* st, decl_context_t decl_context);

static cv_qualifier_t compute_cv_qualifier(AST a);

static exception_spec_t* build_exception_spec(scope_t* st, AST a, 
        decl_context_t decl_context);

static char is_constructor_declarator(AST a);

static scope_entry_t* find_function_declaration(scope_t* st, AST declarator_id, 
        type_t* declarator_type, char* is_overload, decl_context_t decl_context);

static AST get_enclosing_declaration(AST point_of_declarator);

// OpenMP functions needed here
static void build_scope_omp_custom_directive(AST a, scope_t* st, decl_context_t decl_context, char* attr_name);
static void build_scope_omp_threadprivate(AST a, scope_t* st, decl_context_t decl_context, char* attr_name);

// Current linkage, by default C++
static char* current_linkage = "\"C++\"";

static void initialize_builtin_symbols(void);

const decl_context_t default_decl_context = { 0 };

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

    // The global scope is created here
    translation_unit->global_scope = new_namespace_scope(NULL);
	translation_unit->scope_link = scope_link_new();

	// Link the AST root node with the global scope
	scope_link_set(translation_unit->scope_link, a, translation_unit->global_scope);

    // Fix this one day (it makes thing non fully reentrant)
    compilation_options.global_scope = translation_unit->global_scope;
	compilation_options.scope_link = translation_unit->scope_link;

    initialize_builtin_symbols();

	// Refactor this and "build_scope_translation_unit_tree_with_global_scope" one day
    build_scope_declaration_sequence(list, compilation_options.global_scope, default_decl_context);

    if (compilation_options.debug_options.print_scope)
    {
        fprintf(stderr, "============ SYMBOL TABLE ===============\n");
        print_scope(compilation_options.global_scope);
        fprintf(stderr, "========= End of SYMBOL TABLE ===========\n");
    }

    // Clear for sanity
	compilation_options.scope_link = NULL;
    compilation_options.global_scope = NULL;
}

void build_scope_translation_unit_tree_with_global_scope(AST tree,
		scope_t* global_scope, scope_link_t* scope_link,
        decl_context_t decl_context)
{
    compilation_options.global_scope = global_scope;
	compilation_options.scope_link = scope_link;
	
	if (ASTType(tree) != AST_TRANSLATION_UNIT)
	{
		internal_error("This function expects a translation unit tree but '%s'", 
				ast_print_node_type(ASTType(tree)));
	}

	AST list = ASTSon0(tree);
	// The scope will have been already populated with basic things
    build_scope_declaration_sequence(list, compilation_options.global_scope, decl_context);
	
    // Clear for sanity 
	compilation_options.scope_link = NULL;
    compilation_options.global_scope = NULL;
}

// This function initialize global symbols that exist in every translation unit
// prior to its translation
static void initialize_builtin_symbols(void)
{
    // __builtin_va_list is a very special type in GCC
    scope_entry_t* builtin_va_list;

    builtin_va_list = new_symbol(compilation_options.global_scope, "__builtin_va_list");
    builtin_va_list->kind = SK_GCC_BUILTIN_TYPE;
    builtin_va_list->type_information = calloc(1, sizeof(*(builtin_va_list->type_information)));

    builtin_va_list->type_information->kind = TK_DIRECT;
    builtin_va_list->type_information->type = calloc(1, sizeof(*(builtin_va_list->type_information->type)));
    builtin_va_list->type_information->type->kind = STK_VA_LIST;
    builtin_va_list->defined = 1;

    CXX_LANGUAGE()
    {
        // __null is a magic NULL in g++
        scope_entry_t* null_keyword;

        null_keyword = new_symbol(compilation_options.global_scope, "__null");
        null_keyword->kind = SK_VARIABLE;
        null_keyword->type_information = calloc(1, sizeof(*(null_keyword->type_information)));
        null_keyword->type_information->kind = TK_DIRECT;
        null_keyword->type_information->type = calloc(1, sizeof(*(null_keyword->type_information->type)));
        null_keyword->type_information->type->kind = STK_BUILTIN_TYPE;
        null_keyword->type_information->type->builtin_type = BT_INT;
        null_keyword->expression_value = ASTLeaf(AST_OCTAL_LITERAL, 0, "0");
        null_keyword->defined = 1;
    }
}

static void build_scope_declaration_sequence(AST list, scope_t* st, decl_context_t decl_context)
{
    AST iter;
    for_each_element(list, iter)
    {
        build_scope_declaration(ASTSon1(iter), st, decl_context);
    }
}

static int max_line = 0;

// Build scope for a declaration
static void build_scope_declaration(AST a, scope_t* st, decl_context_t decl_context)
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
                build_scope_simple_declaration(a, st, decl_context);
                break;
            }
        case AST_NAMESPACE_DEFINITION :
            {
                // Namespace definitions are of the form
                //   namespace [name]
                //   {
                //      ...
                //   }
                build_scope_namespace_definition(a, st, decl_context);
                break;
            }
        case AST_NAMESPACE_ALIAS :
            {
                build_scope_namespace_alias(a, st, decl_context);
                break;
            }
        case AST_FUNCTION_DEFINITION :
            {
                // A function definition is of the form
                //   [T] f(T1 t, T2 t, T3 t)
                //   {
                //     ...
                //   }
                build_scope_function_definition(a, st, decl_context);
                break;
            }
        case AST_LINKAGE_SPEC :
            {
                build_scope_linkage_specifier(a, st, decl_context);
                break;
            }
        case AST_LINKAGE_SPEC_DECL :
            {
                build_scope_linkage_specifier_declaration(a, st, decl_context);
                break;
            }
        case AST_EXPORT_TEMPLATE_DECLARATION :
        case AST_TEMPLATE_DECLARATION :
            {
                build_scope_template_declaration(a, st, decl_context);
                break;
            }
        case AST_EXPLICIT_INSTANTIATION :
            {
                build_scope_explicit_instantiation(a, st, decl_context);
                break;
            }
        case AST_EXPLICIT_SPECIALIZATION :
            {
                build_scope_explicit_template_specialization(a, st, decl_context);
                break;
            }
        case AST_USING_DIRECTIVE :
            {
                build_scope_using_directive(a, st, decl_context);
                break;
            }
        case AST_USING_DECL :
        case AST_USING_DECL_TYPENAME :
            {
                build_scope_using_declaration(a, st, decl_context);
                break;
            }
        case AST_AMBIGUITY :
            {
                solve_ambiguous_declaration(a, st, decl_context);
                // Restart function
                if (ASTType(a) == AST_AMBIGUITY)
                {
                    internal_error("Ambiguity not handled\n", 0);
                }
                build_scope_declaration(a, st, decl_context);
                break;
            }
        case AST_ASM_DEFINITION :
        case AST_EMPTY_DECL :
        case AST_UNKNOWN_PRAGMA :
            {
                // Do nothing
                break;
            }
			// OpenMP 2.5 constructs
		case AST_OMP_THREADPRIVATE_DIRECTIVE :
			{
				build_scope_omp_threadprivate(a, st, decl_context, OMP_IS_THREADPRIVATE_DIRECTIVE);
				break;
			}
		case AST_OMP_CUSTOM_DIRECTIVE :
			{
				build_scope_omp_custom_directive(a, st, decl_context, NULL);
				break;
			}
			// GCC Extensions
        case AST_GCC_EXTENSION :
            {
                build_scope_declaration(ASTSon0(a), st, decl_context);
                break;
            }
        case AST_GCC_ASM_DEFINITION :
            {
                build_scope_gcc_asm_definition(a, st, decl_context);
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
static void build_scope_gcc_asm_definition(AST a, scope_t* st, decl_context_t decl_context)
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
                    solve_possibly_ambiguous_expression(expression, st, decl_context);
                }
            }
        }
    }
}

// It simply disambiguates
static void build_scope_explicit_instantiation(AST a, scope_t* st, decl_context_t decl_context)
{
    AST decl_specifier_seq = ASTSon1(a);
    AST declarator = ASTSon2(a);


    type_t* simple_type_info = NULL;
    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));

    if (decl_specifier_seq != NULL)
    {
        build_scope_decl_specifier_seq(decl_specifier_seq, st, &gather_info, &simple_type_info, decl_context);
    }

    if (declarator != NULL)
    {
        type_t* declarator_type = NULL;
        build_scope_declarator(declarator, st, &gather_info, simple_type_info, &declarator_type, decl_context);
    }
}

static void build_scope_using_directive(AST a, scope_t* st, decl_context_t decl_context)
{
    int j;
    // First get the involved namespace
    AST global_op = ASTSon0(a);
    AST nested_name = ASTSon1(a);
    AST name = ASTSon2(a);

    scope_entry_list_t* result_list = query_nested_name(st, global_op, 
            nested_name, name, FULL_UNQUALIFIED_LOOKUP, decl_context);

    ERROR_CONDITION((result_list == NULL), "Namespace '%s' not found\n", ASTText(name));

    ERROR_CONDITION((result_list->next != NULL || result_list->entry->kind != SK_NAMESPACE),
            "Symbol '%s' is not a namespace\n", ASTText(name));

    scope_entry_t* entry = result_list->entry;

    // Now add this namespace to the used namespaces of this scope
    // Add it once
    P_LIST_ADD_ONCE(st->use_namespace, st->num_used_namespaces, entry->related_scope);

    // Transitively add related scopes but avoid repeating them
    for (j = 0; j < entry->related_scope->num_used_namespaces; j++)
    {
        P_LIST_ADD_ONCE(st->use_namespace, st->num_used_namespaces, entry->related_scope->use_namespace[j]);
    }
}

static void build_scope_using_declaration(AST a, scope_t* st, decl_context_t decl_context)
{
    AST global_op = ASTSon0(a);
    AST nested_name_specifier = ASTSon1(a);
    AST unqualified_id = ASTSon2(a);

    scope_entry_list_t* used_entity = query_nested_name(st, global_op, nested_name_specifier, 
            unqualified_id, FULL_UNQUALIFIED_LOOKUP, decl_context);

    ERROR_CONDITION((used_entity == NULL), 
            "Entity '%s' not found (%s)\n", 
            prettyprint_in_buffer(a), 
            node_information(a));

    while (used_entity != NULL)
    {
        insert_entry(st, used_entity->entry);

        used_entity = used_entity->next;
    }
}

// Builds scope for a simple declaration
static void build_scope_simple_declaration(AST a, scope_t* st, decl_context_t decl_context)
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
        // This can declare a type if it is a class specifier or enum specifier
        decl_context_t new_decl_context = decl_context;
        if (ASTSon1(a) == NULL)
        {
            new_decl_context.decl_flags |= DF_NO_DECLARATORS;
        }
        else
        {
            new_decl_context.decl_flags &= (~DF_NO_DECLARATORS);
        }

        build_scope_decl_specifier_seq(ASTSon0(a), st, &gather_info, &simple_type_info,
                new_decl_context);
    }

	ASTAttrSetValueType(a, LANG_IS_DECLARATION, tl_type_t, tl_bool(1));
	ASTAttrSetValueType(a, LANG_DECLARATION_SPECIFIERS, tl_type_t, tl_ast(ASTSon0(a)));

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
                solve_ambiguous_init_declarator(init_declarator, st, decl_context);
            }

            AST declarator = ASTSon0(init_declarator);
            AST initializer = ASTSon1(init_declarator);

            type_t* declarator_type;

            // This will create the symbol if it is unqualified
            scope_t* parameters_scope = NULL;
            build_scope_declarator_with_parameter_scope(declarator, st, &parameters_scope, 
                    &gather_info, simple_type_info, &declarator_type, decl_context);

            // This is a simple declaration, thus if it does not declare an
            // extern variable or function, the symbol is already defined here
            if (!gather_info.is_extern
                    && declarator_type->kind != TK_FUNCTION)
            {
                AST declarator_name = get_declarator_name(declarator, st, decl_context);
                scope_entry_list_t* entry_list = query_id_expression(st, declarator_name, 
                        NOFULL_UNQUALIFIED_LOOKUP, decl_context);

                ERROR_CONDITION((entry_list == NULL), "Symbol '%s' just declared has not been found in the scope",
                        prettyprint_in_buffer(declarator_name));

                // The last entry will hold our symbol, no need to look for it in the list
                ERROR_CONDITION((entry_list->entry->defined 
                            && entry_list->entry->kind != SK_TYPEDEF
                            && !BITMAP_TEST(decl_context.decl_flags, DF_ALLOW_REDEFINITION)),
                        "Symbol '%s' in %s has already been defined", prettyprint_in_buffer(declarator_name),
                        node_information(declarator_name));

                DEBUG_CODE()
                {
                    fprintf(stderr, "Defining symbol '");
                    prettyprint(stderr, declarator_name);
                    fprintf(stderr, "'\n");
                }

                entry_list->entry->defined = 1;

                if (initializer != NULL)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Initializer: '%s'\n", ast_print_node_type(ASTType(initializer)));
                    }

                    check_for_initialization(initializer, entry_list->entry->scope, decl_context);
                    entry_list->entry->expression_value = initializer;

					ASTAttrSetValueType(declarator, LANG_INITIALIZER, tl_type_t, tl_ast(initializer));
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
void build_scope_decl_specifier_seq(AST a, scope_t* st, gather_decl_spec_t* gather_info, 
        type_t **simple_type_info, decl_context_t decl_context)
{
    AST iter, list;

    if (ASTType(a) == AST_AMBIGUITY)
    {
        solve_ambiguous_decl_specifier_seq(a, st, decl_context);
        ERROR_CONDITION((ASTType(a) == AST_AMBIGUITY), "Ambiguity not solved", 0);
    }

    // Gather decl specifier sequence information previous to type_spec
    list = ASTSon0(a);
    if (list != NULL)
    {
        for_each_element(list, iter)
        {
            AST spec = ASTSon1(iter);
            gather_decl_spec_information(spec, st, gather_info);
        }
    }

    // Gather decl specifier sequence information after type_spec
    list = ASTSon2(a);
    if (list != NULL)
    {
        for_each_element(list, iter)
        {
            AST spec = ASTSon1(iter);
            gather_decl_spec_information(spec, st, gather_info);
        }
    }

    // Now gather information of the type_spec
    if (ASTSon1(a) != NULL) 
    {
        *simple_type_info = calloc(1, sizeof(**simple_type_info));
        (*simple_type_info)->type = calloc(1, sizeof(*((*simple_type_info)->type)));

        decl_context_t new_decl_context = decl_context;

        if (gather_info->is_friend)
        {
            new_decl_context.decl_flags |= DF_FRIEND;
        }
        else
        {
            new_decl_context.decl_flags &= (~DF_FRIEND);
        }

        gather_type_spec_information(ASTSon1(a), st, *simple_type_info, new_decl_context);
        
        // Now update the type_spec with type information that was caught in the decl_specifier_seq
        if (gather_info->is_long)
        {
            // It is not set to 1 because of gcc long long
            (*simple_type_info)->type->is_long++;
        }

        if (gather_info->is_short)
        {
            (*simple_type_info)->type->is_short = 1;
        }

        if (gather_info->is_unsigned)
        {
            (*simple_type_info)->type->is_unsigned = 1;
        }

        if (gather_info->is_signed)
        {
            (*simple_type_info)->type->is_signed = 1;
        }

        // GCC extension
        if (gather_info->is_complex)
        {
            (*simple_type_info)->type->is_complex = 1;
        }
        
        // cv-qualification
        if (gather_info->is_const)
        {
            (*simple_type_info)->cv_qualifier |= CV_CONST;
        }

        if (gather_info->is_volatile)
        {
            (*simple_type_info)->cv_qualifier |= CV_VOLATILE;
        }
    }
}

/*
 * This function gathers everything that is in a decl_spec and fills gather_info
 *
 * scope_t* sc is unused here
 */
void gather_decl_spec_information(AST a, scope_t* st, gather_decl_spec_t* gather_info)
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
        case AST_GCC_ATTRIBUTE :
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
void gather_type_spec_information(AST a, scope_t* st, type_t* simple_type_info,
        decl_context_t decl_context)
{
    // This can be overriden in gather_type_spec_from_simple_type_specifier if
    // declaring a typedef
    simple_type_info->kind = TK_DIRECT;

    switch (ASTType(a))
    {
        case AST_SIMPLE_TYPE_SPECIFIER :
            gather_type_spec_from_simple_type_specifier(a, st, simple_type_info, decl_context);
            break;
        case AST_ELABORATED_TYPENAME : 
        case AST_ELABORATED_TYPENAME_TEMPLATE : 
            gather_type_spec_from_dependent_typename(a, st, simple_type_info, decl_context);
            break;
        case AST_ENUM_SPECIFIER :
            gather_type_spec_from_enum_specifier(a, st, simple_type_info, decl_context);
            break;
        case AST_CLASS_SPECIFIER :
            gather_type_spec_from_class_specifier(a, st, simple_type_info, decl_context);
            break;
        case AST_ELABORATED_TYPE_ENUM :
            gather_type_spec_from_elaborated_enum_specifier(a, st, simple_type_info, decl_context);
            break;
        case AST_GCC_ELABORATED_TYPE_ENUM :
            gather_type_spec_from_elaborated_class_specifier(ASTSon1(a), st, simple_type_info, decl_context);
            break;
        case AST_ELABORATED_TYPE_CLASS :
            gather_type_spec_from_elaborated_class_specifier(a, st, simple_type_info, decl_context);
            break;
        case AST_GCC_ELABORATED_TYPE_CLASS :
            gather_type_spec_from_elaborated_class_specifier(ASTSon1(a), st, simple_type_info, decl_context);
            break;
        case AST_ELABORATED_TYPE_TEMPLATE_TEMPLATE_CLASS :
        case AST_ELABORATED_TYPE_TEMPLATE_CLASS :
            gather_type_spec_from_elaborated_class_specifier(a, st, simple_type_info, decl_context);
            break;
        case AST_GCC_ELABORATED_TYPE_TEMPLATE_CLASS :
        case AST_GCC_ELABORATED_TYPE_TEMPLATE_TEMPLATE_CLASS :
            gather_type_spec_from_elaborated_class_specifier(ASTSon1(a), st, simple_type_info, decl_context);
            break;
        case AST_CHAR_TYPE :
            simple_type_info->type->kind = STK_BUILTIN_TYPE;
            simple_type_info->type->builtin_type= BT_CHAR;
            break;
        case AST_WCHAR_TYPE :
            simple_type_info->type->kind = STK_BUILTIN_TYPE;
            simple_type_info->type->builtin_type= BT_WCHAR;
            break;
        case AST_BOOL_TYPE :
            simple_type_info->type->kind = STK_BUILTIN_TYPE;
            simple_type_info->type->builtin_type= BT_BOOL;
            break;
        case AST_SHORT_TYPE :
            simple_type_info->type->kind = STK_BUILTIN_TYPE;
            simple_type_info->type->builtin_type= BT_INT;
            simple_type_info->type->is_short = 1;
            break;
        case AST_INT_TYPE :
            simple_type_info->type->kind = STK_BUILTIN_TYPE;
            simple_type_info->type->builtin_type= BT_INT;
            break;
        case AST_LONG_TYPE :
            simple_type_info->type->kind = STK_BUILTIN_TYPE;
            simple_type_info->type->builtin_type= BT_INT;
            simple_type_info->type->is_long = 1;
            break;
        case AST_SIGNED_TYPE :
            simple_type_info->type->kind = STK_BUILTIN_TYPE;
            simple_type_info->type->builtin_type = BT_INT;
            simple_type_info->type->is_signed = 1;
            break;
        case AST_UNSIGNED_TYPE :
            simple_type_info->type->kind = STK_BUILTIN_TYPE;
            simple_type_info->type->builtin_type = BT_INT;
            simple_type_info->type->is_unsigned = 1;
            break;
        case AST_FLOAT_TYPE :
            simple_type_info->type->kind = STK_BUILTIN_TYPE;
            simple_type_info->type->builtin_type = BT_FLOAT;
            break;
        case AST_DOUBLE_TYPE :
            simple_type_info->type->kind = STK_BUILTIN_TYPE;
            simple_type_info->type->builtin_type = BT_DOUBLE;
            break;
        case AST_VOID_TYPE :
            simple_type_info->type->kind = STK_BUILTIN_TYPE;
            simple_type_info->type->builtin_type = BT_VOID;
            break;
        case AST_AMBIGUITY :
            solve_ambiguous_type_specifier(a, st, decl_context);
            // Restart function
            gather_type_spec_information(a, st, simple_type_info, decl_context);
            break;
            // GCC Extensions
        case AST_GCC_TYPEOF :
        case AST_GCC_TYPEOF_EXPR :
            simple_type_info->type->kind = STK_TYPEOF;
            simple_type_info->type->typeof_expr = ASTSon0(a);
            break;
        default:
            internal_error("Unknown node '%s'", ast_print_node_type(ASTType(a)));
    }
}

static void gather_type_spec_from_elaborated_class_specifier(AST a, scope_t* st, type_t* type_info,
        decl_context_t decl_context)
{
    // AST class_key = ASTSon0(a);
    AST global_scope = ASTSon1(a);
    AST nested_name_specifier = ASTSon2(a);
    AST class_symbol = ASTSon3(a);

    scope_entry_list_t* result_list = NULL;

    scope_t* declarating_scope = st;

    lookup_flags_t lookup_flags = LF_ALWAYS_CREATE_SPECIALIZATION;

    CXX_LANGUAGE()
    {
        if (!BITMAP_TEST(decl_context.decl_flags, DF_FRIEND))
        {
            result_list = query_nested_name_flags(st, global_scope, nested_name_specifier, class_symbol,
                    NOFULL_UNQUALIFIED_LOOKUP, lookup_flags, decl_context);
        }
        else
        {
            result_list = query_nested_name_flags(st, global_scope, nested_name_specifier, class_symbol,
                    FULL_UNQUALIFIED_LOOKUP, lookup_flags, decl_context);
        }
    }

    C_LANGUAGE()
    {
        char* class_name = ASTText(class_symbol);

        class_name = strappend("struct ", class_name);

        result_list = query_unqualified_name(st, class_name);
    }

    // Now look for a type
    enum cxx_symbol_kind filter_classes[3] = {SK_CLASS, SK_TEMPLATE_PRIMARY_CLASS, SK_TEMPLATE_SPECIALIZED_CLASS};

    scope_entry_list_t* entry_list = filter_symbol_kind_set(result_list, 3, filter_classes);

    scope_entry_t* entry = (entry_list != NULL) ? entry_list->entry : NULL;

    if (entry != NULL)
    {
        if (entry->kind == SK_TEMPLATE_PRIMARY_CLASS
                && ASTType(class_symbol) == AST_TEMPLATE_ID
                && BITMAP_TEST(decl_context.decl_flags, DF_NO_DECLARATORS)
                && !BITMAP_TEST(decl_context.decl_flags, DF_FRIEND))
        {
            // A primary template has been chosen but we are declarating one
            // specialization
            DEBUG_CODE()
            {
                fprintf(stderr, "Even if a symbol has been found we will not consider it\n");
            }

            entry = NULL;
        }
    }

    scope_entry_t* new_class = NULL;
    if (entry == NULL)
    {
        // Create a stub but only if it is unqualified, otherwise it should exist elsewhere
        if (nested_name_specifier == NULL
                && global_scope == NULL)
        {
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

            new_class = new_symbol(st, class_name);

            DEBUG_CODE()
            {
                fprintf(stderr, "Type not found, creating a stub in scope %p for '%s' %p\n", 
                        st,
                        class_name,
                        new_class);
            }

            new_class->line = ASTLine(class_symbol);
			new_class->point_of_declaration = get_enclosing_declaration(class_symbol);

            new_class->type_information = calloc(1, sizeof(*(new_class->type_information)));
            new_class->type_information->kind = TK_DIRECT;
            new_class->type_information->type = calloc(1, sizeof(*(new_class->type_information->type)));
            new_class->type_information->type->kind = STK_CLASS;
            new_class->type_information->type->type_scope = copy_scope(st);
            

            new_class->type_information->type->class_info = calloc(1, 
                    sizeof(*(new_class->type_information->type->class_info)));

            type_info->type->kind = STK_USER_DEFINED;
            type_info->type->user_defined_type = new_class;

            if (!BITMAP_TEST(decl_context.decl_flags, DF_TEMPLATE) 
                    && ASTType(class_symbol) != AST_TEMPLATE_ID)
            {
                new_class->kind = SK_CLASS;
            }
            else // BITMAP_TEST(decl_context.decl_flags, DF_TEMPLATE) || ASTType(class_symbol) == AST_TEMPLATE_ID
            {

                if (ASTType(class_symbol) != AST_TEMPLATE_ID)
                {
                    // If this is new, there is no mix to be performed
                    new_class->template_parameter_info = decl_context.template_parameters;
                    new_class->num_template_parameters = decl_context.num_template_parameters;

                    new_class->kind = SK_TEMPLATE_PRIMARY_CLASS;
                    build_scope_template_arguments_for_primary_template(st, 
                            st->template_scope, 
                            new_class->template_parameter_info,
                            new_class->num_template_parameters, 
                            &(new_class->type_information->type->template_arguments));
                }
                else
                {
                    new_class->kind = SK_TEMPLATE_SPECIALIZED_CLASS;
                    build_scope_template_arguments(class_symbol, st, declarating_scope, st->template_scope,
                            &(new_class->type_information->type->template_arguments), decl_context);
                }
            }
        }
        else
        {
            DEBUG_CODE()
            {
                // Fix this
                fprintf(stderr, "Type not found but not creating it because it belongs to another scope\n");
            }
        }
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Class type found already declared in line %d, using it\n", entry->line);
        }

        type_info->type->kind = STK_USER_DEFINED;
        type_info->type->user_defined_type = entry;
        new_class = type_info->type->user_defined_type;

        if (BITMAP_TEST(decl_context.decl_flags, DF_EXPLICIT_SPECIALIZATION))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Symbol '%s' does not come from instantiation\n", 
                        entry->symbol_name);
            }
            entry->type_information->type->from_instantiation = 0;
        }
    }

    if (decl_context.template_parameters != NULL)
    {
        if (new_class->template_parameter_info != NULL)
        {
            ERROR_CONDITION((new_class->num_template_parameters != decl_context.num_template_parameters), 
                    "The number of template parameters declared here does not match with a previous declaration\n", 0);

            int i;
            for (i = 0; i < decl_context.num_template_parameters; i++)
            {
                switch (new_class->template_parameter_info[i]->kind)
                {
                    case TPK_TYPE :
                    case TPK_TEMPLATE :
                        {
                            if ((new_class->template_parameter_info[i]->default_tree != NULL
                                        && decl_context.template_parameters[i]->default_tree == NULL))
                            {
                                decl_context.template_parameters[i]->default_tree = 
                                    new_class->template_parameter_info[i]->default_tree;
                                decl_context.template_parameters[i]->default_argument_scope = 
                                    new_class->template_parameter_info[i]->default_argument_scope;
                            }
                            else if ((new_class->template_parameter_info[i]->default_tree == NULL
                                        && decl_context.template_parameters[i]->default_tree != NULL))
                            {
                                new_class->template_parameter_info[i]->default_tree = 
                                    decl_context.template_parameters[i]->default_tree;
                                new_class->template_parameter_info[i]->default_argument_scope = 
                                    decl_context.template_parameters[i]->default_argument_scope;
                            }
                            break;
                        }
                    case TPK_NONTYPE :
                        {
                            if (new_class->template_parameter_info[i]->default_tree != NULL
                                    && decl_context.template_parameters[i]->default_tree == NULL)
                            {
                                decl_context.template_parameters[i]->default_tree = 
                                    new_class->template_parameter_info[i]->default_tree;
                                decl_context.template_parameters[i]->default_argument_scope = 
                                    new_class->template_parameter_info[i]->default_argument_scope;
                            }
                            else if (new_class->template_parameter_info[i]->default_tree == NULL
                                    && decl_context.template_parameters[i]->default_tree != NULL)
                            {
                                new_class->template_parameter_info[i]->default_tree = 
                                    decl_context.template_parameters[i]->default_tree;
                                new_class->template_parameter_info[i]->default_argument_scope = 
                                    decl_context.template_parameters[i]->default_argument_scope;
                            }
                            break;
                        }
                    default :
                        {
                            internal_error("Unexpected template parameter type %d\n", 
                                    new_class->template_parameter_info[i]->kind);
                        }
                }
            }

            // Just overwrite
            new_class->template_parameter_info = decl_context.template_parameters;
            new_class->num_template_parameters = decl_context.num_template_parameters;
        }
    }
}

static void gather_type_spec_from_elaborated_enum_specifier(AST a, scope_t* st, type_t* type_info, decl_context_t decl_context)
{
    AST global_scope = ASTSon0(a);
    AST nested_name_specifier = ASTSon1(a);
    AST symbol = ASTSon2(a);

    scope_entry_list_t* result_list = NULL;

    CXX_LANGUAGE()
    {
        result_list = query_nested_name(st, global_scope, nested_name_specifier, symbol,
                NOFULL_UNQUALIFIED_LOOKUP, decl_context);
    }

    C_LANGUAGE()
    {
        char* enum_name = ASTText(symbol);

        enum_name = strappend("enum ", enum_name);
        result_list = query_unqualified_name(st, enum_name);
    }

    // Look for an enum name
    scope_entry_t* entry = NULL;
    result_list = filter_symbol_kind(result_list, SK_ENUM);

    if (result_list != NULL)
    {
        entry = result_list->entry;
    }

    if (entry == NULL)
    {
        // Create a stub but only if it is unqualified, otherwise it should exist anywhere
        if (nested_name_specifier == NULL
                && global_scope == NULL)
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

            scope_entry_t* new_class = new_symbol(st, enum_name);
            new_class->line = ASTLine(symbol);
			new_class->point_of_declaration = get_enclosing_declaration(symbol);
            new_class->kind = SK_ENUM;
            new_class->type_information = calloc(1, sizeof(*(new_class->type_information)));
            new_class->type_information->kind = TK_DIRECT;
            new_class->type_information->type = calloc(1, sizeof(*(new_class->type_information->type)));
            new_class->type_information->type->kind = STK_ENUM;
            new_class->type_information->type->type_scope = copy_scope(st);

            type_info->type->kind = STK_USER_DEFINED;
            type_info->type->user_defined_type = new_class;
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Enum type not found but not creating it because it belongs to another scope\n");
            }
        }
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Enum type found, using it\n");
        }

        type_info->type->kind = STK_USER_DEFINED;
        type_info->type->user_defined_type = entry;
    }
}

static void gather_type_spec_from_dependent_typename(AST a, scope_t* st, type_t* simple_type_info,
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
            solve_possibly_ambiguous_template_id(class_name, st, decl_context);
        }

        nested_name_spec = ASTSon1(nested_name_spec);
    }

    if (ASTType(name) == AST_TEMPLATE_ID)
    {
        solve_possibly_ambiguous_template_id(name, st, decl_context);
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "Trying to look up a dependent typename '%s'\n",
				prettyprint_in_buffer(a));
    }

    global_scope = ASTSon0(a);
    nested_name_spec = ASTSon1(a);
    name = ASTSon2(a);

    lookup_flags_t lookup_flags = LF_NO_FAIL;

    if (decl_context.template_nesting != 0)
	{
		DEBUG_CODE()
		{
			fprintf(stderr, "We will not instantiate when solving this dependent typename because we are under template hood\n");
		}
		lookup_flags |= LF_NO_INSTANTIATE;
	}

    scope_entry_list_t* result = query_nested_name_flags(st, global_scope, nested_name_spec, name, 
            FULL_UNQUALIFIED_LOOKUP, lookup_flags, decl_context);

    char is_dependent = 0;
    if (result != NULL
            && result->entry->kind != SK_DEPENDENT_ENTITY
            && !(is_dependent = is_dependent_type(result->entry->type_information, decl_context)))
    {
        scope_entry_t* entry = result->entry;

        if (entry->kind != SK_TYPEDEF)
        {
            simple_type_info->type->kind = STK_USER_DEFINED;
            simple_type_info->type->user_defined_type = result->entry;
        }
        else
        {
            *simple_type_info = *entry->type_information->type->aliased_type;
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

    simple_type_info->type->kind = STK_TEMPLATE_DEPENDENT_TYPE;
    simple_type_info->type->typeof_expr = a;
    simple_type_info->type->typeof_scope = copy_scope(st);
}

/*
 * This routine is called in gather_type_spec_information and its purpose is to fill the simple_type
 * with the proper reference of the user defined type.
 */
static void gather_type_spec_from_simple_type_specifier(AST a, scope_t* st, type_t* simple_type_info,
        decl_context_t decl_context)
{
    AST global_op = ASTSon0(a);
    AST nested_name_spec = ASTSon1(a);
    AST type_name = ASTSon2(a) != NULL ? ASTSon2(a) : ASTSon3(a);

    if (ASTType(type_name) == AST_TEMPLATE_ID)
    {
        solve_possibly_ambiguous_template_id(type_name, st, decl_context);
    }
    
    scope_entry_list_t* entry_list = query_nested_name(st, global_op, nested_name_spec, 
            type_name, FULL_UNQUALIFIED_LOOKUP, decl_context);

    ERROR_CONDITION((entry_list == NULL), "The list of types of type '%s' is empty! (%s)\n", 
            prettyprint_in_buffer(a), node_information(a));

    // Filter for non types hiding this type name
    // Fix this, it sounds a bit awkward
    scope_entry_t* simple_type_entry = filter_simple_type_specifier(entry_list);

    ERROR_CONDITION((simple_type_entry == NULL), "Identifier '%s' in %s is not a type\n", 
            ASTText(type_name), node_information(type_name));

    ERROR_CONDITION((simple_type_entry->type_information == NULL 
                || simple_type_entry->type_information->kind != TK_DIRECT 
                || simple_type_entry->type_information->type == NULL), 
            "The named type '%s' has no direct type entry in symbol table\n", ASTText(type_name));

    // if (simple_type_entry->kind != SK_TYPEDEF)
    {
        simple_type_info->type->kind = STK_USER_DEFINED;
        simple_type_info->type->user_defined_type = simple_type_entry;

        // It is useful to save this when printing types
        simple_type_info->type->typeof_expr = a;
        simple_type_info->type->typeof_scope = copy_scope(st);
    }
    // TODO - Check this, I think doing it is unnecessary but should
    // be checked against all the C++ oddities
    // else
    // {
    //     // Bitwise copy, cv-qualification will be in this simple_type_info
    //     // *simple_type_info = *simple_type_entry->type_information->type->aliased_type;
    //     *simple_type_info = *simple_type_entry->type_information;
    // }
}

/*
 * This function is called for enum specifiers. It saves all enumerated values
 * and if it has been given a name, it is registered in the scope.
 */
void gather_type_spec_from_enum_specifier(AST a, scope_t* st, type_t* simple_type_info,
        decl_context_t decl_context)
{
    simple_type_info->type->enum_info = (enum_info_t*) calloc(1, sizeof(*simple_type_info->type->enum_info));

    simple_type_info->type->kind = STK_ENUM;

    simple_type_info->type->type_scope = st;

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

        scope_entry_list_t* enum_entry_list = query_unqualified_name(st, enum_name_str);

        scope_entry_t* new_entry;
            
        if (enum_entry_list != NULL 
                && enum_entry_list->entry->kind == SK_ENUM 
                && enum_entry_list->next == NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Enum '%s' already declared in %p\n", enum_name_str, st);
            }

            new_entry = enum_entry_list->entry;
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Registering enum '%s' in %p\n", enum_name_str, st);
            }

            new_entry = new_symbol(st, enum_name_str);
            new_entry->line = ASTLine(enum_name);
			new_entry->point_of_declaration = enum_name;
            new_entry->kind = SK_ENUM;
        }

        // Copy the type because we are creating it and we would clobber it
        // otherwise
        new_entry->type_information = copy_type(simple_type_info);
        new_entry->defined = 1;

        // Since this type is not anonymous we'll want that simple_type_info
        // refers to this newly created type
        memset(simple_type_info->type, 0, sizeof(*(simple_type_info->type)));
        simple_type_info->type->kind = STK_USER_DEFINED;
        simple_type_info->type->user_defined_type = new_entry;
    }

    AST list, iter;
    list = ASTSon1(a);
    
    if (list != NULL)
    {
        // This is a bit convoluted. Basically simple_enumerator_type is just
        // an arrow to the true SK_ENUM type, but we cannot use directly
        // enumerator_type because we would be clobbering the original symbol
        // type information.
        
        type_t* enumerator_type = simple_type_info;
        simple_type_t* simple_enumerator_type = enumerator_type->type;

        // If the type had name, refer to the enum type
        if (enumerator_type->type->kind == STK_USER_DEFINED)
        {
            simple_enumerator_type = enumerator_type->type->user_defined_type->type_information->type;
        }

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

            scope_entry_t* enumeration_item = new_symbol(st, ASTText(enumeration_name));
            enumeration_item->line = ASTLine(enumeration_name);
			enumeration_item->point_of_declaration = get_enclosing_declaration(enumeration_name);
            enumeration_item->kind = SK_ENUMERATOR;
            enumeration_item->type_information = enumerator_type;

            if (enumeration_expr != NULL)
            {
                solve_possibly_ambiguous_expression(enumeration_expr, st, decl_context);

                AST duplicate_expr = duplicate_ast(enumeration_expr);

                AST fake_initializer = ASTMake1(AST_CONSTANT_INITIALIZER, duplicate_expr, ASTLine(duplicate_expr), NULL);
                enumeration_item->expression_value = fake_initializer;

            }

            P_LIST_ADD(simple_enumerator_type->enum_info->enumeration_list, 
                    simple_enumerator_type->enum_info->num_enumeration,
                    enumeration_item);
        }

    }

}

void build_scope_base_clause(AST base_clause, scope_t* st, scope_t* class_scope, 
        class_info_t* class_info, decl_context_t decl_context)
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

        switch (ASTType(base_specifier))
        {
            case AST_BASE_SPECIFIER :
                {
                    global_op = ASTSon0(base_specifier);
                    nested_name_specifier = ASTSon1(base_specifier);
                    name = ASTSon2(base_specifier);
                    break;
                }
            case AST_BASE_SPECIFIER_VIRTUAL :
            case AST_BASE_SPECIFIER_ACCESS_VIRTUAL :
                {
                    is_virtual = 1;
                    /* Fall through */
                }
            case AST_BASE_SPECIFIER_ACCESS :
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

        // Create a convenience simple type specifier
#if 0
        AST simple_type_specifier = 
            ASTMake3(AST_SIMPLE_TYPE_SPECIFIER, 
                    duplicate_ast(global_op), 
                    duplicate_ast(nested_name_specifier), 
                    duplicate_ast(name), 
                    ASTLine(name), NULL);

        AST type_specifier_seq = ASTMake3(AST_TYPE_SPECIFIER_SEQ,
                NULL, simple_type_specifier, NULL, ASTLine(simple_type_specifier), NULL);

        type_t* type_info = NULL;
        gather_decl_spec_t gather_info;
        memset(&gather_info, 0, sizeof(gather_info));

        build_scope_decl_specifier_seq(type_specifier_seq, st, &gather_info, &type_info, decl_context);

        
        // Replace the fixed nodes
        if (global_op != NULL)
        {
            *global_op = *(ASTSon0(simple_type_specifier));
        }

        if (nested_name_specifier != NULL)
        {
            *nested_name_specifier = *(ASTSon1(simple_type_specifier));
        }

        if (name != NULL)
        {
            *name = *(ASTSon2(simple_type_specifier));
        }

        // This is always a type name
        if (type_info == NULL)
        {
            internal_error("Base type '%s' not found (%s)\n", 
                    prettyprint_in_buffer(base_specifier),
                    node_information(base_specifier));
        }
        
        if (type_info->kind != TK_DIRECT)
        {
            internal_error("Base type '%s' is not a direct type but kind type %d (%s)\n", 
                    prettyprint_in_buffer(base_specifier),
                    type_info->kind,
                    node_information(base_specifier));
        }

        if (type_info->type->kind != STK_USER_DEFINED
                && type_info->type->kind != STK_TEMPLATE_DEPENDENT_TYPE)
        {
            internal_error("Base type '%s' is not type name but kind type %d (%s)\n", 
                    prettyprint_in_buffer(base_specifier), 
                    type_info->type->kind,
                    node_information(base_specifier));
        }

        if (type_info->type->kind == STK_TEMPLATE_DEPENDENT_TYPE)
        {
            // This does not have any further information
            continue;
        }


#endif
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

        scope_entry_list_t* result_list = query_nested_name_flags(st, global_op, nested_name_specifier, name, 
                FULL_UNQUALIFIED_LOOKUP, LF_INSTANTIATE, decl_context);
        result_list = filter_symbol_kind_set(result_list, 7, filter);

        ERROR_CONDITION((result_list == NULL), "Base class '%s' not found!\n", prettyprint_in_buffer(base_specifier));
        scope_entry_t* result = result_list->entry;
        result = give_real_entry(result);

        if (!is_dependent_type(result->type_information, decl_context))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Base class '%s' IS NOT a dependent type\n", prettyprint_in_buffer(base_specifier));
            }
            if (result->kind == SK_TEMPLATE_SPECIALIZED_CLASS)
            {
                if (!result->type_information->type->from_instantiation)
                {
                    scope_entry_list_t* templates_available = query_unqualified_name(result->scope, result->symbol_name);
                    ERROR_CONDITION((templates_available == NULL), "I did not find myself!\n", 0);

                    templates_available = filter_entry_from_list(templates_available, result);

                    matching_pair_t* match_pair = solve_template(templates_available, 
                            result->type_information->type->template_arguments, result->scope, 
                            /* give_exact_match = */ 0, decl_context);

                    if (match_pair != NULL)
                    {
                        instantiate_template_in_symbol(result, match_pair, 
                                result->type_information->type->template_arguments, result->scope,
								decl_context);
                    }
                }
            }

            if (result->related_scope != NULL)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Adding scope %p as base number %d\n", 
                            result->related_scope, class_scope->num_base_scopes);
                }

                // print_scope(result->related_scope, 0);
                scope_t* base_scope = copy_scope(result->related_scope);

                base_scope->contained_in = NULL;

                P_LIST_ADD(class_scope->base_scope, class_scope->num_base_scopes, base_scope);
            }
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Base class '%s' found IS a dependent type\n", prettyprint_in_buffer(base_specifier));
            }
        }

        /* Now add virtual inherited classes */
        class_info_t* base_class_info = result->type_information->type->class_info;

        if (base_class_info != NULL)
        {
            int i;
            for (i = 0; i < base_class_info->num_bases; i++)
            {
                base_class_info_t* base_of_base_class = base_class_info->base_classes_list[i];

                if (base_of_base_class->is_virtual)
                {
                    P_LIST_ADD_ONCE(class_info->base_classes_list, class_info->num_bases, base_of_base_class);
                }
            }
        }

        base_class_info_t* new_base_class = calloc(1, sizeof(*new_base_class));
        new_base_class->class_symbol = result;
        new_base_class->class_type = result->type_information;
        new_base_class->is_virtual = is_virtual;

        P_LIST_ADD(class_info->base_classes_list, class_info->num_bases, new_base_class);
    }
}

/*
 * This function is called for class specifiers
 */
void gather_type_spec_from_class_specifier(AST a, scope_t* st, type_t* simple_type_info,
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

    simple_type_info->type->class_info = calloc(1, sizeof(*simple_type_info->type->class_info));
    simple_type_info->type->kind = STK_CLASS;

    scope_t* inner_scope = new_class_scope(st);

    // Save the inner scope in the class type
    // (it is used when checking member acesses)
    simple_type_info->type->class_info->inner_scope = inner_scope;

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
            
            CXX_LANGUAGE()
            {
                class_entry_list = query_nested_name_flags(st, NULL, 
                        class_head_nested_name, class_head_identifier,
                        NOFULL_UNQUALIFIED_LOOKUP, LF_ALWAYS_CREATE_SPECIALIZATION,
                        decl_context);
            }

            C_LANGUAGE()
            {
                // This can only be an AST_SYMBOL in C
                char* class_name = ASTText(class_head_identifier);
                class_name = strappend("struct ", class_name);

                class_entry_list = query_unqualified_name(st, class_name);
            }

            enum cxx_symbol_kind filter_classes[3] = 
            {
                SK_CLASS, 
                SK_TEMPLATE_PRIMARY_CLASS, 
                SK_TEMPLATE_SPECIALIZED_CLASS
            };

            class_entry_list = filter_symbol_kind_set(class_entry_list, 3, filter_classes);

            if (class_entry_list != NULL 
                    && ((class_entry_list->entry->kind == SK_TEMPLATE_PRIMARY_CLASS
                            && ASTType(class_head_identifier) == AST_TEMPLATE_ID)))
            {
                // We have found a primary template but we are specializing
                class_entry_list = NULL;
            }
            
            if (class_entry_list != NULL)
            {
                // Get the class entry
                class_entry = class_entry_list->entry;

                DEBUG_CODE()
                {
                    fprintf(stderr, "Class '");
                    prettyprint(stderr, class_head_nested_name);
                    prettyprint(stderr, class_head_identifier);
                    fprintf(stderr, "' already declared as %p in scope %p (line %d)\n", class_entry, st,
                        class_entry->line);
                }

                st = class_entry->scope;
                inner_scope = new_class_scope(st);

                // Get its simple type info and adjust its scope
                simple_type_info->type = class_entry->type_information->type;
                simple_type_info->type->class_info->inner_scope = inner_scope;
            }
            else if (class_entry_list == NULL
                    && class_head_nested_name == NULL)
            {
                if (ASTType(class_head_identifier) == AST_SYMBOL)
                {
                    C_LANGUAGE()
                    {
                        char* class_name = ASTText(class_head_identifier);
                        class_name = strappend("struct ", class_name);

                        class_entry = new_symbol(st, class_name);
                    }

                    CXX_LANGUAGE()
                    {
                        class_entry = new_symbol(st, ASTText(class_head_identifier));
                    }
                }
                else
                {
                    class_entry = new_symbol(st, ASTText(ASTSon0(class_head_identifier)));
                }

                DEBUG_CODE()
                {
                    fprintf(stderr, "Registering class '");
                    prettyprint(stderr, class_head_nested_name);
                    prettyprint(stderr, class_head_identifier);
                    fprintf(stderr, "' (%p) in scope %p\n", class_entry, st);
                }

                class_entry->line = ASTLine(class_head_identifier);
				class_entry->point_of_declaration = get_enclosing_declaration(class_head_identifier);

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
                    else // AST_TEMPLATE_ID
                    {
                        class_entry->kind = SK_TEMPLATE_SPECIALIZED_CLASS;
                    }
                }
            }
            else
            {
                internal_error("Unreachable code", 0);
            }

			// This code is completely ugly
            if (decl_context.template_parameters != NULL)
            {
                if (class_entry->template_parameter_info == NULL)
                {
                    // Just overwrite
                    class_entry->template_parameter_info = decl_context.template_parameters;
                    class_entry->num_template_parameters = decl_context.num_template_parameters;
                }
                else // Otherwise first mix them
                {
                    ERROR_CONDITION((class_entry->num_template_parameters != decl_context.num_template_parameters), 
							"The number of template parameters declared in %s is %d and does not match with a previous " 
                            "declaration in line=%d of %d parameters\n", 
                            node_information(a), 
                            decl_context.num_template_parameters,
                            class_entry->line,
                            class_entry->num_template_parameters);

                    int i;
                    for (i = 0; i < decl_context.num_template_parameters; i++)
                    {
                        switch (class_entry->template_parameter_info[i]->kind)
                        {
                            case TPK_TYPE :
                            case TPK_TEMPLATE :
                                {
                                    if ((class_entry->template_parameter_info[i]->default_tree != NULL
                                                && decl_context.template_parameters[i]->default_tree == NULL))
                                    {
                                        decl_context.template_parameters[i]->default_tree = 
                                            class_entry->template_parameter_info[i]->default_tree;
                                        decl_context.template_parameters[i]->default_argument_scope = 
                                            class_entry->template_parameter_info[i]->default_argument_scope;
                                    }
                                    else if ((class_entry->template_parameter_info[i]->default_tree == NULL
                                                && decl_context.template_parameters[i]->default_tree != NULL))
                                    {
                                        class_entry->template_parameter_info[i]->default_tree = 
                                            decl_context.template_parameters[i]->default_tree;
                                        class_entry->template_parameter_info[i]->default_argument_scope = 
                                            decl_context.template_parameters[i]->default_argument_scope;
                                    }
                                    break;
                                }
                            case TPK_NONTYPE :
                                {
                                    if (class_entry->template_parameter_info[i]->default_tree != NULL
                                            && decl_context.template_parameters[i]->default_tree == NULL)
                                    {
                                        decl_context.template_parameters[i]->default_tree = 
                                            class_entry->template_parameter_info[i]->default_tree;
                                        decl_context.template_parameters[i]->default_argument_scope = 
                                            class_entry->template_parameter_info[i]->default_argument_scope;
                                    }
                                    else if (class_entry->template_parameter_info[i]->default_tree == NULL
                                            && decl_context.template_parameters[i]->default_tree != NULL)
                                    {
                                        class_entry->template_parameter_info[i]->default_tree = 
                                            decl_context.template_parameters[i]->default_tree;
                                        class_entry->template_parameter_info[i]->default_argument_scope = 
                                            decl_context.template_parameters[i]->default_argument_scope;
                                    }
                                    break;
                                }
                            default :
                                {
                                    internal_error("Unexpected template parameter type %d\n", 
                                            class_entry->template_parameter_info[i]->kind);
                                }
                        }
                    }

                    // Just overwrite
                    class_entry->template_parameter_info = decl_context.template_parameters;
                    class_entry->num_template_parameters = decl_context.num_template_parameters;
                }
            }

            // Gather template argument information
            switch (class_entry->kind)
            {
                case SK_TEMPLATE_PRIMARY_CLASS :
                    {
                        build_scope_template_arguments_for_primary_template(st, 
                                st->template_scope,
                                class_entry->template_parameter_info, 
                                class_entry->num_template_parameters,
                                &(simple_type_info->type->template_arguments));
                        break;
                    }
                case SK_TEMPLATE_SPECIALIZED_CLASS :
                    {
                        build_scope_template_arguments(class_head_identifier, st, st, st->template_scope, 
                                &(simple_type_info->type->template_arguments), decl_context);
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
            simple_type_info->type->template_class_base_clause = base_clause;
            simple_type_info->type->template_class_body = ASTSon1(a);

            // Copy the type because we are creating it and we would clobber it
            // otherwise
            class_entry->type_information = copy_type(simple_type_info);
            class_entry->related_scope = inner_scope;

            // Clear this flag
            DEBUG_CODE()
            {
                    fprintf(stderr, "Symbol '%s' does not come from instantiation\n",
                            class_entry->symbol_name);
            }
            class_entry->type_information->type->from_instantiation = 0;

            if (BITMAP_TEST(decl_context.decl_flags, DF_EXPLICIT_SPECIALIZATION))
            {
                // Unless this is an explicit specialization that is in a way already instantiated
                DEBUG_CODE()
                {
                    fprintf(stderr, "Symbol '%s' explicitly instantiated\n",
                            class_entry->symbol_name);
                }
                class_entry->type_information->type->from_instantiation = 1;
            }

            // Since this type is not anonymous we'll want that simple_type_info
            // refers to this newly created type
            memset(simple_type_info->type, 0, sizeof(*(simple_type_info->type)));
            simple_type_info->type->kind = STK_USER_DEFINED;
            simple_type_info->type->user_defined_type = class_entry;
        }
        else
        {
            internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(class_head_identifier)));
        }
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
	scope_link_set(compilation_options.scope_link, a, copy_scope(inner_scope));
    
    // Now add the bases
    if (base_clause != NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Adding the bases of this class\n");
        }

        build_scope_base_clause(base_clause, st, inner_scope, 
                class_entry->type_information->type->class_info, decl_context);

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
            scope_entry_t* injected_symbol = new_symbol(inner_scope, class_entry->symbol_name);

            *injected_symbol = *class_entry;
            injected_symbol->do_not_print = 1;

            injected_symbol->injected_class_name = 1;
            injected_symbol->injected_class_referred_symbol = class_entry;
        }
    }

    AST member_specification = ASTSon1(a);
    build_scope_member_specification(inner_scope, member_specification, 
            current_access, simple_type_info, decl_context);
    
    if (class_entry != NULL)
    {
        // If the class had a name, it is completely defined here
        class_entry->defined = 1;
    }
}


void build_scope_member_specification(scope_t* inner_scope, AST member_specification_tree, 
        access_specifier_t default_current_access, type_t* simple_type_info, 
        decl_context_t decl_context)
{
    // Member specification
    access_specifier_t current_access = default_current_access;
    decl_context_t new_decl_context = decl_context;
    new_decl_context.decl_flags &= ~(DF_TEMPLATE);
    new_decl_context.decl_flags &= ~(DF_EXPLICIT_SPECIALIZATION);

    new_decl_context.num_template_parameters = 0;
    new_decl_context.template_parameters = NULL;

    // AST member_specification = member_specification_tree;

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
			build_scope_member_declaration(member_specification, inner_scope, 
					current_access, simple_type_info, /* step= */ 0, new_decl_context);
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
			build_scope_member_declaration(member_specification, inner_scope, 
					current_access, simple_type_info, /* step= */ 1, new_decl_context);
		}
	}
}


/*
 * This function creates a full type using the declarator tree in "a".
 *
 * The base type is fetched from "simple_type_info" and then
 * build_scope_declarator_rec will modify this type to properly represent the
 * correct type.
 *
 * I.e.   int (*f)();
 *
 * Has as a base type "int", but after build_scope_declarator_rec it will be
 * "pointer to function returning int"
 *
 * If the declarator is not abstract, therefore it has a name,
 * build_scope_declarator_name is called to sign it up in the symbol table.
 */
scope_entry_t* build_scope_declarator(AST a, scope_t* st, 
        gather_decl_spec_t* gather_info, type_t* simple_type_info, type_t** declarator_type,
        decl_context_t decl_context)
{
    scope_entry_t* entry = build_scope_declarator_with_parameter_scope(a, 
            st, NULL, gather_info, simple_type_info, declarator_type, decl_context);

    return entry;
}

static scope_entry_t* build_scope_declarator_with_parameter_scope(AST a, scope_t* st, scope_t** parameters_scope, 
        gather_decl_spec_t* gather_info, type_t* simple_type_info, type_t** declarator_type,
        decl_context_t decl_context)
{
    scope_entry_t* entry = NULL;
    // Set base type
    *declarator_type = simple_type_info;

    AST declarator_name = get_declarator_name(a, st, decl_context);

    scope_t* decl_st = st;
    if (declarator_name != NULL)
    {
        if (ASTType(declarator_name) == AST_QUALIFIED_ID
                || ASTType(declarator_name) == AST_QUALIFIED_TEMPLATE)
        {
            char is_dependent = 0;
            decl_st = query_nested_name_spec_flags(st, ASTSon0(declarator_name), ASTSon1(declarator_name), 
                    NULL, &is_dependent, LF_INSTANTIATE, decl_context);

            if (decl_st == NULL)
            {
                WARNING_MESSAGE("Scope of the qualified declarator not found, falling back to current scope", 0);
                decl_st = st;
            }
        }
        else if(ASTType(declarator_name) == AST_QUALIFIED_OPERATOR_FUNCTION_ID)
        {
            decl_st = compilation_options.global_scope;
        }

		ASTAttrSetValueType(a, LANG_IS_DECLARED_NAME, tl_type_t, tl_bool(1));
		ASTAttrSetValueType(a, LANG_DECLARED_NAME, tl_type_t, tl_ast(declarator_name));

		scope_link_set(compilation_options.scope_link, declarator_name, copy_scope(decl_st));
    }

    build_scope_declarator_rec(a, decl_st, parameters_scope, declarator_type, 
            gather_info, &declarator_name, decl_context);
    
    if (declarator_name != NULL)
    {
        // Special case for conversion function ids
        // We fix the return type according to the standard
        if ((*declarator_type)->kind == TK_FUNCTION
                && (*declarator_type)->function->return_type == NULL)
        {
            // This looks like a conversion function id
            AST id_expression = ASTSon0(declarator_name);

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
                get_conversion_function_name(conversion_function_id, st, &conversion_function_type, decl_context);

                (*declarator_type)->function->return_type = conversion_function_type;
            }
        }

        entry = build_scope_declarator_name(declarator_name, *declarator_type, gather_info, st, decl_context);

        DEBUG_CODE()
        {
            fprintf(stderr, "declaring '%s' as ",
                    prettyprint_in_buffer(declarator_name));
        }
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "%s\n", print_declarator(*declarator_type, st));
    }

    return entry;
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
static void set_pointer_type(type_t** declarator_type, scope_t* st, AST pointer_tree, 
        decl_context_t decl_context)
{
    type_t* pointee_type = *declarator_type;

    (*declarator_type) = calloc(1, sizeof(*(*declarator_type)));
    (*declarator_type)->pointer = calloc(1, sizeof(*((*declarator_type)->pointer)));
    (*declarator_type)->pointer->pointee = pointee_type;

    switch (ASTType(pointer_tree))
    {
        case AST_POINTER_SPEC :
            if (ASTSon0(pointer_tree) == NULL
                    && ASTSon1(pointer_tree) == NULL)
            {
                (*declarator_type)->kind = TK_POINTER;
            }
            else
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "---> POINTER TO MEMBER <---\n");
                }

                (*declarator_type)->kind = TK_POINTER_TO_MEMBER;

                scope_entry_list_t* entry_list = NULL;

                AST global_op = ASTSon0(pointer_tree);
                AST nested_name_spec = NULL;
                AST unqualified_id = NULL;
                convert_tree_from_nested_name_to_qualified_id(ASTSon1(pointer_tree), &nested_name_spec, &unqualified_id);

                entry_list = query_nested_name(st, global_op, nested_name_spec,
                        unqualified_id, FULL_UNQUALIFIED_LOOKUP, decl_context);

                if (entry_list != NULL)
                {
                    (*declarator_type)->pointer->pointee_class = entry_list->entry;
                }
                else
                {
                }
            }
            // (*declarator_type)->pointer->cv_qualifier = compute_cv_qualifier(ASTSon2(pointer_tree));
            (*declarator_type)->cv_qualifier = compute_cv_qualifier(ASTSon2(pointer_tree));
            break;
        case AST_REFERENCE_SPEC :
            (*declarator_type)->kind = TK_REFERENCE;
            break;
        case AST_GCC_REFERENCE_SPEC :
            (*declarator_type)->kind = TK_REFERENCE;
            (*declarator_type)->cv_qualifier = compute_cv_qualifier(ASTSon0(pointer_tree));
            break;
        default :
            internal_error("Unhandled node type '%s'\n", ast_print_node_type(ASTType(pointer_tree)));
            break;
    }

    (*declarator_type)->function = NULL;
    (*declarator_type)->array = NULL;
    (*declarator_type)->type = NULL;
}

/*
 * This function converts a type "T" to a "array x of T"
 */
static void set_array_type(type_t** declarator_type, scope_t* st, 
        AST constant_expr, decl_context_t decl_context)
{
    type_t* element_type = *declarator_type;

    if (constant_expr != NULL)
    {
        solve_possibly_ambiguous_expression(constant_expr, st, decl_context);
    }

    (*declarator_type) = calloc(1, sizeof(*(*declarator_type)));
    (*declarator_type)->kind = TK_ARRAY;
    (*declarator_type)->array = calloc(1, sizeof(*((*declarator_type)->array)));
    (*declarator_type)->array->element_type = element_type;
    (*declarator_type)->array->array_expr = constant_expr;
    (*declarator_type)->array->array_expr_scope = copy_scope(st);

    (*declarator_type)->function = NULL;
    (*declarator_type)->type = NULL;
    (*declarator_type)->pointer = NULL;
}

/*
 * This function fetches information for every declarator in the
 * parameter_declaration_clause of a functional declarator
 */
static void set_function_parameter_clause(type_t* declarator_type, scope_t* st, 
        scope_t** parameter_sc, AST parameters, decl_context_t decl_context)
{
    declarator_type->function->num_parameters = 0;
    declarator_type->function->parameter_list = NULL;
    
    // An empty parameter declaration clause is like (void) in C++
    if (ASTType(parameters) == AST_EMPTY_PARAMETER_DECLARATION_CLAUSE)
    {
        // Maybe this needs some kind of fixing
        return;
    }

    AST iter, list;
    list = parameters;
    
    // Do not contaminate the current symbol table
    scope_t* parameters_scope;
    parameters_scope = new_prototype_scope(st);

    // Save this parameter scope
    if (parameter_sc != NULL)
    {
        *parameter_sc = parameters_scope;
    }

    C_LANGUAGE()
    {
        // Nothing to do here with K&R parameters
        if (ASTType(parameters) == AST_KR_PARAMETER_LIST)
        {
            return;
        }
    }

	// Link the scope of the parameters
	scope_link_set(compilation_options.scope_link, parameters, copy_scope(parameters_scope));

    for_each_element(list, iter)
    {
        AST parameter_declaration = ASTSon1(iter);

        if (ASTType(parameter_declaration) == AST_AMBIGUITY)
        {
            solve_ambiguous_parameter_decl(parameter_declaration, st, decl_context);
            ERROR_CONDITION((ASTType(parameter_declaration) == AST_AMBIGUITY), "Ambiguity not solved %s", 
                    node_information(parameter_declaration));
        }

        if (ASTType(parameter_declaration) == AST_VARIADIC_ARG)
        {
            parameter_info_t* new_parameter = calloc(1, sizeof(*new_parameter));
            new_parameter->is_ellipsis = 1;

            P_LIST_ADD(declarator_type->function->parameter_list, declarator_type->function->num_parameters, new_parameter);
            continue;
        }

        // This is never null
        AST parameter_decl_spec_seq = ASTSon0(parameter_declaration);
        // Declarator can be null
        AST parameter_declarator = ASTSon1(parameter_declaration);
        // Default value can be null
        // The scope of this parameter declaration should be "st" and not parameters_scope
        AST default_argument = ASTSon2(parameter_declaration);

        if (default_argument != NULL)
        {
            solve_possibly_ambiguous_expression(default_argument, st, decl_context);
        }

        gather_decl_spec_t gather_info;
        memset(&gather_info, 0, sizeof(gather_info));
        
        type_t* simple_type_info;

        build_scope_decl_specifier_seq(parameter_decl_spec_seq, parameters_scope, &gather_info, &simple_type_info,
                decl_context);

        // It is valid in a function declaration not having a declarator at all
        // (note this is different from having an abstract declarator).
        //
        // int f(int, int*);
        //
        // The first "int" does not contain any declarator while the second has
        // an abstract one

        // If we have a declarator compute its type
        if (parameter_declarator != NULL)
        {
            type_t* type_info;
            scope_entry_t* entry = build_scope_declarator(parameter_declarator, parameters_scope, 
                    &gather_info, simple_type_info, &type_info, decl_context);

            if (entry != NULL)
            {
                entry->defined = 1;
            }

			AST declarator_name = get_declarator_name(parameter_declarator, st, decl_context);
			if (declarator_name != NULL)
			{
				ASTAttrSetValueType(parameter_declarator, LANG_IS_DECLARED_PARAMETER, tl_type_t, tl_bool(1));
			}

            parameter_info_t* new_parameter = calloc(1, sizeof(*new_parameter));
            new_parameter->type_info = type_info;
            new_parameter->default_argument = default_argument;

            P_LIST_ADD(declarator_type->function->parameter_list, 
                    declarator_type->function->num_parameters, new_parameter);
        }
        // If we don't have a declarator just save the base type
        else
        {
            type_t* type_info = simple_type_info;

            parameter_info_t* new_parameter = calloc(1, sizeof(*new_parameter));
            new_parameter->type_info = type_info;
            new_parameter->default_argument = default_argument;

            P_LIST_ADD(declarator_type->function->parameter_list, declarator_type->function->num_parameters, new_parameter);
        }
    }

    if (declarator_type->function->num_parameters == 1
            && !declarator_type->function->parameter_list[0]->is_ellipsis)
    {
        type_t* parameter_type = declarator_type->function->parameter_list[0]->type_info;

        if (parameter_type->kind == TK_DIRECT
                && parameter_type->type->kind == STK_BUILTIN_TYPE
                && parameter_type->type->builtin_type == BT_VOID)
        {
            // This list was really empty
            declarator_type->function->num_parameters = 0;
            declarator_type->function->parameter_list = NULL;
        }
    }
}

/*
 * This function converts a type "T" into a "function (...) returning T" type
 */
static void set_function_type(type_t** declarator_type, scope_t* st, scope_t** parameters_scope, 
        gather_decl_spec_t* gather_info, AST parameter, AST cv_qualif, AST except_spec, 
        decl_context_t decl_context)
{
    type_t* returning_type = *declarator_type;

    (*declarator_type) = calloc(1, sizeof(*(*declarator_type)));
    (*declarator_type)->kind = TK_FUNCTION;
    (*declarator_type)->function = calloc(1, sizeof(*((*declarator_type)->function)));
    (*declarator_type)->function->return_type = returning_type;

    set_function_parameter_clause(*declarator_type, st, parameters_scope, parameter, decl_context);

    // (*declarator_type)->function->cv_qualifier = compute_cv_qualifier(cv_qualif);
    (*declarator_type)->cv_qualifier = compute_cv_qualifier(cv_qualif);

    (*declarator_type)->function->exception_spec = build_exception_spec(st, except_spec, decl_context);

    (*declarator_type)->function->is_static = gather_info->is_static;
    (*declarator_type)->function->is_inline = gather_info->is_inline;
    (*declarator_type)->function->is_virtual = gather_info->is_virtual;
    (*declarator_type)->function->is_explicit = gather_info->is_explicit;
    
    (*declarator_type)->array = NULL;
    (*declarator_type)->pointer = NULL;
    (*declarator_type)->type = NULL;
}

/*
 * This function builds the full type a declarator is representing.  For
 * instance
 *
 *   int (*f)[3];
 *
 * Starts with a base type of "int" and ends being a "pointer to array 3 of int"
 */
static void build_scope_declarator_rec(AST a, scope_t* st, scope_t** parameters_scope, type_t** declarator_type, 
        gather_decl_spec_t* gather_info, AST* declarator_name, decl_context_t decl_context)
{
    ERROR_CONDITION((a == NULL), "This function does not admit NULL trees", 0);

    switch(ASTType(a))
    {
        case AST_DECLARATOR :
        case AST_PARENTHESIZED_ABSTRACT_DECLARATOR :
        case AST_PARENTHESIZED_DECLARATOR :
            {
                build_scope_declarator_rec(ASTSon0(a), st, parameters_scope, declarator_type, 
                        gather_info, declarator_name, decl_context); 
                break;
            }
        case AST_CONVERSION_DECLARATOR :
        case AST_ABSTRACT_DECLARATOR :
            {
                set_pointer_type(declarator_type, st, ASTSon0(a), decl_context);
                if (ASTSon1(a) != NULL)
                {
                    build_scope_declarator_rec(ASTSon1(a), st, parameters_scope, declarator_type, 
                            gather_info, declarator_name, decl_context);
                }
                break;
            }
        case AST_POINTER_DECL :
            {
                set_pointer_type(declarator_type, st, ASTSon0(a), decl_context);
                build_scope_declarator_rec(ASTSon1(a), st, parameters_scope, declarator_type, 
                        gather_info, declarator_name, decl_context);
                break;
            }
        case AST_ABSTRACT_ARRAY :
            {
                set_array_type(declarator_type, st, ASTSon1(a), decl_context);
                if (ASTSon0(a) != NULL)
                {
                    build_scope_declarator_rec(ASTSon0(a), st, parameters_scope, declarator_type, 
                            gather_info, declarator_name, decl_context);
                }
                break;
            }
        case AST_DIRECT_NEW_DECLARATOR :
            {
                set_array_type(declarator_type, st, ASTSon1(a), decl_context);
                if (ASTSon0(a) != NULL)
                {
                    build_scope_declarator_rec(ASTSon0(a), st, parameters_scope, declarator_type, 
                            gather_info, declarator_name, decl_context);
                }
                break;
            }
        case AST_NEW_DECLARATOR :
            {
                set_pointer_type(declarator_type, st, ASTSon0(a), decl_context);
                if (ASTSon0(a) != NULL)
                {
                    build_scope_declarator_rec(ASTSon0(a), st, parameters_scope, declarator_type, 
                            gather_info, declarator_name, decl_context);
                }
                break;
            }
        case AST_DECLARATOR_ARRAY :
            {
                set_array_type(declarator_type, st, ASTSon1(a), decl_context);
                build_scope_declarator_rec(ASTSon0(a), st, parameters_scope, declarator_type, 
                        gather_info, declarator_name, decl_context);
                break;
            }
        case AST_ABSTRACT_DECLARATOR_FUNC :
            {
                set_function_type(declarator_type, st, parameters_scope, gather_info, ASTSon1(a), 
                        ASTSon2(a), ASTSon3(a), decl_context);
                if (ASTSon0(a) != NULL)
                {
                    build_scope_declarator_rec(ASTSon0(a), st, parameters_scope, declarator_type, 
                            gather_info, declarator_name, decl_context);
                }
                break;
            }
        case AST_DECLARATOR_FUNC :
            {
                set_function_type(declarator_type, st, parameters_scope, gather_info, ASTSon1(a), 
                        ASTSon2(a), ASTSon3(a), decl_context);
                build_scope_declarator_rec(ASTSon0(a), st, parameters_scope, declarator_type, 
                        gather_info, declarator_name, decl_context);
                break;
            }
        case AST_DECLARATOR_ID_EXPR :
            {
                if (declarator_name != NULL)
                {
                    *declarator_name = a;
                }

                break;
            }
        case AST_AMBIGUITY :
            {
                solve_ambiguous_declarator(a, st, decl_context);
                // Restart function
                build_scope_declarator_rec(a, st, parameters_scope, declarator_type, 
                        gather_info, declarator_name, decl_context);
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
        gather_decl_spec_t* gather_info, scope_t* st, decl_context_t decl_context)
{
    switch (ASTType(declarator_name))
    {
        case AST_DECLARATOR_ID_EXPR :
            return build_scope_declarator_id_expr(declarator_name, declarator_type, gather_info, st, 
                    decl_context);
            break;
        default:
            internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(declarator_name)));
            break;
    }

    return NULL;
}

/*
 * This function fills information for a declarator_id_expr. Actually only
 * unqualified names can be signed up since qualified names should have been
 * declared elsewhere.
 */
static scope_entry_t* build_scope_declarator_id_expr(AST declarator_name, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, scope_t* st, decl_context_t decl_context)
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
                    return register_new_typedef_name(declarator_id, declarator_type, gather_info, st, decl_context);
                }
                else
                {
                    return register_new_variable_name(declarator_id, declarator_type, gather_info, st, decl_context);
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
                return register_new_variable_name(destructor_id, declarator_type, gather_info, st, decl_context);
                break;
            }
        case AST_TEMPLATE_ID :
            {
                // This can only happen in an explicit template function instantiation.
                // WARNING_MESSAGE("Template id not supported. Skipping it", 0);
                scope_entry_list_t* entry_list = query_id_expression(st, declarator_id, 
                        FULL_UNQUALIFIED_LOOKUP, decl_context);

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
                    solve_possibly_ambiguous_template_id(declarator_id, st, decl_context);
                }

                return register_new_variable_name(operator_id, declarator_type, gather_info, st, decl_context);
                break;
            }
        case AST_CONVERSION_FUNCTION_ID :
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Registering a conversion function ID !!!\n");
                }
                // Ok, according to the standard, this function returns the
                // type defined in the conversion function id
                type_t* conversion_type_info = NULL;

                // Get the type and its name
                char* conversion_function_name = get_conversion_function_name(declarator_id,  st, &conversion_type_info,
                        decl_context);
                AST conversion_id = ASTLeaf(AST_SYMBOL, ASTLine(declarator_id), conversion_function_name);
                return register_new_variable_name(conversion_id, declarator_type, gather_info, st, decl_context);
                break;
            }
        // Qualified ones
        case AST_QUALIFIED_ID :
            {
                // A qualified id "a::b::c"
                if (declarator_type->kind != TK_FUNCTION)
                {
                    scope_entry_list_t* entry_list = query_id_expression(st, declarator_id, FULL_UNQUALIFIED_LOOKUP,
                            decl_context);

                    ERROR_CONDITION((entry_list == NULL), "Qualified id '%s' name not found (%s)", 
                            prettyprint_in_buffer(declarator_id), node_information(declarator_id));

                    return entry_list->entry;
                }
                else
                {
                    char is_overload;
                    scope_entry_t* entry = find_function_declaration(st, declarator_id, declarator_type, &is_overload,
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
        gather_decl_spec_t* gather_info, scope_t* st, decl_context_t decl_context)
{
    // First query for an existing entry in this scope
    scope_entry_list_t* list = query_id_expression(st, declarator_id, 
            NOFULL_UNQUALIFIED_LOOKUP, decl_context);

    // Only enum or classes can exist, otherwise this is an error
    if (list != NULL)
    {
        scope_entry_t* entry = filter_simple_type_specifier(list);

        ERROR_CONDITION((entry == NULL), 
                "Symbol '%s' in %s has been redeclared as a different symbol kind (look at line %d).", 
                ASTText(declarator_id), 
                node_information(declarator_id), 
                list->entry->line);

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
        if (declarator_type->kind != TK_DIRECT
                || declarator_type->type->kind != STK_USER_DEFINED
                || declarator_type->type->user_defined_type != entry)
        {
            ERROR_CONDITION((!equivalent_types(entry->type_information, declarator_type, st, CVE_CONSIDER,
                            decl_context)), 
                    "Symbol '%s' in line %s has been redeclared as a different symbol kind (look at line %d).", 
                    ASTText(declarator_id), 
                    node_information(declarator_id), 
                    entry->line);
        }
        else
        {
            // In this special case, "A" will not be redefined, lets undefine
            // here and let be defined after
            entry->defined = 0;
        }

        return entry;
    }

    scope_entry_t* entry = new_symbol(st, ASTText(declarator_id));

    DEBUG_CODE()
    {
        fprintf(stderr, "Registering typedef '%s'\n", ASTText(declarator_id));
    }

    entry->line = ASTLine(declarator_id);
	entry->point_of_declaration = get_enclosing_declaration(declarator_id);
    // Save aliased type under the type of this declaration
    entry->kind = SK_TYPEDEF;
    entry->type_information = calloc(1, sizeof(*(entry->type_information)));
    entry->type_information->kind = TK_DIRECT;
    entry->type_information->type = calloc(1, sizeof(*(entry->type_information->type)));
    entry->type_information->type->kind = STK_TYPEDEF;
    entry->type_information->type->aliased_type = declarator_type;
    entry->type_information->type->type_scope = st;

    return entry;
}

/*
 * This function registers a new "variable" (non type) name
 */
static scope_entry_t* register_new_variable_name(AST declarator_id, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, scope_t* st, decl_context_t decl_context)
{
    if (declarator_type->kind != TK_FUNCTION)
    {
        // Check for existence of this symbol in this scope
        scope_entry_list_t* entry_list = query_id_expression(st, declarator_id, 
                NOFULL_UNQUALIFIED_LOOKUP, decl_context);

        scope_entry_list_t* check_list = filter_symbol_kind(entry_list, SK_VARIABLE);

        if (check_list != NULL)
        {
            scope_entry_t* entry = check_list->entry;
            return entry;
        }

        enum cxx_symbol_kind valid_symbols[2] = {SK_CLASS, SK_ENUM};
        check_list = filter_symbol_non_kind_set(entry_list, 2, valid_symbols);

        ERROR_CONDITION((check_list != NULL), "The symbol has already been defined as another symbol kind %d", check_list->entry->kind);

        DEBUG_CODE()
        {
            fprintf(stderr, "Registering variable '%s' in %p\n", ASTText(declarator_id), st);
        }
        scope_entry_t* entry = new_symbol(st, ASTText(declarator_id));
        entry->line = ASTLine(declarator_id);
		entry->point_of_declaration = get_enclosing_declaration(declarator_id);
        entry->kind = SK_VARIABLE;
        entry->type_information = declarator_type;

        return entry;
    }
    else
    {
        return register_function(declarator_id, declarator_type, gather_info, st, decl_context);
    }
}

static scope_entry_t* register_function(AST declarator_id, type_t* declarator_type, 
        gather_decl_spec_t* gather_info, scope_t* st, decl_context_t decl_context)
{
    scope_entry_t* entry;

    char is_overload;
    entry = find_function_declaration(st, declarator_id, declarator_type, &is_overload, decl_context);

    if (entry == NULL)
    {

        char* function_name = ASTText(declarator_id);

        if (BITMAP_TEST(decl_context.decl_flags, DF_CONSTRUCTOR))
        {
            function_name = strprepend(function_name, "constructor ");
        }

        scope_entry_t* new_entry = new_symbol(st, function_name);
        new_entry->line = ASTLine(declarator_id);
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

static scope_entry_t* find_function_declaration(scope_t* st, AST declarator_id, type_t* declarator_type, 
        char* is_overload, decl_context_t decl_context)
{
    // This function is a mess and should be rewritten
    lookup_flags_t lookup_flags = LF_NONE;

    if (BITMAP_TEST(decl_context.decl_flags, DF_CONSTRUCTOR))
    {
        lookup_flags |= LF_CONSTRUCTOR;
    }

    scope_entry_list_t* entry_list;
    if (!BITMAP_TEST(decl_context.decl_flags, DF_FRIEND))
    {
        entry_list = query_id_expression_flags(st, declarator_id, 
                NOFULL_UNQUALIFIED_LOOKUP, lookup_flags, decl_context);
    }
    else
    {
        entry_list = query_id_expression_flags(st, declarator_id, 
                FULL_UNQUALIFIED_LOOKUP, lookup_flags, decl_context);
    }

    type_t* function_being_declared = declarator_type;

    function_being_declared->function->template_nesting = decl_context.template_nesting;
    function_being_declared->function->num_template_parameters = decl_context.num_template_parameters;
    function_being_declared->function->template_parameter_info = decl_context.template_parameters;

    function_being_declared->function->num_template_parameters_in_scope = decl_context.num_template_parameters_in_scope;
    function_being_declared->function->template_parameter_in_scope_info = decl_context.template_parameters_in_scope;

    scope_entry_t* equal_entry = NULL;

    char found_equal = 0;
    *is_overload = 0;

    // if (BITMAP_TEST(decl_context.decl_flags, DF_FRIEND))
    // {
    //  lookup_flags |= LF_IN_NAMESPACE_SCOPE;
    // }

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

        type_t* current_function = entry->type_information;

        found_equal = !overloaded_function(function_being_declared, current_function, st, decl_context);
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
                internal_error("Function '%s' has been declared with different prototype", 
                        ASTText(declarator_id));
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
static void build_scope_linkage_specifier(AST a, scope_t* st, decl_context_t decl_context)
{
    AST declaration_sequence = ASTSon1(a);

    if (declaration_sequence == NULL)
        return;

    char* previous_linkage = current_linkage;

    AST linkage_spec = ASTSon0(a);
    current_linkage = ASTText(linkage_spec);

    build_scope_declaration_sequence(declaration_sequence, st, decl_context);

    current_linkage = previous_linkage;
}

/*
 * Similar to build_scope_linkage_specifier but for just one declaration
 */
static void build_scope_linkage_specifier_declaration(AST a, scope_t* st, decl_context_t decl_context)
{
    AST declaration = ASTSon1(a);

    char* previous_linkage = current_linkage;

    AST linkage_spec = ASTSon0(a);
    current_linkage = ASTText(linkage_spec);

    build_scope_declaration(declaration, st, decl_context);

    current_linkage = previous_linkage;
}

/*
 * This function registers a template declaration
 */
static void build_scope_template_declaration(AST a, scope_t* st, decl_context_t decl_context)
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
    scope_t* template_scope = new_template_scope(st);
    template_parameter_t** template_parameters = NULL;
    int num_parameters = 0;
    
    // Construct parameter information
    decl_context_t new_decl_context = decl_context;
    new_decl_context.decl_flags |= DF_TEMPLATE;
    new_decl_context.template_nesting++;

    template_scope->contained_in = st;
    build_scope_template_parameter_list(ASTSon0(a), template_scope, &template_parameters, 
            &num_parameters, new_decl_context);
    template_scope->contained_in = NULL;
    
    // Save template parameters of the current definition
    new_decl_context.num_template_parameters = num_parameters;
    new_decl_context.template_parameters = template_parameters;

    // And save them into the "in scope" template parameter set
    new_decl_context.num_template_parameters_in_scope = 0;
    new_decl_context.template_parameters_in_scope = calloc(1, 
            sizeof(*(new_decl_context.template_parameters_in_scope)));
    // First the inherited ones
    int i;
    for (i = 0; i < decl_context.num_template_parameters_in_scope; i++)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "(1) ### Old template parameter [%d] ###\n", i);
        }
        P_LIST_ADD(new_decl_context.template_parameters_in_scope, 
                new_decl_context.num_template_parameters_in_scope,
                decl_context.template_parameters_in_scope[i]);
    }
    // then the new ones
    for (i = 0; i < num_parameters; i++)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "(1) ### New template parameter [%d] ###\n", i);
        }
        P_LIST_ADD(new_decl_context.template_parameters_in_scope, 
                new_decl_context.num_template_parameters_in_scope,
                template_parameters[i]);
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "### Num template parameters in scope: %d\n",
                new_decl_context.num_template_parameters_in_scope);
    }

    // Save template scope
    template_scope->template_scope = st->template_scope;
    st->template_scope = template_scope;

    AST templated_decl = ASTSon1(a);

    if (ASTType(templated_decl) == AST_AMBIGUITY)
    {
        solve_ambiguous_declaration(templated_decl, st, decl_context);
    }

	// Link the AST with the scope
	scope_link_set(compilation_options.scope_link, a, copy_scope(st));

    switch (ASTType(templated_decl))
    {
        case AST_FUNCTION_DEFINITION :
            {

                build_scope_template_function_definition(templated_decl, st, template_scope, num_parameters, 
                        template_parameters, new_decl_context);
                break;
            }
        case AST_SIMPLE_DECLARATION :
            {
                build_scope_template_simple_declaration(templated_decl, st, template_scope, num_parameters, 
                        template_parameters, new_decl_context);
                break;
            }
        case AST_TEMPLATE_DECLARATION :
            {
                build_scope_template_declaration(templated_decl, st, new_decl_context);
                // build_scope_template_declaration(templated_decl, st, decl_context);
                break;
            }
        default :
            internal_error("Unknown node type '%s' (line=%s)\n", ast_print_node_type(ASTType(templated_decl)), 
                    node_information(templated_decl));
    }

    // Restore template scope
    st->template_scope = template_scope->template_scope;
    template_scope->template_scope = NULL;
}

/*
 * This function registers an explicit template specialization
 */
static void build_scope_explicit_template_specialization(AST a, scope_t* st, decl_context_t decl_context)
{
    scope_t* template_scope = new_template_scope(st);
    template_parameter_t** template_parameters = calloc(1, sizeof(*template_parameters));
    int num_parameters = 0;

    decl_context_t new_decl_context = decl_context;

    new_decl_context.decl_flags |= DF_TEMPLATE;
    new_decl_context.decl_flags |= DF_EXPLICIT_SPECIALIZATION;
    // new_decl_context.template_nesting++;

    new_decl_context.template_parameters = template_parameters;
    new_decl_context.num_template_parameters = 0;
    
    // Save template scope
    template_scope->template_scope = st->template_scope;
    st->template_scope = template_scope;
    
    if (ASTType(ASTSon0(a)) == AST_AMBIGUITY)
    {
        solve_ambiguous_declaration(ASTSon0(a), st, decl_context);
    }

    switch (ASTType(ASTSon0(a)))
    {
        case AST_FUNCTION_DEFINITION :
            {
                build_scope_template_function_definition(ASTSon0(a), st, template_scope, num_parameters, 
                        template_parameters, new_decl_context);
                break;
            }
        case AST_SIMPLE_DECLARATION :
            {
                build_scope_template_simple_declaration(ASTSon0(a), st, template_scope, num_parameters, 
                        template_parameters, new_decl_context);
                break;
            }
        case AST_EXPLICIT_SPECIALIZATION :
            {
                build_scope_explicit_template_specialization(ASTSon0(a), st, new_decl_context);
                break;
            }
        default :
            {
                internal_error("Unknown node type '%s'\n", ast_print_node_type(ASTType(ASTSon0(a))));
            }
    }
    
    // Restore template scope
    st->template_scope = template_scope->template_scope;
    template_scope->template_scope = NULL;
}

static void build_scope_template_function_definition(AST a, scope_t* st, scope_t* template_scope, 
        int num_parameters, template_parameter_t** template_parameters, decl_context_t decl_context)
{
    /* scope_entry_t* entry = */ build_scope_function_definition(a, st, decl_context);
}

static void build_scope_template_simple_declaration(AST a, scope_t* st, scope_t* template_scope, 
        int num_parameters, template_parameter_t** template_parameters, decl_context_t decl_context)
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
     * For the last case we won't do anything at the moment.
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

        build_scope_decl_specifier_seq(decl_specifier_seq, st, &gather_info, &simple_type_info, new_decl_context);

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
        // ERROR_CONDITION((ASTSon0(init_declarator_list) != NULL), "In template declarations only one declarator is valid", 0);

        AST init_declarator = ASTSon1(init_declarator_list);

        if (ASTType(init_declarator) == AST_AMBIGUITY)
        {
            solve_ambiguous_init_declarator(init_declarator, st, decl_context);
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

        /* scope_entry_t* entry = */ build_scope_declarator(declarator, st, 
                &gather_info, simple_type_info, &declarator_type,
                new_decl_context);
        
        // This is a simple declaration, thus if it does not declare an
        // extern variable or function, the symbol is already defined here
        if (!gather_info.is_extern
                && declarator_type->kind != TK_FUNCTION)
        {
            AST declarator_name = get_declarator_name(declarator, st, decl_context);
            scope_entry_list_t* entry_list = query_id_expression(st, declarator_name, 
                    NOFULL_UNQUALIFIED_LOOKUP, decl_context);

            ERROR_CONDITION((entry_list == NULL), "Symbol just declared has not been found in the scope!", 0);

            // The last entry will hold our symbol, no need to look for it in the list
            ERROR_CONDITION((entry_list->entry->defined), "This symbol has already been defined", 0);

            DEBUG_CODE()
            {
                fprintf(stderr, "Defining symbol '");
                prettyprint(stderr, declarator_name);
                fprintf(stderr, "'\n");
            }

            entry_list->entry->defined = 1;

            if (initializer != NULL)
            {
                check_for_initialization(initializer, entry_list->entry->scope,
                        decl_context);
                entry_list->entry->expression_value = initializer;
            }
        }
        else if (declarator_type->kind == TK_FUNCTION)
        {
        }
    }
}

/*
 * This function registers templates parameters in a given scope
 */
static void build_scope_template_parameter_list(AST a, scope_t* st, 
        template_parameter_t*** template_parameters, int* num_parameters,
        decl_context_t decl_context)
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

        build_scope_template_parameter(template_parameter, st, new_template_param, *num_parameters, decl_context);
        P_LIST_ADD(*template_parameters, *num_parameters, new_template_param);
    }
}

/*
 * This function registers one template parameter in a given scope
 */
static void build_scope_template_parameter(AST a, scope_t* st, 
        template_parameter_t* template_parameters, int num_parameter,
        decl_context_t decl_context)
{
    switch (ASTType(a))
    {
        case AST_GCC_PARAMETER_DECL :
        case AST_PARAMETER_DECL :
            build_scope_nontype_template_parameter(a, st, template_parameters, num_parameter, decl_context);
            break;
        case AST_TYPE_PARAMETER_CLASS :
        case AST_TYPE_PARAMETER_TYPENAME :
            build_scope_type_template_parameter(a, st, template_parameters, num_parameter, decl_context);
            break;
        case AST_TYPE_PARAMETER_TEMPLATE :
            build_scope_template_template_parameter(a, st, template_parameters, num_parameter, decl_context);
            break;
        case AST_AMBIGUITY :
            // The ambiguity here is parameter_class vs parameter_decl
            solve_parameter_declaration_vs_type_parameter_class(a);
            // Restart this routine
            build_scope_template_parameter(a, st, template_parameters, num_parameter, decl_context);
            break;
        default :
            internal_error("Unknown node type '%s'", ast_print_node_type(ASTType(a)));
    }
}

static void build_scope_template_template_parameter(AST a, scope_t* st,
        template_parameter_t* template_parameters, int num_parameter,
        decl_context_t decl_context)
{
    // These parameters have the form
    //
    //    TEMPLATE < template_param_list > CLASS [identifier] [= id_expr]
    //
    // "identifier" is then a template-name
    //
    // Construct parameter information
    scope_t* parm_template_scope = new_template_scope(st);
    template_parameter_t** parm_template_param_info = NULL;
    int parm_num_parameters = 0;

    decl_context_t template_params_context = default_decl_context;
    
    parm_template_scope->contained_in = st;
    build_scope_template_parameter_list(ASTSon0(a), parm_template_scope, &parm_template_param_info, 
            &parm_num_parameters, template_params_context);

    // Now create a STK_CLASS
    type_t* new_type = calloc(1, sizeof(*new_type));
    new_type->kind = TK_DIRECT;
    new_type->type = calloc(1, sizeof(*(new_type->type)));
    new_type->type->kind = STK_TEMPLATE_TEMPLATE_PARAMETER;
    new_type->type->template_parameter_nesting = decl_context.template_nesting;
    new_type->type->template_parameter_num = num_parameter;

    
    // Save the info
    template_parameters->type_info = new_type;

    if (ASTSon1(a) != NULL)
    {
        AST symbol = ASTSon1(a);
        char* name = ASTText(symbol);

        scope_entry_t* new_entry = new_symbol(st, name);
        new_entry->line = ASTLine(symbol);
		new_entry->point_of_declaration = symbol;

        new_entry->kind = SK_TEMPLATE_TEMPLATE_PARAMETER;
        new_entry->type_information = new_type;

        new_entry->template_parameter_info = parm_template_param_info;
        new_entry->num_template_parameters = parm_num_parameters;

        build_scope_template_arguments_for_primary_template(st, 
            parm_template_scope, 
            new_entry->template_parameter_info,
            new_entry->num_template_parameters, 
            &(new_entry->type_information->type->template_arguments));
        
        // And save its name
        template_parameters->template_parameter_name = strdup(name);
        new_type->type->template_parameter_name = strdup(name);

        template_parameters->parameter_tree = ASTMake3(AST_SIMPLE_TYPE_SPECIFIER, 
                NULL, NULL, duplicate_ast(symbol), ASTLine(symbol), NULL);

        ASTSon2(template_parameters->parameter_tree) = NULL;
    }
    else
    {
        char* template_param_name = calloc(256, sizeof(char));

        sprintf(template_param_name, " <template-param-%d-%d> ", decl_context.template_nesting, num_parameter+1);
        template_parameters->parameter_tree = ASTLeaf(AST_SYMBOL, ASTLine(a), template_param_name);
    }

    AST id_expr = ASTSon2(a);
    if (id_expr != NULL)
    {
        // This might be ambiguous
        solve_possibly_ambiguous_expression(id_expr, st, decl_context);

        scope_entry_list_t* entry_list = query_id_expression(st, id_expr, 
                FULL_UNQUALIFIED_LOOKUP, decl_context);

        ERROR_CONDITION((entry_list == NULL), "Default argument expression id not found\n", 0);

        enum cxx_symbol_kind valid_templates_arguments[2] = 
        { 
            SK_TEMPLATE_PRIMARY_CLASS, 
            SK_TEMPLATE_TEMPLATE_PARAMETER,
        };
        // specializations are not necessary here
        entry_list = filter_symbol_kind_set(entry_list, 2, valid_templates_arguments);

        ERROR_CONDITION((entry_list == NULL), "No primary template or template template parameter found", 0);

        template_parameters->default_tree = id_expr;
    }

    template_parameters->default_argument_scope = copy_scope(st);

    template_parameters->kind = TPK_TEMPLATE;
}

static void build_scope_type_template_parameter(AST a, scope_t* st,
        template_parameter_t* template_parameters, int num_parameter,
        decl_context_t decl_context)
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
    type_t* new_type = calloc(1, sizeof(*new_type));
    new_type->kind = TK_DIRECT;
    new_type->type = calloc(1, sizeof(*(new_type->type)));
    new_type->type->kind = STK_TYPE_TEMPLATE_PARAMETER;
    new_type->type->template_parameter_nesting = decl_context.template_nesting;
    new_type->type->template_parameter_num = num_parameter;

    // Save the info
    template_parameters->type_info = new_type;


    AST name = ASTSon0(a);
    AST type_id = ASTSon1(a);
    
    if (name != NULL)
    {
        // This is a named type parameter. Register it in the symbol table
        DEBUG_CODE()
        {
            fprintf(stderr, "Registering type template-parameter '%s' in scope %p\n", ASTText(name), st);
        }
        scope_entry_t* new_entry = new_symbol(st, ASTText(name));
        new_entry->line = ASTLine(name);
        new_entry->point_of_declaration = name;
        new_entry->type_information = new_type;
        new_entry->kind = SK_TEMPLATE_TYPE_PARAMETER;

        // And save it in the type
        template_parameters->template_parameter_name = strdup(ASTText(name));
        new_type->type->template_parameter_name = strdup(ASTText(name));

        template_parameters->parameter_tree = name;
    }
    else
    {
        char* template_param_name = calloc(256, sizeof(char));

        sprintf(template_param_name, " <template-param-%d-%d> ", decl_context.template_nesting, num_parameter+1);
        template_parameters->parameter_tree = ASTLeaf(AST_SYMBOL, ASTLine(a), template_param_name);
    }

    if (type_id != NULL)
    {
        // This might be ambiguous
        AST def_arg_type_specifier = ASTSon0(type_id);
        AST def_arg_simple_type_spec = ASTSon1(def_arg_type_specifier);

        if (ASTSon2(def_arg_simple_type_spec) != NULL
                && ASTType(ASTSon2(def_arg_simple_type_spec)) == AST_TEMPLATE_ID)
        {
            solve_possibly_ambiguous_template_id(ASTSon2(def_arg_simple_type_spec), st,
                    decl_context);
        }

        template_parameters->default_argument_scope = copy_scope(st);
        template_parameters->default_tree = type_id;
    }

    template_parameters->kind = TPK_TYPE;
}

static void build_scope_nontype_template_parameter(AST a, scope_t* st,
        template_parameter_t* template_parameters, int num_parameter,
        decl_context_t decl_context)
{
    // As usual there are three parts
    //     decl_specifier_seq [declarator] [ = expression ]
    type_t* simple_type_info;
    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));

    AST decl_specifier_seq = ASTSon0(a);
    AST parameter_declarator = ASTSon1(a);
    AST default_expression = ASTSon2(a);

    build_scope_decl_specifier_seq(decl_specifier_seq, st, &gather_info, &simple_type_info, decl_context);

    simple_type_info->type->template_parameter_nesting = decl_context.template_nesting;
    simple_type_info->type->template_parameter_num = num_parameter;

    if (parameter_declarator != NULL)
    {
        // This will add into the symbol table if it has a name
        scope_entry_t* entry = build_scope_declarator(parameter_declarator, st, 
                &gather_info, simple_type_info, &template_parameters->type_info,
                decl_context);

        if (entry != NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Remembering '%s' as a non-type template parameter in %p\n", entry->symbol_name, st);
            }
            // This is not a variable, but a template parameter
            entry->kind = SK_TEMPLATE_PARAMETER;

            // Save its name
            template_parameters->template_parameter_name = strdup(entry->symbol_name);
            simple_type_info->type->template_parameter_name = strdup(entry->symbol_name);
        }
    }
    // If we don't have a declarator just save the base type
    else
    {
        template_parameters->type_info = simple_type_info;
    }

    if (default_expression != NULL)
    {
        solve_possibly_ambiguous_expression(default_expression, st, decl_context);
    }

    template_parameters->default_argument_scope = copy_scope(st);
    template_parameters->default_tree = default_expression;

    template_parameters->kind = TPK_NONTYPE;
}

static void build_scope_namespace_alias(AST a, scope_t* st, decl_context_t decl_context)
{
    AST alias_ident = ASTSon0(a);
    AST qualified_name = ASTSon1(a);

    AST global_op = ASTSon0(qualified_name);
    AST nested_name_spec = ASTSon1(qualified_name);
    AST name = ASTSon2(qualified_name);

    scope_entry_list_t* entry_list = query_nested_name(st, global_op, nested_name_spec, 
            name, FULL_UNQUALIFIED_LOOKUP, decl_context);

    ERROR_CONDITION((entry_list == NULL), "Namespace not found\n", 0);

    scope_entry_t* entry = entry_list->entry;
    
    ERROR_CONDITION((entry->kind != SK_NAMESPACE), "The referred symbol is not a namespace\n", 0);

    char* alias_name = ASTText(alias_ident);

    scope_entry_t* alias_entry = new_symbol(st, alias_name);

    alias_entry->line = ASTLine(alias_ident);
	alias_entry->point_of_declaration = alias_ident;
    alias_entry->kind = SK_NAMESPACE;
    alias_entry->related_scope = entry->related_scope;
}

/*
 * This function builds symbol table information for a namespace definition
 */
static void build_scope_namespace_definition(AST a, scope_t* st, decl_context_t decl_context)
{
    AST namespace_name = ASTSon0(a);

    if (namespace_name != NULL)
    {
        // Register this namespace if it does not exist
        scope_entry_list_t* list = query_unqualified_name(st, ASTText(namespace_name));

        scope_entry_list_t* check_list = filter_symbol_non_kind(list, SK_NAMESPACE);
        ERROR_CONDITION((check_list != NULL), "Identifier '%s' has already been declared as another symbol kind\n", ASTText(namespace_name));

        scope_entry_t* entry;
        if (list != NULL && list->entry->kind == SK_NAMESPACE)
        {
            entry = list->entry;
        }
        else
        {
            // We register a symbol of type namespace and link to a newly created scope.
            scope_t* namespace_scope = new_namespace_scope(st);

            entry = new_symbol(st, ASTText(namespace_name));
            entry->line = ASTLine(namespace_name);
			entry->point_of_declaration = namespace_name;
            entry->kind = SK_NAMESPACE;
            entry->related_scope = namespace_scope;

			// Link the scope of this newly created namespace
			scope_link_set(compilation_options.scope_link, a, namespace_scope);
        }

        if (ASTSon1(a) != NULL)
        {
            build_scope_declaration_sequence(ASTSon1(a), entry->related_scope, decl_context);
        }
    }
    else
    {
        WARNING_MESSAGE("Unnamed namespace support is missing", 0);
        build_scope_declaration_sequence(ASTSon1(a), compilation_options.global_scope, decl_context);
		// #warning Unnamed namespace support is missing
    }
}

static void build_scope_ctor_initializer(AST ctor_initializer, scope_t* st, 
        scope_t* class_scope, scope_entry_t* function_entry, decl_context_t decl_context)
{
    // Get the class symbol
    char* constructor_name = function_entry->symbol_name;
    constructor_name += strlen("constructor ");

    scope_entry_list_t* class_entry_list = 
        query_in_symbols_of_scope(class_scope, constructor_name);

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

    class_info_t* class_entry_info = class_entry->type_information->type->class_info;

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
                    result_list = query_nested_name(class_scope, global_op, nested_name_spec, symbol, 
                            FULL_UNQUALIFIED_LOOKUP, decl_context);

                    ERROR_CONDITION((result_list == NULL), "Initialized entity in constructor initializer not found (%s)", 
                            node_information(symbol));

                    scope_entry_t* entry = result_list->entry;
                    // This checking code is only partially correct
                    // and covers very obvious cases
                    if (!is_dependent_type(entry->type_information, decl_context))
                    {
                        if (entry->kind == SK_VARIABLE)
                        {
                            ERROR_CONDITION(entry->scope != class_scope,
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
                            for (i = 0; i < class_entry_info->num_bases; i++)
                            {
                                base_class_info_t* base_class_info = 
                                    class_entry_info->base_classes_list[i];

                                if (base_class_info->class_symbol == entry)
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
                            solve_ambiguous_expression_list(expression_list, st,
                                    decl_context);
                        }

                        AST iter;
                        for_each_element(expression_list, iter)
                        {
                            AST expression = ASTSon1(iter);

                            solve_possibly_ambiguous_expression(expression, st,
                                    decl_context);
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

// This function is C99 only
void build_scope_kr_parameter_declaration(AST kr_parameter_declaration, 
        scope_t* parameter_scope, decl_context_t decl_context)
{
    AST declaration_list = kr_parameter_declaration;
    AST iter;

    for_each_element(declaration_list, iter)
    {
        AST simple_decl = ASTSon1(iter);

        build_scope_simple_declaration(simple_decl, parameter_scope, decl_context);
    }
}

/*
 * This function builds symbol table information for a function definition
 */
static scope_entry_t* build_scope_function_definition(AST a, scope_t* st, decl_context_t decl_context)
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
        build_scope_decl_specifier_seq(decl_spec_seq, st, &gather_info, &type_info, decl_context);
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
    scope_t* parameter_scope = NULL;

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

    entry = build_scope_declarator_with_parameter_scope(ASTSon1(a), st, &parameter_scope,
            &gather_info, type_info, &declarator_type, new_decl_context);
    ERROR_CONDITION((entry == NULL), "Function '%s' does not exist! %s", prettyprint_in_buffer(ASTSon1(a)), node_information(a));
	
	{
		// Function declaration name
		AST declarator_name = get_declarator_name(ASTSon1(a), st, decl_context);
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
		}
		else if (ASTType(declarator_name) == AST_SYMBOL)
		{
			ASTAttrSetValueType(declarator_name, LANG_IS_UNQUALIFIED_ID, tl_type_t, tl_bool(1));
			ASTAttrSetValueType(declarator_name, LANG_UNQUALIFIED_ID, tl_type_t, tl_ast(declarator_name));
		}
	}

    // Change scope to the function one
    DEBUG_CODE()
    {
        fprintf(stderr, "Changing scope from %p to %p\n", st, entry->scope);
    }
    st = entry->scope;

	// The scope seen by this function definition
	scope_link_set(compilation_options.scope_link, a, copy_scope(st));

    ERROR_CONDITION((entry->kind != SK_FUNCTION && entry->kind != SK_TEMPLATE_FUNCTION), 
            "This is not a function!!!", 0);

    // Function_body
    AST function_body = ASTSon3(a);
    AST statement = ASTSon0(function_body);

    scope_t* inner_scope = new_function_scope(st, parameter_scope);

    entry->related_scope = inner_scope;

    CXX_LANGUAGE()
    {
        AST ctor_initializer = ASTSon2(a);
        if (ctor_initializer != NULL)
        {
            scope_t* ctor_scope = new_block_scope(st, parameter_scope, inner_scope);
            build_scope_ctor_initializer(ctor_initializer, ctor_scope, st, entry, decl_context);
        }
    }
    C_LANGUAGE()
    {
        AST kr_parameter_declaration = ASTSon2(a);
        if (kr_parameter_declaration != NULL)
        {
            build_scope_kr_parameter_declaration(kr_parameter_declaration, parameter_scope,
                    decl_context);
        }
    }

    if (entry->is_member)
    {
        // If is a member function sign up additional information
        if (!entry->type_information->function->is_static
                && !entry->type_information->function->is_constructor)
        {
            type_t* this_type = calloc(1, sizeof(*this_type));
            this_type->kind = TK_POINTER;
            this_type->pointer = calloc(1, sizeof(*(this_type->pointer)));
            this_type->pointer->pointee = copy_type(entry->class_type);

            // "this" pseudovariable has the same cv-qualification of this member
            this_type->cv_qualifier = entry->type_information->cv_qualifier;

            // This will put the symbol in the function scope, but this is fine
            scope_entry_t* this_symbol = new_symbol(entry->related_scope, "this");

            this_symbol->line = ASTLine(function_body);
            // Not very useful
            this_symbol->point_of_declaration = function_body;
            this_symbol->kind = SK_VARIABLE;
            this_symbol->type_information = this_type;
            this_symbol->defined = 1;
        }
    }

    build_scope_statement(statement, inner_scope, decl_context);
	scope_link_set(compilation_options.scope_link, statement, copy_scope(inner_scope));

	ASTAttrSetValueType(a, LANG_FUNCTION_BODY, tl_type_t, tl_ast(statement));

    ERROR_CONDITION((entry == NULL), "This symbol is undeclared here", 0);
    DEBUG_CODE()
    {
        fprintf(stderr, "Function '%s' is defined\n", entry->symbol_name);
    }
    entry->defined = 1;

    if (BITMAP_TEST(decl_context.decl_flags, DF_TEMPLATE))
    {
        entry->type_information->function->function_body = function_body;
        entry->defined = 1;
    }

    return entry;
}


static void build_scope_member_declaration(AST a, scope_t*  st, 
        access_specifier_t current_access, type_t* class_info,
        int step, decl_context_t decl_context)
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
                    build_scope_simple_member_declaration(a, st, current_access, class_info, decl_context);
                }
                break;
            }
        case AST_FUNCTION_DEFINITION :
            {
                build_scope_member_function_definition(a, st, current_access, class_info, step, decl_context);
                break;
            }
        case AST_GCC_EXTENSION :
            {
                build_scope_member_declaration(ASTSon0(a), st, current_access, class_info, step, decl_context);
                break;
            }
        case AST_TEMPLATE_DECLARATION :
            {
                build_scope_member_template_declaration(a, st, current_access, class_info, step, decl_context);
                break;
            }
        case AST_USING_DECL :
            {
                build_scope_using_declaration(a, st, decl_context);
                break;
            }
        case AST_AMBIGUITY :
            {
                solve_ambiguous_declaration(a, st, decl_context);
                // Restart
                build_scope_member_declaration(a, st, current_access, class_info, step, decl_context);
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
static void build_scope_member_template_declaration(AST a, scope_t* st, 
        access_specifier_t current_access, type_t* class_info, int step,
        decl_context_t decl_context)
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
    scope_t* template_scope = new_template_scope(st);
    template_parameter_t** template_parameters = NULL;
    int num_parameters = 0;

    decl_context_t new_decl_context = decl_context;
    new_decl_context.template_nesting++;

    // Construct parameter information
    template_scope->contained_in = st;
    build_scope_template_parameter_list(ASTSon0(a), template_scope, &template_parameters, &num_parameters, new_decl_context);
    template_scope->contained_in = NULL;

    new_decl_context.template_parameters = template_parameters;
    new_decl_context.num_template_parameters = num_parameters;
    
    // And save them into the "in scope" template parameter set
    int i;
    new_decl_context.template_parameters_in_scope = calloc(1, 
            sizeof(*(new_decl_context.template_parameters_in_scope)));
    new_decl_context.num_template_parameters_in_scope = 0;
    // First the inherited ones
    for (i = 0; i < decl_context.num_template_parameters_in_scope; i++)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "(2) ### Old template parameter [%d] ### %p\n", 
                    i, decl_context.template_parameters_in_scope[i]);
        }
        P_LIST_ADD(new_decl_context.template_parameters_in_scope, 
                new_decl_context.num_template_parameters_in_scope,
                decl_context.template_parameters_in_scope[i]);
    }
    // then the new ones
    for (i = 0; i < num_parameters; i++)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "(2) ### New template parameter [%d] ### %p\n", 
                    i, template_parameters[i]);
        }
        P_LIST_ADD(new_decl_context.template_parameters_in_scope, 
                new_decl_context.num_template_parameters_in_scope,
                template_parameters[i]);
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "### Num template parameters in scope %d\n", 
                new_decl_context.num_template_parameters_in_scope);
    }
    
    // Save template scope
    template_scope->template_scope = st->template_scope;
    st->template_scope = template_scope;

    switch (ASTType(ASTSon1(a)))
    {
        case AST_FUNCTION_DEFINITION :
            {
                build_scope_member_template_function_definition(ASTSon1(a), st, template_scope, num_parameters, template_parameters,
                        current_access, class_info, step, new_decl_context);
            }
            break;
        case AST_SIMPLE_DECLARATION :
            {
                build_scope_member_template_simple_declaration(ASTSon1(a), st, template_scope, num_parameters, template_parameters,
                        current_access, class_info, new_decl_context);
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
    
    // Restore template scope
    st->template_scope = template_scope->template_scope;
    template_scope->template_scope = NULL;
}

static void build_scope_member_template_function_definition(AST a, scope_t* st, scope_t* template_scope, 
        int num_parameters, template_parameter_t** template_parameters,
        access_specifier_t current_access, type_t* class_info, int step,
        decl_context_t decl_context)
{
    decl_context_t new_decl_context = decl_context;
    new_decl_context.decl_flags |= DF_TEMPLATE;

    // Define the function within st scope but being visible template_scope
    /* scope_entry_t* entry = */ build_scope_member_function_definition(a, st, current_access, class_info, step, 
            new_decl_context);
}

static void build_scope_member_template_simple_declaration(AST a, scope_t* st, scope_t* template_scope, 
        int num_parameters, template_parameter_t** template_parameters,
        access_specifier_t current_access, type_t* class_info,
        decl_context_t decl_context)
{
    decl_context_t new_decl_context = decl_context;
    new_decl_context.decl_flags |= DF_TEMPLATE;

    // Define the function within st scope but being visible template_scope
    build_scope_simple_member_declaration(a, st, current_access, class_info, 
            new_decl_context);
}

static scope_entry_t* build_scope_member_function_definition(AST a, scope_t*  st, 
        access_specifier_t current_access, type_t* class_info,
        int step, decl_context_t decl_context)
{
    char* class_name = "";
    class_info_t* class_type = NULL;
    if (class_info->type->kind == STK_USER_DEFINED)
    {
        class_name = class_info->type->user_defined_type->symbol_name;
        class_type = class_info->type->user_defined_type->type_information->type->class_info;
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
        AST declarator_name = get_declarator_name(declarator, st, decl_context);

        char is_constructor = 0;
        AST decl_spec_seq = ASTSon0(a);

        // If ambiguous is due because we don't know how to "lay" the type_specifier
        // but it has type_specifier
        if (decl_spec_seq != NULL 
                && ((ASTType(decl_spec_seq) != AST_AMBIGUITY && ASTSon1(decl_spec_seq) != NULL)
                    || (ASTType(decl_spec_seq) == AST_AMBIGUITY)))
        {
            build_scope_decl_specifier_seq(decl_spec_seq, st, &gather_info, &type_info,
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
        scope_t* parameter_scope = NULL;

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

        entry = build_scope_declarator_with_parameter_scope(ASTSon1(a), st, &parameter_scope,
                &gather_info, type_info, &declarator_type, new_decl_context);

        switch (ASTType(declarator_name))
        {
            case AST_SYMBOL :
                {
                    if (is_constructor)
                    {
                        // This is a constructor
                        P_LIST_ADD(class_type->constructor_list, class_type->num_constructors, entry);
                        entry->type_information->function->is_constructor = 1;
                    }
                    break;
                }
                // This should not appear here
                // case AST_DESTRUCTOR_TEMPLATE_ID : 
            case AST_DESTRUCTOR_ID :
                {
                    // This is the destructor
                    class_type->destructor = entry;
                    break;
                }
            case AST_OPERATOR_FUNCTION_ID :
                {
                    P_LIST_ADD(class_type->operator_function_list, class_type->num_operator_functions, entry);
                    break;
                }
            case AST_CONVERSION_FUNCTION_ID :
                {
                    conversion_function_t* new_conversion = calloc(1, sizeof(*new_conversion));

                    // The conversion type is the return of the conversion function id
                    new_conversion->conversion_type = entry->type_information->function->return_type;
                    new_conversion->cv_qualifier = entry->type_information->cv_qualifier;

                    P_LIST_ADD(class_type->conversion_function_list, class_type->num_conversion_functions, new_conversion);
                    break;
                }
            default :
                {
                    internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(declarator_name)));
                    break;
                }
        }

        entry->is_member = 1;
        entry->class_type = class_info;
    }
    else
    {
        // Build the function definition
        entry = build_scope_function_definition(a, st, decl_context);
    }

    return entry;
}

static void build_scope_simple_member_declaration(AST a, scope_t*  st, 
        access_specifier_t current_access, type_t* class_info, decl_context_t decl_context)
{
    gather_decl_spec_t gather_info;
    type_t* simple_type_info = NULL;

    memset(&gather_info, 0, sizeof(gather_info));

    // Also add additional information about this member function
    char* class_name = "";
    class_info_t* class_type = NULL;
    if (class_info->type->kind == STK_USER_DEFINED)
    {
        class_name = class_info->type->user_defined_type->symbol_name;
        class_type = class_info->type->user_defined_type->type_information->type->class_info;
    }

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

        build_scope_decl_specifier_seq(ASTSon0(a), st, &gather_info,
                &simple_type_info, new_decl_context);

        if (simple_type_info != NULL
				&& simple_type_info->kind == TK_DIRECT
                && simple_type_info->type->kind == STK_USER_DEFINED)
        {
            simple_type_info->type->user_defined_type->is_member = 1;
            simple_type_info->type->user_defined_type->class_type = class_info;
        }

    }

    if (ASTSon1(a) != NULL)
    {
		ASTAttrSetValueType(a, LANG_IS_DECLARATION, tl_type_t, tl_bool(1));
		ASTAttrSetValueType(a, LANG_DECLARATION_SPECIFIERS, tl_type_t, tl_ast(ASTSon0(a)));

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
                        solve_ambiguous_init_declarator(declarator, st, decl_context);
                        // Restart the function
                        build_scope_simple_member_declaration(a, st, current_access, class_info, decl_context);
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
                            /* scope_entry_t* entry = */ build_scope_declarator(identifier, st, &gather_info, 
                                    simple_type_info, &declarator_type, 
                                    decl_context);
                        }

                        AST expression = ASTSon1(declarator);
                        solve_possibly_ambiguous_expression(expression, st, decl_context);
                        break;
                    }
                    // init declarator may appear here because of templates
                case AST_INIT_DECLARATOR :
                case AST_MEMBER_DECLARATOR :
                case AST_GCC_MEMBER_DECLARATOR :
                    {
                        AST declarator_name = get_declarator_name(declarator, st, decl_context);

						ASTAttrSetValueType(declarator, LANG_IS_DECLARED_NAME, tl_type_t, tl_bool(1));
						ASTAttrSetValueType(declarator, LANG_DECLARED_NAME, tl_type_t, tl_ast(declarator_name));

                        AST initializer = ASTSon1(declarator);

						ASTAttrSetValueType(declarator, LANG_INITIALIZER, tl_type_t, tl_ast(initializer));
						
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
                        }

                        type_t* declarator_type = NULL;
                        scope_entry_t* entry = build_scope_declarator(ASTSon0(declarator), st, &gather_info, 
                                simple_type_info, &declarator_type, 
                                new_decl_context);

                        entry->is_member = 1;
                        entry->class_type = class_info;

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
                                            P_LIST_ADD(class_type->constructor_list, class_type->num_constructors, entry);
                                            entry->type_information->function->is_constructor = 1;
                                        }
                                        break;
                                    }
                                case AST_DESTRUCTOR_TEMPLATE_ID : // This can appear here
                                case AST_DESTRUCTOR_ID :
                                    {
                                        // This is the destructor
                                        class_type->destructor = entry;
                                        break;
                                    }
                                case AST_OPERATOR_FUNCTION_ID :
                                case AST_OPERATOR_FUNCTION_ID_TEMPLATE :
                                    {
                                        P_LIST_ADD(class_type->operator_function_list, class_type->num_operator_functions, entry);
                                        break;
                                    }
                                case AST_CONVERSION_FUNCTION_ID :
                                    {
                                        conversion_function_t* new_conversion = calloc(1, sizeof(*new_conversion));

                                        // The conversion type is the return of the conversion function id
                                        new_conversion->conversion_type = entry->type_information->function->return_type;
                                        new_conversion->cv_qualifier = entry->type_information->cv_qualifier;

                                        P_LIST_ADD(class_type->conversion_function_list, class_type->num_conversion_functions, new_conversion);
                                        break;
                                    }
                                case AST_QUALIFIED_ID :
                                    {
                                        // Do nothing with them
                                        // In particular if they come from a friend context
                                        ERROR_CONDITION((!gather_info.is_friend), "I was not expecting a qualified-id in something that is not a friend declaration", 0);
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
                            if (initializer != NULL)
                            {
                                check_for_initialization(initializer, entry->scope, decl_context);
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
static exception_spec_t* build_exception_spec(scope_t* st, AST a, 
        decl_context_t decl_context)
{
    // No exception specifier at all
    if (a == NULL)
        return NULL;

    exception_spec_t* result = calloc(1, sizeof(*result));

    AST type_id_list = ASTSon0(a);

    if (type_id_list == NULL)
        return result;

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
    
        build_scope_decl_specifier_seq(type_specifier_seq, st, &gather_info, &type_info,
                decl_context);

        if (abstract_decl != NULL)
        {
            type_t* declarator_type;
            build_scope_declarator(abstract_decl, st, &gather_info, type_info, &declarator_type,
                    decl_context);
            P_LIST_ADD(result->exception_type_seq, result->num_exception_types,
                    declarator_type);
        }
        else
        {
            type_t* declarator_type = type_info;
            P_LIST_ADD(result->exception_type_seq, result->num_exception_types,
                    declarator_type);
        }
    }

    return result;
}

static void build_scope_template_arguments_for_primary_template(scope_t* st, 
        scope_t* template_scope,
        template_parameter_t** template_parameter_info, 
        int num_template_parameters, template_argument_list_t** template_arguments)
{
    *template_arguments = calloc(sizeof(1), sizeof(*(*template_arguments)));
    (*template_arguments)->num_arguments = 0;

    int i;
    for (i = 0; i < num_template_parameters; i++)
    {
        template_parameter_t* template_parameter = template_parameter_info[i];

        switch (template_parameter->kind)
        {
            case TPK_TYPE :
                {
                    template_argument_t* new_template_argument = calloc(1, sizeof(*new_template_argument));

                    new_template_argument->kind = TAK_TYPE;
                    new_template_argument->type = template_parameter->type_info;
                    new_template_argument->scope = template_scope; 
                    new_template_argument->argument_tree = template_parameter->parameter_tree;

                    P_LIST_ADD((*template_arguments)->argument_list, (*template_arguments)->num_arguments, new_template_argument);

                    break;
                }
            case TPK_TEMPLATE :
                {
                    template_argument_t* new_template_argument = calloc(1, sizeof(*new_template_argument));

                    new_template_argument->kind = TAK_TEMPLATE;
                    new_template_argument->type = template_parameter->type_info;
                    new_template_argument->scope = template_scope; 
                    new_template_argument->argument_tree = template_parameter->parameter_tree;

                    P_LIST_ADD((*template_arguments)->argument_list, (*template_arguments)->num_arguments, new_template_argument);

                    break;
                }
            case TPK_NONTYPE :
                {
                    template_argument_t* new_template_argument = calloc(1, sizeof(*new_template_argument));

                    new_template_argument->kind = TAK_NONTYPE;
                    new_template_argument->type = template_parameter->type_info;

                    char* param_name = template_parameter->template_parameter_name;
                    if (param_name == NULL)
                    {
                        param_name = get_unique_name();

                        // Sign up this artificial identifier used only
                        // for evaluation purposes
                        scope_entry_t* new_entry = new_symbol(template_scope, param_name); 
                        new_entry->kind = SK_TEMPLATE_PARAMETER;
                        new_entry->type_information = template_parameter->type_info;
                    }

                    AST symbol_tree = ASTLeaf(AST_SYMBOL, 0, param_name);
                    AST expression_tree = ASTMake1(AST_EXPRESSION, symbol_tree, 0, NULL);

                    new_template_argument->argument_tree = expression_tree;
                    new_template_argument->scope = copy_scope(template_parameter->default_argument_scope->contained_in);

                    scope_t* old_template_scope = new_template_argument->scope->template_scope;
                    new_template_argument->scope->template_scope = template_parameter->default_argument_scope;
                    new_template_argument->scope->template_scope->template_scope = old_template_scope;

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

#if 0
static void update_template_parameter_types(type_t** update_type,
        template_argument_list_t* argument_list, decl_context_t decl_context)
{
    if (update_type == NULL
            || *update_type == NULL)
        return;

    switch ((*update_type)->kind)
    {
        case TK_REFERENCE :
        case TK_POINTER :
            {
                update_template_parameter_types(&((*update_type)->pointer->pointee), 
                        argument_list, decl_context);
                break;
            }
        case TK_POINTER_TO_MEMBER :
            {
                update_template_parameter_types(&((*update_type)->pointer->pointee), 
                        argument_list, decl_context);
                scope_entry_t* pointee_class = (*update_type)->pointer->pointee_class;
                update_template_parameter_types(&(pointee_class->type_information), 
                        argument_list, decl_context);
                break;
            }
        case TK_FUNCTION :
            {
                int i;
                function_info_t* function_info = (*update_type)->function;

                update_template_parameter_types(&(function_info->return_type), 
                        argument_list, decl_context);

                for (i = 0; i < function_info->num_parameters; i++)
                {
                    update_template_parameter_types(&(function_info->parameter_list[i]->type_info),
                            argument_list, decl_context);
                }
                break;
            }
        case TK_DIRECT :
            {
                if ((*update_type)->type->kind == STK_TYPE_TEMPLATE_PARAMETER)
                {
                    // type_t* replace_type = argument_list->argument_list[(*update_type)->type->template_parameter_num]->type;
                    *update_type = 
                        advance_over_typedefs(argument_list->argument_list[(*update_type)->type->template_parameter_num]->type);
                }
                else if ((*update_type)->type->kind == STK_TEMPLATE_TEMPLATE_PARAMETER)
                {
                    internal_error("Not yet implemented", 0);
                }
                else if ((*update_type)->type->kind == STK_TEMPLATE_DEPENDENT_TYPE)
                {
                    AST a = (*update_type)->type->typeof_expr;
                    AST global_scope = ASTSon0(a);
                    AST nested_name_spec = ASTSon1(a);
                    AST name = ASTSon2(a);

                    scope_t* replace_scope = new_template_scope((*update_type)->type->typeof_scope);

                    Iterator* it = (Iterator*) hash_iterator_create(
                            (*update_type)->type->typeof_scope->hash);
                    for (iterator_first(it); !iterator_finished(it); iterator_next(it))
                    {
                        scope_entry_list_t* entry_list = (scope_entry_list_t*) iterator_item(it);

                        scope_entry_t* entry = entry_list->entry;

                        scope_entry_t* new_entry = new_symbol(replace_scope, entry->symbol_name);

                        type_t* entry_base_type = base_type(entry->type_information);
                        if (entry_base_type->type->template_parameter_num >= argument_list->num_arguments)
                        {
                            continue;
                        }

                        if (entry->kind == SK_TEMPLATE_TYPE_PARAMETER)
                        {
                            new_entry->kind = SK_TYPEDEF;

                            new_entry->type_information = calloc(1, sizeof(*(new_entry->type_information)));
                            new_entry->type_information->kind = TK_DIRECT;

                            new_entry->type_information->type = calloc(1, sizeof(*(new_entry->type_information->type)));
                            new_entry->type_information->type->kind = STK_TYPEDEF;
                            new_entry->type_information->type->aliased_type = copy_type(
                                    advance_over_typedefs(entry->type_information));

                            update_template_parameter_types(&(new_entry->type_information->type->aliased_type),
                                    argument_list, decl_context);
                        }
                        else if (entry->kind == SK_TEMPLATE_PARAMETER)
                        {
                            *new_entry = *entry;
                            new_entry->scope = replace_scope;

                            // This is no more a SK_TEMPLATE_PARAMETER
                            new_entry->kind = SK_VARIABLE;
                            AST expression = duplicate_ast(
                                    argument_list->argument_list[(
                                        entry->type_information->type->template_parameter_num
                                        )]->argument_tree);
                            AST constant_initializer = ASTMake1(AST_CONSTANT_INITIALIZER, expression, ASTLine(expression), NULL);

                            new_entry->expression_value = constant_initializer;
                        }
                        else if (entry->kind == SK_TYPEDEF)
                        {
                            break;
                        }
                        else if (entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
                        {
                            internal_error("Not implemented", 0);
                        }
                        else
                        {
                            internal_error("Unexpected symbol type '%d' line %d\n", entry->kind, entry->line);
                        }
                    }

                    scope_t* search_scope = copy_scope((*update_type)->type->typeof_scope->contained_in);
                    search_scope->template_scope = replace_scope;

                    scope_entry_list_t* entry_list = query_nested_name_flags(search_scope,
                            global_scope, nested_name_spec, name, FULL_UNQUALIFIED_LOOKUP,
                            LF_NO_FAIL, decl_context);

                    if (entry_list != NULL)
                    {
                        scope_entry_t* entry = entry_list->entry;
                        (*update_type) = entry->type_information;
                    }
                    else
                    {
                        internal_error("Argument type not found\n", 0);
                    }
                }
                else if ((*update_type)->type->kind == STK_USER_DEFINED)
                {
                    scope_entry_t* entry = (*update_type)->type->user_defined_type;

                    if (entry->kind == SK_TEMPLATE_TYPE_PARAMETER)
                    {
                        type_t* template_type = entry->type_information;

                        *update_type = argument_list->argument_list[template_type->type->template_parameter_num]->type;
                    }
                    // Handle templates. We create specializations here if needed
                    else if (entry->kind == SK_TEMPLATE_PRIMARY_CLASS
                            || entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS)
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "Updating types of template '%s'\n", entry->symbol_name);
                        }
                        template_argument_list_t* new_template_arguments = 
                            copy_template_argument_list(entry->type_information->type->template_arguments);

                        int i;

                        DEBUG_CODE()
                        {
                            for (i = 0; i < new_template_arguments->num_arguments; i++)
                            {
                                char c = (entry->type_information->type->template_arguments->argument_list[i]->type ==
                                        new_template_arguments->argument_list[i]->type);
                                ERROR_CONDITION(c, "These pointers have to be different!!!\n", 0);
                            }
                        }

                        for (i = 0; i < new_template_arguments->num_arguments; i++)
                        {
                            DEBUG_CODE()
                            {
                                fprintf(stderr, "Before template argument type update %d (%p) : %s\n", i,
                                        new_template_arguments->argument_list[i]->type,
                                        print_declarator(new_template_arguments->argument_list[i]->type, entry->scope));
                            }
                            
                            // update_template_parameter_types(
                            //         &(new_template_arguments->argument_list[i]->type),
                            //         new_template_arguments, decl_context);

                            update_template_parameter_types(
                                    &(new_template_arguments->argument_list[i]->type),
                                    argument_list, decl_context);

                            DEBUG_CODE()
                            {
                                fprintf(stderr, "After template argument type update %d (%p) : %s\n", i,
                                        new_template_arguments->argument_list[i]->type,
                                        print_declarator(new_template_arguments->argument_list[i]->type, entry->scope));
                            }
                        }

                        scope_entry_list_t* candidates = query_unqualified_name(entry->scope, entry->symbol_name);

                        matching_pair_t* new_match_template = solve_template(candidates, new_template_arguments, entry->scope, 
                                /* give_exact_match = */ 1, decl_context);

                        scope_entry_t* new_specialization = NULL;
                        if (new_match_template == NULL)
                        {
                            new_specialization = create_holding_symbol_for_template(entry, new_template_arguments,
                                    entry->scope, entry->line);
                        }
                        else
                        {
                            new_specialization = new_match_template->entry;
                        }

                        type_t* new_user_defined_type = calloc(1, sizeof(*new_user_defined_type));
                        new_user_defined_type->kind = TK_DIRECT;

                        new_user_defined_type->type = calloc(1, sizeof(*(new_user_defined_type->type)));
                        new_user_defined_type->type->kind = STK_USER_DEFINED;
                        new_user_defined_type->type->user_defined_type = new_specialization;

                        *update_type = new_user_defined_type;
                    }
                }

                break;
            }
        case TK_ARRAY :
            {
                update_template_parameter_types(&((*update_type)->array->element_type), 
                        argument_list, decl_context);
                break;
            }
        default :
            {
                internal_error("Unknown type kind '%d'\n", (*update_type)->kind);
                break;
            }
    }
}
#endif

// Replaces symbols of template parameters with template arguments seen so far
scope_t* replace_template_parameters_with_template_arguments(template_parameter_t* curr_template_parameter, 
        template_parameter_t** template_parameter_list,
        template_argument_list_t** template_arguments)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "Replacing template parameters up to parameter %d\n", (*template_arguments)->num_arguments - 1);
    }
    // Replace types in the parameter scope
    scope_t* replace_scope = new_template_scope(curr_template_parameter->default_argument_scope->contained_in);

    Iterator* it = (Iterator*) hash_iterator_create(
            curr_template_parameter->default_argument_scope->hash);
    for (iterator_first(it); !iterator_finished(it); iterator_next(it))
    {
        scope_entry_list_t* entry_list = (scope_entry_list_t*) iterator_item(it);

        scope_entry_t* entry = entry_list->entry;

        scope_entry_t* new_entry = new_symbol(replace_scope, entry->symbol_name);

        type_t* entry_base_type = base_type(entry->type_information);
        if (entry_base_type->type->template_parameter_num >=
                (*template_arguments)->num_arguments)
        {
            // This argument has not still been binded
            continue;
        }

        if (entry->kind == SK_TEMPLATE_TYPE_PARAMETER)
        {
            new_entry->kind = SK_TYPEDEF;

            new_entry->type_information = calloc(1, sizeof(*(new_entry->type_information)));
            new_entry->type_information->kind = TK_DIRECT;

            type_t* template_argument_type = 
                ((*template_arguments)->argument_list)[entry_base_type->type->template_parameter_num]->type;

            new_entry->type_information->type = calloc(1, sizeof(*(new_entry->type_information->type)));
            new_entry->type_information->type->kind = STK_TYPEDEF;
            new_entry->type_information->type->aliased_type = copy_type(
                    advance_over_typedefs(template_argument_type));

            DEBUG_CODE()
            {
                fprintf(stderr, "Making type template parameter '%s' of type '%s'\n", 
                        new_entry->symbol_name,
                        print_declarator(new_entry->type_information, replace_scope));
            }
        }
        else if (entry->kind == SK_TEMPLATE_PARAMETER)
        {
            // *new_entry = *entry;
            // new_entry->scope = replace_scope;

            // This is no more a SK_TEMPLATE_PARAMETER
            new_entry->kind = SK_VARIABLE;
            AST expression = duplicate_ast(
                    (*template_arguments)->
                    argument_list[entry->type_information->type->template_parameter_num]->argument_tree);
            AST constant_initializer = ASTMake1(AST_CONSTANT_INITIALIZER, expression, ASTLine(expression), 
                    NULL);

            new_entry->expression_value = constant_initializer;

            int k = entry_base_type->type->template_parameter_num;
            new_entry->type_information = template_parameter_list[k]->type_info;

            DEBUG_CODE()
            {
                fprintf(stderr, "Making non-type template parameter '%s' to have value %s\n", 
                        new_entry->symbol_name,
                        prettyprint_in_buffer(constant_initializer));
            }
        }
        else if (entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
        {
            new_entry->kind = SK_TEMPLATE_ALIAS;
            type_t* template_argument_type = 
                ((*template_arguments)->argument_list)[entry_base_type->type->template_parameter_num]->type;

            new_entry->template_alias_type = template_argument_type;

            DEBUG_CODE()
            {
                fprintf(stderr, "Making template template parameter '%s' to be an alias to %s\n", 
                        new_entry->symbol_name,
                        print_declarator(template_argument_type, replace_scope));
            }
        }
        else
        {
            internal_error("Unexpected node type '%d'\n", entry->kind);
        }
    }

    // DEBUG_CODE()
    // {
    //     fprintf(stderr, "#############\n");
    //     print_scope(replace_scope);
    //     fprintf(stderr, "#############\n");
    // }

    return replace_scope;
}

void build_scope_template_arguments(AST class_head_id, 
        scope_t* primary_template_scope, // Where the primary template id is looked up 
                                         // (A::B<int>, the primary template is searched in A::)
        scope_t* arguments_scope,        // Where the arguments are looked up (in the point of the declaration)
        scope_t* template_scope,         // Arguments that are expressions and might involve template symbols
        template_argument_list_t** template_arguments,
        decl_context_t decl_context)
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
    // Count the arguments
    if (list != NULL)
    {
        for_each_element(list, iter)
        {
            num_arguments++;
        }
    }

    solve_possibly_ambiguous_template_id(class_head_id, arguments_scope, decl_context);
    
    // Complete arguments with default ones
    //
    // First search primary template
    AST template_name = ASTSon0(class_head_id);

    DEBUG_CODE()
    {
        fprintf(stderr, "Looking for primary template '%s' in '%p'\n", ASTText(template_name), primary_template_scope);
    }

    scope_entry_list_t* templates_list = query_unqualified_name(primary_template_scope, ASTText(template_name));
    
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
    if (primary_template_list->entry->kind == SK_TEMPLATE_ALIAS)
    {
        type_t* alias_type_info = primary_template_list->entry->template_alias_type;

        if (alias_type_info->kind != TK_DIRECT
                || alias_type_info->type->kind != STK_USER_DEFINED)
        {
            internal_error("Expected a template name type\n", 0);
        }

        primary_template_list = create_list_from_entry(alias_type_info->type->user_defined_type);
    }

    scope_entry_t* primary_template = NULL;
    primary_template = primary_template_list->entry;

    list = ASTSon1(class_head_id);
    if (list != NULL)
    {
        for_each_element(list, iter)
        {
            AST template_argument = ASTSon1(iter);

            switch (ASTType(template_argument))
            {
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

                        build_scope_decl_specifier_seq(type_specifier_seq, arguments_scope, &gather_info, &type_info,
                                decl_context);

                        type_t* declarator_type;
                        if (abstract_decl != NULL)
                        {
                            build_scope_declarator(abstract_decl, arguments_scope, &gather_info, type_info, &declarator_type,
                                    decl_context);
                        }
                        else
                        {
                            declarator_type = type_info;
                        }
                        new_template_argument->type = declarator_type;
                        new_template_argument->argument_tree = template_argument;
                        new_template_argument->scope = copy_scope(arguments_scope);

                        // Finally add to the template argument list
                        P_LIST_ADD((*template_arguments)->argument_list, (*template_arguments)->num_arguments, new_template_argument);
                        break;
                    }
                case AST_TEMPLATE_EXPRESSION_ARGUMENT :
                    {
                        if ((*template_arguments)->num_arguments >= 
                                primary_template->num_template_parameters)
                        {
                            // Do nothing, we may be checking for ambiguities
                            break;
                        }
                        // This expression is of limited nature
                        template_argument_t* new_template_argument = calloc(1, sizeof(*new_template_argument));
                        new_template_argument->kind = TAK_NONTYPE;

                        AST expr_template_argument = ASTSon0(template_argument);

                        new_template_argument->argument_tree = expr_template_argument;
                        new_template_argument->scope = template_scope;

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

    if (primary_template->num_template_parameters > num_arguments)
    {
        // We have to complete with default arguments
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


            if (curr_template_parameter->default_argument_scope == NULL)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Not completing because of lacking information for parameter #%d\n", k);
                }
                continue;
            }

            scope_t* replace_scope = replace_template_parameters_with_template_arguments(curr_template_parameter,
                    primary_template->template_parameter_info,
                    template_arguments);
            scope_t* template_argument_scope = curr_template_parameter->default_argument_scope->contained_in;
            curr_template_arg->scope = copy_scope(template_argument_scope);
            curr_template_arg->scope->template_scope = replace_scope;

            switch (curr_template_parameter->kind)
            {
                case TPK_TEMPLATE :
                    {
                        curr_template_arg->kind = TAK_TEMPLATE;

                        curr_template_arg->argument_tree = curr_template_parameter->default_tree;

                        AST id_expression = curr_template_arg->argument_tree;

                        scope_entry_list_t* entry_list = query_id_expression(curr_template_arg->scope, id_expression, 
                                FULL_UNQUALIFIED_LOOKUP, decl_context);

                        if (entry_list == NULL)
                        {
                            internal_error("Template template parameter default argument '%s' not found\n", 
                                    prettyprint_in_buffer(curr_template_parameter->default_tree));
                        }

                        enum cxx_symbol_kind template_template_classes[2] = 
                        {
                            SK_TEMPLATE_PRIMARY_CLASS, 
                            SK_TEMPLATE_ALIAS
                        };

                        entry_list = filter_symbol_kind_set(entry_list, 2, template_template_classes);

                        if (entry_list == NULL)
                        {
                            internal_error("Template template parameter default argument '%s' not found\n", 
                                    prettyprint_in_buffer(curr_template_parameter->default_tree));
                        }

                        scope_entry_t* entry = entry_list->entry;

                        if (entry->kind == SK_TEMPLATE_ALIAS)
                        {
                            curr_template_arg->type = entry->template_alias_type;
                        }
                        else
                        {
                            simple_type_t* simple_type_info = calloc(1, sizeof(*simple_type_info));
                            
                            simple_type_info->kind = STK_USER_DEFINED;
                            simple_type_info->user_defined_type = entry;

                            // It is useful to save this when printing types
                            simple_type_info->typeof_expr = id_expression;
                            simple_type_info->typeof_scope = copy_scope(curr_template_arg->scope);

                            curr_template_arg->type = simple_type_to_type(simple_type_info);
                        }

                        break;
                    }
                case TPK_TYPE :
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "Replacing argument scope symbols\n");
                        }

                        curr_template_arg->kind = TAK_TYPE;

                        curr_template_arg->argument_tree = curr_template_parameter->default_tree;

                        // Construct the type
                        AST default_arg_type_spec_seq = ASTSon0(curr_template_arg->argument_tree);
                        // This declarator can be null
                        AST default_arg_declarator = ASTSon1(curr_template_arg->argument_tree);

                        type_t* type_info = NULL;
                        gather_decl_spec_t gather_info;
                        memset(&gather_info, 0, sizeof(gather_info));

                        build_scope_decl_specifier_seq(default_arg_type_spec_seq, curr_template_arg->scope, 
                                &gather_info, &type_info, decl_context);

                        if (default_arg_declarator != NULL)
                        {
                            type_t* declarator_type = NULL;
                            build_scope_declarator(default_arg_declarator, curr_template_arg->scope, &gather_info,
                                    type_info, &declarator_type, decl_context);
                            curr_template_arg->type = declarator_type;
                        }
                        else
                        {
                            curr_template_arg->type = type_info;
                        }

                        break;
                    }
                case TPK_NONTYPE :
                    {
                        curr_template_arg->kind = TAK_NONTYPE;
                        curr_template_arg->argument_tree = curr_template_parameter->default_tree;
                        curr_template_arg->type = curr_template_parameter->type_info;
                        break;
                    }
                default:
                    internal_error("Unknown template parameter kind %d\n", curr_template_parameter->kind);
            }

            // Was given implicitly
            curr_template_arg->implicit = 1;

            DEBUG_CODE()
            {
                fprintf(stderr, "Adding '%s' as a template argument of '%s'\n", 
                        prettyprint_in_buffer(curr_template_arg->argument_tree),
                        prettyprint_in_buffer(class_head_id));
            }
            // Finally add to the template argument list
            P_LIST_ADD((*template_arguments)->argument_list, (*template_arguments)->num_arguments, curr_template_arg);
        }
#if 0
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

            // Something is wrong here, maybe due to an ambiguity
            if (curr_template_parameter->default_argument_scope == NULL)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Not completing because of lacking information for parameter #%d\n", k);
                }
            }
            else
            {
                switch (curr_template_parameter->kind)
                {
                    case TPK_TEMPLATE :
                        {
                            scope_t* template_argument_scope = curr_template_parameter->default_argument_scope->contained_in;

                            curr_template_arg->kind = TAK_TEMPLATE;
                            curr_template_arg->scope = copy_scope(template_argument_scope);

                            curr_template_arg->argument_tree = curr_template_parameter->default_tree;

                            curr_template_arg->type = 
                                primary_template->template_parameter_info[(*template_arguments)->num_arguments]->default_type;

                            DEBUG_CODE()
                            {
                                fprintf(stderr, "Before template template type update: %s\n",
                                        print_declarator(curr_template_arg->type, curr_template_arg->scope));
                            }
                            if ((curr_template_arg->type->kind == TK_DIRECT)
                                    && (curr_template_arg->type->type->kind == STK_USER_DEFINED)
                                    && (curr_template_arg->type->type->user_defined_type->kind 
                                        == SK_TEMPLATE_TEMPLATE_PARAMETER))
                            {
                                *(curr_template_arg->type) = 
                                    *((*template_arguments)->
                                            argument_list[curr_template_arg->type->type->template_parameter_num]->type);
                            }
                            DEBUG_CODE()
                            {
                                fprintf(stderr, "After template template type update: %s\n", 
                                        print_declarator(curr_template_arg->type, curr_template_arg->scope));
                            }

                            break;
                        }
                    case TPK_TYPE :
                        {
                            scope_t* template_argument_scope = curr_template_parameter->default_argument_scope->contained_in;

                            curr_template_arg->kind = TAK_TYPE;
                            curr_template_arg->scope = copy_scope(template_argument_scope);

                            curr_template_arg->argument_tree = curr_template_parameter->default_tree;

                            curr_template_arg->type = 
                                copy_type(primary_template->template_parameter_info[(*template_arguments)->num_arguments]
                                        ->default_type);

                            DEBUG_CODE()
                            {
                                fprintf(stderr, "Before template type update: %s\n",
                                print_declarator(curr_template_arg->type, curr_template_arg->scope));
                            }
                            update_template_parameter_types(&(curr_template_arg->type),
                                    (*template_arguments), decl_context);

                            DEBUG_CODE()
                            {
                                fprintf(stderr, "After template type update: %s\n",
                                print_declarator(curr_template_arg->type, curr_template_arg->scope));
                            }

                            break;
                        }
                    case TPK_NONTYPE :
                        {
                            scope_t* template_argument_scope = curr_template_parameter->default_argument_scope->contained_in;

                            curr_template_arg->kind = TAK_NONTYPE;
                            curr_template_arg->argument_tree = curr_template_parameter->default_tree;
                            curr_template_arg->scope = copy_scope(template_argument_scope);

                            // Replace types in the parameter scope
                            scope_t* replace_scope = new_template_scope(curr_template_parameter->default_argument_scope->contained_in);

                            Iterator* it = (Iterator*) hash_iterator_create(
                                    curr_template_parameter->default_argument_scope->hash);
                            for (iterator_first(it); !iterator_finished(it); iterator_next(it))
                            {
                                scope_entry_list_t* entry_list = (scope_entry_list_t*) iterator_item(it);

                                scope_entry_t* entry = entry_list->entry;

                                scope_entry_t* new_entry = new_symbol(replace_scope, entry->symbol_name);

                                type_t* entry_base_type = base_type(entry->type_information);
                                if (entry_base_type->type->template_parameter_num >=
                                        (*template_arguments)->num_arguments)
                                {
                                    continue;
                                }


                                if (entry->kind == SK_TEMPLATE_TYPE_PARAMETER)
                                {
                                    new_entry->kind = SK_TYPEDEF;

                                    new_entry->type_information = calloc(1, sizeof(*(new_entry->type_information)));
                                    new_entry->type_information->kind = TK_DIRECT;

                                    new_entry->type_information->type = calloc(1, sizeof(*(new_entry->type_information->type)));
                                    new_entry->type_information->type->kind = STK_TYPEDEF;
                                    new_entry->type_information->type->aliased_type = copy_type(
                                            advance_over_typedefs(entry->type_information));

                                    update_template_parameter_types(&(new_entry->type_information->type->aliased_type),
                                            (*template_arguments), decl_context);
                                }
                                else if (entry->kind == SK_TEMPLATE_PARAMETER)
                                {
                                    *new_entry = *entry;
                                    new_entry->scope = replace_scope;

                                    // This is no more a SK_TEMPLATE_PARAMETER
                                    new_entry->kind = SK_VARIABLE;
                                    AST expression = duplicate_ast(
                                            (*template_arguments)->
                                            argument_list[entry->type_information->type->template_parameter_num]->argument_tree);
                                    AST constant_initializer = ASTMake1(AST_CONSTANT_INITIALIZER, expression, ASTLine(expression), 
                                            NULL);

                                    new_entry->expression_value = constant_initializer;
                                }
                                else if (entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
                                {
                                    internal_error("Not implemented", 0);
                                }
                                else
                                {
                                    internal_error("Unexpected node type '%d'\n", entry->kind);
                                }
                            }
                                    
                            scope_t* old_template_scope = curr_template_arg->scope->template_scope;
                            curr_template_arg->scope->template_scope = replace_scope;
                            curr_template_arg->scope->template_scope->template_scope = old_template_scope;

                            break;
                        }
                    default:
                        internal_error("Unknown template parameter kind %d\n", curr_template_parameter->kind);
                }

            }

        }
#endif
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

typedef void (*stmt_scope_handler_t)(AST a, scope_t* st, decl_context_t decl_context, char* attrib_to_set);
typedef 
struct stmt_scope_handler_map_tag
{
	void (*handler)(AST a, scope_t* st, decl_context_t decl_context, char* attrib_to_set);
	char* attr_name;
} stmt_scope_handler_map_t;

static void build_scope_compound_statement(AST a, scope_t* st, decl_context_t decl_context, char* attr_name)
{
    scope_t* block_scope = new_block_scope(st, st->prototype_scope, st->function_scope);

	// Note that we set the scope link to the list node and no to the compound statement
	// node because we want to have the scope just before the compound and after the compound
	//
	//   the scope of the compound -> *  {
	//                                      * <-- the list scope
	//                                   }
	//
	// This adds a bit of burden when enlarging an empty compound

    AST list = ASTSon0(a);
    if (list != NULL)
    {
		scope_link_set(compilation_options.scope_link, list, copy_scope(block_scope));

		build_scope_statement_seq(list, block_scope, decl_context);
    }

	ASTAttrSetValueType(a, LANG_IS_COMPOUND_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_condition(AST a, scope_t* st, decl_context_t decl_context)
{
	// scope_link_set(compilation_options.scope_link, a, copy_scope(st));

    if (ASTSon0(a) != NULL 
            && ASTSon1(a) != NULL)
    {
        // This condition declares something in this scope
        AST type_specifier_seq = ASTSon0(a);
        AST declarator = ASTSon1(a);

        if (ASTType(type_specifier_seq) == AST_AMBIGUITY)
        {
            solve_ambiguous_decl_specifier_seq(type_specifier_seq, st, decl_context);
        }
        
        ERROR_CONDITION((ASTType(declarator) == AST_AMBIGUITY), "Unexpected ambiguity", 0);

        // A type_specifier_seq is essentially a subset of a
        // declarator_specifier_seq so we can reuse existing functions
        type_t* type_info = NULL;
        gather_decl_spec_t gather_info;
        memset(&gather_info, 0, sizeof(gather_info));
    
        build_scope_decl_specifier_seq(type_specifier_seq, st, &gather_info, &type_info,
                decl_context);

        type_t* declarator_type = NULL;
        scope_entry_t* entry = build_scope_declarator(declarator, st, &gather_info, type_info, &declarator_type,
                decl_context);

        solve_possibly_ambiguous_expression(ASTSon2(a), st, decl_context);
        
        entry->expression_value = ASTSon2(a);
    }
    else
    {
        solve_possibly_ambiguous_expression(ASTSon2(a), st, decl_context);
		ASTAttrSetValueType(a, LANG_IS_EXPRESSION_NEST, tl_type_t, tl_bool(1));
		ASTAttrSetValueType(a, LANG_EXPRESSION_NESTED, tl_type_t, tl_ast(ASTSon2(a)));
    }
}

static void build_scope_while_statement(AST a, scope_t* st, decl_context_t decl_context, char* attr_name)
{
    scope_t* block_scope = new_block_scope(st, st->prototype_scope, st->function_scope);

    build_scope_condition(ASTSon0(a), block_scope, decl_context);
	scope_link_set(compilation_options.scope_link, ASTSon0(a), copy_scope(block_scope));

    if (ASTSon1(a) != NULL)
    {
        build_scope_statement(ASTSon1(a), block_scope, decl_context);
		scope_link_set(compilation_options.scope_link, ASTSon1(a), copy_scope(block_scope));
    }

	ASTAttrSetValueType(a, LANG_IS_WHILE_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_ambiguity_handler(AST a, scope_t* st, decl_context_t decl_context, 
		char* attrib_to_set)
{
    solve_ambiguous_statement(a, st, decl_context);
    // Restart
    build_scope_statement(a, st, decl_context);
}

static void build_scope_declaration_statement(AST a, scope_t* st, decl_context_t decl_context, char* attr_name)
{
    AST declaration = ASTSon0(a);

    build_scope_declaration(declaration, st, decl_context);

	ASTAttrSetValueType(a, LANG_IS_DECLARATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void solve_expression_ambiguities(AST a, scope_t* st, decl_context_t decl_context, char* attr_name)
{
    solve_possibly_ambiguous_expression(ASTSon0(a), st, decl_context);

	ASTAttrSetValueType(a, LANG_IS_EXPRESSION_NEST, tl_type_t, tl_bool(1));
	ASTAttrSetValueType(a, LANG_EXPRESSION_NESTED, tl_type_t, tl_ast(ASTSon0(a)));
}

static void build_scope_if_else_statement(AST a, scope_t* st, decl_context_t decl_context, char* attr_name)
{
    scope_t* block_scope = new_block_scope(st, st->prototype_scope, st->function_scope);

    AST condition = ASTSon0(a);
    build_scope_condition(condition, block_scope, decl_context);
	scope_link_set(compilation_options.scope_link, condition, copy_scope(block_scope));

    AST then_branch = ASTSon1(a);
    build_scope_statement(then_branch, block_scope, decl_context);
	scope_link_set(compilation_options.scope_link, then_branch, copy_scope(block_scope));

    AST else_branch = ASTSon2(a);
    if (else_branch != NULL)
    {
        build_scope_statement(else_branch, block_scope, decl_context);
		scope_link_set(compilation_options.scope_link, else_branch, copy_scope(block_scope));
    }

	ASTAttrSetValueType(a, LANG_IS_IF_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_for_statement(AST a, scope_t* st, decl_context_t decl_context, char* attr_name)
{
    AST for_init_statement = ASTSon0(a);
    AST condition = ASTSon1(a);
    AST expression = ASTSon2(a);
    AST statement = ASTSon3(a);

    if (ASTType(for_init_statement) == AST_AMBIGUITY)
    {
        solve_ambiguous_for_init_statement(for_init_statement, st, decl_context);
    }

    scope_t* block_scope = new_block_scope(st, st->prototype_scope, st->function_scope);

    if (ASTType(for_init_statement) == AST_SIMPLE_DECLARATION)
    {
        build_scope_simple_declaration(for_init_statement, block_scope, decl_context);
    }
    else if (ASTType(for_init_statement) == AST_EXPRESSION_STATEMENT)
    {
        solve_expression_ambiguities(for_init_statement, block_scope, decl_context, LANG_IS_EXPRESSION_STATEMENT);
    }
	scope_link_set(compilation_options.scope_link, for_init_statement, copy_scope(block_scope));

    if (condition != NULL)
    {
        build_scope_condition(condition, block_scope, decl_context);
		scope_link_set(compilation_options.scope_link, condition, copy_scope(block_scope));
    }

    if (expression != NULL)
    {
        solve_possibly_ambiguous_expression(expression, block_scope, decl_context);
		scope_link_set(compilation_options.scope_link, expression, copy_scope(block_scope));
    }
    
    build_scope_statement(statement, block_scope, decl_context);
	scope_link_set(compilation_options.scope_link, statement, copy_scope(block_scope));

	ASTAttrSetValueType(a, LANG_IS_FOR_STATEMENT, tl_type_t, tl_bool(1));
	ASTAttrSetValueType(a, LANG_FOR_INIT_CONSTRUCT, tl_type_t, tl_ast(for_init_statement));
	ASTAttrSetValueType(a, LANG_FOR_CONDITION, tl_type_t, tl_ast(condition));
	ASTAttrSetValueType(a, LANG_FOR_ITERATION_EXPRESSION, tl_type_t, tl_ast(expression));
	ASTAttrSetValueType(a, LANG_FOR_BODY_STATEMENT, tl_type_t, tl_ast(statement));
}

static void build_scope_switch_statement(AST a, scope_t* st, decl_context_t decl_context, char* attr_name)
{
    scope_t* block_scope = new_block_scope(st, st->prototype_scope, st->function_scope);

    AST condition = ASTSon0(a);
    AST statement = ASTSon1(a);

    build_scope_condition(condition, block_scope, decl_context);
	scope_link_set(compilation_options.scope_link, condition, copy_scope(block_scope));

    build_scope_statement(statement, block_scope, decl_context);
	scope_link_set(compilation_options.scope_link, statement, copy_scope(block_scope));

	ASTAttrSetValueType(a, LANG_IS_SWITCH_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_labeled_statement(AST a, scope_t* st, decl_context_t decl_context, char* attr_name)
{
    AST statement = ASTSon1(a);
    build_scope_statement(statement, st, decl_context);

	ASTAttrSetValueType(a, LANG_IS_LABELED_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_default_statement(AST a, scope_t* st, decl_context_t decl_context, char* attr_name)
{
    AST statement = ASTSon0(a);
    build_scope_statement(statement, st, decl_context);

	ASTAttrSetValueType(a, LANG_IS_DEFAULT_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_case_statement(AST a, scope_t* st, decl_context_t decl_context, char* attr_name)
{
    AST constant_expression = ASTSon0(a);
    AST statement = ASTSon1(a);
    solve_possibly_ambiguous_expression(constant_expression, st, decl_context);

    build_scope_statement(statement, st, decl_context);

	ASTAttrSetValueType(a, LANG_IS_CASE_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_return_statement(AST a, scope_t* st, decl_context_t decl_context, char* attr_name)
{
    AST expression = ASTSon0(a);
    if (expression != NULL)
    {
        solve_possibly_ambiguous_expression(expression, st, decl_context);
    }

	ASTAttrSetValueType(a, LANG_IS_RETURN_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_try_block(AST a, scope_t* st, decl_context_t decl_context, char* attr_name)
{
    AST compound_statement = ASTSon0(a);

    build_scope_statement(compound_statement, st, decl_context);

    AST handler_seq = ASTSon1(a);
    AST iter;

    for_each_element(handler_seq, iter)
    {
        AST handler = ASTSon1(iter);

        AST exception_declaration = ASTSon0(handler);
        AST compound_statement = ASTSon1(handler);

        if (ASTType(exception_declaration) != AST_ANY_EXCEPTION)
        {
            scope_t* block_scope = new_block_scope(st, st->prototype_scope, st->function_scope);

			scope_link_set(compilation_options.scope_link, exception_declaration, copy_scope(block_scope));

            AST type_specifier_seq = ASTSon0(exception_declaration);
            // This declarator can be null
            AST declarator = ASTSon1(exception_declaration);

            type_t* type_info = NULL;
            gather_decl_spec_t gather_info;
            memset(&gather_info, 0, sizeof(gather_info));

            build_scope_decl_specifier_seq(type_specifier_seq, block_scope, &gather_info, &type_info,
                    decl_context);

            if (declarator != NULL)
            {
                type_t* declarator_type = NULL;
                build_scope_declarator(declarator, block_scope, &gather_info, type_info, &declarator_type,
                        decl_context);
            }
        }

        build_scope_statement(compound_statement, st, decl_context);
    }

	ASTAttrSetValueType(a, LANG_IS_TRY_BLOCK, tl_type_t, tl_bool(1));
}

static void build_scope_do_statement(AST a, scope_t* st, decl_context_t decl_context, char* attr_name) 
{
    AST statement = ASTSon0(a);
    AST expression = ASTSon1(a);

    build_scope_statement(statement, st, decl_context);
    solve_possibly_ambiguous_expression(expression, st, decl_context);

	ASTAttrSetValueType(a, LANG_IS_DO_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_null(AST a, scope_t* st, decl_context_t decl_context, char* attr_name)
{
    // Do nothing
}

/* OpenMP 2.5 handlers */
static void build_scope_omp_data_clause(AST a, scope_t* st, decl_context_t decl_context)
{
	AST list = a;
	AST iter;

	for_each_element(list, iter)
	{
		AST variable = ASTSon1(iter);

		// This is not technically an expression (just a name for an object),
		// but this is a strict subset of expressions, so we use the same code
		solve_possibly_ambiguous_expression(variable, st, decl_context);
	}
}


static void build_scope_omp_directive(AST a, scope_t* st, decl_context_t decl_context, char* attr_name) 
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
						solve_possibly_ambiguous_expression(expression, st, decl_context);
						ASTAttrSetValueType(clause, OMP_IS_IF_CLAUSE, tl_type_t, tl_bool(1));
						break;
					}
				case AST_OMP_NUM_THREADS_CLAUSE :
					{
						AST expression = ASTSon0(clause);
						solve_possibly_ambiguous_expression(expression, st, decl_context);
						ASTAttrSetValueType(clause, OMP_IS_NUM_THREADS_CLAUSE, tl_type_t, tl_bool(1));
						break;
					}
				case AST_OMP_SCHEDULE_CLAUSE :
					{
						ASTAttrSetValueType(clause, OMP_IS_SCHEDULE_CLAUSE, tl_type_t, tl_bool(1));
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
						build_scope_omp_data_clause(ASTSon0(clause), st, decl_context);
						break;
					}
				case AST_OMP_PRIVATE_CLAUSE :
					{
						ASTAttrSetValueType(clause, OMP_IS_PRIVATE_CLAUSE, tl_type_t, tl_bool(1));
						build_scope_omp_data_clause(ASTSon0(clause), st, decl_context);
						break;
					}
				case AST_OMP_FIRSTPRIVATE_CLAUSE :
					{
						ASTAttrSetValueType(clause, OMP_IS_FIRSTPRIVATE_CLAUSE, tl_type_t, tl_bool(1));
						build_scope_omp_data_clause(ASTSon0(clause), st, decl_context);
						break;
					}
				case AST_OMP_LASTPRIVATE_CLAUSE :
					{
						ASTAttrSetValueType(clause, OMP_IS_LASTPRIVATE_CLAUSE, tl_type_t, tl_bool(1));
						build_scope_omp_data_clause(ASTSon0(clause), st, decl_context);
						break;
					}
				case AST_OMP_COPYPRIVATE_CLAUSE :
					{
						ASTAttrSetValueType(clause, OMP_IS_COPYPRIVATE_CLAUSE, tl_type_t, tl_bool(1));
						build_scope_omp_data_clause(ASTSon0(clause), st, decl_context);
						break;
					}
				case AST_OMP_COPYIN_CLAUSE :
					{
						ASTAttrSetValueType(clause, OMP_IS_COPYIN_CLAUSE, tl_type_t, tl_bool(1));
						build_scope_omp_data_clause(ASTSon0(clause), st, decl_context);
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
				case AST_OMP_REDUCTION_CLAUSE :
					{
						build_scope_omp_data_clause(ASTSon1(clause), st, decl_context);

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
						ASTAttrSetValueType(clause, OMP_REDUCTION_OPERATOR, tl_type_t, tl_ast(ASTSon0(clause)));
						ASTAttrSetValueType(clause, OMP_REDUCTION_VARIABLES, tl_type_t, tl_ast(ASTSon1(clause)));
						ASTAttrSetValueType(clause, OMP_REDUCTION_NEUTER, tl_type_t, tl_ast(ASTSon2(clause)));
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

								solve_possibly_ambiguous_expression(expression, st, decl_context);
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

static void build_scope_omp_custom_directive(AST a, scope_t* st, decl_context_t decl_context, char* attr_name)
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

	build_scope_omp_directive(a, st, decl_context, attr_name);
}

static void build_scope_omp_construct(AST a, scope_t* st, decl_context_t decl_context, char* attr_name)
{
	ASTAttrSetValueType(a, OMP_IS_OMP_CONSTRUCT, tl_type_t, tl_bool(1));
	ASTAttrSetValueType(a, attr_name, tl_type_t, tl_bool(1));

	ASTAttrSetValueType(a, OMP_CONSTRUCT_DIRECTIVE, tl_type_t, tl_ast(ASTSon0(a)));

	build_scope_omp_directive(ASTSon0(a), st, decl_context, NULL);
	if (ASTSon1(a) != NULL)
	{
		ASTAttrSetValueType(a, OMP_CONSTRUCT_BODY, tl_type_t, tl_ast(ASTSon1(a)));
		build_scope_statement(ASTSon1(a), st, decl_context);
	}
}

static void build_scope_omp_custom_construct(AST a, scope_t* st, decl_context_t decl_context, char* attr_name)
{
	ASTAttrSetValueType(a, OMP_IS_OMP_CONSTRUCT, tl_type_t, tl_bool(1));
	ASTAttrSetValueType(a, attr_name, tl_type_t, tl_bool(1));

	ASTAttrSetValueType(a, OMP_CONSTRUCT_DIRECTIVE, tl_type_t, tl_ast(ASTSon0(a)));

	build_scope_omp_custom_directive(ASTSon0(a), st, decl_context, NULL);
	if (ASTSon1(a) != NULL)
	{
		ASTAttrSetValueType(a, OMP_CONSTRUCT_BODY, tl_type_t, tl_ast(ASTSon1(a)));
		build_scope_statement(ASTSon1(a), st, decl_context);
	}
}

static void build_scope_omp_threadprivate(AST a, scope_t* st, decl_context_t decl_context, char* attr_name)
{
	ASTAttrSetValueType(a, OMP_IS_THREADPRIVATE_DIRECTIVE, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, OMP_CONSTRUCT_DIRECTIVE, tl_type_t, tl_ast(a));
    ASTAttrSetValueType(ASTSon0(a), OMP_IS_PARAMETER_CLAUSE, tl_type_t, tl_bool(1));
    build_scope_omp_data_clause(ASTSon0(a), st, decl_context);
}

static void build_scope_omp_flush_directive(AST a, scope_t* st, decl_context_t decl_context, char* attr_name)
{
    // At the moment do nothing
	ASTAttrSetValueType(a, OMP_IS_FLUSH_DIRECTIVE, tl_type_t, tl_bool(1));
}

static void build_scope_omp_sections_construct(AST a, scope_t* st, decl_context_t decl_context, char* attr_name)
{
	ASTAttrSetValueType(a, OMP_CONSTRUCT_DIRECTIVE, tl_type_t, tl_ast(ASTSon0(a)));
    build_scope_omp_directive(ASTSon0(a), st, decl_context, NULL);

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
            // build_scope_omp_directive(ASTSon0(omp_section), st, decl_context)

			AST omp_section_body = ASTSon1(omp_section);
            build_scope_statement(omp_section_body, st, decl_context);

			ASTAttrSetValueType(omp_section, OMP_IS_SECTION_CONSTRUCT, tl_type_t, tl_bool(1));
			ASTAttrSetValueType(omp_section, OMP_CONSTRUCT_BODY, tl_type_t, tl_ast(omp_section_body));
        }

		ASTAttrSetValueType(a, OMP_CONSTRUCT_BODY, tl_type_t, tl_ast(section_sequence));
    }

	ASTAttrSetValueType(a, attr_name, tl_type_t, tl_bool(1));
}

static void build_scope_omp_critical_construct(AST a, scope_t* st, decl_context_t decl_context, char* attr_name)
{
	AST critical_directive = ASTSon0(a);
	ASTAttrSetValueType(a, OMP_CONSTRUCT_DIRECTIVE, tl_type_t, tl_ast(critical_directive));

	AST body_construct = ASTSon1(a);
    if (body_construct != NULL)
    {
		ASTAttrSetValueType(a, OMP_CONSTRUCT_BODY, tl_type_t, tl_ast(body_construct));
		build_scope_statement(body_construct, st, decl_context);
    }

	AST region_phrase = ASTSon0(critical_directive);
	if (region_phrase != NULL)
	{
		solve_possibly_ambiguous_expression(region_phrase, st, decl_context);

		ASTAttrSetValueType(region_phrase, OMP_IS_PARAMETER_CLAUSE, tl_type_t, tl_bool(1));
	}

	ASTAttrSetValueType(a, OMP_IS_CRITICAL_CONSTRUCT, tl_type_t, tl_bool(1));
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
    STMT_HANDLER(AST_GOTO_STATEMENT, build_scope_null, LANG_IS_GOTO_STATEMENT),
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
	STMT_HANDLER(AST_OMP_CUSTOM_CONSTRUCT, build_scope_omp_custom_construct, OMP_IS_CUSTOM_CONSTRUCT),
	STMT_HANDLER(AST_OMP_CUSTOM_DIRECTIVE, build_scope_omp_custom_directive, NULL)
};

void build_scope_member_specification_with_scope_link(scope_t* inner_scope, AST member_specification_tree, 
        access_specifier_t current_access, type_t* simple_type_info, 
        decl_context_t decl_context, scope_link_t* scope_link)
{
	compilation_options.scope_link = scope_link;

	build_scope_member_specification(inner_scope, member_specification_tree, 
			current_access, simple_type_info, decl_context);

	compilation_options.scope_link = NULL;
}

static void build_scope_statement_seq(AST a, scope_t* st, decl_context_t decl_context)
{
    AST list = a;
    if (list != NULL)
    {
        AST iter;
        for_each_element(list, iter)
        {
            build_scope_statement(ASTSon1(iter), st, decl_context);
        }
    }
}

void build_scope_statement_seq_with_scope_link(AST a, scope_t* st, scope_link_t* scope_link)
{
	compilation_options.scope_link = scope_link;

	build_scope_statement_seq(a, st, default_decl_context);

	compilation_options.scope_link = NULL;
}

void build_scope_declaration_sequence_with_scope_link(AST a, scope_t* st, decl_context_t decl_context, scope_link_t* scope_link)
{
	compilation_options.scope_link = scope_link;

	build_scope_declaration_sequence(a, st, decl_context);

	compilation_options.scope_link = NULL;
}

static void build_scope_statement(AST a, scope_t* st, decl_context_t decl_context)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "==== Statement line [%s] ====\n", node_information(a));
    }

    stmt_scope_handler_t f = stmt_scope_handlers[ASTType(a)].handler;
	char* attr_name = stmt_scope_handlers[ASTType(a)].attr_name;

    if (f != NULL)
    {
        f(a, st, decl_context, attr_name);
    }
    else
    {
        WARNING_MESSAGE("Statement node type '%s' does not have handler in %s", ast_print_node_type(ASTType(a)),
                node_information(a));
    }
}

/*
 * This function returns the node that holds the name for a non-abstract
 * declarator
 */
AST get_declarator_name(AST a, scope_t* st, decl_context_t decl_context)
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
                return get_declarator_name(ASTSon0(a), st, decl_context); 
                break;
            }
        case AST_POINTER_DECL :
            {
                return get_declarator_name(ASTSon1(a), st, decl_context);
                break;
            }
        case AST_DECLARATOR_ARRAY :
            {
                return get_declarator_name(ASTSon0(a), st, decl_context);
                break;
            }
        case AST_DECLARATOR_FUNC :
            {
                return get_declarator_name(ASTSon0(a), st, decl_context);
                break;
            }
        case AST_DECLARATOR_ID_EXPR :
            {
                return ASTSon0(a);
                break;
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
                solve_ambiguous_declarator(a, st, decl_context);

                // Restart function
                return get_declarator_name(a, st, decl_context);
            }
        default:
            {
                internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(a)));
            }
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
                solve_ambiguous_declarator(a, NULL, decl_context);
                // Restart function
                return get_leftmost_declarator_name(a, decl_context);
            }
        default:
            {
                internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(a)));
            }
    }
}

char* get_conversion_function_name(AST conversion_function_id, scope_t* st, 
        type_t** result_conversion_type, decl_context_t decl_context)
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

    build_scope_decl_specifier_seq(type_specifier, st, &gather_info, &simple_type_info,
            decl_context);

    type_t* type_info = NULL;

    if (conversion_declarator != NULL)
    {
        // Fix this
        build_scope_declarator(conversion_declarator, st, &gather_info, simple_type_info, &type_info,
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

    char* result = "";

    result = strappend(result, "operator ");

    type_t* type_iter = advance_over_typedefs(type_info);

    char* conversion_declarator_name = "";

    while (type_iter->kind == TK_POINTER
            || type_iter->kind == TK_REFERENCE)
    {
        char* current_conversion_declarator = "";
        if (type_iter->kind == TK_POINTER)
        {
            current_conversion_declarator = strappend(current_conversion_declarator, "* ");
        }
        else // if (type_iter->kind == TK_REFERENCE)
        {
            current_conversion_declarator = strappend(current_conversion_declarator, "& ");
        }
        if ((type_iter->cv_qualifier & CV_CONST) == CV_CONST)
        {
            current_conversion_declarator = strappend(current_conversion_declarator, "const ");
        }

        if ((type_iter->cv_qualifier & CV_VOLATILE) == CV_VOLATILE)
        {
            current_conversion_declarator = strappend(current_conversion_declarator, "volatile ");
        }

        conversion_declarator_name = strprepend(conversion_declarator_name, current_conversion_declarator);

        type_iter = advance_over_typedefs(type_iter->pointer->pointee);
    }
    

    if (type_iter->kind != TK_DIRECT)
    {
        internal_error("Expecting a simple type here", 0);
    }

    simple_type_info->type = type_iter->type;

    if ((simple_type_info->cv_qualifier & CV_CONST) == CV_CONST)
    {
        result = strappend(result, "const ");
    }

    if ((simple_type_info->cv_qualifier & CV_VOLATILE) == CV_VOLATILE)
    {
        result = strappend(result, "volatile ");
    }

    if (simple_type_info->type->is_long)
    {
        result = strappend(result, "long ");
    }

    if (simple_type_info->type->is_short)
    {
        result = strappend(result, "short ");
    }

    if (simple_type_info->type->is_unsigned)
    {
        result = strappend(result, "unsigned ");
    }

    switch (simple_type_info->type->kind)
    {
        case STK_BUILTIN_TYPE :
            {
                switch (simple_type_info->type->builtin_type)
                {
                    case BT_INT :
                        result = strappend(result, "int");
                        break;
                    case BT_BOOL :
                        result = strappend(result, "bool");
                        break;
                    case BT_FLOAT :
                        result = strappend(result, "float");
                        break;
                    case BT_DOUBLE :
                        result = strappend(result, "double");
                        break;
                    case BT_WCHAR :
                        result = strappend(result, "wchar_t");
                        break;
                    case BT_CHAR :
                        result = strappend(result, "char");
                        break;
                    case BT_VOID :
                        result = strappend(result, "void");
                        break;
                    default:
                        internal_error("Invalid type %d", 0);
                }
                break;
            }
        case STK_USER_DEFINED :
            {
                type_t* conversion_type = simple_type_info->type->user_defined_type->type_information;

                if (conversion_type->type->kind != STK_TYPE_TEMPLATE_PARAMETER)
                {
                    result = strappend(result, simple_type_info->type->user_defined_type->symbol_name);
                }
                else
                {
                    type_t* template_parameter_type = simple_type_info->type->user_defined_type->type_information;
                    simple_type_t* simple_type = template_parameter_type->type;

                    char c[50];
                    snprintf(c, 49, "#%d-#%d", simple_type->template_parameter_nesting, simple_type->template_parameter_num);
                    result = strappend(result, c);
                }
                break;
            }
        default :
            internal_error("Unexpected simple type kind", 0);
    }

    result = strappend(result, conversion_declarator_name);

    DEBUG_CODE()
    {
        fprintf(stderr, "Conversion function name '%s'\n", result);
    }

    return result;
}

static AST get_enclosing_declaration(AST point_of_declarator)
{
    while (point_of_declarator != NULL
            && ASTType(point_of_declarator) != AST_SIMPLE_DECLARATION
            && ASTType(point_of_declarator) != AST_MEMBER_DECLARATION
            && ASTType(point_of_declarator) != AST_PARAMETER_DECL)
    {
        point_of_declarator = ASTParent(point_of_declarator);
    }

    return point_of_declarator;
}
