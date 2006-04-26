#include <string.h>
#include <stdio.h>
#include "cxx-driver.h"
#include "cxx-buildsymtab.h"
#include "cxx-symtab.h"
#include "cxx-prettyprint.h"
#include "cxx-typeutils.h"
#include "cxx-utils.h"
#include "cxx-cexpr.h"
#include "cxx-ambiguity.h"
#include "cxx-printsymtab.h"
#include "hash_iterator.h"

#include <signal.h>

/*
 * This file builds symbol table. If ambiguous nodes are found disambiguating
 * routines will be called prior to filling symbolic information. Note that
 * disambiguating routines will use the currently built symbol table.
 *
 * Note that some "semantic checks" performed here are intended only to verify
 * that lookup and symbol registration are performed correctly. By no means
 * this is a full type checking phase
 */

static void build_symtab_declaration(AST a, symtab_t* st);
static void build_symtab_declaration_sequence(AST a, symtab_t* st);
static void build_symtab_simple_declaration(AST a, symtab_t* st);
static void build_symtab_decl_specifier_seq(AST a, symtab_t* st, gather_decl_spec_t* gather_info, 
		simple_type_t** type_info);
static symtab_entry_t* build_symtab_declarator(AST a, symtab_t* st, gather_decl_spec_t* gather_info, 
		simple_type_t* type_info, type_t** declarator_type);

static void build_symtab_namespace_definition(AST a, symtab_t* st);
static symtab_entry_t* build_symtab_function_definition(AST a, symtab_t* st);

static void build_symtab_member_declaration(AST a, symtab_t*  st, 
		access_specifier_t current_access, simple_type_t* simple_type_info);
static void build_symtab_simple_member_declaration(AST a, symtab_t*  st, 
		access_specifier_t current_access, simple_type_t* simple_type_info);
static void build_symtab_member_function_definition(AST a, symtab_t*  st, 
		access_specifier_t current_access, simple_type_t* class_info);

static void build_symtab_statement(AST statement, symtab_t* st);

static void gather_type_spec_from_simple_type_specifier(AST a, symtab_t* st, simple_type_t* type_info);
static void gather_type_spec_from_enum_specifier(AST a, symtab_t* st, simple_type_t* type_info);
static void gather_type_spec_from_class_specifier(AST a, symtab_t* st, simple_type_t* type_info);

static void build_symtab_declarator_rec(AST a, symtab_t* st, type_t** declarator_type, AST* declarator_name);

static void gather_decl_spec_information(AST a, symtab_t* st, gather_decl_spec_t* gather_info);
static void gather_type_spec_information(AST a, symtab_t* st, simple_type_t* type_info);

static symtab_entry_t* build_symtab_declarator_name(AST declarator_name, type_t* declarator_type, 
		gather_decl_spec_t* gather_info, symtab_t* st);
static symtab_entry_t* build_symtab_declarator_id_expr(AST declarator_name, type_t* declarator_type, 
		gather_decl_spec_t* gather_info, symtab_t* st);

static void build_symtab_linkage_specifier(AST a, symtab_t* st);
static void build_symtab_linkage_specifier_declaration(AST a, symtab_t* st);

static void build_symtab_template_declaration(AST a, symtab_t* st);

static void build_symtab_template_parameter_list(AST a, symtab_t* st, 
		template_parameter_t** template_param_info, int* num_parameters);
static void build_symtab_template_parameter(AST a, symtab_t* st, 
		template_parameter_t* template_param_info);
static void build_symtab_nontype_template_parameter(AST a, symtab_t* st,
		template_parameter_t* template_param_info);
static void build_symtab_type_template_parameter(AST a, symtab_t* st,
		template_parameter_t* template_param_info);

static symtab_entry_t* register_new_typedef_name(AST declarator_id, type_t* declarator_type, 
		gather_decl_spec_t* gather_info, symtab_t* st);
static symtab_entry_t* register_new_variable_name(AST declarator_id, type_t* declarator_type, 
		gather_decl_spec_t* gather_info, symtab_t* st);
static symtab_entry_t* register_function(AST declarator_id, type_t* declarator_type, 
		gather_decl_spec_t* gather_info, symtab_t* st);

static void build_symtab_template_simple_declaration(AST a, symtab_t* st, symtab_t* template_scope, 
		int num_parameters, template_parameter_t** template_param_info);

static cv_qualifier_t compute_cv_qualifier(AST a);

static exception_spec_t* build_exception_spec(symtab_t* st, AST a);

static AST get_declarator_name(AST a);

static symtab_entry_t* find_function_declaration(symtab_t* st, AST declarator_id, 
		type_t* declarator_type, char* is_overload);

// Current linkage, by default C++
static char* current_linkage = "\"C++\"";

// Builds symtab for the translation unit
void build_symtab_translation_unit(AST a)
{
	AST list = ASTSon0(a);

	if (list == NULL)
		return;

	// The global scope is created here
	compilation_options.global_scope = new_symtab();

	build_symtab_declaration_sequence(list, compilation_options.global_scope);

	fprintf(stderr, "============ SYMBOL TABLE ===============\n");
	print_scope(compilation_options.global_scope);
	fprintf(stderr, "========= End of SYMBOL TABLE ===========\n");
}

static void build_symtab_declaration_sequence(AST list, symtab_t* st)
{
	AST iter;
	for_each_element(list, iter)
	{
		build_symtab_declaration(ASTSon1(iter), st);
	}
}

// Build symtab for a declaration
static void build_symtab_declaration(AST a, symtab_t* st)
{
	switch (ASTType(a))
	{
		case AST_SIMPLE_DECLARATION :
			{
				// Simple declarations are of the form [optional]
				//
				//   int a;
				//   class A { ... } [a];
				//   struct C { ... } [c];
				//   enum E { ... } [e];
				//   int f(int [k]);
				build_symtab_simple_declaration(a, st);
				break;
			}
		case AST_NAMESPACE_DEFINITION :
			{
				// Namespace definitions are of the form
				//   namespace name
				//   {
				//      ...
				//   }
				build_symtab_namespace_definition(a, st);
				break;
			}
		case AST_FUNCTION_DEFINITION :
			{
				// A function definition is of the form
				//   [T] f(T1 t, T2 t, T3 t)
				//   {
				//     ...
				//   }
				build_symtab_function_definition(a, st);
				break;
			}
		case AST_LINKAGE_SPEC :
			{
				build_symtab_linkage_specifier(a, st);
				break;
			}
		case AST_LINKAGE_SPEC_DECL :
			{
				build_symtab_linkage_specifier_declaration(a, st);
				break;
			}
		case AST_EXPORT_TEMPLATE_DECLARATION :
		case AST_TEMPLATE_DECLARATION :
			{
				build_symtab_template_declaration(a, st);
				break;
			}
		case AST_EXPLICIT_INSTANTIATION :
			{
				break;
			}
		case AST_EXPLICIT_SPECIALIZATION :
			{
				break;
			}
		default :
			{
				internal_error("A declaration of kind '%s' is still unsupported\n", 
						ast_print_node_type(ASTType(a)));
				break;
			}
	}
}

// Builds symtab for a simple declaration
static void build_symtab_simple_declaration(AST a, symtab_t* st)
{
	// Empty declarations are meaningless for the symbol table
	// They are of the form
	//    ;
	if (ASTType(a) == AST_EMPTY_DECL)
		return;

	simple_type_t* simple_type_info = NULL;
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
	 * includes a class specifier, enum specifier or an elaborated type name.
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
		build_symtab_decl_specifier_seq(ASTSon0(a), st, &gather_info, &simple_type_info);
	}

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
			AST declarator = ASTSon0(init_declarator);
			AST initializer = ASTSon1(init_declarator);

			type_t* declarator_type;

			// This will create the symbol if it is unqualified
			build_symtab_declarator(declarator, st, &gather_info, 
					simple_type_info, &declarator_type);

			// This is a simple declaration, thus if it does not declare an
			// extern variable or function, the symbol is already defined here
			if (!gather_info.is_extern
					&& declarator_type->kind != TK_FUNCTION)
			{
				AST declarator_name = get_declarator_name(declarator);
				symtab_entry_list_t* entry_list = query_id_expression(st, declarator_name);

				if (entry_list == NULL)
				{
					internal_error("Symbol just declared has not been found in the symtab!", 0);
				}

				// The last entry will hold our symbol, no need to look for it in the list
				if (entry_list->entry->defined)
				{
					running_error("This symbol has already been defined", 0);
				}

				fprintf(stderr, "Defining symbol '");
				prettyprint(stderr, declarator_name);
				fprintf(stderr, "'\n");
				entry_list->entry->defined = 1;

				if (initializer != NULL)
				{
					// We do not fold it here
					entry_list->entry->expression_value = initializer;
				}
			}
			else
			{
				if (initializer != NULL)
				{
					running_error("An extern symbol cannot be initialized");
				}
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
static void build_symtab_decl_specifier_seq(AST a, symtab_t* st, gather_decl_spec_t* gather_info, 
		simple_type_t **simple_type_info)
{
	AST iter, list;

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
		gather_type_spec_information(ASTSon1(a), st, *simple_type_info);
		
		// Now update the type_spec with type information that was caught in the decl_specifier_seq
		if (gather_info->is_long)
		{
			// It is not set to 1 because of gcc long long
			(*simple_type_info)->is_long++;
		}

		if (gather_info->is_short)
		{
			(*simple_type_info)->is_short = 1;
		}

		if (gather_info->is_unsigned)
		{
			(*simple_type_info)->is_unsigned = 1;
		}

		if (gather_info->is_signed)
		{
			(*simple_type_info)->is_signed = 1;
		}
		
		// cv-qualification
		(*simple_type_info)->cv_qualifier = CV_NONE;
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
 * symtab_t* st is unused here
 */
static void gather_decl_spec_information(AST a, symtab_t* st, gather_decl_spec_t* gather_info)
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
			gather_info->is_long = 1;
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
		// Unknown node
		default:
			internal_error("Unknown node '%s'", ast_print_node_type(ASTType(a)));
			break;
	}
}


/*
 * This function fills simple_type_info with type information.
 *
 * symtab_t* st is unused here
 */
static void gather_type_spec_information(AST a, symtab_t* st, simple_type_t* simple_type_info)
{
	switch (ASTType(a))
	{
		case AST_SIMPLE_TYPE_SPECIFIER:
			gather_type_spec_from_simple_type_specifier(a, st, simple_type_info);
			break;
		case AST_ENUM_SPECIFIER :
			gather_type_spec_from_enum_specifier(a, st, simple_type_info);
			break;
		case AST_CLASS_SPECIFIER :
			gather_type_spec_from_class_specifier(a, st, simple_type_info);
			break;
		case AST_CHAR_TYPE :
			simple_type_info->kind = STK_BUILTIN_TYPE;
			simple_type_info->builtin_type= BT_CHAR;
			break;
		case AST_WCHAR_TYPE :
			simple_type_info->kind = STK_BUILTIN_TYPE;
			simple_type_info->builtin_type= BT_WCHAR;
			break;
		case AST_BOOL_TYPE :
			simple_type_info->kind = STK_BUILTIN_TYPE;
			simple_type_info->builtin_type= BT_BOOL;
			break;
		case AST_SHORT_TYPE :
			simple_type_info->kind = STK_BUILTIN_TYPE;
			simple_type_info->builtin_type= BT_INT;
			simple_type_info->is_short = 1;
			break;
		case AST_INT_TYPE :
			simple_type_info->kind = STK_BUILTIN_TYPE;
			simple_type_info->builtin_type= BT_INT;
			break;
		case AST_LONG_TYPE :
			simple_type_info->kind = STK_BUILTIN_TYPE;
			simple_type_info->builtin_type= BT_INT;
			simple_type_info->is_long = 1;
			break;
		case AST_SIGNED_TYPE :
			simple_type_info->kind = STK_BUILTIN_TYPE;
			simple_type_info->builtin_type = BT_INT;
			simple_type_info->is_signed = 1;
			break;
		case AST_UNSIGNED_TYPE :
			simple_type_info->kind = STK_BUILTIN_TYPE;
			simple_type_info->builtin_type = BT_INT;
			simple_type_info->is_unsigned = 1;
			break;
		case AST_FLOAT_TYPE :
			simple_type_info->kind = STK_BUILTIN_TYPE;
			simple_type_info->builtin_type = BT_FLOAT;
			break;
		case AST_DOUBLE_TYPE :
			simple_type_info->kind = STK_BUILTIN_TYPE;
			simple_type_info->builtin_type = BT_DOUBLE;
			break;
		case AST_VOID_TYPE :
			simple_type_info->kind = STK_BUILTIN_TYPE;
			simple_type_info->builtin_type = BT_VOID;
			break;
		default:
			internal_error("Unknown node '%s'", ast_print_node_type(ASTType(a)));
	}
}

/*
 * This routine is called in gather_type_spec_information and its purpose is to fill the simple_type
 * with the proper reference of the user defined type.
 */
static void gather_type_spec_from_simple_type_specifier(AST a, symtab_t* st, simple_type_t* simple_type_info)
{
	// TODO - We shall check nested namespaces and global qualifier, ignore it for now
	AST type_name = ASTSon2(a);

	switch (ASTType(type_name))
	{
		case AST_SYMBOL :
			fprintf(stderr, "Looking up for type '%s' in %p\n", ASTText(type_name), st);
			symtab_entry_list_t* entry_list = query_in_current_and_upper_scope(st, ASTText(type_name));

			// Filter for non types hiding this type name
			// Fix this, it sounds a bit awkward
			symtab_entry_t* simple_type_entry = filter_simple_type_specifier(entry_list);

			if (simple_type_entry == NULL)
			{
				running_error("Identifier '%s' in line %d is not a type\n", ASTText(type_name), 
						ASTLine(type_name));
			}

			if (simple_type_entry->type_information == NULL
					|| simple_type_entry->type_information->kind != TK_DIRECT
					|| simple_type_entry->type_information->type == NULL)
			{
				internal_error("The named type '%s' has no direct type entry in symbol table\n", 
						ASTText(type_name));
			}

			simple_type_info->kind = STK_USER_DEFINED;
			simple_type_info->user_defined_type = simple_type_entry;
			break;
		case AST_TEMPLATE_ID :
			internal_error("What to do to a template id?", 0);
			break;
		default:
			internal_error("Unknown node '%s'", ast_print_node_type(ASTType(a)));
	};
}

/*
 * This function is called for enum specifiers. It saves all enumerated values
 * and if it has been given a name, it is registered in the symtab.
 */
void gather_type_spec_from_enum_specifier(AST a, symtab_t* st, simple_type_t* simple_type_info)
{
	simple_type_info->enum_info = (enum_info_t*) calloc(1, sizeof(*simple_type_info->enum_info));

	simple_type_info->kind = STK_ENUM;

	AST enum_name = ASTSon0(a);
	// If it has name, we register this type name in the symbol table
	if (enum_name != NULL)
	{
		fprintf(stderr, "Registering enum '%s' in %p\n", ASTText(enum_name), st);
		symtab_entry_t* new_entry = new_symbol(st, ASTText(enum_name));

		if (new_entry == NULL)
		{
			running_error("Symbol '%s' redefined in line %d\n", 
					ASTText(enum_name), ASTLine(enum_name));
		}

		new_entry->kind = SK_ENUM;
		// Copy the type because we are creating it and we would clobber it
		// otherwise
		new_entry->type_information = copy_type(simple_type_to_type(simple_type_info));

		// Since this type is not anonymous we'll want that simple_type_info
		// refers to this newly created type
		memset(simple_type_info, 0, sizeof(*simple_type_info));
		simple_type_info->kind = STK_USER_DEFINED;
		simple_type_info->user_defined_type = new_entry;
	}

	AST list, iter;
	list = ASTSon1(a);
	
	literal_value_t enum_value = literal_value_minus_one();

	if (list != NULL)
	{
		// If the type had name, refer to the enum type
		if (simple_type_info->kind == STK_USER_DEFINED)
		{
			simple_type_info = simple_type_info->user_defined_type->type_information->type;
		}

		// For every enumeration, sign them up in the symbol table
		for_each_element(list, iter)
		{
			AST enumeration = ASTSon1(iter);
			AST enumeration_name = ASTSon0(enumeration);
			AST enumeration_expr = ASTSon1(enumeration);

			// Note that enums do not define an additional scope
			fprintf(stderr, "Registering enumerator '%s'\n", ASTText(enumeration_name));
			symtab_entry_t* enumeration_item = new_symbol(st, ASTText(enumeration_name));

			enumeration_item->kind = SK_ENUMERATOR;
			if (enumeration_expr == NULL)
			{
				// If no value, take the previous and increment it
				enum_value = increment_literal_value(enum_value);
			}
			else
			{
				enum_value = evaluate_constant_expression(enumeration_expr, st);
			}

			enumeration_item->expression_value = tree_from_literal_value(enum_value);

			// DEBUG
			fprintf(stderr, "Enumerator '%s' has value = ", ASTText(enumeration_name));
			prettyprint(stderr, enumeration_item->expression_value);
			fprintf(stderr, "\n");
			// - DEBUG

			P_LIST_ADD(simple_type_info->enum_info->enumeration_list, 
					simple_type_info->enum_info->num_enumeration,
					enumeration_item);
		}

	}

}

/*
 * This function is called for class specifiers
 */
void gather_type_spec_from_class_specifier(AST a, symtab_t* st, simple_type_t* simple_type_info)
{
	// TODO - Class head
	AST class_head = ASTSon0(a);
	AST class_key = ASTSon0(class_head);

	AST class_head_identifier = ASTSon2(class_head);
	// AST class_head_base_clause = ASTSon3(class_head);
	// fprintf(stderr, "TODO class head", name);

	simple_type_info->class_info = calloc(1, sizeof(*simple_type_info->class_info));
	simple_type_info->kind = STK_CLASS;

	symtab_t* inner_scope = enter_scope(st);
	
	if (class_head_identifier != NULL)
	{
		// If the class has name, register it in the symbol table
		char* name;
		if (ASTType(class_head_identifier) == AST_SYMBOL
				|| ASTType(class_head_identifier) == AST_TEMPLATE_ID)
		{
			if (ASTType(class_head_identifier) == AST_SYMBOL)
			{
				name = ASTText(class_head_identifier);
			}
			else // AST_TEMPLATE_ID
			{
				name = ASTText(ASTSon0(class_head_identifier));

				simple_type_info->template_arguments = ASTSon1(class_head_identifier);
			}
			fprintf(stderr, "Registering class '%s' in %p\n", name, st);

			symtab_entry_t* entry = new_symbol(st, name);

			entry->kind = SK_CLASS;

			// Copy the type because we are creating it and we would clobber it
			// otherwise
			entry->type_information = copy_type(simple_type_to_type(simple_type_info));

			entry->inner_scope = inner_scope;

			// Since this type is not anonymous we'll want that simple_type_info
			// refers to this newly created type
			memset(simple_type_info, 0, sizeof(*simple_type_info));
			simple_type_info->kind = STK_USER_DEFINED;
			simple_type_info->user_defined_type = entry;
		}
		else
		{
			internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(class_head_identifier)));
		}
	}

	// Member specification
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

	AST member_specification = ASTSon1(a);


	// For every member_declaration
	while (member_specification != NULL)
	{
		// If it has an access specifier, update it
		if (ASTSon0(member_specification) != NULL)
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
					internal_error("Unknown node type '%s'\n", ast_print_node_type(ASTType(ASTSon0(a))));
			}
		}

		// For every member declaration, sign it up in the symbol table for this class
		if (ASTSon1(member_specification) != NULL)
		{
			build_symtab_member_declaration(ASTSon1(member_specification), inner_scope, current_access, simple_type_info);
		}

		member_specification = ASTSon2(member_specification);
	}
}


/*
 * This function creates a full type using the declarator tree in "a".
 *
 * The base type is fetched from "simple_type_info" and then
 * build_symtab_declarator_rec will modify this type to properly represent the
 * correct type.
 *
 * I.e.   int (*f)();
 *
 * Has as a base type "int", but after build_symtab_declarator_rec it will be
 * "pointer to function returning int"
 *
 * If the declarator is not abstract, therefore it has a name,
 * build_symtab_declarator_name is called to sign it up in the symbol table.
 */
static symtab_entry_t* build_symtab_declarator(AST a, symtab_t* st, gather_decl_spec_t* gather_info, 
		simple_type_t* simple_type_info, type_t** declarator_type)
{
	symtab_entry_t* entry = NULL;
	// Set base type
	*declarator_type = simple_type_to_type(simple_type_info);

	AST declarator_name = NULL;

	build_symtab_declarator_rec(a, st, declarator_type, &declarator_name);

	if (declarator_name != NULL)
	{
		entry = build_symtab_declarator_name(declarator_name, *declarator_type, gather_info, st);

		fprintf(stderr, "declaring ");
		prettyprint(stderr, declarator_name);
		fprintf(stderr, " as ");
	}
	print_declarator(*declarator_type, st); fprintf(stderr, "\n");

	return entry;
}

/*
 * This functions converts a type "T" to a "pointer to T"
 */
static void set_pointer_type(type_t** declarator_type, symtab_t* st, AST pointer_tree)
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
				(*declarator_type)->kind = TK_POINTER_TO_MEMBER;

				symtab_entry_list_t* entry_list =
					query_nested_name_spec(st, NULL, ASTSon0(pointer_tree), ASTSon1(pointer_tree));

				if (entry_list != NULL)
				{
					(*declarator_type)->pointer->pointee_class = entry_list->entry;
				}
			}
			(*declarator_type)->pointer->cv_qualifier = compute_cv_qualifier(ASTSon2(pointer_tree));
			break;
		case AST_REFERENCE_SPEC :
			(*declarator_type)->kind = TK_REFERENCE;
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
 * This function covnerts a type "T" to a "array x of T"
 */
static void set_array_type(type_t** declarator_type, symtab_t* st, AST constant_expr)
{
	type_t* element_type = *declarator_type;

	(*declarator_type) = calloc(1, sizeof(*(*declarator_type)));
	(*declarator_type)->kind = TK_ARRAY;
	(*declarator_type)->array = calloc(1, sizeof(*((*declarator_type)->array)));
	(*declarator_type)->array->element_type = element_type;
	(*declarator_type)->array->array_expr = constant_expr;

	(*declarator_type)->function = NULL;
	(*declarator_type)->type = NULL;
	(*declarator_type)->pointer = NULL;
}

/*
 * This function fetches information for every declarator in the
 * parameter_declaration_clause of a functional declarator
 */
static void set_function_parameter_clause(type_t* declarator_type, symtab_t* st, AST parameters)
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
	symtab_t* parameters_scope = enter_scope(st);

	for_each_element(list, iter)
	{
		AST parameter_declaration = ASTSon1(iter);

		if (ASTType(parameter_declaration) == AST_VARIADIC_ARG)
		{
			// TODO - Fix this, we shall remember this last parameter is a variadic
			continue;
		}

		// This is never null
		AST parameter_decl_spec_seq = ASTSon0(parameter_declaration);
		// Declarator can be null
		AST parameter_declarator = ASTSon1(parameter_declaration);
		// Default value can be null
		// AST parameter_default_value = ASTSon2(parameter_declaration);

		gather_decl_spec_t gather_info;
		memset(&gather_info, 0, sizeof(gather_info));
		
		simple_type_t* simple_type_info;

		build_symtab_decl_specifier_seq(parameter_decl_spec_seq, parameters_scope, &gather_info, &simple_type_info);

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
			build_symtab_declarator(parameter_declarator, parameters_scope, 
					&gather_info, simple_type_info, &type_info);
			P_LIST_ADD(declarator_type->function->parameter_list, 
					declarator_type->function->num_parameters, type_info);
		}
		// If we don't have a declarator just save the base type
		else
		{
			type_t* type_info = simple_type_to_type(simple_type_info);
			P_LIST_ADD(declarator_type->function->parameter_list, declarator_type->function->num_parameters, type_info);
		}
	}
}

/*
 * This function converts a type "T" into a "function (...) returning T" type
 */
static void set_function_type(type_t** declarator_type, symtab_t* st, AST parameter, AST cv_qualif, AST except_spec)
{
	type_t* returning_type = *declarator_type;

	(*declarator_type) = calloc(1, sizeof(*(*declarator_type)));
	(*declarator_type)->kind = TK_FUNCTION;
	(*declarator_type)->function = calloc(1, sizeof(*((*declarator_type)->function)));
	(*declarator_type)->function->return_type = returning_type;

	set_function_parameter_clause(*declarator_type, st, parameter);

	(*declarator_type)->function->cv_qualifier = compute_cv_qualifier(cv_qualif);

	(*declarator_type)->function->exception_spec = build_exception_spec(st, except_spec);
	
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
static void build_symtab_declarator_rec(AST a, symtab_t* st, type_t** declarator_type, AST* declarator_name)
{
	if (a == NULL)
	{
		internal_error("This function does not admit NULL trees", 0);
	}

	switch(ASTType(a))
	{
		case AST_DECLARATOR :
		case AST_PARENTHESIZED_ABSTRACT_DECLARATOR :
		case AST_PARENTHESIZED_DECLARATOR :
			{
				build_symtab_declarator_rec(ASTSon0(a), st, declarator_type, declarator_name); 
				break;
			}
		case AST_ABSTRACT_DECLARATOR :
			{
				set_pointer_type(declarator_type, st, ASTSon0(a));
				if (ASTSon1(a) != NULL)
				{
					build_symtab_declarator_rec(ASTSon1(a), st, declarator_type, declarator_name);
				}
				break;
			}
		case AST_POINTER_DECL :
			{
				set_pointer_type(declarator_type, st, ASTSon0(a));
				build_symtab_declarator_rec(ASTSon1(a), st, declarator_type, declarator_name);
				break;
			}
		case AST_ABSTRACT_ARRAY :
			{
				set_array_type(declarator_type, st, ASTSon1(a));
				if (ASTSon0(a) != NULL)
				{
					build_symtab_declarator_rec(ASTSon0(a), st, declarator_type, declarator_name);
				}
				break;
			}
		case AST_DECLARATOR_ARRAY :
			{
				set_array_type(declarator_type, st, ASTSon1(a));
				build_symtab_declarator_rec(ASTSon0(a), st, declarator_type, declarator_name);
				break;
			}
		case AST_ABSTRACT_DECLARATOR_FUNC :
			{
				set_function_type(declarator_type, st, ASTSon1(a), ASTSon2(a), ASTSon3(a));
				if (ASTSon0(a) != NULL)
				{
					build_symtab_declarator_rec(ASTSon0(a), st, declarator_type, declarator_name);
				}
				break;
			}
		case AST_DECLARATOR_FUNC :
			{
				set_function_type(declarator_type, st, ASTSon1(a), ASTSon2(a), ASTSon3(a));
				build_symtab_declarator_rec(ASTSon0(a), st, declarator_type, declarator_name);
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
		default:
			{
				internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(a)));
			}
	}
}

/*
 * This function returns the node that holds the name for a non-abstract
 * declarator
 */
static AST get_declarator_name(AST a)
{
	if (a == NULL)
	{
		internal_error("This function does not admit NULL trees", 0);
	}

	switch(ASTType(a))
	{
		case AST_DECLARATOR :
		case AST_PARENTHESIZED_DECLARATOR :
			{
				return get_declarator_name(ASTSon0(a)); 
				break;
			}
		case AST_POINTER_DECL :
			{
				return get_declarator_name(ASTSon1(a));
				break;
			}
		case AST_DECLARATOR_ARRAY :
			{
				return get_declarator_name(ASTSon0(a));
				break;
			}
		case AST_DECLARATOR_FUNC :
			{
				return get_declarator_name(ASTSon0(a));
				break;
			}
		case AST_DECLARATOR_ID_EXPR :
			{
				return ASTSon0(a);
				break;
			}
		default:
			{
				internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(a)));
			}
	}
}



/*
 * This function fills the symbol table with the information of this declarator
 */
static symtab_entry_t* build_symtab_declarator_name(AST declarator_name, type_t* declarator_type, 
		gather_decl_spec_t* gather_info, symtab_t* st)
{
	switch (ASTType(declarator_name))
	{
		case AST_DECLARATOR_ID_EXPR :
			return build_symtab_declarator_id_expr(declarator_name, declarator_type, gather_info, st);
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
static symtab_entry_t* build_symtab_declarator_id_expr(AST declarator_name, type_t* declarator_type, 
		gather_decl_spec_t* gather_info, symtab_t* st)
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
					return register_new_typedef_name(declarator_id, declarator_type, gather_info, st);
				}
				else
				{
					return register_new_variable_name(declarator_id, declarator_type, gather_info, st);
				}
				break;
			}
		case AST_DESTRUCTOR_ID :
			{
				// An unqualified destructor name "~name"
				// 'name' should be a class in this scope
				AST destructor_id = ASTSon0(declarator_id);
				return register_new_variable_name(destructor_id, declarator_type, gather_info, st);
				break;
			}
		case AST_TEMPLATE_ID :
			{
				// An unqualified template_id "identifier<stuff>"
				break;
			}
		case AST_OPERATOR_FUNCTION_ID :
			{
				// An unqualified operator_function_id "operator +"
				break;
			}
		case AST_CONVERSION_FUNCTION_ID :
			{
				// An unqualified conversion_function_id "operator T"
				// Why this has no qualified equivalent ?
				break;
			}
		// Qualified ones
		case AST_QUALIFIED_ID :
			{
				// A qualified id "a::b::c"
				if (declarator_type->kind != TK_FUNCTION)
				{
					symtab_entry_list_t* entry_list = query_id_expression(st, declarator_id);
					return entry_list->entry;
				}
				else
				{
					char is_overload;
					symtab_entry_t* entry = find_function_declaration(st, declarator_id, declarator_type, &is_overload);
					return entry;
				}
				break;
			}
		case AST_QUALIFIED_TEMPLATE :
			{
				// A qualified template "a::b::template c" [?]
				break;
			}
		case AST_QUALIFIED_TEMPLATE_ID :
			{
				// A qualified template_id "a::b::c<int>"
				break;
			}
		case AST_QUALIFIED_OPERATOR_FUNCTION_ID :
			{
				// A qualified operator function_id "a::b::operator +"
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
static symtab_entry_t* register_new_typedef_name(AST declarator_id, type_t* declarator_type, 
		gather_decl_spec_t* gather_info, symtab_t* st)
{
	// First query for an existing entry
	symtab_entry_list_t* list = query_in_current_scope(st, ASTText(declarator_id));

	// Only enum or classes can exist, otherwise this is an error
	if (list != NULL)
	{
		if (list->next == NULL)
		{
			symtab_entry_t* entry = filter_simple_type_specifier(list);
			// This means this was not just a type specifier 
			if (entry == NULL)
			{
				running_error("Symbol '%s' in line %d has been redeclared as a different symbol kind.",
						ASTText(declarator_id), ASTLine(declarator_id));
			}
		}
		else // More than one symbol sounds extremely suspicious
		{
			running_error("Symbol '%s' in line %d has been redeclared as a different symbol kind.",
					ASTText(declarator_id), ASTLine(declarator_id));
		}
	}

	symtab_entry_t* entry = new_symbol(st, ASTText(declarator_id));

	fprintf(stderr, "Registering typedef '%s'\n", ASTText(declarator_id));

	// Save aliased type under the type of this declaration
	entry->kind = SK_TYPEDEF;
	entry->type_information = calloc(1, sizeof(*(entry->type_information)));
	entry->type_information->kind = TK_DIRECT;
	entry->type_information->type = calloc(1, sizeof(*(entry->type_information->type)));
	entry->type_information->type->kind = STK_TYPEDEF;
	entry->type_information->type->aliased_type = declarator_type;

	// TODO - cv qualification
	return entry;
}

/*
 * This function registers a new "variable" (non type) name
 */
static symtab_entry_t* register_new_variable_name(AST declarator_id, type_t* declarator_type, 
		gather_decl_spec_t* gather_info, symtab_t* st)
{
	if (declarator_type->kind != TK_FUNCTION)
	{
		// Check for existence of this symbol
		symtab_entry_list_t* entry_list = query_in_current_scope(st, ASTText(declarator_id));
		if (entry_list != NULL)
		{
			if ((entry_list->entry->kind != SK_CLASS &&
						entry_list->entry->kind != SK_ENUM) ||
					entry_list->next != NULL)
			{
				running_error("Symbol '%s' has been redefined as another symbol kind", 
						ASTText(declarator_id), entry_list->entry->kind);
			}
		}

		fprintf(stderr, "Registering variable '%s'\n", ASTText(declarator_id));
		symtab_entry_t* entry = new_symbol(st, ASTText(declarator_id));
		entry->kind = SK_VARIABLE;
		entry->type_information = declarator_type;

		return entry;
	}
	else
	{
		return register_function(declarator_id, declarator_type, gather_info, st);
	}
}

static symtab_entry_t* register_function(AST declarator_id, type_t* declarator_type, 
		gather_decl_spec_t* gather_info, symtab_t* st)
{
	symtab_entry_t* entry;

	char is_overload;
	entry = find_function_declaration(st, declarator_id, declarator_type, &is_overload);

	if (entry == NULL)
	{
		if (!is_overload)
		{
			fprintf(stderr, "Registering function '%s'\n", ASTText(declarator_id));
		}
		else
		{
			fprintf(stderr, "Registering overload for function '%s'\n", ASTText(declarator_id));
		}
		symtab_entry_t* new_entry = new_symbol(st, ASTText(declarator_id));
		new_entry->kind = SK_FUNCTION;
		new_entry->type_information = declarator_type;

		return new_entry;
	}
	else
	{
		return entry;
	}
}

static symtab_entry_t* find_function_declaration(symtab_t* st, AST declarator_id, type_t* declarator_type, char* is_overload)
{
	symtab_entry_list_t* entry_list = query_id_expression(st, declarator_id);

	function_info_t* function_being_declared = declarator_type->function;
	symtab_entry_t* equal_entry = NULL;
	char found_equal = 0;
	*is_overload = 0;

	while (entry_list != NULL && !found_equal)
	{
		symtab_entry_t* entry = entry_list->entry;

		if (entry->kind != SK_FUNCTION)
		{
			// running_error("Symbol '%s' already declared as a different symbol type", ASTText(declarator_id), entry->kind);
			entry_list = entry_list->next;
			continue;
		}

		function_info_t* current_function = entry->type_information->function;

		found_equal = !overloaded_function(function_being_declared, current_function, st);
		if (found_equal)
		{
			equal_entry = entry;
		}

		entry_list = entry_list->next;
	}

	// This is bad programming
	if (!found_equal)
	{
		*is_overload = 1;
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
static void build_symtab_linkage_specifier(AST a, symtab_t* st)
{
	AST declaration_sequence = ASTSon1(a);

	if (declaration_sequence == NULL)
		return;

	char* previous_linkage = current_linkage;

	AST linkage_spec = ASTSon0(a);
	current_linkage = ASTText(linkage_spec);

	build_symtab_declaration_sequence(declaration_sequence, st);

	current_linkage = previous_linkage;
}

/*
 * Similar to build_symtab_linkage_specifier but for just one declaration
 */
static void build_symtab_linkage_specifier_declaration(AST a, symtab_t* st)
{
	AST declaration = ASTSon1(a);

	char* previous_linkage = current_linkage;

	AST linkage_spec = ASTSon0(a);
	current_linkage = ASTText(linkage_spec);

	build_symtab_declaration(declaration, st);

	current_linkage = previous_linkage;
}

/*
 * This function registers a template declaration
 */
static void build_symtab_template_declaration(AST a, symtab_t* st)
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
	symtab_t* template_scope = enter_scope(st);
	template_parameter_t** template_param_info = calloc(1, sizeof(*template_param_info));
	int num_parameters = 0;
	
	build_symtab_template_parameter_list(ASTSon0(a), template_scope, template_param_info, &num_parameters);

	switch (ASTType(ASTSon1(a)))
	{
		case AST_FUNCTION_DEFINITION :
			break;
		case AST_SIMPLE_DECLARATION :
			build_symtab_template_simple_declaration(ASTSon1(a), st, template_scope, num_parameters, template_param_info);
			break;
		default :
			internal_error("Unknown node type '%s'\n", ast_print_node_type(ASTType(a)));
	}
}

static void build_symtab_template_simple_declaration(AST a, symtab_t* st, symtab_t* template_scope, 
		int num_parameters, template_parameter_t** template_param_info)
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
	 *   T A<P>::d = expr;       // For static member initialization
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

	simple_type_t* simple_type_info = NULL;
	gather_decl_spec_t gather_info;
	memset(&gather_info, 0, sizeof(gather_info));

	if (decl_specifier_seq != NULL)
	{
		build_symtab_decl_specifier_seq(decl_specifier_seq, template_scope, &gather_info, &simple_type_info);
	}

	// Let's see what has got declared here
	if (simple_type_info->kind == STK_USER_DEFINED)
	{
		symtab_entry_t* entry = simple_type_info->user_defined_type;
		if (entry->kind == SK_CLASS)
		{
			// This is a primary template class if its template arguments are null
			if (entry->type_information->type->template_arguments == NULL)
			{
				entry->kind = SK_TEMPLATE_PRIMARY_CLASS;
			}
			else
			{
				// Otherwise this is a specialization (either partial or 'total')
				entry->kind = SK_TEMPLATE_SPECIALIZED_CLASS;
			}

			// Correct scoping
		}

		// Export this symbol in the outermost symbol table
		insert_entry(st, entry);

		// Fix scope. This is awkward. Due to the way symbol table is built,
		// there are two scopes involved here
		//
		//     <template_parameter_scope>
		//      + -- parameters
		//      + -- class name
		//      + -- <class scope>
		//            + -- members of the class
		//            
		// We have just pulled out the class name from the template parameter scope but we also want
		// parameters to be in class scope. To be correct we should remove them from the template parameter
		// scope but this is harmless. (I think)
		Iterator* it = (Iterator*) hash_iterator_create(template_scope->hash);
		for (iterator_first(it); !iterator_finished(it); iterator_next(it))
		{
			symtab_entry_list_t* entry_list = (symtab_entry_list_t*) iterator_item(it);
			
			// Technically here will not appear repeated things, so only export
			// the first one
			// Do not readd the class symbol!
			if (entry != entry_list->entry)
			{
				insert_entry(entry->inner_scope, entry_list->entry);
			}
		}
	}

	// There can be just one declarator here
	if (init_declarator_list != NULL)
	{
		if (ASTSon0(init_declarator_list) != NULL)
		{
			running_error("In template declarations only one declarator is valid", 0);
		}

		AST declarator = ASTSon1(init_declarator_list);

		type_t* declarator_type = NULL;
		symtab_entry_t* entry = build_symtab_declarator(declarator, template_scope, &gather_info, simple_type_info, &declarator_type);
		
		// TODO - Think about this
		// Now embed this entry in the outer symbol table
		insert_entry(st, entry);
	}
}

/*
 * This function register templates parameters in a given scope
 */
static void build_symtab_template_parameter_list(AST a, symtab_t* st, 
		template_parameter_t** template_param_info, int* num_parameters)
{
	AST iter;
	AST list = a;

	for_each_element(list, iter)
	{
		AST template_parameter = ASTSon1(iter);

		template_parameter_t* new_template_param = calloc(1, sizeof(*new_template_param));

		build_symtab_template_parameter(template_parameter, st, new_template_param);

		P_LIST_ADD(template_param_info, *num_parameters, new_template_param);
	}
}

/*
 * This function register one template parameter in a given scope
 */
static void build_symtab_template_parameter(AST a, symtab_t* st, 
		template_parameter_t* template_param_info)
{
	switch (ASTType(a))
	{
		case AST_PARAMETER_DECL :
			build_symtab_nontype_template_parameter(a, st, template_param_info);
			break;
		case AST_TYPE_PARAMETER_CLASS :
		case AST_TYPE_PARAMETER_TYPENAME :
			build_symtab_type_template_parameter(a, st, template_param_info);
			break;
		case AST_TYPE_PARAMETER_TEMPLATE :
			// Think about it
			internal_error("Node template template-parameters still not supported", 0);
			break;
		case AST_AMBIGUITY :
			// The ambiguity here is parameter_class vs parameter_decl
			solve_parameter_declaration_vs_type_parameter_class(a);
			// Restart this routine
			build_symtab_template_parameter(a, st, template_param_info);
			break;
		default :
			internal_error("Unknown node type '%s'", ast_print_node_type(ASTType(a)));
	}
}

static void build_symtab_type_template_parameter(AST a, symtab_t* st,
		template_parameter_t* template_param_info)
{
	// This parameters have the form
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
	new_type->type->kind = STK_TEMPLATE_CLASS;

	// Save the info
	template_param_info->type_info = new_type;

	AST name = ASTSon0(a);
	AST type_id = ASTSon1(a);
	
	if (name != NULL)
	{
		// This is a named type parameter. Register it in the symbol table
		fprintf(stderr, "Registering type template-parameter '%s'\n", ASTText(name));
		symtab_entry_t* new_entry = new_symbol(st, ASTText(name));
		new_entry->type_information = new_type;
		new_entry->kind = SK_TEMPLATE_PARAMETER;
	}
	
	template_param_info->default_argument = type_id;
}

static void build_symtab_nontype_template_parameter(AST a, symtab_t* st,
		template_parameter_t* template_param_info)
{
	// As usual there are three parts
	//     decl_specifier_seq [declarator] [ = expression ]
	simple_type_t* simple_type_info;
	gather_decl_spec_t gather_info;
	memset(&gather_info, 0, sizeof(gather_info));

	AST decl_specifier_seq = ASTSon0(a);
	AST parameter_declarator = ASTSon1(a);

	build_symtab_decl_specifier_seq(decl_specifier_seq, st, &gather_info, &simple_type_info);

	if (parameter_declarator != NULL)
	{
		// This will add into the symbol table if it has a name
		symtab_entry_t* entry = build_symtab_declarator(parameter_declarator, st, 
				&gather_info, simple_type_info, &template_param_info->type_info);

		if (entry != NULL)
		{
			fprintf(stderr, "Remembering '%s' as a non-type template parameter\n", entry->symbol_name);
			// This is not a variable, but a template parameter
			entry->kind = SK_TEMPLATE_PARAMETER;
		}
	}
	// If we don't have a declarator just save the base type
	else
	{
		template_param_info->type_info = simple_type_to_type(simple_type_info);
	}
}

/*
 * This function builds symbol table information for a namespace definition
 */
static void build_symtab_namespace_definition(AST a, symtab_t* st)
{
	AST namespace_name = ASTSon0(a);

	// Register this namespace if it does not exist
	symtab_entry_list_t* list = query_in_current_scope(st, ASTText(namespace_name));

	if (list != NULL 
			&& (list->next != NULL 
				|| list->entry->kind != SK_NAMESPACE))
	{
		running_error("Identifier '%s' has already been declared as another symbol kind\n", ASTText(namespace_name));
	}
	
	symtab_entry_t* entry;
	if (list != NULL && list->entry->kind == SK_NAMESPACE)
	{
		entry = list->entry;
	}
	else
	{
		// We register a symbol of type namespace and link to a newly created scope.
		symtab_t* namespace_scope = enter_scope(st);

		entry = new_symbol(st, ASTText(namespace_name));
		entry->kind = SK_NAMESPACE;
		entry->inner_scope = namespace_scope;
	}

	build_symtab_declaration_sequence(ASTSon1(a), entry->inner_scope);
}

/*
 * This function builds symbol table information for a function definition
 */
static symtab_entry_t* build_symtab_function_definition(AST a, symtab_t* st)
{
	fprintf(stderr, "Registering function!\n");
	// A function definition has four parts
	//   decl_specifier_seq declarator ctor_initializer function_body

	// decl_specifier_seq [optional]
	// If there is no decl_specifier_seq this has to be a destructor, constructor or conversion function
	gather_decl_spec_t gather_info;
	memset(&gather_info, 0, sizeof(gather_info));
	simple_type_t* type_info = NULL;

	if (ASTSon0(a) != NULL)
	{
		AST decl_spec_seq = ASTSon0(a);

		build_symtab_decl_specifier_seq(decl_spec_seq, st, &gather_info, &type_info);
	}

	// declarator
	// Declarator can be a qualified one
	// ¿¿¿TODO???
	type_t* declarator_type;
	
	symtab_entry_t* entry = NULL;
	// if (type_info != NULL)
	// {
		entry = build_symtab_declarator(ASTSon1(a), st, &gather_info, type_info, &declarator_type);
		if (entry == NULL)
		{
			internal_error("This function does not exist!", 0);
		}
	// }

	// Nothing will be done with ctor_initializer at the moment
	// Function_body
	AST function_body = ASTSon3(a);
	AST statement = ASTSon0(function_body);

	symtab_t* inner_scope = enter_scope(st);

	entry->inner_scope = inner_scope;

	build_symtab_statement(statement, inner_scope);

	if (entry == NULL)
	{
		running_error("This symbol is undeclared here", 0);
	}
	else 
	{
		fprintf(stderr, "Function '%s' is defined\n", entry->symbol_name);
		entry->defined = 1;
	}

	return entry;
}

static void build_symtab_statement(AST a, symtab_t* st)
{
#warning TODO symtab statement
}

static void build_symtab_member_declaration(AST a, symtab_t*  st, 
		access_specifier_t current_access, simple_type_t* class_info)
{
	switch (ASTType(a))
	{
		case AST_MEMBER_DECLARATION :
			{
				build_symtab_simple_member_declaration(a, st, current_access, class_info);
				break;
			}
		case AST_FUNCTION_DEFINITION :
			{
				build_symtab_member_function_definition(a, st, current_access, class_info);
				break;
			}
		default:
			{
				internal_error("Unsupported node '%s'\n", ast_print_node_type(ASTType(a)));
				break;
			}
	}
}

static void build_symtab_member_function_definition(AST a, symtab_t*  st, 
		access_specifier_t current_access, simple_type_t* class_info)
{
	char* class_name = "";
	class_info_t* class_type = NULL;
	if (class_info->kind == STK_USER_DEFINED)
	{
		class_name = class_info->user_defined_type->symbol_name;
		class_type = class_info->user_defined_type->type_information->type->class_info;
	}

	AST declarator = ASTSon1(a);

	AST declarator_name = get_declarator_name(declarator);

	// Get the declarator name
	switch (ASTType(declarator_name))
	{
		case AST_SYMBOL :
			{
				symtab_entry_t* entry = build_symtab_function_definition(a, st);
				if (strcmp(ASTText(declarator_name), class_name) == 0)
				{
					// This is a constructor
					P_LIST_ADD(class_type->constructor_list, class_type->num_constructors, entry);
				}
				break;
			}
		case AST_DESTRUCTOR_ID :
			{
				symtab_entry_t* entry = build_symtab_function_definition(a, st);
				// This is the destructor
				class_type->destructor = entry;
				break;
			}
		case AST_OPERATOR_FUNCTION_ID :
			{
				break;
			}
		case AST_CONVERSION_FUNCTION_ID :
			{
				break;
			}
		default :
			{
				internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(declarator_name)));
				break;
			}
	}
}

static void build_symtab_simple_member_declaration(AST a, symtab_t*  st, 
		access_specifier_t current_access, simple_type_t* class_info)
{
	gather_decl_spec_t gather_info;
	simple_type_t* simple_type_info = NULL;

	memset(&gather_info, 0, sizeof(gather_info));

	if (ASTSon0(a) != NULL)
	{
		build_symtab_decl_specifier_seq(ASTSon0(a), st, &gather_info, &simple_type_info);
	}

	if (ASTSon1(a) != NULL)
	{
		AST list = ASTSon1(a);
		AST iter;

		for_each_element(list, iter)
		{
			AST declarator = ASTSon1(iter);

			switch (ASTType(declarator))
			{
				case AST_MEMBER_DECLARATOR :
					{
						type_t* declarator_type = NULL;
						build_symtab_declarator(ASTSon0(declarator), st, &gather_info, simple_type_info, &declarator_type);
						break;
					}
				default :
					{
						internal_error("Unhandled node '%s'", ast_print_node_type(ASTType(declarator)));
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

	if (ASTType(a) != AST_NODE_LIST)
	{
		internal_error("This function expects a list", 0);
	}

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
static exception_spec_t* build_exception_spec(symtab_t* st, AST a)
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
		simple_type_t* type_info;
		gather_decl_spec_t gather_info;
		memset(&gather_info, 0, sizeof(gather_info));
	
		build_symtab_decl_specifier_seq(type_specifier_seq, st, &gather_info, &type_info);

		if (abstract_decl != NULL)
		{
			type_t* declarator_type;
			build_symtab_declarator(abstract_decl, st, &gather_info, type_info, &declarator_type);
			P_LIST_ADD(result->exception_type_seq, result->num_exception_types,
					declarator_type);
		}
		else
		{
			type_t* declarator_type = simple_type_to_type(type_info);
			P_LIST_ADD(result->exception_type_seq, result->num_exception_types,
					declarator_type);
		}
	}

	return result;
}

