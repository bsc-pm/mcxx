#include <string.h>
#include <stdio.h>
#include "cxx-driver.h"
#include "cxx-buildsymtab.h"
#include "cxx-symtab.h"
#include "cxx-prettyprint.h"
#include "cxx-utils.h"

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
static void build_symtab_declarator(AST a, symtab_t* st, gather_decl_spec_t* gather_info, 
		simple_type_t* type_info, type_t** declarator_type);

static void build_symtab_namespace_definition(AST a, symtab_t* st);
static void build_symtab_function_definition(AST a, symtab_t* st);

static void build_symtab_member_declaration(AST a, symtab_t*  st, 
		access_specifier_t current_access, simple_type_t* simple_type_info);
static void build_symtab_simple_member_declaration(AST a, symtab_t*  st, 
		access_specifier_t current_access, simple_type_t* simple_type_info);

static void build_symtab_statement(AST statement, symtab_t* st);

static void gather_type_spec_from_simple_type_specifier(AST a, symtab_t* st, simple_type_t* type_info);
static void gather_type_spec_from_enum_specifier(AST a, symtab_t* st, simple_type_t* type_info);
static void gather_type_spec_from_class_specifier(AST a, symtab_t* st, simple_type_t* type_info);

static void build_symtab_declarator_rec(AST a, symtab_t* st, type_t** declarator_type, AST* declarator_name);

static void gather_decl_spec_information(AST a, symtab_t* st, gather_decl_spec_t* gather_info);
static void gather_type_spec_information(AST a, symtab_t* st, simple_type_t* type_info);

static void build_symtab_declarator_name(AST declarator_name, type_t* declarator_type, 
		gather_decl_spec_t* gather_info, symtab_t* st);
static void build_symtab_declarator_id_expr(AST declarator_name, type_t* declarator_type, 
		gather_decl_spec_t* gather_info, symtab_t* st);

static type_t* simple_type_to_type(simple_type_t* simple_type_info);

static void register_new_typedef_name(AST declarator_id, type_t* declarator_type, 
		gather_decl_spec_t* gather_info, symtab_t* st);
static void register_new_variable_name(AST declarator_id, type_t* declarator_type, 
		gather_decl_spec_t* gather_info, symtab_t* st);

// Debug purposes
static void print_declarator(type_t* printed_declarator, symtab_t* st);


// Builds symtab for the translation unit
void build_symtab_translation_unit(AST a)
{
	AST list = ASTSon0(a);

	if (list == NULL)
		return;

	// The global scope is created here
	compilation_options.global_scope = new_symtab();

	build_symtab_declaration_sequence(list, compilation_options.global_scope);
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
				// Simple declarations are of the form
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

			type_t* declarator_type;
			build_symtab_declarator(declarator, st, &gather_info, 
					simple_type_info, &declarator_type);
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
 *
 * Recall our grammar defines a decl_specifier_seq as 
 *    decl_specifier_seq -> nontype_decl_specifier_seq[opt] type_spec nontype_decl_specifier_seq[opt]
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
				running_error("Identifier '%s' in line %d is not a type %p\n", ASTText(type_name), 
						ASTLine(type_name), simple_type_entry);
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

			enumeration_item_t* enumeration_item = calloc(1, sizeof(*enumeration_item));

			enumeration_item->name = strdup(ASTText(enumeration_name));
			enumeration_item->value = enumeration_expr;

			P_LIST_ADD(simple_type_info->enum_info->enumeration_list, 
					simple_type_info->enum_info->num_enumeration,
					enumeration_item);
		}

	}

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
		new_entry->type_information = simple_type_to_type(simple_type_info);

		// Since this type is not anonymous we'll want that simple_type_info
		// refers to this newly created type
		memset(simple_type_info, 0, sizeof(*simple_type_info));
		simple_type_info->kind = STK_USER_DEFINED;
		simple_type_info->user_defined_type = new_entry;
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
	
	if (class_head_identifier != NULL)
	{
		// If the class has name, register it in the symbol table
		char* name;
		if (ASTType(class_head_identifier) == AST_SYMBOL)
		{
			fprintf(stderr, "Registering class '%s' in %p\n", ASTText(class_head_identifier), st);
			name = ASTText(class_head_identifier);

			symtab_entry_t* entry = new_symbol(st, name);

			entry->kind = SK_CLASS;

			// entry->type_information = calloc(1, sizeof(*(entry->type_information)));
			// entry->type_information->type = simple_type_info;
			entry->type_information = simple_type_to_type(simple_type_info);

			// Since this type is not anonymous we'll want that simple_type_info
			// refers to this newly created type
			memset(simple_type_info, 0, sizeof(*simple_type_info));
			simple_type_info->kind = STK_USER_DEFINED;
			simple_type_info->user_defined_type = entry;
		}
		else if (ASTType(class_head_identifier) == AST_TEMPLATE_ID)
		{
			// This happens only in explicit specializations/instantiations
			name = ASTText(ASTSon0(class_head_identifier));
			fprintf(stderr, "Unsupported template id yet\n");
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

	symtab_t* inner_scope = enter_scope(st);

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
static void build_symtab_declarator(AST a, symtab_t* st, gather_decl_spec_t* gather_info, 
		simple_type_t* simple_type_info, type_t** declarator_type)
{
	fprintf(stderr, "Declarator\n");

	// Set base type
	*declarator_type = simple_type_to_type(simple_type_info);

	AST declarator_name = NULL;

	build_symtab_declarator_rec(a, st, declarator_type, &declarator_name);

	print_declarator(*declarator_type, st); fprintf(stderr, "\n");

	if (declarator_name != NULL)
	{
		build_symtab_declarator_name(declarator_name, *declarator_type, gather_info, st);
	}
}

/*
 * This functions converts a type "T" to a "pointer to T"
 */
static void set_pointer_type(type_t** declarator_type, symtab_t* st, AST pointer_tree)
{
	// TODO - Pointer to member
	type_t* pointee_type = *declarator_type;

	(*declarator_type) = calloc(1, sizeof(*(*declarator_type)));
	(*declarator_type)->kind = TK_POINTER;
	(*declarator_type)->pointer = calloc(1, sizeof(*((*declarator_type)->pointer)));
	(*declarator_type)->pointer->pointee = pointee_type;

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

	(*declarator_type)->kind = TK_FUNCTION;
	(*declarator_type)->function = calloc(1, sizeof(*((*declarator_type)->function)));
	(*declarator_type)->function->return_type = returning_type;

	set_function_parameter_clause(*declarator_type, st, parameter);
	
	// TODO, cv-qualifier & exception
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
		case AST_DECLARATOR_ID_TYPE_NAME :
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
				get_declarator_name(ASTSon0(a));
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

	return NULL;
}



/*
 * This function fills the symbol table with the information of this declarator
 */
static void build_symtab_declarator_name(AST declarator_name, type_t* declarator_type, 
		gather_decl_spec_t* gather_info, symtab_t* st)
{
	switch (ASTType(declarator_name))
	{
		case AST_DECLARATOR_ID_EXPR :
			build_symtab_declarator_id_expr(declarator_name, declarator_type, gather_info, st);
			break;
		case AST_DECLARATOR_ID_TYPE_NAME :
			break;
		default:
			internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(declarator_name)));
			break;
	}
}

/*
 * This function fills information for a declarator_id_expr. Actually only
 * unqualified names can be signed up since qualified names should have been
 * declared elsewhere.
 */
static void build_symtab_declarator_id_expr(AST declarator_name, type_t* declarator_type, 
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
					register_new_typedef_name(declarator_id, declarator_type, gather_info, st);
					fprintf(stderr, "Declared new typedef '%s'\n", ASTText(declarator_id));
				}
				else
				{
					register_new_variable_name(declarator_id, declarator_type, gather_info, st);
					fprintf(stderr, "Declared variable '%s'\n", ASTText(declarator_id));
				}
				break;
			}
		case AST_DESTRUCTOR_ID :
			{
				// An unqualified destructor name "~name"
				// 'name' should be a class in this scope
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
}

/*
 * This function registers a new typedef name.
 */
static void register_new_typedef_name(AST declarator_id, type_t* declarator_type, 
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

	// Save aliased type under the type of this declaration
	entry->kind = SK_TYPEDEF;
	entry->type_information = calloc(1, sizeof(*(entry->type_information)));
	entry->type_information->kind = TK_DIRECT;
	entry->type_information->type = calloc(1, sizeof(*(entry->type_information->type)));
	entry->type_information->type->aliased_type = declarator_type;

	// TODO - cv qualification
}

/*
 * This function registers a new "variable" (non type) name
 */
static void register_new_variable_name(AST declarator_id, type_t* declarator_type, 
		gather_decl_spec_t* gather_info, symtab_t* st)
{
	symtab_entry_list_t* list = query_in_current_scope(st, ASTText(declarator_id));
	symtab_entry_t* entry = NULL;

	// Only enum or classes can exist, otherwise this is an error
	if (list != NULL)
	{
		int num_types = 0, num_other = 0;
		// Look for the first variable
		while ((list != NULL) && 
				(entry == NULL))
		{
			if (list->entry->kind == SK_VARIABLE)
			{
				entry = list->entry;
			}
			else if (list->entry->kind == SK_ENUM
					|| list->entry->kind == SK_CLASS)
			{
				num_types++;
			}
			else
			{
				num_other++;
			}
			list = list->next;
		}
		
		// Basically if there is something other than one elaborated type for
		// this identifier, it is being incorrectly redeclared.
		//
		// Recall that:
		//
		//   struct A { };
		//   A A;
		//
		// Is well-formed.
		if (num_types > 1 
				|| num_other != 0)
		{
			internal_error("Symbol '%s' has been redeclared as a different symbol kind.", 
					ASTText(declarator_id));
		}

		/*
		 * If this symbol has been already defined and it is not flagged as
		 * extern nor is a function, then this symbol is being redefined
		 */
		if (entry->defined
				&& !gather_info->is_extern
				&& (entry->type_information->kind != TK_FUNCTION))
		{
			running_error("Symbol '%s' has already been defined.", ASTText(declarator_id));
		}
	}

	if (entry == NULL)
	{
		fprintf(stderr, "Registering variable '%s'\n", ASTText(declarator_id));
		entry = new_symbol(st, ASTText(declarator_id));
		entry->kind = SK_VARIABLE;
		// entry->type_information = copy_type(declarator_type);
		entry->type_information = declarator_type;

		// TODO - cv qualification
	}


	// If this symbol is not a function and it has not ben flagged as "extern"
	// it is defined here.
	if (!gather_info->is_extern && 
			entry->type_information->kind != TK_FUNCTION)
	{
		fprintf(stderr, "Symbol '%s' has been defined.\n", ASTText(declarator_id));
		entry->defined = 1;
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
static void build_symtab_function_definition(AST a, symtab_t* st)
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
	
	// If no type was given, this identifier has to exist elsewhere in a class
	// since it can only be a constructor, destructor or conversion function
	if (type_info != NULL)
	{
		build_symtab_declarator(ASTSon1(a), st, &gather_info, type_info, &declarator_type);
	}

	// Nothing will be done with ctor_initializer at the moment
	//
	// TODO - Handle the case when the function has already been defined. 
	// TODO - Handle overload !!!
	
	// Function_body
	AST function_body = ASTSon3(a);
	AST statement = ASTSon0(function_body);

	symtab_t* inner_scope = enter_scope(st);

	build_symtab_statement(statement, inner_scope);

	AST declarator_name = get_declarator_name(ASTSon1(a));
	symtab_entry_list_t* entry_list = query_id_expression(st, declarator_name);

	if (entry_list == NULL)
	{
		running_error("This symbol is undeclared here", 0);
	}
	else 
	{
		fprintf(stderr, "Function '%s' is defined\n", entry_list->entry->symbol_name);
		entry_list->entry->defined = 1;
	}
}

static void build_symtab_statement(AST a, symtab_t* st)
{
#warning TODO
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
				break;
			}
		default:
			{
				internal_error("Unsupported node '%s'\n", ast_print_node_type(ASTType(a)));
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

/* Copy functions */

// This function copies the type information of an enum
enum_info_t* copy_enum_info(enum_info_t* enum_info)
{
	enum_info_t* result = calloc(1, sizeof(*result));

	*result = *enum_info;

	int i;
	for (i = 0; i < result->num_enumeration; i++)
	{
		result->enumeration_list[i]->name = strdup(enum_info->enumeration_list[i]->name);
		result->enumeration_list[i]->value = duplicate_ast(enum_info->enumeration_list[i]->value);
	}

	return result;
}

// This function copies the type information of a pointer
pointer_info_t* copy_pointer_info(pointer_info_t* pointer_info)
{
	pointer_info_t* result = calloc(1, sizeof(*result));
	*result = *pointer_info;
	
	result->pointee = copy_type(result->pointee);

	return result;
}

// This function copies the type information of an array
array_info_t* copy_array_info(array_info_t* array_info)
{
	array_info_t* result = calloc(1, sizeof(*result));
	*result = *array_info;
	
	result->array_expr = duplicate_ast(array_info->array_expr);
	result->element_type = copy_type(array_info->element_type);
	
	return result;
}

// This function copies the type information of a function
function_info_t* copy_function_info(function_info_t* function_info)
{
	function_info_t* result = calloc(1, sizeof(*result));
	*result = *function_info;

	result->return_type = copy_type(function_info->return_type);
	
	int i;
	for (i = 0; i < function_info->num_parameters; i++)
	{
		result->parameter_list[i] = copy_type(function_info->parameter_list[i]);
	}
	
	return result;
}

// This function copies a full fledged type
type_t* copy_type(type_t* type)
{
	type_t* result = calloc(1, sizeof(*result));

	*result = *type;

	if (result->pointer != NULL)
	{
		result->pointer = copy_pointer_info(type->pointer);
	}

	if (result->array != NULL)
	{
		result->array = copy_array_info(type->array);
	}

	if (result->function != NULL)
	{
		result->function = copy_function_info(type->function);
	}

	if (result->type != NULL)
	{
		result->type = copy_simple_type(type->type);
	}

	return result;
}

// This function copies class type information
class_info_t* copy_class_info(class_info_t* class_info)
{
	class_info_t* result = calloc(1, sizeof(*result));

	*result = *class_info;

	int i;
	for (i = 0; i < result->num_members; i++)
	{
		result->member_list[i]->name = strdup(class_info->member_list[i]->name);
		result->member_list[i]->type_info = copy_type(result->member_list[i]->type_info);
	}
	
	return result;
}

// This function copies a simple type
simple_type_t* copy_simple_type(simple_type_t* type_info)
{
	simple_type_t* result = calloc(1, sizeof(*result));

	// Bitwise copy for every thing that can be directly copied
	*result = *type_info;

	if (result->enum_info != NULL)
	{
		result->enum_info = copy_enum_info(type_info->enum_info);
	}

	if (result->class_info != NULL)
	{
		result->class_info = copy_class_info(type_info->class_info);
	}

	return result;
}

// Gives the name of a builtin type
static const char* get_builtin_type_name(simple_type_t* simple_type_info, symtab_t* st)
{
	static char result[256] = {0};
	switch (simple_type_info->kind)
	{
		case STK_BUILTIN_TYPE :
			{
				switch (simple_type_info->builtin_type)
				{
					case BT_INT :
						return "int";
						break;
					case BT_BOOL :
						return "bool";
						break;
					case BT_FLOAT :
						return "float";
						break;
					case BT_DOUBLE :
						return "double";
						break;
					case BT_WCHAR :
						return "wchar_t";
						break;
					case BT_CHAR :
						return "char";
						break;
					case BT_VOID :
						return "void";
						break;
					case BT_UNKNOWN :
					default :
						return "¿¿¿unknown builtin type???";
						break;
				}
				break;
			}
		case STK_USER_DEFINED :
			{
				symtab_entry_t* user_defined_type = simple_type_info->user_defined_type;
				switch (user_defined_type->kind)
				{
					case SK_ENUM :
						snprintf(result, 255, "enum %s", user_defined_type->symbol_name);
						break;
					case SK_CLASS :
						snprintf(result, 255, "class %s", user_defined_type->symbol_name);
						break;
					case SK_TYPEDEF :
						snprintf(result, 255, "typedef %s", user_defined_type->symbol_name);
						break;
					default :
						return "¿¿¿unknown user defined type???";
				}

				return result;
			}
		case STK_ENUM :
			return "enum <anonymous>";
		case STK_CLASS :
			return "class <anonymous>";
		default :
			{
			}
	}
	return "¿¿¿unknown type???";
}

/*
 * This function just creates a full type_t from a simple_type_t.
 * It is useful when no declarator information is available.
 */
static type_t* simple_type_to_type(simple_type_t* simple_type_info)
{
	type_t* result = calloc(1, sizeof(*result));
	result->kind = TK_DIRECT;
	// result->type = copy_simple_type(simple_type_info);
	result->type = simple_type_info;

	return result;
}

// This prints a declarator in English. It is intended for debugging purposes
static void print_declarator(type_t* printed_declarator, symtab_t* st)
{
	do 
	{
		switch (printed_declarator->kind)
		{
			case TK_DIRECT :
				fprintf(stderr, "%s", get_builtin_type_name(printed_declarator->type, st));
				printed_declarator = NULL;
				break;
			case TK_POINTER :
				fprintf(stderr, "pointer to ");
				printed_declarator = printed_declarator->pointer->pointee;
				break;
			case TK_ARRAY :
				fprintf(stderr, "array ");
				prettyprint(stderr, printed_declarator->array->array_expr);
				fprintf(stderr, " of ");
				printed_declarator = printed_declarator->array->element_type;
				break;
			case TK_FUNCTION :
				{
					int i;
					fprintf(stderr, "function (");
					for (i = 0; i < printed_declarator->function->num_parameters; i++)
					{
						print_declarator(printed_declarator->function->parameter_list[i], st);
						if ((i+1) < printed_declarator->function->num_parameters)
						{
							fprintf(stderr, ", ");
						}
					}
					fprintf(stderr, ") returning ");
					printed_declarator = printed_declarator->function->return_type;
					break;
				}
			default :
				internal_error("Unhandled type kind '%d'\n", printed_declarator->kind);
				break;
		}
	} while (printed_declarator != NULL);
}
