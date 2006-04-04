#include <string.h>
#include <stdio.h>
#include "cxx-buildsymtab.h"
#include "cxx-symtab.h"
#include "cxx-utils.h"

static void build_symtab_declaration(AST a, symtab_t* st);
static void build_symtab_simple_declaration(AST a, symtab_t* st);
static void build_symtab_decl_specifier_seq(AST a, symtab_t* st);

void build_symtab_translation_unit(AST a)
{
	AST head = a;

	while (ASTSon0(head) != NULL) 
		head = ASTSon0(head);

	symtab_t* st = new_symtab();

	while (head != a)
	{
		build_symtab_declaration(ASTSon1(head), st);
		
		head = ASTParent(head);
	}

	build_symtab_declaration(ASTSon1(a), st);
}

static void build_symtab_declaration(AST a, symtab_t* st)
{
	if (ASTType(a) != AST_SIMPLE_DECLARATION)
	{
		internal_error("Only simple declarations supported at the moment", 0);
	}

	build_symtab_simple_declaration(a, st);
}

static void build_symtab_simple_declaration(AST a, symtab_t* st)
{
	if (ASTType(a) == AST_EMPTY_DECL)
		return;

	if (ASTSon0(a) != NULL)
	{
		build_symtab_decl_specifier_seq(ASTSon0(a), st);
	}
}

static void gather_decl_spec_information(AST a, gather_decl_spec_t* gather_info);
static void gather_type_spec_information(AST a, symtab_t* st, type_info_t* type_info);

static void build_symtab_decl_specifier_seq(AST a, symtab_t* st)
{
	AST iter, list;

	// First gather all the information of non type related decl_specifier_seq 
	gather_decl_spec_t gather_info;

	// Clear stack debris
	memset(&gather_info, 0, sizeof(gather_info));

	// Gather decl specifier sequence information
	list = ASTSon0(a);
	if (list != NULL)
	{
		for_each_element(list, iter)
		{
			AST spec = ASTSon1(iter);
			gather_decl_spec_information(spec, &gather_info);
		}
	}

	list = ASTSon2(a);
	if (list != NULL)
	{
		for_each_element(list, iter)
		{
			AST spec = ASTSon1(iter);
			gather_decl_spec_information(spec, &gather_info);
		}
	}

	// Now gather information of the type_spec
	if (ASTSon1(a) != NULL)
	{
		type_info_t* type_info = calloc(sizeof(*type_info), 1);
		gather_type_spec_information(ASTSon1(a), st, type_info);
	}
}

static void gather_decl_spec_information(AST a, gather_decl_spec_t* gather_info)
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

void gather_type_spec_from_simple_type_specifier(AST a, symtab_t* st, type_info_t* type_info);
void gather_type_spec_from_enum_specifier(AST a, symtab_t* st, type_info_t* type_info);

static void gather_type_spec_information(AST a, symtab_t* st, type_info_t* type_info)
{
	switch (ASTType(a))
	{
		case AST_SIMPLE_TYPE_SPECIFIER:
			gather_type_spec_from_simple_type_specifier(a, st, type_info);
			break;
		case AST_ENUM_SPECIFIER :
			gather_type_spec_from_enum_specifier(a, st, type_info);
			break;
		case AST_CHAR_TYPE :
			fprintf(stderr, "Tipus char\n");
			type_info->builtin_type= BT_CHAR;
			break;
		case AST_WCHAR_TYPE :
			fprintf(stderr, "Tipus wchar_t\n");
			type_info->builtin_type= BT_WCHAR;
			break;
		case AST_BOOL_TYPE :
			fprintf(stderr, "Tipus bool\n");
			type_info->builtin_type= BT_BOOL;
			break;
		case AST_SHORT_TYPE :
			fprintf(stderr, "Tipus short\n");
			type_info->builtin_type= BT_INT;
			type_info->is_short = 1;
			break;
		case AST_INT_TYPE :
			fprintf(stderr, "Tipus int\n");
			type_info->builtin_type= BT_INT;
			break;
		case AST_LONG_TYPE :
			fprintf(stderr, "Tipus long\n");
			type_info->builtin_type= BT_INT;
			type_info->is_long = 1;
			break;
		case AST_SIGNED_TYPE :
			fprintf(stderr, "Tipus signed\n");
			type_info->builtin_type = BT_INT;
			type_info->is_signed = 1;
			break;
		case AST_UNSIGNED_TYPE :
			fprintf(stderr, "Tipus unsigned\n");
			type_info->builtin_type = BT_INT;
			type_info->is_unsigned = 1;
			break;
		case AST_FLOAT_TYPE :
			fprintf(stderr, "Tipus float\n");
			type_info->builtin_type = BT_FLOAT;
			break;
		case AST_DOUBLE_TYPE :
			fprintf(stderr, "Tipus double\n");
			type_info->builtin_type = BT_DOUBLE;
			break;
		case AST_VOID_TYPE :
			fprintf(stderr, "Tipus void\n");
			type_info->builtin_type = BT_VOID;
			break;
		default:
			internal_error("Unknown node '%s'", ast_print_node_type(ASTType(a)));
	}
}

void gather_type_spec_from_simple_type_specifier(AST a, symtab_t* st, type_info_t* type_info)
{
	// TODO - We shall check nested namespaces and global qualifier, ignore it for now
	AST type_name = ASTSon2(a);

	switch (ASTType(type_name))
	{
		case AST_SYMBOL :
			fprintf(stderr, "Tipus d'usuari '%s'\n", ASTText(type_name));
			break;
		case AST_TEMPLATE_ID :
			fprintf(stderr, "Template id '%s'\n", ASTText(ASTSon0(type_name)));
			break;
		default:
			internal_error("Unknown node '%s'", ast_print_node_type(ASTType(a)));
	};
}

void gather_type_spec_from_enum_specifier(AST a, symtab_t* st, type_info_t* type_info)
{
	type_info->enum_info = (enum_info_t*) calloc(sizeof(*type_info->enum_info), 1);

	AST list, iter;
	list = ASTSon1(a);

	for_each_element(list, iter)
	{
		AST enumeration = ASTSon1(iter);
		AST enumeration_name = ASTSon0(enumeration);
		AST enumeration_expr = ASTSon1(enumeration);
		
		type_info->enum_info->num_enumeration++;
		type_info->enum_info->enumeration_list = 
			realloc(type_info->enum_info->enumeration_list, 
					(type_info->enum_info->num_enumeration) * sizeof(*(type_info->enum_info->enumeration_list)));

		type_info->enum_info->enumeration_list[type_info->enum_info->num_enumeration - 1].name = 
			strdup(ASTText(enumeration_name));
		type_info->enum_info->enumeration_list[type_info->enum_info->num_enumeration - 1].value = enumeration_expr;
	}

	AST enum_name = ASTSon0(a);
	if (enum_name != NULL)
	{
		// TODO - Register this enum type into the symbol table
	}
}
