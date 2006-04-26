#include <stdio.h>
#include "cxx-printsymtab.h"
#include "hash_iterator.h"
#include "cxx-typeutils.h"

/*
 * Building a symbol table for C++ is such a hard thing that we need ways to debug it.
 */
static void print_symtab_entry_list(symtab_entry_list_t* entry_list, symtab_t* st);
static void print_symtab_entry(symtab_entry_t* entry, symtab_t* st);

static int global_indent = 0;

void print_scope(symtab_t* st)
{
	if (st == NULL)
		return;

	Iterator *it;
	
	it = (Iterator*) hash_iterator_create(st->hash);
	for ( iterator_first(it); 
			!iterator_finished(it); 
			iterator_next(it))
	{
		symtab_entry_list_t* entry_list = (symtab_entry_list_t*) iterator_item(it);

		print_symtab_entry_list(entry_list, st);
	}
}

static void print_symtab_entry_list(symtab_entry_list_t* entry_list, symtab_t* st)
{
	while (entry_list != NULL)
	{
		print_symtab_entry(entry_list->entry, st);
		entry_list = entry_list->next;
	}
}

static char* symbol_kind_names[] =
{
	[SK_UNDEFINED] = "SK_UNDEFINED",
	[SK_CLASS] = "SK_CLASS",
	[SK_ENUM] = "SK_ENUM",
	[SK_ENUMERATOR] = "SK_ENUMERATOR",
	[SK_FUNCTION] = "SK_FUNCTION",
	[SK_LABEL] = "SK_LABEL",
	[SK_NAMESPACE] = "SK_NAMESPACE",
	[SK_VARIABLE] = "SK_VARIABLE",
	[SK_TYPEDEF] = "SK_TYPEDEF",
	[SK_TEMPLATE_PRIMARY_CLASS] = "SK_TEMPLATE_PRIMARY_CLASS",
	[SK_TEMPLATE_SPECIALIZED_CLASS] = "SK_TEMPLATE_SPECIALIZED_CLASS",
	[SK_TEMPLATE_FUNCTION] = "SK_TEMPLATE_FUNCTION",
	[SK_TEMPLATE_PARAMETER] = "SK_TEMPLATE_PARAMETER", 
};

#define PRINT_INDENTED_LINE(f, fmt, ...) \
	do { \
		int i; \
		for (i = 0; i < 4*global_indent; i++) \
		{ \
			fprintf(f, " "); \
		} \
		fprintf(f, fmt, __VA_ARGS__ ); \
	} while (0);

static void print_symtab_entry(symtab_entry_t* entry, symtab_t* st)
{
	int indent_level = global_indent;

	PRINT_INDENTED_LINE(stderr, "\"%s\" %s", entry->symbol_name, symbol_kind_names[entry->kind]);

	if (entry->defined)
	{
		fprintf(stderr, " [DEFINED]");
	}

	fprintf(stderr, "\n");

	if (entry->kind == SK_VARIABLE)
	{
		PRINT_INDENTED_LINE(stderr, "%s", "\tType: ");
		print_declarator(entry->type_information, st);
		fprintf(stderr, "\n");
	}
	if (entry->kind == SK_TYPEDEF)
	{
		PRINT_INDENTED_LINE(stderr, "%s", "\tAliased type: ");
		print_declarator(entry->type_information->type->aliased_type, st);
		fprintf(stderr, "\n");
	}
	if (entry->kind == SK_NAMESPACE
			|| entry->kind == SK_CLASS
			|| entry->kind == SK_TEMPLATE_PRIMARY_CLASS
			|| entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS)
	{
		global_indent++;
		print_scope(entry->inner_scope);
	}
	if (entry->kind == SK_FUNCTION
			|| entry->kind == SK_TEMPLATE_FUNCTION)
	{
		PRINT_INDENTED_LINE(stderr, "%s", "\tPrototype: ");
		print_declarator(entry->type_information, st);
		fprintf(stderr, "\n");
		global_indent++;
		print_scope(entry->inner_scope);
	}

	global_indent = indent_level;
}
