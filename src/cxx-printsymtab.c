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
	[SK_TEMPLATE_CLASS] = "SK_TEMPLATE_CLASS",
	[SK_TEMPLATE_FUNCTION] = "SK_TEMPLATE_FUNCTION",
	[SK_TEMPLATE_PARAMETER] = "SK_TEMPLATE_PARAMETER", 
};

static void print_symtab_entry(symtab_entry_t* entry, symtab_t* st)
{
	int indent_level = global_indent;
	{
		int i;
		for (i = 0; i < 4*indent_level; i++)
		{
			fprintf(stderr, " ");
		}
	}

	fprintf(stderr, "\"%s\" %s\n", entry->symbol_name, symbol_kind_names[entry->kind]);

	if (entry->kind == SK_VARIABLE)
	{
		fprintf(stderr, "\tType: ");
		print_declarator(entry->type_information, st);
		fprintf(stderr, "\n");
	}
	if (entry->kind == SK_TYPEDEF)
	{
		fprintf(stderr, "\tAliased type: ");
		print_declarator(entry->type_information->type->aliased_type, st);
		fprintf(stderr, "\n");
	}
	if (entry->kind == SK_NAMESPACE)
	{
		global_indent++;
		print_scope(entry->inner_scope);
	}
	if (entry->kind == SK_CLASS)
	{
		global_indent++;
		print_scope(entry->inner_scope);
	}
	if (entry->kind == SK_FUNCTION)
	{
		global_indent++;
		print_scope(entry->inner_scope);
	}

	global_indent = indent_level;
}
