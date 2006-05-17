#include <stdio.h>
#include "cxx-printscope.h"
#include "hash_iterator.h"
#include "cxx-typeutils.h"

/*
 * Building a symbol table for C++ is such a hard thing that we need ways to debug it.
 */
static void print_scope_entry_list(scope_entry_list_t* entry_list, scope_t* st, int global_indent);
static void print_scope_entry(scope_entry_t* entry, scope_t* st, int global_indent);

void print_scope(scope_t* st, int global_indent)
{
	if (st == NULL)
		return;

	Iterator *it;
	
	it = (Iterator*) hash_iterator_create(st->hash);
	for ( iterator_first(it); 
			!iterator_finished(it); 
			iterator_next(it))
	{
		scope_entry_list_t* entry_list = (scope_entry_list_t*) iterator_item(it);

		print_scope_entry_list(entry_list, st, global_indent);
	}
}

static void print_scope_entry_list(scope_entry_list_t* entry_list, scope_t* st, int global_indent)
{
	while (entry_list != NULL)
	{
		print_scope_entry(entry_list->entry, st, global_indent);
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

static void indent_at_level(FILE* f, int n)
{
	int i;
	for (i = 0; i < 4*n; i++) 
	{ 
		fprintf(f, " "); 
	} 
}

#define PRINT_INDENTED_LINE(f, n, fmt, ...) \
	do { \
		indent_at_level(f, n); \
		fprintf(f, fmt, __VA_ARGS__ ); \
	} while (0);

static void print_scope_entry(scope_entry_t* entry, scope_t* st, int global_indent)
{
	PRINT_INDENTED_LINE(stderr, global_indent, "[%p] \"%s\" %s",st, entry->symbol_name, symbol_kind_names[entry->kind]);

	if (entry->defined)
	{
		fprintf(stderr, " [DEFINED]");
	}

	fprintf(stderr, "\n");

	if (entry->kind == SK_VARIABLE
			|| entry->kind == SK_TEMPLATE_PARAMETER)
	{
		PRINT_INDENTED_LINE(stderr, global_indent+1, "%s", "Type: ");
		print_declarator(entry->type_information, st);
		fprintf(stderr, "\n");
	}
	if (entry->kind == SK_TYPEDEF)
	{
		PRINT_INDENTED_LINE(stderr, global_indent+1, "%s", "Aliased type: ");
		print_declarator(entry->type_information->type->aliased_type, st);
		fprintf(stderr, "\n");
	}

	if (entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS
			|| entry->kind == SK_TEMPLATE_PRIMARY_CLASS)
	{
		PRINT_INDENTED_LINE(stderr, global_indent+1, "Template: %p\n", entry);
	}

	if (entry->kind == SK_NAMESPACE
			|| entry->kind == SK_CLASS
			|| entry->kind == SK_TEMPLATE_PRIMARY_CLASS
			|| entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS)
	{
		print_scope(entry->inner_scope, global_indent+1);
	}

	if (entry->kind == SK_FUNCTION
			|| entry->kind == SK_TEMPLATE_FUNCTION)
	{
		PRINT_INDENTED_LINE(stderr, global_indent+1, "%s", "Prototype: ");
		print_declarator(entry->type_information, st);
		fprintf(stderr, "\n");
		print_scope(entry->inner_scope, global_indent+1);
	}
}
