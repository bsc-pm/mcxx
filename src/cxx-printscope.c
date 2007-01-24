#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cxx-utils.h"
#include "cxx-driver.h"
#include "cxx-prettyprint.h"
#include "cxx-printscope.h"
#include "hash_iterator.h"
#include "cxx-typeutils.h"

/*
 * Building a symbol table for C++ is such a hard thing that we need ways to debug it.
 */
static void print_scope_brief(scope_t* st, char* qualif_name);
static void print_scope_full(scope_t* st, int global_indent);
static void print_scope_entry_list_brief(scope_entry_list_t* entry_list, scope_t* st, char* qualif_name);
static void print_scope_entry_list(scope_entry_list_t* entry_list, scope_t* st, int global_indent);
static void print_scope_entry(scope_entry_t* entry, scope_t* st, int global_indent);
static void print_scope_entry_brief(scope_entry_t* entry, scope_t* st, char* qualif_name);

static void indent_at_level(FILE* f, int n)
{
    int i;
    for (i = 0; i < 4*n; i++) 
    { 
        fprintf(f, " "); 
    } 
}

#define PRINT_INDENTED_LINE(f, n, ...) \
    do { \
        indent_at_level(f, n); \
        fprintf(f,  __VA_ARGS__ ); \
    } while (0);

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
    [SK_TEMPLATE_ALIAS] = "SK_TEMPLATE_ALIAS",
    [SK_TEMPLATE_PARAMETER] = "SK_TEMPLATE_PARAMETER", 
    [SK_TEMPLATE_TYPE_PARAMETER] = "SK_TEMPLATE_TYPE_PARAMETER", 
    [SK_TEMPLATE_TEMPLATE_PARAMETER] = "SK_TEMPLATE_TEMPLATE_PARAMETER", 
    [SK_SCOPE] = "SK_SCOPE",
    // GCC Extension for builtin types
    [SK_GCC_BUILTIN_TYPE] = "SK_GCC_BUILTIN_TYPE",
};

static char* scope_names[] =
{
    [UNDEFINED_SCOPE] = "UNDEFINED_SCOPE",
    [NAMESPACE_SCOPE] = "NAMESPACE_SCOPE",
    [FUNCTION_SCOPE] = "FUNCTION_SCOPE",
    [PROTOTYPE_SCOPE] = "PROTOTYPE_SCOPE",
    [BLOCK_SCOPE] = "BLOCK_SCOPE",
    [CLASS_SCOPE] = "CLASS_SCOPE",
    [TEMPLATE_SCOPE] = "TEMPLATE_SCOPE",
};

struct brief_lines_t
{
    char* symbol_name;
    char* rest_of_line;
};

static struct brief_lines_t** brief_lines;
static int num_brief_lines;

static int cmpstringp(const void *p1, const void *p2)
{
    return strcmp((*(struct brief_lines_t**) p1)->symbol_name, 
            (*(struct brief_lines_t**) p2)->symbol_name);
}


static void print_brief_lines(struct brief_lines_t** brief_lines)
{
    // Sort lines
    P_LIST_ADD(brief_lines, num_brief_lines, NULL);
    qsort(brief_lines, num_brief_lines-1, sizeof(struct brief_lines_t*), cmpstringp);
    // Print lines
    while (*brief_lines != NULL)
    {
        fprintf(stderr, "%s %s\n", (*brief_lines)->symbol_name, 
                (*brief_lines)->rest_of_line);
        brief_lines++;
    }
}

void print_scope(scope_t* st)
{
    // print_scope_full(st, 0);
    if (compilation_options.debug_options.print_scope_brief)
    {
        num_brief_lines = 0;
        brief_lines = NULL;
        print_scope_brief(st, "");
        print_brief_lines(brief_lines);
    }
    else
    {
        print_scope_full(st, 0);
    }
}

static void print_scope_full(scope_t* st, int global_indent)
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
        if (entry_list->entry->do_not_print)
        {
            entry_list = entry_list->next;
            continue;
        }
        print_scope_entry(entry_list->entry, st, global_indent);

        if (entry_list->entry->related_scope != NULL)
        {
            if (entry_list->entry->related_scope->template_scope != NULL)
            {
                PRINT_INDENTED_LINE(stderr, global_indent+1, "[TEMPLATE_SCOPE - %p]\n", 
                        entry_list->entry->related_scope->template_scope);
                print_scope_full(entry_list->entry->related_scope->template_scope, global_indent+2);
            }
            if (entry_list->entry->related_scope->kind == FUNCTION_SCOPE)
            {
                if (entry_list->entry->related_scope->prototype_scope != NULL)
                {
                    PRINT_INDENTED_LINE(stderr, global_indent+1, "[PROTOTYPE_SCOPE - %p]\n",
                            entry_list->entry->related_scope->prototype_scope);
                    print_scope_full(entry_list->entry->related_scope->prototype_scope, global_indent+2);
                }
                if (entry_list->entry->related_scope->function_scope != NULL)
                {
                    PRINT_INDENTED_LINE(stderr, global_indent+1, "[FUNCTION_SCOPE - %p]\n",
                            entry_list->entry->related_scope->function_scope);
                    print_scope_full(entry_list->entry->related_scope->function_scope, global_indent+2);
                }
            }

            if (entry_list->entry->related_scope != NULL)
            {
                PRINT_INDENTED_LINE(stderr, global_indent+1, "[%s - %p]\n", 
                        scope_names[entry_list->entry->related_scope->kind],
                        entry_list->entry->related_scope);
                print_scope_full(entry_list->entry->related_scope, global_indent+2);
            }
        }

        entry_list = entry_list->next;
    }
}



static void print_scope_entry(scope_entry_t* entry, scope_t* st, int global_indent)
{
    PRINT_INDENTED_LINE(stderr, global_indent, "* \"%s\" %s", entry->symbol_name, symbol_kind_names[entry->kind]);

    if (entry->defined)
    {
        fprintf(stderr, " [DEFINED]");
    }

    fprintf(stderr, "\n");

    PRINT_INDENTED_LINE(stderr, global_indent+1, "Declared in line %d\n", entry->line);

    if (entry->kind == SK_VARIABLE
            || entry->kind == SK_TEMPLATE_PARAMETER
            || entry->kind == SK_TEMPLATE_TYPE_PARAMETER
            || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Type: %s\n", 
                print_declarator(entry->type_information, st));
    }

    if (entry->kind == SK_TYPEDEF)
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1,  "Aliased type: %s\n",
                print_declarator(entry->type_information->type->aliased_type, st));
    }

    if (entry->kind == SK_GCC_BUILTIN_TYPE)
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "(builtin type)\n");
    }

    if (entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS
            || entry->kind == SK_TEMPLATE_PRIMARY_CLASS)
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Template: %p\n", entry);
    }

    if (entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS)
    {
        if (entry->type_information->type->from_instantiation)
        {
            PRINT_INDENTED_LINE(stderr, global_indent+1, "Specialization instantiated\n");
        }
        else
        {
            PRINT_INDENTED_LINE(stderr, global_indent+1, "Specialization created but not instantiated\n");
        }
    }

    if (entry->kind == SK_NAMESPACE
            || entry->kind == SK_CLASS
            || entry->kind == SK_TEMPLATE_PRIMARY_CLASS
            || entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS)
    {
        // print_scope_full(entry->related_scope, global_indent+1);
    }

    if (entry->kind == SK_ENUMERATOR)
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Type: %s\n",
                print_declarator(entry->type_information, st));
    }

    if ((entry->kind == SK_VARIABLE || entry->kind == SK_ENUMERATOR)
            && entry->expression_value != NULL)
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Expression value: %s\n",
                prettyprint_in_buffer(entry->expression_value));
    }

    if (entry->kind == SK_FUNCTION
            || entry->kind == SK_TEMPLATE_FUNCTION)
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Prototype: %s\n",
                print_declarator(entry->type_information, st));
        // print_scope_full(entry->related_scope, global_indent+1);
		C_LANGUAGE()
		{
			if (entry->type_information->function->lacks_prototype)
			{
				PRINT_INDENTED_LINE(stderr, global_indent+1, "This function does not have prototype yet\n");
			}
		}
    }

	if (entry->is_member)
	{
		PRINT_INDENTED_LINE(stderr, global_indent+1, "Is member\n");
	}
}

// Brief versions of print scope

static void print_scope_brief(scope_t* st, char* qualif_name)
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

        print_scope_entry_list_brief(entry_list, st, qualif_name);
    }
}


static void print_scope_entry_list_brief(scope_entry_list_t* entry_list, scope_t* st, char* qualif_name)
{
    while (entry_list != NULL)
    {
        if (entry_list->entry->do_not_print)
        {
            entry_list = entry_list->next;
            continue;
        }
        print_scope_entry_brief(entry_list->entry, st, qualif_name);

        entry_list = entry_list->next;
    }
}

// Format of the line
// qualified-name def symbol-kind
//
// Where 'def' can be "D" or "U"
// and symbol_kind one of the SK_xxx defined in cxx-scope.h
static void print_scope_entry_brief(scope_entry_t* entry, scope_t* st, char* qualif_name)
{
    struct brief_lines_t* new_brief_line = calloc(1, sizeof(*new_brief_line));

    new_brief_line->symbol_name = qualif_name;
    new_brief_line->symbol_name = strappend(new_brief_line->symbol_name,
            "::");
    char* symbol_name = entry->symbol_name;

    if (entry->kind == SK_TEMPLATE_PRIMARY_CLASS
            || entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS)
    {
        symbol_name = strappend(symbol_name, "<");
        int i = 0;

        simple_type_t* simple_type_info = entry->type_information->type;
        template_argument_list_t* template_arguments = simple_type_info->template_arguments;

        int num_template_arguments = template_arguments->num_arguments;
        for (i = 0; i < num_template_arguments; i++)
        {
            symbol_name = strappend(symbol_name, 
                    prettyprint_in_buffer(template_arguments->argument_list[i]->argument_tree));
            if ((i + 1) != num_template_arguments)
            {
                symbol_name = strappend(symbol_name, ", ");
            }
        }
        symbol_name = strappend(symbol_name, " >");
    }

    new_brief_line->symbol_name = strappend(new_brief_line->symbol_name,
            symbol_name);

    new_brief_line->rest_of_line = calloc(256, sizeof(char));

    sprintf(new_brief_line->rest_of_line, "%s %s", entry->defined ? "D" : "U",
            symbol_kind_names[entry->kind]);

    P_LIST_ADD(brief_lines, num_brief_lines, new_brief_line);

    if (entry->related_scope != NULL)
    {
        char* new_qualif_name = strappend(qualif_name,
                strappend("::", symbol_name));
        if (entry->related_scope->template_scope != NULL)
        {
            print_scope_brief(entry->related_scope->template_scope, 
                    strappend(new_qualif_name, "::(template)"));
        }
        if (entry->related_scope->kind == FUNCTION_SCOPE)
        {
            if (entry->related_scope->prototype_scope != NULL)
            {
                print_scope_brief(entry->related_scope->prototype_scope, 
                        strappend(new_qualif_name, "::(prototype)"));
            }
            if (entry->related_scope->function_scope != NULL)
            {
                print_scope_brief(entry->related_scope->function_scope, 
                        strappend(new_qualif_name, "::(function)"));
            }
        }

        if (entry->related_scope != NULL)
        {
            print_scope_brief(entry->related_scope, new_qualif_name);
        }
    }
}
