#include "fortran03-scope.h"
#include "cxx-utils.h"
#include "cxx-typeutils.h"
#include "cxx-scope.h"
#include "cxx-entrylist.h"
#include <string.h>
#include <ctype.h>

long long unsigned int _bytes_fortran_scope = 0;

typedef struct type_tag* implicit_letter_set_t['z' - 'a' + 1];

typedef
struct implicit_info_data_tag
{
    char letter_set_is_shared;
    implicit_letter_set_t* implicit_letter_set;
} implicit_info_data_t;

struct implicit_info_tag
{
    implicit_info_data_t* data;
};

static implicit_letter_set_t* allocate_implicit_letter_set(void)
{
    implicit_letter_set_t* result = counted_calloc(1, sizeof(*result), &_bytes_fortran_scope);

    return result;
}

static implicit_info_data_t* allocate_implicit_info_data(void)
{
    implicit_info_data_t* result = counted_calloc(1, sizeof(*result), &_bytes_fortran_scope);

    result->implicit_letter_set = allocate_implicit_letter_set();

    return result;
}
    
static implicit_info_t* allocate_implicit_info(void)
{
    implicit_info_t* result = counted_calloc(1, sizeof(*result), &_bytes_fortran_scope);

    result->data = allocate_implicit_info_data();
    result->data->implicit_letter_set = allocate_implicit_letter_set();

    return result;
}

static implicit_info_t* allocate_implicit_info_sharing_set(implicit_info_t* implicit_letter_set)
{
    implicit_info_t* result = counted_calloc(1, sizeof(*result), &_bytes_fortran_scope);

    result->data = allocate_implicit_info_data();
    result->data->implicit_letter_set = implicit_letter_set->data->implicit_letter_set;
    result->data->letter_set_is_shared = 1;

    return result;
}

static implicit_info_t* get_default_fortran_implicit(void)
{
    static implicit_info_t* result;
    if (result == NULL)
    {
        result = allocate_implicit_info();

        char c;
        for (c = 'a'; c <= 'z'; c++)
        {
            (*(result->data->implicit_letter_set))[c - 'a'] = get_float_type();
        }
        for (c = 'i'; c <= 'n'; c++)
        {
            (*(result->data->implicit_letter_set))[c - 'a'] = get_signed_int_type();
        }
        result->data->letter_set_is_shared = 1;
    }

    return result;
}

static void copy_on_write_implicit(decl_context_t decl_context)
{
    if (decl_context.implicit_info->data->letter_set_is_shared)
    {
        implicit_info_data_t* old_implicit_info = decl_context.implicit_info->data;

        decl_context.implicit_info->data = allocate_implicit_info_data();

        if (old_implicit_info->implicit_letter_set != NULL)
        {
            memcpy(decl_context.implicit_info->data->implicit_letter_set,
                    old_implicit_info->implicit_letter_set,
                    sizeof (*old_implicit_info->implicit_letter_set));
        }
    }
}

void set_implicit_info(decl_context_t decl_context, char from_letter, char to_letter, type_t* type)
{
    copy_on_write_implicit(decl_context);

    char letter = from_letter;
    while (letter <= to_letter)
    {
        ERROR_CONDITION(!('a' <= tolower(letter)
                    && tolower(letter) <= 'z'), "Invalid letter %c", letter);
        (*(decl_context.implicit_info->data->implicit_letter_set))[tolower(letter) - 'a'] = type;

        letter++;
    }
}

void set_implicit_none(decl_context_t decl_context)
{
    copy_on_write_implicit(decl_context);

    decl_context.implicit_info->data->implicit_letter_set = NULL;
}

char is_implicit_none(decl_context_t decl_context)
{
    return decl_context.implicit_info->data->implicit_letter_set == NULL;
}

char implicit_has_been_set(decl_context_t decl_context)
{
    return !decl_context.implicit_info->data->letter_set_is_shared;
}

decl_context_t new_program_unit_context(decl_context_t decl_context)
{
    decl_context_t result = new_block_context(decl_context);
    result = new_function_context(result);
    result.implicit_info = allocate_implicit_info_sharing_set(get_default_fortran_implicit());
    result.current_scope->related_entry = NULL;

    return result;
}

decl_context_t new_internal_program_unit_context(decl_context_t decl_context)
{
    decl_context_t result = new_block_context(decl_context);
    result = new_function_context(result);
    result.implicit_info = allocate_implicit_info_sharing_set(decl_context.implicit_info);
    result.current_scope->related_entry = NULL;

    return result;
}

static scope_entry_t* new_implicit_symbol(decl_context_t decl_context, AST locus, const char* name)
{
    // Special names for operators and other non regularly named stuff will not get here
    if (('a' <= tolower(name[0]))
            && (tolower(name[0]) <= 'z'))
    {
        type_t* implicit_type = 
            (*(decl_context.implicit_info->data->implicit_letter_set))[tolower(name[0]) - 'a'];
        if (implicit_type == NULL)
            return NULL;

        scope_entry_t* sym = new_symbol(decl_context, decl_context.current_scope, strtolower(name));
        sym->kind = SK_VARIABLE;
        sym->type_information = implicit_type;
        sym->entity_specs.is_implicit_basic_type = 1;
        
        if (locus != NULL)
        {
            sym->file = ASTFileName(locus);
            sym->line = ASTLine(locus);
        }

        return sym;
    }

    return NULL;
}

type_t* get_implicit_type_for_symbol(decl_context_t decl_context, const char* name)
{
    type_t* implicit_type = NULL;

    if (decl_context.implicit_info->data->implicit_letter_set != NULL)
    {
        implicit_type = 
            (*(decl_context.implicit_info->data->implicit_letter_set))[tolower(name[0]) - 'a'];
    }

    if (implicit_type == NULL)
        implicit_type = get_void_type();

    return implicit_type;
}

scope_entry_t* query_name_no_implicit_or_builtin(decl_context_t decl_context, const char* name)
{
    scope_entry_list_t* entry_list = query_name_str(decl_context, strtolower(name));

    scope_entry_t* result = NULL;

    if (entry_list != NULL )
    {
        result = entry_list_head(entry_list);
        entry_list_free(entry_list);

        if (result->entity_specs.is_builtin)
            result = NULL;
    }

    return result;
}

scope_entry_t* query_name_no_implicit(decl_context_t decl_context, const char* name)
{
    scope_entry_list_t* entry_list = query_name_str(decl_context, strtolower(name));

    scope_entry_t* result = NULL;

    if (entry_list != NULL )
    {
        result = entry_list_head(entry_list);
        entry_list_free(entry_list);
    }

    return result;
}

scope_entry_t* query_name_with_locus(decl_context_t decl_context, AST locus, const char* name)
{
    scope_entry_t* result = query_name_no_implicit(decl_context, name);

    if (result == NULL)
    {
        if (decl_context.implicit_info != NULL
                && decl_context.implicit_info->data != NULL
                && decl_context.implicit_info->data->implicit_letter_set != NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Getting implicit entity for name '%s'\n", name);
            }
            result = new_implicit_symbol(decl_context, locus, name);
        }
        DEBUG_CODE()
        {
            if (result == NULL)
            {
                fprintf(stderr, "SCOPE: There is no implicit name for entity '%s'\n", name);
            }
        }
    }

    return result;
}

decl_context_t fortran_new_block_context(decl_context_t decl_context)
{
    decl_context_t result = new_block_context(decl_context);
    return result;
}

scope_entry_t* new_fortran_symbol(decl_context_t decl_context, const char* name)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Creating new symbol '%s' in scope '%p'\n", 
                strtolower(name),
                decl_context.current_scope);
    }
    return new_symbol(decl_context, decl_context.current_scope, strtolower(name));
}

scope_entry_t* query_name_in_class(decl_context_t class_context, const char* name)
{
    scope_entry_t* entry = NULL;
    scope_entry_list_t* entry_list = class_context_lookup(class_context, DF_NONE, strtolower(name));
    if (entry_list != NULL)
    {
        entry = entry_list_head(entry_list);
    }
    entry_list_free(entry_list);

    return entry;
}
