#include "fortran03-scope.h"
#include "cxx-utils.h"
#include "cxx-typeutils.h"
#include "cxx-scope.h"
#include "cxx-entrylist.h"
#include <string.h>
#include <ctype.h>

long long unsigned int _bytes_fortran_scope = 0;

typedef struct type_tag* implicit_set_info_t['z' - 'a' + 1];

struct implicit_info_tag
{
    implicit_set_info_t* implicit_set_info;
    char shared;
};

static implicit_info_t* get_default_fortran_implicit(void)
{
    static implicit_info_t* result = NULL;
    if (result == NULL)
    {
        result = counted_calloc(1, 
                sizeof(*result), 
                &_bytes_fortran_scope);
        result->implicit_set_info = counted_calloc(1, 
                sizeof(*(result->implicit_set_info)), 
                &_bytes_fortran_scope);
        char c;
        for (c = 'a'; c <= 'z'; c++)
        {
            (*(result->implicit_set_info))[c - 'a'] = get_float_type();
        }
        for (c = 'i'; c <= 'n'; c++)
        {
            (*(result->implicit_set_info))[c - 'a'] = get_signed_int_type();
        }
        result->shared = 1;
    }
    return result;
}

void set_implicit_info(decl_context_t decl_context, char from_letter, char to_letter, type_t* type)
{
    if (decl_context.implicit_info->shared)
    {
        // Copy on write
        // Not shared anymore, we own it
        implicit_set_info_t* old_implicit_set_info = decl_context.implicit_info->implicit_set_info;

        decl_context.implicit_info->shared = 0;

        implicit_set_info_t* new_implicit_set_info 
            = counted_calloc(1, sizeof(*new_implicit_set_info), &_bytes_fortran_scope);
        decl_context.implicit_info->implicit_set_info = new_implicit_set_info;

        // Copy        
        if (old_implicit_set_info != NULL)
        {
            memcpy(decl_context.implicit_info->implicit_set_info,
                    old_implicit_set_info,
                    sizeof(*new_implicit_set_info));
        }
    }

    char letter = from_letter;
    while (letter <= to_letter)
    {
        ERROR_CONDITION(!('a' <= tolower(letter)
                    && tolower(letter) <= 'z'), "Invalid letter %c", letter);
        (*(decl_context.implicit_info->implicit_set_info))[tolower(letter) - 'a'] = type;

        letter++;
    }
}

void set_implicit_none(decl_context_t decl_context)
{
    decl_context.implicit_info->shared = 0;
    decl_context.implicit_info->implicit_set_info = NULL;
}

decl_context_t new_program_unit_context(decl_context_t decl_context)
{
    decl_context_t result = new_block_context(decl_context);
    result.implicit_info = get_default_fortran_implicit();

    return result;
}

decl_context_t new_internal_program_unit_context(decl_context_t decl_context)
{
    decl_context_t result = new_block_context(decl_context);

    result.implicit_info = counted_calloc(1, 
            sizeof(*(result.implicit_info)), &_bytes_fortran_scope);

    result.implicit_info->shared = 1;
    result.implicit_info->implicit_set_info = decl_context.implicit_info->implicit_set_info;

    return result;
}

static scope_entry_t* new_implicit_symbol(decl_context_t decl_context, const char* name)
{
    // Special names for operators and other non regularly named stuff will not get here
    if (('a' <= tolower(name[0]))
            && (tolower(name[0]) <= 'z'))
    {
        type_t* implicit_type = 
            (*(decl_context.implicit_info->implicit_set_info))[tolower(name[0]) - 'a'];
        if (implicit_type == NULL)
            return NULL;

        scope_entry_t* sym = new_symbol(decl_context, decl_context.current_scope, strtolower(name));
        sym->kind = SK_VARIABLE;
        sym->entity_specs.is_implicit = 1;
        sym->type_information = implicit_type;

        return sym;
    }

    return NULL;
}

scope_entry_t* query_name_no_implicit(decl_context_t decl_context, const char* name)
{
    scope_entry_list_t* entry_list = query_unqualified_name_str(decl_context, strtolower(name));

    scope_entry_t* result = NULL;

    if (entry_list != NULL )
    {
        result = entry_list_head(entry_list);
        entry_list_free(entry_list);
    }

    return result;
}

scope_entry_t* query_name(decl_context_t decl_context, const char* name)
{
    scope_entry_t* result = query_name_no_implicit(decl_context, name);

    if (result == NULL)
    {
        if (decl_context.implicit_info->implicit_set_info != NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: getting implicit entity for name '%s'\n", name);
            }
            result = new_implicit_symbol(decl_context, name);
        }
        DEBUG_CODE()
        {
            if (result == NULL)
            {
                fprintf(stderr, "SCOPE: there is no implicit name for entity '%s'\n", name);
            }
        }
    }

    return result;
}

decl_context_t fortran_new_block_context(decl_context_t decl_context)
{
    decl_context_t result = new_block_context(decl_context);
    result.implicit_info = counted_calloc(1, sizeof(*result.implicit_info), &_bytes_fortran_scope);
    *result.implicit_info = *decl_context.implicit_info;

    return result;
}

scope_entry_t* new_fortran_symbol(decl_context_t decl_context, const char* name)
{
    return new_symbol(decl_context, decl_context.current_scope, strtolower(name));
}
