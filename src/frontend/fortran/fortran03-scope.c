#include "fortran03-scope.h"
#include "cxx-utils.h"
#include "cxx-typeutils.h"
#include "cxx-scope.h"
#include "cxx-entrylist.h"
#include <ctype.h>

long long unsigned int _bytes_fortran_scope = 0;

struct implicit_info_tag
{
    struct type_tag* implicit_set['z' - 'a' + 1];
};

static implicit_info_t* get_default_fortran_implicit(void)
{
    static implicit_info_t* result = NULL;
    if (result == NULL)
    {
        result = counted_calloc(1, sizeof(*result), &_bytes_fortran_scope);
        char c;
        for (c = 'a'; c <= 'z'; c++)
        {
            result->implicit_set[c - 'a'] = get_float_type();
        }
        for (c = 'i'; c <= 'n'; c++)
        {
            result->implicit_set[c - 'a'] = get_signed_int_type();
        }
    }
    return result;
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
    result.implicit_info = decl_context.implicit_info;

    return result;
}


static scope_entry_t* new_implicit_symbol(decl_context_t decl_context, const char* name)
{
    scope_entry_t* sym = new_symbol(decl_context, decl_context.current_scope, name);
    sym->kind = SK_VARIABLE;
    sym->entity_specs.is_implicit = 1;
    sym->type_information = decl_context.implicit_info->implicit_set[tolower(name[0]) - 'a'];

    return sym;
}

scope_entry_t* query_name(decl_context_t decl_context, const char* name)
{
    scope_entry_list_t* entry_list = query_unqualified_name_str(decl_context, name);

    scope_entry_t* result = NULL;

    if (entry_list == NULL )
    {
        if (decl_context.implicit_info != NULL)
        {
            result = new_implicit_symbol(decl_context, name);
        }
    }
    else
    {
        result = entry_list_head(entry_list);
        entry_list_free(entry_list);
    }

    return result;
}
