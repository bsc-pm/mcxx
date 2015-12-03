#include "fortran03-mangling.h"
#include "fortran03-scope.h"
#include "cxx-scope.h"
#include "cxx-process.h"

// Note: we implement default manglings for each compiler

static const char* gfortran_mangle_symbol(scope_entry_t* entry)
{
    entry = fortran_get_ultimate_symbol(entry);

    const char* c;
    scope_entry_t* in_module = symbol_entity_specs_get_in_module(entry);
    if (in_module != NULL)
    {
        uniquestr_sprintf(&c, "__%s_MOD_%s", in_module->symbol_name, entry->symbol_name);
    }
    else
    {
        uniquestr_sprintf(&c, "%s_", entry->symbol_name);
    }
    return c;
}

static const char* ifort_mangle_symbol(scope_entry_t* entry)
{
    entry = fortran_get_ultimate_symbol(entry);

    const char* c;
    scope_entry_t* in_module = symbol_entity_specs_get_in_module(entry);
    if (in_module != NULL)
    {
        uniquestr_sprintf(&c, "%s_mp_%s_", in_module->symbol_name, entry->symbol_name);
    }
    else
    {
        uniquestr_sprintf(&c, "%s_", entry->symbol_name);
    }
    return c;
}

static const char* xlf_mangle_symbol(scope_entry_t* entry)
{
    // NOTE: We assume -qextname
    entry = fortran_get_ultimate_symbol(entry);

    const char* c;
    scope_entry_t* in_module = symbol_entity_specs_get_in_module(entry);
    if (in_module != NULL)
    {
        if (entry->kind == SK_FUNCTION)
        {
            uniquestr_sprintf(&c, "__%s_NMOD_%s_", in_module->symbol_name, entry->symbol_name);
        }
        else
        {
            // This is probably useless for source-to-source, but OK
            uniquestr_sprintf(&c, "&&%s&N&%s_", in_module->symbol_name, entry->symbol_name);
        }
    }
    else
    {
        uniquestr_sprintf(&c, "%s_", entry->symbol_name);
    }
    return c;
}

const char* fortran_mangle_symbol(scope_entry_t* entry)
{
    ERROR_CONDITION(entry->kind != SK_VARIABLE
            && entry->kind != SK_FUNCTION,
            "Cannot mangle symbol of kind '%s'\n", symbol_kind_name(entry));
    ERROR_CONDITION(entry->decl_context->current_scope->kind == BLOCK_SCOPE
            && entry->decl_context->current_scope->related_entry->kind == SK_FUNCTION,
            "Cannot mangle local entity '%s' (%s)\n",
            entry->symbol_name, locus_to_str(entry->locus));
    ERROR_CONDITION(symbol_entity_specs_get_is_intrinsic_subroutine(entry)
            || symbol_entity_specs_get_is_intrinsic_subroutine(entry),
            "Cannot mangle intrinsic procedure '%s'\n",
            entry->symbol_name);

    return (CURRENT_CONFIGURATION->fortran_name_mangling->mangle)(entry);
}

static fortran_name_mangling_t gfortran_name_mangling =
{
    "gfortran",
    "GNU Fortran",
    gfortran_mangle_symbol,
};

static fortran_name_mangling_t ifort_name_mangling =
{
    "ifort",
    "Intel Fortran",
    ifort_mangle_symbol,
};

static fortran_name_mangling_t xlf_name_mangling =
{
    "xlf",
    "IBM XL Fortran (-qextname)",
    xlf_mangle_symbol,
};

fortran_name_mangling_t* default_fortran_name_mangling = &gfortran_name_mangling;
fortran_name_mangling_t* fortran_name_mangling_list[] =
{
    &gfortran_name_mangling,
    &ifort_name_mangling,
    &xlf_name_mangling,
    NULL,
};
