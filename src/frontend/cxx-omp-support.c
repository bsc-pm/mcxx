#include "cxx-omp-support.h"
#include "cxx-utils.h"
#include <stdlib.h>
#include <string.h>

typedef
struct omp_udr_reduction_tag
{
    type_t* type;
    union 
    {
        const char* operator_name;
        scope_entry_t* entry;
    } _red;
    omp_udr_associativity_t assoc;
    char is_builtin;
    AST identity;
} omp_udr_reduction_t;

int _omp_udr_num;
omp_udr_reduction_t** _omp_udr_reduction;

void omp_udr_register_reduction_builtin(type_t* type, 
        const char* operator_name, 
        AST identity,
        omp_udr_associativity_t assoc)
{
    type = advance_over_typedefs(type);

    omp_udr_reduction_t* new_udr = calloc(1, sizeof(*new_udr));
    new_udr->type = type;
    new_udr->is_builtin = 1;
    new_udr->assoc = assoc;
    new_udr->identity = identity;
    new_udr->_red.operator_name = operator_name;

    P_LIST_ADD(_omp_udr_reduction, _omp_udr_num, new_udr);
}

void omp_udr_register_reduction_function(type_t* type,
        scope_entry_t* entry, 
        AST identity,
        omp_udr_associativity_t assoc)
{
    type = advance_over_typedefs(type);

    omp_udr_reduction_t* new_udr = calloc(1, sizeof(*new_udr));
    new_udr->type = type;
    new_udr->is_builtin = 0;
    new_udr->assoc = assoc;
    new_udr->identity = identity;
    new_udr->_red.entry = entry;

    P_LIST_ADD(_omp_udr_reduction, _omp_udr_num, new_udr);
}

char omp_udr_lookup_function(type_t* t, 
        scope_entry_t* entry, 
        AST* identity, 
        omp_udr_associativity_t* assoc)
{
    t = advance_over_typedefs(t);

    int i;
    for (i = 0; i < _omp_udr_num; i++)
    {
        if (!_omp_udr_reduction[i]->is_builtin
                && _omp_udr_reduction[i]->type == t
                && _omp_udr_reduction[i]->_red.entry == entry)
        {
            *identity = _omp_udr_reduction[i]->identity;
            *assoc = _omp_udr_reduction[i]->assoc;
            return 1;
        }
    }

    return 0;
}

char omp_udr_lookup_builtin(type_t* t, 
        const char* operator_name,
        AST *identity,
        omp_udr_associativity_t* assoc)
{
    t = advance_over_typedefs(t);

    int i;
    for (i = 0; i < _omp_udr_num; i++)
    {
        if (_omp_udr_reduction[i]->is_builtin
                && _omp_udr_reduction[i]->type == t
                && (strcmp(operator_name, _omp_udr_reduction[i]->_red.operator_name) == 0))
        {
            *identity = _omp_udr_reduction[i]->identity;
            *assoc = _omp_udr_reduction[i]->assoc;
            return 1;
        }
    }

    return 0;
}
