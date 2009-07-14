#include "cxx-omp-support.h"
#include "cxx-utils.h"
#include "cxx-buildscope.h"
#include <stdlib.h>
#include <string.h>

typedef
struct omp_udr_reduction_tag
{
    type_t* type;

    const char* reductor_name;

    omp_udr_associativity_t assoc;
    char is_builtin;
    AST identity;
} omp_udr_reduction_t;

int _omp_udr_num;
omp_udr_reduction_t** _omp_udr_reduction;

static void omp_udr_register_reduction_(type_t* type, 
        const char* reductor_name, 
        AST identity,
        omp_udr_associativity_t assoc,
        char is_builtin)
{
    type = advance_over_typedefs(type);

    omp_udr_reduction_t* new_udr = calloc(1, sizeof(*new_udr));
    new_udr->type = type;
    new_udr->is_builtin = is_builtin;
    new_udr->assoc = assoc;
    new_udr->identity = identity;
    new_udr->reductor_name = reductor_name;

    P_LIST_ADD(_omp_udr_reduction, _omp_udr_num, new_udr);
}

void omp_udr_register_reduction_builtin(type_t* type, 
        const char* reductor_name, 
        AST identity,
        omp_udr_associativity_t assoc)
{
    omp_udr_register_reduction_(type, 
            reductor_name, 
            identity, 
            assoc, 
            /* is_builtin */ 1);
}

void omp_udr_register_reduction(type_t* type, 
        const char* reductor_name,
        AST identity,
        omp_udr_associativity_t assoc)
{
    omp_udr_register_reduction_(type, 
            reductor_name,
            identity, 
            assoc, 
            /* is_builtin */ 0);
}

char omp_udr_lookup_reduction(type_t* t, 
        const char* reductor_name, 
        AST* identity, 
        omp_udr_associativity_t* assoc,
        char *is_builtin)
{
    t = advance_over_typedefs(t);

    int i;
    for (i = 0; i < _omp_udr_num; i++)
    {
        if (equivalent_types(_omp_udr_reduction[i]->type, t)
                && _omp_udr_reduction[i]->is_builtin
                && (strcmp(_omp_udr_reduction[i]->reductor_name, reductor_name) == 0))
        {
            *identity = _omp_udr_reduction[i]->identity;
            *assoc = _omp_udr_reduction[i]->assoc;
            *is_builtin = _omp_udr_reduction[i]->is_builtin;
            return 1;
        }
    }

    return 0;
}

void omp_udr_initialize_basic_types(decl_context_t decl_context)
{
    // This populates basic types

    type_t* all_arithmetic_types[] =
    {
        get_char_type(),
        get_signed_short_int_type(),
        get_signed_int_type(),
        get_signed_long_int_type(),
        get_signed_long_long_int_type(),
        get_signed_char_type(),
        get_signed_short_int_type(),
        get_signed_int_type(),
        get_signed_long_int_type(),
        get_signed_long_long_int_type(),
        get_unsigned_char_type(),
        get_unsigned_short_int_type(),
        get_unsigned_int_type(),
        get_unsigned_long_int_type(),
        get_unsigned_long_long_int_type(),
        get_float_type(),
        get_double_type(),
        get_long_double_type(),
        NULL,
    };

    typedef
    struct 
    {
        const char* operator_name;
        AST neuter_tree;
    } reduction_info_t; 

    AST zero = internal_expression_parse("0", decl_context);
    AST one = internal_expression_parse("1", decl_context);
    AST neg_zero = internal_expression_parse("~0", decl_context);

    reduction_info_t builtin_arithmetic_operators[] =
    {
        {"+", zero}, 
        {"-", zero}, 
        {"*", one}, 
        {NULL, NULL}
    };

    reduction_info_t builtin_logic_bit_operators[] =
    {
        {"&", neg_zero}, 
        {"|", zero}, 
        {"^", zero}, 
        {"&&", one}, 
        {"||", zero}, 
        {NULL, NULL}
    };
    
    int i;
    type_t* type;
    for (i = 0; (type = all_arithmetic_types[i]) != NULL; i++)
    {
        int j;
        for (j = 0; builtin_arithmetic_operators[j].operator_name != NULL; j++)
        {
            omp_udr_register_reduction_builtin(type,
                    builtin_arithmetic_operators[j].operator_name,
                    builtin_arithmetic_operators[j].neuter_tree,
                    OMP_UDR_ORDER_LEFT
                    );

        }
        if (is_integral_type(type))
        {
            for (j = 0; builtin_logic_bit_operators[j].operator_name != NULL; j++)
            {
                omp_udr_register_reduction_builtin(type,
                        builtin_logic_bit_operators[j].operator_name,
                        builtin_logic_bit_operators[j].neuter_tree,
                        OMP_UDR_ORDER_LEFT
                        );
            }
        }
    }
}
