#include "fortran03-typeutils.h"
#include "fortran03-prettyprint.h"
#include "cxx-nodecl-decls.h"
#include "cxx-limits.h"
#include "cxx-utils.h"
#include <string.h>

const char* fortran_print_type_str(type_t* t)
{
    const char* result = "";
    char is_pointer = 0;
    if (is_pointer_type(t))
    {
        is_pointer = 1;
        t = pointer_type_get_pointee_type(t);
    }

    struct array_spec_tag {
        nodecl_t lower;
        nodecl_t upper;
        char is_undefined;
    } array_spec_list[MCXX_MAX_ARRAY_SPECIFIER] = { { nodecl_null(), nodecl_null(), 0 }  };

    int array_spec_idx;
    for (array_spec_idx = MCXX_MAX_ARRAY_SPECIFIER - 1; 
            is_fortran_array_type(t);
            array_spec_idx--)
    {
        if (array_spec_idx < 0)
        {
            internal_error("too many array dimensions %d\n", MCXX_MAX_ARRAY_SPECIFIER);
        }

        if (!array_type_is_unknown_size(t))
        {
            array_spec_list[array_spec_idx].lower = array_type_get_array_lower_bound(t);
            array_spec_list[array_spec_idx].upper = array_type_get_array_upper_bound(t);
        }
        else
        {
            array_spec_list[array_spec_idx].is_undefined = 1;
        }

        t = array_type_get_element_type(t);
    }

    char is_array = (array_spec_idx != (MCXX_MAX_ARRAY_SPECIFIER - 1));

    if (is_bool_type(t)
            || is_integer_type(t)
            || is_float_type(t)
            || is_double_type(t)
            || is_complex_type(t))
    {
        const char* type_name = NULL;
        char c[128] = { 0 };

        if (is_bool_type(t))
        {
            type_name = "LOGICAL";
        }
        // Do not move this after is_integer_type
        else if (is_character_type(t))
        {
            type_name = "CHARACTER";
        }
        else if (is_integer_type(t))
        {
            type_name = "INTEGER";
        }
        else if (is_float_type(t)
                || is_double_type(t))
        {
            type_name = "REAL";
        }
        else if (is_complex_type(t))
        {
            type_name = "COMPLEX";
        }
        else
        {
            internal_error("unreachable code", 0);
        }

        snprintf(c, 127, "%s(%zd)", type_name, type_get_size(t));
        c[127] = '\0';

        result = uniquestr(c);
    }
    else if (is_class_type(t))
    {
        scope_entry_t* entry = named_type_get_symbol(t);
        char c[128] = { 0 };
        snprintf(c, 127, "TYPE(%s)", 
                entry->symbol_name);
        c[127] = '\0';

        result = uniquestr(c);
    }
    else if (is_fortran_character_type(t))
    {
        nodecl_t length = array_type_get_array_size_expr(t);
        char c[128] = { 0 };
        snprintf(c, 127, "CHARACTER(LEN=%s)",
                nodecl_is_null(length) ? "*" : fortran_prettyprint_in_buffer(nodecl_get_ast(length)));
        c[127] = '\0';
        result = uniquestr(c);
    }
    else if (is_function_type(t))
    {
        result = "PROCEDURE";
        // result = strappend(result, "(");

        // int i;
        // 
        // int n = function_type_get_num_parameters(t);
        // for (i = 0; i < n; i++)
        // {
        //     type_t* param_type = function_type_get_parameter_type_num(t, i);
        //     char c[256];
        //     snprintf(c, 255, "%s%s", (i == 0 ? "" : ", "), 
        //             fortran_print_type_str(param_type));
        //     c[255] = '\0';

        //     result = strappend(result, c);
        // }

        // result = strappend(result, ")");
    }
    else 
    {
        internal_error("Not a FORTRAN printable type '%s'\n", print_declarator(t));
    }

    if (is_pointer)
    {
        result = strappend(result, ", POINTER");
    }

    if (is_array)
    {
        array_spec_idx++;
        result = strappend(result, ", DIMENSION(");

        while (array_spec_idx <= (MCXX_MAX_ARRAY_SPECIFIER - 1))
        {
            if (!array_spec_list[array_spec_idx].is_undefined)
            {
                result = strappend(result, fortran_prettyprint_in_buffer(nodecl_get_ast(array_spec_list[array_spec_idx].lower)));
                result = strappend(result, ":");
                result = strappend(result, fortran_prettyprint_in_buffer(nodecl_get_ast(array_spec_list[array_spec_idx].upper)));
            }
            else
            {
                result = strappend(result, ":");
            }
            if ((array_spec_idx + 1) <= (MCXX_MAX_ARRAY_SPECIFIER - 1))
            {
                result = strappend(result, ", ");
            }
            array_spec_idx++;
        }

        result = strappend(result, ")");
    }

    // if (is_character)
    // {
    // }

    return result;
}

char is_pointer_to_array_type(type_t* t)
{
    return (is_pointer_type(t)
            && is_array_type(pointer_type_get_pointee_type(t)));
}

int get_rank_of_type(type_t* t)
{
    if (!is_fortran_array_type(t)
            && !is_pointer_to_array_type(t))
        return 0;

    if (is_pointer_to_array_type(t))
    {
        t = pointer_type_get_pointee_type(t);
    }

    int result = 0;
    while (is_fortran_array_type(t))
    {
        result++;
        t = array_type_get_element_type(t);
    }

    return result;
}

type_t* get_rank0_type(type_t* t)
{
    while (is_fortran_array_type(t))
    {
        t = array_type_get_element_type(t);
    }
    return t;
}


char is_fortran_character_type(type_t* t)
{
    return (is_array_type(t)
            && is_character_type(array_type_get_element_type(t)));
}

char is_pointer_to_fortran_character_type(type_t* t)
{
    if (is_pointer_type(t))
    {
        return is_fortran_character_type(pointer_type_get_pointee_type(t));
    }
    return 0;
}

type_t* replace_return_type_of_function_type(type_t* function_type, type_t* new_return_type)
{
    ERROR_CONDITION(!is_function_type(function_type), "Must be a function type", 0);

    int num_parameters = function_type_get_num_parameters(function_type);
    if (!function_type_get_lacking_prototype(function_type))
    {
        parameter_info_t parameter_info[1 + num_parameters];
        memset(&parameter_info, 0, sizeof(parameter_info));
        int i;
        for (i = 0; i < num_parameters; i++)
        {
            parameter_info[i].type_info = function_type_get_parameter_type_num(function_type, i);
        }

        return get_new_function_type(new_return_type, parameter_info, num_parameters);
    }
    else
    {
        return get_nonproto_function_type(new_return_type, num_parameters);
    }
}

char equivalent_tkr_types(type_t* t1, type_t* t2)
{
    type_t* r1 = get_rank0_type(t1);
    type_t* r2 = get_rank0_type(t2);

    if (!equivalent_types(get_unqualified_type(r1), get_unqualified_type(r2)))
        return 0;

    int rank1 = get_rank_of_type(t1);
    int rank2 = get_rank_of_type(t2);

    if (rank1 != rank2)
        return 0;

    return 1;
}

type_t* update_basic_type_with_type(type_t* type_info, type_t* basic_type)
{
    if (is_pointer_type(type_info))
    {
        return get_pointer_type(
                update_basic_type_with_type(pointer_type_get_pointee_type(type_info), basic_type)
                );
    }
    else if (is_array_type(type_info))
    {
        return get_array_type_bounds(
                update_basic_type_with_type(array_type_get_element_type(type_info), basic_type),
                array_type_get_array_lower_bound(type_info),
                array_type_get_array_upper_bound(type_info),
                array_type_get_array_size_expr_context(type_info));

    }
    else if (is_function_type(type_info))
    {
        return replace_return_type_of_function_type(type_info, basic_type);
    }
    else
    {
        return basic_type;
    }
}

char basic_type_is_void(type_t* t)
{
    if (t == NULL)
    {
        return 0;
    }
    else if (is_void_type(t))
    {
        return 1;
    }
    else if (is_array_type(t))
    {
        return basic_type_is_void(array_type_get_element_type(t));
    }
    else if (is_function_type(t))
    {
        return basic_type_is_void(function_type_get_return_type(t));
    }
    else if (is_pointer_type(t))
    {
        return basic_type_is_void(pointer_type_get_pointee_type(t));
    }
    else
        return 0;
}

char is_fortran_array_type(type_t* t)
{
    return is_array_type(t)
        && !is_fortran_character_type(t);
}

char is_pointer_to_fortran_array_type(type_t* t)
{
    return is_pointer_type(t)
        && is_fortran_array_type(pointer_type_get_pointee_type(t));
}

type_t* rebuild_array_type(type_t* rank0_type, type_t* array_type)
{
    ERROR_CONDITION(!is_scalar_type(rank0_type)
            && !is_fortran_character_type(rank0_type), "Invalid rank0 type", 0);

    if (!is_fortran_array_type(array_type))
    {
        return rank0_type;
    }
    else
    {
        type_t* t = rebuild_array_type(rank0_type, array_type_get_element_type(array_type));
        if (!array_type_is_unknown_size(array_type))
        {
            return get_array_type_bounds(t, 
                    array_type_get_array_lower_bound(array_type),
                    array_type_get_array_upper_bound(array_type),
                    array_type_get_array_size_expr_context(array_type));
        }
        else
        {
            return get_array_type(t, nodecl_null(), array_type_get_array_size_expr_context(array_type));
        }
    }
}

type_t* get_n_ranked_type(type_t* scalar_type, int rank, decl_context_t decl_context)
{
    ERROR_CONDITION(is_fortran_array_type(scalar_type), "This is not a scalar type!", 0);

    if (rank == 0)
    {
        return scalar_type;
    }
    else if (rank > 0)
    {
        return get_array_type(get_n_ranked_type(scalar_type, rank-1, decl_context), nodecl_null(), decl_context);
    }
    else
    {
        internal_error("Invalid rank %d\n", rank);
    }
}

char is_fortran_intrinsic_type(type_t* t)
{
    if (is_pointer_type(t))
        t = pointer_type_get_pointee_type(t);

    return (is_integer_type(t)
            || is_floating_type(t)
            || is_complex_type(t)
            || is_bool_type(t)
            || is_fortran_character_type(t));
}

char are_conformable_types(type_t* t1, type_t* t2)
{
    if (get_rank_of_type(t1) == get_rank_of_type(t2))
        return 1;
    else if (get_rank_of_type(t1) == 1
            || get_rank_of_type(t2) == 1)
        return 1;
    else
        return 0;
}
