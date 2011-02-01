#include "fortran03-typeutils.h"
#include "fortran03-prettyprint.h"
#include "cxx-utils.h"

const char* fortran_print_type_str(type_t* t)
{
    const char* result = "";
    char is_pointer = 0;
    if (is_pointer_type(t))
    {
        is_pointer = 1;
        t = pointer_type_get_pointee_type(t);
    }

#define MAX_ARRAY_SPEC 16
    struct array_spec_tag {
        AST lower;
        AST upper;
    } array_spec_list[MAX_ARRAY_SPEC] = { { 0, 0 }  };

    int array_spec_idx;
    for (array_spec_idx = MAX_ARRAY_SPEC - 1; 
            is_array_type(t); 
            array_spec_idx--)
    {
        if (array_spec_idx < 0)
        {
            internal_error("too many array dimensions %d\n", MAX_ARRAY_SPEC);
        }

        array_spec_list[array_spec_idx].lower = array_type_get_array_lower_bound(t);
        array_spec_list[array_spec_idx].upper = array_type_get_array_lower_bound(t);

        t = array_type_get_element_type(t);
    }

    char is_array = (array_spec_idx != (MAX_ARRAY_SPEC - 1));

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

        while (array_spec_idx <= (MAX_ARRAY_SPEC - 1))
        {
            result = strappend(result, fortran_prettyprint_in_buffer(array_spec_list[array_spec_idx].lower));
            result = strappend(result, ":");
            result = strappend(result, fortran_prettyprint_in_buffer(array_spec_list[array_spec_idx].upper));
            if ((array_spec_idx + 1) != (MAX_ARRAY_SPEC - 1))
            {
                result = strappend(result, ", ");
            }
            array_spec_idx++;
        }

        result = strappend(result, ")");
    }

    return result;
}

char is_pointer_to_array(type_t* t)
{
    return (is_pointer_type(t)
            && is_array_type(pointer_type_get_pointee_type(t)));
}

int get_rank_of_type(type_t* t)
{
    if (!is_array_type(t)
            && !is_pointer_to_array(t))
        return 0;

    if (is_pointer_to_array(t))
    {
        t = pointer_type_get_pointee_type(t);
    }

    int result = 0;
    while (is_array_type(t))
    {
        result++;
        t = array_type_get_element_type(t);
    }

    return result;
}

type_t* get_rank0_type(type_t* t)
{
    while (is_array_type(t))
    {
        t = array_type_get_element_type(t);
    }
    return t;
}

