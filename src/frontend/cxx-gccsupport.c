#include <string.h>
#include <ctype.h>

#include "cxx-ast.h"
#include "cxx-scope.h"
#include "cxx-buildscope.h"
#include "cxx-gccsupport.h"
#include "cxx-utils.h"
#include "cxx-cexpr.h"
#include "cxx-typeutils.h"
#include "cxx-ambiguity.h"

/*
 * Very specific bits of gcc support should be in this file
 */

static void gather_one_gcc_attribute(char* attribute_name,
        AST expression_list,
        gather_decl_spec_t* gather_info,
        decl_context_t decl_context)
{
    /*
     * Vector support
     */
    if (strcmp(attribute_name, "vector_size") == 0)
    {
        if (ASTSon0(expression_list) != NULL)
        {
            running_error("%s: error: attribute 'vector_size' only allows one argument",
                    node_information(expression_list));
        }

        AST argument = advance_expression_nest(ASTSon1(expression_list));
        if (ASTType(argument) == AST_DECIMAL_LITERAL)
        {
            literal_value_t literal_value = 
                evaluate_constant_expression(argument, decl_context);
            unsigned int vector_size = literal_value_to_uint(literal_value);

            gather_info->vector_size = vector_size;
            gather_info->is_vector = 1;
        }
        else
        {
            fprintf(stderr, "%s: warning: ignoring attribute 'vector_size'\n",
                    node_information(expression_list));
        }
    }
    else if (strcmp(attribute_name, "mode") == 0)
    {
        if (ASTSon0(expression_list) != NULL)
        {
            running_error("%s: error: attribute 'vector_size' only allows one argument",
                    node_information(expression_list));
        }

        AST argument = advance_expression_nest(ASTSon1(expression_list));

        char ignored = 0;
        if (ASTType(argument) == AST_SYMBOL)
        {
            char *vector_mode = ASTText(argument);

            if (vector_mode[0] != 'V')
            {
                ignored = 1;
            }
            else
            {
                char *number_of_elements_str = strdup(&(vector_mode[1]));
                char *p = number_of_elements_str;

                while (isdigit(*p))
                {
                    p++;
                }

                if (*p == '\0')
                {
                    ignored = 1;
                }
                else
                {
                    char size;
                    char nature;

                    size = *p;

                    // End the number_of_elements_str here
                    *p = '\0';

                    p++;


                    if ((*p == '\0') || 
                            (p == number_of_elements_str))
                    {
                        ignored = 1;
                    }
                    else
                    {
                        nature = *p;
                        p++;

                        int num_elements = atoi(number_of_elements_str);

                        /*
                           From gcc documentation: 

                           QI - An integer that is as wide as the smallest addressable unit, usually 8 bits.
                           HI - An integer, twice as wide as a QI mode integer, usually 16 bits.
                           SI - An integer, four times as wide as a QI mode integer, usually 32 bits.
                           DI - An integer, eight times as wide as a QI mode integer, usually 64 bits.
                           SF - A floating point value, as wide as a SI mode integer, usually 32 bits.
                           DF - A floating point value, as wide as a DI mode integer, usually 64 bits. 
                         */
                        if (*p != '\0')
                        {
                            ignored = 1;
                        }
                        else
                        {
                            if (nature == 'I')
                            {
                                if (size == 'Q')
                                {
                                    gather_info->vector_mode_type = get_signed_char_type();
                                }
                                else if (size == 'H')
                                {
                                    gather_info->vector_mode_type = get_signed_short_int_type();
                                }
                                else if (size == 'S')
                                {
                                    gather_info->vector_mode_type = get_signed_int_type();
                                }
                                else if (size == 'D')
                                {
                                    gather_info->vector_mode_type = get_signed_long_long_int_type();
                                }
                                else
                                {
                                    ignored = 1;
                                }
                            }
                            else if (nature == 'F')
                            {
                                if (size == 'S')
                                {
                                    gather_info->vector_mode_type = get_float_type();
                                }
                                else if (size == 'D')
                                {
                                    gather_info->vector_mode_type = get_double_type();
                                }
                                else
                                {
                                    ignored = 1;
                                }
                            }
                            else
                            {
                                ignored = 1;
                            }

                            if (!ignored)
                            {
                                gather_info->vector_size = num_elements
                                    * get_sizeof_type(gather_info->vector_mode_type);
                                gather_info->is_vector = 1;
                            }
                        }
                    }
                }
            }
        }
        else
        {
            ignored = 1;
        }

        if (ignored)
        {
            fprintf(stderr, "%s: warning: ignoring attribute 'mode'\n",
                    node_information(expression_list));
        }
        else
        {
            fprintf(stderr, "%s: warning: attribute 'mode' is deprecated better use 'vector_size'\n", 
                    node_information(expression_list));
        }
    }
    else 
    {
        // Do nothing
    }
}

void gather_gcc_attribute(AST attribute, 
        gather_decl_spec_t* gather_info, 
        decl_context_t decl_context)
{
    ERROR_CONDITION(ASTType(attribute) != AST_GCC_ATTRIBUTE,
            "This cannot be NULL", 0);
    // Remove any ambiguity lurking there
    // but they are ignored
    AST iter;
    AST list = ASTSon0(attribute);

    if (list != NULL)
    {
        for_each_element(list, iter)
        {
            AST gcc_attribute_expr = ASTSon1(iter);

            AST expression_list = ASTSon2(gcc_attribute_expr);
            if (expression_list != NULL)
            {
                AST iter2;

                for_each_element(expression_list, iter2)
                {
                    AST expression = ASTSon1(iter2);
                    solve_possibly_ambiguous_expression(expression, decl_context);
                }
            }

            // This might be gperf-ectionated
            AST identif = ASTSon0(gcc_attribute_expr);
            char *attribute_name = ASTText(identif);

            gather_one_gcc_attribute(attribute_name, expression_list, gather_info, decl_context);
        }
    }
}

void gather_gcc_attribute_list(AST attribute_list, 
        gather_decl_spec_t *gather_info, 
        decl_context_t decl_context)
{
    ERROR_CONDITION(attribute_list == NULL,
            "This cannot be NULL", 0);
    AST iter;
    for_each_element(attribute_list, iter)
    {
        AST attribute = ASTSon1(iter);

        gather_gcc_attribute(attribute, gather_info, decl_context);
    }
}
