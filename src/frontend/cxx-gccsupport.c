/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
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
#include "cxx-exprtype.h"
#include "cxx-attrnames.h"
#include "cxx-tltype.h"

/*
 * Very specific bits of gcc support should be in this file
 */

static char fix_gather_type_to_match_mode(gather_decl_spec_t* gather_info, 
        char floating,
        _size_t bytes)
{
    type_t* signed_0_integral_types[] =
    {
        // char is always making things hard
        get_char_type(),
        get_signed_short_int_type(),
        get_signed_int_type(),
        get_signed_long_int_type(),
        get_signed_long_long_int_type(),
        NULL,
    };

    type_t* signed_integral_types[] =
    {
        get_signed_char_type(),
        get_signed_short_int_type(),
        get_signed_int_type(),
        get_signed_long_int_type(),
        get_signed_long_long_int_type(),
        NULL,
    };

    type_t* unsigned_integral_types[] =
    {
        get_unsigned_char_type(),
        get_unsigned_short_int_type(),
        get_unsigned_int_type(),
        get_unsigned_long_int_type(),
        get_unsigned_long_long_int_type(),
        NULL,
    };

    type_t* float_types[] =
    {
        get_float_type(),
        get_double_type(),
        get_long_double_type(),
        NULL,
    };

    type_t** types = signed_0_integral_types;

    if (floating)
        types = float_types;
    else if (gather_info->is_unsigned)
        types = unsigned_integral_types;
    else if (gather_info->is_signed)
        types = signed_integral_types;

    char match_found = 0;
    type_t* match_type = NULL;

    int i = 0;
    while (types[i] != NULL
            && !match_found)
    {
        if (type_get_size(types[i]) == bytes)
        {
            match_found = 1;
            match_type = types[i];
        }
        i++;
    }

    if (match_found)
    {
        gather_info->is_overriden_type = 1;
        gather_info->mode_type = match_type;
    }

    return match_found;
}

static void gather_one_gcc_attribute(const char* attribute_name,
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
                    ast_location(expression_list));
        }

        // Evaluate the expression
        AST argument = ASTSon1(expression_list);
        if (check_for_expression(argument, decl_context))
        {
            if (!is_dependent_expression(argument, decl_context))
            {
                if (is_constant_expression(argument, decl_context))
                {
                    literal_value_t literal_value = evaluate_constant_expression(argument, decl_context);
                    char is_valid = 0;
                    int vector_size = literal_value_to_uint(literal_value, &is_valid);

                    if (is_valid)
                    {
                        gather_info->vector_size = vector_size;
                        gather_info->is_vector = 1;
                    }
                    else
                    {
                        // This is maybe overly defensive
                        internal_error("unreachable code", 0);
                    }
                }
                else
                {
                    fprintf(stderr, "%s: warning: ignoring attribute 'vector_size' since the expression is not constant\n",
                            ast_location(expression_list));
                }
            }
        }
        else
        {
            fprintf(stderr, "%s: warning: ignoring attribute 'vector_size' since the expression is not valid\n",
                    ast_location(expression_list));
        }
    }
    else if (strcmp(attribute_name, "spu_vector") == 0)
    {
        // Hardcoded to what a SPU can do
        gather_info->is_vector = 1;
        gather_info->vector_size = 16;
    }
    else if (strcmp(attribute_name, "altivec") == 0)
    {
        AST argument = advance_expression_nest(ASTSon1(expression_list));
        if (ASTType(argument) == AST_SYMBOL)
        {
            const char *argument_text = ASTText(argument);
            if (strcmp(argument_text, "vector__") == 0)
            {
                // Hardcoded to what an Altivec unit can do
                // that (oh, miracle!) is the same as a SPU
                gather_info->is_vector = 1;
                gather_info->vector_size = 16;
            }
        }
    }
    else if (strcmp(attribute_name, "mode") == 0
            || strcmp(attribute_name, "__mode__") == 0)
    {
        if (ASTSon0(expression_list) != NULL)
        {
            running_error("%s: error: attribute 'mode' only allows one argument",
                    ast_location(expression_list));
        }

        AST argument = advance_expression_nest(ASTSon1(expression_list));

        char ignored = 0;
        if (ASTType(argument) == AST_SYMBOL)
        {
            const char *size_mode = ASTText(argument);

            // FIXME - Can a vector mode start with two underscores ?
            if (size_mode[0] != 'V')
            {
                // Do nothing if we don't do sizeof
                if (!CURRENT_CONFIGURATION->disable_sizeof)
                {
                    /*
                       QI - An integer that is as wide as the smallest addressable unit, usually 8 bits.
                       HI - An integer, twice as wide as a QI mode integer, usually 16 bits.
                       SI - An integer, four times as wide as a QI mode integer, usually 32 bits.
                       DI - An integer, eight times as wide as a QI mode integer, usually 64 bits.
                       SF - A floating point value, as wide as a SI mode integer, usually 32 bits.
                       DF - A floating point value, as wide as a DI mode integer, usually 64 bits. 
                     */
                    struct 
                    {
                        const char* mode_name;
                        char floating;
                        _size_t bytes;
                    } mode_list[] =
                    {
                        // Integral types
                        { "QI",     0, 1 }, 
                        { "__QI__", 0, 1 }, 
                        { "HI",     0, 2 },
                        { "__HI__", 0, 2 },
                        { "SI",     0, 4 },
                        { "__SI__", 0, 4 },
                        { "DI",     0, 8 },
                        { "__DI__", 0, 8 },
                        // Floating types
                        { "SF",     1, 4 },
                        { "__SF__", 1, 4 },
                        { "DF",     1, 8 },
                        { "__DF__", 1, 8 },
                    };

                    char found = 0;

                    if (strcmp(size_mode, "__pointer__") == 0
                        || strcmp(size_mode, "pointer") == 0)
                    {
                        fix_gather_type_to_match_mode(gather_info,
                                /* floating */ 0, CURRENT_CONFIGURATION->type_environment->sizeof_pointer);
                        found = 1;
                    }
                    else if (strcmp(size_mode, "__word__") == 0
                            || strcmp(size_mode, "word") == 0)
                    {
                        // what is word mode??? At the moment use the size of a long
                        // since it matches what gcc does
                        fix_gather_type_to_match_mode(gather_info,
                                /* floating */ 0, CURRENT_CONFIGURATION->type_environment->sizeof_signed_long);
                        found = 1;
                    }
                    else if (strcmp(size_mode, "__byte__") == 0
                        || strcmp(size_mode, "byte") == 0)
                    {
                        fix_gather_type_to_match_mode(gather_info,
                                /* floating */ 0, /* 1 byte */ 1);
                        found = 1;
                    }

                    // Find in the table above if not found yet
                    int i;
                    int max_mode = STATIC_ARRAY_LENGTH(mode_list);
                    for (i = 0; i < max_mode && !found; i++)
                    {
                        if (strcmp(size_mode, mode_list[i].mode_name) == 0)
                        {
                            found = 1;
                            fix_gather_type_to_match_mode(gather_info, 
                                    mode_list[i].floating, mode_list[i].bytes);
                        }
                    }

                    if (!found)
                    {
                        ignored = 1;
                    }
                }
            }
            else
            {
                fprintf(stderr, "%s: warning: attribute 'mode' is deprecated better use 'vector_size'\n", 
                        ast_location(expression_list));

                char number_of_elements_str[256];
                strncpy(number_of_elements_str, &(size_mode[1]), 255);
                number_of_elements_str[255] = '\0';
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
                                    gather_info->mode_type = get_signed_char_type();
                                }
                                else if (size == 'H')
                                {
                                    gather_info->mode_type = get_signed_short_int_type();
                                }
                                else if (size == 'S')
                                {
                                    gather_info->mode_type = get_signed_int_type();
                                }
                                else if (size == 'D')
                                {
                                    gather_info->mode_type = get_signed_long_long_int_type();
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
                                    gather_info->mode_type = get_float_type();
                                }
                                else if (size == 'D')
                                {
                                    gather_info->mode_type = get_double_type();
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
                                    * get_sizeof_type(gather_info->mode_type);
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
                    ast_location(expression_list));
        }
    }

    // Save it in the gather_info structure
    if (gather_info->num_gcc_attributes == MAX_GCC_ATTRIBUTES_PER_SYMBOL)
    {
        running_error("Too many gcc attributes, maximum supported is %d\n", MAX_GCC_ATTRIBUTES_PER_SYMBOL);
    }
    gather_gcc_attribute_t* current_gcc_attribute = &(gather_info->gcc_attributes[gather_info->num_gcc_attributes]);
    gather_info->num_gcc_attributes++;

    current_gcc_attribute->attribute_name = uniquestr(attribute_name);
    current_gcc_attribute->expression_list = expression_list;
}

void gather_gcc_attribute(AST attribute, 
        gather_decl_spec_t* gather_info, 
        decl_context_t decl_context)
{
    ERROR_CONDITION(ASTType(attribute) != AST_GCC_ATTRIBUTE,
            "Invalid node", 0);
    AST iter;
    AST list = ASTSon0(attribute);

    ASTAttrSetValueType(attribute, LANG_IS_GCC_ATTRIBUTE, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(attribute, LANG_GCC_ATTRIBUTE_LIST, tl_type_t, tl_ast(list));

    if (list != NULL)
    {
        for_each_element(list, iter)
        {
            AST gcc_attribute_expr = ASTSon1(iter);

            AST identif = ASTSon0(gcc_attribute_expr);
            AST expression_list = ASTSon2(gcc_attribute_expr);

            const char *attribute_name = ASTText(identif);

            ASTAttrSetValueType(gcc_attribute_expr, LANG_IS_GCC_ATTRIBUTE_VALUE, tl_type_t, tl_bool(1));
            ASTAttrSetValueType(gcc_attribute_expr, LANG_GCC_ATTRIBUTE_VALUE_NAME, tl_type_t, tl_ast(identif));
            ASTAttrSetValueType(gcc_attribute_expr, LANG_GCC_ATTRIBUTE_VALUE_ARGS, tl_type_t, tl_ast(expression_list));

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
