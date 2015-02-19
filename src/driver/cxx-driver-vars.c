/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information
  regarding developers and contributors.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/



#include "cxx-driver-decls.h"

typedef
struct expanded_var_tag
{
    const char* var_name;
    const char* (*expander)(translation_unit_t*);
} expanded_var_t;

static const char* expander_output(translation_unit_t* t)
{
    return t->output_filename;
}

static const char* expander_output(translation_unit_t* t)
{
    return t->output_filename;
}

static const char* expander_input(translation_unit_t* t)
{
    return t->input_filename;
}

static expanded_var_t expanded_var_list[] =
{
    // Keep this list sorted
    { "$OUTPUT", expander_output },
    { "$INPUT", expander_input },
    { "$PP_OUTPUT", NULL },
    { "$EXE_OUTPUT", NULL },
    { "
};

static char is_valid_var_name(const char* str)
{
    return 0;
}

static const char* smart_append(const char* str, const char* app)
{
    if (str == NULL)
        return app;
    else if (app == NULL)
        return str;
    else 
        return strappend(str, app);
}

static const char* expand_string(const char* value, translation_unit_t* translation_unit)
{
    const char* output = NULL;

    const char *p = value, *q = value;

    while (*p != '\0')
    {
        if (*p == '$')
        {
            const char *k = p + 1;

            while (*k != '\0' 
                    && *k != '\t'
                    && *k != ' ')
            {
                k++;
            }

            if (k > (p + 1))
            {
                int num_chars = k - p;
                int length = num_chars + 1;
                char var_name[length];
                strncpy(var_name, p, num_chars);
                var_name[num_chars] = '\0';

                if (is_valid_var_name(var_name, translation_unit))
                {
                    // From q to p we have a non expanded string

                    num_chars = p - q;
                    length = num_chars + 1;

                    if (num_chars > 0)
                    {
                        char to_append[length];
                        strncpy(to_append, q, num_chars);
                        to_append[num_chars] = '\0';

                        output = smart_append(output, to_append);
                    }

                    const char* var_value = get_value_var_name(var_name);

                    output = smart_append(output, var_value, translation_unit);

                    p += num_chars;
                    q = p;
                }
            }
        }

        p++;
    }

    if (q != p)
    {
        int num_chars = p - q;
        int length = num_chars + 1;

        char to_append[length];
        strncpy(to_append, q, num_chars);
        to_append[num_chars] = '\0';

        output = smart_append(output, to_append);
    }

    return output;
}
