/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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



#ifndef CXX_PRETTYPRINT_INTERNAL_H
#define CXX_PRETTYPRINT_INTERNAL_H

#if !defined(CXX_PRETTYPRINT_C) && !defined(FORTRAN_PRETTYPRINT_C)
  #error Wrongly included file. Include it only from cxx-prettyprint.c or fortran-prettyprint.c
#endif

typedef struct prettyprint_context_tag
{
    int level;
    const char *indent_str;
    char internal_output;
    prettyprint_callback_t callback;
    void *callback_data;

    // Angular brackets are so troublesome in C++
    char last_is_left_angle;
    char last_is_right_angle;

    // Column
    int column;
} prettyprint_context_t;

typedef
struct prettyprint_behaviour_tag
{
    // States if the output is meant to be internal
    // i.e.: comments and preprocessor tokens will be output with special
    // keeping marks or will be converted to standard syntax
    char internal_output;
} prettyprint_behaviour_t;


typedef void (*prettyprint_handler_t)(FILE* f, AST a, prettyprint_context_t* pt_ctx);

typedef struct {
    char* handler_name;
    prettyprint_handler_t handler;
    char* parameter;
} prettyprint_entry_t;

#define HANDLER_PROTOTYPE(name) \
 static void name(FILE* f, AST a, prettyprint_context_t* pt_ctx)

#define NODE_HANDLER(type, handler, parameter) \
  [type] = {#handler, handler, parameter}

// Initial behaviour
static prettyprint_behaviour_t prettyprint_behaviour = 
{ 
    /* .internal_output = */ 1 
};

void prettyprint_set_not_internal_output(void)
{
    prettyprint_behaviour.internal_output = 0;
}

void prettyprint_set_internal_output(void)
{
    prettyprint_behaviour.internal_output = 1;
}

static void prettyprint_context_init(prettyprint_context_t* pt_ctx)
{
    memset(pt_ctx, 0, sizeof(*pt_ctx));
    // This will be configurable one day
    pt_ctx->indent_str = "    ";
    pt_ctx->level = 0;
    pt_ctx->internal_output = prettyprint_behaviour.internal_output;
}

static void prettyprint_context_copy(prettyprint_context_t* dest,
        const prettyprint_context_t* src)
{
    // Nothing else must be done
    memcpy(dest, src, sizeof(*dest));
}


#define NEW_PT_CONTEXT(_name, _init_fun) \
    prettyprint_context_t _v_##_name; \
    prettyprint_context_t *_name = &_v_##_name; \
    prettyprint_context_copy(_name, pt_ctx); \
    _init_fun(_name)

#define NEW_PT_CONTEXT_ARG(_name, _init_fun, _arg) \
    prettyprint_context_t _v_##_name; \
    prettyprint_context_t *_name = &_v_##_name; \
    prettyprint_context_copy(_name, pt_ctx); \
    _init_fun(_name, _arg)

static void prettyprint_level(FILE* f, AST a, prettyprint_context_t* pt_ctx);

#define HELPER_PARAMETER \
    (handlers_list[ASTKind(a)].parameter)

#define HELPER_PARAMETER_STRING \
    ((handlers_list[ASTKind(a)].parameter != NULL) ? (handlers_list[ASTKind(a)].parameter) : "")


extern const char* prettyprint_in_buffer_common(AST a, 
        void (*pretty_func)(FILE*, AST, prettyprint_context_t* pt_ctx), 
        prettyprint_context_t *pt_ctx);
extern int character_level_vfprintf(FILE* stream,
        prettyprint_context_t *pt_ctx,
        const char* format, va_list args);
extern int token_fprintf(FILE *stream, AST node UNUSED_PARAMETER, prettyprint_context_t* pt_ctx, const char *format, ...);


#endif // CXX_PRETTYPRINT_INTERNAL_H
