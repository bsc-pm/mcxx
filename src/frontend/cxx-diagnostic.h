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

#ifndef CXX_DIAGNOSTIC_H
#define CXX_DIAGNOSTIC_H

#include "cxx-macros.h"
#include "cxx-locus.h"

MCXX_BEGIN_DECLS

struct diagnostic_context_tag;
typedef struct diagnostic_context_tag diagnostic_context_t;

void diagnostics_reset(void);
int diagnostics_get_error_count(void);
int diagnostics_get_warn_count(void);

void error_printf_at(const locus_t*, const char* format, ...) CHECK_PRINTF(2,3);
void warn_printf_at(const locus_t*, const char* format, ...)  CHECK_PRINTF(2,3);
void info_printf_at(const locus_t*, const char* format, ...)  CHECK_PRINTF(2,3);
void warn_or_error_printf_at(const locus_t*, char emit_error, const char* format, ...)  CHECK_PRINTF(3,4);
void fatal_printf_at(const locus_t*, const char* format, ...) NORETURN CHECK_PRINTF(2, 3);
void sorry_printf_at(const locus_t*, const char* format, ...) NORETURN CHECK_PRINTF(2, 3);

// Change diagnosting context

diagnostic_context_t* diagnostic_context_get_current(void);

void diagnostic_context_push(diagnostic_context_t*);
void diagnostic_context_pop(void);

void diagnostic_context_commit(diagnostic_context_t*);
void diagnostic_context_discard(diagnostic_context_t*);

diagnostic_context_t* diagnostic_context_new_buffered(void);

diagnostic_context_t* diagnostic_context_push_buffered(void);
void diagnostic_context_pop_and_discard(void);
void diagnostic_context_pop_and_commit(void);

typedef struct header_message_fun_tag header_message_fun_t;
struct header_message_fun_tag
{
    const char* (*message_fun)(void*);
    void *data;
};

diagnostic_context_t* diagnostic_context_new_instantiation(header_message_fun_t);
diagnostic_context_t* diagnostic_context_push_instantiation(header_message_fun_t);

MCXX_END_DECLS

#endif // CXX_DIAGNOSTIC_H
