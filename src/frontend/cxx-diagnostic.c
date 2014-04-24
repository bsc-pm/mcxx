/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#include "cxx-diagnostic.h"
#include "cxx-process.h"
#include "cxx-driver-decls.h"
#include <stdio.h>
#include <stdarg.h>
#include <signal.h>

static int error_count = 0;
static int warn_count = 0;

void diagnostics_reset(void)
{
    error_count = 0;
    warn_count = 0;
}

int diagnostics_get_error_count(void)
{
    return error_count;
}

int diagnostics_get_warn_count(void)
{
    return warn_count;
}

void error_printf(const char* format, ...)
{
    va_list va;
    va_start(va, format);
    vfprintf(stderr, format, va);
    va_end(va);
    error_count++;

    if (CURRENT_CONFIGURATION->debug_options.print_nodecl_graphviz)
        ast_dump_graphviz(CURRENT_COMPILED_FILE->parsed_tree, stderr);

    if (CURRENT_CONFIGURATION->debug_options.abort_on_ice)
        raise(SIGABRT);
}

void warn_printf(const char* format, ...)
{
    va_list va;
    va_start(va, format);
    vfprintf(stderr, format, va);
    va_end(va);
    warn_count++;
}

void info_printf(const char* format, ...)
{
    va_list va;
    va_start(va, format);
    vfprintf(stderr, format, va);
    va_end(va);
}

void warn_or_error_printf(char emit_error, const char* format, ...)
{
    va_list va;
    va_start(va, format);
    vfprintf(stderr, format, va);
    va_end(va);
    if (emit_error)
    {
        error_count++;

        if (CURRENT_CONFIGURATION->debug_options.abort_on_ice)
            raise(SIGABRT);
    }
    else
    {
        warn_count++;
    }
}
