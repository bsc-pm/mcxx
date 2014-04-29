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

#define _GNU_SOURCE
#include <stdio.h>
#include <stdarg.h>
#include <signal.h>

#include "cxx-diagnostic.h"
#include "cxx-process.h"
#include "cxx-driver-decls.h"
#include "cxx-utils.h"

typedef enum diagnostic_severity_tag
{
    DS_INVALID = 0,
    DS_INFO = 1,
    DS_WARNING = 2,
    DS_ERROR = 3,
} diagnostic_severity_t;

typedef void (*diagnose_fun_t)(diagnostic_context_t*, diagnostic_severity_t, const char*);
typedef int (*get_count_fun_t)(diagnostic_context_t*, diagnostic_severity_t);
typedef void (*discard_fun_t)(diagnostic_context_t*);
typedef void (*commit_fun_t)(diagnostic_context_t*, diagnostic_context_t*);

struct diagnostic_context_tag
{
    diagnose_fun_t diagnose;
    get_count_fun_t get_count;
    discard_fun_t discard;
    commit_fun_t commit;

    // Data
    int num_info;
    int num_error;
    int num_warning;
};

static diagnostic_context_t* current_diagnostic;

//
// Diagnose to stderr
//
typedef struct diagnostic_context_stderr_tag diagnostic_context_stderr_t;

struct diagnostic_context_stderr_tag
{
    diagnostic_context_t _base;
};

static void diagnose_to_stderr(diagnostic_context_stderr_t* ctx, diagnostic_severity_t severity, const char* message)
{
    fprintf(stderr, message);
    xfree((char*)message);

    switch (severity)
    {
        case DS_INFO:
            ctx->_base.num_info++;
            break;
        case DS_WARNING:
            ctx->_base.num_warning++;
            break;
        case DS_ERROR:
            ctx->_base.num_error++;
            break;
        default:
            internal_error("Invalid severity value %d", severity);
    }
}

static int diagnose_to_stderr_count(diagnostic_context_stderr_t* ctx UNUSED_PARAMETER, diagnostic_severity_t severity)
{
    switch (severity)
    {
        case DS_INFO:
            // We do not count these currently
            return 0;
            break;
        case DS_WARNING:
            return ctx->_base.num_warning;
            break;
        case DS_ERROR:
            return ctx->_base.num_error;
            break;
        default:
            internal_error("Invalid severity value %d", severity);
    }
    return -1;
}

static void diagnose_to_stderr_discard(diagnostic_context_stderr_t* ctx UNUSED_PARAMETER)
{
    internal_error("Attempt to discard stderr diagnostic context", 0);
}

static void diagnose_to_stderr_commit(diagnostic_context_stderr_t* ctx UNUSED_PARAMETER,
        diagnostic_context_t* dest UNUSED_PARAMETER)
{
    internal_error("Attempt to commit stderr diagnostic context", 0);
}

static diagnostic_context_stderr_t diagnostic_context_stderr =
{
    ._base = {
        .diagnose = (diagnose_fun_t)diagnose_to_stderr,
        .get_count = (get_count_fun_t)diagnose_to_stderr_count,
        .discard = (discard_fun_t)diagnose_to_stderr_discard,
        .commit = (commit_fun_t)diagnose_to_stderr_commit,
        // Data
        .num_info = 0,
        .num_error = 0,
        .num_warning = 0,
    },
};

//
// Diagnose to buffer
//
struct severity_message_pair_tag
{
    diagnostic_severity_t severity;
    const char* message;
};

typedef struct diagnostic_buffered_tag diagnostic_context_buffered_t;

struct diagnostic_buffered_tag
{
    diagnostic_context_t _base;
    int num_diagnostics;
    struct severity_message_pair_tag *diagnostics;
};

static void diagnose_to_buffer(diagnostic_context_buffered_t* ctx,
        diagnostic_severity_t severity,
        const char* message)
{
    struct severity_message_pair_tag pair = { severity, message };
    P_LIST_ADD(ctx->diagnostics, ctx->num_diagnostics, pair);

    switch (severity)
    {
        case DS_INFO:
            ctx->_base.num_info++;
            break;
        case DS_WARNING:
            ctx->_base.num_warning++;
            break;
        case DS_ERROR:
            ctx->_base.num_error++;
            break;
        default:
            internal_error("Invalid severity value %d", severity);
    }
}

static int diagnose_to_buffer_count(diagnostic_context_buffered_t* ctx,
        diagnostic_severity_t severity)
{
    switch (severity)
    {
        case DS_INFO:
            return ctx->_base.num_info;
        case DS_WARNING:
            return ctx->_base.num_warning;
        case DS_ERROR:
            return ctx->_base.num_error;
        default:
            internal_error("Invalid severity value %d", severity);
    }
}

static void diagnose_to_buffer_discard(diagnostic_context_buffered_t* ctx)
{
    int i;
    for (i = 0; i < ctx->num_diagnostics; i++)
    {
        xfree((char*)ctx->diagnostics[i].message);
    }
    xfree(ctx->diagnostics);
    xfree(ctx);
}

static void diagnose_to_buffer_commit(diagnostic_context_buffered_t* ctx, diagnostic_context_t* dest)
{
    int i;
    for (i = 0; i < ctx->num_diagnostics; i++)
    {
        (dest->diagnose)(dest, ctx->diagnostics[i].severity, ctx->diagnostics[i].message);
    }
    xfree(ctx->diagnostics);
    xfree(ctx);
}

diagnostic_context_t* diagnostic_context_new_buffered(void)
{
    diagnostic_context_buffered_t *result = xcalloc(1, sizeof(*result));

    result->_base.diagnose = (diagnose_fun_t)diagnose_to_buffer;
    result->_base.get_count = (get_count_fun_t)diagnose_to_buffer_count;
    result->_base.discard = (discard_fun_t)diagnose_to_buffer_discard;
    result->_base.commit = (commit_fun_t)diagnose_to_buffer_commit;

    return (diagnostic_context_t*)result;
}

//
// Generic interface
//
void diagnostics_reset(void)
{
    current_diagnostic = (diagnostic_context_t*)&diagnostic_context_stderr;

    current_diagnostic->num_info
        = current_diagnostic->num_warning
        = current_diagnostic->num_error
        = 0;
}

diagnostic_context_t* diagnostic_context_get_current(void)
{
    return current_diagnostic;
}

int diagnostics_get_error_count(void)
{
    return (current_diagnostic->get_count)(current_diagnostic, DS_ERROR);
}

int diagnostics_get_warn_count(void)
{
    return (current_diagnostic->get_count)(current_diagnostic, DS_WARNING);
}

void error_printf(const char* format, ...)
{
    va_list va;
    va_start(va, format);
    char* message = NULL;
    vasprintf(&message, format, va);
    va_end(va);

    (current_diagnostic->diagnose)(current_diagnostic, DS_ERROR, message);
}

void warn_printf(const char* format, ...)
{
    va_list va;
    va_start(va, format);
    char* message = NULL;
    vasprintf(&message, format, va);
    va_end(va);

    (current_diagnostic->diagnose)(current_diagnostic, DS_WARNING, message);
}

void info_printf(const char* format, ...)
{
    va_list va;
    va_start(va, format);
    char* message = NULL;
    vasprintf(&message, format, va);
    va_end(va);

    (current_diagnostic->diagnose)(current_diagnostic, DS_INFO, message);
}

void warn_or_error_printf(char emit_error, const char* format, ...)
{
    va_list va;
    va_start(va, format);
    char* message = NULL;
    vasprintf(&message, format, va);
    va_end(va);

    diagnostic_severity_t severity = DS_WARNING;
    if (emit_error)
        severity = DS_ERROR;

    (current_diagnostic->diagnose)(current_diagnostic, severity, message);
}
