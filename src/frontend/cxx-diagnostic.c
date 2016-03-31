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

static int max_diagnostic_contexts = 0;
static int current_diagnostic_idx = 0;
static diagnostic_context_t** diagnostic_stack = 0;
#define current_diagnostic_context (diagnostic_stack[current_diagnostic_idx])

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
    fputs(message, stderr);

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
            if (debug_options.abort_on_ice)
                raise(SIGABRT);
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

char same_diag_pair(struct severity_message_pair_tag m1, struct severity_message_pair_tag m2)
{
    return (m1.severity == m2.severity
            && m1.message == m2.message);
}


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
    int prev = ctx->num_diagnostics;
    P_LIST_ADD_ONCE_FUN(ctx->diagnostics, ctx->num_diagnostics, pair, same_diag_pair);

    if (prev < ctx->num_diagnostics)
    {
        // Was it actually added?
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
    DELETE(ctx->diagnostics);
    DELETE(ctx);
}

static void diagnose_to_buffer_commit(diagnostic_context_buffered_t* ctx, diagnostic_context_t* dest)
{
    int i;
    for (i = 0; i < ctx->num_diagnostics; i++)
    {
        (dest->diagnose)(dest, ctx->diagnostics[i].severity, ctx->diagnostics[i].message);
    }
    DELETE(ctx->diagnostics);
    DELETE(ctx);
}

diagnostic_context_t* diagnostic_context_new_buffered(void)
{
    diagnostic_context_buffered_t *result = NEW0(diagnostic_context_buffered_t);

    result->_base.diagnose = (diagnose_fun_t)diagnose_to_buffer;
    result->_base.get_count = (get_count_fun_t)diagnose_to_buffer_count;
    result->_base.discard = (discard_fun_t)diagnose_to_buffer_discard;
    result->_base.commit = (commit_fun_t)diagnose_to_buffer_commit;

    return (diagnostic_context_t*)result;
}

//
// Diagnose for instantiation
//

typedef struct diagnostic_buffered_instantiation_tag
diagnostic_context_buffered_instantiation_t;

struct diagnostic_buffered_instantiation_tag
{
    diagnostic_context_buffered_t _base;
    header_message_fun_t header_message_fun;
};

static void diagnose_to_buffer_instantiation_commit(diagnostic_context_buffered_instantiation_t* ctx, diagnostic_context_t* dest)
{
    diagnostic_context_buffered_t* buffered_ctx = (diagnostic_context_buffered_t*)ctx;
    if (buffered_ctx->num_diagnostics > 0)
    {
        const char* header_message = (ctx->header_message_fun.message_fun)(ctx->header_message_fun.data);

        // Create a big message
        size_t len = 0;
        len += strlen(header_message);

        diagnostic_severity_t severity = DS_INFO;
        int i;
        for (i = 0; i < buffered_ctx->num_diagnostics; i++)
        {
            len += strlen(buffered_ctx->diagnostics[i].message);
            severity = severity < buffered_ctx->diagnostics[i].severity
                ?  buffered_ctx->diagnostics[i].severity
                : severity;
        }

        // Final NULL
        len += 1;

        char* merged_message = NEW_VEC(char, len);
        merged_message[0] = '\0';

        merged_message = strcat(merged_message, header_message);

        for (i = 0; i < buffered_ctx->num_diagnostics; i++)
        {
            merged_message = strcat(merged_message, buffered_ctx->diagnostics[i].message);
        }

        merged_message[len - 1] = '\0';

        const char* unique_merged_message = uniquestr(merged_message);
        DELETE(merged_message);

        (dest->diagnose)(dest, severity, unique_merged_message);
    }

    DELETE(ctx->header_message_fun.data);
    DELETE(buffered_ctx->diagnostics);
    DELETE(ctx);
}

diagnostic_context_t* diagnostic_context_new_instantiation(header_message_fun_t header_message_fun)
{
    diagnostic_context_buffered_instantiation_t *result = NEW0(diagnostic_context_buffered_instantiation_t);

    result->header_message_fun = header_message_fun;
    result->_base._base.diagnose = (diagnose_fun_t)diagnose_to_buffer;
    result->_base._base.get_count = (get_count_fun_t)diagnose_to_buffer_count;
    result->_base._base.discard = (discard_fun_t)diagnose_to_buffer_discard;
    result->_base._base.commit = (commit_fun_t)diagnose_to_buffer_instantiation_commit;

    return (diagnostic_context_t*)result;
}

diagnostic_context_t* diagnostic_context_push_instantiation(header_message_fun_t header_message)
{
    diagnostic_context_t* ctx = diagnostic_context_new_instantiation(header_message);
    diagnostic_context_push(ctx);

    return ctx;
}

//
// Diagnostic context manipulation
//

void diagnostic_context_discard(diagnostic_context_t* ctx)
{
    ERROR_CONDITION(ctx == current_diagnostic_context, "Trying to discard the current context", 0);
    (ctx->discard)(ctx);
}

void diagnostic_context_commit(diagnostic_context_t* ctx)
{
    ERROR_CONDITION(ctx == current_diagnostic_context, "Trying to commit the current context to itself", 0);
    (ctx->commit)(ctx, current_diagnostic_context);
}

void diagnostic_context_push(diagnostic_context_t* ctx)
{
    current_diagnostic_idx++;

    if (current_diagnostic_idx == max_diagnostic_contexts)
    {
        max_diagnostic_contexts *= 2;
        diagnostic_stack = NEW_REALLOC(diagnostic_context_t*, diagnostic_stack, max_diagnostic_contexts);
    }

    diagnostic_stack[current_diagnostic_idx] = ctx;
}

void diagnostic_context_pop(void)
{
    current_diagnostic_idx--;
    ERROR_CONDITION(current_diagnostic_idx < 0, "Underflow of diagnostic contexts", 0);
}

diagnostic_context_t* diagnostic_context_push_buffered(void)
{
    diagnostic_context_t *ctx = diagnostic_context_new_buffered();
    diagnostic_context_push(ctx);

    return ctx;
}

void diagnostic_context_pop_and_discard()
{
    diagnostic_context_t* ctx = current_diagnostic_context;
    diagnostic_context_pop();
    diagnostic_context_discard(ctx);
}

void diagnostic_context_pop_and_commit()
{
    diagnostic_context_t* ctx = current_diagnostic_context;
    diagnostic_context_pop();
    diagnostic_context_commit(ctx);
}

void diagnostics_reset(void)
{
    DELETE(diagnostic_stack);

    max_diagnostic_contexts = 4;
    current_diagnostic_idx = 0;
    diagnostic_stack = NEW_VEC0(diagnostic_context_t*, max_diagnostic_contexts);

    diagnostic_stack[current_diagnostic_idx] = (diagnostic_context_t*)&diagnostic_context_stderr;

    current_diagnostic_context->num_info
        = current_diagnostic_context->num_warning
        = current_diagnostic_context->num_error
        = 0;
}

extern inline diagnostic_context_t* diagnostic_context_get_current(void)
{
    return current_diagnostic_context;
}

int diagnostics_get_error_count(void)
{
    return (current_diagnostic_context->get_count)(current_diagnostic_context, DS_ERROR);
}

int diagnostics_get_warn_count(void)
{
    return (current_diagnostic_context->get_count)(current_diagnostic_context, DS_WARNING);
}

//
// Generic interface
//

void error_printf_at(const locus_t* locus, const char* format, ...)
{
    va_list va;
    va_start(va, format);
    const char* message = NULL;
    uniquestr_vsprintf(&message, format, va);
    va_end(va);

    if (locus != NULL)
    {
        uniquestr_sprintf(&message, "%s: error: %s",
                locus_to_str(locus),
                message);
    }
    else
    {
        uniquestr_sprintf(&message, "error: %s", message);
    }

    (current_diagnostic_context->diagnose)(current_diagnostic_context, DS_ERROR, message);
}

void warn_printf_at(const locus_t* locus, const char* format, ...)
{
    va_list va;
    va_start(va, format);
    const char* message = NULL;
    uniquestr_vsprintf(&message, format, va);
    va_end(va);

    if (locus != NULL)
    {
        uniquestr_sprintf(&message, "%s: warning: %s",
                locus_to_str(locus),
                message);
    }
    else
    {
        uniquestr_sprintf(&message, "warning: %s", message);
    }

    (current_diagnostic_context->diagnose)(current_diagnostic_context, DS_WARNING, message);
}

void info_printf_at(const locus_t* locus, const char* format, ...)
{
    va_list va;
    va_start(va, format);
    const char* message = NULL;
    uniquestr_vsprintf(&message, format, va);
    va_end(va);

    if (locus != NULL)
    {
        uniquestr_sprintf(&message, "%s: info: %s",
                locus_to_str(locus),
                message);
    }
    else
    {
        uniquestr_sprintf(&message, "info: %s", message);
    }

    (current_diagnostic_context->diagnose)(current_diagnostic_context, DS_INFO, message);
}

void warn_or_error_printf_at(const locus_t* locus, char emit_error, const char* format, ...)
{
    va_list va;
    va_start(va, format);
    const char* message = NULL;
    uniquestr_vsprintf(&message, format, va);
    va_end(va);

    diagnostic_severity_t severity = DS_WARNING;
    if (emit_error)
        severity = DS_ERROR;

    const char* kind_message = "warning";
    if (emit_error)
        kind_message = "error";

    if (locus != NULL)
    {
        uniquestr_sprintf(&message, "%s: %s: %s",
                locus_to_str(locus),
                kind_message,
                message);
    }
    else
    {
        uniquestr_sprintf(&message, "%s: %s",
                kind_message,
                message);
    }

    (current_diagnostic_context->diagnose)(current_diagnostic_context, severity, message);
}

void fatal_printf_at(const locus_t* locus, const char* format, ...)
{
    va_list va;
    va_start(va, format);
    const char* message = NULL;
    uniquestr_vsprintf(&message, format, va);
    va_end(va);

    if (locus != NULL)
    {
        uniquestr_sprintf(&message, "%s: fatal: %s",
                locus_to_str(locus),
                message);
    }
    else
    {
        uniquestr_sprintf(&message, "fatal: %s", message);
    }

    fatal_error("%s", message);
}

void sorry_printf_at(const locus_t* locus, const char* format, ...)
{
    va_list va;
    va_start(va, format);
    const char* message = NULL;
    uniquestr_vsprintf(&message, format, va);
    va_end(va);

    if (locus != NULL)
    {
        uniquestr_sprintf(&message, "%s: sorry: %s",
                locus_to_str(locus),
                message);
    }
    else
    {
        uniquestr_sprintf(&message, "sorry: %s", message);
    }

    fatal_error("%s", message);
}
