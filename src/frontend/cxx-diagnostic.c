#include "cxx-diagnostic.h"
#include <stdio.h>
#include <stdarg.h>

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
