#include "cxx-diagnostic.h"
#include <stdio.h>
#include <stdarg.h>

void error_printf(const char* format, ...)
{
    va_list va;
    va_start(va, format);
    vfprintf(stderr, format, va);
    va_end(va);
}

void warn_printf(const char* format, ...)
{
    va_list va;
    va_start(va, format);
    vfprintf(stderr, format, va);
    va_end(va);
}

void info_printf(const char* format, ...)
{
    va_list va;
    va_start(va, format);
    vfprintf(stderr, format, va);
    va_end(va);
}
