#ifndef CXX_DIAGNOSTIC_H
#define CXX_DIAGNOSTIC_H

#include "cxx-macros.h"

MCXX_BEGIN_DECLS

void diagnostics_reset(void);
int diagnostics_get_error_count(void);
int diagnostics_get_warn_count(void);

void error_printf(const char* format, ...) __attribute__((format(gnu_printf, 1, 2)));
void warn_printf(const char* format, ...) __attribute__((format(gnu_printf, 1, 2)));
void info_printf(const char* format, ...) __attribute__((format(gnu_printf, 1, 2)));

MCXX_END_DECLS

#endif // CXX_DIAGNOSTIC_H
