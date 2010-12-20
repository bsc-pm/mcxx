#ifndef FORTRAN03_LEXER_H
#define FORTRAN03_LEXER_H

#include "libmf03-common.h"
#include "cxx-lexer.h"

LIBMF03_EXTERN struct scan_file_descriptor* fortran_scanning_now;

LIBMF03_EXTERN int mf03_open_file_for_scanning(const char* scanned_filename, const char* input_filename);
LIBMF03_EXTERN int mf03_prepare_string_for_scanning(const char* str);

LIBMF03_EXTERN int mf03_flex_debug;
LIBMF03_EXTERN int mf03debug;

#endif // FORTRAN03_LEXER_H
