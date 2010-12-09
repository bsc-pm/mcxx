#ifndef FORTRAN03_LEXER_H
#define FORTRAN03_LEXER_H

#include "cxx-lexer.h"

extern struct scan_file_descriptor* fortran_scanning_now;

extern int mf03_open_file_for_scanning(const char* scanned_filename, const char* input_filename);
extern int mf03_prepare_string_for_scanning(const char* str);


#endif // FORTRAN03_LEXER_H
