#ifndef CXX_LEXER_H
#define CXX_LEXER_H
#include "cxx-macros.h"

MCXX_BEGIN_DECLS

typedef 
struct token_atrib_tag 
{
    char* token_text;
    int token_line;
} token_atrib_t;

#include <stdio.h>

struct scan_file_descriptor 
{
    char in_include_file;
    char* filename;

    // Current filename due to include lines
    char* current_filename;
    int line_number;
    FILE* file_descriptor;
    struct yy_buffer_state* scanning_buffer;
};

extern struct scan_file_descriptor scanning_now;

int mcxx_open_file_for_scanning(char* scanned_filename, char* input_filename);
int mc99_open_file_for_scanning(char* scanned_filename, char* input_filename);

int mcxx_prepare_string_for_scanning(const char* str);
int mc99_prepare_string_for_scanning(const char* str);

MCXX_END_DECLS

#endif // CXX_LEXER_H
