/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "fortran03-new-scanner.h"

#include "cxx-process.h"
#include "cxx-utils.h"
#include "cxx-diagnostic.h"
#include "fortran03-lexer.h"
#include "fortran03-utils.h"
#include "fortran03-parser-internal.h"

#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>

#include <ctype.h>


#ifdef FORTRAN_NEW_SCANNER
#define new_mf03lex mf03lex
#define new_mf03_open_file_for_scanning mf03_open_file_for_scanning
#define new_mf03_prepare_string_for_scanning mf03_prepare_string_for_scanning
#endif // FORTRAN_NEW_SCANNER

/*
   Include stack.

   The maxim level of nesting is not defined in Fortran 95 standard.
   We have set it to 99.
   */

enum {
    MAX_INCLUDE_DEPTH = 99,
    MAX_KEYWORDS_PER_STMT = 16,
};


typedef
struct token_location_tag
{
    const char* filename;
    int line;
    int column;
} token_location_t;

struct scan_file_descriptor
{
    const char *current_pos; // position in the buffer

    const char *buffer; // scanned buffer
    size_t buffer_size; // number of characters in buffer relevant for scanning

    const char* scanned_filename;

    int fd; // if fd >= 0 this is a mmap

    token_location_t current_location;
};

enum lexer_textual_form
{
    LEXER_TEXTUAL_INVALID_FORM = 0,
    LEXER_TEXTUAL_FREE_FORM,
    LEXER_TEXTUAL_FIXED_FORM,
};

typedef
enum language_part_tag
{
    LANG_TOP_LEVEL = 0,
    LANG_NONEXECUTABLE_PART,
    LANG_EXECUTABLE_PART,
} language_part_t;

enum lexing_substate
{
    LEXER_SUBSTATE_NORMAL = 0,

    // !$OMP ...
    LEXER_SUBSTATE_PRAGMA_DIRECTIVE,
    LEXER_SUBSTATE_PRAGMA_FIRST_CLAUSE,
    LEXER_SUBSTATE_PRAGMA_CLAUSE,
    LEXER_SUBSTATE_PRAGMA_VAR_LIST,
};

typedef
struct fixed_form_state_tag
{
    // Language part: non-executable vs executable
    language_part_t language_part;

    // States that we are inside an INTERFACE block
    char in_interface_generic_spec:1;
} fixed_form_state_t;

static
struct new_lexer_state_t
{
    enum lexer_textual_form form;
    enum lexing_substate substate;

    int include_stack_size;
    struct scan_file_descriptor include_stack[MAX_INCLUDE_DEPTH];
    struct scan_file_descriptor *current_file;

    // If not null, we are scanning through a sentinel (i.e. "omp")
    char *sentinel;

    // beginning of line
    char bol:1;
    // last token was end of line (the parser does not like redundant EOS)
    char last_eos:1; 
    // states that we are inside a string-literal (changes the way we handle
    // continuations)
    char character_context:1;
    // we are scanning a format statement
    char in_format_statement:1;
    // we have seen a nonblock do construct
    char in_nonblock_do_construct:1;
    // previous character was a letter
    char previous_was_letter:1;
    // we are in a comment
    char in_comment:1;

    // Delimiter for 'a' "b" character constant
    char character_context_delim;
    // Length of Holleritz character constant
    char character_context_hollerith_length;

    // Stack of nonblock DO labels we need this to properly nest them
    int num_nonblock_labels;
    int size_nonblock_labels_stack;
    int *nonblock_labels_stack;

    // Stack of pragma constructs we need this to properly nest them when the
    // END pragma is optional
    int num_pragma_constructs;
    int size_pragma_constructs_stack;
    char** pragma_constructs_stack;

    // Extra state required by fixed form
    fixed_form_state_t fixed_form;
} lexer_state;

static void init_fixed_form(void)
{
    lexer_state.fixed_form.language_part = LANG_TOP_LEVEL;
}

static token_location_t get_current_location(void)
{
    return lexer_state.current_file->current_location;
}

#ifdef FORTRAN_NEW_SCANNER
int mf03_flex_debug = 1;
#endif

static void init_lexer_state(void);

extern int new_mf03_open_file_for_scanning(const char* scanned_filename,
        const char* input_filename,
        char is_fixed_form)
{
    int fd = open(scanned_filename, O_RDONLY);
    if (fd < 0)
    {
        fatal_error("error: cannot open file '%s' (%s)", scanned_filename, strerror(errno));
    }

    // Get size of file because we need it for the mmap
    struct stat s;
    int status = fstat (fd, &s);
    if (status < 0)
    {
        fatal_error("error: cannot get status of file '%s' (%s)", scanned_filename, strerror(errno));
    }

    const char *mmapped_addr = mmap(0, s.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
    if (mmapped_addr == MAP_FAILED)
    {
        fatal_error("error: cannot map file '%s' in memory (%s)", scanned_filename, strerror(errno));
    }

    lexer_state.form = !is_fixed_form ? LEXER_TEXTUAL_FREE_FORM : LEXER_TEXTUAL_FIXED_FORM;
    lexer_state.include_stack_size = 0;
    lexer_state.current_file = &lexer_state.include_stack[lexer_state.include_stack_size];

    lexer_state.current_file->scanned_filename = scanned_filename;

    lexer_state.current_file->fd = fd;
    lexer_state.current_file->buffer_size = s.st_size;
    lexer_state.current_file->current_pos
        = lexer_state.current_file->buffer = mmapped_addr;

    lexer_state.current_file->current_location.filename = input_filename;
    lexer_state.current_file->current_location.line = 1;
    lexer_state.current_file->current_location.column = 1;

    init_lexer_state();

    return 0;
}

static
struct special_token_table_tag
{
    const char* keyword;
    int token_id;
    char preserve_eos;
} special_tokens[] =
// Keep this table sorted by the 'keyword' field!
{
    {"@END GLOBAL@", END_GLOBAL, 0 },
    {"@EXPRESSION@", SUBPARSE_EXPRESSION, 0 },
    {"@GLOBAL@", GLOBAL, 0 },
    {"@IS_VARIABLE@", TOKEN_IS_VARIABLE, 0 },
    {"@NODECL-LITERAL-EXPR@",  NODECL_LITERAL_EXPR, 0 },
    {"@NODECL-LITERAL-STMT@", NODECL_LITERAL_STMT, 0 },
    {"@OMP-DECLARE-REDUCTION@", SUBPARSE_OPENMP_DECLARE_REDUCTION, 1 },
    {"@OMP-DEPEND-ITEM@", SUBPARSE_OPENMP_DEPEND_ITEM, 1 },
    {"@OMPSS-DEPENDENCY-EXPR@", SUBPARSE_OMPSS_DEPENDENCY_EXPRESSION, 1 },
    {"@PROGRAM-UNIT@", SUBPARSE_PROGRAM_UNIT, 0 },
    {"@STATEMENT@", SUBPARSE_STATEMENT, 0 },
    {"@SYMBOL-LITERAL-REF@", SYMBOL_LITERAL_REF, 0 },
    {"@TYPEDEF@", TYPEDEF, 0 },
    {"@TYPE-LITERAL-REF@", TYPE_LITERAL_REF, 0 },
};

static int special_token_table_comp(
        const void* p1,
        const void* p2)
{
    const struct special_token_table_tag* v1 = (const struct special_token_table_tag*)p1;
    const struct special_token_table_tag* v2 = (const struct special_token_table_tag*)p2;

    return strcasecmp(v1->keyword, v2->keyword);
}

static void peek_init(void);
static void init_lexer_state(void)
{
    lexer_state.substate = LEXER_SUBSTATE_NORMAL;
    lexer_state.bol = 1;
    lexer_state.last_eos = 1;
    lexer_state.in_nonblock_do_construct = 0;
    lexer_state.num_nonblock_labels = 0;
    lexer_state.num_pragma_constructs = 0;

    init_fixed_form();

    peek_init();
}

static const char * const TL_SOURCE_STRING = "MERCURIUM_INTERNAL_SOURCE";

extern int new_mf03_prepare_string_for_scanning(const char* str)
{
    static int num_string = 0;

    DEBUG_CODE()
    {
        fprintf(stderr, "* Going to parse string in Fortran\n");
        fprintf(stderr, "%s\n", str);
        fprintf(stderr, "* End of parsed string\n");
    }

    lexer_state.include_stack_size = 0;
    lexer_state.current_file = &(lexer_state.include_stack[lexer_state.include_stack_size]);

    const char* filename = NULL;
    uniquestr_sprintf(&filename, "%s-%s-%d", TL_SOURCE_STRING, CURRENT_COMPILED_FILE->input_filename, num_string);
    num_string++;

    lexer_state.form = LEXER_TEXTUAL_FREE_FORM;
    lexer_state.current_file->fd = -1; // not an mmap
    lexer_state.current_file->buffer_size = strlen(str);
    lexer_state.current_file->current_pos
        = lexer_state.current_file->buffer = str;

    lexer_state.current_file->current_location.filename = filename;
    lexer_state.current_file->current_location.line = 1;
    lexer_state.current_file->current_location.column = 1;

    init_lexer_state();

    return 0;
}

static inline void close_current_file(void)
{
    if (lexer_state.current_file->fd >= 0)
    {
        int res = munmap((void*)lexer_state.current_file->buffer, lexer_state.current_file->buffer_size);
        if (res < 0)
        {
            fatal_error("error: unmaping of file '%s' failed (%s)\n", lexer_state.current_file->current_location.filename, strerror(errno));
        }
        res = close(lexer_state.current_file->fd);
        if (res < 0)
        {
            fatal_error("error: closing file '%s' failed (%s)\n", lexer_state.current_file->current_location.filename, strerror(errno));
        }
        lexer_state.current_file->fd = -1;
    }
}

static int commit_text(int token_id, const char* str, token_location_t loc);

static inline char process_end_of_file(char *emit_extra_eos)
{
    *emit_extra_eos = !lexer_state.last_eos;

    // Are we in the last file?
    if (lexer_state.include_stack_size == 0)
    {
        close_current_file();
        return 1;
    }
    else
    {
        DEBUG_CODE() DEBUG_MESSAGE("End of included file %s switching back to %s", 
                lexer_state.current_file->current_location.filename,
                lexer_state.include_stack[lexer_state.include_stack_size-1].current_location.filename);

        close_current_file();

        lexer_state.include_stack_size--;
        lexer_state.current_file = &(lexer_state.include_stack[lexer_state.include_stack_size]);
        lexer_state.last_eos = 1;
        lexer_state.bol = 1;
        lexer_state.in_nonblock_do_construct = 0;

        return 0;
    }
}


static inline char is_blank(int c)
{
    return (c == ' ' || c == '\t');
}

static inline char is_newline(int c)
{
    return c == '\n'
        || c == '\r';
}

static inline char is_letter(int c)
{
    return ('a' <= c && c <= 'z')
        || ('A' <= c && c <= 'Z');
}

static inline char is_decimal_digit(int c)
{
    return ('0' <= c && c <= '9');
}

static inline char is_binary_digit(int c)
{
    return c == '0' || c == '1';
}

static inline char is_octal_digit(int c)
{
    return '0' <= c && c <= '7';
}

static inline char is_hex_digit(int c)
{
    return ('0' <= c && c <= '9')
        || ('a' <= c && c <= 'f')
        || ('A' <= c && c <= 'F');
}

typedef
struct tiny_dyncharbuf_tag
{
    int capacity;
    int num;
    char *buf;
} tiny_dyncharbuf_t;

static inline void tiny_dyncharbuf_new(tiny_dyncharbuf_t* t, int initial_size)
{
    t->capacity = initial_size;
    t->buf = NEW_VEC(char, t->capacity);
    t->num = 0;
}

static inline void tiny_dyncharbuf_add(tiny_dyncharbuf_t* t, char c)
{
    if (t->num >= t->capacity)
    {
        t->capacity *= 2;
        t->buf = NEW_REALLOC(char, t->buf, (t->capacity + 1));
    }
    t->buf[t->num] = c;
    t->num++;
}

static inline void tiny_dyncharbuf_add_str(tiny_dyncharbuf_t* t, const char* str)
{
    if (str == NULL)
        return;

    unsigned int i;
    for (i = 0; i < strlen(str); i++)
    {
        tiny_dyncharbuf_add(t, str[i]);
    }
}

static inline char past_eof(void)
{
    return (lexer_state.current_file->current_pos >= ((lexer_state.current_file->buffer + lexer_state.current_file->buffer_size)));
}

static char handle_preprocessor_line(void)
{
    const char* keep = lexer_state.current_file->current_pos;
    int keep_column = lexer_state.current_file->current_location.column;

#define ROLLBACK \
    { \
        lexer_state.current_file->current_pos = keep; \
        lexer_state.current_file->current_location.column = keep_column; \
        return 0; \
    }

    // We allow the following syntax
    // ^([ ]*line)?[ ]+[0-9]+[ ]*("[^"]*"[ ]+([1234][ ]+)*$
    while (!past_eof()
            && is_blank(lexer_state.current_file->current_pos[0]))
    {
        lexer_state.current_file->current_pos++;
        lexer_state.current_file->current_location.column++;
    }

    if (past_eof())
        ROLLBACK;

    if (lexer_state.current_file->current_pos[0] == 'l')
    {
        // Attempt to match 'line'
        const char c[] = "line";
        int i = 1; // we already know it starts by 'l'
        lexer_state.current_file->current_pos++;
        while (c[i] != '\0'
                && !past_eof()
                && c[i] == lexer_state.current_file->current_pos[0])
        {
            lexer_state.current_file->current_pos++;
            lexer_state.current_file->current_location.column++;

            i++;
        }

        if (past_eof()
                || c[i] != '\0')
            ROLLBACK;

        if (!is_blank(lexer_state.current_file->current_pos[0]))
            ROLLBACK;

        while (!past_eof()
                && is_blank(lexer_state.current_file->current_pos[0]))
        {
            lexer_state.current_file->current_pos++;
            lexer_state.current_file->current_location.column++;
        }
    }

    // linenum
    if (!is_decimal_digit(lexer_state.current_file->current_pos[0]))
        ROLLBACK;

    int linenum = 0;

    while (!past_eof()
            && is_decimal_digit(lexer_state.current_file->current_pos[0]))
    {
        linenum = 10*linenum + (lexer_state.current_file->current_pos[0] - '0');
        lexer_state.current_file->current_pos++;
        lexer_state.current_file->current_location.column++;
    }

    // This is not possible, fix it to 1
    if (linenum == 0)
        linenum = 1;

    token_location_t filename_loc = lexer_state.current_file->current_location;

    while (!past_eof()
            && is_blank(lexer_state.current_file->current_pos[0]))
    {
        lexer_state.current_file->current_pos++;
        lexer_state.current_file->current_location.column++;
    }

    if (past_eof())
    {
        lexer_state.current_file->current_location.line = linenum;
        lexer_state.current_file->current_location.column = 1;

        return 1;
    }
    else if (is_newline(lexer_state.current_file->current_pos[0]))
    {
        if (lexer_state.current_file->current_pos[0] == '\r')
        {
            lexer_state.current_file->current_pos++;
            if (!past_eof()
                    && lexer_state.current_file->current_pos[0] == '\n')
                lexer_state.current_file->current_pos++;
        }
        else // '\n'
        {
            lexer_state.current_file->current_pos++;
        }

        lexer_state.current_file->current_location.line = linenum;
        lexer_state.current_file->current_location.column = 1;

        return 1;
    }
    else if (lexer_state.current_file->current_pos[0] == '"')
    {
        lexer_state.current_file->current_pos++;
        lexer_state.current_file->current_location.column++;

        const char *start = lexer_state.current_file->current_pos;

        // FIXME - escaped characters
        while (!past_eof()
                && lexer_state.current_file->current_pos[0] != '"'
                && !is_newline(lexer_state.current_file->current_pos[0]))
        {
            lexer_state.current_file->current_pos++;
            lexer_state.current_file->current_location.column++;
        }

        if (lexer_state.current_file->current_pos[0] != '"')
            ROLLBACK;

        const char* final = lexer_state.current_file->current_pos - 1;

        if (final - start == 0)
            ROLLBACK;

        int num_chars = final - start + 1;
        int num_bytes_buffer = num_chars + 1;
        char filename[num_bytes_buffer];
        memcpy(filename, start, num_chars);
        filename[num_bytes_buffer - 1] = '\0';

        // advance delim
        lexer_state.current_file->current_pos++;
        lexer_state.current_file->current_location.column++;

        for (;;)
        {
            while (!past_eof()
                    && is_blank(lexer_state.current_file->current_pos[0]))
            {
                lexer_state.current_file->current_pos++;
                lexer_state.current_file->current_location.column++;
            }

            if (past_eof())
                break;

            if (is_newline(lexer_state.current_file->current_pos[0]))
                break;

            token_location_t flag_loc = lexer_state.current_file->current_location;

            int flag = 0;
            if (is_decimal_digit(lexer_state.current_file->current_pos[0]))
            {
                while (!past_eof()
                        && is_decimal_digit(lexer_state.current_file->current_pos[0]))
                {
                    flag = 10*flag + (lexer_state.current_file->current_pos[0] - '0');

                    lexer_state.current_file->current_pos++;
                    lexer_state.current_file->current_location.column++;
                }

                if (1 <= flag
                        && flag <= 4)
                {
                    // We could do something with these
                }
                else
                {
                    warn_printf_at(
                            make_locus(
                                flag_loc.filename,
                                flag_loc.line,
                                flag_loc.column),
                            "invalid flag %d\n",
                            flag);
                }
            }
            else
            {
                warn_printf_at(
                        make_locus(
                            lexer_state.current_file->current_location.filename,
                            lexer_state.current_file->current_location.line,
                            lexer_state.current_file->current_location.column),
                        "unexpected tokens at end of line-marker\n");
                break;
            }
        }

        // Go to end of the line
        while (!past_eof()
                && !is_newline(lexer_state.current_file->current_pos[0]))
        {
            lexer_state.current_file->current_pos++;
            lexer_state.current_file->current_location.column++;
        }

        if (past_eof())
        {
        }
        else if (is_newline(lexer_state.current_file->current_pos[0]))
        {
            lexer_state.current_file->current_location.line++;
            lexer_state.current_file->current_location.column = 1;
            if (lexer_state.current_file->current_pos[0] == '\n')
            {
                lexer_state.current_file->current_pos++;
            }
            else // if (lexer_state.current_file->current_pos[0] == '\r')
            {
                lexer_state.current_file->current_pos++;
                if (!past_eof()
                        && lexer_state.current_file->current_pos[0] == '\n')
                {
                    lexer_state.current_file->current_pos++;
                }
            }
        }

        lexer_state.current_file->current_location.filename = uniquestr(filename);
        lexer_state.current_file->current_location.line = linenum;
        lexer_state.current_file->current_location.column = 1;

        return 1;
    }
    else
    {
        warn_printf_at(
                make_locus(
                    filename_loc.filename,
                    filename_loc.line,
                    filename_loc.column),
                "invalid filename, ignoring line-marker\n");

        // Go to end of the line
        while (!past_eof()
                && !is_newline(lexer_state.current_file->current_pos[0]))
        {
            lexer_state.current_file->current_pos++;
            lexer_state.current_file->current_location.column++;
        }

        if (!past_eof())
        {
            lexer_state.current_file->current_location.line++;
            lexer_state.current_file->current_location.column = 1;
            if (lexer_state.current_file->current_pos[0] != '\n')
            {
                lexer_state.current_file->current_pos++;
            }
            else // if (lexer_state.current_file->current_pos[0] != '\r')
            {
                lexer_state.current_file->current_pos++;
                if (!past_eof()
                        && lexer_state.current_file->current_pos[0] != '\n')
                {
                    lexer_state.current_file->current_pos++;
                }
            }
        }

        return 1;
    }
#undef ROLLBACK
}

static inline char is_known_sentinel_str(char *sentinel, const char **out_sentinel)
{
    int i;
    char found = 0;
    for (i = 0; i < CURRENT_CONFIGURATION->num_pragma_custom_prefix; i++)
    {
        if (strcasecmp(sentinel, CURRENT_CONFIGURATION->pragma_custom_prefix[i]) == 0)
        {
            found = 1;
            if (out_sentinel != NULL)
            {
                *out_sentinel = CURRENT_CONFIGURATION->pragma_custom_prefix[i];
            }
            break;
        }
    }
    return found;
}

static char finish_character(char result)
{
    if (!is_newline(result))
    {
        if (lexer_state.character_context)
        {
            if (lexer_state.character_context_hollerith_length > 0)
            {
                // Hollerith
                lexer_state.character_context_hollerith_length--;
                if (lexer_state.character_context_hollerith_length == 0)
                    lexer_state.character_context = 0;
            }
            else if (result == lexer_state.character_context_delim)
            {
                // This is not exact but is in general quite effective
                lexer_state.character_context = 0;
                lexer_state.character_context_delim = 0;
            }

            lexer_state.previous_was_letter = 0;
        }
        else
        {
            if (!lexer_state.in_comment)
            {
                if (is_decimal_digit(result))
                {
                    if (lexer_state.character_context_hollerith_length == 0)
                    {
                        if (!lexer_state.previous_was_letter)
                            lexer_state.character_context_hollerith_length = result - '0';
                    }
                    else
                    {
                        lexer_state.character_context_hollerith_length =
                            lexer_state.character_context_hollerith_length * 10
                            + result - '0';
                    }
                }
                else if ((result == '\''
                            || result == '"'))
                {
                    lexer_state.character_context = 1;
                    lexer_state.character_context_delim = result;
                }
                else if (tolower(result) == 'h'
                        && lexer_state.character_context_hollerith_length > 0)
                {
                    lexer_state.character_context = 1;
                }
                else
                {
                    lexer_state.character_context_hollerith_length = 0;
                }

                lexer_state.previous_was_letter = is_letter(result);
            }

            if (!lexer_state.in_comment
                    && result == '!')
            {
                // Maybe this starts a comment, we need to peek a bit in order to be sure
                const char * p = lexer_state.current_file->current_pos;

                lexer_state.current_file->current_pos++;
                if (!past_eof())
                {
                    if (lexer_state.current_file->current_pos[0] == '$')
                    {
                        lexer_state.current_file->current_pos++;
                        if (!past_eof())
                        {
                            if (is_blank(lexer_state.current_file->current_pos[0]))
                            {
                                if (CURRENT_CONFIGURATION->disable_empty_sentinels)
                                    lexer_state.in_comment = 1;
                            }
                            else if (is_letter(lexer_state.current_file->current_pos[0]))
                            {
                                // Maybe this is a known sentinel
                                tiny_dyncharbuf_t sentinel;
                                tiny_dyncharbuf_new(&sentinel, 8);
                                while (!past_eof()
                                        && is_letter(lexer_state.current_file->current_pos[0]))
                                {
                                    tiny_dyncharbuf_add(&sentinel, lexer_state.current_file->current_pos[0]);
                                    lexer_state.current_file->current_pos++;
                                }
                                tiny_dyncharbuf_add(&sentinel, '\0');

                                if (!is_known_sentinel_str(sentinel.buf, NULL))
                                {
                                    lexer_state.in_comment = 1;
                                }
                                DELETE(sentinel.buf);
                            }
                            else
                            {
                                lexer_state.in_comment = 1;
                            }
                        }
                    }
                    else
                    {
                        lexer_state.in_comment = 1;
                    }
                }

                lexer_state.current_file->current_pos = p;
            }
        }

        lexer_state.current_file->current_location.column++;
        lexer_state.current_file->current_pos++;
    }
    else
    {
        lexer_state.in_comment = 0;
        lexer_state.character_context_hollerith_length = 0;
        lexer_state.character_context = 0;

        lexer_state.current_file->current_location.line++;
        lexer_state.current_file->current_location.column = 1;

        lexer_state.current_file->current_pos++;
        if (result == '\r'
                && !past_eof()
                && lexer_state.current_file->current_pos[0] == '\n')
        {
            // DOS: \r\n will act like a single '\r'
            lexer_state.current_file->current_pos++;
        }
    }

    return result;
}

static inline int fixed_form_get(token_location_t* loc)
{
    int result = EOF;
    while (!past_eof())
    {
        result = lexer_state.current_file->current_pos[0];

        // If this is a newline or an inline comment initiator
        if (is_newline(result)
                || (result == '!'
                    && !lexer_state.in_comment
                    && !lexer_state.character_context
                    && lexer_state.current_file->current_location.column != 1
                    && lexer_state.current_file->current_location.column != 6
                    && (lexer_state.current_file->current_location.column
                        <= CURRENT_CONFIGURATION->input_column_width)))
        {
            if (is_newline(result)
                    && lexer_state.character_context
                    && (lexer_state.current_file->current_location.column
                        <= CURRENT_CONFIGURATION->input_column_width))
            {
                // We pad with blanks till the width of the statement field
                result = ' ';
                // Note: finish_character will always advance
                // current_file->current_pos so decrease it here
                lexer_state.current_file->current_pos--;
                break;
            }

            // Now we have to check if the next line continues this one
            const char* const keep = lexer_state.current_file->current_pos;
            const token_location_t keep_location = lexer_state.current_file->current_location;

#define ROLLBACK \
            { \
                lexer_state.current_file->current_pos = keep; \
                lexer_state.current_file->current_location = keep_location; \
                break; \
            }

            if (result == '!')
            {
                // Move to the end of the line
                while (!is_newline(lexer_state.current_file->current_pos[0]))
                {
                    lexer_state.current_file->current_location.column++;
                    lexer_state.current_file->current_pos++;
                }
            }

            if (past_eof())
                ROLLBACK;

            // Handle the newline
            if (lexer_state.current_file->current_pos[0] == '\n')
            {
                lexer_state.current_file->current_location.column = 1;
                lexer_state.current_file->current_location.line++;

                lexer_state.current_file->current_pos++;
                if (past_eof())
                    ROLLBACK;
            }
            else if (lexer_state.current_file->current_pos[0] == '\r')
            {
                lexer_state.current_file->current_location.column = 1;
                lexer_state.current_file->current_location.line++;

                lexer_state.current_file->current_pos++;
                if (past_eof())
                    ROLLBACK;

                if (lexer_state.current_file->current_pos[0] == '\n')
                {
                    lexer_state.current_file->current_pos++;
                    if (past_eof())
                        ROLLBACK;
                }
            }
            else
            {
                internal_error("Code unreachable", 0);
            }

            if (past_eof())
                ROLLBACK;

            char can_continue = 1;
            for (;;)
            {
                // Now it may happen that the next line is just a comment
                // but we have to be extra careful with sentinels
                if ((lexer_state.sentinel == NULL
                            && (lexer_state.current_file->current_pos[0] == '!'
                                || lexer_state.current_file->current_pos[0] == '*'
                                || tolower(lexer_state.current_file->current_pos[0]) == 'c'))
                        || tolower(lexer_state.current_file->current_pos[0]) == 'd')
                {
                    if (tolower(lexer_state.current_file->current_pos[0]) != 'd')
                    {
                        lexer_state.current_file->current_location.column++;
                        lexer_state.current_file->current_pos++;
                        if (!past_eof())
                        {
                            if (lexer_state.current_file->current_pos[0] == '$')
                            {
                                char sentinel[4] = { '\0', '\0', '\0', '\0' };
                                // Cowardly refusing to continue through a comment line
                                // if it has the form !$<known-sentinel> or !$ and we
                                // did not disable empty sentinels
                                char ok = 1;
                                int i;
                                lexer_state.current_file->current_pos++;
                                lexer_state.current_file->current_location.column++;
                                for (i = 0; i < 3; i++)
                                {
                                    if (past_eof())
                                    {
                                        ok = 0;
                                        break;
                                    }
                                    // Note that this is OK, it will make for a shorter sentinel
                                    if (!is_letter(lexer_state.current_file->current_pos[0]))
                                        break;

                                    sentinel[i] = lexer_state.current_file->current_pos[0];
                                    lexer_state.current_file->current_pos++;
                                    lexer_state.current_file->current_location.column++;
                                }

                                if (!ok)
                                {
                                    can_continue = 0;
                                    break;
                                }

                                // If this is a known sentinel, do not traverse it
                                // Note: sentinels in fixed form _must_ be 3 letters long
                                if (strlen(sentinel) == 3
                                        && is_known_sentinel_str(sentinel, NULL))
                                {
                                    can_continue = 0;
                                    break;
                                }

                                // Maybe it is an empty sentinel
                                if (sentinel[0] == ' '
                                        && sentinel[1] == ' '
                                        && sentinel[2] == ' '
                                        && !CURRENT_CONFIGURATION->disable_empty_sentinels)
                                {
                                    can_continue = 0;
                                    break;
                                }
                            }
                        }
                    }

                    while (!past_eof()
                            && !is_newline(lexer_state.current_file->current_pos[0]))
                    {
                        lexer_state.current_file->current_location.column++;
                        lexer_state.current_file->current_pos++;
                    }
                    if (past_eof())
                    {
                        can_continue = 0;
                        break;
                    }

                    // Advance newline
                    if (lexer_state.current_file->current_pos[0] == '\n')
                    {
                        lexer_state.current_file->current_location.column = 1;
                        lexer_state.current_file->current_location.line++;

                        lexer_state.current_file->current_pos++;
                        if (past_eof())
                        {
                            can_continue = 0;
                            break;
                        }
                    }
                    else if (lexer_state.current_file->current_pos[0] == '\r')
                    {
                        lexer_state.current_file->current_location.column = 1;
                        lexer_state.current_file->current_location.line++;

                        lexer_state.current_file->current_pos++;
                        if (past_eof())
                        {
                            can_continue = 0;
                            break;
                        }

                        if (lexer_state.current_file->current_pos[0] == '\n')
                        {
                            lexer_state.current_file->current_pos++;
                            if (past_eof())
                            {
                                can_continue = 0;
                                break;
                            }
                        }
                    }
                    else
                    {
                        internal_error("Code unreachable", 0);
                    }

                    // continue to the next line (redundant)
                    continue;
                }
                else if (is_blank(lexer_state.current_file->current_pos[0])
                        // It may happen that this line is completely empty
                        || is_newline(lexer_state.current_file->current_pos[0]))
                {
                    // Make sure that we do not find a comment (except in the 6th column)
                    const char* const keep2 = lexer_state.current_file->current_pos;
                    const token_location_t keep_location2 = lexer_state.current_file->current_location;

                    while (!past_eof()
                            && is_blank(lexer_state.current_file->current_pos[0]))
                    {
                        if (lexer_state.current_file->current_pos[0] == '\t'
                                && lexer_state.current_file->current_location.column == 1)
                        {
                            lexer_state.current_file->current_pos++;
                            lexer_state.current_file->current_location.column = 6;
                        }
                        else
                        {
                            lexer_state.current_file->current_pos++;
                            lexer_state.current_file->current_location.column++;
                        }
                    }

                    if (!past_eof()
                            && ((lexer_state.current_file->current_pos[0] == '!'
                                    && lexer_state.current_file->current_location.column != 6)
                                // It may happen that this line is just a bunch of whitespace
                                || (is_newline(lexer_state.current_file->current_pos[0]))))
                    {
                        // Now advance till end of line
                        while (!past_eof()
                                && !is_newline(lexer_state.current_file->current_pos[0]))
                        {
                            lexer_state.current_file->current_location.column++;
                            lexer_state.current_file->current_pos++;
                        }
                        if (past_eof())
                        {
                            can_continue = 0;
                            break;
                        }

                        // Advance newline
                        if (lexer_state.current_file->current_pos[0] == '\n')
                        {
                            lexer_state.current_file->current_location.column = 1;
                            lexer_state.current_file->current_location.line++;

                            lexer_state.current_file->current_pos++;
                            if (past_eof())
                            {
                                can_continue = 0;
                                break;
                            }
                        }
                        else if (lexer_state.current_file->current_pos[0] == '\r')
                        {
                            lexer_state.current_file->current_location.column = 1;
                            lexer_state.current_file->current_location.line++;

                            lexer_state.current_file->current_pos++;
                            if (past_eof())
                            {
                                can_continue = 0;
                                break;
                            }

                            if (lexer_state.current_file->current_pos[0] == '\n')
                            {
                                lexer_state.current_file->current_pos++;
                                if (past_eof())
                                {
                                    can_continue = 0;
                                    break;
                                }
                            }
                        }
                        else
                        {
                            internal_error("Code unreachable", 0);
                        }

                        // Continue to the next line (redundant)
                        continue;
                    }
                    else
                    {
                        // Not a comment
                        lexer_state.current_file->current_pos = keep2;
                        lexer_state.current_file->current_location = keep_location2;
                        break;
                    }
                }
                else
                {
                    break;
                }
            }

            if (!can_continue)
                ROLLBACK;

            if (lexer_state.sentinel == NULL)
            {
                // There should be 5 blanks and then a nonblank character other
                // than zero If there is a tab at the beginning of the line we
                // handle it special (see is_tab_form)
                char ok = 1;
                char is_tab_form = 0;
                int i;
                for (i = 0; i < 5; i++)
                {
                    if (past_eof()
                            || (lexer_state.current_file->current_pos[0] != ' '
                                && lexer_state.current_file->current_pos[0] != '\t'))
                    {
                        ok = 0;
                        break;
                    }

                    if (i == 0
                            && (lexer_state.current_file->current_pos[0] == '\t'))
                    {
                        is_tab_form = 1;
                        lexer_state.current_file->current_location.column = 6;
                        lexer_state.current_file->current_pos++;
                        break;
                    }

                    lexer_state.current_file->current_location.column++;
                    lexer_state.current_file->current_pos++;
                }
                if (!ok)
                    ROLLBACK;

                if (past_eof())
                    ROLLBACK;

                if (is_newline(lexer_state.current_file->current_pos[0]))
                    ROLLBACK;

                if (!is_tab_form)
                {
                    if (lexer_state.current_file->current_pos[0] == '0'
                            || lexer_state.current_file->current_pos[0] == ' '
                            || lexer_state.current_file->current_pos[0] == '\t')
                    {
                        // Maybe this is an empty line or a line with just a comment
                        while (!past_eof()
                                && is_blank(lexer_state.current_file->current_pos[0]))
                        {
                            lexer_state.current_file->current_location.column++;
                            lexer_state.current_file->current_pos++;
                        }

                        if (past_eof())
                            ROLLBACK;

                        if (is_newline(lexer_state.current_file->current_pos[0]))
                        {
                            // continue to the next line
                            continue;
                        }
                        else if (lexer_state.current_file->current_pos[0] == '!')
                        {
                            // This is a comment line, skip
                            lexer_state.current_file->current_location.column++;
                            lexer_state.current_file->current_pos++;

                            while (!past_eof()
                                    && !is_newline(lexer_state.current_file->current_pos[0]))
                            {
                                lexer_state.current_file->current_location.column++;
                                lexer_state.current_file->current_pos++;
                            }
                            if (past_eof())
                                ROLLBACK;

                            // continue to the next line
                            continue;
                        }
                        else
                        {
                            ROLLBACK;
                        }
                    }
                }
                else
                {
                    // A continuation only if the tab is followed by a nonzero digit
                    if (!is_decimal_digit(lexer_state.current_file->current_pos[0])
                            || lexer_state.current_file->current_pos[0] == '0')
                        ROLLBACK;
                }

                lexer_state.current_file->current_location.column++;
                lexer_state.current_file->current_pos++;
            }
            else
            {
                int length = strlen(lexer_state.sentinel);
                ERROR_CONDITION(
                        length != 0
                        && length != 3,
                        "In fixed form sentinels can only be 0 or 3 letters long", 0);

                if (lexer_state.current_file->current_pos[0] != '!'
                        && lexer_state.current_file->current_pos[0] != '*'
                        && tolower(lexer_state.current_file->current_pos[0]) != 'c')
                    ROLLBACK;

                lexer_state.current_file->current_location.column++;
                lexer_state.current_file->current_pos++;

                if (past_eof())
                    ROLLBACK;

                if (lexer_state.current_file->current_pos[0] != '$')
                    ROLLBACK;

                lexer_state.current_file->current_location.column++;
                lexer_state.current_file->current_pos++;

                char ok = 1;
                int i;
                for (i = 0; i < length; i++)
                {
                    if (past_eof())
                    {
                        ok = 0;
                        break;
                    }
                    if (tolower(lexer_state.current_file->current_pos[0]) !=
                            tolower(lexer_state.sentinel[i]))
                    {
                        ok = 0;
                        break;
                    }

                    lexer_state.current_file->current_location.column++;
                    lexer_state.current_file->current_pos++;
                }
                if (!ok)
                    ROLLBACK;
                // Complete the label field
                // (this loop is empty if length == 3)
                for (i = 2 + length; i < 5; i++)
                {
                    if (past_eof())
                    {
                        ok = 0;
                        break;
                    }

                    if (lexer_state.current_file->current_pos[0] != ' ')
                    {
                        ok = 0;
                        break;
                    }

                    lexer_state.current_file->current_location.column++;
                    lexer_state.current_file->current_pos++;
                }
                if (!ok)
                    ROLLBACK;

                if (past_eof())
                    ROLLBACK;

                // A continuation only if the sentinel is followed by a
                // character other than zero or space
                if (lexer_state.current_file->current_pos[0] == '0'
                        || lexer_state.current_file->current_pos[0] == ' '
                        /* || lexer_state.current_file->current_pos[0] == '\t' */)
                    ROLLBACK;

                lexer_state.current_file->current_location.column++;
                lexer_state.current_file->current_pos++;
            }

            // this is a valid continuation line
            continue; /* redundant, here just for clarity */
#undef ROLLBACK
        }
        else if (lexer_state.current_file->current_location.column > CURRENT_CONFIGURATION->input_column_width)
        {
            lexer_state.current_file->current_location.column++;
            lexer_state.current_file->current_pos++;
        }
        else if (result == ' ')
        {
            if (lexer_state.character_context)
                break;

            lexer_state.current_file->current_location.column++;
            lexer_state.current_file->current_pos++;
        }
        else if (result == '\t')
        {
            if (lexer_state.character_context)
                break;

            if (lexer_state.current_file->current_location.column < 6)
                lexer_state.current_file->current_location.column += 6;
            else
                lexer_state.current_file->current_location.column++;
            lexer_state.current_file->current_pos++;
        }
        else if (lexer_state.current_file->current_location.column == 1
                && (tolower(result) == 'c'
                    || tolower(result) == 'd'
                    || result == '*'))
        {
            result = '!';
            break;
        }
        else if (lexer_state.current_file->current_location.column == 6
                && result == '0')
        {
            // A zero in column 6 is to be ignored
            lexer_state.current_file->current_location.column++;
            lexer_state.current_file->current_pos++;
        }
        else
        {
            // done
            break;
        }
    }

    *loc = lexer_state.current_file->current_location;
    if (past_eof())
        return EOF;

    return finish_character(result);
}

static inline int free_form_get(token_location_t* loc)
{
    if (past_eof())
    {
        *loc = lexer_state.current_file->current_location;
        return EOF;
    }

    int result = lexer_state.current_file->current_pos[0];

    while (result == '&'
            && !lexer_state.in_comment)
    {
        const char* const keep = lexer_state.current_file->current_pos;
        const char keep_bol = lexer_state.bol;
        const token_location_t keep_location = lexer_state.current_file->current_location;

        lexer_state.current_file->current_pos++;
        lexer_state.current_file->current_location.column++;
        lexer_state.bol = 0;

        // blanks
        while (!past_eof()
                && is_blank(lexer_state.current_file->current_pos[0]))
        {
            lexer_state.current_file->current_location.column++;

            lexer_state.current_file->current_pos++;
        }

#define ROLLBACK \
        { \
            lexer_state.current_file->current_location = keep_location; \
            lexer_state.bol = keep_bol; \
            lexer_state.current_file->current_pos = keep; \
            break; \
        }

        if (past_eof())
            ROLLBACK;

        if (!lexer_state.character_context)
        {
            // When we are not in character context we allow a comment here
            if (lexer_state.current_file->current_pos[0] == '!')
            {
                while (!past_eof()
                        && !is_newline(lexer_state.current_file->current_pos[0]))
                {
                    lexer_state.current_file->current_location.column++;

                    lexer_state.current_file->current_pos++;
                }
                if (past_eof())
                    ROLLBACK;
            }
            else if (is_newline(lexer_state.current_file->current_pos[0]))
            {
                // handled below
            }
            else
                ROLLBACK;
        }

        if (lexer_state.current_file->current_pos[0] == '\n')
        {
            lexer_state.current_file->current_location.line++;
            lexer_state.current_file->current_location.column = 1;

            lexer_state.current_file->current_pos++;
            if (past_eof())
                ROLLBACK;
        }
        else if (lexer_state.current_file->current_pos[0] == '\r')
        {
            lexer_state.current_file->current_location.line++;
            lexer_state.current_file->current_location.column = 1;

            lexer_state.current_file->current_pos++;
            if (!past_eof()
                    && lexer_state.current_file->current_pos[0] == '\n')
            {
                lexer_state.current_file->current_pos++;
                if (past_eof())
                    ROLLBACK;
            }
        }
        else
            ROLLBACK;

        // Now we need to peek if there is another &, so we have to do
        // token pasting, otherwise we will return a blank
        //
        // Note that the following part is complicated because we
        // need to skip lines with only blanks or only a comment
        char do_rollback = 0;
        for (;;)
        {
            // Skip blanks
            while (!past_eof()
                    && is_blank(lexer_state.current_file->current_pos[0]))
            {
                lexer_state.current_file->current_location.column++;

                lexer_state.current_file->current_pos++;
            }
            if (past_eof())
                break;

            if (is_newline(lexer_state.current_file->current_pos[0]))
            {
                // This is a line of only blanks (i.e. an empty line)
                if (lexer_state.current_file->current_pos[0] == '\n')
                {
                    lexer_state.current_file->current_location.line++;
                    lexer_state.current_file->current_location.column = 1;

                    lexer_state.current_file->current_pos++;
                    if (past_eof())
                        break;
                }
                else // '\r'
                {
                    lexer_state.current_file->current_location.line++;
                    lexer_state.current_file->current_location.column = 1;

                    lexer_state.current_file->current_pos++;
                    if (!past_eof()
                            && lexer_state.current_file->current_pos[0] == '\n')
                    {
                        lexer_state.current_file->current_pos++;
                        if (past_eof())
                            break;
                    }
                }

                // This line was empty, continue to the next
                continue;
            }
            else if (lexer_state.current_file->current_pos[0] == '!')
            {
                if (lexer_state.sentinel != NULL)
                {
                    lexer_state.current_file->current_pos++;
                    lexer_state.current_file->current_location.column++;
                    if (past_eof())
                        break;

                    if (lexer_state.current_file->current_pos[0] != '$')
                    {
                        // This is a comment like the following case
                        // !$OMP TASK &
                        // !!$OMP FOO                  <-- comment
                        // !$OMP SHARED(BAR)           <-- legitimate continuation
                        while (!past_eof()
                                && !is_newline(lexer_state.current_file->current_pos[0]))
                        {
                            lexer_state.current_file->current_location.column++;
                            lexer_state.current_file->current_pos++;
                        }

                        if (past_eof())
                            break;

                        // Comment done (we have not yet handled the newline, it will
                        // be handled in the next iteration)
                        continue;
                    }

                    lexer_state.current_file->current_pos++;
                    lexer_state.current_file->current_location.column++;
                    if (past_eof())
                        break;

                    int i = 0;
                    while (!past_eof()
                            && lexer_state.sentinel[i] != '\0'
                            && (tolower(lexer_state.current_file->current_pos[0])
                                == tolower(lexer_state.sentinel[i])))
                    {
                        lexer_state.current_file->current_pos++;
                        lexer_state.current_file->current_location.column++;
                        i++;
                    }

                    if (past_eof())
                        ROLLBACK;

                    if (lexer_state.sentinel[i] != '\0')
                    {
                        do_rollback = 1;
                        break;
                    }

                    // Sentinel done, skip blanks
                    while (!past_eof()
                            && is_blank(lexer_state.current_file->current_pos[0]))
                    {
                        lexer_state.current_file->current_location.column++;

                        lexer_state.current_file->current_pos++;
                    }
                    if (past_eof())
                        break;

                    // We are done
                    break;
                }
                else
                {
                    // This line is just a comment
                    while (!past_eof()
                            && !is_newline(lexer_state.current_file->current_pos[0]))
                    {
                        lexer_state.current_file->current_location.column++;
                        lexer_state.current_file->current_pos++;
                    }

                    if (past_eof())
                        break;

                    // Comment done (we have not yet handled the newline, it will
                    // be handled in the next iteration)
                    continue;
                }
            }
            else if (lexer_state.current_file->current_pos[0] == '#')
            {
                lexer_state.current_file->current_location.column++;
                lexer_state.current_file->current_pos++;
                if (past_eof())
                    break;

                if (handle_preprocessor_line())
                    continue;

                // We are done
                break;
            }
            else if (lexer_state.sentinel != NULL)
            {
                // We expected a sentinel
                do_rollback = 1;
                break;
            }
            else
            {
                // We are done
                break;
            }
        }
        if (past_eof())
            ROLLBACK;
        if (do_rollback)
            ROLLBACK;

        if (lexer_state.current_file->current_pos[0] == '&')
        {
            lexer_state.current_file->current_location.column++;

            lexer_state.current_file->current_pos++;
            if (past_eof())
                ROLLBACK;
            result = lexer_state.current_file->current_pos[0];
        }
        else
        {
            if (lexer_state.character_context)
            {
                // in character context the first nonblank is the next character
                result = lexer_state.current_file->current_pos[0];
            }
            else
            {
                result = ' ';
                // Compensate this artificial character
                lexer_state.current_file->current_pos--;
                lexer_state.current_file->current_location.column--;
            }
        }
#undef ROLLBACK
    }

    *loc = lexer_state.current_file->current_location;

    return finish_character(result);
}

static inline int input_get(token_location_t* loc)
{
    switch (lexer_state.form)
    {
        case LEXER_TEXTUAL_FREE_FORM:
            return free_form_get(loc);
        case LEXER_TEXTUAL_FIXED_FORM:
            return fixed_form_get(loc);
        default: { internal_error("Code unreachable", 0); }
    }
}

typedef
struct peek_token_info_tag
{
    int letter;
    token_location_t loc;
} peek_token_info_t;

static struct peek_queue_tag
{
    peek_token_info_t* buffer;
    int size;

    // Negative offsets from (size - 1)
    int front; // where we take
    int back;  // where we add
} _peek_queue;
enum { PEEK_INITIAL_SIZE = 16 };

static inline void peek_init(void)
{
    if (_peek_queue.buffer == NULL)
    {
        _peek_queue.buffer = NEW_VEC(peek_token_info_t, PEEK_INITIAL_SIZE);
        _peek_queue.size = PEEK_INITIAL_SIZE;
    }
    _peek_queue.back = 0;
    _peek_queue.front = 0;
}

static inline char peek_empty(void)
{
    return (_peek_queue.front == _peek_queue.back);
}

static inline int peek_size(void)
{
    return (_peek_queue.front - _peek_queue.back);
}

#if 0
static void peek_print(void)
{
    fprintf(stderr, "-- PEEK SIZE %d\n", peek_size());
    int i;
    for (i = _peek_queue.front; i > _peek_queue.back; i--)
    {
        fprintf(stderr, "PEEK AT [%03d] : [%03d] => |%c|\n",
                i,
                (_peek_queue.size - 1) + i,
                _peek_queue.buffer[(_peek_queue.size - 1) + i].letter);
    }
    fprintf(stderr, "--\n");
}
#endif

static inline void peek_grow(void)
{
    int new_size = _peek_queue.size * 2;
    peek_token_info_t *new_buffer = NEW_VEC(peek_token_info_t, new_size);

    memcpy(&new_buffer[(new_size - 1) + _peek_queue.back + 1],
            _peek_queue.buffer,
            _peek_queue.size * sizeof(*new_buffer));

    DELETE(_peek_queue.buffer);
    _peek_queue.buffer = new_buffer;
    _peek_queue.size = new_size;
}

static inline void peek_add(int c, token_location_t loc)
{
    if ((_peek_queue.size - 1) + _peek_queue.back < 0)
    {
        peek_grow();
    }
    _peek_queue.buffer[(_peek_queue.size - 1) + _peek_queue.back].letter = c;
    _peek_queue.buffer[(_peek_queue.size - 1) + _peek_queue.back].loc = loc;
    _peek_queue.back--;
}

static inline void peek_take(void)
{
    ERROR_CONDITION(_peek_queue.back == _peek_queue.front, "empty peek queue", 0);

    _peek_queue.front--;
    if (_peek_queue.back == _peek_queue.front)
    {
        _peek_queue.back
            = _peek_queue.front
            = 0;
    }
}

static inline peek_token_info_t peek_get(int n)
{
    ERROR_CONDITION(((_peek_queue.size - 1) + _peek_queue.front) - n < 0, "invalid peek index %d", n);

    return _peek_queue.buffer[((_peek_queue.size - 1) + _peek_queue.front) - n];
}

static inline void peek_insert(int n, int c, token_location_t loc)
{
    ERROR_CONDITION(((_peek_queue.size - 1) + _peek_queue.front) - n < 0, "invalid peek index %d", n);
    if ((_peek_queue.size - 1) + _peek_queue.back < 0)
    {
        peek_grow();
    }

    // peek_print();

    // Now shift left all the items
    int i = _peek_queue.back;
    while (i < (_peek_queue.front - n))
    {
        _peek_queue.buffer[(_peek_queue.size - 1) + i]
        = _peek_queue.buffer[(_peek_queue.size - 1) + i + 1];
        i++;
    }
    _peek_queue.back--;

    _peek_queue.buffer[((_peek_queue.size - 1) + _peek_queue.front) - n].letter = c;
    _peek_queue.buffer[((_peek_queue.size - 1) + _peek_queue.front) - n].loc = loc;

    // peek_print();
    // internal_error("TEST", 0);
}

static inline int get_loc(token_location_t *loc)
{
    token_location_t tmp_loc;

    int c;
    if (!peek_empty())
    {
        peek_token_info_t p = peek_get(0);
        peek_take();

        if (mf03_flex_debug)
        {
            fprintf(stderr, "[PEEK] ");
        }

        c = p.letter;
        tmp_loc = p.loc;
    }
    else
    {
        if (mf03_flex_debug)
        {
            fprintf(stderr, "[FILE] ");
        }
        c = input_get(&tmp_loc);
    }

    if (mf03_flex_debug)
    {
        if (isprint(c))
        {
            fprintf(stderr, "GET LETTER '%c' AND LOCUS |%s:%d:%d|\n",
                    c,
                    tmp_loc.filename,
                    tmp_loc.line,
                    tmp_loc.column);
        }
        else
        {
            fprintf(stderr, "GET LETTER '0x%X' AND LOCUS |%s:%d:%d|\n",
                    c,
                    tmp_loc.filename,
                    tmp_loc.line,
                    tmp_loc.column);
        }
    }
    if (loc != NULL)
        *loc = tmp_loc;

    return c;
}

static inline int get(void)
{
    return get_loc(NULL);
}

static inline int peek_loc(int n, token_location_t *loc)
{
    int s = peek_size();
    if (n >= s)
    {
        int d = n - s + 1;

        int i;
        for (i = 0; i < d; i++)
        {
            token_location_t loc2;
            int c = input_get(&loc2);
            peek_add(c, loc2);

            if (mf03_flex_debug)
            {
                if (isprint(c))
                {
                    fprintf(stderr, "PEEK LETTER %d of %d '%c' AND LOCUS |%s:%d:%d|\n",
                            i, d - 1,
                            c,
                            loc2.filename,
                            loc2.line,
                            loc2.column);
                }
                else
                {
                    fprintf(stderr, "PEEK LETTER %d of %d '0x%X' AND LOCUS |%s:%d:%d|\n",
                            i, d - 1,
                            c,
                            loc2.filename,
                            loc2.line,
                            loc2.column);
                }
            }
        }
    }

    peek_token_info_t p = peek_get(n);

    if (loc != NULL)
        *loc = p.loc;

    return p.letter;
}

static inline int peek(int n)
{
    return peek_loc(n, NULL);
}


static char* scan_kind(void)
{
    tiny_dyncharbuf_t str;
    tiny_dyncharbuf_new(&str, 32);

    int c = get();
    ERROR_CONDITION(c != '_', "input stream is incorrectly located (c=%c)", c);

    token_location_t loc;
    c = peek_loc(0, &loc);
    if (is_decimal_digit(c)
            || is_letter(c))
    {
        tiny_dyncharbuf_add(&str, '_');

        if (is_decimal_digit(c))
        {
            while (is_decimal_digit(c))
            {
                tiny_dyncharbuf_add(&str, c);
                get();
                c = peek(0);
            }
        }
        else if (is_letter(c))
        {
            while (is_letter(c)
                    || is_decimal_digit(c)
                    || c == '_')
            {
                tiny_dyncharbuf_add(&str, c);
                get();
                c = peek(0);
            }
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }
    else
    {
        error_printf_at(
                make_locus(
                    loc.filename,
                    loc.line,
                    loc.column),
                "invalid kind-specifier\n");
    }

    tiny_dyncharbuf_add(&str, '\0');

    return str.buf;
}

static int commit_text(int token_id, const char* str,
        token_location_t loc)
{
    if (mf03_flex_debug)
    {
        fprintf(stderr, "COMMITTING TOKEN %02d WITH TEXT |%s| AND LOCUS |%s:%d:%d|\n\n",
                token_id, str,
                loc.filename,
                loc.line,
                loc.column);
    }
    lexer_state.last_eos = (token_id == EOS);
    lexer_state.bol = 0;

    mf03lval.token_atrib.token_text = uniquestr(str);
    mf03lloc.first_filename = loc.filename;
    mf03lloc.first_line = loc.line;
    mf03lloc.first_column = loc.column;

    return token_id;
}

static int commit_text_and_free(int token_id, char* str,
        token_location_t loc)
{
    token_id = commit_text(token_id, str, loc);
    DELETE(str);
    return token_id;
}

static inline void scan_character_literal(
        const char* prefix,
        char delim,
        char allow_suffix_boz,
        token_location_t loc,
        // out
        int* token_id,
        char** text)
{
    ERROR_CONDITION(prefix == NULL, "Invalid prefix", 0);

    tiny_dyncharbuf_t str;
    tiny_dyncharbuf_new(&str, strlen(prefix) + 32);

    tiny_dyncharbuf_add_str(&str, prefix);

    *token_id = CHAR_LITERAL;

    char can_be_binary = allow_suffix_boz;
    char can_be_octal = allow_suffix_boz;
    char can_be_hexa = allow_suffix_boz;

    char unended_literal = 0;
    int c = peek(0);
    token_location_t loc2 = loc;

    for (;;)
    {
        if (c != delim
                && !is_newline(c))
        {
            get_loc(&loc2);
            tiny_dyncharbuf_add(&str, c);

            can_be_binary = 
                can_be_binary &&
                is_binary_digit(c);

            can_be_octal = 
                can_be_octal &&
                is_octal_digit(c);

            can_be_hexa = 
                can_be_hexa &&
                is_hex_digit(c);

            c = peek_loc(0, &loc2);
        }
        else if (c == delim)
        {
            get_loc(&loc2);
            tiny_dyncharbuf_add(&str, c);
            c = peek_loc(0, &loc2);

            if (c != delim)
                break;

            can_be_binary
                = can_be_hexa
                = can_be_octal
                = 0;

            get_loc(&loc2);
            tiny_dyncharbuf_add(&str, c);
            c = peek_loc(0, &loc2);
        }
        else // c == '\n' || c == '\r'
        {
            error_printf_at(
                    make_locus(
                        loc2.filename,
                        loc2.line,
                        loc2.column),
                    "unended character literal\n");
            unended_literal = 1;
            break;
        }
    }

    if (unended_literal)
    {
        tiny_dyncharbuf_add(&str, delim);
        can_be_binary =
            can_be_octal =
            can_be_hexa = 0;
    }

    if (can_be_binary
            || can_be_octal
            || can_be_hexa)
    {
        c = peek(0);
        if (can_be_binary
                && (tolower(c) == 'b'))
        {
            get();
            tiny_dyncharbuf_add(&str, c);
            *token_id = BINARY_LITERAL;
        }
        else if (can_be_octal
                && (tolower(c) == 'o'))
        {
            get();
            tiny_dyncharbuf_add(&str, c);
            *token_id = OCTAL_LITERAL;
        }
        else if (can_be_hexa
                && (tolower(c) == 'x'
                    || tolower(c) == 'z'))
        {
            get();
            tiny_dyncharbuf_add(&str, c);
            *token_id = HEX_LITERAL;
        }
    }

    tiny_dyncharbuf_add(&str, '\0');

    *text = str.buf;
}

static char* scan_fractional_part_of_real_literal(void)
{
    // We assume that we are right after the '.'

    // a valid real literal at this point must be [.][0-9]*([edq][+-]?[0-9]+)_kind

    tiny_dyncharbuf_t str;
    tiny_dyncharbuf_new(&str, 32);

    tiny_dyncharbuf_add(&str, '.');

    int c = peek(0);

    while (is_decimal_digit(c))
    {
        get();
        tiny_dyncharbuf_add(&str, c);
        c = peek(0);
    }

    if (tolower(c) == 'e'
            || tolower(c) == 'd'
            || tolower(c) == 'q')
    {
        char e = c;
        token_location_t exp_loc;
        c = peek_loc(1, &exp_loc);

        if (is_decimal_digit(c))
        {
            tiny_dyncharbuf_add(&str, e);
            get();
        }
        else if (c == '+' || c == '-')
        {
            char s = c;
            c = peek_loc(2, &exp_loc);
            if (!is_decimal_digit(c))
            {
                error_printf_at(
                        make_locus(
                            exp_loc.filename,
                            exp_loc.line,
                            exp_loc.column),
                        "missing exponent in real literal\n");
                // 1.23e+a
                // 1.23e-a
                tiny_dyncharbuf_add(&str, '\0');
                return str.buf;
            }

            tiny_dyncharbuf_add(&str, e);
            get();
            tiny_dyncharbuf_add(&str, s);
            get();
        }
        else
        {
            // 1.23ea
            error_printf_at(
                    make_locus(
                        exp_loc.filename,
                        exp_loc.line,
                        exp_loc.column),
                    "missing exponent in real literal\n");
            tiny_dyncharbuf_add(&str, '\0');
            return str.buf;
        }

        while (is_decimal_digit(c))
        {
            get();
            tiny_dyncharbuf_add(&str, c);
            c = peek(0);
        }
    }
    if (c == '_')
    {
        char *kind_str = scan_kind();
        tiny_dyncharbuf_add_str(&str, kind_str);
        DELETE(kind_str);
    }

    tiny_dyncharbuf_add(&str, '\0');
    return str.buf;
}

static char is_include_line(void)
{
    int peek_idx = 0;
    int p = peek(peek_idx);

    const char c[] = "include";

    int i = 1; // letter 'i' has already been matched

    while (c[i] != '\0'
            && c[i] == tolower(p))
    {
        i++;

        peek_idx++;
        p = peek(peek_idx);
    }

    if (c[i] != '\0')
        return 0;

    // Skip blanks after INCLUDE
    while (is_blank(p))
    {
        peek_idx++;
        p = peek(peek_idx);
    }

    // Expect here a ' or "
    int delim = p;
    if (delim != '\''
            && delim != '\"')
        return 0;
    // Skip delimiter
    peek_idx++;
    p = peek(peek_idx);

    tiny_dyncharbuf_t include_filename_buf;
    tiny_dyncharbuf_new(&include_filename_buf, 32);

    // Get the name of the included file
    while (p != EOF
            && !is_newline(p)
            && p != delim)
    {
        tiny_dyncharbuf_add(&include_filename_buf, p);

        peek_idx++;
        p = peek(peek_idx);
    }
    tiny_dyncharbuf_add(&include_filename_buf, '\0');

    if (p != delim
            || include_filename_buf.num == 1)
    {
        DELETE(include_filename_buf.buf);
        return 0;
    }

    // Skip delimiter
    peek_idx++;
    p = peek(peek_idx);

    // Blanks until the end of the line
    while (p != EOF
            && is_blank(p))
    {
        peek_idx++;
        p = peek(peek_idx);
    }

    if (p == '!')
    {
        // There is a comment after the include, skip everything until the next
        // newline
        while (!is_newline(p))
        {
            peek_idx++;
            p = peek(peek_idx);
        }
    }

    if (!is_newline(p))
    {
        // There is junk
        DELETE(include_filename_buf.buf);
        return 0;
    }

    token_location_t loc;
    peek_loc(peek_idx, &loc);

    // Now consume all the characters since this looks like a legitimate
    // include line (note that we need also to get character at peek(0))
    while (peek_idx >= 0)
    {
        get();
        peek_idx--;
    }

    lexer_state.current_file->current_location.line = loc.line + 1;
    lexer_state.current_file->current_location.column = 1;

    // Now get the filename
    // We first attempt the current working directory
    const char* current_dir = ".";
    const char* include_filename = fortran_find_file_in_directories(
            1,
            &current_dir,
            include_filename_buf.buf,
            /* origin */ loc.filename);
    // otherwise use -Idir options
    if (include_filename == NULL)
    {
        include_filename = fortran_find_file_in_directories(
                CURRENT_CONFIGURATION->num_include_dirs,
                CURRENT_CONFIGURATION->include_dirs,
                include_filename_buf.buf,
                /* origin */ loc.filename);
    }

    if (include_filename == NULL)
    {
        fatal_printf_at(
                make_locus(
                    loc.filename,
                    loc.line,
                    loc.column),
                "included file '%s' not found\n",
                include_filename_buf.buf);
    }
    DELETE(include_filename_buf.buf);

    int fd = open(include_filename, O_RDONLY);
    if (fd < 0)
    {
        fatal_printf_at(
                make_locus(
                    loc.filename,
                    loc.line,
                    loc.column),
                "cannot open included file '%s' (%s)\n",
                include_filename,
                strerror(errno));
    }

    // Get size of file because we need it for the mmap
    struct stat s;
    int status = fstat (fd, &s);
    if (status < 0)
    {
        fatal_printf_at(
                make_locus(
                    loc.filename,
                    loc.line,
                    loc.column),
                "cannot get status of included file '%s' (%s)\n",
                include_filename, strerror(errno));
    }

    const char *mmapped_addr = mmap(0, s.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
    if (mmapped_addr == MAP_FAILED)
    {
        fatal_printf_at(
                make_locus(
                    loc.filename,
                    loc.line,
                    loc.column),
                "cannot map included file '%s' in memory (%s)",
                include_filename,
                strerror(errno));
    }

    lexer_state.include_stack_size++;
    if (lexer_state.include_stack_size == MAX_INCLUDE_DEPTH)
    {
        fatal_printf_at(
                make_locus(
                    loc.filename,
                    loc.line,
                    loc.column),
                "too many nested included files");
    }

    lexer_state.current_file = &lexer_state.include_stack[lexer_state.include_stack_size];

    lexer_state.current_file->scanned_filename = include_filename;

    lexer_state.current_file->fd = fd;
    lexer_state.current_file->buffer_size = s.st_size;
    lexer_state.current_file->current_pos
        = lexer_state.current_file->buffer = mmapped_addr;

    lexer_state.current_file->current_location.filename = include_filename;
    lexer_state.current_file->current_location.line = 1;
    lexer_state.current_file->current_location.column = 1;

    lexer_state.bol = 1;
    lexer_state.last_eos = 1;
    lexer_state.in_nonblock_do_construct = 0;
    lexer_state.num_nonblock_labels = 0;
    lexer_state.num_pragma_constructs = 0;

    return 1;
}

static inline char is_format_statement(void)
{
    int peek_idx = 0;
    int p = peek(peek_idx);

    // Skip blanks after label
    while (is_blank(p))
    {
        peek_idx++;
        p = peek(peek_idx);
    }

    int i = 0;
    const char c[] = "format";
    while (c[i] != '\0'
            && tolower(p) == c[i])
    {
        peek_idx++;
        p = peek(peek_idx);

        i++;
    }

    if (c[i] != '\0')
        return 0;

    // Skip blanks after FORMAT keyword
    while (is_blank(p))
    {
        peek_idx++;
        p = peek(peek_idx);
    }

    if (p != '(')
        return 0;

    // Skip opening parenthesis
    peek_idx++;
    p = peek(peek_idx);

    char delim = 0;
    int parenthesis_level = 1;
    char in_string = 0;

    tiny_dyncharbuf_t int_str;
    tiny_dyncharbuf_new(&int_str, 5);

    char prev_was_letter = 0;
    for (;;)
    {
        if (p == EOF
                || is_newline(p))
            break;

        if (!in_string)
        {
            if (p == '!'
                    || p == ';')
                break;

            if (p == '(')
            {
                parenthesis_level++;
            }
            else if (p == ')')
            {
                if (parenthesis_level > 0)
                {
                    parenthesis_level--;
                    if (parenthesis_level == 0)
                        break;
                }
            }
            else if (p == '\''
                    || p == '"')
            {
                delim = p;
                in_string = 1;
            }
            // Holleritz
            else if (tolower(p) == 'h'
                    && int_str.num > 0)
            {
                tiny_dyncharbuf_add(&int_str, '\0');
                int skip = atoi(int_str.buf);
                if (skip > 0)
                {
                    peek_idx++;
                    p = peek(peek_idx);

                    while (skip > 0
                            && !is_newline(p))
                    {
                        skip--;

                        peek_idx++;
                        p = peek(peek_idx);
                    }
                    // we read too much
                    peek_idx--;
                }
                int_str.num = 0;
            }

            if (is_decimal_digit(p)
                    && !prev_was_letter)
            {
                tiny_dyncharbuf_add(&int_str, p);
            }
            else
            {
                int_str.num = 0;
            }

            prev_was_letter = is_letter(p);
        }
        else if (in_string)
        {
            if (p == delim)
            {
                if (peek(peek_idx + 1) != delim)
                {
                    in_string = 0;
                }
                else
                {
                    // skip this delimiter
                    peek_idx++;
                }
            }
        }

        peek_idx++;
        p = peek(peek_idx);
    }

    // Unbalanced parentheses or opened string
    if ((parenthesis_level > 0) || (in_string == 1)) 
        return 0;

    // Skip the closing parenthesis
    peek_idx++;
    p = peek(peek_idx);

    // Skip blanks after closing parenthesis
    while (is_blank(p))
    {
        peek_idx++;
        p = peek(peek_idx);
    }

    // Expect an end-of-statement here
    if (!is_newline(p)
            && p != ';'
            && p != '!')
        return 0;

    // This FORMAT seems fine
    return 1;
}

static char is_nonexecutable_statement(int k)
{
    switch (k)
    {
        case TOKEN_TYPE:
        case TOKEN_PRIVATE:
        case TOKEN_PUBLIC:
        case TOKEN_PROGRAM:
        case TOKEN_SUBROUTINE:
        case TOKEN_FUNCTION:
        case TOKEN_MODULE:
            // case TOKEN_MODULE_PROCEDURE:
        case TOKEN_BLOCKDATA:
        case TOKEN_USE:
        case TOKEN_IMPLICIT:
        case TOKEN_PARAMETER:
        case TOKEN_FORMAT:
        case TOKEN_ENTRY:
        case TOKEN_ACCESS:
        case TOKEN_ALLOCATABLE:
        case TOKEN_COMMON:
        case TOKEN_DATA:
        case TOKEN_DIMENSION:
        case TOKEN_EQUIVALENCE:
        case TOKEN_EXTERNAL:
        case TOKEN_INTENT:
        case TOKEN_INTRINSIC:
        case TOKEN_NAMELIST:
        case TOKEN_OPTIONAL:
        case TOKEN_POINTER:
        case TOKEN_SAVE:
        case TOKEN_VALUE:
        case TOKEN_VOLATILE:
        case TOKEN_TARGET:
        case TOKEN_BIND:
            return 1;
        default:
            return 0;
    }
}

static inline int preanalyze_advance_parenthesis(int peek_idx)
{
    int p = peek(peek_idx);
    if (p != '(')
        return peek_idx;

    int parenthesis_level = 0;
    char in_string = 0;

    char delim = 0;

    tiny_dyncharbuf_t int_str;
    tiny_dyncharbuf_new(&int_str, 5);

    char prev_was_letter = 0;
    for (;;)
    {
        if (p == EOF
                || is_newline(p))
            break;

        if (!in_string)
        {
            if (p == '!'
                    || p == ';')
                break;

            if (p == '(')
            {
                parenthesis_level++;
            }
            else if (p == ')')
            {
                if (parenthesis_level > 0)
                {
                    parenthesis_level--;
                    if (parenthesis_level == 0)
                        break;
                }
            }
            else if (p == '\''
                    || p == '"')
            {
                delim = p;
                in_string = 1;
            }
            // Holleritz
            else if (tolower(p) == 'h'
                    && int_str.num > 0)
            {
                tiny_dyncharbuf_add(&int_str, '\0');
                int skip = atoi(int_str.buf);
                if (skip > 0)
                {
                    peek_idx++;
                    p = peek(peek_idx);

                    while (skip > 0
                            && !is_newline(p))
                    {
                        skip--;

                        peek_idx++;
                        p = peek(peek_idx);
                    }
                    // we read too much
                    peek_idx--;
                }
                int_str.num = 0;
            }

            if (is_decimal_digit(p)
                    && !prev_was_letter)
            {
                tiny_dyncharbuf_add(&int_str, p);
            }
            else
            {
                int_str.num = 0;
            }

            prev_was_letter = is_letter(p);
        }
        else if (in_string)
        {
            if (p == delim)
            {
                if (peek(peek_idx + 1) != delim)
                {
                    in_string = 0;
                }
                else
                {
                    // skip this delimiter
                    peek_idx++;
                }
            }
        }

        peek_idx++;
        p = peek(peek_idx);
    }

    DELETE(int_str.buf);
    return peek_idx;
}

static inline void preanalyze_statement(char expect_label)
{
    int peek_idx = 0;
    int p = peek(peek_idx);

    // Early return, we do not have to preanalyze anything here
    if (p == '#'
            || p == EOF
            || is_newline(p))
        return;

    if (p == '!')
    {
        token_location_t loc;
        peek_loc(peek_idx, &loc);
        if (loc.column == 1)
        {
            int line = loc.line;
            p = peek_loc(peek_idx + 1, &loc);
            if (loc.column == 2
                    && loc.line == line
                    && p == '$')
            {
                // Looks like a directive,
                // check the sentinel

                // Maybe this is a known sentinel, check 0 or 3 letters
                char sentinel[4] = { '\0', '\0', '\0', '\0' };
                int i;
                for (i = 0; i < 3; i++)
                {
                    p = peek_loc(peek_idx + 2 + i, &loc);
                    if (loc.column == (3 + i)
                            && loc.line == line)
                    {
                        sentinel[i] = p;
                    }
                }

                if (is_letter(sentinel[0])
                        && is_letter(sentinel[1])
                        && is_letter(sentinel[2]))
                {
                    if (is_known_sentinel_str(sentinel, /* out_sentinel */ NULL))
                    {
                        peek_loc(peek_idx + 5, &loc);
                        peek_insert(peek_idx + 5, ' ', loc);
                    }
                }
                else
                {
                    peek_loc(peek_idx + 2, &loc);
                    if (loc.column > 6
                            && loc.line == line
                            && !CURRENT_CONFIGURATION->disable_empty_sentinels)
                    {
                        peek_insert(peek_idx + 2, ' ', loc);
                    }
                }
            }
        }

        // Do not do anything else
        return;
    }

    // Read the whole statement until a newline
    // or ; (out of character-context)

    char got_label = 0;
    if (expect_label)
    {
        // Skip label (if any)
        token_location_t loc;
        p = peek_loc(peek_idx, &loc);
        while (loc.column < 6)
        {
            if (!is_decimal_digit(p))
            {
                error_printf_at(
                        make_locus(
                            loc.filename,
                            loc.line,
                            loc.column),
                        "invalid character in label field\n");
                break;
            }
            peek_idx++;
            p = peek_loc(peek_idx, &loc);
        }
        if (peek_idx > 0)
        {
            got_label = 1;
            peek_insert(peek_idx, ' ', loc);
            peek_idx++;
            p = peek(peek_idx);
        }
    }

    // Keep this for later
    int peek_idx_start_of_stmt = peek_idx;

    char delim = 0;
    char in_string = 0;

    char free_comma = 0;
    char free_equal = 0;
    char free_colon = 0;
    int parenthesis_level = 0;

    while (!is_newline(p)
            && p != EOF
            && ((p != ';'
                    && p != '!') || in_string))
    {
        if (p == '\'' || p == '"')
        {
            if (!in_string)
            {
                in_string = 1;
                delim = p;
            }
            else if (p == delim)
            {
                int p1 = peek(peek_idx + 1);
                if (p1 == p)
                {
                    // skip this extra delimiter
                    peek_idx++;
                }
                else
                {
                    in_string = 0;
                }
            }
        }
        else if (!in_string)
        {
            if (p == '(')
            {
                parenthesis_level++;
            }
            else if (p == ')')
            {
                // At this point the statement can be wrong so protect
                // parenthesis_level from becoming negative
                if (parenthesis_level > 0)
                    parenthesis_level--;
            }
            else if (parenthesis_level == 0)
            {
                if (p == ',')
                {
                    free_comma = 1;
                }
                else if (p == '=')
                {
                    free_equal = 1;
                }
                else if (p == ':')
                {
                    free_colon = 1;
                }
            }
        }

        peek_idx++;
        p = peek(peek_idx);
    }

    if (free_equal
            && !free_colon
            && !free_comma)
    {
        // This looks like an assignment expression
        // we do not have to do anything
        lexer_state.fixed_form.language_part = LANG_EXECUTABLE_PART;
        return;
    }

    // Now get the (longest) initial keyword
    tiny_dyncharbuf_t keyword;
    tiny_dyncharbuf_new(&keyword, 32);

    peek_idx = peek_idx_start_of_stmt;

    char done_with_keywords = 0;
    char allow_named_label = 1;
    while (!done_with_keywords)
    {
        p = peek(peek_idx);

        keyword.num = 0;
        struct fortran_keyword_tag *kw = NULL;
        int peek_idx_end_of_keyword = 0;
        if (is_letter(p))
        {
            while (is_letter(p)
                    || p == '_'
                    || is_decimal_digit(p))
            {
                tiny_dyncharbuf_add(&keyword, p);

                struct fortran_keyword_tag *t = fortran_keywords_lookup(keyword.buf, keyword.num);
                if (t != NULL)
                {
                    kw = t;
                    peek_idx_end_of_keyword = peek_idx;
                }

                peek_idx++;
                p = peek(peek_idx);
            }
        }

        if (!got_label
                && strncasecmp("include", keyword.buf, keyword.num) == 0)
        {
            // Note that INCLUDE is not a keyword
            // This is likely an include line, skip
            return;
        }

        if (p == ':'
                && keyword.num > 0
                && allow_named_label)
        {
            // FOO:IF(A>1)STOP1
            //    ^
            peek_idx++;
            allow_named_label = 0;
            // try again
            continue;
        }

        if (kw == NULL)
        {
            // Well, we could not classify this statement with any known
            // keyword, maybe there are no more keywords or it is an error
            break;
        }
        allow_named_label = 0;

        // Special keywords that deserve special treatment
        switch (kw->token_id)
        {
            default:
                {
                    // In general it is enough to just add a blank after the keyword
                    p = peek(peek_idx_end_of_keyword + 1);
                    if (is_letter(p) || is_decimal_digit(p))
                    {
                        token_location_t loc;
                        peek_loc(peek_idx_end_of_keyword + 1, &loc);
                        peek_insert(peek_idx_end_of_keyword + 1, ' ', loc); 
                        peek_idx = peek_idx_end_of_keyword + 2;
                    }
                    else
                    {
                        peek_idx = peek_idx_end_of_keyword + 1;
                    }

                    if (is_nonexecutable_statement(kw->token_id))
                    {
                        lexer_state.fixed_form.language_part = LANG_NONEXECUTABLE_PART;
                    }
                    else
                    {
                        lexer_state.fixed_form.language_part = LANG_EXECUTABLE_PART;
                    }

                    // Remember that this is an INTERFACE with generic-spec
                    if (kw->token_id == TOKEN_INTERFACE
                            && is_letter(p))
                    {
                        lexer_state.fixed_form.in_interface_generic_spec = 1;
                    }
                    else if (kw->token_id == TOKEN_ENDINTERFACE)
                    {
                        lexer_state.fixed_form.in_interface_generic_spec = 0;
                    }

                    // This keyword identifies this statement
                    done_with_keywords = 1;
                    break;
                }
            case TOKEN_MODULE:
                {
                    // Special case for MODULEidentifier vs MODULEPROCEDUREidentifier
                    // In general it is enough to just add a blank after the keyword
                    p = peek(peek_idx_end_of_keyword + 1);
                    if (is_letter(p) || is_decimal_digit(p))
                    {
                        token_location_t loc;
                        peek_loc(peek_idx_end_of_keyword + 1, &loc);
                        peek_insert(peek_idx_end_of_keyword + 1, ' ', loc); 
                        peek_idx = peek_idx_end_of_keyword + 2;
                    }
                    else
                    {
                        peek_idx = peek_idx_end_of_keyword + 1;
                    }

                    if (is_nonexecutable_statement(kw->token_id))
                    {
                        lexer_state.fixed_form.language_part = LANG_NONEXECUTABLE_PART;
                    }
                    else
                    {
                        lexer_state.fixed_form.language_part = LANG_EXECUTABLE_PART;
                    }

                    if (!lexer_state.fixed_form.in_interface_generic_spec)
                    {
                        // This keyword identifies this statement
                        done_with_keywords = 1;
                    }
                    // otherwise this may be a MODULE PROCEDURE
                    break;
                }
            case TOKEN_DO:
                {
                    // DO10E=1,100 must become DO 10 E=1,100
                    token_location_t loc;
                    peek_loc(peek_idx_end_of_keyword + 1, &loc);
                    peek_insert(peek_idx_end_of_keyword + 1, ' ', loc); 
                    peek_idx = peek_idx_end_of_keyword + 2;

                    p = peek(peek_idx);
                    if (is_decimal_digit(p))
                    {
                        while (is_decimal_digit(p))
                        {
                            peek_idx++;
                            p = peek(peek_idx);
                        }
                        peek_insert(peek_idx, ' ', loc); 
                        peek_idx++;
                    }

                    lexer_state.fixed_form.language_part = LANG_EXECUTABLE_PART;
                    done_with_keywords = 1;
                    break;
                }
            case TOKEN_INTEGER:
            case TOKEN_REAL:
            case TOKEN_COMPLEX:
            case TOKEN_LOGICAL:
            case TOKEN_CHARACTER:
            case TOKEN_DOUBLEPRECISION:
            case TOKEN_DOUBLECOMPLEX:
                {
                    p = peek(peek_idx_end_of_keyword + 1);
                    if (p == '*')
                    {
                        // INTEGER*8E1 cannot be lexed as
                        // [INTEGER][*][REAL_LITERAL=8E1] but like [INTEGER][*][INTEGER_LITERAL=8][IDENTIFIER=E1]
                        int peek_idx2 = peek_idx_end_of_keyword + 2;

                        p = peek(peek_idx2);
                        if (is_decimal_digit(p))
                        {
                            while (is_decimal_digit(p))
                            {
                                peek_idx2++;
                                p = peek(peek_idx2);
                            }

                            if (tolower(p) == 'e'
                                    || tolower(p) == 'd'
                                    || tolower(p) == 'q')
                            {
                                token_location_t loc;
                                peek_loc(peek_idx2, &loc);
                                peek_insert(peek_idx2, ' ', loc);
                                peek_idx = peek_idx2 + 2;
                            }
                            else if (kw->token_id == TOKEN_CHARACTER
                                    && p == ',')
                            {
                                // CHARACTER*10,
                                //             ^
                                // skip comma
                                peek_idx = peek_idx2 + 1;
                            }
                            else
                            {
                                // regular case
                                peek_idx = peek_idx2;
                            }
                        }
                        else if (kw->token_id == TOKEN_CHARACTER
                                && p == '(')
                        {
                            // CHARACTER*(10)
                            //           ^
                            peek_idx2 = preanalyze_advance_parenthesis(peek_idx2);
                            peek_idx2++;

                            p = peek(peek_idx2);
                            if (p == ',')
                            {
                                // CHARACTER*(10),
                                //               ^
                                // skip comma
                                peek_idx2++;
                            }

                            peek_idx = peek_idx2;
                        }
                        else
                        {
                            // Unexpected case
                            peek_idx = peek_idx2;
                        }
                    }
                    else if (is_letter(p))
                    {
                        token_location_t loc;
                        peek_loc(peek_idx_end_of_keyword, &loc);
                        peek_insert(peek_idx_end_of_keyword + 1, ' ', loc); 
                        peek_idx = peek_idx_end_of_keyword + 2;
                    }
                    else if (p == '(')
                    {
                        int peek_idx2 = peek_idx_end_of_keyword + 1;
                        //INTEGER(8)A
                        //       ^
                        // Advance parentheses of the kind
                        peek_idx2 = preanalyze_advance_parenthesis(peek_idx2);

                        peek_idx = peek_idx2 + 1;
                    }
                    else
                    {
                        // Unexpected case
                        peek_idx = peek_idx_end_of_keyword + 1;
                    }

                    if (lexer_state.fixed_form.language_part == LANG_NONEXECUTABLE_PART)
                    {
                        // If this is nonexecutable, we will assume that we are
                        // seeing a type-decl-statement, so this keyword
                        // identifies this statement
                        done_with_keywords = 1;
                    }
                    else
                    {
                        p = peek(peek_idx);
                        if (is_letter(p))
                        {
                            // If we were in the executable part (or top
                            // level!) we will could be seing INTEGER FUNCTION
                            // F so the keyword does not identify that
                            // statement (instead FUNCTION would)
                            //
                            // But we will ensure that the next is permissible in this
                            // case, otherwise
                            //
                            // INTEGER*1VALUES(4)
                            //
                            // will be tokenized as INTEGER*1 VALUE S(4) which is nonsense

                            tiny_dyncharbuf_t next_keyword;
                            tiny_dyncharbuf_new(&next_keyword, 16);

                            int peek_idx2 = peek_idx;
                            p = peek(peek_idx2);

                            char next_kw_is_ok = 0;
                            while (is_letter(p)
                                    && !next_kw_is_ok)
                            {
                                tiny_dyncharbuf_add(&next_keyword, p);

                                struct fortran_keyword_tag *t =
                                    fortran_keywords_lookup(next_keyword.buf, next_keyword.num);
                                if (t != NULL)
                                {
                                    switch (t->token_id)
                                    {
                                        case TOKEN_FUNCTION:
                                        case TOKEN_PURE:
                                        case TOKEN_ELEMENTAL:
                                        case TOKEN_IMPURE:
                                        case TOKEN_RECURSIVE:
                                            // This seems OK at this point
                                            next_kw_is_ok = 1;
                                            break;
                                        default:
                                            // Wait and see
                                            break;
                                    }
                                }

                                peek_idx2++;
                                p = peek(peek_idx2);
                            }

                            DELETE(next_keyword.buf);

                            if (!next_kw_is_ok)
                            {
                                // Not followed by an acceptable keyword, the
                                // current keyword identifies this statement
                                done_with_keywords = 1;
                            }
                        }
                        else
                        {
                            // Not followed by a letter, this keyword identifies this statement
                            done_with_keywords = 1;
                        }
                    }
                    break;
                }
            case TOKEN_PURE:
            case TOKEN_ELEMENTAL:
            case TOKEN_IMPURE:
            case TOKEN_RECURSIVE:
                {
                    // None of these keywords starts a statement of their own
                    // (i.e. there is no PURE-statement), so just add a blank
                    // if needed and continue
                    p = peek(peek_idx_end_of_keyword + 1);
                    if (is_letter(p))
                    {
                        token_location_t loc;
                        peek_loc(peek_idx_end_of_keyword, &loc);
                        peek_insert(peek_idx_end_of_keyword + 1, ' ', loc); 
                        peek_idx = peek_idx_end_of_keyword + 2;
                    }
                    break;
                }
            case TOKEN_IF:
            case TOKEN_ELSEIF:
                {
                    // These need an extra blank after the keyword that follows
                    // the closing parenthesis
                    // Note that IF/ELSEIF keyword identify their statements,
                    // but unfortunately they may be composed of another statement/label so
                    // we need to identify both, to do this we just advance the peek index
                    // after the parentheses
                    p = peek(peek_idx_end_of_keyword + 1);
                    if (p == '(')
                    {
                        // IF(A)STOP1
                        //   ^
                        // IF(A)THENLABEL
                        //   ^
                        // ELSEIF(B)THENLABEL
                        //       ^

                        int peek_idx2 = peek_idx_end_of_keyword + 1;
                        peek_idx2 = preanalyze_advance_parenthesis(peek_idx2);

                        peek_idx = peek_idx2 + 1;
                    }
                    lexer_state.fixed_form.language_part = LANG_EXECUTABLE_PART;
                    break;
                }
            case TOKEN_ASSIGN:
                {
                    // ASSIGN 100 TO I we need to add a blank after ASSIGN and
                    // move until TO so another blank is added after it
                    //
                    // Note: we could add another blank imemdiately before TO
                    // but TO cannot be the suffix of an integer constant
                    token_location_t loc;
                    peek_loc(peek_idx_end_of_keyword, &loc);
                    peek_insert(peek_idx_end_of_keyword + 1, ' ', loc); 
                    peek_idx = peek_idx_end_of_keyword + 2;

                    p = peek(peek_idx);
                    if (is_decimal_digit(p))
                    {
                        // Advance right before TO
                        while (is_decimal_digit(p))
                        {
                            peek_idx++;
                            p = peek(peek_idx);
                        }
                    }
                    else
                    {
                        // Prudently stop here
                        done_with_keywords = 1;
                    }

                    lexer_state.fixed_form.language_part = LANG_EXECUTABLE_PART;
                    break;
                }
                /* find default case at the beginning */
        }
    }

    DELETE(keyword.buf);
}

static inline char is_known_sentinel(char** sentinel)
{
    int c;
    // c = peek(0); // $
    get();

    tiny_dyncharbuf_t tmp_sentinel;
    tiny_dyncharbuf_new(&tmp_sentinel, 4);

    c = peek(0);
    while ((is_letter(c)
                || is_decimal_digit(c)
                || c == '_'))
    {
        tiny_dyncharbuf_add(&tmp_sentinel, c);
        get();
        c = peek(0);
    }
    tiny_dyncharbuf_add(&tmp_sentinel, '\0');

    const char *out_sentinel = NULL;
    char found = is_known_sentinel_str(tmp_sentinel.buf, &out_sentinel);

    if (!found)
    {
        *sentinel = tmp_sentinel.buf;
        return 0;
    }
    else
    {
        DELETE(tmp_sentinel.buf);
        *sentinel = xstrdup(out_sentinel);
        return 1;
    }
}

static const char* format_pragma_string(const char* c)
{
    char *tmp = xstrdup(c);

    char* p = tmp;

    while (*p != '\0')
    {
        if (*p == '|')
            *p = ' ';
        p++;
    }

    return tmp;
}

static int compute_length_match(const char* lexed_directive,
        const char* available_directive,
        const char **discard_source)
{
    *discard_source = NULL;
    const char *p = lexed_directive;
    const char *q = available_directive;

    while (*p != '\0'
            && *q != '\0')
    {
        if (*q == '|')
        {
            // A '|' should match like [[:blank:]]* 
            while (*p == ' ' || *p == '\t')
                p++;
            // We advanced too much
            p--;
        }
        else if (*q != tolower(*p))
        {
            return 0;
        }
        q++;
        p++;
    }

    *discard_source = p;
    return (q - available_directive);
}

static const char* return_pragma_prefix_longest_match_inner(pragma_directive_set_t* pragma_directive_set,
        const char* lexed_directive,
        const char **discard_source,
        pragma_directive_kind_t* directive_kind)
{
    const char* longest_match_so_far = NULL;
    int length_match = 0;

    int j;
    char exact_match = 0;
    int size_lexed_directive = strlen(lexed_directive);
    for (j = 0; j < pragma_directive_set->num_directives && !exact_match; j++)
    {
        const char * current_discard_source = NULL;

        int current_match = compute_length_match(lexed_directive, pragma_directive_set->directive_names[j],
                &current_discard_source);

        if (current_match >= length_match && current_match != 0)
        {
            int size_directive = strlen(pragma_directive_set->directive_names[j]);
            if (current_match == size_lexed_directive && size_lexed_directive == size_directive)
            {
                exact_match = 1;
            }
            length_match = current_match;
            longest_match_so_far = pragma_directive_set->directive_names[j];
            *discard_source = current_discard_source;
            *directive_kind = pragma_directive_set->directive_kinds[j];
        }
    }

    return longest_match_so_far;
}

static const char* return_pragma_prefix_longest_match(
        const char* prefix, 
        char is_end_directive,
        const char* lexed_directive,

        // out
        char** relevant_directive,
        pragma_directive_kind_t* kind)
{
    const char* longest_match = NULL;
    const char* discard_source = NULL;

    const char* looked_up_directive = lexed_directive;
    if (is_end_directive)
    {
        // skip 'end'
        looked_up_directive += 3;
        while (is_blank(*looked_up_directive))
            looked_up_directive++;
    }

    int i;
    for (i = 0; i < CURRENT_CONFIGURATION->num_pragma_custom_prefix; i++)
    {
        if (strcmp(CURRENT_CONFIGURATION->pragma_custom_prefix[i], prefix) == 0)
        {
            pragma_directive_set_t* pragma_directive_set = CURRENT_CONFIGURATION->pragma_custom_prefix_info[i];
            longest_match = return_pragma_prefix_longest_match_inner(pragma_directive_set, 
                    looked_up_directive, &discard_source, kind);
        }
    }

    const char *start = lexed_directive;
    const char *end = discard_source;

    if (longest_match != NULL)
    {
        ERROR_CONDITION(start == NULL || end == NULL || (end <= start),
                "Invalid values for the cursors", 0);
        // Keep the relevant directive of the lexed input
        *relevant_directive = NEW_VEC(char, (end - start + 1));
        strncpy(*relevant_directive, start, end - start);
        (*relevant_directive)[end - start] = '\0';

        // Now advance the token stream
        // The first letter is always already consumed
        start++;

        while (start != end)
        {
            start++;
            get();
        }
    }

    return longest_match;
}

// This is the lexer <-> parser interface from yacc/bison
// this function just returns the next token from the current
// input stream
extern int new_mf03lex(void)
{
    for (;;)
    {
        int c0;
        token_location_t loc;

        // We are forced to peek because we have to return
        // an artificial token for nonblock labels
        if (lexer_state.substate == LEXER_SUBSTATE_NORMAL
                && lexer_state.last_eos)
        {
            if (lexer_state.form == LEXER_TEXTUAL_FIXED_FORM)
            {
                preanalyze_statement(/* expect_label */ 1);
            }

            if (lexer_state.num_nonblock_labels > 0
                    // peek here
                    && is_decimal_digit(c0 = peek_loc(0, &loc)))
            {

                char label_str[6];
                label_str[0] = c0;

                int i = 1;
                int peek_idx = 1;
                int c = peek(peek_idx);
                while (i < 5
                        && is_decimal_digit(c))
                {
                    label_str[i] = c;
                    i++;
                    peek_idx++;
                    c = peek(peek_idx);
                }

                label_str[i] = '\0';

                if (!is_decimal_digit(c))
                {
                    int label = atoi(label_str);
                    if (lexer_state.nonblock_labels_stack[lexer_state.num_nonblock_labels - 1] == label)
                    {
                        lexer_state.num_nonblock_labels--;
                        return commit_text(TOKEN_END_NONBLOCK_DO, label_str, loc);
                    }
                }
            }
        }

        c0 = get_loc(&loc);
        if (lexer_state.substate == LEXER_SUBSTATE_NORMAL)
        {
            switch (c0)
            {
                case EOF:
                    {
                        char emit_extra_eos = 0;
                        char end_of_scan = process_end_of_file(&emit_extra_eos);

                        if (emit_extra_eos)
                        {
                            return commit_text(EOS, NULL, get_current_location());
                        }

                        if (end_of_scan)
                        {
                            return 0;
                        }
                        else
                            continue;
                    }
                case '#':
                    {
                        if (!lexer_state.bol)
                            break;

                        if (handle_preprocessor_line())
                        {
                            lexer_state.bol = 1;
                            continue;
                        }

                        break;
                    }
                case ' ':
                case '\t':
                    {
                        // Whitespace ignored
                        continue;
                    }
                case '\n':
                case '\r':
                    {
                        // Regarding \r\n in DOS, the get function will always skip \n if it finds it right after \r
                        if (!lexer_state.last_eos)
                        {
                            int n = commit_text(EOS, NULL, loc);
                            lexer_state.sentinel = NULL;
                            lexer_state.bol = 1;
                            return n;
                        }
                        else
                        {
                            if (mf03_flex_debug)
                            {
                                fprintf(stderr, "SKIPPING NEWLINE %s:%d:%d\n",
                                        loc.filename,
                                        loc.line,
                                        loc.column);
                            }
                            lexer_state.sentinel = NULL;
                            lexer_state.bol = 1;
                        }
                        continue;
                    }
                case '!':
                    {
                        // comment
                        c0 = peek(0);
                        if (c0 == '$')
                        {
                            int c1 = peek(1);
                            if (is_blank(c1))
                            {
                                // Conditional compilation
                                if (!CURRENT_CONFIGURATION->disable_empty_sentinels)
                                {
                                    get(); // $
                                    get(); // <<blank>>
                                    lexer_state.sentinel = "";
                                    continue;
                                }
                                // otherwise handle it as if it were a comment
                            }
                            else if (is_letter(c1)
                                    || is_decimal_digit(c1)
                                    || c1 == '_')
                            {
                                char* sentinel = NULL;
                                if (is_known_sentinel(&sentinel))
                                {
                                    lexer_state.substate = LEXER_SUBSTATE_PRAGMA_DIRECTIVE;
                                    lexer_state.sentinel = xstrdup(sentinel);
                                    return commit_text_and_free(PRAGMA_CUSTOM, sentinel, loc);
                                }
                                else
                                {
                                    warn_printf_at(
                                            make_locus(
                                                loc.filename,
                                                loc.line,
                                                loc.column),
                                            "ignoring unknown '!$%s' directive\n",
                                            sentinel);

                                    // Return an UNKNOWN_PRAGMA
                                    int sentinel_length = strlen(sentinel);
                                    int prefix_length = /* !$ */ 2 + sentinel_length;

                                    tiny_dyncharbuf_t str;
                                    tiny_dyncharbuf_new(&str, prefix_length + 32);

                                    tiny_dyncharbuf_add_str(&str, "!$");
                                    tiny_dyncharbuf_add_str(&str, sentinel);

                                    DELETE(sentinel);

                                    // Everything will be scanned as a single token
                                    int c = peek(0);
                                    while (!is_newline(c))
                                    {
                                        tiny_dyncharbuf_add(&str, c);
                                        get();
                                        c = peek(0);
                                    }
                                    tiny_dyncharbuf_add(&str, '\0');
                                    return commit_text_and_free(UNKNOWN_PRAGMA, str.buf, loc);
                                }
                            }
                        }

                        while (!is_newline(c0))
                        {
                            get();
                            c0 = peek(0);
                        }

                        if (c0 == '\r')
                        {
                            if (peek(1) == '\n')
                                get();
                        }

                        // we are now right before \n (or \r)
                        continue;
                    }
                case ';':
                    {
                        if (!lexer_state.last_eos)
                        {
                            return commit_text(EOS, NULL, loc);
                        }
                        if (mf03_flex_debug)
                        {
                            fprintf(stderr, "SKIPPING SEMICOLON %s:%d:%d\n",
                                    loc.filename,
                                    loc.line,
                                    loc.column);
                        }
                        continue;
                    }
                case '(' :
                    {
                        if (lexer_state.in_format_statement)
                        {
                            tiny_dyncharbuf_t str;
                            tiny_dyncharbuf_new(&str, 32);

                            tiny_dyncharbuf_add(&str, c0);

                            char delim = 0;
                            int level = 1;
                            char in_string = 0;

                            // Everything will be scanned as a single token
                            int c = peek(0);
                            while (!is_newline(c)
                                    && (level > 0))
                            {
                                if (!in_string)
                                {
                                    if (c == '(')
                                    {
                                        level++;
                                    }
                                    else if (c == ')')
                                    {
                                        level--;
                                    }
                                    else if (c == '\'' || c == '"')
                                    {
                                        delim = c;
                                        in_string = 1;
                                    }
                                }
                                else if (c == delim)
                                {
                                    int c1 = peek(1);
                                    if (c1 != delim)
                                    {
                                        in_string = 0;
                                    }
                                    else
                                    {
                                        tiny_dyncharbuf_add(&str, c);
                                        // skip delimiter
                                        get();
                                    }
                                }
                                tiny_dyncharbuf_add(&str, c);
                                get();
                                c = peek(0);
                            }
                            tiny_dyncharbuf_add(&str, '\0');

                            lexer_state.in_format_statement = 0;
                            return commit_text_and_free(FORMAT_SPEC, str.buf, loc);
                        }

                        int c1 = peek(0);
                        if (c1 == '/')
                        {
                            get();
                            return commit_text(TOKEN_LPARENT_SLASH, "(/", loc);
                        }
                        else
                        {
                            return commit_text('(', "(", loc);
                        }
                    }
                case '/' :
                    {
                        int c1 = peek(0);
                        if (c1 == '/')
                        {
                            get();
                            return commit_text(TOKEN_DOUBLE_SLASH, "//", loc);
                        }
                        else if (c1 == '=')
                        {
                            get();
                            return commit_text(TOKEN_NOT_EQUAL, "/=", loc);
                        }
                        else if (c1 == ')')
                        {
                            get();
                            return commit_text(TOKEN_SLASH_RPARENT, "/)", loc);
                        }
                        else
                            return commit_text('/', "/", loc);
                    }
                case ')' :
                    {
                        const char s[] = {c0, '\0'};
                        return commit_text(c0, s, loc);
                    }
                case '[' :
                case ']' :
                case ',' :
                case '%' :
                case '+' :
                case '-' :
                case ':' :
                case '{' :
                case '}' :
                    {
                        const char s[] = {c0, '\0'};
                        return commit_text(c0, s, loc);
                    }
                case '*' :
                    {
                        int c1 = peek(0);
                        if (c1 == '*')
                        {
                            get();
                            return commit_text(TOKEN_RAISE, "**", loc);
                        }
                        else
                        {
                            return commit_text('*', "*", loc);
                        }
                    }
                case '<' :
                    {
                        int c1 = peek(0);
                        if (c1 == '=')
                        {
                            get();
                            return commit_text(TOKEN_LOWER_OR_EQUAL_THAN, "<=", loc);
                        }
                        else
                        {
                            return commit_text(TOKEN_LOWER_THAN, "<", loc);
                        }
                    }
                case '>' :
                    {
                        int c1 = peek(0);
                        if (c1 == '=')
                        {
                            get();
                            return commit_text(TOKEN_GREATER_OR_EQUAL_THAN, ">=", loc);
                        }
                        else
                        {
                            return commit_text(TOKEN_GREATER_THAN, ">", loc);
                        }
                    }
                case '=':
                    {
                        int c1 = peek(0);
                        if (c1 == '=')
                        {
                            get();
                            return commit_text(TOKEN_EQUAL, "==", loc);
                        }
                        else if (c1 == '>')
                        {
                            get();
                            return commit_text(TOKEN_POINTER_ACCESS, "=>", loc);
                        }
                        else
                        {
                            return commit_text('=', "=", loc);
                        }
                    }
                case '.':
                    {
                        int c1 = peek(0);
                        int c2 = peek(1);
                        if (tolower(c1) == 'e'
                                && tolower(c2) == 'q')
                        {
                            int c3 = peek(2);
                            int c4 = peek(3);
                            if (c3 == '.')
                            {
                                get(); // e
                                get(); // q
                                get(); // .
                                return commit_text(TOKEN_EQUAL, "==", loc);
                            }
                            else if (tolower(c3) == 'v'
                                    && c4 == '.')
                            {
                                char str[sizeof(".eqv.")];
                                str[0] = c0;    // .
                                str[1] = get(); // e
                                str[2] = get(); // q
                                str[3] = get(); // v
                                str[4] = get(); // .
                                str[5] = '\0';
                                return commit_text(TOKEN_LOGICAL_EQUIVALENT, str, loc);
                            }
                        }
                        else if (tolower(c1) == 'n')
                        {
                            if (tolower(c2) == 'e')
                            {
                                int c3 = peek(2);
                                int c4 = peek(3);
                                int c5 = peek(4);
                                if (c3 == '.')
                                {
                                    get(); // n
                                    get(); // e
                                    get(); // .
                                    return commit_text(TOKEN_NOT_EQUAL, "/=", loc);
                                }
                                else if (tolower(c3) == 'q'
                                        && tolower(c4) == 'v'
                                        && c5 == '.')
                                {
                                    char str[sizeof(".neqv.")];
                                    str[0] = c0;    // .
                                    str[1] = get(); // n
                                    str[2] = get(); // e
                                    str[3] = get(); // q
                                    str[4] = get(); // v
                                    str[5] = get(); // .
                                    str[6] = '\0';
                                    return commit_text(TOKEN_LOGICAL_NOT_EQUIVALENT, str, loc);
                                }
                            }
                            else if (tolower(c2) == 'o')
                            {
                                int c3 = peek(2);
                                int c4 = peek(3);
                                if (tolower(c3) == 't'
                                        && c4 == '.')
                                {
                                    char str[sizeof(".not.")];
                                    str[0] = c0;    // .
                                    str[1] = get(); // n
                                    str[2] = get(); // o
                                    str[3] = get(); // t
                                    str[4] = get(); // .
                                    str[5] = '\0';
                                    return commit_text(TOKEN_LOGICAL_NOT, str, loc);
                                }
                            }
                        }
                        else if (tolower(c1) == 'l')
                        {
                            int c3 = peek(2);
                            if (tolower(c2) == 'e'
                                    && c3 == '.')
                            {
                                get(); // l
                                get(); // e
                                get(); // .
                                return commit_text(TOKEN_LOWER_OR_EQUAL_THAN, "<=", loc);
                            }
                            else if (tolower(c2) == 't'
                                    && c3 == '.')
                            {
                                get(); // l
                                get(); // t
                                get(); // .
                                return commit_text(TOKEN_LOWER_THAN, "<", loc);
                            }
                        }
                        else if (tolower(c1) == 'g')
                        {
                            int c3 = peek(2);
                            if (tolower(c2) == 'e'
                                    && c3 == '.')
                            {
                                get(); // g
                                get(); // e
                                get(); // .
                                return commit_text(TOKEN_GREATER_OR_EQUAL_THAN, ">=", loc);
                            }
                            else if (tolower(c2) == 't'
                                    && c3 == '.')
                            {
                                get(); // g
                                get(); // t
                                get(); // .
                                return commit_text(TOKEN_GREATER_THAN, ">", loc);
                            }
                        }
                        else if (tolower(c1) == 'o'
                                && tolower(c2) == 'r')
                        {
                            int c3 = peek(2);
                            if (c3 == '.')
                            {
                                char str[sizeof(".or.")];
                                str[0] = c0;    // .
                                str[1] = get(); // o
                                str[2] = get(); // r
                                str[3] = get(); // .
                                str[4] = '\0';
                                return commit_text(TOKEN_LOGICAL_OR, str, loc);
                            }
                        }
                        else if (tolower(c1) == 'a'
                                && tolower(c2) == 'n')
                        {
                            int c3 = peek(2);
                            int c4 = peek(3);
                            if (tolower(c3) == 'd'
                                    && c4 == '.')
                            {
                                char str[sizeof(".and.")];
                                str[0] = c0;    // .
                                str[1] = get(); // a
                                str[2] = get(); // n
                                str[3] = get(); // d
                                str[4] = get(); // .
                                str[5] = '\0';
                                return commit_text(TOKEN_LOGICAL_AND, str, loc);
                            }
                        }
                        else if (tolower(c1) == 't'
                                && tolower(c2) == 'r'
                                && tolower(peek(2)) == 'u'
                                && tolower(peek(3)) == 'e'
                                && tolower(peek(4)) == '.')
                        {
                            char str[6 + 1];
                            str[0] = '.';
                            str[1] = get(); // t
                            str[2] = get(); // r
                            str[3] = get(); // u
                            str[4] = get(); // e
                            str[5] = get(); // .
                            str[6] = '\0';

                            int c = peek(0);
                            if (c == '_')
                            {
                                char* kind_str = scan_kind();

                                tiny_dyncharbuf_t t_str;
                                tiny_dyncharbuf_new(&t_str, 32);

                                tiny_dyncharbuf_add_str(&t_str, str);
                                tiny_dyncharbuf_add_str(&t_str, kind_str);
                                DELETE(kind_str);
                                tiny_dyncharbuf_add(&t_str, '\0');

                                return commit_text_and_free(TOKEN_TRUE, t_str.buf, loc);
                            }
                            else
                            {
                                return commit_text(TOKEN_TRUE, str, loc);
                            }
                        }
                        else if (tolower(c1) == 'f'
                                && tolower(c2) == 'a'
                                && tolower(peek(2)) == 'l'
                                && tolower(peek(3)) == 's'
                                && tolower(peek(4)) == 'e'
                                && tolower(peek(5)) == '.')
                        {
                            char str[7 + 1];
                            str[0] = '.';
                            str[1] = get(); // f
                            str[2] = get(); // a
                            str[3] = get(); // l
                            str[4] = get(); // s
                            str[5] = get(); // e
                            str[6] = get(); // .
                            str[7] = '\0';

                            int c = peek(0);
                            if (c == '_')
                            {
                                char* kind_str = scan_kind();

                                tiny_dyncharbuf_t t_str;
                                tiny_dyncharbuf_new(&t_str, 32);

                                tiny_dyncharbuf_add_str(&t_str, str);
                                tiny_dyncharbuf_add_str(&t_str, kind_str);
                                DELETE(kind_str);
                                tiny_dyncharbuf_add(&t_str, '\0');

                                return commit_text_and_free(TOKEN_FALSE, t_str.buf, loc);
                            }
                            else
                            {
                                return commit_text(TOKEN_FALSE, str, loc);
                            }
                        }
                        else if (is_decimal_digit(c1))
                        {
                            char* fractional_part = scan_fractional_part_of_real_literal();
                            return commit_text_and_free(REAL_LITERAL, fractional_part, loc);
                        }

                        if (is_letter(c1))
                        {
                            tiny_dyncharbuf_t user_def_op;
                            tiny_dyncharbuf_new(&user_def_op, 32);

                            tiny_dyncharbuf_add(&user_def_op, c0);
                            tiny_dyncharbuf_add(&user_def_op, c1);
                            get();

                            int c = c2;
                            while (c != '.'
                                    && is_letter(c))
                            {
                                tiny_dyncharbuf_add(&user_def_op, c);
                                get();
                                c = peek(0);
                            }

                            if (c != '.')
                            {
                                error_printf_at(
                                        make_locus(
                                            loc.filename,
                                            loc.line,
                                            loc.column),
                                        "unended user-defined operator name\n");
                            }
                            else
                            {
                                get(); // .
                            }

                            tiny_dyncharbuf_add(&user_def_op, '.');
                            tiny_dyncharbuf_add(&user_def_op, '\0');
                            return commit_text_and_free(USER_DEFINED_OPERATOR, user_def_op.buf, loc); // '.[a-z].'
                        }
                        break;
                    }
                    // string literals
                case '"':
                case '\'':
                    {
                        char prefix[2] = {c0, '\0'};

                        int token_id = 0;
                        char *text = NULL;

                        scan_character_literal(prefix, /* delim */ c0, /* allow_suffix_boz */ 1, loc, &token_id, &text);

                        return commit_text_and_free(token_id, text, loc);
                        break;
                    }
                    // letters
                case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H':
                case 'I': case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P':
                case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
                case 'Y': case 'Z':
                case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h':
                case 'i': case 'j': case 'k': case 'l': case 'm': case 'n': case 'o': case 'p':
                case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x':
                case 'y': case 'z':
                    {
                        if (lexer_state.bol
                                && (tolower(c0) == 'i'))
                        {
                            // Maybe this is an include line
                            if (is_include_line())
                                continue;
                        }

                        if (tolower(c0) == 'b'
                                || tolower(c0) == 'o'
                                || tolower(c0) == 'z'
                                || tolower(c0) == 'x')
                        {
                            int c1 = peek(0);

                            if (c1 == '\'' || c1 == '"')
                            {
                                tiny_dyncharbuf_t str;
                                tiny_dyncharbuf_new(&str, 32);

                                tiny_dyncharbuf_add(&str, c0);
                                tiny_dyncharbuf_add(&str, c1);
                                get();

                                int token_id = 0;
                                token_location_t loc2 = loc;
                                int c;
                                int length = 0;
                                switch (c0)
                                {
                                    case 'b': case 'B':
                                        {
                                            token_id = BINARY_LITERAL;
                                            c = peek_loc(0, &loc2);
                                            while (c != c1
                                                    && is_binary_digit(c))
                                            {
                                                tiny_dyncharbuf_add(&str, c);
                                                get();
                                                c = peek_loc(0, &loc2);
                                                length++;
                                            }

                                            if (c != c1
                                                    && !is_newline(c))
                                            {
                                                error_printf_at(
                                                        make_locus(
                                                            loc2.filename,
                                                            loc2.line,
                                                            loc2.column),
                                                        "invalid binary digit\n");
                                            }
                                            break;
                                        }
                                    case 'o' : case 'O':
                                        {
                                            token_id = OCTAL_LITERAL;
                                            c = peek_loc(0, &loc2);
                                            while (c != c1
                                                    && is_octal_digit(c))
                                            {
                                                tiny_dyncharbuf_add(&str, c);
                                                get();
                                                c = peek_loc(0, &loc2);
                                                length++;
                                            }

                                            if (c != c1
                                                    && !is_newline(c))
                                            {
                                                error_printf_at(
                                                        make_locus(
                                                            loc2.filename,
                                                            loc2.line,
                                                            loc2.column),
                                                        "invalid octal digit\n");
                                            }
                                            break;
                                        }
                                    case 'x': case 'X':
                                    case 'z': case 'Z':
                                        {
                                            token_id = HEX_LITERAL;
                                            c = peek_loc(0, &loc2);
                                            while (c != c1
                                                    && is_hex_digit(c))
                                            {
                                                tiny_dyncharbuf_add(&str, c);
                                                get();
                                                c = peek_loc(0, &loc2);
                                                length++;
                                            }


                                            if (c != c1
                                                    && !is_newline(c))
                                            {
                                                error_printf_at(
                                                        make_locus(
                                                            loc2.filename,
                                                            loc2.line,
                                                            loc2.column),
                                                        "invalid hexadecimal digit\n");
                                            }
                                            break;
                                        }
                                    default:
                                        internal_error("Code unreachable", 0);
                                }

                                if (c == c1)
                                {
                                    tiny_dyncharbuf_add(&str, c1);
                                    get();

                                    if (length == 0)
                                    {
                                        error_printf_at(
                                                make_locus(
                                                    loc2.filename,
                                                    loc2.line,
                                                    loc2.column),
                                                "empty integer literal\n");

                                        tiny_dyncharbuf_add(&str, 0);
                                        tiny_dyncharbuf_add(&str, c1);
                                    }

                                    c = peek(0);
                                    if (c == '_')
                                    {
                                        char *kind_str = scan_kind();
                                        tiny_dyncharbuf_add_str(&str, kind_str);
                                    }
                                }
                                else
                                {
                                    error_printf_at(
                                            make_locus(
                                                loc2.filename,
                                                loc2.line,
                                                loc2.column),
                                            "unended integer literal\n");
                                    tiny_dyncharbuf_add(&str, c1);
                                }

                                tiny_dyncharbuf_add(&str, '\0');
                                return commit_text(token_id, str.buf, loc);
                            }
                        }

                        int token_id = IDENTIFIER;
                        tiny_dyncharbuf_t identifier;
                        tiny_dyncharbuf_new(&identifier, 32);
                        tiny_dyncharbuf_add(&identifier, c0);

                        // peek as many letters as possible
                        int c = peek(0);
                        while (is_letter(c)
                                || is_decimal_digit(c)
                                || (c == '_'
                                    && peek(1) != '\''
                                    && peek(1) != '"'))
                        {
                            tiny_dyncharbuf_add(&identifier, c);
                            get();
                            c = peek(0);
                        }
                        tiny_dyncharbuf_add(&identifier, '\0');

                        int c2 = peek(1);
                        if (c == '_'
                                && (c2 == '\''
                                    || c2 == '"'))
                        {
                            int c1 = c;
                            get();
                            get();

                            tiny_dyncharbuf_t t_str;
                            tiny_dyncharbuf_new(&t_str, strlen(identifier.buf) + 32 + 1);

                            tiny_dyncharbuf_add_str(&t_str, identifier.buf);
                            DELETE(identifier.buf);

                            tiny_dyncharbuf_add(&t_str, c1); // _
                            tiny_dyncharbuf_add(&t_str, c2); // " or '
                            tiny_dyncharbuf_add(&t_str, '\0');

                            char *text = NULL;
                            scan_character_literal(t_str.buf, /* delim */ c2, /* allow_suffix_boz */ 0, loc,
                                    &token_id, &text);
                            DELETE(t_str.buf);

                            return commit_text_and_free(token_id, text, loc);
                        }

                        struct fortran_keyword_tag *result =
                            fortran_keywords_lookup(identifier.buf, strlen(identifier.buf));

                        ERROR_CONDITION(lexer_state.in_format_statement
                                && (result == NULL
                                    || result->token_id != TOKEN_FORMAT),
                                "Invalid token for format statement", 0);

                        if (result != NULL)
                        {
                            token_id = result->token_id;
                        }

                        if (token_id == TOKEN_DO)
                        {
                            // Special treatment required for nonblock DO constructs
                            // so the label is properly matched
                            int peek_idx = 0;

                            c = peek(peek_idx);
                            while (is_blank(c))
                            {
                                peek_idx++;
                                c = peek(peek_idx);
                            }

                            if (is_decimal_digit(c))
                            {
                                lexer_state.in_nonblock_do_construct = 1;
                            }
                        }

                        return commit_text_and_free(token_id, identifier.buf, loc);
                    }
                    // Numbers
                case '0': case '1': case '2': case '3': case '4':
                case '5': case '6': case '7': case '8': case '9':
                    {
                        tiny_dyncharbuf_t digits;
                        tiny_dyncharbuf_new(&digits, 32);

                        tiny_dyncharbuf_add(&digits, c0);

                        int c = peek(0);
                        while (is_decimal_digit(c))
                        {
                            tiny_dyncharbuf_add(&digits, c);
                            get();
                            c = peek(0);
                        }
                        tiny_dyncharbuf_add(&digits, '\0');

                        if (tolower(c) == 'e'
                                || tolower(c) == 'd'
                                || tolower(c) == 'q')
                        {
                            char* fractional_part = scan_fractional_part_of_real_literal();

                            tiny_dyncharbuf_t t_str;
                            tiny_dyncharbuf_new(&t_str, strlen(digits.buf) + strlen(fractional_part) + 1);
                            tiny_dyncharbuf_add_str(&t_str, digits.buf);
                            DELETE(digits.buf);
                            tiny_dyncharbuf_add_str(&t_str, fractional_part);
                            DELETE(fractional_part);

                            tiny_dyncharbuf_add(&t_str, '\0');

                            return commit_text_and_free(REAL_LITERAL, t_str.buf, loc);
                        }
                        else if (c == '.')
                        {
                            // There are two cases here
                            //   1.op. must be tokenized as DECIMAL_LITERAL USER_DEFINED_OPERATOR
                            //   1.2   must be tokenized as REAL_LITERAL
                            // note that
                            //     1.e.2 is the first case
                            //     1.e+2 is the second case

                            // Check for 1.op.
                            int peek_idx = 1;
                            char d = peek(peek_idx);
                            while (is_letter(d)
                                    && peek_idx <= 32) // operator-names are limited to 32
                            {
                                peek_idx++;
                                d = peek(peek_idx);
                            }

                            if (d == '.'
                                    && peek_idx > 1)
                            {
                                // This is case 1.op.
                                return commit_text_and_free(DECIMAL_LITERAL, digits.buf, loc);
                            }
                            else
                            {
                                // scan the fractional part of a real
                                get(); // we must be past the '.' for scan_fractional_part_of_real_literal
                                char* fractional_part = scan_fractional_part_of_real_literal();

                                tiny_dyncharbuf_t t_str;
                                tiny_dyncharbuf_new(&t_str, strlen(digits.buf) + strlen(fractional_part) + 1);
                                tiny_dyncharbuf_add_str(&t_str, digits.buf);
                                DELETE(digits.buf);
                                tiny_dyncharbuf_add_str(&t_str, fractional_part);
                                DELETE(fractional_part);

                                tiny_dyncharbuf_add(&t_str, '\0');

                                return commit_text_and_free(REAL_LITERAL, t_str.buf, loc);
                            }
                        }
                        else if (c == '_')
                        {
                            // 1_"HELLO"
                            int c2 = peek(1);
                            if (c2 == '\''
                                    || c2 == '"')
                            {
                                int c1 = peek(0);
                                get();
                                c2 = peek(0);
                                get();

                                tiny_dyncharbuf_t t_str;
                                tiny_dyncharbuf_new(&t_str, strlen(digits.buf) + 32 + 1);

                                tiny_dyncharbuf_add_str(&t_str, digits.buf);
                                DELETE(digits.buf);

                                tiny_dyncharbuf_add(&t_str, c1); // _
                                tiny_dyncharbuf_add(&t_str, c2); // " or '
                                tiny_dyncharbuf_add(&t_str, '\0');

                                int token_id;
                                char* text;
                                scan_character_literal(/*prefix */ t_str.buf, /* delim */ c2, /* allow_suffix_boz */ 0, loc,
                                        &token_id, &text);
                                DELETE(t_str.buf);

                                return commit_text_and_free(token_id, text, loc);
                            }
                            else
                            {
                                char *kind_str = scan_kind();

                                tiny_dyncharbuf_t t_str;
                                tiny_dyncharbuf_new(&t_str, strlen(digits.buf) + strlen(kind_str) + 1);

                                tiny_dyncharbuf_add_str(&t_str, digits.buf);
                                DELETE(digits.buf);

                                tiny_dyncharbuf_add_str(&t_str, kind_str);
                                DELETE(kind_str);
                                tiny_dyncharbuf_add(&t_str, '\0');

                                return commit_text_and_free(DECIMAL_LITERAL, t_str.buf, loc);
                            }
                        }
                        else if (tolower(c) == 'h')
                        {
                            get();
                            // This is a Holleritz constant
                            int length = atoi(digits.buf);
                            DELETE(digits.buf);

                            if (length == 0)
                            {
                                error_printf_at(
                                        make_locus(
                                            loc.filename,
                                            loc.line,
                                            loc.column),
                                        "ignoring invalid Hollerith constant of length 0\n");
                                continue;
                            }
                            else
                            {
                                tiny_dyncharbuf_t holl;
                                tiny_dyncharbuf_new(&holl, length + 1);
                                int ok = 1;
                                for (int i = 0; i < length; i++)
                                {
                                    c = peek(0);
                                    if (is_newline(c)
                                            || c == EOF)
                                    {
                                        error_printf_at(
                                                make_locus(
                                                    loc.filename,
                                                    loc.line,
                                                    loc.column),
                                                "unended Hollerith constant\n");
                                        ok = 0;
                                        break;
                                    }
                                    tiny_dyncharbuf_add(&holl, c);
                                    get();
                                }
                                tiny_dyncharbuf_add(&holl, '\0');

                                if (!ok)
                                    continue;

                                return commit_text_and_free(TOKEN_HOLLERITH_CONSTANT, holl.buf, loc);
                            }
                        }
                        else
                        {
                            if (lexer_state.last_eos
                                    && strlen(digits.buf) < 6 /* maximum length of a label */)
                            {
                                if (is_format_statement())
                                {
                                    lexer_state.in_format_statement = 1;
                                }
                            }
                            else if (lexer_state.in_nonblock_do_construct)
                            {
                                lexer_state.in_nonblock_do_construct = 0;

                                int label = atoi(digits.buf);
                                if (lexer_state.num_nonblock_labels > 0
                                        && lexer_state.nonblock_labels_stack[lexer_state.num_nonblock_labels - 1] == label)
                                {
                                    // DO 50 I = 1, 10
                                    //   DO 50 J = 1, 20  ! This second 50 is a TOKEN_SHARED_LABEL
                                    return commit_text_and_free(TOKEN_SHARED_LABEL, digits.buf, loc);
                                }
                                else
                                {
                                    if (lexer_state.num_nonblock_labels == lexer_state.size_nonblock_labels_stack)
                                    {
                                        lexer_state.size_nonblock_labels_stack = 2*lexer_state.size_nonblock_labels_stack + 1;
                                        lexer_state.nonblock_labels_stack = NEW_REALLOC(
                                                int,
                                                lexer_state.nonblock_labels_stack,
                                                lexer_state.size_nonblock_labels_stack);
                                    }
                                    lexer_state.nonblock_labels_stack[lexer_state.num_nonblock_labels] = label;
                                    lexer_state.num_nonblock_labels++;
                                }
                            }

                            return commit_text_and_free(DECIMAL_LITERAL, digits.buf, loc);
                        }
                    }
                case '@':
                    {
                        tiny_dyncharbuf_t str;
                        tiny_dyncharbuf_new(&str, 32);

                        tiny_dyncharbuf_add(&str, c0);
                        int c = peek(0);
                        while (c != '@'
                                && !is_newline(c)
                                && c != EOF)
                        {
                            tiny_dyncharbuf_add(&str, c);
                            get();
                            c = peek(0);
                        }
                        if (c == '@')
                        {
                            tiny_dyncharbuf_add(&str, c);
                            get();
                        }
                        tiny_dyncharbuf_add(&str, '\0');

                        int token_id = 0;
                        char preserve_eos = 0;
                        if (strncmp(str.buf, "@STATEMENT-PH", strlen("@STATEMENT-PH")) == 0)
                        {
                            // FIXME - make the check above a bit more robust
                            token_id = STATEMENT_PLACEHOLDER;
                        }
                        else
                        {
                            struct special_token_table_tag k;
                            k.keyword = str.buf;

                            struct special_token_table_tag *result =
                                (struct special_token_table_tag*)
                                bsearch(&k, special_tokens,
                                        sizeof(special_tokens) / sizeof(special_tokens[0]),
                                        sizeof(special_tokens[0]),
                                        special_token_table_comp);

                            if (result == NULL)
                            {
                                error_printf_at(
                                        make_locus(
                                            loc.filename,
                                            loc.line,
                                            loc.column),
                                        "invalid special token '%s', ignoring\n",
                                        str.buf);
                                continue;
                            }
                            else
                            {
                                token_id = result->token_id;
                                preserve_eos = result->preserve_eos;
                            }
                        }

                        char last_eos = lexer_state.last_eos;
                        int n = commit_text_and_free(token_id, str.buf, loc);
                        if (preserve_eos)
                            lexer_state.last_eos = last_eos;
                        return n;
                    }
                default: { /* do nothing */ }
            }
        } // LEXER_SUBSTATE_NORMAL
        else if (lexer_state.substate == LEXER_SUBSTATE_PRAGMA_DIRECTIVE
                || lexer_state.substate == LEXER_SUBSTATE_PRAGMA_FIRST_CLAUSE
                || lexer_state.substate == LEXER_SUBSTATE_PRAGMA_CLAUSE
                || lexer_state.substate == LEXER_SUBSTATE_PRAGMA_VAR_LIST)

        {
            switch (c0)
            {
                case EOF:
                    {
                        lexer_state.substate = LEXER_SUBSTATE_NORMAL;
                        DELETE(lexer_state.sentinel);
                        lexer_state.sentinel = NULL;
                        error_printf_at(
                                make_locus(
                                    loc.filename,
                                    loc.line,
                                    loc.column),
                                "unexpected end-of-file in directive\n");
                        continue;
                    }
                case '\n':
                case '\r':
                    {
                        lexer_state.substate = LEXER_SUBSTATE_NORMAL;
                        DELETE(lexer_state.sentinel);
                        lexer_state.sentinel = NULL;
                        if (!lexer_state.last_eos)
                        {
                            int n = commit_text(PRAGMA_CUSTOM_NEWLINE, NULL, loc);
                            lexer_state.bol = 1;
                            lexer_state.last_eos = 1;
                            return n;
                        }
                        lexer_state.bol = 1;
                        continue;
                    }
                case '!':
                    {
                        // A comment, skip until the end
                        while (!is_newline(c0))
                        {
                            get();
                            c0 = peek(0);
                        }

                        if (c0 == '\r')
                        {
                            if (peek(1) == '\n')
                                get();
                        }

                        continue;
                    }
                case ' ':
                case '\t':
                    {
                        // skip blanks
                        continue;
                    }
                default : { /* do nothing */ }
            }

            if (lexer_state.substate == LEXER_SUBSTATE_PRAGMA_DIRECTIVE)
            {
                // !$OMP xxx
                //       ^
                switch (c0)
                {
                    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H':
                    case 'I': case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P':
                    case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
                    case 'Y': case 'Z':
                    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h':
                    case 'i': case 'j': case 'k': case 'l': case 'm': case 'n': case 'o': case 'p':
                    case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x':
                    case 'y': case 'z':
                        {
                            // !$OMP xxx yyy zzz
                            //       ^
                            // identifier(\s\+{identifier})*
                            // and then we are told how much we have to back up
                            tiny_dyncharbuf_t str;
                            tiny_dyncharbuf_new(&str, 32);

                            tiny_dyncharbuf_add(&str, c0);
                            int peek_idx = 0;

                            char c = peek(peek_idx);
                            while (is_letter(c)
                                    || is_decimal_digit(c)
                                    || c == '_')
                            {
                                tiny_dyncharbuf_add(&str, c);
                                peek_idx++;
                                c = peek(peek_idx);
                            }

                            while (is_blank(c))
                            {
                                tiny_dyncharbuf_t t_str;
                                tiny_dyncharbuf_new(&t_str, 32);

                                while (is_blank(c))
                                {
                                    tiny_dyncharbuf_add(&t_str, c);
                                    peek_idx++;
                                    c = peek(peek_idx);
                                }

                                if (is_letter(c))
                                {
                                    while (is_letter(c)
                                            || is_decimal_digit(c)
                                            || c == '_')
                                    {
                                        tiny_dyncharbuf_add(&t_str, c);
                                        peek_idx++;
                                        c = peek(peek_idx);
                                    }
                                    tiny_dyncharbuf_add(&t_str, '\0');

                                    tiny_dyncharbuf_add_str(&str, t_str.buf);
                                    DELETE(t_str.buf);
                                }
                                else
                                {
                                    DELETE(t_str.buf);
                                    break;
                                }
                            }
                            tiny_dyncharbuf_add(&str, '\0');

                            char is_end_directive = (strlen(str.buf) > 3
                                    && strncasecmp(str.buf, "end", 3) == 0);

                            char* relevant_directive = NULL;
                            pragma_directive_kind_t directive_kind = PDK_NONE; 
                            const char* longest_match = return_pragma_prefix_longest_match(
                                    lexer_state.sentinel, is_end_directive, str.buf,
                                    // out
                                    &relevant_directive,
                                    &directive_kind);

                            int token_id = 0;
                            switch (directive_kind)
                            {
                                case PDK_DIRECTIVE:
                                    {
                                        if (!is_end_directive)
                                        {
                                            token_id = PRAGMA_CUSTOM_DIRECTIVE;
                                        }
                                        else
                                        {
                                            fatal_printf_at(
                                                    make_locus(
                                                        loc.filename,
                                                        loc.line,
                                                        loc.column),
                                                    "invalid directive '!$%s END %s'\n",
                                                    strtoupper(lexer_state.sentinel),
                                                    strtoupper(relevant_directive));
                                        }
                                        break;
                                    }
                                case PDK_CONSTRUCT_NOEND :
                                    {
                                        if (!is_end_directive)
                                        {
                                            token_id = PRAGMA_CUSTOM_CONSTRUCT_NOEND;
                                        }
                                        else
                                        {
                                            token_id = PRAGMA_CUSTOM_END_CONSTRUCT_NOEND;
                                        }
                                        break;
                                    }
                                case PDK_CONSTRUCT :
                                    {
                                        if (!is_end_directive)
                                        {
                                            if (lexer_state.num_pragma_constructs == lexer_state.size_pragma_constructs_stack)
                                            {
                                                lexer_state.size_pragma_constructs_stack = 2*lexer_state.size_pragma_constructs_stack + 1;
                                                lexer_state.pragma_constructs_stack = NEW_REALLOC(
                                                        char*,
                                                        lexer_state.pragma_constructs_stack,
                                                        lexer_state.size_pragma_constructs_stack);
                                            }
                                            lexer_state.pragma_constructs_stack[lexer_state.num_pragma_constructs] = xstrdup(longest_match);
                                            lexer_state.num_pragma_constructs++;
                                            token_id = PRAGMA_CUSTOM_CONSTRUCT;
                                        }
                                        else
                                        {
                                            if (lexer_state.num_pragma_constructs > 0)
                                            {
                                                char* top = lexer_state.pragma_constructs_stack[lexer_state.num_pragma_constructs-1];
                                                if (strcmp(top, longest_match) != 0)
                                                {
                                                    fatal_printf_at(
                                                            make_locus(
                                                                loc.filename,
                                                                loc.line,
                                                                loc.column),
                                                            "invalid nesting for '!$%s %s', expecting '!$%s END %s'\n", 
                                                            strtoupper(lexer_state.sentinel), 
                                                            strtoupper(relevant_directive),
                                                            strtoupper(lexer_state.sentinel), 
                                                            strtoupper(format_pragma_string(top)));
                                                }
                                                else
                                                {
                                                    DELETE(top);
                                                    lexer_state.num_pragma_constructs--;
                                                    token_id = PRAGMA_CUSTOM_END_CONSTRUCT;
                                                }
                                            }
                                            else
                                            {
                                                fatal_printf_at(
                                                        make_locus(
                                                            loc.filename,
                                                            loc.line,
                                                            loc.column),
                                                        "bad nesting for '!$%s %s'\n",
                                                        strtoupper(lexer_state.sentinel), 
                                                        strtoupper(relevant_directive));
                                            }
                                        }
                                        break;
                                    }
                                case PDK_NONE :
                                    {
                                        fatal_printf_at(
                                                make_locus(
                                                    loc.filename,
                                                    loc.line,
                                                    loc.column),
                                                "unknown directive '!$%s %s'",
                                                strtoupper(lexer_state.sentinel),
                                                strtoupper(str.buf));
                                        break;
                                    }
                                default: internal_error("Invalid pragma directive kind kind=%d", directive_kind);
                            }

                            lexer_state.substate = LEXER_SUBSTATE_PRAGMA_FIRST_CLAUSE;
                            int n = commit_text_and_free(token_id, relevant_directive, loc);
                            DELETE(str.buf);
                            return n;
                            break;
                        }
                    default : { /* do nothing */ }
                }
            }
            else if (lexer_state.substate == LEXER_SUBSTATE_PRAGMA_FIRST_CLAUSE)
            {
                // !$OMP PARALLEL xxx
                //                ^
                switch (c0)
                {
                    case '(':
                        {
                            lexer_state.substate = LEXER_SUBSTATE_PRAGMA_VAR_LIST;
                            return commit_text('(', "(", loc);
                            break;
                        }
                    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H':
                    case 'I': case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P':
                    case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
                    case 'Y': case 'Z':
                    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h':
                    case 'i': case 'j': case 'k': case 'l': case 'm': case 'n': case 'o': case 'p':
                    case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x':
                    case 'y': case 'z':
                        {
                            tiny_dyncharbuf_t str;
                            tiny_dyncharbuf_new(&str, 32);

                            tiny_dyncharbuf_add(&str, c0);

                            char c = peek(0);
                            while (is_letter(c)
                                    || c == '_'
                                    || is_decimal_digit(c))
                            {
                                tiny_dyncharbuf_add(&str, c);
                                get();
                                c = peek(0);
                            }
                            tiny_dyncharbuf_add(&str, '\0');

                            // advance blanks now because we want to move onto
                            // the next '(' if any
                            while (is_blank(c))
                            {
                                get();
                                c = peek(0);
                            }

                            if (c == '(')
                            {
                                lexer_state.substate = LEXER_SUBSTATE_PRAGMA_VAR_LIST;
                            }
                            else
                            {
                                lexer_state.substate = LEXER_SUBSTATE_PRAGMA_CLAUSE;
                            }

                            return commit_text_and_free(PRAGMA_CUSTOM_CLAUSE, str.buf, loc);
                            break;
                        }
                    default: { /* do nothing */ }
                }
            }
            else if (lexer_state.substate == LEXER_SUBSTATE_PRAGMA_CLAUSE)
            {
                // !$OMP PARALLEL FOO(X) xxx ...
                //                       ^
                switch (c0)
                {
                    case ',':
                        {
                            return commit_text(',', ",", loc);
                            continue;
                        }
                    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H':
                    case 'I': case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P':
                    case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
                    case 'Y': case 'Z':
                    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h':
                    case 'i': case 'j': case 'k': case 'l': case 'm': case 'n': case 'o': case 'p':
                    case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x':
                    case 'y': case 'z':
                        {
                            tiny_dyncharbuf_t str;
                            tiny_dyncharbuf_new(&str, 32);

                            tiny_dyncharbuf_add(&str, c0);

                            char c = peek(0);
                            while (is_letter(c)
                                    || c == '_'
                                    || is_decimal_digit(c))
                            {
                                tiny_dyncharbuf_add(&str, c);
                                get();
                                c = peek(0);
                            }
                            tiny_dyncharbuf_add(&str, '\0');

                            // advance blanks now because we want to move onto
                            // the next '(' if any
                            while (is_blank(c))
                            {
                                get();
                                c = peek(0);
                            }

                            if (c == '(')
                            {
                                lexer_state.substate = LEXER_SUBSTATE_PRAGMA_VAR_LIST;
                            }

                            return commit_text_and_free(PRAGMA_CUSTOM_CLAUSE, str.buf, loc);
                            break;
                        }
                    default: { /* do nothing */ }
                }
            }
            else if (lexer_state.substate == LEXER_SUBSTATE_PRAGMA_VAR_LIST)
            {
                // !$OMP PARALLEL FOO(xxx)
                //                    ^
                if (c0 == '(')
                {
                    return commit_text('(', "(", loc);
                }
                else if (c0 == ')')
                {
                    lexer_state.substate = LEXER_SUBSTATE_PRAGMA_CLAUSE;
                    return commit_text(')', ")", loc);
                }
                else
                {
                    tiny_dyncharbuf_t str;
                    tiny_dyncharbuf_new(&str, 32);

                    tiny_dyncharbuf_add(&str, c0);

                    int parentheses = 0;
                    int c = peek(0);
                    while ((c != ')'
                                || parentheses > 0)
                            && !is_newline(c)
                            && c != EOF)
                    {
                        tiny_dyncharbuf_add(&str, c);

                        if (c == '(')
                            parentheses++;
                        else if (c == ')')
                            parentheses--;

                        get();
                        c = peek(0);
                    }
                    tiny_dyncharbuf_add(&str, '\0');

                    if (c != ')')
                    {
                        error_printf_at(
                                make_locus(
                                    loc.filename,
                                    loc.line,
                                    loc.column),
                                "unended clause\n");
                    }

                    return commit_text_and_free(PRAGMA_CLAUSE_ARG_TEXT, str.buf, loc);
                }
            }
        }
        else
        {
            internal_error("invalid lexer substate", 0);
        }

        // Default case, unclassifiable token
        if (isprint(c0))
        {
            error_printf_at(
                    make_locus( 
                        loc.filename,
                        loc.line,
                        loc.column),
                    "unexpected character: `%c' (0x%X)\n",
                    c0, c0);
        }
        else
        {
            error_printf_at(
                    make_locus( 
                        loc.filename,
                        loc.line,
                        loc.column),
                    "unexpected character: 0x%X\n\n",
                    c0);
        }
    }
    internal_error("Code unreachable", 0);
}
