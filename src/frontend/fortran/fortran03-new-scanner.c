#ifdef HAVE_CONFIG_H
   #include "config.h"
#endif


#include "cxx-process.h"
#include "cxx-utils.h"
#include "cxx-diagnostic.h"
#include "fortran03-lexer.h"
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
#endif // NEW_FORTRAN_SCANNER

/*
   Include stack.

   The maxim level of nesting is not defined in Fortran 95 standard.
   We have set it to 99.
 */
#define MAX_INCLUDE_DEPTH 99

struct scan_file_descriptor 
{
    // This is the (physical) filename being scanned
    const char* filename;

    // This is the logical filename that we are scanning.
    // current_filename != filename only in Fortran fixed-form because we scan
    // the output of prescanner
    const char* current_filename;

    union {
        // file descriptor + flex buffer
        struct {
            FILE* file_descriptor;
            struct yy_buffer_state* scanning_buffer;
        };

        // memory buffer/mmap
        struct {
            const char *current_pos; // position in the buffer

            const char *buffer; // scanned buffer
            size_t buffer_size; // number of characters in buffer relevant for scanning

            int fd; // if fd >= 0 this is a mmap
        };
    };

    // Line of current token
    unsigned int line_number;
    // Column where the current token starts
    unsigned column_number;
    // Fortran: After a joined line we have to move to this line if new_line > 0 
    unsigned int new_line; 
    // Fortran: Number of joined lines so far
    unsigned int joined_lines;
};

enum lexer_textual_form
{
    LX_INVALID_FORM = 0,
    LX_FREE_FORM = 1,

    // Not yet implemented
    LX_FIXED_FORM = 2,
};

typedef
struct token_location_tag
{
    const char* filename;
    int line;
    int column;
} token_location_t;

typedef
struct include_info_tag
{
    struct scan_file_descriptor desc;
    token_location_t current_location;
} include_info_t;

static
struct new_lexer_state_t
{
    enum lexer_textual_form form;

    int include_stack_size;
    include_info_t include_stack[MAX_INCLUDE_DEPTH];

    // beginning of line
    char bol:1;
    // last token was end of line (the parser does not like redundant EOS)
    char last_eos:1; 

    token_location_t *current_location;

    // states that we are inside a string-literal (changes the way we handle
    // continuations)
    char character_context:1;
} lexer_state;

static token_location_t get_current_location(void)
{
    return *lexer_state.current_location;
}

static struct scan_file_descriptor* fortran_scanning_now;
int mf03_flex_debug = 1;

static inline void peek_init(void);

extern int new_mf03_open_file_for_scanning(const char* scanned_filename, const char* input_filename)
{
    int fd = open(scanned_filename, O_RDONLY);
    if (fd < 0)
    {
		running_error("error: cannot open file '%s' (%s)", scanned_filename, strerror(errno));
    }

    // Get size of file because we need it for the mmap
    struct stat s;
    int status = fstat (fd, & s);
    if (status < 0)
    {
        running_error("error: cannot get status of file '%s' (%s)", scanned_filename, strerror(errno));
    }

    const char *mmapped_addr = mmap(0, s.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
    if (mmapped_addr == MAP_FAILED)
    {
        running_error("error: cannot map file '%s' in memory (%s)", scanned_filename, strerror(errno));
    }

    lexer_state.include_stack_size = 0;
    fortran_scanning_now = &lexer_state.include_stack[lexer_state.include_stack_size].desc;
    lexer_state.current_location = &lexer_state.include_stack[lexer_state.include_stack_size].current_location;

	memset(fortran_scanning_now, 0, sizeof(*fortran_scanning_now));
	fortran_scanning_now->filename = uniquestr(scanned_filename);

	fortran_scanning_now->fd = fd;
	fortran_scanning_now->buffer_size = s.st_size;
    fortran_scanning_now->current_pos
        = fortran_scanning_now->buffer = mmapped_addr;
    fortran_scanning_now->current_filename = uniquestr(input_filename);

	lexer_state.current_location->filename = uniquestr(input_filename);
	lexer_state.current_location->line = 1;
	lexer_state.current_location->column = 0;

    lexer_state.bol = 1;
    lexer_state.last_eos = 1;

    peek_init();

    return 0;
}

// KEEP THIS TABLE SORTED
static
struct keyword_table_tag
{
    const char* keyword;
    int token_id;
} keyword_table[] =
{
    {"abstract", TOKEN_ABSTRACT},
    {"access", TOKEN_ACCESS},
    {"acquired", TOKEN_ACQUIRED},
    {"action", TOKEN_ACTION},
    {"advance", TOKEN_ADVANCE},
    {"all", TOKEN_ALL},
    {"allocatable", TOKEN_ALLOCATABLE},
    {"allocate", TOKEN_ALLOCATE},
    {"allstop", TOKEN_ALLSTOP},
    {"assign", TOKEN_ASSIGN},
    {"assignment", TOKEN_ASSIGNMENT},
    {"associate", TOKEN_ASSOCIATE},
    {"asynchronous", TOKEN_ASYNCHRONOUS},
    {"backspace", TOKEN_BACKSPACE},
    {"bind", TOKEN_BIND},
    {"blank", TOKEN_BLANK},
    {"block", TOKEN_BLOCK},
    {"blockdata", TOKEN_BLOCKDATA}, 
    {"c", TOKEN_C},
    {"call", TOKEN_CALL},
    {"case", TOKEN_CASE},
    {"character", TOKEN_CHARACTER},
    {"class", TOKEN_CLASS},
    {"close", TOKEN_CLOSE},
    {"codimension", TOKEN_CODIMENSION},
    {"common", TOKEN_COMMON},
    {"complex", TOKEN_COMPLEX},
    {"concurrent", TOKEN_CONCURRENT},
    {"contains", TOKEN_CONTAINS},
    {"contiguous", TOKEN_CONTIGUOUS},
    {"continue", TOKEN_CONTINUE},
    {"convert", TOKEN_CONVERT},
    {"critical", TOKEN_CRITICAL},
    {"cycle", TOKEN_CYCLE},
    {"data", TOKEN_DATA},
    {"deallocate", TOKEN_DEALLOCATE},
    {"decimal", TOKEN_DECIMAL},
    {"default", TOKEN_DEFAULT},
    {"deferred", TOKEN_DEFERRED},
    {"delim", TOKEN_DELIM},
    {"dimension", TOKEN_DIMENSION},
    {"direct", TOKEN_DIRECT},
    {"do", TOKEN_DO},
    {"double", TOKEN_DOUBLE},
    {"doublecomplex", TOKEN_DOUBLECOMPLEX},
    {"doubleprecision", TOKEN_DOUBLEPRECISION}, 
    {"elemental", TOKEN_ELEMENTAL},
    {"else", TOKEN_ELSE},
    {"elseif", TOKEN_ELSEIF}, 
    {"elsewhere", TOKEN_ELSEWHERE},
    {"encoding", TOKEN_ENCODING},
    {"end", TOKEN_END},
    {"endassociate", TOKEN_ENDASSOCIATE},
    {"endblock", TOKEN_ENDBLOCK},
    {"endblockdata", TOKEN_ENDBLOCKDATA},
    {"endcritical", TOKEN_ENDCRITICAL},
    {"enddo", TOKEN_ENDDO},
    {"endfile", TOKEN_ENDFILE},
    {"endfunction", TOKEN_ENDFUNCTION},
    {"endif", TOKEN_ENDIF},
    {"endinterface", TOKEN_ENDINTERFACE},
    {"endmodule", TOKEN_ENDMODULE},
    {"endprocedure", TOKEN_ENDPROCEDURE},
    {"endprogram", TOKEN_ENDPROGRAM},
    {"endselect", TOKEN_ENDSELECT},
    {"endsubmodule", TOKEN_ENDSUBMODULE},
    {"endsubroutine", TOKEN_ENDSUBROUTINE},
    {"endtype", TOKEN_ENDTYPE},
    {"entry", TOKEN_ENTRY},
    {"enum", TOKEN_ENUM},
    {"enumerator", TOKEN_ENUMERATOR},
    {"eor", TOKEN_EOR},
    {"equivalence", TOKEN_EQUIVALENCE},
    {"err", TOKEN_ERR},
    {"errmsg", TOKEN_ERRMSG},
    {"exist", TOKEN_EXIST},
    {"exit", TOKEN_EXIT},
    {"extends", TOKEN_EXTENDS},
    {"external", TOKEN_EXTERNAL},
    {"file", TOKEN_FILE},
    {"final", TOKEN_FINAL},
    {"flush", TOKEN_FLUSH},
    {"fmt", TOKEN_FMT},
    {"forall", TOKEN_FORALL},
    {"form", TOKEN_FORM},
    {"format", TOKEN_FORMAT},
    {"formatted", TOKEN_FORMATTED},
    {"function", TOKEN_FUNCTION},
    {"generic", TOKEN_GENERIC},
    {"go", TOKEN_GO},
    {"goto", TOKEN_GOTO},
    {"id", TOKEN_ID},
    {"if", TOKEN_IF},
    {"images", TOKEN_IMAGES},
    {"implicit", TOKEN_IMPLICIT},
    {"import", TOKEN_IMPORT},
    {"impure", TOKEN_IMPURE},
    {"in", TOKEN_IN},
    {"inout", TOKEN_INOUT},
    {"inquire", TOKEN_INQUIRE},
    {"integer", TOKEN_INTEGER},
    {"intent", TOKEN_INTENT},
    {"interface", TOKEN_INTERFACE},
    {"intrinsic", TOKEN_INTRINSIC},
    {"iolength", TOKEN_IOLENGTH},
    {"iomsg", TOKEN_IOMSG},
    {"iostat", TOKEN_IOSTAT},
    {"is", TOKEN_IS},
    {"kind", TOKEN_KIND},
    {"len", TOKEN_LEN},
    {"lock", TOKEN_LOCK},
    {"logical", TOKEN_LOGICAL},
    {"memory", TOKEN_MEMORY},
    {"module", TOKEN_MODULE},
    {"mold", TOKEN_MOLD},
    {"name", TOKEN_NAME},
    {"named", TOKEN_NAMED},
    {"namelist", TOKEN_NAMELIST},
    {"newunit", TOKEN_NEWUNIT},
    {"nextrec", TOKEN_NEXTREC},
    {"nml", TOKEN_NML},
    {"none", TOKEN_NONE},
    {"nopass", TOKEN_NOPASS},
    {"nullify", TOKEN_NULLIFY},
    {"number", TOKEN_NUMBER},
    {"only", TOKEN_ONLY},
    {"open", TOKEN_OPEN},
    {"opencl", TOKEN_OPENCL},
    {"opened", TOKEN_OPENED},
    {"operator", TOKEN_OPERATOR},
    {"optional", TOKEN_OPTIONAL},
    {"out", TOKEN_OUT},
    {"overridable", TOKEN_OVERRIDABLE},
    {"pad", TOKEN_PAD},
    {"parameter", TOKEN_PARAMETER},
    {"pass", TOKEN_PASS},
    {"pause", TOKEN_PAUSE},
    {"pending", TOKEN_PENDING},
    {"pixel", TOKEN_PIXEL},
    {"pointer", TOKEN_POINTER},
    {"pos", TOKEN_POS},
    {"position", TOKEN_POSITION},
    {"precision", TOKEN_PRECISION},
    {"print", TOKEN_PRINT},
    {"private", TOKEN_PRIVATE},
    {"procedure", TOKEN_PROCEDURE},
    {"program", TOKEN_PROGRAM},
    {"protected", TOKEN_PROTECTED},
    {"public", TOKEN_PUBLIC},
    {"pure", TOKEN_PURE},
    {"read", TOKEN_READ},
    {"readwrite", TOKEN_READWRITE},
    {"real", TOKEN_REAL},
    {"rec", TOKEN_REC},
    {"recl", TOKEN_RECL},
    {"recursive", TOKEN_RECURSIVE},
    {"result", TOKEN_RESULT},
    {"return", TOKEN_RETURN},
    {"rewind", TOKEN_REWIND},
    {"round", TOKEN_ROUND},
    {"save", TOKEN_SAVE},
    {"select", TOKEN_SELECT},
    {"selectcase", TOKEN_SELECTCASE},
    {"sequence", TOKEN_SEQUENCE},
    {"sequential", TOKEN_SEQUENTIAL},
    {"sign", TOKEN_SIGN},
    {"size", TOKEN_SIZE},
    {"source", TOKEN_SOURCE},
    {"stat", TOKEN_STAT},
    {"status", TOKEN_STATUS},
    {"stop", TOKEN_STOP},
    {"stream", TOKEN_STREAM},
    {"submodule", TOKEN_SUBMODULE},
    {"subroutine", TOKEN_SUBROUTINE},
    {"sync", TOKEN_SYNC},
    {"target", TOKEN_TARGET},
    {"then", TOKEN_THEN},
    {"to", TOKEN_TO},
    {"type", TOKEN_TYPE},
    {"unformatted", TOKEN_UNFORMATTED},
    {"unit", TOKEN_UNIT},
    {"unlock", TOKEN_UNLOCK},
    {"use", TOKEN_USE},
    {"value", TOKEN_VALUE},
    {"vector", TOKEN_VECTOR},
    {"volatile", TOKEN_VOLATILE},
    {"wait", TOKEN_WAIT},
    {"where", TOKEN_WHERE},
    {"while", TOKEN_WHILE},
    {"write", TOKEN_WRITE}
};

static int keyword_table_comp(
        const void* p1,
        const void* p2)
{
    const struct keyword_table_tag* v1 = (const struct keyword_table_tag*)p1;
    const struct keyword_table_tag* v2 = (const struct keyword_table_tag*)p2;

    return strcasecmp(v1->keyword, v2->keyword);
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
	fortran_scanning_now = &(lexer_state.include_stack[lexer_state.include_stack_size].desc);
    lexer_state.current_location = &(lexer_state.include_stack[lexer_state.include_stack_size].current_location);
  
	fortran_scanning_now->line_number = 1;
	fortran_scanning_now->column_number = 0;
	fortran_scanning_now->new_line = 0;
	fortran_scanning_now->joined_lines = 0;
	
	const char* current_filename = CURRENT_COMPILED_FILE->input_filename;
	fortran_scanning_now->filename = xcalloc(strlen(TL_SOURCE_STRING) + strlen(current_filename) + 10, sizeof(char));
    char filename[256];
	snprintf(filename, 255, "%s-%s-%d", TL_SOURCE_STRING, current_filename, num_string);
    filename[255] = '\0';
    fortran_scanning_now->filename = uniquestr(filename);

    fortran_scanning_now->fd = -1; // not an mmap
    fortran_scanning_now->buffer_size = strlen(str);
    fortran_scanning_now->current_pos
        = fortran_scanning_now->buffer = str;

    lexer_state.current_location->filename = fortran_scanning_now->filename;
    lexer_state.current_location->line = 1;
    lexer_state.current_location->column = 0;
	
	num_string++;
    lexer_state.last_eos = 1;

    peek_init();

	return 0;
}

static char process_end_of_file(void)
{
	// Are we in the last file?
	if (lexer_state.include_stack_size == 0)
	{
		return 1;
	}
	else
	{
		DEBUG_CODE() DEBUG_MESSAGE("End of included file %s switching back to %s", 
				lexer_state.current_location->filename, lexer_state.include_stack[lexer_state.include_stack_size-1].current_location.filename);

        if (fortran_scanning_now->fd >= 0)
        {
            int res = munmap((void*)fortran_scanning_now->buffer, fortran_scanning_now->buffer_size);
            if (res < 0)
            {
                running_error("error: unmaping of file '%s' failed (%s)\n", fortran_scanning_now->filename, strerror(errno));
            }
            res = close(fortran_scanning_now->fd);
            if (res < 0)
            {
                running_error("error: closing file '%s' failed (%s)\n", fortran_scanning_now->filename, strerror(errno));
            }
        }

		lexer_state.include_stack_size--;
		fortran_scanning_now = &(lexer_state.include_stack[lexer_state.include_stack_size].desc);
		lexer_state.current_location = &(lexer_state.include_stack[lexer_state.include_stack_size].current_location);
        lexer_state.last_eos = 1;

		return 0;
	}
}


static inline char is_blank(char c)
{
    return (c == ' ' || c == '\t');
}

static inline char is_newline(char c)
{
    return c == '\n'
        || c == '\r';
}

static inline char past_eof(void)
{
    return (fortran_scanning_now->current_pos >= ((fortran_scanning_now->buffer + fortran_scanning_now->buffer_size)));
}

static inline char is_end_of_file(void)
{
    return past_eof();
}

static inline char free_form_get(void)
{
    if (past_eof())
        return EOF;

    char result = fortran_scanning_now->current_pos[0];

    while (result == '&')
    {
        const char* keep = fortran_scanning_now->current_pos;
        // int keep_line = lexer_state.current_location->line;
        int keep_column = lexer_state.current_location->column;

        // blanks
        while (!past_eof()
                && is_blank(fortran_scanning_now->current_pos[0]))
        {
            lexer_state.current_location->column++;

            fortran_scanning_now->current_pos++;
        }

        if (past_eof())
            return EOF;

        if (!lexer_state.character_context)
        {
            // When we are not in character context we allow a comment here
            if (fortran_scanning_now->current_pos[0] == '!')
            {
                while (!past_eof()
                        && !is_newline(fortran_scanning_now->current_pos[0]))
                {
                    lexer_state.current_location->column++;

                    fortran_scanning_now->current_pos++;
                }
                if (past_eof())
                    return EOF;
            }
            else if (is_newline(fortran_scanning_now->current_pos[0]))
            {
                // handled below
            }
            else
            {
                // not a continuation
                lexer_state.current_location->column = keep_column;

                fortran_scanning_now->current_pos = keep;
                break;
            }
        }

        if (fortran_scanning_now->current_pos[0] == '\n')
        {
            lexer_state.current_location->line++;
            lexer_state.current_location->column = 0;

            fortran_scanning_now->current_pos++;
            if (past_eof())
                return EOF;
        }
        else if (fortran_scanning_now->current_pos[0] == '\r')
        {
            lexer_state.current_location->line++;
            lexer_state.current_location->column = 0;

            fortran_scanning_now->current_pos++;
            if (past_eof())
                return EOF;
            if (fortran_scanning_now->current_pos[0] == '\n')
            {
                fortran_scanning_now->current_pos++;
                if (past_eof())
                    return EOF;
            }
        }
        else
        {
            // not a continuation
            lexer_state.current_location->column = keep_column;

            fortran_scanning_now->current_pos = keep;
            break;
        }

        // Now we need to peek if there is another &, so we have to do
        // token pasting, otherwise we will return a blank
        while (!past_eof()
                && is_blank(fortran_scanning_now->current_pos[0]))
        {
            lexer_state.current_location->column++;

            fortran_scanning_now->current_pos++;
        }
        if (past_eof())
            return EOF;

        if (fortran_scanning_now->current_pos[0] == '&')
        {
            lexer_state.current_location->column++;

            fortran_scanning_now->current_pos++;
            if (past_eof())
                return EOF;
            result = fortran_scanning_now->current_pos[0];
        }
        else
        {
            if (lexer_state.character_context)
            {
                // in character context the first nonblank is the next character
                result = fortran_scanning_now->current_pos[0];
            }
            else
            {
                result = ' ';
                // Compensate this artificial character
                lexer_state.current_location->column--;
            }
        }
    }

    if (result != '\n' 
            && result != '\r')
    {
        lexer_state.current_location->column++;
        fortran_scanning_now->current_pos++;
    }
    else
    {
        lexer_state.current_location->line++;
        lexer_state.current_location->column = 0;

        fortran_scanning_now->current_pos++;
        // DOS: Try to eat the '\n'
        if (!past_eof()
                && fortran_scanning_now->current_pos[0] == '\n')
        {
            fortran_scanning_now->current_pos++;
        }
    }

    return result;
}

typedef
struct peek_token_info_tag
{
    char letter;
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
        _peek_queue.buffer = xmalloc(PEEK_INITIAL_SIZE * sizeof(*_peek_queue.buffer));
        _peek_queue.size = PEEK_INITIAL_SIZE;
        _peek_queue.back = 0;
        _peek_queue.front = 0;
    }
}

static inline char peek_empty(void)
{
    return (_peek_queue.front == _peek_queue.back);
}

static inline int peek_size(void)
{
    return (_peek_queue.front - _peek_queue.back);
}

static inline void peek_add(char c, token_location_t loc)
{
    if ((_peek_queue.size - 1) + _peek_queue.back < 0)
    {
        int new_size = _peek_queue.size * 2;
        peek_token_info_t *new_buffer = xmalloc(new_size * sizeof(*new_buffer));

        memcpy(&new_buffer[(new_size - 1) + _peek_queue.back],
                _peek_queue.buffer,
                _peek_queue.size);

        xfree(_peek_queue.buffer);
        _peek_queue.buffer = new_buffer;
        _peek_queue.size = new_size;
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

static inline char get_loc(token_location_t *loc)
{
    token_location_t tmp_loc;

    char c;
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
        c = free_form_get();
        tmp_loc = get_current_location();
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

static inline char get(void)
{
    return get_loc(NULL);
}

static inline char peek_loc(int n, token_location_t *loc)
{
    int s = peek_size();
    if (n >= s)
    {
        int d = n - s + 1;

        int i;
        for (i = 0; i < d; i++)
        {
            char c = free_form_get();
            token_location_t loc2 = get_current_location();
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

static inline char peek(int n)
{
    return peek_loc(n, NULL);
}


static inline char is_letter(char c)
{
    return ('a' <= c && c <= 'z')
        || ('A' <= c && c <= 'Z');
}

static inline char is_decimal_digit(char c)
{
    return ('0' <= c && c <= '9');
}

enum {
    MAX_IDENTIFIER_LENGTH = 128,
    MAX_NUMERIC_LITERAL = 128,
    MAX_KEYWORD_LENGTH = 32,
    MAX_USER_DEFINED_OPERATOR = 32,
    MAX_KIND_LENGTH = 32,
};

static void scan_kind(char* out_str)
{
    char c = get();
    ERROR_CONDITION(c != '_', "input stream is incorrectly located (c=%c)", c);

    int i = 0;
    c = peek(0);
    if (is_decimal_digit(c))
    {
        while (i < MAX_KIND_LENGTH
                && is_decimal_digit(c))
        {
            get();
            out_str[i] = c;
            c = peek(0);
            i++;
        }
    }
    else if (is_letter(c))
    {
        while (i < MAX_KIND_LENGTH
                && is_letter(c))
        {
            get();
            out_str[i] = c;
            c = peek(0);
            i++;
        }
    }

    out_str[i] = '\0';
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

    mf03lval.token_atrib.token_text = uniquestr(str);
    mf03lloc.first_filename = loc.filename;
    mf03lloc.first_line = loc.line;
    mf03lloc.first_column = loc.column;

    // The parser still uses this. We will eventually remove this info
    fortran_scanning_now->current_filename = loc.filename;
    fortran_scanning_now->line_number = loc.line;
    fortran_scanning_now->column_number = loc.column;

    return token_id;
}

static int commit_text_and_free(int token_id, char* str,
        token_location_t loc)
{
    token_id = commit_text(token_id, str, loc);
    xfree(str);
    return token_id;
}

static inline int scan_character_literal(
        const char* prefix,
        char delim,
        char allow_suffix_boz,
        token_location_t loc)
{
    ERROR_CONDITION(prefix == NULL, "Invalid prefix", 0);
#define ADD_CHAR(x) \
    { \
        if (i >= size) \
        { \
            size *= 2; \
            str = xrealloc(str, sizeof(*str) * (size + 1)); \
        } \
        str[i] = (x); \
        i++; \
    }
    int token_id = 0;

    int size = 32;
    int prefix_length = strlen(prefix);
    int i = prefix_length;
    char *str = xmalloc(sizeof(*str) * (prefix_length + size + 1));
    strncpy(str, prefix, prefix_length);

    char can_be_binary = allow_suffix_boz;
    char can_be_octal = allow_suffix_boz;
    char can_be_hexa = allow_suffix_boz;

    lexer_state.character_context = 1;

    char unended_literal = 0;
    char c = peek(0);
    token_location_t loc2;

    for (;;)
    {
        if (c != delim
                && c != '\n'
                && c != '\r')
        {
            get_loc(&loc2);
            ADD_CHAR(c);

            can_be_binary = 
                can_be_binary &&
                (c == '0' || c == '1');

            can_be_octal = 
                can_be_octal &&
                ('0' <= c && c <= '7');

            can_be_hexa = 
                can_be_hexa &&
                (('0' <= c && c <= '9')
                 || ('a' <= c && c <= 'f')
                 || ('A' <= c && c <= 'F'));

            c = peek_loc(0, &loc2);
        }
        else if (c == delim)
        {
            get_loc(&loc2);
            ADD_CHAR(c);
            c = peek_loc(0, &loc2);

            if (c != delim)
                break;

            can_be_binary
                = can_be_hexa
                = can_be_octal
                = 0;

            get_loc(&loc2);
            ADD_CHAR(c);
        }
        else // c == '\n' || c == '\r'
        {
            error_printf("%s:%d:%d: error: unended character literal\n",
                    loc2.filename,
                    loc2.line,
                    loc2.column);
            unended_literal = 1;
            get_loc(&loc2);
            break;
        }
    }

    lexer_state.character_context = 0;

    if (unended_literal)
        return 0;

    token_id = CHAR_LITERAL;
    if (can_be_binary
            || can_be_octal
            || can_be_hexa)
    {
        c = peek(0);
        if (can_be_binary
                && (c == 'b' || c == 'B'))
        {
            get();
            ADD_CHAR(c);
            token_id = BINARY_LITERAL;
        }
        else if (can_be_octal
                && (c == 'o' || c == 'O'))
        {
            get();
            ADD_CHAR(c);
            token_id = OCTAL_LITERAL;
        }
        else if (can_be_hexa
                && (c == 'x' || c == 'X'
                    || c == 'z' || c == 'Z'))
        {
            get();
            ADD_CHAR(c);
            token_id = HEX_LITERAL;
        }
    }

    ADD_CHAR('\0');

    return commit_text_and_free(token_id, str, loc);
#undef ADD_CHAR
}

// This is the lexer <-> parser interface from yacc/bison
// this function just returns the next token from the current
// input stream
extern int new_mf03lex (void)
{
    for (;;)
    {
        // Did we reach the end of the file?
        if (is_end_of_file())
        {
            char end_of_scan = process_end_of_file();
            if (end_of_scan)
            {
                if (!lexer_state.last_eos)
                {
                    // Make sure we force a final EOS
                    return commit_text(EOS, NULL, get_current_location());
                }
                return 0;
            }
            else
                continue;
        }

        token_location_t loc;
        char c0 = get_loc(&loc);
        switch (c0)
        {
            case ' ':
            case '\t':
                {
                    // Whitespace ignored
                    continue;
                }
            case '\n':
            case '\r':
                {
                    // Regarding \r\n in DOS, the get function will always skip \n if it finds it right after \n
                    if (!lexer_state.last_eos)
                    {
                        return commit_text(EOS, NULL, loc);
                    }
                    continue;
                }
            case '!':
                {
                    // comment
                    c0 = get();
                    while (!is_newline(c0))
                    {
                        c0 = get();
                    }

                    if (c0 == '\r')
                    {
                        if (peek(0) == '\n')
                        {
                            get();
                        }
                    }

                    continue;
                }
            case ';':
                {
                    if (!lexer_state.last_eos)
                    {
                        return commit_text(EOS, NULL, loc);
                    }
                    break;
                }
            case '(' :
                {
                    char c1 = peek(0);
                    if (c1 != '/')
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
                    char c1 = peek(0);
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
            case '[' :
            case ']' :
            case ',' :
            case '%' :
            case '+' :
            case '-' :
            case ':' :
                {
                    const char s[] = {c0, '\0'};
                    return commit_text(c0, s, loc);
                }
            case '*' :
                {
                    char c1 = peek(0);
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
                    char c1 = peek(0);
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
                    char c1 = peek(0);
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
                    char c1 = peek(0);
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
                    char c1 = peek(0);
                    char c2 = peek(1);
                    if (c1 == 'e'
                            && c2 == 'q')
                    {
                        char c3 = peek(2);
                        char c4 = peek(3);
                        if (c3 == '.')
                        {
                            get(); // e
                            get(); // q
                            get(); // .
                            return commit_text(TOKEN_EQUAL, ".eq.", loc);
                        }
                        else if (c3 == 'v'
                                && c4 == '.')
                        {
                            get(); // e
                            get(); // q
                            get(); // v
                            get(); // .
                            return commit_text(TOKEN_LOGICAL_EQUIVALENT, ".eqv.", loc);
                        }
                    }
                    else if (c1 == 'n')
                    {
                        if (c2 == 'e')
                        {
                            char c3 = peek(2);
                            char c4 = peek(3);
                            char c5 = peek(4);
                            if (c3 == '.')
                            {
                                get(); // n
                                get(); // e
                                get(); // .
                                return commit_text(TOKEN_NOT_EQUAL, ".ne.", loc);
                            }
                            else if (c3 == 'q'
                                    && c4 == 'v'
                                    && c5 == '.')
                            {
                                get(); // n
                                get(); // e
                                get(); // q
                                get(); // v
                                get(); // .
                                return commit_text(TOKEN_LOGICAL_NOT_EQUIVALENT, ".neqv.", loc);
                            }
                        }
                        else if (c2 == 'o')
                        {
                            char c3 = peek(2);
                            char c4 = peek(3);
                            if (c3 == 't'
                                    && c4 == '.')
                            {
                                get(); // n
                                get(); // o
                                get(); // t
                                get(); // .
                                return commit_text(TOKEN_LOGICAL_NOT, ".not.", loc);
                            }
                        }
                    }
                    else if (c1 == 'l')
                    {
                        char c3 = peek(2);
                        if (c2 == 'e'
                                && c3 == '.')
                        {
                            get(); // l
                            get(); // e
                            get(); // .
                            return commit_text(TOKEN_LOWER_OR_EQUAL_THAN, ".le.", loc);
                        }
                        else if (c2 == 't'
                                && c3 == '.')
                        {
                            get(); // l
                            get(); // t
                            get(); // .
                            return commit_text(TOKEN_LOWER_THAN, ".lt.", loc);
                        }
                    }
                    else if (c1 == 'g')
                    {
                        char c3 = peek(2);
                        if (c2 == 'e'
                                && c3 == '.')
                        {
                            get(); // g
                            get(); // e
                            get(); // .
                            return commit_text(TOKEN_GREATER_OR_EQUAL_THAN, ".ge.", loc);
                        }
                        else if (c2 == 't'
                                && c3 == '.')
                        {
                            get(); // g
                            get(); // t
                            get(); // .
                            return commit_text(TOKEN_GREATER_THAN, ".gt.", loc);
                        }
                    }
                    else if (c1 == 'o'
                            && c2 == 'r')
                    {
                        char c3 = peek(2);
                        if (c3 == '.')
                        {
                            get(); // o
                            get(); // r
                            get(); // .
                            return commit_text(TOKEN_LOGICAL_OR, ".or.", loc);
                        }
                    }
                    else if (c1 == 'a'
                            && c2 == 'n')
                    {
                        char c3 = peek(2);
                        char c4 = peek(3);
                        if (c3 == 'n'
                                && c4 == '.')
                        {
                            get(); // a
                            get(); // n
                            get(); // d
                            get(); // .
                            return commit_text(TOKEN_LOGICAL_AND, ".and.", loc);
                        }
                    }
                    else if (c1 == 't'
                            && c2 == 'r'
                            && peek(2) == 'u'
                            && peek(3) == 'e'
                            && peek(4) == '.')
                    {
                        char str[6 + MAX_KIND_LENGTH + 1];
                        str[0] = '.';
                        str[1] = get(); // t
                        str[2] = get(); // r
                        str[3] = get(); // u
                        str[4] = get(); // e
                        str[5] = get(); // .
                        char c = peek(0);
                        if (c == '_')
                        {
                            scan_kind(&str[6]);
                        }
                        else
                        {
                            str[6] = '\0';
                        }
                        return commit_text(TOKEN_TRUE, str, loc);
                    }
                    else if (c1 == 'f'
                            && c2 == 'a'
                            && peek(2) == 'l'
                            && peek(3) == 's'
                            && peek(4) == 'e'
                            && peek(5) == '.')
                    {
                        char str[7 + MAX_KIND_LENGTH + 1];
                        str[0] = '.';
                        str[1] = get(); // f
                        str[2] = get(); // a
                        str[3] = get(); // l
                        str[4] = get(); // s
                        str[5] = get(); // e
                        str[6] = get(); // .
                        char c = peek(0);
                        if (c == '_')
                        {
                            scan_kind(&str[7]);
                        }
                        else
                        {
                            str[7] = '\0';
                        }
                        return commit_text(TOKEN_FALSE, str, loc);
                    }
                    else if (is_decimal_digit(c1))
                    {
                        char str[MAX_NUMERIC_LITERAL + MAX_KIND_LENGTH + 1];
                        // a valid real literal at this point must be [.][0-9]+([edq][+-]?[0-9]+)_kind
                        str[0] = c0;
                        str[1] = c1; get();
                        int i = 2;

                        char c = c2;
                        while (is_decimal_digit(c))
                        {
                            get();
                            if (i >= MAX_NUMERIC_LITERAL)
                                break;
                            str[i] = c;
                            i++;

                            c = peek(0);
                        }

                        if (c == 'e' || c == 'E'
                                || c == 'd' || c == 'D'
                                || c == 'q' || c == 'Q')
                        {
                            get();
                            if (i >= MAX_NUMERIC_LITERAL)
                            {
                                c0 = c;
                                break;
                            }
                            str[i] = c;
                            i++;

                            c = peek(0);
                            if (c == '+' || c == '-')
                            {
                                get();
                                if (i >= MAX_NUMERIC_LITERAL)
                                {
                                    c0 = c;
                                    break;
                                }
                                str[i] = c;
                                i++;
                            }

                            c = peek(0);
                            while (is_decimal_digit(c))
                            {
                                get();
                                if (i >= MAX_NUMERIC_LITERAL)
                                    break;
                                str[i] = c;
                                i++;

                                c = peek(0);
                            }
                        }
                        if (c == '_')
                        {
                            scan_kind(&str[i]);
                        }
                        return commit_text(REAL_LITERAL, str, loc);
                    }
                    else if (is_letter(c1))
                    {
                        char user_def_op[MAX_USER_DEFINED_OPERATOR + 1];
                        user_def_op[0] = c0;
                        user_def_op[1] = c1; get();
                        int i = 2;

                        char c = c2;
                        while (is_letter(c))
                        {
                            get();
                            ERROR_CONDITION(i >= MAX_USER_DEFINED_OPERATOR, "User defined operator too long", 0);
                            user_def_op[i] = c;
                            i++;

                            c = peek(0);
                        }

                        if (c == '.')
                        {
                            user_def_op[i] = '.';
                            i++;
                            user_def_op[i] = '\0';
                            return commit_text(USER_DEFINED_OPERATOR, user_def_op, loc); // '.[a-z].'
                        }
                        else
                        {
                            // Unexpected character at this point
                            c0 = c;
                        }
                    }
                }
                // string literals
            case '"':
            case '\'':
                {
                    char prefix[2] = {c0, '\0'};
                    int t = scan_character_literal(prefix, /* delim */ c0, /* allow_suffix_boz */ 1, loc);
                    if (t != 0)
                        return t;
                    else
                        continue;
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
                    if (c0 == 'b' || c0 == 'B'
                            || c0 == 'o' || c0 == 'O'
                            || c0 == 'z' || c0 == 'Z'
                            || c0 == 'x' || c0 == 'X')
                    {
                        char c1 = peek(0);

                        if (c1 == '\'' || c1 == '"')
                        {
                            char str[MAX_NUMERIC_LITERAL + MAX_KIND_LENGTH + 1];
                            str[0] = c0;
                            str[1] = c1; get();
                            int i = 2;

                            char invalid_digit = 0;
                            int token_id = 0;
                            token_location_t loc2;
                            char c;
                            switch (c0)
                            {
                                case 'b': case 'B':
                                    {
                                        token_id = BINARY_LITERAL;
                                        c = peek_loc(0, &loc2);
                                        while (c != c1
                                                && c != '\n'
                                                && c != '\r')
                                        {
                                            if (c == '0'
                                                    || c == '1')
                                            {
                                                if (i >= MAX_NUMERIC_LITERAL)
                                                    break;
                                                str[i] = c;
                                                i++;
                                            }
                                            else
                                            {
                                                error_printf("%s:%d:%d: error: invalid binary digit\n",
                                                        loc2.filename,
                                                        loc2.line,
                                                        loc2.column);
                                                invalid_digit = 1;
                                            }
                                            get();

                                            c = peek_loc(0, &loc2);
                                        }
                                        break;
                                    }
                                case 'o' : case 'O':
                                    {
                                        token_id = OCTAL_LITERAL;
                                        c = peek_loc(0, &loc2);
                                        while (c != c1
                                                && c != '\n'
                                                && c != '\r')
                                        {
                                            if ('0' <= c
                                                    && c == '7')
                                            {
                                                if (i >= MAX_NUMERIC_LITERAL)
                                                    break;
                                                str[i] = c;
                                                i++;
                                            }
                                            else
                                            {
                                                error_printf("%s:%d:%d: error: invalid octal digit\n",
                                                        loc2.filename,
                                                        loc2.line,
                                                        loc2.column);
                                                invalid_digit = 1;
                                            }
                                            get();

                                            c = peek_loc(0, &loc2);
                                        }
                                        break;
                                    }
                                case 'x': case 'X':
                                case 'z': case 'Z':
                                    {
                                        token_id = HEX_LITERAL;
                                        c = peek_loc(0, &loc2);
                                        while (c != c1
                                                && c != '\n'
                                                && c != '\r')
                                        {
                                            get();
                                            if ((c <= '0'
                                                        && c <= '9')
                                                    || (c <= 'a'
                                                        && c <= 'f')
                                                    || (c <= 'A'
                                                        && c <= 'F'))
                                            {
                                                if (i >= MAX_NUMERIC_LITERAL)
                                                    break;
                                                str[i] = c;
                                                i++;
                                            }
                                            else
                                            {
                                                error_printf("%s:%d:%d: error: invalid hexadecimal digit\n",
                                                        loc2.filename,
                                                        loc2.line,
                                                        loc2.column);
                                                invalid_digit = 1;
                                            }

                                            c = peek_loc(0, &loc2);
                                        }
                                        break;
                                    }
                                default:
                                    internal_error("Code unreachable", 0);
                            }

                            if (c == c1)
                            {
                                str[i] = c1;
                                i++;
                                get();

                                c = peek(0);
                            }
                            else
                            {
                                error_printf("%s:%d:%d: error: unended integer literal\n",
                                        loc2.filename,
                                        loc2.line,
                                        loc2.column);
                                get();
                                continue;
                            }

                            if (invalid_digit)
                                continue;

                            if (i == 3)
                            {
                                error_printf("%s:%d:%d: error: empty integer literal\n",
                                        loc2.filename,
                                        loc2.line,
                                        loc2.column);
                                continue;
                            }

                            str[i] = '\0';
                            if (c == '_')
                            {
                                scan_kind(&str[i]);
                            }

                            return commit_text(token_id, str, loc);
                        }
                    }

                    // peek as many letters as possible
                    char identifier[MAX_IDENTIFIER_LENGTH];
                    identifier[0] = c0;

                    int i = 1;

                    char c = peek(0);
                    while (is_letter(c)
                            || c == '_')
                    {
                        get();
                        if (i >= MAX_IDENTIFIER_LENGTH)
                            break;
                        identifier[i] = c;
                        i++;

                        c = peek(0);
                    }
                    identifier[i] = '\0';

                    char c2 = peek(1);
                    if (i <= 33 // A named kind cannot be longer than 32 letters
                            && c == '_'
                            && (c2 == '\''
                                || c2 == '"'))
                    {
                        identifier[i] = c; get(); // overwrites '\0' above
                        i++;
                        identifier[i] = c2; get();
                        i++;
                        identifier[i] = '\0';

                        int t = scan_character_literal(identifier, /* delim */ c2, /* allow_suffix_boz */ 0, loc);
                        if (t != 0)
                            return t;
                        else
                            continue;
                    }

                    struct keyword_table_tag k;
                    k.keyword = identifier;

                    struct keyword_table_tag *result =
                        (struct keyword_table_tag*)
                        bsearch(&k, keyword_table,
                                sizeof(keyword_table) / sizeof(keyword_table[0]),
                                sizeof(keyword_table[0]),
                                keyword_table_comp);

                    if (result == NULL)
                    {
                        return commit_text(IDENTIFIER, identifier, loc);
                    }
                    else
                    {
                        return commit_text(result->token_id, identifier, loc);
                    }
                }
                // Numbers
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9':
                {
                    char digits[MAX_NUMERIC_LITERAL + MAX_KIND_LENGTH + 1];

                    digits[0] = c0;

                    int i = 1;
                    char c = peek(0);
                    while (is_decimal_digit(c))
                    {
                        get();
                        ERROR_CONDITION(i >= MAX_NUMERIC_LITERAL, "Too many digits (max=%d)", MAX_NUMERIC_LITERAL);
                        digits[i] = c;
                        i++;

                        c = peek(0);
                    }
                    digits[i] = '\0';

                    if (c == '.')
                    {
                        // There are two cases here
                        //   1.op. must be tokenized as DECIMAL_LITERAL USER_DEFINED_OPERATOR
                        //   1.2   must be tokenized as REAL_LITERAL
                        // note that
                        //     1.e.2 is the first case
                        //     1.e+2 is the second case
                    }
                    else if (c == '_')
                    {
                        // 1_"HELLO"
                        char c2 = peek(1);
                        if (c2 == '\''
                                || c2 == '"')
                        {
                            // note that since we limit the digit to MAX_NUMERIC_LITERAL there will still be room
                            // for the _ and " (or ')
                            digits[i] = c; get(); // overwrite '\0' above
                            i++;
                            digits[i] = c2; get();
                            i++;
                            digits[i] = '\0';

                            int t = scan_character_literal(/*prefix */ digits, /* delim */ c2, /* allow_suffix_boz */ 0, loc);
                            if (t != 0)
                                return t;
                            else
                                continue;
                        }
                        else
                        {
                            scan_kind(&digits[i]);
                            return commit_text(DECIMAL_LITERAL, digits, loc);
                        }
                    }
                    else if (c == 'h'
                            || c == 'H')
                    {
                        // This is a Holleritz constant
                        internal_error("Holleritz constant not yet implemented", 0);
                    }
                    else
                    {
                        return commit_text(DECIMAL_LITERAL, digits, loc);
                    }
                }
            default: { /* do nothing */ }
        }

        // Default case, unclassifiable token
        if (isprint(c0))
        {
            error_printf("%s:%d:%d: error: unexpected character: `%c' (0x%X)\n", 
                    loc.filename,
                    loc.line,
                    loc.column,
                    c0, c0);
        }
        else
        {
            error_printf("%s:%d:%d: error: unexpected character: 0x%X\n\n", 
                    loc.filename,
                    loc.line,
                    loc.column,
                    c0);
        }
    }
    internal_error("Code unreachable peek=%c", peek(0));
}
