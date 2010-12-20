#ifndef CXX_PRETTYPRINT_INTERNAL_H
#define CXX_PRETTYPRINT_INTERNAL_H

#if !defined(CXX_PRETTYPRINT_C) && !defined(FORTRAN_PRETTYPRINT_C)
  #error Wrongly included file. Include it only from cxx-prettyprint.c or fortran-prettyprint.c
#endif

typedef struct prettyprint_context_tag
{
    int level;
    const char *indent_str;
    char internal_output;
    prettyprint_callback_t callback;
    void *callback_data;

    // Angular brackets are so troublesome in C++
    char last_is_left_angle;
    char last_is_right_angle;

    // Column
    int column;
} prettyprint_context_t;

typedef
struct prettyprint_behaviour_tag
{
    // States if the output is meant to be internal
    // i.e.: comments and preprocessor tokens will be output with special
    // keeping marks or will be converted to standard syntax
    char internal_output;
} prettyprint_behaviour_t;


typedef void (*prettyprint_handler_t)(FILE* f, AST a, prettyprint_context_t* pt_ctx);

typedef struct {
    char* handler_name;
    prettyprint_handler_t handler;
    char* parameter;
} prettyprint_entry_t;

#define HANDLER_PROTOTYPE(name) \
 static void name(FILE* f, AST a, prettyprint_context_t* pt_ctx)

#define NODE_HANDLER(type, handler, parameter) \
  [type] = {#handler, handler, parameter}

// Initial behaviour
static prettyprint_behaviour_t prettyprint_behaviour = 
{ 
    /* .internal_output = */ 1 
};

void prettyprint_set_not_internal_output(void)
{
    prettyprint_behaviour.internal_output = 0;
}

void prettyprint_set_internal_output(void)
{
    prettyprint_behaviour.internal_output = 1;
}

static void prettyprint_context_init(prettyprint_context_t* pt_ctx)
{
    memset(pt_ctx, 0, sizeof(*pt_ctx));
    // This will be configurable one day
    pt_ctx->indent_str = "    ";
    pt_ctx->level = 0;
    pt_ctx->internal_output = prettyprint_behaviour.internal_output;
}

static void prettyprint_context_copy(prettyprint_context_t* dest,
        const prettyprint_context_t* src)
{
    // Nothing else must be done
    memcpy(dest, src, sizeof(*dest));
}


#define NEW_PT_CONTEXT(_name, _init_fun) \
    prettyprint_context_t _v_##_name; \
    prettyprint_context_t *_name = &_v_##_name; \
    prettyprint_context_copy(_name, pt_ctx); \
    _init_fun(_name)

#define NEW_PT_CONTEXT_ARG(_name, _init_fun, _arg) \
    prettyprint_context_t _v_##_name; \
    prettyprint_context_t *_name = &_v_##_name; \
    prettyprint_context_copy(_name, pt_ctx); \
    _init_fun(_name, _arg)

static void prettyprint_level(FILE* f, AST a, prettyprint_context_t* pt_ctx);

#define HELPER_PARAMETER \
    (handlers_list[ASTType(a)].parameter)

#define HELPER_PARAMETER_STRING \
    ((handlers_list[ASTType(a)].parameter != NULL) ? (handlers_list[ASTType(a)].parameter) : "")


static char* prettyprint_in_buffer_common(AST a, 
        void (*pretty_func)(FILE*, AST, prettyprint_context_t* pt_ctx), 
        prettyprint_context_t *pt_ctx)
{
    char *result = NULL;
#ifdef HAVE_OPEN_MEMSTREAM
    size_t size = 0;

    FILE* temporal_stream = open_memstream(&result, &size);
    pretty_func(temporal_stream, a, pt_ctx);
    fclose(temporal_stream);
#else
    FILE* temporal_file = tmpfile();

    pretty_func(temporal_file, a, pt_ctx);

    int bytes_file = ftell(temporal_file) + 20;
    rewind(temporal_file);

    result = calloc(bytes_file, sizeof(char));
    fread(result, bytes_file, sizeof(char), temporal_file);
    fclose(temporal_file);
#endif
    int c = strlen(result) - 1;

    while (result[c] == '\n')
    {
        result[c] = '\0';
        c--;
    }

    return result;
}

static int character_level_vfprintf(FILE* stream, prettyprint_context_t *pt_ctx, const char* format, va_list args)
{
    int result;
    int size = 512;
    char* c = calloc(size, sizeof(char));
    va_list va;

    va_copy(va, args);
    result = vsnprintf(c, size, format, va);
    va_end(va);

    while (result < 0 || result >= size)
    {
        va_copy(va, args);
        size *= 2;
        free(c);
        c = calloc(size, sizeof(char));
        result = vsnprintf(c, size, format, va);
        va_end(va);
    }

    fprintf(stream, "%s", c);

    if (result > 0)
    {
        pt_ctx->last_is_left_angle = (c[result - 1] == '<');
        pt_ctx->last_is_right_angle = (c[result - 1] == '>');

        char *p = strrchr(c, '\n');
        if (p == NULL)
        {
            pt_ctx->column += strlen(c);
        }
        else
        {
            pt_ctx->column = (ptrdiff_t)((&c[result-1]) - p);
        }
    }

    free(c);

    return result;
}


static int token_fprintf(FILE *stream, AST node UNUSED_PARAMETER, prettyprint_context_t* pt_ctx, const char *format, ...)
{
    int result = 0;
    va_list args;

    va_start(args, format);
    result = character_level_vfprintf(stream, pt_ctx, format, args);
    va_end(args);

    return result;
}


#endif // CXX_PRETTYPRINT_INTERNAL_H
