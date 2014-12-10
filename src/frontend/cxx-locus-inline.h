#include "cxx-locus.h"
#include <stdlib.h>
#include "string_utils.h"

struct locus_tag
{
    const char* filename;
    unsigned int line, col;
};

static inline const char* locus_to_str(const locus_t* l)
{
    const char* result = NULL;
    if (l == NULL)
        return ":0";

    if (l->col != 0)
        uniquestr_sprintf(&result, "%s:%d:%d", l->filename, l->line, l->col);
    else
        uniquestr_sprintf(&result, "%s:%d", l->filename, l->line);

    return result;
}

static inline const char* locus_get_filename(const locus_t* l)
{
    if (l == NULL)
        return "";
    return l->filename;
}

static inline unsigned int locus_get_line(const locus_t* l)
{
    return l == NULL ? 0 : l->line;
}

static inline unsigned int locus_get_col(const locus_t* l)
{
    return l == NULL ? 0 : l->col;
}
