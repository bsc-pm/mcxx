#include "cxx-locus.h"
#include "mem.h"
#include <string.h>
#include "uniquestr.h"
#include "string_utils.h"

struct locus_tag
{
    const char* filename;
    unsigned int line, col;
};

// Heavily inspired in lib/char_hash.c contributed by Jan Hoogerbrugge

struct locus_link
{
    unsigned int hash;
    locus_t* locus;
    struct locus_link* next;
};

static struct locus_link *hash_table[49999];

static unsigned int hash_locus(const char *filename, unsigned int line, unsigned int col)
{
    unsigned int hash = 0;
    const char *p;

    for (p = filename; *p; p++)
        hash = ((hash << 5) + hash) ^ *p;

    hash = ((hash << 5) + hash) ^ line;
    hash = ((hash << 5) + hash) ^ col;

    return hash;
}

const locus_t* make_locus(const char* filename, unsigned int line, unsigned int col)
{
    if (filename == NULL)
        filename = "";

    unsigned int hash = hash_locus(filename, line, col);
    unsigned int hash_index = hash % (sizeof(hash_table) / sizeof(hash_table[0]));


    struct locus_link *p, *p_prev = 0, *new_link;

    for (p = hash_table[hash_index]; p; p_prev = p, p = p->next)
    {
        if (p->hash == hash
                && strcmp(p->locus->filename, filename) == 0
                && p->locus->line == line
                && p->locus->col == col)
        {
            // Move to the head of the list to favour temporal locality
            if (p != hash_table[hash_index])
            {
                p_prev->next = p->next;
                p->next = hash_table[hash_index];
                hash_table[hash_index] = p;
            }

            return p->locus;
        }
    }

    new_link = xmalloc(sizeof(*new_link));
    new_link->locus = xmalloc(sizeof(*new_link->locus));
    new_link->locus->filename = uniquestr(filename);
    new_link->locus->line = line;
    new_link->locus->col = col;
    new_link->hash = hash; 
    new_link->next = hash_table[hash_index];
    hash_table[hash_index] = new_link; 

    return new_link->locus;
}

const char* locus_to_str(const locus_t* l)
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

const char* locus_get_filename(const locus_t* l)
{
    if (l == NULL)
        return "";
    return l->filename;
}

unsigned int locus_get_line(const locus_t* l)
{
    return l == NULL ? 0 : l->line;
}

unsigned int locus_get_col(const locus_t* l)
{
    return l == NULL ? 0 : l->col;
}
