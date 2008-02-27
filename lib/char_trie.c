#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// See below for public interface of uniqstr
// -- Private implementation of uniqstr
//
static unsigned long long int _bytes_used_char_trie = 0;

unsigned long long int char_trie_used_memory(void)
{
    return _bytes_used_char_trie;
}

typedef 
struct char_trie_tag char_trie_t;

typedef struct char_trie_element_tag
{
    char elem;
    // Only valid when elem == 0
    const char *str;
    char_trie_t* next;
} char_trie_element_t;

struct char_trie_tag
{
    int num_elements;
    char_trie_element_t* elements;
};

static struct char_trie_tag const CHAR_TRIE_INITIALIZER =
{
    .num_elements = 0,
    .elements = NULL
};

static char_trie_element_t *lookup_element(const char_trie_t* char_trie, char entity);

static const char* lookup_list(const char_trie_t* char_trie, const char* str, int length)
{
    if (length == 0)
    {
        if (char_trie->num_elements > 0
                && char_trie->elements[0].elem == 0)
        {
            return char_trie->elements[0].str;
        }
        return NULL;
    }
    else
    {
        char_trie_element_t* elem = lookup_element(char_trie, *str);
        if (elem == NULL)
            return NULL;
        else
            return lookup_list(elem->next, str + 1, length - 1);
    }
}

static const char* create_elements(char_trie_t* char_trie, const char *orig_str, const char* str, int length);


static const char* insert_list_rec(char_trie_t* char_trie, const char *orig_str, const char *str, int length)
{
    if (length == 0)
    {
        if ((char_trie->num_elements == 0)
                || (char_trie->elements[0].elem != 0))
        {
            return create_elements(char_trie, orig_str, str, length);
        }
        return NULL;
    }
    else
    {
        char_trie_element_t* elem = lookup_element(char_trie, *str);
        if (elem == NULL)
        {
            // Create all the remaining elements since they will not be found
            // anymore
            return create_elements(char_trie, orig_str, str, length);
        }
        else
        {
            return insert_list_rec(elem->next, orig_str, str + 1, length - 1);
        }
    }
}

static const char* insert_list(char_trie_t* char_trie, const char *orig_str, int length)
{
    return insert_list_rec(char_trie, orig_str, orig_str, length);
}

static char_trie_element_t *lookup_element_rec(const char_trie_t* char_trie, char entity, int lower, int upper)
{
    if (lower > upper)
        return NULL;

    int middle = (lower + upper) / 2;

    char i_middle = char_trie->elements[middle].elem;
    char i_entity = entity;

    if (i_entity < i_middle)
        return lookup_element_rec(char_trie, entity, lower, middle - 1);
    else if (i_middle < i_entity)
        return lookup_element_rec(char_trie, entity, middle + 1, upper);
    else 
        return &(char_trie->elements[middle]);
}

static char_trie_element_t *lookup_element(const char_trie_t* char_trie, char entity)
{
    return lookup_element_rec(char_trie, entity, 0, char_trie->num_elements - 1);
}

static int elements_compare(const void* p1, const void* p2)
{
    char_trie_element_t* t1 = (char_trie_element_t*)p1;
    char_trie_element_t* t2 = (char_trie_element_t*)p2;

    char i1 = t1->elem;
    char i2 = t2->elem;

    if (i1 == i2)
        return 0;
    else if (i1 < i2)
        return -1;
    else 
        return 1;
}

static const char* create_elements(char_trie_t* char_trie, const char* orig_str, const char* str, int length)
{
    char_trie->num_elements++;
    char_trie->elements = realloc(char_trie->elements, 
            char_trie->num_elements * sizeof(*(char_trie->elements)));
    if (length == 0)
    {
        char_trie->elements[char_trie->num_elements - 1].elem = 0;
        // No next after the "end of list"
        char_trie->elements[char_trie->num_elements - 1].next = NULL;
        // Store the original string (this should be the unique strdup ever)
        char_trie->elements[char_trie->num_elements - 1].str = strdup(orig_str);
        _bytes_used_char_trie += (strlen(orig_str) + 1);

        return char_trie->elements[char_trie->num_elements - 1].str;
    }
    else
    {
        char_trie->elements[char_trie->num_elements - 1].elem = *str;
        char_trie->elements[char_trie->num_elements - 1].next = calloc(1, sizeof(char_trie_t));
        _bytes_used_char_trie += sizeof(char_trie_t);

        const char* result =
        create_elements(char_trie->elements[char_trie->num_elements - 1].next,
                orig_str,
                str + 1, length - 1);

        qsort(char_trie->elements, char_trie->num_elements, sizeof(*(char_trie->elements)), 
                elements_compare);

        return result;
    }
}

typedef
struct stack_tag
{
    int pos;
    char elem;
} stack_t;

static void print_trie_rec(const char_trie_t* char_trie, int level, stack_t* stack)
{
    int i;

    for (i = 0; i < char_trie->num_elements; i++)
    {
        if (char_trie->elements[i].elem == 0)
        {
            int j;
            for (j = 0; j < level; j++)
            {
                fprintf(stderr, "[%d] %c ", stack[j].pos, stack[j].elem);
            }
            fprintf(stderr, "<end>\n");
        }
        else
        {
            stack[level].pos = i;
            stack[level].elem = char_trie->elements[i].elem;

            print_trie_rec(char_trie->elements[i].next, level + 1, stack);
        }
    }
}

static void print_trie(const char_trie_t* char_trie)
{
    stack_t stack[256];
    print_trie_rec(char_trie, 0, stack);
}

// -- Public interface for uniqstr
static char_trie_t _global_char_trie =
{
    .num_elements = 0,
    .elements = NULL
};

const char *uniquestr(const char* c)
{
    if (c == NULL)
        return NULL;

    const char* result = NULL;
    result = lookup_list(&_global_char_trie, c, strlen(c));
    if (result == NULL)
    {
        result = insert_list(&_global_char_trie, c, strlen(c));
    }

    return result;
}
