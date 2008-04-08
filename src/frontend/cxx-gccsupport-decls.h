#ifndef CXX_GCCSUPPORT_DECLS_H
#define CXX_GCCSUPPORT_DECLS_H

#define MAX_GCC_ATTRIBUTES_PER_SYMBOL (16)

typedef struct gather_gcc_attribute_tag
{
    const char *attribute_name;
    AST expression_list;
} gather_gcc_attribute_t;

#endif // CXX_GCCSUPPORT_DECLS_H
