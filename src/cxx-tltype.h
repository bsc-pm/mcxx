#ifndef CXX_TLTYPES_H
#define CXX_TLTYPES_H

#include "cxx-macros.h"
#include "cxx-ast.h"

MCXX_BEGIN_DECLS

union tl_type_data_tag;

typedef struct tl_type_array_tag
{
    int _elements;
    union tl_type_data_tag* _array;
} tl_type_array_t;

typedef union tl_type_data_tag
{
    int _integer;
    char _boolean;
    AST _ast;
    tl_type_array_t _array;
    char* _string;
} tl_type_data_t;

typedef enum tl_type_kind_tag
{
    TL_UNDEFINED = 0,
    TL_INTEGER,
    TL_BOOL,
    TL_AST,
    TL_STRING,
    TL_ARRAY
} tl_type_kind_t;

typedef struct tl_type_tag
{
    tl_type_kind_t kind;
    tl_type_data_t data;
} tl_type_t;

tl_type_t tl_bool(char c);
tl_type_t tl_integer(int i);
tl_type_t tl_ast(AST a);
tl_type_t tl_string(const char* str);

MCXX_END_DECLS

#endif // CXX_TLTYPES_H
