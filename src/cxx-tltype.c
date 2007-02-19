#include <string.h>
#include "cxx-tltype.h"

tl_type_t tl_bool(char c)
{
    tl_type_t result;

    result.kind = TL_BOOL;
    result.data._boolean = c;

    return result;
}

tl_type_t tl_integer(int i)
{
    tl_type_t result;

    result.kind = TL_INTEGER;
    result.data._integer = i;

    return result;
}

tl_type_t tl_ast(AST a)
{
    tl_type_t result;

    result.kind = TL_AST;
    result.data._ast = a;

    return result;
}

tl_type_t tl_string(const char* str)
{
    tl_type_t result;
    
    result.kind = TL_STRING;
    result.data._string = strdup(str);

    return result;
}
