#ifndef CXX_TYPE_DECLS_H
#define CXX_TYPE_DECLS_H

#include "cxx-macros.h"

MCXX_BEGIN_DECLS

struct type_tag;
typedef struct type_tag type_t;

// Information of a parameter feeded to get_new_function_type
typedef 
struct parameter_info_tag
{
    // This parameter is '...'
    char is_ellipsis;
    // Otherwise it has the type here
    struct type_tag* type_info;
    // If not null, original_type holds the original type (array or function)
    struct type_tag* original_type;
} parameter_info_t;

// Standard conversions info
typedef
enum standard_conversion_item_tag
{
    SCI_INVALID = 0,
    SCI_NO_CONVERSION = 1,
    // Special value for identities
    SCI_IDENTITY,
    // Zero or one of these
    SCI_LVALUE_TO_RVALUE,
    SCI_ARRAY_TO_POINTER,
    SCI_FUNCTION_TO_POINTER,
    // Zero or one of these
    SCI_INTEGRAL_PROMOTION,
    SCI_VECTOR_INTEGRAL_PROMOTION,
    SCI_FLOATING_PROMOTION,
    SCI_INTEGRAL_CONVERSION,
    SCI_FLOATING_CONVERSION,
    SCI_FLOATING_INTEGRAL_CONVERSION,
    SCI_POINTER_CONVERSION,
    SCI_POINTER_TO_MEMBER_CONVERSION,
    SCI_BOOLEAN_CONVERSION,
    // Zero or one of this
    SCI_QUALIFICATION_CONVERSION,
} standard_conversion_item_t;

typedef
struct standard_conversion_tag
{
    struct type_tag* orig;
    struct type_tag* dest;
    standard_conversion_item_t conv[3];
} standard_conversion_t;

extern const standard_conversion_t no_scs_conversion;

MCXX_END_DECLS

#endif // CXX_TYPE_DECLS_H
