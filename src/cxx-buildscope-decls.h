#ifndef CXX_BUILDSCOPE_DECLS_H
#define CXX_BUILDSCOPE_DECLS_H

#include "cxx-scope-decls.h"

typedef 
struct gather_decl_spec_tag {
    char is_auto;
    char is_register;
    char is_static;
    char is_extern;
    char is_mutable;
    char is_thread;
    char is_friend;
    char is_typedef;
    char is_signed;
    char is_unsigned;
    char is_short;
    char is_long;
    char is_const;
    char is_volatile;
    char is_inline;
    char is_virtual;
    char is_explicit;
    char is_complex;
} gather_decl_spec_t;

#define BITMAP(x) (1 << (x))

typedef 
enum decl_flags_tag
{
    DF_NONE = 0,
    DF_TEMPLATE = BITMAP(0),
    DF_CONSTRUCTOR = BITMAP(1),
    DF_NO_DECLARATORS = BITMAP(2),
    DF_FRIEND = BITMAP(3),
    DF_EXPLICIT_SPECIALIZATION = BITMAP(4),
    DF_NO_FAIL = BITMAP(5),
    DF_ALLOW_REDEFINITION = BITMAP(6),
    DF_ALWAYS_CREATE_SPECIALIZATION = BITMAP(7),
} decl_flags_t;

#undef BITMAP


// Inherited attributes
typedef 
struct decl_context_tag
{
    // Several declaration flags
    decl_flags_t decl_flags;

    // Template nesting level
    int template_nesting;

    // Template parameter information without taking
    // into account the current scope
    template_parameter_t** template_parameters;
    int num_template_parameters;

    // Used in template functions
    template_parameter_t** template_parameters_in_scope;
    int num_template_parameters_in_scope;
} decl_context_t;

#endif // CXX_BUILDSCOPE_DECLS_H
