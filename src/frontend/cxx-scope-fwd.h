#ifndef CXX_SCOPE_FWD_H
#define CXX_SCOPE_FWD_H

#include "cxx-macros.h"

MCXX_BEGIN_DECLS

struct scope_entry_tag;
typedef struct scope_entry_tag scope_entry_t;

struct decl_context_tag;
typedef struct decl_context_tag decl_context_t;

struct template_parameter_tag;
typedef struct template_parameter_tag template_parameter_t;

struct template_parameter_value_tag;
typedef struct template_parameter_value_tag template_parameter_value_t; 

struct template_parameter_list_tag;
typedef struct template_parameter_list_tag template_parameter_list_t;

struct default_argument_info_tag;
typedef struct default_argument_info_tag default_argument_info_t;

struct scope_tag;
typedef struct scope_tag scope_t;

MCXX_END_DECLS

#endif // CXX_SCOPE_FWD_H
