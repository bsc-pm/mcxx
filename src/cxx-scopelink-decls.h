#ifndef CXX_SCOPELINK_DECLS_H
#define CXX_SCOPELINK_DECLS_H

#include "cxx-macros.h"
#include "hash.h"

MCXX_BEGIN_DECLS

typedef 
struct scope_link_tag
{
    Hash* h;
} scope_link_t;

MCXX_END_DECLS

#endif
