#ifndef CXX_SCOPELINK_H
#define CXX_SCOPELINK_H

#include "hash.h"

typedef struct scope_link_tag
{
	Hash* h;
} scope_link_t;

scope_link_t* new_scope_link(void);

#endif // CXX_SCOPELINK_H
