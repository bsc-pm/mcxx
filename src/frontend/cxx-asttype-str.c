#include "libmcxx-common.h"
#include "cxx-macros.h"
#include "cxx-asttype-str.h"
#include "cxx-asttype.h"
#include "cxx-process.h"
#include <string.h>

// Workaround caused by an __inline added by gperf
// that causes link issues in gcc
#define __inline
#define __attribute__(X)
#include "cxx-asttype-str-internal.h"
#undef __inline
#undef __attribute__
//

node_t ast_node_name_to_kind(const char* name)
{
	ERROR_CONDITION(name == NULL, "Invalid name", 0);
	node_str_t* n = ast_node_name_to_kind_(name, strlen(name));

	if (n == NULL)
		return AST_INVALID_NODE;
	else
		return n->kind;
}
