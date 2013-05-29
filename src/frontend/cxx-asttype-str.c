#include "cxx-asttype-str.h"
#include "cxx-process.h"

extern struct node_str_t * ast_node_name_to_kind_ (register const char *str, register unsigned int len);

node_t ast_node_name_to_kind(const char* name)
{
	ERROR_CONDITION(name == NULL, "Invalid name", 0);
	node_str_t* n = ast_node_name_to_kind_(name, strlen(name));

	if (n == NULL)
		return AST_INVALID_NODE;
	else
		return n->kind;
}
