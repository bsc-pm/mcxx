#include "tl-scopelink.hpp"
namespace TL
{

Scope* ScopeLink::get_scope(AST_t* ast)
{
	if (ast == NULL)
	{
		return NULL;
	}

	AST _ast = ast->_ast;
	scope_t* st = scope_link_get(_scope_link, _ast);

	if (st == NULL)
		return NULL;

	Scope* result = new Scope(st);

	return result;
}

}
