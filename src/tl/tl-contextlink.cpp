#include "tl-contextlink.hpp"
namespace TL
{

Context* ContextLink::get_context(AST_t& ast)
{
	AST _ast = ast._ast;
	scope_t* st = scope_link_get(_scope_link, _ast);

	if (st == NULL)
		return NULL;

	Context* result = new Context(st);

	return result;
}

}
