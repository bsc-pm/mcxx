#include "tl-builtin.hpp"
#include "tl-object.hpp"
#include "tl-ast.hpp"

namespace TL
{

Object* Object::get_attribute(const std::string& name) const
{
	tl_type_t* tl_value = this->get_extended_attribute(name);

	if (tl_value == NULL)
	{
		return NULL;
	}

	switch (tl_value->kind)
	{
		case TL_INTEGER :
			{
				TL::Integer* i = new TL::Integer(tl_value->data._integer);
				return i;
				break;
			}
		case TL_BOOL :
			{
				TL::Bool* b = new TL::Bool(tl_value->data._boolean);
				return b;
				break;
			}
		case TL_ARRAY :
			{
#warning Implement this
				return new TL::Undefined();
				break;
			}
		case TL_AST :
			{
				TL::AST_t* ast = TL::AST_t::wrap_ast(tl_value->data._ast);
				return ast;
				break;
			}
		default:
			{
				return new TL::Undefined();
				break;
			}
	}
}

}
