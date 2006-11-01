#include "tl-builtin.hpp"
#include "tl-object.hpp"
#include "tl-ast.hpp"

namespace TL
{

Object Object::get_attribute(const std::string& name) const
{
	tl_type_t* tl_value = this->get_extended_attribute(name);

	if (tl_value == NULL)
	{
		return Undefined();
	}

	switch (tl_value->kind)
	{
		case TL_INTEGER :
			{
				TL::Integer i(tl_value->data._integer);
				return i;
				break;
			}
		case TL_BOOL :
			{
				TL::Bool b(tl_value->data._boolean);
				return b;
				break;
			}
		case TL_ARRAY :
			{
// #warning Implement this
				return TL::Undefined();
				break;
			}
		case TL_AST :
			{
				TL::AST_t ast(tl_value->data._ast);
				return ast;
				break;
			}
		default:
			{
				return TL::Undefined();
				break;
			}
	}
}

}
