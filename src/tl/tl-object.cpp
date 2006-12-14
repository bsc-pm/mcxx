#include "tl-builtin.hpp"
#include "tl-object.hpp"
#include "tl-ast.hpp"

namespace TL
{

bool Object::has_attribute(const std::string& name) const
{
	return (this->get_extended_attribute(name) != NULL);
}

Object& Object::get_attribute(const std::string& name) const
{
	tl_type_t* tl_value = this->get_extended_attribute(name);

	if (tl_value == NULL)
	{
		std::cerr << "Attribute '" << name << "' not found" << std::endl;
		TL::Undefined* und = new TL::Undefined();
		return *(und);
	}

	switch (tl_value->kind)
	{
		case TL_INTEGER :
			{
				Integer* i = new Integer(tl_value->data._integer);
				return (*i);
				break;
			}
		case TL_BOOL :
			{
				Bool* b = new Bool(tl_value->data._boolean);
				return (*b);
				break;
			}
		case TL_AST :
			{
				AST_t* ast = new AST_t(tl_value->data._ast);
				return (*ast);
				break;
			}
		case TL_STRING :
			{
				String* str = new String(tl_value->data._string);
				return (*str);
				break;
			}
		case TL_ARRAY :
		case TL_UNDEFINED :
			{
// #warning Implement this
				break;
			}
	}

	TL::Undefined* und = new TL::Undefined();
	return (*und);
}

}
