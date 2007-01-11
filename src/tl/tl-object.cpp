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
	// Fix this function because it leaks everythime you call it
	//
	//   * we want to return a reference in order to make it convertible to any
	//   derived class of Object without slicing the result
	//
	//   * returning a pointer makes the code ugly thus returning a reference works
	//   * maybe using another approach like
	//
	//     void get_attribute(const std::string& name, <derived-class-of-object>& result);
	//     
	//     would be better (one function for every type)

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
