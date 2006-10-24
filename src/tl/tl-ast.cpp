#include "tl-builtin.hpp"
#include "tl-ast.hpp"
#include "cxx-tltype.h"
#include "extstruct.h"

namespace TL
{

Object* AST_t::attributes(const std::string& name) const
{
    //  First get the extended attribute
    void* p = extensible_struct_get_field_pointer(&ast_extensible_schema,
            &(this->_ast->extended_data),
            name.c_str());

    if (p == NULL)
        return NULL;

    tl_type_t* tl_value = (tl_type_t*) p;

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
                break;
            }
        case TL_AST :
            {
                TL::AST_t* ast = new TL::AST_t(tl_value->data._ast);
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

std::string AST_t::prettyprint() const
{
	char* c = prettyprint_in_buffer(this->_ast);
	std::string result(c);
	return result;
}

void AST_t::replace_with(const AST_t& ast)
{
#warning TODO - Check if this makes sense
	*(this->_ast) = *_ast;
}

AST_t AST_t::duplicate()
{
	AST_t result(duplicate_ast(this->_ast));
	return result;
}

void AST_t::add_sibling(AST_t& t)
{
#warning TODO - Implement this
}

std::vector<AST_t> AST_t::get_all_subtrees_predicate(const Predicate& p)
{
	std::vector<AST_t> result;
	tree_iterator(*this, p, result);

	return result;
}

void AST_t::tree_iterator(const AST_t& a, const Predicate& p, std::vector<AST_t>& result)
{
	if (p(a))
	{
		result.push_back(a);
	}

	AST tree = a._ast;

	for (int i = 0; i < ASTNumChildren(tree); i++)
	{
		if (ASTChild(tree, i) != NULL)
		{
			tree_iterator(AST_t(ASTChild(tree, i)), p, result);
		}
	}
}

std::string AST_t::internal_ast_type() const
{
	char* inner_name = ast_node_names[ASTType(this->_ast)];
	std::string result(inner_name);
	return result;
}

}
