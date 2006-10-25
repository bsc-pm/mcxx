#include "tl-builtin.hpp"
#include "tl-ast.hpp"

namespace TL
{

tl_type_t* AST_t::get_extended_attribute(const std::string& name) const
{
    //  First get the extended attribute
    void* p = extensible_struct_get_field_pointer(&ast_extensible_schema,
            &(this->_ast->extended_data),
            name.c_str());

    return (tl_type_t*) p;
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

AST_t AST_t::duplicate() const
{
	AST_t result(duplicate_ast(this->_ast));
	return result;
}

void AST_t::add_sibling(AST_t& t)
{
#warning TODO - Implement this
}

std::vector<AST_t> AST_t::get_all_subtrees_predicate(const Predicate& p) const
{
	std::vector<AST_t> result;
	tree_iterator(*this, p, result);

	return result;
}

void AST_t::tree_iterator(const AST_t& a, const Predicate& p, std::vector<AST_t>& result) const
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
