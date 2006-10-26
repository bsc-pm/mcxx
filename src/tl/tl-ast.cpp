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

void AST_t::replace_with(AST_t* ast)
{
	AST orig_tree = this->_ast;
	*(this->_ast) = *(ast->_ast);
	relink_parent(orig_tree, this->_ast);
}

AST_t* AST_t::duplicate() const
{
	AST_t* result = new AST_t(duplicate_ast(this->_ast));
	return result;
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

void AST_t::append_to_translation_unit(AST_t* tree)
{
	AST this_translation_unit = get_translation_unit(this->_ast);
	AST tree_translation_unit = get_translation_unit(tree->_ast);

	AST this_declaration_seq = ASTSon0(this_translation_unit);
	AST tree_declaration_seq = ASTSon0(tree_translation_unit);

	if (this_declaration_seq == NULL)
	{
		ASTSon0(this_translation_unit) = tree_declaration_seq;
	}
	else
	{
		append_list(this_declaration_seq, tree_declaration_seq);
	}
}

void AST_t::append_list(AST orig_list, AST appended_list)
{
	if (ASTType(orig_list) != AST_NODE_LIST
			|| ASTType(appended_list) != AST_NODE_LIST)
	{
		std::cerr << "You tried to append two lists that are not" << std::endl;
		return;
	}

	// Appending one list to another is as easy as making the deeper node
	// "appended_list" to point to the top node of "orig_list"
	
	// First replace appended_list as the new child of the parent of orig_list
	relink_parent(orig_list, appended_list);
	
	// Now link the deeper node of the list to the top node of "orig_list"
	AST iter = appended_list;
	while (ASTSon0(iter) != NULL)
	{
		iter = ASTSon0(iter);
	}

	ASTSon0(iter) = orig_list;
	ASTParent(orig_list) = iter;
}

void AST_t::relink_parent(AST previous_child, AST new_child)
{
	AST parent = ASTParent(previous_child);
	int i;
	for (i = 0; i < ASTNumChildren(parent); i++)
	{
		if (ASTChild(parent, i) == previous_child)
			break;
	}
	if (i == ASTNumChildren(parent))
	{
		std::cerr << "Claimed parent does not have the node as a child" << std::endl;
		return;
	}

	AST parent_new_child = ASTParent(new_child);
	if (parent_new_child != NULL)
	{
		int j;
		for (j = 0; j < ASTNumChildren(parent_new_child); j++)
		{
			if (ASTChild(parent_new_child, j) == new_child)
				break;
		}
		if (j == ASTNumChildren(parent))
		{
			std::cerr << "Claimed parent of new child does not have the node as a child" << std::endl;
			return;
		}

		// Disable for sanity this son now
		ASTChild(parent, j) = NULL;
	}

	// Relink
	ASTChild(parent, i) = new_child;
	ASTParent(new_child) = parent;
}

AST AST_t::get_translation_unit(AST node)
{
	if  (node == NULL)
		return NULL;

	if (ASTType(node) == AST_TRANSLATION_UNIT)
	{
		return node;
	}

	return ASTParent(node);
}

}
