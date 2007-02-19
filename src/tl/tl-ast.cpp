#include "tl-builtin.hpp"
#include "tl-ast.hpp"
#include "tl-scopelink.hpp"
#include "tl-predicate.hpp"
#include "cxx-ast.h"
#include <sstream>

namespace TL
{
    bool AST_t::operator<(AST_t n) const
    {
        return this->_ast < n._ast;
    }

    bool AST_t::operator==(AST_t n) const
    {
        return ast_equal(this->_ast, n._ast);
    }

    bool AST_t::operator!=(AST_t n) const
    {
        return !(this->operator==(n));
    }

    AST_t& AST_t::operator=(AST_t n)
    {
        this->_ast = n._ast;
        return (*this);
    }

    tl_type_t* AST_t::get_extended_attribute(const std::string& name) const
    {
        //  First get the extended attribute
        void* p = extensible_struct_get_field_pointer(&ast_extensible_schema,
                this->_ast->extended_data,
                name.c_str());

        return (tl_type_t*) p;
    }

    std::string AST_t::prettyprint(bool with_commas) const
    {
        if (with_commas && ASTType(this->_ast) == AST_NODE_LIST)
        {
            return list_handler_in_buffer(this->_ast);
        }
        else
        {
            char* c = prettyprint_in_buffer(this->_ast);
            std::string result(c);
            return result;
        }
    }

    void AST_t::replace(AST_t ast)
    {
        if (ASTType(ast._ast) == AST_NODE_LIST)
        {
            // If the replacement is a list but the original is not, let's check two cases
            // maybe this list is a one-element list or not.
            if (ASTType(this->_ast) != AST_NODE_LIST)
            {
                // If it is a one element list
                if (ASTSon0(ast._ast) == NULL)
                {
                    // then replace the whole thing with the list information
                    AST_t repl_tree(ASTSon1(ast._ast));
                    replace_with(repl_tree);
                }
                // If this is not a one-element list then try to replace using
                // a typical replace_in_list but this may fail sometimes
                // because we'll look for the first enclosing list
                else 
                {
                    // Maybe we should yield a message here
                    std::cerr << "Warning: Replacing a non-list tree at '" 
                        << this->get_locus() 
                        << "' with a list tree of more than one element" << std::endl;
                    replace_in_list(ast);
                }
            }
            // If both are lists is easy
            else
            {
                replace_in_list(ast);
            }
        }
        // If the thing being replaced is a list, but the replacement
        // is not, then convert the latter into a list
        else if (ASTType(_ast) == AST_NODE_LIST
                && ASTType(_ast) != AST_NODE_LIST)
        {
            // Create a single element list
            AST single(ASTListLeaf(ast._ast));
            replace_in_list(single);
        }
        // Otherwise replace directly. Neither the replaced nor the replacement
        // are lists in this case.
        else
        {
            replace_with(ast);
        }
    }

    void AST_t::replace_with(AST_t ast)
    {
        AST previous_parent = ASTParent(this->_ast);
        *(this->_ast) = *(ast._ast);
        ASTParent(this->_ast) = previous_parent;

        // Relink sons
        for (int i = 0; i < ASTNumChildren(this->_ast); i++)
        {
            if (ASTChild(this->_ast, i) != NULL)
            {
                ASTParent(ASTChild(this->_ast, i)) = this->_ast;
            }
        }
    }

    AST_t AST_t::duplicate() const
    {
        AST_t result(duplicate_ast(this->_ast));
        return result;
    }

    std::pair<AST_t, ScopeLink> AST_t::duplicate_with_scope(ScopeLink scope_link) const
    {
        scope_link_t* new_sl = scope_link_new();

        AST duplicated_tree = duplicate_ast_with_scope_link(this->_ast, scope_link._scope_link, new_sl);

        ScopeLink sl(new_sl);
        AST_t ast(duplicated_tree);
        std::pair<AST_t, ScopeLink> result(ast, sl);

        return result;
    }

    void AST_t::tree_iterator(AST_t& a, const Predicate<AST_t>& predicate, 
            RecursiveFlag recursive_flag, ObjectList<AST_t>& result)
    {
        AST tree = a._ast;
        if (tree == NULL)
            return;

        bool matched = false;
        if (predicate(a))
        {
            matched = true;
            result.push_back(a);
        }

        if ((recursive_flag == RECURSIVE)
                || (recursive_flag == NON_RECURSIVE && !matched)
                || (recursive_flag == LIST_TRIP && matched))
        {
            for (int i = 0; i < ASTNumChildren(tree); i++)
            {
                if (ASTChild(tree, i) != NULL)
                {
                    AST_t iterate(ASTChild(tree, i));
                    tree_iterator(iterate, predicate, recursive_flag, result);
                }
            }
        }
    }

    ObjectList<AST_t> AST_t::depth_subtrees(const Predicate<AST_t>& pred, RecursiveFlag recursive_flag)
    {
        ObjectList<AST_t> result;

        tree_iterator(*this, pred, recursive_flag, result);

        return result;
    }

    std::string AST_t::internal_ast_type() const
    {
        char* inner_name = ast_node_names[ASTType(this->_ast)];
        std::string result(inner_name);
        return result;
    }

    void AST_t::append_to_translation_unit(AST_t tree)
    {
        AST this_translation_unit = get_translation_unit(this->_ast);

        AST this_declaration_seq = ASTSon0(this_translation_unit);
        AST tree_declaration_seq = tree._ast;

        if (this_declaration_seq == NULL)
        {
            ASTSon0(this_translation_unit) = tree_declaration_seq;
        }
        else
        {
            append_list(this_declaration_seq, tree_declaration_seq);
        }
    }

    void AST_t::prepend_to_translation_unit(AST_t tree)
    {
        AST this_translation_unit = get_translation_unit(this->_ast);

        AST this_declaration_seq = ASTSon0(this_translation_unit);

        // Go to the very first one!
        while (ASTSon0(this_declaration_seq) != NULL)
        {
            this_declaration_seq = ASTSon0(this_declaration_seq);
        }

        AST tree_declaration_seq = tree._ast;

        if (this_declaration_seq == NULL)
        {
            ASTSon0(this_translation_unit) = tree_declaration_seq;
        }
        else
        {
            prepend_list(this_declaration_seq, tree_declaration_seq);
        }
    }

    void AST_t::prepend_list(AST orig_list, AST prepended_list)
    {
        if (ASTType(orig_list) != AST_NODE_LIST
                || ASTType(prepended_list) != AST_NODE_LIST)
        {
            std::cerr << "You tried to prepend two lists that are not " 
                << "orig_list=" << ast_print_node_type(ASTType(orig_list)) << " "
                << "prepend_list=" << ast_print_node_type(ASTType(prepended_list)) << std::endl;
            return;
        }

        // Relink the parent, first remove pointer to the prepended_list
        if (ASTParent(prepended_list) != NULL)
        {
            AST parent = ASTParent(prepended_list);
            for (int i = 0; i < ASTNumChildren(parent); i++)
            {
                if (ASTChild(parent, i) == prepended_list)
                {
                    ASTChild(parent, i) = NULL;
                    break;
                }
            }
        }

        // Now make the prepended_list as the son
        AST original_previous = ASTSon0(orig_list);

        ASTSon0(orig_list) = prepended_list;
        ASTParent(prepended_list) = orig_list;

        // Go to the deeper node of prepended_list
        AST iter = prepended_list;
        while (ASTSon0(iter) != NULL)
        {
            iter = ASTSon0(iter);
        }

        ASTSon0(iter) = original_previous;
        if (original_previous != NULL)
        {
            ASTParent(original_previous) = iter;
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

    AST AST_t::get_translation_unit()
    {
        return this->get_translation_unit(this->_ast);
    }

    AST AST_t::get_translation_unit(AST node)
    {
        if  (node == NULL)
            return NULL;

        while (node != NULL && 
                ASTType(node) != AST_TRANSLATION_UNIT)
        {
            node = ASTParent(node);
        }

        return node;
    }

    AST_t AST_t::get_enclosing_block()
    {
        AST node = _ast;
        while (!is_extensible_block(node))
        {
            node = ASTParent(node);
        }

        AST_t result(node);
        return result;
    }

    bool AST_t::is_extensible_block(AST node)
    {
        return (ASTType(node) == AST_COMPOUND_STATEMENT
                || ASTType(node) == AST_CLASS_SPECIFIER
                || ASTType(node) == AST_TRANSLATION_UNIT
                || ASTType(node) == AST_NAMESPACE_DEFINITION);
    }

    AST AST_t::get_list_of_extensible_block(AST node)
    {
        switch ((int)ASTType(node))
        {
            case AST_COMPOUND_STATEMENT :
                {
                    // This can be null
                    return ASTSon0(node);
                    break;
                }
            case AST_CLASS_SPECIFIER :
                {
                    // This one has to be handled specially because of its
                    // special nature
                    return ASTSon1(node);
                    break;
                }
            case AST_TRANSLATION_UNIT :
                {
                    // This can be null
                    return ASTSon0(node);
                    break;
                }
            case AST_NAMESPACE_DEFINITION :
                {
                    // This can be null
                    return ASTSon1(node);
                    break;
                }
            case AST_NODE_LIST :
                {
                    return node;
                    break;
                }
            default:
                return NULL;
        }
    }


    void AST_t::append_to_member_spec(AST member_spec, AST member_decl)
    {
        // AST new_member_spec = ASTMake3(AST_MEMBER_SPEC, NULL, member_decl, 
        //         ASTSon2(member_spec), ASTLine(member_spec), NULL);
        // ASTSon2(member_spec) = new_member_spec;
        // ASTParent(new_member_spec) = member_spec;
    }

    void AST_t::prepend_to_member_spec(AST member_spec, AST member_decl)
    {
        // AST orig_parent = ASTParent(member_spec);
        // AST new_member_spec = NULL;
        // if (ASTSon0(member_spec) != NULL)
        // {
        //     AST aux_member_spec = ASTMake3(AST_MEMBER_SPEC, NULL, member_decl, member_spec, 
        //             ASTLine(member_spec), NULL);
        //     new_member_spec = ASTMake3(AST_MEMBER_SPEC, ASTSon0(member_spec),
        //             NULL, aux_member_spec, ASTLine(aux_member_spec), NULL);
        //     ASTSon0(member_spec) = NULL;
        // }
        // else
        // {
        //     new_member_spec = ASTMake3(AST_MEMBER_SPEC, NULL, member_decl, member_spec,
        //             ASTLine(member_spec), NULL);
        // }

        // ASTParent(new_member_spec) = orig_parent;

        // if (orig_parent != NULL)
        // {
        //     for (int i = 0; i < ASTNumChildren(orig_parent); i++)
        //     {
        //         if (ASTChild(orig_parent, i) == member_spec)
        //         {
        //             ASTChild(orig_parent, i) = new_member_spec;
        //         }
        //     }
        // }
    }

    void AST_t::append(AST_t t)
    {
        if (ASTType(t._ast) != AST_NODE_LIST)
        {
            std::cerr << "The appended tree is not a list. No append performed" << std::endl;
            return;
        }

        AST appended_list = t._ast;

        AST enclosing_list = get_enclosing_list(this->_ast);

        if (enclosing_list == NULL)
        {
            std::cerr << "Cannot found a suitable list to append" << std::endl;
        }

        append_list(enclosing_list, appended_list);
    }

    void AST_t::prepend(AST_t t)
    {
        if (ASTType(t._ast) != AST_NODE_LIST)
        {
            std::cerr << "The prepended tree is not a list. No prepend performed" << std::endl;
            return;
        }

        AST prepended_list = t._ast;

        AST enclosing_list = get_enclosing_list(this->_ast);

        if (enclosing_list == NULL)
        {
            std::cerr << "Cannot found a suitable list to prepend" << std::endl;
        }

        prepend_list(enclosing_list, prepended_list);
    }

    AST_t AST_t::get_enclosing_function_definition(bool jump_templates)
    {
        AST node = _ast;

        while (node != NULL && 
                ASTType(node) != AST_FUNCTION_DEFINITION)
        {
            node = ASTParent(node);
        }

        // Jump over template declarations
        if (jump_templates)
        {
            // Now the node is an AST_FUNCTION_DEFINITION
            while (ASTParent(node) != NULL
                    && ASTType(ASTParent(node)) == AST_TEMPLATE_DECLARATION)
            {
                node = ASTParent(node);
            }
        }

        AST_t result(node);
        return result;
    }

    void AST_t::remove_in_list()
    {
        AST list = this->_ast;
        // Look for the enclosing list
        while (list != NULL &&
                ASTType(list) != AST_NODE_LIST)
        {
            list = ASTParent(list);
        }

        if (list == NULL)
        {
            std::cerr << "A suitable list has not been found" << std::endl;
            return;
        }

        AST parent = ASTParent(list);
        AST next = ASTSon0(list);

        if (next != NULL)
        {
            ASTParent(next) = parent;
        }

        int i;
        for (i = 0; i < ASTNumChildren(parent); i++)
        {
            if (ASTChild(parent, i) == list)
            {
                ASTChild(parent, i) = next;
                break;
            }
        }
    }

    AST AST_t::get_enclosing_list(AST ast)
    {
        AST list = ast;

        // Look for the enclosing list
        while (list != NULL &&
                ASTType(list) != AST_NODE_LIST)
        {
            list = ASTParent(list);
        }

        return list;
    }

    void AST_t::replace_in_list(AST_t ast)
    {
        if (ASTType(ast._ast) != AST_NODE_LIST)
        {
            std::cerr << "The replacement tree is not a list. No replacement performed" << std::endl;
            return;
        }

        AST list = get_enclosing_list(this->_ast);

        if (list == NULL)
        {
            std::cerr << "A suitable list has not been found" << std::endl;
            return;
        }

        AST previous = ASTSon0(list);

        AST_t replaced(list);
        replaced.replace_with(ast);

        while (ASTSon0(list) != NULL)
        {
            list = ASTSon0(list);
        }

        ASTSon0(list) = previous;
        if (previous != NULL)
        {
            ASTParent(previous) = list;
        }
    }

    void AST_t::prepend_sibling_function(AST_t t)
    {
        AST_t enclosing_function = this->get_enclosing_function_definition(/*jump_templates*/true);

        AST list = ASTParent(enclosing_function._ast);
        AST prepended_list = get_list_of_extensible_block(t._ast);

        prepend_list(list, prepended_list);
    }

    void AST_t::append_sibling_function(AST_t t)
    {
    }

    void AST_t::replace_text(const std::string& str)
    {
        ASTText(this->_ast) = strdup(str.c_str());
    }

    int AST_t::get_line() const
    {
        if (this->_ast == NULL)
        {
            return 0;
        }
        else
        {
            return ASTLine(this->_ast);
        }
    }

    std::string AST_t::get_file() const
    {
        if (this->_ast == NULL)
        {
            return "(invalid node)";
        }
        else
        {
            return this->_ast->filename;
        }
    }

    std::string AST_t::get_locus() const
    {
        std::stringstream ss;
        
        ss << this->get_file() << ":" << this->get_line();

        return ss.str();
    }
}
