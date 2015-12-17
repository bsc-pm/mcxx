/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information
  regarding developers and contributors.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/




#include "tl-builtin.hpp"
#include "tl-ast.hpp"
#include "tl-scopelink.hpp"
#include "tl-predicate.hpp"
#include "cxx-ast.h"
#include "cxx-attrnames.h"
#include "cxx-utils.h"
#include <sstream>
#include <cstdio>
#include <cerrno>
#include <cstring>

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
        return default_get_extended_attribute(
                ast_get_extensible_struct(this->_ast),
                name);
    }

    bool AST_t::set_extended_attribute(const std::string &str, const tl_type_t &data)
    {
        return default_set_extended_attribute(
                ast_get_initalized_extensible_struct(this->_ast),
                str,
                data);
    }

    std::string AST_t::prettyprint(bool with_commas) const
    {
        // This is always internal
        prettyprint_set_internal_output();
        const char *c = NULL;
        if (with_commas && ASTKind(this->_ast) == AST_NODE_LIST)
        {
            c = list_handler_in_buffer(this->_ast);
        }
        else
        {
            c = prettyprint_in_buffer(this->_ast);
        }

        std::string result(c == NULL ? "" : c);
        if (c != NULL)
            DELETE((void*)c);
        return result;
    }

    std::ostream& operator<< (std::ostream& o, const AST_t& a)
    {
        return (o << a.prettyprint());
    }

    std::string AST_t::prettyprint_external() const
    {
        // This is always external
        prettyprint_set_not_internal_output();

        const char* c = prettyprint_in_buffer(this->_ast);
        std::string result(c == NULL ? "" : c);
        if (c != NULL)
            DELETE((void*)c);
        return result;
    }

    void AST_t::prettyprint_in_file(const CompiledFile& compiled_file, bool internal) const
    {
        if (!internal)
        {
            prettyprint_set_not_internal_output();
        }
        else
        {
            prettyprint_set_internal_output();
        }

        FILE *file = fopen(compiled_file.get_filename().c_str(), "a");
        if (file == NULL)
        {
            fatal_error("Could not open output file '%s' (%s)\n", 
                    compiled_file.get_filename().c_str(), 
                    strerror(errno));
        }

        ::prettyprint(file, this->_ast);

        fclose(file);
    }

    void AST_t::replace(AST_t ast)
    {
        if (ast._ast == NULL)
        {
            internal_error("Trying to replace a tree with an empty tree", 0);
        }

        if (this->_ast == NULL)
        {
            internal_error("Trying to replace an empty tree with another tree", 0);
        }

        if (ASTKind(ast._ast) == AST_NODE_LIST)
        {
            // If the replacement is a list but the original is not, let's check two cases
            // maybe this list is a one-element list or not.
            if (ASTKind(this->_ast) != AST_NODE_LIST)
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
                    // std::cerr << "Warning: Replacing a non-list tree at '" 
                    //     << this->get_locus() 
                    //     << "' with a list tree of more than one element" << std::endl;
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
        else if (ASTKind(_ast) == AST_NODE_LIST
                && ASTKind(_ast) != AST_NODE_LIST)
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
        if (ast._ast == NULL)
        {
            internal_error("Trying to replace a tree with an empty tree.", 0);
        }

        if (this->_ast == NULL)
        {
            internal_error("Trying to replace an empty tree with another tree", 0);
        }

        AST previous_parent = ASTParent(this->_ast);
        ast_replace(this->_ast, ast._ast);
        ast_set_parent(this->_ast, previous_parent);

        // Relink sons
        for (int i = 0; i < ASTNumChildren(this->_ast); i++)
        {
            if (ASTChild(this->_ast, i) != NULL)
            {
                ast_set_parent(ASTChild(this->_ast, i), this->_ast);
            }
        }
    }

    AST_t AST_t::duplicate() const
    {
        AST_t result(ast_copy(this->_ast));
        return result;
    }

    std::pair<AST_t, ScopeLink> AST_t::duplicate_with_scope(ScopeLink scope_link) const
    {
        AST duplicated_tree = ast_copy_with_scope_link(this->_ast, scope_link._scope_link);

        AST_t ast(duplicated_tree);
        std::pair<AST_t, ScopeLink> result(ast, scope_link);

        return result;
    }

    void AST_t::tree_iterator(const AST_t& a, const TraverseASTFunctor& functor, 
            ObjectList<AST_t>& result)
    {
        AST tree = a._ast;
        if (tree == NULL)
            return;

        ASTTraversalResult current_node = functor(a);
        if (current_node.matches())
        {
            result.push_back(a);
        }

        if (current_node.recurse())
        {
            for (int i = 0; i < ASTNumChildren(tree); i++)
            {
                if (ASTChild(tree, i) != NULL)
                {
                    AST_t iterate(ASTChild(tree, i));
                    tree_iterator(iterate, functor, result);
                }
            }
        }
    }

    ObjectList<AST_t> AST_t::depth_subtrees(const Predicate<AST_t>& pred, RecursiveFlag recursive_flag) const
    {
        ObjectList<AST_t> result;

        // Construct a functor that emulates old behaviour
        TraverseASTPredicate compat_functor(pred, recursive_flag);

        tree_iterator(*this, compat_functor, result);

        return result;
    }

    ObjectList<AST_t> AST_t::depth_subtrees(const TraverseASTFunctor& functor) const
    {
        ObjectList<AST_t> result;

        tree_iterator(*this, functor, result);

        return result;
    }

    std::string AST_t::internal_ast_type() const
    {
        char* inner_name = ast_node_names[ASTKind(this->_ast)];
        std::string result(inner_name);
        return result;
    }

    node_t AST_t::internal_ast_type_() const
    {
        return ASTKind(this->_ast);
    }

    void AST_t::append_to_translation_unit(AST_t tree)
    {
        if (tree._ast == NULL)
        {
            return;
        }

        AST this_translation_unit = get_translation_unit(this->_ast);

        AST this_declaration_seq = ASTSon0(this_translation_unit);
        AST tree_declaration_seq = tree._ast;

        if (this_declaration_seq == NULL)
        {
            ast_set_child(this_translation_unit, 0, tree_declaration_seq);
        }
        else
        {
            append_list(this_declaration_seq, tree_declaration_seq);
        }
    }

    void AST_t::prepend_to_translation_unit(AST_t tree)
    {
        if (tree._ast == NULL)
        {
            return;
        }

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
            ast_set_child(this_translation_unit, 0, tree_declaration_seq);
        }
        else
        {
            prepend_list(this_declaration_seq, tree_declaration_seq);
        }
    }

    // XXX - Fixme, implement it using ast_list_concat
    void AST_t::prepend_list(AST orig_list, AST prepended_list)
    {
        if (ASTKind(orig_list) != AST_NODE_LIST
                || ASTKind(prepended_list) != AST_NODE_LIST)
        {
            std::cerr << "You tried to prepend two lists that are not " 
                << "orig_list=" << ast_print_node_type(ASTKind(orig_list)) << " "
                << "prepend_list=" << ast_print_node_type(ASTKind(prepended_list)) << std::endl;
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
                    ast_set_child(parent, i, NULL);
                    break;
                }
            }
        }

        // Now make the prepended_list as the son
        AST original_previous = ASTSon0(orig_list);

        ast_set_child(orig_list, 0, prepended_list);

        // Go to the deeper node of prepended_list
        AST iter = prepended_list;
        while (ASTSon0(iter) != NULL)
        {
            iter = ASTSon0(iter);
        }

        ast_set_child(iter, 0, original_previous);
    }

    // XXX - Fixme, implement it using ast_list_concat
    void AST_t::append_list(AST orig_list, AST appended_list)
    {
        if (ASTKind(orig_list) != AST_NODE_LIST
                || ASTKind(appended_list) != AST_NODE_LIST)
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

        ast_set_child(iter, 0, orig_list);
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
            ast_set_child(parent, j, NULL);
        }

        // Relink
        ast_set_child(parent, i, new_child);
    }

    // AST AST_t::get_translation_unit()
    // {
    //     return this->get_translation_unit(this->_ast);
    // }

    AST_t AST_t::get_translation_unit() const
    {
        return get_translation_unit(this->_ast);
    }

    AST AST_t::get_translation_unit(AST node)
    {
        if  (node == NULL)
            return NULL;

        while (node != NULL && 
                ASTKind(node) != AST_TRANSLATION_UNIT)
        {
            node = ASTParent(node);
        }

        return node;
    }

    AST_t AST_t::get_enclosing_block() const
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
        return (ASTKind(node) == AST_COMPOUND_STATEMENT
                || ASTKind(node) == AST_CLASS_SPECIFIER
                || ASTKind(node) == AST_TRANSLATION_UNIT
                || ASTKind(node) == AST_NAMESPACE_DEFINITION);
    }

    AST AST_t::get_list_of_extensible_block(AST node)
    {
        switch ((int)ASTKind(node))
        {
            case AST_COMPOUND_STATEMENT :
                {
                    // This can be null
                    return ASTSon0(node);
                    break;
                }
            case AST_CLASS_SPECIFIER :
                {
                    // This can be null
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
        }
        return NULL;
    }


    void AST_t::append_to_member_spec(AST, AST)
    {
    }

    void AST_t::prepend_to_member_spec(AST, AST)
    {
    }

    void AST_t::append(AST_t t)
    {
        if (t._ast == NULL)
        {
            // Do nothing
            return;
        }

        if (ASTKind(t._ast) != AST_NODE_LIST)
        {
            std::cerr << "The appended tree is not a list. No append performed" << std::endl;
            return;
        }

        AST appended_list = t._ast;

        AST enclosing_list = get_enclosing_list(this->_ast);

        if (enclosing_list == NULL)
        {
            std::cerr << "Cannot find a suitable list to append" << std::endl;
        }

        append_list(enclosing_list, appended_list);
    }

    void AST_t::prepend(AST_t t)
    {
        if (t._ast == NULL)
        {
            // Do nothing
            return;
        }

        if (ASTKind(t._ast) != AST_NODE_LIST)
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

    AST_t AST_t::get_enclosing_function_definition_declaration() const
    {
        return get_enclosing_function_definition(/* jump_templates = */ true, 
                /* jump_external_decl = */ true);
    }

    AST_t AST_t::get_enclosing_class_specifier(bool jump_templates) const
    {
        AST node = _ast;

        while (node != NULL
                && ASTKind(node) != AST_CLASS_SPECIFIER)
        {
            node = ASTParent(node);
        }

        while (node != NULL
                && ASTKind(node) != AST_SIMPLE_DECLARATION)
        {
            node = ASTParent(node);
        }

        if (jump_templates)
        {
            while (node != NULL
                    && ASTParent(node) != NULL
                    && ASTKind(ASTParent(node)) == AST_TEMPLATE_DECLARATION)
            {
                node = ASTParent(node);
            }
        }

        AST_t result(node);
        return node;
    }

    AST_t AST_t::get_enclosing_namespace_definition() const
    {
        AST node = _ast;

        while (node != NULL
                && ASTKind(node) != AST_NAMESPACE_DEFINITION
                && ASTKind(node) != AST_GCC_NAMESPACE_DEFINITION)
        {
            node = ASTParent(node);
        }

        AST_t result(node);
        return node;
    }

    //! Returns the enclosing statement
    AST_t AST_t::get_enclosing_statement() const
    {
        AST_t a = *this;
        while (a.is_valid()
                && !(TL::Bool)a.get_attribute(LANG_IS_STATEMENT))
        {
            a = a.get_parent();
        }

        return a;
    }

    AST_t AST_t::get_enclosing_function_definition(bool jump_templates, bool jump_external_decl) const
    {
        AST node = _ast;

        while (node != NULL && 
                ASTKind(node) != AST_FUNCTION_DEFINITION)
        {
            node = ASTParent(node);
        }

        // Jump over template declarations
        if (jump_templates)
        {
            // Now the node is an AST_FUNCTION_DEFINITION
            while (node != NULL 
                    && ASTParent(node) != NULL
                    && ASTKind(ASTParent(node)) == AST_TEMPLATE_DECLARATION)
            {
                node = ASTParent(node);
            }
        }

        // Extern declarations pose a problem, let's jump them as well
        if (jump_external_decl)
        {
            while (node != NULL
                    && ASTParent(node) != NULL
                    && ASTKind(ASTParent(node)) == AST_LINKAGE_SPEC_DECL)
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
                ASTKind(list) != AST_NODE_LIST)
        {
            list = ASTParent(list);
        }

        if (list == NULL)
        {
            std::cerr << "A suitable list has not been found" << std::endl;
            return;
        }

        AST parent = ASTParent(list);
        AST previous = ASTSon0(list);

        if (previous != NULL)
        {
            ast_set_parent(previous, parent);
        }

        int i;
        for (i = 0; i < ASTNumChildren(parent); i++)
        {
            if (ASTChild(parent, i) == list)
            {
                ast_set_child(parent, i, previous);
                break;
            }
        }
    }

    AST AST_t::get_enclosing_list(AST ast)
    {
        if (ast == NULL)
        {
            return NULL;
        }

        AST list = ast;

        // Look for the enclosing list
        while (list != NULL &&
                ASTKind(list) != AST_NODE_LIST)
        {
            list = ASTParent(list);
        }

        return list;
    }

    void AST_t::replace_in_list(AST_t ast)
    {
        if (ast._ast == NULL)
        {
            std::cerr << "Cannot replace a list using an empty tree" << std::endl;
            return;
        }
        if (this->_ast == NULL)
        {
            std::cerr << "This tree is empty" << std::endl;
            return;
        }

        if (ASTKind(ast._ast) != AST_NODE_LIST)
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

        ast_set_child(list, 0, previous);
    }

    void AST_t::prepend_sibling_function(AST_t t)
    {
        // Do nothing
        if (t._ast == NULL)
        {
            return;
        }

        AST_t enclosing_function = this->
            get_enclosing_function_definition(/*jump_templates*/true, 
                    /*jump_external_decl*/true);

        AST list = ASTParent(enclosing_function._ast);
        AST prepended_list = get_list_of_extensible_block(t._ast);

        prepend_list(list, prepended_list);
    }

    void AST_t::append_sibling_function(AST_t)
    {
    }

    AST_t AST_t::get_enclosing_global_tree_(AST_t t)
    {
        AST_t enclosing_global_tree = t;
        AST_t enclosing_function = enclosing_global_tree
            .get_enclosing_function_definition(/*jump_templates*/true, 
                    /*jump_external_decl*/true);

        if (enclosing_function.is_valid())
        {
            enclosing_global_tree = enclosing_function.get_parent();
        }

        AST_t enclosing_class = enclosing_global_tree.get_enclosing_class_specifier(/*jump_templates*/ true);
        while (enclosing_class.is_valid())
        {
            enclosing_global_tree = enclosing_class.get_parent();
            enclosing_class = 
                enclosing_global_tree.get_enclosing_class_specifier(/*jump_templates*/ true);
        }

        AST_t enclosing_namespace = enclosing_global_tree.get_enclosing_namespace_definition();
        while (enclosing_namespace.is_valid())
        {
            enclosing_global_tree = enclosing_namespace.get_parent();
            enclosing_namespace = 
                enclosing_global_tree.get_enclosing_namespace_definition();
        }

        return enclosing_global_tree;
    }

    AST_t AST_t::get_enclosing_global_tree() const
    {
        return get_enclosing_global_tree_(*this);
    }

    void AST_t::prepend_sibling_global(AST_t t)
    {
        if (t._ast == NULL)
        {
            return;
        }

        AST_t enclosing_global_tree = get_enclosing_global_tree_(this->_ast);

        AST list = get_enclosing_list(enclosing_global_tree._ast);
        AST prepended_list = get_list_of_extensible_block(t._ast);

        prepend_list(list, prepended_list);
    }

    void AST_t::replace_text(const std::string& str)
    {
        if (this->_ast == NULL)
            return;

        ast_set_text(this->_ast, str.c_str());
    }

    unsigned int AST_t::get_line() const
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
            const char * c = ast_get_filename(this->_ast);
            if (c == NULL)
            {
                return "(unknown file)";
            }
            else
            {
                return c;
            }
        }
    }

    std::string AST_t::get_locus() const
    {
        std::stringstream ss;
        
        ss << this->get_file() << ":" << this->get_line();

        return ss.str();
    }

    bool AST_t::is_list() const
    {
        return is_valid()
            && ASTKind(_ast) == AST_NODE_LIST;
    }

    ASTIterator AST_t::get_list_iterator() const
    {
        ASTIterator iterator(_ast);
        return _ast;
    }

    ASTIterator::ASTIterator(AST ast)
        : _ast(ast), _current(ast)
    {
        if (ast == NULL 
                || ASTKind(ast) != AST_NODE_LIST)
        {
            if (ast != NULL)
            {
                std::cerr << "Node at '" 
                    << ast_get_filename(ast) << ":" << ASTLine(ast) 
                    << "' is not a list" << std::endl;
            }
            _ast = NULL;
            _current = NULL;
        }
    }

    AST_t ASTIterator::item()
    {
        if (_current == NULL)
        {
            return AST();
        }
        else
        {
            return AST(ASTSon1(_current));
        }
    }

    void ASTIterator::next()
    {
        if (!is_last())
        {
            _current = ASTParent(_current);
        }
        else
        {
            _current = NULL;
        }
    }

    void ASTIterator::previous()
    {
        if (_current != NULL)
        {
            _current = ASTSon0(_current);
        }
    }

    void ASTIterator::reset()
    {
        _current = _ast;
    }

    void ASTIterator::rewind()
    {
        reset();
        while (!is_first())
        {
            previous();
        }
    }

    bool ASTIterator::is_first()
    {
        if (_current == NULL)
        {
            return true;
        }

        return (ASTSon0(_current) == NULL);
    }

    bool ASTIterator::is_last()
    {
        if (_current == NULL)
            return true;

        AST _possible_next = ASTParent(_current);
        if (_possible_next == NULL
                || ASTKind(_possible_next) != AST_NODE_LIST)
        {
            return true;
        }
        else if (ASTKind(_possible_next) == AST_NODE_LIST
                && ASTSon0(_possible_next) != _current)
        {
            return true;
        }
        return false;
    }

    bool ASTIterator::end()
    {
        return (_current == NULL);
    }

    AST_t ASTIterator::get_parent_of_list()
    {
        // This is similar to a rewind, actually, but without modifying
        // _current
        AST it = _ast;

        while (it != NULL
                && ASTKind(it) == AST_NODE_LIST)
        {
            it = ASTParent(it);
        }

        return AST_t(it);
    }

    ASTTraversalResult ast_traversal_result_helper(bool matches, bool recurse)
    {
        if (matches && recurse)
        {
            return ASTTraversalResult(ASTTraversalMatching::NODE_DOES_MATCH, 
                    ASTTraversalRecursion::DO_RECURSE);
        }
        else if (!matches && recurse)
        {
            return ASTTraversalResult(ASTTraversalMatching::NODE_DOES_NOT_MATCH, 
                    ASTTraversalRecursion::DO_RECURSE);
        }
        else if (matches && !recurse)
        {
            return ASTTraversalResult(ASTTraversalMatching::NODE_DOES_MATCH, 
                    ASTTraversalRecursion::DO_NOT_RECURSE);
        }
        else /* if (!matches && !recurse) */
        {
            return ASTTraversalResult(ASTTraversalMatching::NODE_DOES_NOT_MATCH, 
                    ASTTraversalRecursion::DO_NOT_RECURSE);
        }

    }

    bool AST_t::has_text() const
    {
        return ASTText(_ast) != NULL;
    }

    std::string AST_t::get_text() const
    {
        return ASTText(_ast);
    }

    AST_t AST_t::get_parent() const
    {
        return ASTParent(this->_ast);
    }

    bool AST_t::is_in_a_list() const
    {
        AST_t parent = get_parent();

        return (parent.is_list());
    }

    ASTIterator AST_t::get_enclosing_list() const
    {
        ASTIterator ast_iterator(get_parent()._ast);
        return ast_iterator;
    }

    ObjectList<AST_t> AST_t::children() const
    {
        ObjectList<AST_t> result;

        result.append(AST_t(ASTSon0(_ast)));
        result.append(AST_t(ASTSon1(_ast)));
        result.append(AST_t(ASTSon2(_ast)));
        result.append(AST_t(ASTSon3(_ast)));

        return result;
    }

    static const char * auxiliar_handler_prettprint(AST a, void *data)
    {
        Functor<AST_t::callback_result, AST_t> *functor 
            = reinterpret_cast<Functor<AST_t::callback_result, AST_t>*>(data);

        AST_t wrapped_tree(a);
        AST_t::callback_result result = (*functor)(wrapped_tree);

        if (result.first)
        {
            return strdup(result.second.c_str());
        }
        else
        {
            return NULL;
        }
    }

    std::string AST_t::prettyprint_with_callback(const Functor<callback_result, AST_t> &functor)
    {
        // This const cast is fine
        const char *c = prettyprint_in_buffer_callback(_ast,
                auxiliar_handler_prettprint, const_cast<Functor<callback_result, AST_t>*>(&functor));

        std::string result(c);
        DELETE((void*)c);
        return result;
    } 

    AST_t AST_t::get_link_to_child(const std::string& str) const
    {
        return AST_t(ast_get_link_to_child(_ast, str.c_str()));
    }
}
