/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#ifndef TL_AST_HPP
#define TL_AST_HPP

#include <typeinfo>
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <utility>
#include "cxx-ast.h"
#include "cxx-prettyprint.h"
#include "tl-compilerpipeline.hpp"
#include "tl-object.hpp"
#include "tl-objectlist.hpp"
#include "tl-predicate.hpp"
#include "tl-predicateutils.hpp"

namespace TL
{
    class ScopeLink;

    class AST_t;
    class ASTIterator
    {
        private:
            AST _ast;
            AST _current;
            ASTIterator(AST ast);
        public:
            ~ASTIterator() { }

            AST_t item();
            void next();
            void previous();
            void reset();
            void rewind();
            bool is_first();
            bool is_last();
            bool end();

            friend class AST_t;
    };

    class AST_t : public Object
    {
        public:
            enum RecursiveFlag
            {
                RECURSIVE = 0, // All nodes are visited
                NON_RECURSIVE, // Only non-matching nodes have their sons visited
                LIST_TRIP      // Only matching nodes have their sons visited
            };
        protected:
            AST _ast;

            static void tree_iterator(AST_t& a, const Predicate<AST_t>& predicate, 
                    RecursiveFlag recursive_flag, ObjectList<AST_t>& result);

            tl_type_t* get_extended_attribute(const std::string& name) const;

            static AST get_translation_unit(AST node);
            static void prepend_list(AST orig_list, AST prepended_list);
            static void append_list(AST orig_list, AST appended_list);
            static void relink_parent(AST previous_child, AST new_child);

            static bool is_extensible_block(AST node);
            static AST get_list_of_extensible_block(AST node);

            static AST get_enclosing_list(AST node);

            static void append_to_member_spec(AST member_spec, AST member_decl);
            static void prepend_to_member_spec(AST member_spec, AST member_decl);

        public:
            /*
             * Constructor
             */
            AST_t()
                : _ast(NULL)
            {
            }

            AST_t(AST _wrapped_tree)
                : _ast(_wrapped_tree)
            {
            }

            AST_t(RefPtr<Object> obj)
            {
                RefPtr<AST_t> cast = RefPtr<AST_t>::cast_dynamic(obj);

                if (cast.get_pointer() == NULL)
                {
                    if (typeid(*obj.get_pointer()) != typeid(Undefined))
                    {
                        std::cerr << "Bad initialization of AST_t" << std::endl;
                    }

                    this->_ast = NULL;
                }
                else
                {
                    this->_ast = cast->_ast;
                }
            }

            AST_t(const AST_t& ast)
                : _ast(ast._ast)
            {
            }


            /*
             * Destructor
             */
            virtual ~AST_t()
            {
            }

            bool is_valid() const
            {
                return (_ast != NULL);
            }

            bool is_list() const;
            ASTIterator get_list_iterator();

            bool operator<(AST_t n) const;
            bool operator==(AST_t n) const;
            bool operator!=(AST_t n) const;
            AST_t& operator=(AST_t n);

            std::string prettyprint(bool with_commas = false) const;
            void prettyprint_in_file(const CompiledFile& compiled_file,  bool internal = false) const;

            void replace(AST_t ast);

            void replace_with(AST_t ast);
            void replace_in_list(AST_t ast);

            void remove_in_list();

            AST_t duplicate() const;

            std::pair<AST_t, ScopeLink> duplicate_with_scope(ScopeLink scope_link) const;

            ObjectList<AST_t> depth_subtrees(const Predicate<AST_t>& pred = AlwaysTrue<AST_t>(), RecursiveFlag recursive_flag = RECURSIVE);

            std::string internal_ast_type() const;

            virtual bool is_ast() const
            {
                return true;
            }

            void prepend_sibling_function(AST_t t);
            void append_sibling_function(AST_t t);

            void prepend_to_translation_unit(AST_t t);
            void append_to_translation_unit(AST_t t);

            void append(AST_t t);
            void prepend(AST_t t);

            AST_t get_enclosing_block();
            AST_t get_enclosing_function_definition(bool jump_templates = false);

            // AST get_translation_unit();

            void replace_text(const std::string& str);

            int get_line() const;
            std::string get_file() const;
            std::string get_locus() const;

            friend class Type;
            friend class Scope;
            friend class ScopeLink;
            friend class DepthTraverse;
            friend class Source;
            // mmm
            friend class Expression;
    };

    template<const char* _ATTR>
    class PredicateBool : public Predicate<AST_t>
    {
        public:
            virtual bool operator()(AST_t& ast) const
            {
                TL::Bool attr = ast.get_attribute(_ATTR);
                return attr;
            }
            virtual ~PredicateBool() { }
    };

    class PredicateAttr : public Predicate<AST_t>
    {
        private:
            std::string _attr_name;
        public:
            PredicateAttr(const std::string &attr_name)
                : _attr_name(attr_name)
            {
            }

            virtual bool operator()(AST_t& ast) const
            {
                return TL::Bool(ast.get_attribute(_attr_name));
            }
    };

}

#endif // TL_AST_HPP
