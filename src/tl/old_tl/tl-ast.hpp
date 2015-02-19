/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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




#ifndef TL_AST_HPP
#define TL_AST_HPP

#include "tl-common.hpp"
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
    
    //! \addtogroup Wrap Wrapping classes
    //! @{

    class AST_t;

    //! Class that wraps an AST node of type AST_NODE_LIST
    class LIBTL_CLASS ASTIterator
    {
        private:
            //! The wrapped list AST
            AST _ast;
            //! The current iterator
            AST _current;
            //! Private constructor only accessible from TL::AST_t
            ASTIterator(AST ast);
        public:
            //! Destructor of an ASTIterator
            ~ASTIterator() { }

            //! Get the current tree in the list
            AST_t item();

            //! Move the iterator forward in the list
            void next();

            //! Move the iterator backwards in the list
            void previous();

            //! Restart the iterator in the position of the list
            // where the wrapper was created.
            void reset();

            //! Rewind the iterator to the beginning of the whole list.
            /*!
             * Use this function before traversing a list
             */
            void rewind();

            //! Returns the tree that holds all the list
            AST_t get_parent_of_list();

            //! States whether this is the first element in the list
            bool is_first();
            //! States whether this is the last element in the list
            bool is_last();
            //! States whether we have gone beyond the last element
            // in the list
            bool end();

            //! Only AST_t can create an ASTIterator
            friend class AST_t;
    };

    //! Class used for traversals along the tree
    /*! This class contains the value of the match
     * of a given tree */
    struct LIBTL_CLASS ASTTraversalMatching
    {
        enum TraversalMatching
        {
            INVALID = 0,
            //! This tree matches any of the predicates of the traversal
            NODE_DOES_MATCH,
            //! This tree does not match any of the predicates of the traversal
            NODE_DOES_NOT_MATCH,
        } __value;

        ASTTraversalMatching(TraversalMatching crit)
            : __value(crit) { }
    };

    //! Class used for traversals along the tree
    /*! This class contains the value of the recursivity
     * behaviour of a givene tree */
    struct LIBTL_CLASS ASTTraversalRecursion
    {
        enum TraversalRecursion
        {
            INVALID = 0,
            //! In this tree, recursively traverse its children
            DO_RECURSE,
            //! In this tree, do not recurse its children
            DO_NOT_RECURSE,
        } __value;

        ASTTraversalRecursion(TraversalRecursion rec)
            : __value(rec) { }
    };

    //! Class used for traversals along the tree
    /*! This class represents the result of a traversal
     * functor. It contains both a matching result and a recursivity
     * behaviour values.
     */
    struct LIBTL_CLASS ASTTraversalResult
    {
        private:
            ASTTraversalMatching _match;
            ASTTraversalRecursion _rec;
        public:
            ASTTraversalResult(ASTTraversalMatching match, ASTTraversalRecursion rec)
                : _match(match), _rec(rec)
            {
            }

            bool recurse() const
            {
                return (_rec.__value == ASTTraversalRecursion::DO_RECURSE);
            }

            bool matches() const
            {
                return (_match.__value == ASTTraversalMatching::NODE_DOES_MATCH);
            }
    };

    //! Helper function to build values of ASTTraversalResult given the match and the recurse
    // values
    /*!
     * \param match The matching value. A true value means NODE_DOES_MATCH. A false value means NODE_DOES_NOT_MATCH
     * \param recurse The recursivity behaviour. A true value means DO_RECURSE. A false value means DO_NOT_RECURSE
     */
    LIBTL_EXTERN ASTTraversalResult ast_traversal_result_helper(bool match, bool recurse);

    //! Functor used when traversing trees
    /*! This functor specifies a AST_t as a parameter and a ASTTraversalResult as result.
     */
    typedef Functor<ASTTraversalResult, AST_t> TraverseASTFunctor;

    //! Class that wraps AST trees in the compiler
    class LIBTL_CLASS AST_t : public Object
    {
        public:
            /*! \deprecated Do not use. Instead use TraverseASTFunctor */
            enum RecursiveFlag
            {
                //! All nodes will be visited, regardless the boolean result
                //! of the predicate
                RECURSIVE = 0, 
                //! Only those nodes that do not yield a true result after the
                //! functor, will have their children visited
                NON_RECURSIVE, 
                //! Only those nodes that yield a true result after the functor,
                //! will have their children visited
                LIST_TRIP     
            };
        protected:
            //! Wrapped tree
            AST _ast;

            //! Helper function for traversing the tree
            static void tree_iterator(const AST_t& a, const TraverseASTFunctor& functor, 
                    ObjectList<AST_t>& result);

            //! Implements the access to extended attributes of an AST_t
            tl_type_t* get_extended_attribute(const std::string& name) const;
            //! Implements the access to extended attributes of an AST_t
            virtual bool set_extended_attribute(const std::string&, const tl_type_t &data);

            //! Given a tree it moves towards the root until it reached the translation unit node
            static AST get_translation_unit(AST node);
            //! Given a list, prepend another before it
            static void prepend_list(AST orig_list, AST prepended_list);
            //! Given a list, append another after it
            static void append_list(AST orig_list, AST appended_list);
            //! Function that relinks properly a child with its parent when replacing it
            static void relink_parent(AST previous_child, AST new_child);

            //! States if given tree is either a compound statement, a class specifier, a translation unit
            //! or a namespace definition
            static bool is_extensible_block(AST node);
            //! Given a tree of extensible nature, returns the list that can be extended
            static AST get_list_of_extensible_block(AST node);

            //! Returns the innermost enclosing list of a given tree
            static AST get_enclosing_list(AST node);

            //! Returns the innermost tree at the global scope starting from \a tree
            static AST_t get_enclosing_global_tree_(AST_t tree);

            //! Given a member specificacion of a class definition, append a member declaration to it
            static void append_to_member_spec(AST member_spec, AST member_decl);
            //! Given a member specificacion of a class definition, prepend a member declaration to it
            static void prepend_to_member_spec(AST member_spec, AST member_decl);

        public:
            //! Constructor that wraps an invalid or empty tree
            AST_t()
                : _ast(NULL)
            {
            }

            //! Constructor to wrap AST trees
            AST_t(AST _wrapped_tree) 
                : _ast(_wrapped_tree)
            {
            }

            explicit AST_t(RefPtr<Object> obj) 
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

            //! Copy constructor
            AST_t(const AST_t& ast)
                : Object(ast), _ast(ast._ast)
            {
            }

            /*
             * Destructor
             */
            virtual ~AST_t()
            {
            }

            //! States if the tree is valid (or non empty)
            bool is_valid() const
            {
                return (_ast != NULL);
            }

            //! States whether the tree is a list
            bool is_list() const;
            //! Returns an ASTIterator of this list
            ASTIterator get_list_iterator() const;

            //! States whether the node is in a list
            bool is_in_a_list() const;
            //! Returns an iterator to the enclosing list
            ASTIterator get_enclosing_list() const;

            bool operator<(AST_t n) const;
            bool operator==(AST_t n) const;
            bool operator!=(AST_t n) const;
            AST_t& operator=(AST_t n);

            //! Prettyprints the tree
            /*! 
             * \param with_commas Means that this list should be printed with commas, 
             * otherwise it will be printed as a sequence with only interspersing blanks
             * 
             * Note that this function can print internal marks not recognized by any
             * other compiler, so if you want to feed an external tool use prettyprint_external
             */
            std::string prettyprint(bool with_commas = false) const;

            //! Prettyprints the tree for an external tool 
            /*!
              * In contrast to what prettyprint does, this function disables internal
              * prettyprinting, so special marks used to express special nodes are disabled
              */
            std::string prettyprint_external() const;

            //! Prettyprints a tree in a given CompiledFile. 
            /*!
             * \param compiled_file The file where this tree will be prettyprinted on
             * \param internal Means that some special trees will not be
             * expanded to their real representation. This affects to internal
             * generated comments and preprocessor lines.
             */
            void prettyprint_in_file(const CompiledFile& compiled_file,  bool internal = false) const;

            //! Prettyprints a tree but calling a callback per each AST node prettyprinted
            /*! Functor will be called with every AST being printed, thus you
             * can change on-the-fly prettyprinting. This is a simple minded
             * replace strategy on output.
             */
            typedef std::pair<bool, std::string> callback_result;
            std::string prettyprint_with_callback(const Functor<callback_result, AST_t> &functor);

            //! Replaces current tree in a smart way
            /*
             * \param ast The tree used to replace the current one 
             *
             * This function takes care of replacements performed
             * within lists. Lists replaced within lists will be inlined
             * to the enclosing list, instead of creating a new nested 
             * list item.
             */
            void replace(AST_t ast);

            //! This function should not be used directly, instead use replace
            /*
             * \param ast The tree used to replace the current one 
             *
             * This function directly replaces current tree without considering
             * whether it is a list or not
             */
            void replace_with(AST_t ast);

            //! This function should not be used directly, instead use replace(AST_t)
            /*
             * \param ast The tree used to replace the current one 
             *
             * This function expects current tree to be a list.
             */
            void replace_in_list(AST_t ast);

            //! This function is used to remove an element from the innermost enclosing list
            /*
             * This function cannot be used in trees that are not actually
             * enclosed in anything (though in practice most are).
             */
            void remove_in_list();

            //! Duplicates a tree.
            /*!
             * This function also copies the extended information of the tree.
             */
            AST_t duplicate() const;

            //! Duplicates a tree and makes it to be in the same context as the original one
            /*!
             * This function also copies the extended information of the tree.
             *
             * \param scope_link The scope link considered
             * \return A pair with the duplicated tree and an updated scope link
             */
            std::pair<AST_t, ScopeLink> duplicate_with_scope(ScopeLink scope_link) const;

            /*!
             * \deprecated Do not use, instead use depth_subtrees(const TraverseASTFunctor&)
             * \param pred A predicate receiving an AST_t
             * \param recursive_flag Specifies how the recursive traverse is performed depending on the result of pred
             * \return A list of visited trees that matched the predicate
             */
            ObjectList<AST_t> depth_subtrees(const Predicate<AST_t>& pred = AlwaysTrue<AST_t>(), RecursiveFlag recursive_flag = RECURSIVE) const;

            //! Returns a list of visited trees that matched the traverse functor
            /*!
             * \param functor A traverse functor specifying the visited nodes and whose ones match
             * \return A list of visited trees that matched the predicate
             */
            ObjectList<AST_t> depth_subtrees(const TraverseASTFunctor& functor) const;

            //! Returns a list of children
            /*!
             * \return A list containing the children of this node. A children
             * in the list might be invalid if the tree does not have such
             * children.
             */
            ObjectList<AST_t> children() const;

            //! Debug function - Do not use 
            /*! Returns a string with the name of the internal ast kind */
            std::string internal_ast_type() const;
            //! Debug function - Do not use 
            /*! Returns the internal ast kind */
            node_t internal_ast_type_() const;

            //! Debug function - Do not use 
            /*! Returns the wrapped tree */
            AST get_internal_ast() const
            {
                return _ast;
            }

            //! Debug function - Do not use
            /*! Returns the address of the tree field */
            AST* get_internal_ast_field_ptr()
            {
                return &(this->_ast);
            }

            /* End debug functions */

            //! Returns the translation unit tree
            AST_t get_translation_unit() const;

            //! Prepends a tree as a sibling of the current enclosing function
            void prepend_sibling_function(AST_t t);
            //! Appends a tree as a sibling of the current enclosing function
            /*!
             * \bug Not yet implemented
             */
            void append_sibling_function(AST_t t);

            //! Prepends a tree in the previous global point
            void prepend_sibling_global(AST_t t);

            //! Adds tree to the very beginning of all declarations of current translation unit
            void prepend_to_translation_unit(AST_t t);
            //! Adds tree to the very end of all declarations of current translation unit
            void append_to_translation_unit(AST_t t);

            //! Appends an item to the current list
            /*!
             * Current tree must be a list.
             * \param t The appended tree
             */
            void append(AST_t t);
            //! Prepends an item to the current list
            /*!
             * Current tree must be a list.
             * \param t The prepended tree
             */
            void prepend(AST_t t);

            //! Returns the innermost enclosing block (compound statement) of this tree
            AST_t get_enclosing_block() const;

            //! Returns the enclosing function definition tree
            /*!
             * \param jump_templates Tells whether template headers of this function definition must be skipped too
             */
            AST_t get_enclosing_function_definition(bool jump_templates = false, bool jump_external_decl = false) const;

            //! Returns the point where the function definition begins its whole declaration
            /*!
             * This is equivalent to call
             * @code
             * a.get_enclosing_function_definition(true, true);
             * @endcode
             */
            AST_t get_enclosing_function_definition_declaration() const;

            //! Returns the enclosing class specifier
            /*!
             * \param jump_templates Tells whether template headers of this class specifier must be skipped too
             */
            AST_t get_enclosing_class_specifier(bool jump_templates = false) const;

            //! Returns the enclosing namespace definition
            AST_t get_enclosing_namespace_definition() const;

            //! Returns the enclosing statement
            AST_t get_enclosing_statement() const;

            //! Returns a reference tree that is in global scope
            /*! 
              This is the same tree used by prepend_sibling_global to prepend
              an element
              */
            AST_t get_enclosing_global_tree() const;

            //! States whether this tree has a related text
            bool has_text() const;

            //! Returns the text of this tree
            std::string get_text() const;
            
            //! Replaces the text item of the tree.
            void replace_text(const std::string& str);

            //! Gets the line of this tree
            unsigned int get_line() const;
            //! Gets the file of this tree
            std::string get_file() const;
            //! Gets a string of the form "file:line" with the file and line of this tree
            std::string get_locus() const;

            //! Returns the parent tree
            AST_t get_parent() const;

            //! Returns the tree name with this link
            AST_t get_link_to_child(const std::string& str) const;

            friend class Type;
            friend class Scope;
            friend class ScopeLink;
            friend class DepthTraverse;
            friend class Source;
            // mmm
            friend class Expression;
    };

    LIBTL_EXTERN std::ostream& operator<< (std::ostream& o, const AST_t& a);
    
    //! @}
   
    //! \addtogroup Traverse
    //! @{

    //! Wrap class for deprecated Predicate<AST_t> traversals 
    class LIBTL_CLASS TraverseASTPredicate : public TraverseASTFunctor
    {
        private:
            //! The predicate
            const Predicate<AST_t>& _pred;
            //! The specified recursive flag
            AST_t::RecursiveFlag _rec;
        public:
            //! Constructor given a Predicate<AST_t> and a RecursiveFlags
            TraverseASTPredicate(const Predicate<AST_t>& pred, AST_t::RecursiveFlag rec = AST_t::RECURSIVE)
                : _pred(pred), _rec(rec)
            {
            }

            //! Function imiting the behaviour of deprecated traversals with Predicate<AST_t>
            virtual ASTTraversalResult do_(ArgType node) const
            {
                bool matches = _pred(node);
                bool recurse = false;

                if (_rec == AST_t::RECURSIVE)
                {
                    recurse = true;
                }
                else if ((_rec == AST_t::NON_RECURSIVE) && !matches)
                {
                    recurse = true;
                }
                else if ((_rec == AST_t::LIST_TRIP && matches))
                {
                    recurse = true;
                }

                return ast_traversal_result_helper(matches, recurse);
            }
    };
    
    //! @}

    //! \addtogroup Functors
    //! @{
    
    //! Convenience template class for predicates after a given AST attribute
#ifndef _WIN32
    template<const char* _ATTR>
    class PredicateAST : public Predicate<AST_t>
    {
        public:
            virtual bool do_(AST_t& ast) const
            {
                if (!ast.is_valid())
                    return false;
                
                RefPtr<Object> obj = ast.get_attribute(_ATTR);
                if(typeid(*(obj.get_pointer())) == typeid(Undefined))
                {
                    return false;
                }
                else
                {
                    return true;                  
                }
            }

            DEPRECATED PredicateAST()
                : Predicate<AST_t>()
            {
            }

            virtual ~PredicateAST() { }
    };
#endif

    //! Convenience class for matching nodes after an attribute.
    /*!
     * This class is similar to PredicateAST but here the
     * requested attribute can be defined in runtime
     */
    class LIBTL_CLASS PredicateAttr : public Predicate<AST_t>
    {
        private:
            std::string _attr_name;
        public:
            PredicateAttr(const std::string &attr_name)
                : _attr_name(attr_name)
            {
            }

            virtual bool do_(ArgType ast) const
            {
                if (!ast.is_valid())
                    return false;

                RefPtr<Object> obj = ast.get_attribute(_attr_name);
                if(typeid(*(obj.get_pointer())) == typeid(Undefined))
                {
                    return false;
                }
                else
                {
                    return true;
                }
            }
    };

    //! Convenience class for matching nodes that have a given computed type
    class LIBTL_CLASS PredicateType : public Predicate<AST_t>
    {
        private:
            node_t _type;
        public:
            PredicateType(node_t type)
                : _type(type)
                {
                }
            virtual bool do_(ArgType ast) const
            {
                return (ast.internal_ast_type_() == _type);
            }
    };
    
    //! @}
}

#endif // TL_AST_HPP
