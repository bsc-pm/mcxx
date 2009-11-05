/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#ifndef TL_SOURCE_T_HPP
#define TL_SOURCE_T_HPP

#include "tl-common.hpp"
#include <string>
#include "tl-object.hpp"
#include "tl-ast.hpp"
#include "tl-scope.hpp"
#include "tl-scopelink.hpp"
#include "tl-refptr.hpp"
#include "cxx-lexer.h"
#include "cxx-driver.h"
#include "cxx-scope.h"
#include "cxx-buildscope.h"

namespace TL
{
    class Source;

    //! Auxiliar class used by Source
    class LIBTL_CLASS SourceChunk
    {
        private:
            int _refcount;
        public:
            virtual std::string get_source() const = 0;
            virtual ~SourceChunk() { } 
            virtual bool is_source_text() { return false; }
            virtual bool is_source_ref() { return false; }

            SourceChunk()
                : _refcount(1)
            {
            }

            void obj_reference()
            {
                this->_refcount++;
            }

            void obj_unreference()
            {
                this->_refcount--;

                if (this->_refcount == 0)
                {
                    delete this;
                }
            }
    };

    //! A chunk of literal text
    class LIBTL_CLASS SourceText : public SourceChunk
    {
        private:
            std::string _source;
        public:
            virtual std::string get_source() const
            {
                return _source;
            }

            SourceText(const std::string& str)
                : _source(str)
            {
            }

            virtual ~SourceText() { } 

            virtual bool is_source_text() { return true; }

            friend class Source;
    };

    //! A chunk that references another Source
    class LIBTL_CLASS SourceRef : public SourceChunk
    {
        private:
            RefPtr<Source> _src;
        public:
            SourceRef(RefPtr<Source> src)
                : _src(src)
            {
            }
            virtual std::string get_source() const;

            virtual ~SourceRef() { }

            virtual bool is_source_ref() { return true; }

            friend class Source;
    };

    typedef RefPtr<SourceChunk> SourceChunkRef;

    typedef RefPtr<ObjectList<SourceChunkRef> > chunk_list_ref_t;

    //! A class used to generate in, a convenient way, code in the compiler
    class LIBTL_CLASS Source : public Object
    {
        public:
            //! Flags that modify the behaviour of parsing
            enum ParseFlags
            {
                UNKNOWN = 0,
                DEFAULT = 1 << 0,
                //! Allows symbols be redefined, thus overwriting previous definitions
                ALLOW_REDECLARATION = 1 << 1,
                //! Does not check an expression
                DO_NOT_CHECK_EXPRESSION = 1 << 2,
            };
        private:
            chunk_list_ref_t _chunk_list;

            void append_text_chunk(const std::string& str);
            void append_source_ref(SourceChunkRef src);

            bool all_blanks() const;

        public:
            //! Constructor
            /*!
             * Creates an empty source
             */
            Source()
                : _chunk_list(0)
            {
                _chunk_list = chunk_list_ref_t(new ObjectList<SourceChunkRef>());
            }

            virtual ~Source()
            {
            }

            //! Constructor
            /*!
             * Creates a source after a string.
             */
            Source(const std::string& str)
                : _chunk_list(0)
            {
                _chunk_list = chunk_list_ref_t(new ObjectList<SourceChunkRef>());
                _chunk_list->push_back(SourceChunkRef(new SourceText(str)));
            }

            Source(RefPtr<Object> obj)
                : _chunk_list(0)
            {
                RefPtr<Source> cast = RefPtr<Source>::cast_dynamic(obj);

                if (cast.get_pointer() == NULL)
                {
                    if (typeid(*obj.get_pointer()) != typeid(Undefined))
                    {
                        std::cerr << "Bad initialization of Source" << std::endl;
                    }
                    else
                    {
                        _chunk_list = chunk_list_ref_t(new ObjectList<SourceChunkRef>());
                    }
                }
                else
                {
                    // Share the list
                    _chunk_list = cast->_chunk_list;
                }

                // ---
            }

            //! Copy-constructor
            Source(const Source& src)
                // This is fine, we want to share the same source list for both
                : Object(src), _chunk_list(src._chunk_list)
            {
            }

            //! States that this is a source
            virtual bool is_source() const
            {
                return true;
            }
            
            //! Returns the textual information held by this Source
            /*!
             * Referenced Source objects in this one are recursively called their get_source
             * to form the whole text.
             */
            std::string get_source(bool with_newlines = false) const;

            //! This is a convenience function that calls get_source
            operator std::string();
            
            //! Convenience function to build lists with separators
            /*!
             * If the original source is empty, no separator will be added.
             */
            Source& append_with_separator(const std::string& src, const std::string& separator);
            //! Convenience function to build lists with separators
            /*!
             * If the original source is empty, no separator will be added.
             */
            Source& append_with_separator(Source& src, const std::string& separator);

            //! Appends a reference to a Source in this object
            /*!
             * Do not create cyclic dependences of sources
             */
            Source& operator<<(Source& src);
            //! Appends a text chunk
            Source& operator<<(const std::string& str);
            //! Appends a text chunk after an integer
            /*!
             * \param n This integer is: converted into decimal base and appended as a string
             */
            Source& operator<<(int n);

            //! Appends a reference to Source
            Source& operator<<(RefPtr<Source>);

            // -- new family of parse_XXX
            // These should work correctly in C++ as they are able to get the exact
            // declaration context of the reference tree (ref_tree)
            //! Parses a top-level declaration in context of global scope
            /*!
             * \param ref_tree Reference tree used when parsing this code
             * \param scope_link Scope link used to get the scope of \a ref_tree
             */
            AST_t parse_global(AST_t ref_tree, TL::ScopeLink scope_link);
            //! Parses a statement
            /*!
             * \param ref_tree Reference tree used when parsing this code
             * \param scope_link Scope link used to get the scope of \a ref_tree
             * \param parse_flags Parsing flags
             */
            AST_t parse_statement(AST_t ref_tree, TL::ScopeLink scope_link, ParseFlags parse_flags = DEFAULT);
            //! Parses an expression
            /*!
             * \param ref_tree Reference tree used when parsing this code
             * \param scope_link Scope link used to get the scope of \a ref_tree
             * \param parse_flags Parsing flags
             */
            AST_t parse_expression(AST_t ref_tree, TL::ScopeLink scope_link, ParseFlags parse_flags = DEFAULT);
            //! Parses an expression list
            /*!
             * \param ref_tree Reference tree used when parsing this code
             * \param scope_link Scope link used to get the scope of \a ref_tree
             * \param parse_flags Parsing flags
             */
            AST_t parse_expression_list(AST_t ref_tree, TL::ScopeLink scope_link, ParseFlags parse_flags = DEFAULT);
            //! Parses a declaration
            /*!
             * \param ref_tree Reference tree used when parsing this code
             * \param scope_link Scope link used to get the scope of \a ref_tree
             * \param parse_flags Parsing flags
             */
            AST_t parse_declaration(AST_t ref_tree, TL::ScopeLink scope_link, ParseFlags parse_flags = DEFAULT);
            //! Parses a member declaration
            /*!
             * \param ref_tree Reference tree used when parsing this code
             * \param scope_link Scope link used to get the scope of \a ref_tree
             * \param class_type Class type where this member should belong to
             */
            AST_t parse_member(AST_t ref_tree, TL::ScopeLink scope_link, Type class_type);
            //! Convenience function to parse a type and synthesize it
            /*!
             * \param ref_tree Reference tree used when parsing this code
             * \param scope_link Scope link used to get the scope of \a ref_tree
             * \return The synthesized type
             */
            Type parse_type(AST_t ref_tree, TL::ScopeLink scope_link);
            //! Convenience function to parse a comma separated list of types and synthesize them
            /*!
             * \param ref_tree Reference tree used when parsing this code
             * \param scope_link Scope link used to get the scope of \a ref_tree
             * \return The synthesized type
             */
            ObjectList<Type> parse_type_list(AST_t ref_tree, TL::ScopeLink scope_link);
            // -- end of new family of parse_XXX

            // Format string for debugging
            static std::string format_source(const std::string&);

            //! States whether this Source is empty
            bool empty() const;

            bool operator==(const Source& src) const;
            bool operator!=(const Source& src) const;
            bool operator<(const Source& src) const;
            Source& operator=(const Source& src);
    };

    //! Creates an inner comment in the code
    /*!
     * When these are prettyprinted onto the output file
     * they are converted into normal C or C++ comments
     */
    LIBTL_EXTERN std::string comment(const std::string& str);
    //! Creates an inner preprocessor line
    /*!
     * When these are prettyprinted onto the output file
     * they are converted into normal C or C++ preprocessor lines
     */
    LIBTL_EXTERN std::string preprocessor_line(const std::string& str);

    //! Creates a placeholder for the given AST_t
    /*!
     * This function creates a placeholder for statements. Once parsed, \a
     * placeholder can be used as a reference tree for further parsings and can
     * be replaced. If it is not replaced it will default to an empty
     * statement.
     */
    LIBTL_EXTERN std::string statement_placeholder(AST_t& placeholder);

    //! Convenience function to convert a list into a string
    /*!
     * \param t List of elements of type T
     * \param to_str Function that gives a std::string after an object of type T
     * \param separator Separator that will be used to separe the strings
     * \return A single string with the result of \a to_str applied to all elements of \a t separated with \a separator
     */
    template <class T>
    std::string to_string(const ObjectList<T>& t, Functor<std::string, T>& to_str, const std::string& separator = "")
    {
        std::string result;

        for (typename ObjectList<T>::const_iterator it = t.begin();
                it != t.end();
                it++)
        {
            if (it != t.begin())
            {
                result = result + separator;
            }

            result = result + to_str(*it);
        }

        return result;
    }

    LIBTL_EXTERN std::string to_string(const ObjectList<std::string>& t, const std::string& separator = "");
}

#endif // TL_SOURCE_T_HPP
