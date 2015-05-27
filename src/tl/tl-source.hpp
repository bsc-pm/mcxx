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




#ifndef TL_SOURCE_T_HPP
#define TL_SOURCE_T_HPP

#include "tl-source-fwd.hpp"
#include "tl-common.hpp"
#include "tl-object.hpp"
#include "tl-objectlist.hpp"
#include "tl-type-fwd.hpp"
#include "tl-nodecl-fwd.hpp"
#include "tl-scope.hpp"

#include <string>

#include <memory>

#include "cxx-lexer.h"
#include "cxx-utils.h"
#include "cxx-scope.h"
#include "cxx-buildscope.h"

namespace TL
{
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
        public:
            typedef std::shared_ptr<Source> SourceSharedPtr;
        private:
            SourceSharedPtr _src;
        public:
            SourceRef(SourceSharedPtr src)
                : _src(src)
            {
            }
            virtual std::string get_source() const;

            virtual ~SourceRef() { }

            virtual bool is_source_ref() { return true; }

            friend class Source;
    };

    typedef std::shared_ptr<SourceChunk> SourceChunkRef;

    typedef std::shared_ptr<ObjectList<SourceChunkRef> > chunk_list_ref_t;

    struct SourceLanguage
    {
        enum L
        {
            Current = 0, 
            // Forced languages
            C,
            CPlusPlus,
            Fortran
        };

        SourceLanguage() : _l(Current) { }

        SourceLanguage(L l) : _l(l) { }
        SourceLanguage(const SourceLanguage& s) : _l(s._l) { }
        SourceLanguage& operator=(const SourceLanguage& s)
        {
            if (this != &s)
            {
                this->_l = s._l;
            }
            return *this;
        }

        L get_language() const
        {
            if (_l != Current)
            {
                return _l;
            }
            else
            {
                if (IS_C_LANGUAGE)
                    return C;
                if (IS_CXX_LANGUAGE)
                    return CPlusPlus;
                if (IS_FORTRAN_LANGUAGE)
                    return Fortran;
                internal_error("Code unreachable", 0);
            }
        }

        bool operator==(const L& l) const
        {
            return (_l == l);
        }

        bool operator!=(const L& l) const
        {
            return !this->operator==(l);
        }

        SourceLanguage& operator=(const L& l)
        {
            this->_l = l;
            return *this;
        }
        
        private:
        L _l;
    };

    class ReferenceScope
    {
        private:
            Scope _scope;
        public:
            ReferenceScope(Scope sc);
            ReferenceScope(Nodecl::NodeclBase nodecl);

            Scope get_scope() const;
    };

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

            static SourceLanguage source_language;

            typedef void (*compute_nodecl_fun_t)(AST, const decl_context_t*, nodecl_t*);
            typedef const decl_context_t* (*decl_context_map_fun_t)(const decl_context_t*);

        private:
            chunk_list_ref_t _chunk_list;

            void append_text_chunk(const std::string& str);
            void append_source_ref(SourceChunkRef src);

            bool all_blanks() const;

            typedef int (*prepare_lexer_fun_t)(const char*);
            typedef int (*parse_fun_t)(AST*);

            Nodecl::NodeclBase parse_common(ReferenceScope ref_scope,
                    ParseFlags parse_flags,
                    const std::string& substring_prefix,
                    prepare_lexer_fun_t prepare_lexer,
                    parse_fun_t parse,
                    compute_nodecl_fun_t compute_nodecl,
                    decl_context_map_fun_t decl_context_map_fun);

            static void switch_language(source_language_t& n);
            static void restore_language(source_language_t n);

        public:
            //! Constructor
            /*!
             * Creates an empty source
             */
            Source()
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
            {
                _chunk_list = chunk_list_ref_t(new ObjectList<SourceChunkRef>());
                _chunk_list->push_back(SourceChunkRef(new SourceText(str)));
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
             * Referenced Source objects in this one are recursively called
             * their get_source to form the whole text.
             *
             * \a with_newlines Formats in a pretty way the source code so it can be more readable
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

            //! Parsing at global level
            /*
             * This means parsing at file-scope/global namespace scope. Note
             * that some languages (mainly Fortran) restrict what can be declared
             * here.
             */
            Nodecl::NodeclBase parse_global(ReferenceScope sc, ParseFlags flags = DEFAULT);

            //! Parsing a declaration at namespace scope
            /*
             * In C/C++ this means parsing at namespace scope level
             * In Fortran this function behaves like parse_global
             */
            Nodecl::NodeclBase parse_declaration(ReferenceScope sc, ParseFlags flags = DEFAULT);

            //! Parse a statement
            /*!
             * Scope should be a block scope
             */
            Nodecl::NodeclBase parse_statement(ReferenceScope sc, ParseFlags flags = DEFAULT);

            //! Parse an expression
            Nodecl::NodeclBase parse_expression(ReferenceScope sc, ParseFlags flags = DEFAULT);

            //! Parse an member declaration
            /*!
             * This is C++ only. This parses a new member declaration
             * Scope should be class scope
             */
            Nodecl::NodeclBase parse_member(ReferenceScope sc, ParseFlags flags = DEFAULT);

            //! Parse an id-expression
            /*!
             * This is C++ only. The returned tree is not typechecked or looked up.
             * This is just an abstract tree denoting an entity. Can be used for
             * some lookup functions
             */
            Nodecl::NodeclBase parse_id_expression(ReferenceScope sc, ParseFlags flags = DEFAULT);


            //! Parse type-id from C/C++
            /*!
             * Use this to parse C/C++ type-ids like int, int[10], int* or T
             *
             * If called from Fortran it will behave as if it were C.
             * If some problem happens, the result will be an error type
             */
            TL::Type parse_c_type_id(ReferenceScope sc, ParseFlags flags = DEFAULT);

            //! Generic parser
            /*!
             * Use this one for custom created subparsers. Make sure you provide everything
             */
            Nodecl::NodeclBase parse_generic(ReferenceScope ref_scope,
                    ParseFlags parse_flags,
                    const std::string& substring_prefix,
                    compute_nodecl_fun_t compute_nodecl,
                    decl_context_map_fun_t decl_context_map_fun);

            // Format string for debugging
            static std::string format_source(const std::string&);

            //! States whether this Source is empty
            bool empty() const;

            bool operator==(const Source& src) const;
            bool operator!=(const Source& src) const;
            bool operator<(const Source& src) const;
            Source& operator=(const Source& src);

            // Adapters for expressions
            static void c_cxx_check_expression_adapter(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output);
            static void fortran_check_expression_adapter(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output);
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

    //! Creates a placeholder for the given Nodecl::NodeclBase
    /*!
     * This function creates a placeholder for statements. Once parsed, \a
     * placeholder can be used as a reference tree for further parsings and can
     * be replaced. If it is not replaced it will default to an empty
     * statement.
     */
    LIBTL_EXTERN std::string statement_placeholder(Nodecl::NodeclBase& placeholder);

    //! Creates a #line marker
    /*!
     * This function adds a line marker useable for cpp-style preprocessing. 
     * Use this to add more context to your trees, so warnings and messages
     * have better context
     * \a filename Can be an empty string
     * \a line Line
     * \a column Column
     */
    LIBTL_EXTERN std::string line_marker(const std::string& filename,
            unsigned int line,
            unsigned int column);
    LIBTL_EXTERN std::string line_marker(const std::string& filename, unsigned int line);

    //! Convenience function to convert a list into a string
    /*!
     * \param t List of elements of type T
     * \param to_str Function that gives a std::string after an object of type T
     * \param separator Separator that will be used to separe the strings
     * \return A single string with the result of \a to_str applied to all elements of \a t separated with \a separator
     */
    template <class T>
    std::string to_string(const ObjectList<T>& t, std::function<std::string(T)>& to_str, const std::string& separator = "")
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

    // Use these to embed TL::NodeclBase in Source where an expression is valid
    LIBTL_EXTERN std::string as_expression(const Nodecl::NodeclBase& nodecl);
    
    // Use these to embed TL::NodeclBase in Source where a statement is valid
    LIBTL_EXTERN std::string as_statement(const Nodecl::NodeclBase& nodecl);

    // Use these to embed TL::Type in Source where a typename would be valid
    LIBTL_EXTERN std::string as_type(TL::Type t);

    // Use these to embed TL::Symbol in Source where an id-expression would be valid
    LIBTL_EXTERN std::string as_symbol(TL::Symbol s);

    LIBTL_EXTERN std::string pad_to_column(unsigned int n);
}

#endif // TL_SOURCE_T_HPP
