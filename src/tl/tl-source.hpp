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
#ifndef TL_SOURCE_T_HPP
#define TL_SOURCE_T_HPP

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

    class SourceChunk
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

    class SourceText : public SourceChunk
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

    class SourceRef : public SourceChunk
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

    class Source : public Object
    {
        public:
            enum ParseFlags
            {
                UNKNOWN = 0,
                DEFAULT = 1 << 0,
                ALLOW_REDECLARATION = 1 << 1,
                DO_NOT_CHECK_EXPRESSION = 2 << 1,
            };
        private:
            chunk_list_ref_t _chunk_list;

            void append_text_chunk(const std::string& str);
            void append_source_ref(SourceChunkRef src);

            bool all_blanks() const;
            AST_t parse_declaration_inner(TL::Scope ctx, TL::ScopeLink scope_link, ParseFlags parse_flags = DEFAULT);

            std::string format_source(const std::string&);
        public:
            Source()
                : _chunk_list(0)
            {
                _chunk_list = chunk_list_ref_t(new ObjectList<SourceChunkRef>());
            }

            virtual ~Source()
            {
            }

            Source(const std::string& str)
                : _chunk_list(0)
            {
                _chunk_list = chunk_list_ref_t(new ObjectList<SourceChunkRef>());
                _chunk_list->push_back(SourceChunkRef(new SourceText(str)));
            }

            Source(const Source& src)
                // This is fine, we want to share the same source list for both
                : Object(src), _chunk_list(src._chunk_list)
            {
            }

            virtual bool is_source() const
            {
                return true;
            }
            
            std::string get_source(bool with_newlines = false) const;
            
            Source& append_with_separator(const std::string& src, const std::string& separator);
            Source& append_with_separator(Source& src, const std::string& separator);

            Source& operator<<(Source& src);
            Source& operator<<(const std::string& str);
            Source& operator<<(int n);

            // -- deprecated family of parse_XXX
            // These are deprecated and only work reasonably well in C, in C++
            // they do not get the proper declarating context
            AST_t parse_global(TL::Scope ctx, TL::ScopeLink scope_link) DEPRECATED;
            AST_t parse_statement(TL::Scope ctx, TL::ScopeLink scope_link) DEPRECATED;
            AST_t parse_expression(TL::Scope ctx) DEPRECATED;
            AST_t parse_expression(TL::Scope ctx, TL::ScopeLink scope_link) DEPRECATED;
            AST_t parse_declaration(TL::Scope ctx, TL::ScopeLink scope_link, ParseFlags parse_flags = DEFAULT) DEPRECATED;
            AST_t parse_member(TL::Scope ctx, TL::ScopeLink scope_link, Type class_type) DEPRECATED;
            // -- end of deprecated family

            // -- new family of parse_XXX
            // These should work correctly in C++ as they are able to get the exact
            // declaration context of the reference tree (ref_tree)
            AST_t parse_global(AST_t ref_tree, TL::ScopeLink scope_link);
            AST_t parse_statement(AST_t ref_tree, TL::ScopeLink scope_link);
            AST_t parse_expression(AST_t ref_tree, TL::ScopeLink scope_link, ParseFlags parse_flags = DEFAULT);
            AST_t parse_declaration(AST_t ref_tree, TL::ScopeLink scope_link, ParseFlags parse_flags = DEFAULT);
            AST_t parse_member(AST_t ref_tree, TL::ScopeLink scope_link, Type class_type);
            Type parse_type(AST_t ref_tree, TL::ScopeLink scope_link);
            // -- end of new family of parse_XXX

            bool empty() const;

            bool operator==(const Source& src) const;
            bool operator!=(const Source& src) const;
            bool operator<(const Source& src) const;
            Source& operator=(const Source& src);
    };

    std::string comment(const std::string& str);
    std::string preprocessor_line(const std::string& str);

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

    std::string to_string(const ObjectList<std::string>& t, const std::string& separator = "");
}

#endif // TL_SOURCE_T_HPP
