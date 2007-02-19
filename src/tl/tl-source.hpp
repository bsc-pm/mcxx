#ifndef TL_SOURCE_T_HPP
#define TL_SOURCE_T_HPP

#include <string>
#include "tl-object.hpp"
#include "tl-ast.hpp"
#include "tl-scope.hpp"
#include "tl-scopelink.hpp"
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
        public:
            virtual std::string get_source() const = 0;
            virtual ~SourceChunk() { } 
            virtual bool is_source_text() { return false; }
            virtual bool is_source_ref() { return false; }
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
            Source* _src;
        public:
            SourceRef(Source& src)
                : _src(&src)
            {
            }
            virtual std::string get_source() const;

            virtual ~SourceRef() { }

            virtual bool is_source_ref() { return true; }

            friend class Source;
    };

    class Source : public Object
    {
        public:
            enum ParseFlags
            {
                UNKNOWN = 0,
                DEFAULT = 1 << 0,
                ALLOW_REDECLARATION = 1 << 1
            };
        private:
            std::vector<SourceChunk*>* _chunk_list;

            void append_text_chunk(const std::string& str);
            void append_source_ref(Source& src);

            bool all_blanks() const;
        public:
            Source()
            {
                _chunk_list = new std::vector<SourceChunk*>();
            }

            Source(const std::string& str)
            {
                _chunk_list = new std::vector<SourceChunk*>();
                (*_chunk_list).push_back(new SourceText(str));
            }

            Source(const Source& src)
                : _chunk_list(src._chunk_list)
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

            AST_t parse_global(TL::Scope ctx, TL::ScopeLink scope_link);
            AST_t parse_statement(TL::Scope ctx, TL::ScopeLink scope_link);
            AST_t parse_expression(TL::Scope ctx);
            AST_t parse_expression(TL::Scope ctx, TL::ScopeLink scope_link);
            AST_t parse_declaration(TL::Scope ctx, TL::ScopeLink scope_link, ParseFlags parse_flags = DEFAULT);
            AST_t parse_member(TL::Scope ctx, TL::ScopeLink scope_link, Type class_type);

            bool empty() const;

            bool operator==(const Source& src) const;
            bool operator!=(const Source& src) const;
            bool operator<(const Source& src) const;
            Source& operator=(const Source& src);
    };

    std::string comment(const std::string& str);

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
