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
#include "tl-source.hpp"
#include "cxx-exprtype.h"
#include "cxx-ambiguity.h"
#include <iostream>
#include <sstream>
#include <iomanip>
#include <cstring>
#include "cxx-printscope.h"
#include "cxx-utils.h"
#include "cxx-parser.h"
#include "c99-parser.h"

namespace TL
{
    std::string SourceRef::get_source() const
    {
        return _src->get_source();
    }

    void Source::append_text_chunk(const std::string& str)
    {
        if (_chunk_list->empty())
        {
            _chunk_list->push_back(SourceChunkRef(new SourceText(str)));
        }
        else
        {
            SourceChunkRef last = *(_chunk_list->rbegin());

            if (last->is_source_text())
            {
                RefPtr<SourceText> text = RefPtr<SourceText>::cast_dynamic(last);
                text->_source += str;
            }
            else
            {
                _chunk_list->push_back(SourceChunkRef(new SourceText(str)));
            }
        }
    }

    void Source::append_source_ref(SourceChunkRef ref)
    {
        _chunk_list->push_back(ref);
    }

    Source& Source::operator<<(const std::string& str)
    {
        append_text_chunk(str);
        return *this;
    }

    Source& Source::operator<<(int num)
    {
        std::stringstream ss;
        ss << num;
        append_text_chunk(ss.str());
        return *this;
    }

    Source& Source::operator<<(Source& src)
    {
        RefPtr<Source> ref_src = RefPtr<Source>(new Source(src));

        SourceChunkRef new_src = SourceChunkRef(new SourceRef(ref_src));

        append_source_ref(new_src);
        return *this;
    }

    Source& Source::operator<<(RefPtr<Source> src)
    {
        SourceChunkRef new_src = SourceChunkRef(new SourceRef(src));

        append_source_ref(new_src);
        return *this;
    }

    Source::operator std::string()
    {
        return this->get_source(true);
    }

    std::string Source::get_source(bool with_newlines) const
    {
        std::string temp_result;
        for(ObjectList<SourceChunkRef>::const_iterator it = _chunk_list->begin();
                it != _chunk_list->end();
                it++)
        {
            temp_result += (*it)->get_source();
        }
        std::string result;

        if (!with_newlines)
        {
            result = temp_result;
        }
        else
        {
            // Eases debugging
            bool inside_string = false;
            char current_string = ' ';
            for (unsigned int i = 0; i < temp_result.size(); i++)
            {
                char c = temp_result[i];

                if (!inside_string)
                {
                    if (c == '\'' 
                            || c == '"')
                    {
                        inside_string = true;
                        current_string = c;
                    }
                }
                else
                {
                    if (c == current_string
                            && ((i == 1 && temp_result[i-1] != '\\')
                                || (i > 1 && 
                                    (temp_result[i-1] != '\\'
                                     || temp_result[i-2] == '\\')))
                       )
                    {
                        inside_string = false;
                    }
                }

                result += c;
                // Do not split if we are inside a string!
                if (!inside_string
                        && (c == ';' || c == '{' || c == '}'))
                {
                    result += '\n';
                }
            }
        }

        return result;
    }

    AST_t Source::parse_global(AST_t ref_tree, TL::ScopeLink scope_link)
    {
        AST_t global_tree = ref_tree.get_translation_unit();

        return parse_declaration(global_tree, scope_link);
    }

    AST_t Source::parse_statement(AST_t ref_tree, TL::ScopeLink scope_link, ParseFlags parse_flags)
    {
        std::string mangled_text = "@STATEMENT@ " + this->get_source(true);
        char* str = strdup(mangled_text.c_str());

        CXX_LANGUAGE()
        {
            mcxx_prepare_string_for_scanning(str);
        }
        C_LANGUAGE()
        {
            mc99_prepare_string_for_scanning(str);
        }

        int parse_result = 0;
        AST a;

        CXX_LANGUAGE()
        {
            parse_result = mcxxparse(&a);
        }
        C_LANGUAGE()
        {
            parse_result = mc99parse(&a);
        }

        if (parse_result != 0)
        {
            running_error("Could not parse statement\n\n%s\n", 
                    format_source(this->get_source(true)).c_str());
        }

        bool do_not_check_expression = false;
        int parse_flags_int = (int)parse_flags;
        if ((parse_flags_int & Source::DO_NOT_CHECK_EXPRESSION) 
                == Source::DO_NOT_CHECK_EXPRESSION)
        {
            do_not_check_expression = true;
        }
        
        // Get the scope and declarating context of the reference tree
        decl_context_t decl_context = scope_link_get_decl_context(scope_link._scope_link, ref_tree._ast);
        if (a != NULL)
        {
            if (do_not_check_expression)
            {
                enter_test_expression();
                // As a gotcha, ambiguous decl-expr are defaulted to
                // expressions if they could not be properly checked
                decl_context.decl_flags = (decl_flags_t)(decl_context.decl_flags | DF_AMBIGUITY_FALLBACK_TO_EXPR);
            }
            build_scope_statement_seq_with_scope_link(a, decl_context, scope_link._scope_link);
            if (do_not_check_expression)
            {
                leave_test_expression();
            }
        }

        // Set properly the context of the reference tree
        scope_link_set(scope_link._scope_link, a, decl_context);

        AST_t result(a);
        return result;
    }

    AST_t Source::parse_expression(AST_t ref_tree, TL::ScopeLink scope_link, ParseFlags parse_flags)
    {
        std::string mangled_text = "@EXPRESSION@ " + this->get_source(true);
        char* str = strdup(mangled_text.c_str());

        CXX_LANGUAGE()
        {
            mcxx_prepare_string_for_scanning(str);
        }
        C_LANGUAGE()
        {
            mc99_prepare_string_for_scanning(str);
        }

        AST a;
        int parse_result = 0;

        CXX_LANGUAGE()
        {
            parse_result = mcxxparse(&a);
        }
        C_LANGUAGE()
        {
            parse_result = mc99parse(&a);
        }

        if (parse_result != 0)
        {
            running_error("Could not parse the expression '%s'", 
                    format_source(this->get_source(true)).c_str());
        }

        bool do_not_check_expression = false;
        int parse_flags_int = (int)parse_flags;
        if ((parse_flags_int & Source::DO_NOT_CHECK_EXPRESSION) 
                == Source::DO_NOT_CHECK_EXPRESSION)
        {
            do_not_check_expression = true;
        }

        // Get the scope and declarating context of the reference tree
        CURRENT_CONFIGURATION->scope_link = scope_link._scope_link;
        decl_context_t decl_context = scope_link_get_decl_context(scope_link._scope_link, ref_tree._ast);

        if (a != NULL)
        {
            enter_test_expression();
            char c = check_for_expression(a, decl_context);
            leave_test_expression();

            if (!c && !do_not_check_expression)
            {
                if (CURRENT_CONFIGURATION->strict_typecheck)
                {
                    internal_error("Could not check expression '%s'\n", prettyprint_in_buffer(a));
                }
                else
                {
                    WARNING_MESSAGE("Could not check expression '%s'\n", prettyprint_in_buffer(a));
                }
            }
        }

        CURRENT_CONFIGURATION->scope_link = NULL;

        AST_t result(a);

        scope_link_set(scope_link._scope_link, a, decl_context);

        return result;
    }

    AST_t Source::parse_expression_list(AST_t ref_tree, TL::ScopeLink scope_link, ParseFlags parse_flags)
    {
        std::string mangled_text = "@EXPRESSION-LIST@ " + this->get_source(true);
        char* str = strdup(mangled_text.c_str());

        CXX_LANGUAGE()
        {
            mcxx_prepare_string_for_scanning(str);
        }
        C_LANGUAGE()
        {
            mc99_prepare_string_for_scanning(str);
        }

        AST a;
        int parse_result = 0;

        CXX_LANGUAGE()
        {
            parse_result = mcxxparse(&a);
        }
        C_LANGUAGE()
        {
            parse_result = mc99parse(&a);
        }

        if (parse_result != 0)
        {
            running_error("Could not parse the expression-list '%s'", 
                    format_source(this->get_source(true)).c_str());
        }

        bool do_not_check_expression = false;
        int parse_flags_int = (int)parse_flags;
        if ((parse_flags_int & Source::DO_NOT_CHECK_EXPRESSION) 
                == Source::DO_NOT_CHECK_EXPRESSION)
        {
            do_not_check_expression = true;
        }

        // Get the scope and declarating context of the reference tree
        CURRENT_CONFIGURATION->scope_link = scope_link._scope_link;
        decl_context_t decl_context = scope_link_get_decl_context(scope_link._scope_link, ref_tree._ast);

        if (a != NULL)
        {
            enter_test_expression();
            char c = check_for_expression_list(a, decl_context);
            leave_test_expression();

            if (!c && !do_not_check_expression)
            {
                if (CURRENT_CONFIGURATION->strict_typecheck)
                {
                    internal_error("Could not check expression-list '%s'\n", prettyprint_in_buffer(a));
                }
                else
                {
                    WARNING_MESSAGE("Could not check expression-list '%s'\n", prettyprint_in_buffer(a));
                }
            }
        }

        CURRENT_CONFIGURATION->scope_link = NULL;

        AST_t result(a);

        scope_link_set(scope_link._scope_link, a, decl_context);

        return result;
    }

    AST_t Source::parse_declaration(AST_t ref_tree, TL::ScopeLink scope_link, ParseFlags parse_flags)
    {
        std::string mangled_text = "@DECLARATION@ " + this->get_source(true);
        char* str = strdup(mangled_text.c_str());

        CXX_LANGUAGE()
        {
            mcxx_prepare_string_for_scanning(str);
        }
        C_LANGUAGE()
        {
            mc99_prepare_string_for_scanning(str);
        }

        int parse_result = 0;
        AST a;

        CXX_LANGUAGE()
        {
            parse_result = mcxxparse(&a);
        }
        C_LANGUAGE()
        {
            parse_result = mc99parse(&a);
        }

        if (parse_result != 0)
        {
            running_error("Could not parse declaration\n\n%s\n", 
                    format_source(this->get_source(true)).c_str());
        }
        
        // Get the scope and declarating context of the reference tree
        decl_context_t decl_context = scope_link_get_decl_context(scope_link._scope_link, ref_tree._ast);

        int parse_flags_int = (int)parse_flags;
        if ((parse_flags_int & Source::ALLOW_REDECLARATION) == Source::ALLOW_REDECLARATION)
        {
            decl_context.decl_flags = (decl_flags_t)((int)(decl_context.decl_flags) | DF_ALLOW_REDEFINITION);
        }

        if (a != NULL)
        {
            build_scope_declaration_sequence_with_scope_link(a, decl_context, scope_link._scope_link);
        }

        // Set properly the context of the reference tree
        scope_link_set(scope_link._scope_link, a, decl_context);

        AST_t result(a);
        return result;
    }

    AST_t Source::parse_member(AST_t /* ref_tree */, TL::ScopeLink scope_link, Type class_type)
    {
        std::string mangled_text = "@MEMBER@ " + this->get_source(true);
        char* str = strdup(mangled_text.c_str());

        mcxx_prepare_string_for_scanning(str);

        int parse_result = 0;
        AST a;
        parse_result = mcxxparse(&a);

        if (parse_result != 0)
        {
            running_error("Could not parse member declaration\n\n%s\n", 
                    format_source(this->get_source(true)).c_str());
        }
        
        // Fix the context for the user (the reference tree is unused in this function actually...)
        decl_context_t decl_context = class_type_get_inner_context(get_actual_class_type(class_type._type_info));
        build_scope_member_specification_with_scope_link(decl_context, scope_link._scope_link, a, AS_PUBLIC, 
                class_type._type_info);

        // Set properly the context of the reference tree
        scope_link_set(scope_link._scope_link, a, decl_context);

        return AST_t(a);
    }

    Type Source::parse_type(AST_t ref_tree, TL::ScopeLink scope_link)
    {
        std::string mangled_text = "@TYPE@ " + this->get_source(true);
        char* str = strdup(mangled_text.c_str());

        CXX_LANGUAGE()
        {
            mcxx_prepare_string_for_scanning(str);
        }
        C_LANGUAGE()
        {
            mc99_prepare_string_for_scanning(str);
        }

        int parse_result = 0;
        AST type_specifier_seq;
        CXX_LANGUAGE()
        {
            parse_result = mcxxparse(&type_specifier_seq);
        }
        C_LANGUAGE()
        {
            parse_result = mc99parse(&type_specifier_seq);
        }

        if (parse_result != 0)
        {
            running_error("Could not parse type specifier\n\n%s\n", 
                    format_source(this->get_source(true)).c_str());
        }

        // Get the scope and declarating context of the reference tree
        decl_context_t decl_context = scope_link_get_decl_context(scope_link._scope_link, ref_tree._ast);

        // Set properly the context of the reference tree
        scope_link_set(scope_link._scope_link, type_specifier_seq, decl_context);

        type_t* type_info = NULL;
        gather_decl_spec_t gather_info;
        memset(&gather_info, 0, sizeof(gather_info));

        build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &type_info,
                decl_context);

        return Type(type_info);
    }

    ObjectList<Type> Source::parse_type_list(AST_t ref_tree, TL::ScopeLink scope_link)
    {
        std::string mangled_text = "@TYPE-LIST@ " + this->get_source(true);
        char* str = strdup(mangled_text.c_str());

        CXX_LANGUAGE()
        {
            mcxx_prepare_string_for_scanning(str);
        }
        C_LANGUAGE()
        {
            mc99_prepare_string_for_scanning(str);
        }

        int parse_result = 0;
        AST type_specifier_seq_list;
        CXX_LANGUAGE()
        {
            parse_result = mcxxparse(&type_specifier_seq_list);
        }
        C_LANGUAGE()
        {
            parse_result = mc99parse(&type_specifier_seq_list);
        }

        if (parse_result != 0)
        {
            running_error("Could not parse type specifier list\n\n%s\n", 
                    format_source(this->get_source(true)).c_str());
        }

        ObjectList<Type> result;

        // Get the scope and declarating context of the reference tree
        decl_context_t decl_context = scope_link_get_decl_context(scope_link._scope_link, ref_tree._ast);

        // Set properly the context of the reference tree
        scope_link_set(scope_link._scope_link, type_specifier_seq_list, decl_context);

        AST iter, list = type_specifier_seq_list;

        for_each_element(list, iter)
        {
            AST type_spec = ASTSon1(iter);

            type_t* type_info = NULL;
            gather_decl_spec_t gather_info;
            memset(&gather_info, 0, sizeof(gather_info));

            build_scope_decl_specifier_seq(type_spec, &gather_info, &type_info,
                    decl_context);

            result.append(Type(type_info));
        }

        return result;
    }

    bool Source::operator==(const Source& src) const
    {
        return this->get_source() == src.get_source();
    }

    bool Source::operator!=(const Source &src) const
    {
        return !(this->operator==(src));
    }

    bool Source::operator<(const Source &src) const
    {
        return this->get_source() < src.get_source();
    }

    Source& Source::operator=(const Source& src)
    {
        if (this != &src)
        {
            // The same as *(_chunk_list.operator->()) = *(src._chunk_list.operator->()); but clearer
            _chunk_list->clear();
            for(ObjectList<SourceChunkRef>::const_iterator it = src._chunk_list->begin();
                    it != src._chunk_list->end();
                    it++)
            {
                _chunk_list->push_back(*it);
            }
        }
        return (*this);
    }

    Source& Source::append_with_separator(const std::string& src, const std::string& separator)
    {
        if (all_blanks())
        {
            append_text_chunk(src);
        }
        else
        {
            append_text_chunk(separator + src);
        }

        return (*this);
    }

    Source& Source::append_with_separator(Source& src, const std::string& separator)
    {
        if (!all_blanks())
        {
            append_text_chunk(separator);
        }
        RefPtr<Source> ref_source = RefPtr<Source>(new Source(src));
        append_source_ref(SourceChunkRef(new SourceRef(ref_source)));

        return (*this);
    }

    bool Source::empty() const
    {
        return all_blanks();
    }

    bool Source::all_blanks() const
    {
        if (_chunk_list->empty())
        {
            return true;
        }

        bool blanks = true;
        std::string code = this->get_source();
        int len = code.size();

        for (int i = 0; (i < len) && blanks; i++)
        {
            blanks &= (code[i] == ' ') || (code[i] == '\t');
        }

        return blanks;
    }

    std::string comment(const std::string& str)
    {
        std::string result;

        result = "@-C-@" + str + "@-CC-@";
        return result;
    }

    std::string preprocessor_line(const std::string& str)
    {
        std::string result;

        result = "@-P-@" + str + "@-PP-@";
        return result;
    }

    std::string statement_placeholder(AST_t& placeholder)
    {
        // This code violates all aliasing assumptions since we are codifying
        // an address into a string and using it later to get the original
        // address. This is kind of a hack.
        AST* ast_field_ptr = placeholder.get_internal_ast_field_ptr();
        char c[256];
        snprintf(c, 255, "@STATEMENT-PH::%p@", (void*)ast_field_ptr);
        c[255] = '\0';
        return std::string(c);
    }

    std::string to_string(const ObjectList<std::string>& t, const std::string& separator)
    {
        std::string result;

        for (ObjectList<std::string>::const_iterator it = t.begin();
                it != t.end();
                it++)
        {
            if (it != t.begin())
            {
                result = result + separator;
            }

            result = result + (*it);
        }

        return result;
    }

    // This is quite inefficient but will do
    std::string Source::format_source(const std::string& src)
    {
        int line = 1;

        std::stringstream ss;

        ss << "[" << std::setw(5) << line << std::setw(0) << "] ";


        for (std::string::const_iterator it = src.begin();
                it != src.end();
                it++)
        {
            ss << *it;
            if (*it == '\n')
            {
                line++;
                ss << "[" << std::setw(5) << line << std::setw(0) << "] ";
            }
        }

        return ss.str();
    }
}
