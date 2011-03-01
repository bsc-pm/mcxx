/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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
#ifdef FORTRAN_SUPPORT
#include "fortran03-lexer.h"
#include "fortran03-parser.h"
#include "fortran03-buildscope.h"
#include "fortran03-exprtype.h"
#endif

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
                        && (c == ';' 
                            || (!IS_FORTRAN_LANGUAGE
                                && (c == '{' || c == '}'))))
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

    template <typename T>
    T Source::parse_generic(AST_t ref_tree, 
            TL::ScopeLink scope_link, 
            ParseFlags parse_flags,
            const std::string& subparsing_prefix,
            prepare_lexer_fun_t prepare_lexer,
            parse_fun_t parse_function,
            typename FinishParseFun<T>::Type finish_parse
            )
    {
        std::string mangled_text = subparsing_prefix + " " + this->get_source(true);
        prepare_lexer(mangled_text.c_str());

        int parse_result = 0;
        AST a;

        parse_result = parse_function(&a);

        if (parse_result != 0)
        {
            running_error("Could not parse source\n\n%s\n", 
                    format_source(this->get_source(true)).c_str());
        }

        decl_context_t decl_context = scope_link_get_decl_context(scope_link._scope_link, ref_tree._ast);

        return finish_parse(parse_flags, 
                decl_context,
                scope_link._scope_link,
                a);
    }

    template <typename T>
    T Source::parse_generic_lang(AST_t ref_tree, 
            TL::ScopeLink scope_link, 
            ParseFlags parse_flags,
            const std::string& subparsing_prefix,
            typename FinishParseFun<T>::Type finish_parse
            )
    {
        prepare_lexer_fun_t prepare_lexer = NULL;
        parse_fun_t parse_function = NULL;
        C_LANGUAGE()
        {
            prepare_lexer = mc99_prepare_string_for_scanning;
            parse_function = mc99parse;
        }
        CXX_LANGUAGE()
        {
            prepare_lexer = mcxx_prepare_string_for_scanning;
            parse_function = mcxxparse;
        }
#ifdef FORTRAN_SUPPORT
        FORTRAN_LANGUAGE()
        {
            prepare_lexer = mf03_prepare_string_for_scanning;
            parse_function = mf03parse;
        }
#endif

        return parse_generic<T>(ref_tree, scope_link, parse_flags, 
                subparsing_prefix, prepare_lexer, parse_function, finish_parse);
    }

    static AST_t finish_parse_statement_c_cxx(
            Source::ParseFlags parse_flags, 
            decl_context_t decl_context,
            scope_link_t* scope_link,
            AST a)
    {
        bool do_not_check_expression = false;
        int parse_flags_int = (int)parse_flags;
        if ((parse_flags_int & Source::DO_NOT_CHECK_EXPRESSION) 
                == Source::DO_NOT_CHECK_EXPRESSION)
        {
            do_not_check_expression = true;
        }
        
        if (a != NULL)
        {
            if (do_not_check_expression)
            {
                enter_test_expression();
                // As a gotcha, ambiguous decl-expr are defaulted to
                // expressions if they could not be properly checked
                decl_context.decl_flags = (decl_flags_t)(decl_context.decl_flags | DF_AMBIGUITY_FALLBACK_TO_EXPR);
            }
            build_scope_statement_seq_with_scope_link(a, decl_context, scope_link);
            if (do_not_check_expression)
            {
                leave_test_expression();
            }
        }

        // Set properly the context of the reference tree
        scope_link_set(scope_link, a, decl_context);

        return AST_t(a);
    }

#ifdef FORTRAN_SUPPORT
    static AST_t finish_parse_block_fortran(
            Source::ParseFlags parse_flags, 
            decl_context_t decl_context,
            scope_link_t* scope_link,
            AST a)
    {
        fortran_build_scope_statement(a, decl_context);
        scope_link_set(scope_link, a, decl_context);

        return AST_t(a);
    }
#endif

    AST_t Source::parse_statement(AST_t ref_tree, TL::ScopeLink scope_link, ParseFlags parse_flags)
    {
        FinishParseFun<AST_t>::Type finish_parse = NULL;
        C_LANGUAGE()
        {
            finish_parse = finish_parse_statement_c_cxx;
        }
        CXX_LANGUAGE()
        {
            finish_parse = finish_parse_statement_c_cxx;
        }
#ifdef FORTRAN_SUPPORT
        FORTRAN_LANGUAGE()
        {
            finish_parse = finish_parse_block_fortran;
        }
#endif

        return parse_generic_lang<AST_t>(ref_tree, scope_link, parse_flags, 
                "@STATEMENT@", finish_parse);
    }

    static AST_t finish_parse_expression_c_cxx(
            Source::ParseFlags parse_flags, 
            decl_context_t decl_context,
            scope_link_t* scope_link,
            AST a)
    {
        bool do_not_check_expression = false;
        int parse_flags_int = (int)parse_flags;
        if ((parse_flags_int & Source::DO_NOT_CHECK_EXPRESSION) 
                == Source::DO_NOT_CHECK_EXPRESSION)
        {
            do_not_check_expression = true;
        }

        // Get the scope and declarating context of the reference tree
        CURRENT_CONFIGURATION->scope_link = scope_link;
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
                    std::cerr << ast_location(a)
                        << ": warning: internally generated expression '" 
                        << prettyprint_in_buffer(a) 
                        <<  "' is bad" 
                        << std::endl;
                }
            }
        }

        CURRENT_CONFIGURATION->scope_link = NULL;

        scope_link_set(scope_link, a, decl_context);

        return AST_t(a);
    }

#ifdef FORTRAN_SUPPORT
    static AST_t finish_parse_expression_fortran(
            Source::ParseFlags parse_flags, 
            decl_context_t decl_context,
            scope_link_t* scope_link,
            AST a)
    {
        fortran_check_expression(a, decl_context);
        scope_link_set(scope_link, a, decl_context);

        return AST_t(a);
    }
#endif

    AST_t Source::parse_expression(AST_t ref_tree, TL::ScopeLink scope_link, ParseFlags parse_flags)
    {
        FinishParseFun<AST_t>::Type finish_parse = NULL;
        C_LANGUAGE()
        {
            finish_parse = finish_parse_expression_c_cxx;
        }
        CXX_LANGUAGE()
        {
            finish_parse = finish_parse_expression_c_cxx;
        }
#ifdef FORTRAN_SUPPORT
        FORTRAN_LANGUAGE()
        {
            finish_parse = finish_parse_expression_fortran;
        }
#endif

        return parse_generic_lang<AST_t>(ref_tree, scope_link, parse_flags, 
                "@EXPRESSION@", finish_parse);
    }

    static AST_t finish_parse_expression_list_c_cxx(
            Source::ParseFlags parse_flags, 
            decl_context_t decl_context,
            scope_link_t* scope_link,
            AST a)
    {
        bool do_not_check_expression = false;
        int parse_flags_int = (int)parse_flags;
        if ((parse_flags_int & Source::DO_NOT_CHECK_EXPRESSION) 
                == Source::DO_NOT_CHECK_EXPRESSION)
        {
            do_not_check_expression = true;
        }

        if (a != NULL)
        {
            enter_test_expression();
            char c = check_for_expression_list(a, decl_context);
            leave_test_expression();

            if (!c && !do_not_check_expression)
            {
                if (CURRENT_CONFIGURATION->strict_typecheck)
                {
                    internal_error("Could not check expression-list '%s'\n", list_handler_in_buffer(a));
                }
                else
                {
                    std::cerr << ast_location(a)
                        << ": warning: internally generated expression list '" 
                        << list_handler_in_buffer(a) 
                        <<  "' is bad" 
                        << std::endl;
                }
            }
        }

        CURRENT_CONFIGURATION->scope_link = NULL;

        scope_link_set(scope_link, a, decl_context);

        return AST_t(a);
    }

    AST_t Source::parse_expression_list(AST_t ref_tree, TL::ScopeLink scope_link, ParseFlags parse_flags)
    {
#ifdef FORTRAN_SUPPORT
        FORTRAN_LANGUAGE()
        {
            internal_error("This function cannot be called in Fortran", 0);
        }
#endif

        return parse_generic_lang<AST_t>(ref_tree, scope_link, parse_flags,
                "@EXPRESSION-LIST@", finish_parse_expression_list_c_cxx);
    }

    static AST_t finish_parse_declaration_c_cxx(
            Source::ParseFlags parse_flags, 
            decl_context_t decl_context,
            scope_link_t* scope_link,
            AST a)
    {
        int parse_flags_int = (int)parse_flags;
        if ((parse_flags_int & Source::ALLOW_REDECLARATION) == Source::ALLOW_REDECLARATION)
        {
            decl_context.decl_flags = (decl_flags_t)((int)(decl_context.decl_flags) | DF_ALLOW_REDEFINITION);
        }

        if (a != NULL)
        {
            build_scope_declaration_sequence_with_scope_link(a, decl_context, scope_link);
        }

        // Set properly the context of the reference tree
        scope_link_set(scope_link, a, decl_context);

        return AST_t(a);
    }

    AST_t Source::parse_declaration(AST_t ref_tree, TL::ScopeLink scope_link, ParseFlags parse_flags)
    {
#ifdef FORTRAN_SUPPORT
        FORTRAN_LANGUAGE()
        {
            internal_error("This function cannot be called in Fortran", 0);
        }
#endif

        return parse_generic_lang<AST_t>(ref_tree, scope_link, parse_flags,
                "@DECLARATION@", finish_parse_declaration_c_cxx);
    }

    AST_t Source::parse_member(AST_t ref_tree, TL::ScopeLink scope_link, Symbol class_symb)
    {
        type_t* t = get_user_defined_type(class_symb.get_internal_symbol());

        return parse_member(ref_tree, scope_link, t);
    }

    AST_t Source::parse_member(AST_t /* ref_tree */, TL::ScopeLink scope_link, Type class_type)
    {
#ifdef FORTRAN_SUPPORT
        FORTRAN_LANGUAGE()
        {
            internal_error("This function cannot be called in Fortran", 0);
        }
#endif 
        // This is a special case
        std::string mangled_text = "@MEMBER@ " + this->get_source(true);

        mcxx_prepare_string_for_scanning(mangled_text.c_str());

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

    AST_t Source::parse_id_expression_wo_check(Scope scope, TL::ScopeLink scope_link, ParseFlags parse_flags)
    {
        std::string mangled_text = "@ID_EXPRESSION@ " + this->get_source(true);

        CXX_LANGUAGE()
        {
            mcxx_prepare_string_for_scanning(mangled_text.c_str());
        }
        C_LANGUAGE()
        {
            mc99_prepare_string_for_scanning(mangled_text.c_str());
        }
#ifdef FORTRAN_SUPPORT
        FORTRAN_LANGUAGE()
        {
            internal_error("This function cannot be called in Fortran", 0);
        }
#endif 

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
            running_error("Could not parse id-expression\n\n%s\n", 
                    format_source(this->get_source(true)).c_str());
        }
        
        // Get the scope and declarating context of the reference tree
        decl_context_t decl_context = scope.get_decl_context();

        // Set properly the context of the reference tree
        scope_link_set(scope_link._scope_link, a, decl_context);

        AST_t result(a);
        return result;
    }

    AST_t Source::parse_id_expression(Scope scope, TL::ScopeLink scope_link, ParseFlags parse_flags)
    {
        std::string mangled_text = "@ID_EXPRESSION@ " + this->get_source(true);
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
            running_error("Could not parse id-expression\n\n%s\n", 
                    format_source(this->get_source(true)).c_str());
        }
        
        // Get the scope and declarating context of the reference tree
        decl_context_t decl_context = scope.get_decl_context();

        enter_test_expression();
        check_for_expression(a, decl_context);
        leave_test_expression();

        // Set properly the context of the reference tree
        scope_link_set(scope_link._scope_link, a, decl_context);

        AST_t result(a);
        return result;
    }

    AST_t Source::parse_id_expression(AST_t ref_tree, TL::ScopeLink scope_link, ParseFlags parse_flags)
    {
        Scope scope = scope_link.get_scope(ref_tree);
        return parse_id_expression(scope, scope_link, parse_flags);
    }

    static Type finish_parse_type_c_cxx(
            Source::ParseFlags parse_flags, 
            decl_context_t decl_context,
            scope_link_t* scope_link,
            AST type_id)
    {
        // Set properly the context of the reference tree
        scope_link_set(scope_link, type_id, decl_context);

        type_t* type_info = NULL;
        gather_decl_spec_t gather_info;
        memset(&gather_info, 0, sizeof(gather_info));

        AST type_specifier_seq = ASTSon0(type_id);
        AST abstract_decl = ASTSon1(type_id);

        build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &type_info,
                decl_context);

        type_t* declarator_type = type_info;
        compute_declarator_type(abstract_decl, &gather_info, type_info, &declarator_type,
                decl_context);

        return Type(declarator_type);
    }

    Type Source::parse_type(AST_t ref_tree, TL::ScopeLink scope_link)
    {
#ifdef FORTRAN_SUPPORT
        FORTRAN_LANGUAGE()
        {
            internal_error("This function cannot be called in Fortran", 0);
        }
#endif 

        return parse_generic_lang<Type>(ref_tree, scope_link, Source::DEFAULT,
                "@TYPE@", finish_parse_type_c_cxx);
    }

    static ObjectList<Type> finish_parse_type_list_c_cxx(
            Source::ParseFlags parse_flags, 
            decl_context_t decl_context,
            scope_link_t* scope_link,
            AST type_specifier_seq_list)
    {
        ObjectList<Type> result;

        // Set properly the context of the reference tree
        scope_link_set(scope_link, type_specifier_seq_list, decl_context);

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

    ObjectList<Type> Source::parse_type_list(AST_t ref_tree, TL::ScopeLink scope_link)
    {
#ifdef FORTRAN_SUPPORT
        FORTRAN_LANGUAGE()
        {
            internal_error("This function cannot be called in Fortran", 0);
        }
#endif 

        return parse_generic_lang<ObjectList<Type> >(ref_tree, scope_link, Source::DEFAULT,
                "@TYPE-LIST@", finish_parse_type_list_c_cxx);
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

    static bool string_is_blank(const std::string& src)
    {
        for (std::string::const_iterator it = src.begin();
                it != src.end();
                it++)
        {
            if (*it != ' '
                    || *it != '\t')
            {
                return false;
            }
        }
        return true;
    }

    Source& Source::append_with_separator(const std::string& src, const std::string& separator)
    {
        if (!string_is_blank(src))
        {
            if (all_blanks())
            {
                append_text_chunk(src);
            }
            else
            {
                append_text_chunk(separator + src);
            }
        }

        return (*this);
    }

    Source& Source::append_with_separator(Source& src, const std::string& separator)
    {
        if (!src.all_blanks())
        {
            if (!all_blanks())
            {
                append_text_chunk(separator);
            }
            RefPtr<Source> ref_source = RefPtr<Source>(new Source(src));
            append_source_ref(SourceChunkRef(new SourceRef(ref_source)));
        }

        return (*this);
    }

    bool Source::empty() const
    {
        return all_blanks();
    }

    bool Source::all_blanks() const
    {
        if (_chunk_list->empty())
            return true;

        std::string str = this->get_source();
        return string_is_blank(str);
    }

    std::string comment(const std::string& str)
    {
        std::string result;

        result = "@-C-@" + str + "@-CC-@";
        return result;
    }

    std::string line_marker(const std::string& filename, int line)
    {
        std::stringstream ss;

       ss << "#line " << line;

       if (filename == "")
       {
           ss << "\n";
       }
       else
       {
           ss << "\"" << filename << "\"\n";
       }
       
       return ss.str();
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
