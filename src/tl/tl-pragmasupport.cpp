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




#include <typeinfo>
#include <cctype>
#include "cxx-utils.h"
#include "cxx-diagnostic.h"
#include "tl-pragmasupport.hpp"

namespace TL
{
    ObjectList<std::string> NullClauseTokenizer::tokenize(const std::string& str) const
    {
        ObjectList<std::string> result;
        result.append(str);
        return result;
    }

    ExpressionTokenizer::ExpressionTokenizer(const char separator)
        : ClauseTokenizer( ), _separator(separator)
    {}
    
    ObjectList<std::string> ExpressionTokenizer::tokenize(const std::string& str) const
    {
        int bracket_nesting = 0;
        ObjectList<std::string> result;

        std::string temporary("");
        for (std::string::const_iterator it = str.begin();
                it != str.end();
                it++)
        {
            const char & c(*it);

            if (c == _separator 
                    && bracket_nesting == 0
                    && temporary != "")
            {
                result.append(temporary);
                temporary = "";
            }
            else
            {
                if (c == '('
                        || c == '{'
                        || c == '[')
                {
                    bracket_nesting++;
                }
                else if (c == ')'
                        || c == '}'
                        || c == ']')
                {
                    bracket_nesting--;
                }
                temporary += c;
            }
        }

        if (temporary != "")
        {
            result.append(temporary);
        }

        return result;
    }

    ExpressionTokenizerTrim::ExpressionTokenizerTrim(const char separator)
        : ExpressionTokenizer(separator)
    {}
    
    ObjectList<std::string> ExpressionTokenizerTrim::tokenize(const std::string& str) const
    {
        ObjectList<std::string> result;
        result = ExpressionTokenizer::tokenize(str);

        std::transform(result.begin(), result.end(), result.begin(), trimExp);

        return result;
    }

    std::string ExpressionTokenizerTrim::trimExp (const std::string &str) 
    {
        std::string::size_type first = str.find_first_not_of(" \t");
        std::string::size_type last = str.find_last_not_of(" \t");

        if (first == std::string::npos)
        {
            // The string is only blanks
            return std::string();
        }
        else
        {
            return str.substr(first, last - first + 1);
        }
    }

    // Initialize here the warnings to the dispatcher
    PragmaCustomCompilerPhase::PragmaCustomCompilerPhase()
          : _ignore_template_functions(false)
    {
    }

    void PragmaCustomCompilerPhase::pre_run(DTO& data_flow)
    {
        // Do nothing
    }

    void PragmaCustomCompilerPhase::run(DTO& data_flow)
    {
        Nodecl::NodeclBase node = *std::static_pointer_cast<Nodecl::NodeclBase>(data_flow["nodecl"]);

        this->walk(node);
    }

    void PragmaCustomCompilerPhase::walk(Nodecl::NodeclBase& node)
    {
        PragmaVisitor visitor(_pragma_map_dispatcher, _ignore_template_functions);
        visitor.walk(node);
    }

    void PragmaCustomCompilerPhase::register_directive(
            const std::string& pragma_handled,
            const std::string& str)
    {
        register_new_directive(CURRENT_CONFIGURATION, pragma_handled.c_str(), str.c_str(), 0, 0);
    }

    void PragmaCustomCompilerPhase::register_construct(
            const std::string& pragma_handled,
            const std::string& str,
            bool bound_to_statement)
    {
        if (IS_FORTRAN_LANGUAGE)
        {
            register_new_directive(CURRENT_CONFIGURATION, pragma_handled.c_str(), str.c_str(), 1, bound_to_statement);
        }
        else
        {
            register_new_directive(CURRENT_CONFIGURATION, pragma_handled.c_str(), str.c_str(), 1, 0);
        }
    }

    void PragmaCustomCompilerPhase::warning_pragma_unused_clauses(bool warning)
    {
        // _pragma_dispatcher.set_warning_clauses(warning);
    }

    std::string PragmaCustomCompilerPhase::remove_separators_of_directive(const std::string& str)
    {
        std::string result(str);
        // Remove '|'
        result.erase(std::remove(result.begin(), result.end(), '|'), result.end());

        return result;
    }

    SinglePragmaMapDispatcher& PragmaCustomCompilerPhase::dispatcher(const std::string &pragma_handled)
    {
        return _pragma_map_dispatcher[pragma_handled];
    }

    std::string PragmaClauseArgList::get_raw_arguments() const
    {
        std::string result;

        for (Nodecl::List::const_iterator it = this->begin();
                it != this->end();
                it++)
        {
            result += it->get_text();
        }

        return result;
    }

    ObjectList<std::string> PragmaClauseArgList::get_tokenized_arguments(const ClauseTokenizer& tokenizer) const
    {
        std::string raw = this->get_raw_arguments();
        return tokenizer.tokenize(raw);
    }

    ObjectList<Nodecl::NodeclBase> PragmaClauseArgList::get_arguments_as_expressions(ReferenceScope ref_scope, 
            const ClauseTokenizer& tokenizer) const
    {
        ObjectList<std::string> str_list = this->get_tokenized_arguments(tokenizer);
        ObjectList<Nodecl::NodeclBase> result;

        for (ObjectList<std::string>::iterator it = str_list.begin();
                it != str_list.end();
                it++)
        {
            if (it->empty())
                continue;

            Source src;
            src << "#line " << this->get_line() << " \"" << this->get_filename() << "\"\n"
                << pad_to_column(this->get_column()) << *it;

            Nodecl::NodeclBase current_expr = src.parse_expression(ref_scope);

            result.append(current_expr);
        }

        return result;
    }

    ObjectList<Nodecl::NodeclBase> PragmaClauseArgList::get_arguments_as_expressions(const ClauseTokenizer& tokenizer) const
    {
        return get_arguments_as_expressions(this->retrieve_context(), tokenizer);
    }

    std::string PragmaCustomSingleClause::get_raw_arguments() const
    {
        return PragmaClauseArgList(this->get_arguments().as<Nodecl::List>()).get_raw_arguments();
    }

    ObjectList<std::string> PragmaCustomSingleClause::get_tokenized_arguments(const ClauseTokenizer& tokenizer) const
    {
        return PragmaClauseArgList(this->get_arguments().as<Nodecl::List>()).get_tokenized_arguments(tokenizer);
    }

    ObjectList<Nodecl::NodeclBase> PragmaCustomSingleClause::get_arguments_as_expressions(ReferenceScope ref_scope, 
            const ClauseTokenizer& tokenizer) const
    {
        return PragmaClauseArgList(this->get_arguments().as<Nodecl::List>()).get_arguments_as_expressions(ref_scope, tokenizer);
    }

    ObjectList<Nodecl::NodeclBase> PragmaCustomSingleClause::get_arguments_as_expressions(const ClauseTokenizer& tokenizer) const
    {
        return get_arguments_as_expressions(this->retrieve_context(), tokenizer);
    }

    void PragmaCustomSingleClause::mark_as_used()
    {
        this->set_type(TL::Type::get_void_type());
    }

    void PragmaCustomSingleClause::mark_as_unused()
    {
        this->set_type(TL::Type());
    }

    bool PragmaCustomSingleClause::is_marked_as_used() const
    {
        return this->get_type().is_valid();
    }

    bool PragmaCustomSingleClause::is_marked_as_unused() const
    {
        return !this->is_marked_as_used();
    }

    ObjectList<std::string> PragmaCustomClause::get_raw_arguments() const
    {
        ObjectList<std::string> result;
        for (PragmaCustomClauseList::const_iterator it = _pragma_clauses.begin();
                it != _pragma_clauses.end();
                it++)
        {
            result.append(TL::PragmaCustomSingleClause(*it).get_raw_arguments());
        }
        return result;
    }

    PragmaCustomClause::PragmaCustomClause(Nodecl::PragmaCustomLine pragma_line,
            ObjectList<Nodecl::PragmaCustomClause> pragma_clauses)
        : _pragma_line(pragma_line),
          _pragma_clauses(pragma_clauses)
    {
        mark_as_used();
    }

    void PragmaCustomClause::mark_as_used()
    {
        for (PragmaCustomClauseList::const_iterator it = _pragma_clauses.begin();
                it != _pragma_clauses.end();
                it++)
        {
            TL::PragmaCustomSingleClause(*it).mark_as_used();
        }
    }

    void PragmaCustomClause::mark_as_unused()
    {
        for (PragmaCustomClauseList::const_iterator it = _pragma_clauses.begin();
                it != _pragma_clauses.end();
                it++)
        {
            TL::PragmaCustomSingleClause(*it).mark_as_unused();
        }
    }

    bool PragmaCustomClause::is_defined() const
    {
        return !_pragma_clauses.empty();
    }

    bool PragmaCustomParameter::is_defined() const
    {
        return !this->empty();
    }

    void PragmaCustomParameter::mark_as_used()
    {
        if (is_defined())
        {
            this->set_type(TL::Type::get_void_type());
        }
    }

    void PragmaCustomParameter::mark_as_unused()
    {
        if (is_defined())
        {
            this->set_type(TL::Type());
        }
    }

    bool PragmaCustomParameter::is_marked_as_used() const
    {
        // If not defined it has somehow been "used"
        if (!is_defined())
            return true;
        return this->get_type().is_valid();
    }

    bool PragmaCustomParameter::is_marked_as_unused() const
    {
        return !this->is_marked_as_used();
    }

    bool PragmaCustomClause::is_singleton() const
    {
        return _pragma_clauses.size() == 1;
    }

    ObjectList<std::string> PragmaCustomClause::get_tokenized_arguments(const ClauseTokenizer& tokenizer) const
    {
        ObjectList<std::string> result;
        for (PragmaCustomClauseList::const_iterator it = _pragma_clauses.begin();
                it != _pragma_clauses.end();
                it++)
        {
            result.append(TL::PragmaCustomSingleClause(*it).get_tokenized_arguments(tokenizer));
        }
        return result;
    }

    ObjectList<Nodecl::NodeclBase> PragmaCustomClause::get_arguments_as_expressions(
        ReferenceScope ref_scope, 
        const ClauseTokenizer & tokenizer) const
    {
        ObjectList<Nodecl::NodeclBase> result;
        for (PragmaCustomClauseList::const_iterator it = _pragma_clauses.begin();
                it != _pragma_clauses.end();
                it++)
        {
            result.append(TL::PragmaCustomSingleClause(*it).get_arguments_as_expressions(ref_scope, tokenizer));
        }
        return result;
    }

    std::string PragmaCustomClause::get_locus_str() const
    {
        return _pragma_line.get_locus_str();
    }

    const locus_t* PragmaCustomClause::get_locus() const
    {
        return _pragma_line.get_locus();
    }

    ObjectList<Nodecl::NodeclBase> PragmaCustomClause::get_arguments_as_expressions(const ClauseTokenizer& tokenizer) const
    {
        return this->get_arguments_as_expressions(_pragma_line.retrieve_context(), tokenizer);
    }

    TL::PragmaCustomClause PragmaCustomLine::get_clause(const ObjectList<std::string>& aliased_names,
            const ObjectList<std::string>& deprecated_names) const
    {
        ObjectList<Nodecl::PragmaCustomClause> result;
        ObjectList<TL::PragmaCustomSingleClause> clauses = this->get_all_clauses();

        for (ObjectList<TL::PragmaCustomSingleClause>::iterator it = clauses.begin();
                it != clauses.end();
                it++)
        {
            TL::PragmaCustomSingleClause &clause = *it;

            if (aliased_names.contains(clause.get_text())
                    || deprecated_names.contains(clause.get_text()))
            {
                result.append(clause);
            }
            if (deprecated_names.contains(clause.get_text()))
            {
                warn_printf_at(clause.get_locus(),
                        "clause '%s' is deprecated. Instead use '%s'\n",
                        clause.get_text().c_str(),
                        aliased_names[0].c_str());
            }
        }

        return PragmaCustomClause(*this, result);
    }

    TL::PragmaCustomClause PragmaCustomLine::get_clause(const ObjectList<std::string>& aliased_names) const
    {
        return get_clause(aliased_names, ObjectList<std::string>());
    }

    TL::PragmaCustomClause PragmaCustomLine::get_clause(const std::string &name, 
            const ObjectList<std::string>& deprecated_names) const
    {
        ObjectList<std::string> set;
        set.append(name);
        return get_clause(set, deprecated_names);
    }
    
    TL::PragmaCustomClause PragmaCustomLine::get_clause(const std::string &name, const std::string& deprecated_name) const
    {
        ObjectList<std::string> deprecated_names;
        deprecated_names.append(deprecated_name);

        return get_clause(name, deprecated_names);
    }

    TL::PragmaCustomClause PragmaCustomLine::get_clause(const std::string &name) const
    {
        return get_clause(name, ObjectList<std::string>());
    }

    ObjectList<Nodecl::PragmaCustomClause> PragmaCustomLine::get_all_clauses_nodes() const
    {
        ObjectList<Nodecl::PragmaCustomClause> result;
        Nodecl::List clauses = this->get_clauses().as<Nodecl::List>();
        for (Nodecl::List::iterator it = clauses.begin(); 
                it != clauses.end();
                it++)
        {
            Nodecl::PragmaCustomClause clause = it->as<Nodecl::PragmaCustomClause>();
            result.append(clause);
        }

        Nodecl::List end_clauses = this->get_end_clauses().as<Nodecl::List>();
        for (Nodecl::List::iterator it = end_clauses.begin(); 
                it != end_clauses.end();
                it++)
        {
            Nodecl::PragmaCustomClause clause = it->as<Nodecl::PragmaCustomClause>();
            result.append(clause);
        }

        return result;
    }

    ObjectList<TL::PragmaCustomSingleClause> PragmaCustomLine::get_all_clauses() const
    {
        ObjectList<Nodecl::PragmaCustomClause> nodes = this->get_all_clauses_nodes();
        ObjectList<TL::PragmaCustomSingleClause> result;
        for (ObjectList<Nodecl::PragmaCustomClause>::iterator it = nodes.begin();
                it != nodes.end();
                it++)
        {
            result.append(*it);
        }

        return result;
    }

    ObjectList<std::string> PragmaCustomLine::get_all_clause_names() const
    {
        ObjectList<Nodecl::PragmaCustomClause> nodes = this->get_all_clauses_nodes();

        ObjectList<std::string> clauses_strings = nodes
            .map<std::string>(&Nodecl::NodeclBase::get_text);

        return clauses_strings;
    }

    PragmaCustomParameter PragmaCustomLine::get_parameter() const
    {
        return PragmaCustomParameter(this->get_parameters().as<Nodecl::List>());
    }

    PragmaCustomParameter PragmaCustomLine::get_parameter_no_mark_used() const
    {
        return PragmaCustomParameter(this->get_parameters().as<Nodecl::List>(), (int)0);
    }

    void PragmaCustomLine::diagnostic_unused_clauses() const
    {
        PragmaCustomParameter param = this->get_parameter_no_mark_used();
        if (param.is_marked_as_unused())
        {
            warn_printf_at(param.get_locus(),
                    "ignoring parameter '%s' of this pragma\n",
                    param.get_raw_arguments().c_str());
        }

        ObjectList<Nodecl::PragmaCustomClause> nodes = this->get_all_clauses_nodes();
        for (ObjectList<Nodecl::PragmaCustomClause>::iterator it = nodes.begin();
                it != nodes.end();
                it++)
        {
            TL::PragmaCustomSingleClause current_clause(*it);

            if (current_clause.is_marked_as_unused())
            {
                warn_printf_at(it->get_locus(),
                        "ignoring clause '%s'\n",
                        current_clause.get_text().c_str());
            }
        }
    }

    TL::PragmaCustomLine TL::PragmaCustomDeclaration::get_pragma_line() const
    {
        return TL::PragmaCustomLine(this->Nodecl::PragmaCustomDeclaration::get_pragma_line().as<Nodecl::PragmaCustomLine>());
    }

    TL::PragmaCustomLine TL::PragmaCustomStatement::get_pragma_line() const
    {
        return TL::PragmaCustomLine(this->Nodecl::PragmaCustomStatement::get_pragma_line().as<Nodecl::PragmaCustomLine>());
    }

    TL::PragmaCustomLine TL::PragmaCustomDirective::get_pragma_line() const
    {
        return TL::PragmaCustomLine(this->Nodecl::PragmaCustomDirective::get_pragma_line().as<Nodecl::PragmaCustomLine>());
    }

    ReferenceScope TL::PragmaCustomDirective::get_context_of_declaration() const
    {
        return ReferenceScope(this->Nodecl::PragmaCustomDirective::get_context_of_decl().as<ReferenceScope>());
    }
    
    bool PragmaUtils::is_pragma_construct(const std::string& prefix,
            const std::string& pragma_name,
            Nodecl::NodeclBase n)
    {
        Nodecl::PragmaCustomLine pragma_line;
        std::string current_prefix;
        if (n.is<Nodecl::PragmaCustomDirective>())
        {
            current_prefix = n.get_text();
            pragma_line = n.as<Nodecl::PragmaCustomDirective>().get_pragma_line().as<Nodecl::PragmaCustomLine>();
        }
        else if (n.is<Nodecl::PragmaCustomStatement>())
        {
            current_prefix = n.get_text();
            pragma_line = n.as<Nodecl::PragmaCustomStatement>().get_pragma_line().as<Nodecl::PragmaCustomLine>();
        }
        else if (n.is<Nodecl::PragmaCustomDeclaration>())
        {
            current_prefix = n.get_text();
            pragma_line = n.as<Nodecl::PragmaCustomDeclaration>().get_pragma_line().as<Nodecl::PragmaCustomLine>();
        }
        else
        {
            return false;
        }

        return (current_prefix == prefix
                && pragma_line.get_text() == pragma_name);
    }

    bool PragmaUtils::is_pragma_construct(const std::string& prefix,
            Nodecl::NodeclBase n)
    {
        Nodecl::PragmaCustomLine pragma_line;
        std::string current_prefix;
        if (n.is<Nodecl::PragmaCustomDirective>())
        {
            current_prefix = n.get_text();
            pragma_line = n.as<Nodecl::PragmaCustomDirective>().get_pragma_line().as<Nodecl::PragmaCustomLine>();
        }
        else if (n.is<Nodecl::PragmaCustomStatement>())
        {
            current_prefix = n.get_text();
            pragma_line = n.as<Nodecl::PragmaCustomStatement>().get_pragma_line().as<Nodecl::PragmaCustomLine>();
        }
        else if (n.is<Nodecl::PragmaCustomDeclaration>())
        {
            current_prefix = n.get_text();
            pragma_line = n.as<Nodecl::PragmaCustomDeclaration>().get_pragma_line().as<Nodecl::PragmaCustomLine>();
        }
        else
        {
            return false;
        }

        return (current_prefix == prefix);
    }
    
    Nodecl::PragmaCustomLine TL::PragmaCustomClause::get_pragma_line() const
    {
        return _pragma_line;
    }
}
