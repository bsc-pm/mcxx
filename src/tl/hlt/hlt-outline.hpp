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
#ifndef HLT_OUTLINE_HPP
#define HLT_OUTLINE_HPP

#include "hlt-transform.hpp"

#include "tl-objectlist.hpp"
#include "tl-symbol.hpp"
#include "tl-langconstruct.hpp"

namespace TL
{
    namespace HLT
    {
        struct LIBHLT_CLASS Outline : public BaseTransform
        {
            public:
                enum ParameterPassing
                {
                    INVALID = 0,
                    DO_NOT_PASS,
                    POINTER,
                    VALUE,
                    // Not implemented
                    /* REFERENCE */
                };

                struct ParameterInfo
                {
                    Symbol related_symbol;
                    ParameterPassing passing;
                    std::string outline_ref;
                };
            private:
                ScopeLink _sl;
                FunctionDefinition* _function_def;
                bool _packed_arguments;
                bool _do_not_embed;
                bool _use_nonlocal_scope;
                int _outline_num;
                bool _outline_performed;
                bool _overriden_outline_name;
                ParameterPassing _default_parameter_passing;
                ObjectList<ParameterInfo> _parameter_info;

                ObjectList<Statement> _outline_statements;

                Source _outline_name;
                Source _outlined_source;
                Source _additional_decls_source;

                Symbol _enclosing_function;

                Source _packed_argument_typename;

                bool _is_member;
                bool _is_inlined_member;
                bool _is_templated;
                ObjectList<TemplateHeader> _template_header;

                ObjectList<Symbol> _replaced_symbols;
                ObjectList<Symbol> _parameter_passed_symbols;

                void do_outline();
                void compute_outline_name(
                        Source &template_headers_fwd, 
                        Source &template_headers, 
                        Source &required_qualification,
                        Source &static_qualifier);
                void compute_referenced_entities();
                void compute_outlined_body(Source &outlined_body);

                void declare_members(Source template_headers);
                void fill_nonmember_forward_declarations(Source template_headers, Source &forward_declarations);
                void fill_member_forward_declarations(Source template_headers, Source &forward_declarations);

                Source get_parameter_declarations(Scope scope_of_decls);
                void compute_additional_declarations(Source template_headers, Scope scope_of_decls);

                void set_parameter_passing_if_not_set(Symbol sym);

                void embed_outline();

                static int _num_outlines;
            protected:
                virtual Source get_source();
            public:
                Outline(ScopeLink sl, Statement stmt);
                Outline(ScopeLink sl, ObjectList<Statement> stmt_list);

                Outline &use_packed_arguments();

                Outline &set_outline_name(const std::string& str);

                std::string get_outline_name();

                Outline& do_not_embed();

                void set_default_parameter_passing(ParameterPassing);
                void set_parameter_passing(Symbol, ParameterPassing);

                ParameterPassing get_parameter_passing(Symbol);

                std::string get_packing_struct_typename();

                ObjectList<Symbol> get_parameter_list();

                ~Outline();
        };
    }
}

#endif // HLT_OUTLINE_HPP
