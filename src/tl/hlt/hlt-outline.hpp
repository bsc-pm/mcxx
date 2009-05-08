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
            private:
                bool _packed_arguments;
                bool _use_nonlocal_scope;
                int _outline_num;

                ObjectList<Statement> _outline_statements;

                Source _outline_name;
                Source _outlined_source;
                Source _additional_decls_source;

                Symbol _enclosing_function;

                Source _packed_argument_typename;

                bool _is_member;
                bool _is_inlined_member;
                bool _is_templated;
                ObjectList<AST_t> _template_header;
                ObjectList<Symbol> _referenced_symbols;

                void do_outline();
                void compute_outline_name(Source &template_headers, Source &required_qualification);
                void compute_referenced_entities(Source &outline_parameters);
                void compute_outlined_body(Source &outlined_body);

                static int _num_outlines;
            protected:
                virtual Source get_source();
            public:
                Outline(Statement stmt);
                Outline(ObjectList<Statement> stmt_list);

                Outline &use_packed_arguments();

                Source get_additional_declarations();
        };
    }
}

#endif // HLT_OUTLINE_HPP
