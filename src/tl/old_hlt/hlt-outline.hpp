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
        //! \addtogroup HLT High Level Transformations
        //! @{

        //! This class implements 'outlining' of parts of code
        /*! 
          Outlining means creating a function off a part of code (this is like
          the opposite of inlining). This class works on a given statement, or
          set of statements in the very same scope, and creates a function with
          that statement in it. It takes care of creating the required
          parameters and returns information about the newly created function.
          */
        struct LIBHLT_CLASS Outline : public BaseTransform
        {
            public:
                //! Kind of parameter pass
                enum ParameterPassing
                {
                    INVALID = 0,
                    //! This symbol must not be passed at all to the outlined function
                    DO_NOT_PASS,
                    //! This symbol must be passed by pointer
                    POINTER,
                    //! This symbol must be passed by value
                    VALUE,
                    // Not implemented
                    /* REFERENCE */
                };
            private:
                struct ParameterInfo
                {
                    Symbol related_symbol;
                    ParameterPassing passing;
                    std::string outline_ref;
                };

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
                bool _has_linkage_specifier;
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
                //! Creates an Outline object given a single statement
                Outline(ScopeLink sl, Statement stmt);
                //! Creates an Outline object given a set of statements
                /*!
                  \param sl ScopeLink
                  \param stmt_list This is a list of statements that should share the very same scope
                 */
                Outline(ScopeLink sl, ObjectList<Statement> stmt_list);

                //! Enables packed arguments
                /*! 
                  Packed arguments will pack all the arguments into a single
                  structure and pass the structure by pointer 
                 */
                Outline &use_packed_arguments();

                //! Sets the outline function name
                /*! 
                  %Outline function will have a computed name unless it is set using this function 
                 */
                Outline &set_outline_name(const std::string& str);

                //! Returns the outline function name
                /*!
                  If set_outline_name was called, the name set on the call will be returned here, otherwise
                  this will be the computed name of the outline function
                 */
                std::string get_outline_name();

                //! Disables embedding newly created outline
                /*!
                  This disables automatic embedding of the outline. This is discouraged since
                  sometimes it is not obvious where the outline must be placed.
                 */
                Outline& do_not_embed();

                //! Sets a default parameter pass mode
                /*!
                  For all symbols without an explicit pass mode, this will be the pass mode used.
                  If no default pass mode is defined, it will be POINTER
                  \sa ParameterPassing
                 */
                void set_default_parameter_passing(ParameterPassing passing);
                //! Sets a parameter pass mode for a given Symbol
                /*!
                  \param symbol The symbol whose pass mode is being set
                  \param passing Passing mode
                  \sa ParameterPassing
                 */
                void set_parameter_passing(Symbol symbol, ParameterPassing passing);

                //! Returns the pass mode for a given symbol
                /*!
                  If the symbol had not a pass mode set beforehand, the default
                  passing mode will be returned.
                 */
                ParameterPassing get_parameter_passing(Symbol);

                //! Returns the typename of the packing struct
                /*!
                  If packed arguments are used, this is the name of the packed type
                  as it should be used when filling it in point where the outline
                  should be called.
                 */
                std::string get_packing_struct_typename();

                //! Returns the parameters expected by the outline function
                /*! 
                  If packed arguments are used, this list contain the fields, in
                  declaration order, of the struct.
                  */
                ObjectList<Symbol> get_parameter_list();

                ~Outline();
        };
        //! @}
    }
}

#endif // HLT_OUTLINE_HPP
