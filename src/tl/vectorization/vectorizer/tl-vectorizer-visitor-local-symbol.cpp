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

#include "tl-vectorizer-visitor-local-symbol.hpp"

#include "tl-vectorization-analysis-interface.hpp"
#include "tl-vectorization-utils.hpp"
#include "tl-nodecl-utils.hpp"


namespace TL
{
namespace Vectorization
{
    VectorizerVisitorLocalSymbol::VectorizerVisitorLocalSymbol(
            VectorizerEnvironment& environment) : _environment(environment)
    {
    }

    /*
    void VectorizerVisitorLocalSymbol::symbol_type_promotion(
            const Nodecl::Symbol& n)
    {
        TL::Symbol tl_sym = n.get_symbol();
        TL::Type tl_sym_type = tl_sym.get_type().no_ref();
        TL::Type vector_type;

        //TL::Symbol
        if (tl_sym_type.is_mask())
        {
            vector_type = tl_sym_type;

            VECTORIZATION_DEBUG()
            {
                fprintf(stderr,"VECTORIZER: '%s' mask type vectorization "\
                        "(size %d)\n", n.prettyprint().c_str(),
                       vector_type.get_mask_num_elements());
            }
        }
        else if (tl_sym_type.is_scalar_type())
        {
            vector_type = Utils::get_qualified_vector_to(tl_sym_type,
                    _environment._vectorization_factor);

            VECTORIZATION_DEBUG()
            {
                fprintf(stderr,"VECTORIZER: TL::Type promotion '%s' from '%s'"\
                       " to '%s'\n", n.prettyprint().c_str(),
                       tl_sym_type.get_simple_declaration(
                           n.retrieve_context(), "").c_str(),
                       vector_type.get_simple_declaration(
                           n.retrieve_context(), "").c_str());
            }

            tl_sym.set_type(vector_type);
            tl_sym_type = tl_sym.get_type();
        }

        //Nodecl::Symbol
        Nodecl::Symbol new_sym = Nodecl::Symbol::make(tl_sym, n.get_locus());
        new_sym.set_type(tl_sym_type.get_lvalue_reference_to());

        n.replace(new_sym);
    }
    */

    void VectorizerVisitorLocalSymbol::vectorize_local_symbols_type(
            const Nodecl::NodeclBase& n)
    {
        VECTORIZATION_DEBUG()
        {
            fprintf(stderr, "VECTORIZER: -- Local Symbols --\n");
        }

        objlist_nodecl_symbol_t local_symbol_list = 
            Nodecl::Utils::get_local_symbols_occurrences(n);

        for(objlist_nodecl_symbol_t::iterator it = local_symbol_list.begin();
                it != local_symbol_list.end();
                it++)
        {
            //std::cerr << "STUDYING: " << it->prettyprint() << " at line " << it->get_locus_str() << std::endl;
            const Nodecl::Symbol& nodecl_sym = *it;
            TL::Symbol tl_sym = nodecl_sym.get_symbol();
            TL::Type tl_sym_type = tl_sym.get_type();

            if (!tl_sym_type.is_vector() && !tl_sym_type.is_mask() &&
                    !VectorizationAnalysisInterface::_vectorizer_analysis->
                    is_uniform(_environment._analysis_simd_scope,
                        nodecl_sym, nodecl_sym) &&
                    !VectorizationAnalysisInterface::_vectorizer_analysis->
                    is_linear(_environment._analysis_simd_scope,
                        nodecl_sym))
            {
                TL::Type vector_type;

                if (tl_sym_type.is_bool())
                {
                    vector_type = TL::Type::get_mask_type(
                            _environment._vectorization_factor);
                }
                else
                {
                    vector_type = Utils::get_qualified_vector_to(
                        tl_sym_type, _environment._vectorization_factor);
                }

                tl_sym.set_type(vector_type);

                VECTORIZATION_DEBUG()
                {
                    fprintf(stderr,"VECTORIZER: '%s' TL::Symbol type promotion from '%s'"\
                            " to '%s'\n", nodecl_sym.prettyprint().c_str(),
                            tl_sym_type.get_simple_declaration(
                                n.retrieve_context(), "").c_str(),
                            vector_type.get_simple_declaration(
                                n.retrieve_context(), "").c_str());
                }
            }
            else
            {
                if (!tl_sym.get_type().is_vector())
                    fprintf(stderr,"VECTORIZER: '%s' is uniform\n", 
                            nodecl_sym.prettyprint().c_str());
            }
        }

        VECTORIZATION_DEBUG()
        {
            fprintf(stderr, "VECTORIZER: -------------------\n");
        }
    }

    void VectorizerVisitorLocalSymbol::visit(const Nodecl::ForStatement& n)
    {
        vectorize_local_symbols_type(n);
    }

    void VectorizerVisitorLocalSymbol::visit(const Nodecl::WhileStatement& n)
    {
        vectorize_local_symbols_type(n);
    }

    void VectorizerVisitorLocalSymbol::visit(const Nodecl::FunctionCode& n)
    {
        vectorize_local_symbols_type(n);
    }
  
    Nodecl::NodeclVisitor<void>::Ret VectorizerVisitorLocalSymbol::
        unhandled_node(const Nodecl::NodeclBase& n)
    {
        internal_error("VectorizerVisitorLocalSymbol: Unexpected node %s.\n",
                ast_print_node_type(n.get_kind()));
        
        return Ret();
    }
}
}
