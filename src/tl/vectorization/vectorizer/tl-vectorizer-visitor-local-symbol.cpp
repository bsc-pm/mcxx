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

        struct LocalReferences : public Nodecl::ExhaustiveVisitor<void>
        {
            VectorizerEnvironment& _environment;
            TL::ObjectList<TL::Symbol> &local_symbols;
            LocalReferences(VectorizerEnvironment& environment_,
                    TL::ObjectList<TL::Symbol>& local_symbols_)
                : _environment(environment_),
                local_symbols(local_symbols_) { }

            TL::ObjectList<TL::Symbol> promoted_to_vector;

            void turn_local_to_vector(TL::Symbol tl_sym, Nodecl::NodeclBase n)
            {
                std::cerr << "MAKING |" << tl_sym.get_name() << "| at " << n.get_locus_str() << " VECTORIAL " << std::endl;
                TL::Type tl_sym_type = tl_sym.get_type().no_ref();

                TL::Type vector_type;

                if (tl_sym_type.is_bool())
                {
                    vector_type = TL::Type::get_mask_type(
                            _environment._vectorization_factor);
                }
                else if (tl_sym_type.is_integral_type()
                        || tl_sym_type.is_floating_type())
                {
                    vector_type = Utils::get_qualified_vector_to(
                            tl_sym_type, _environment._vectorization_factor);
                }
                else if (tl_sym_type.is_class()
                        && Utils::class_type_can_be_vectorized(tl_sym_type))
                {
                    vector_type = Utils::get_class_of_vector_fields(
                            tl_sym_type,
                            _environment._vectorization_factor);
                }
                else
                {
                    running_error("%s: error: cannot vectorize '%s' of type '%s'\n",
                            n.get_locus_str().c_str(),
                            n.prettyprint().c_str(),
                            tl_sym_type.get_declaration(tl_sym.get_scope(), "").c_str());
                }

                if (tl_sym.get_type().is_lvalue_reference())
                {
                    vector_type = vector_type.get_lvalue_reference_to();
                }

                tl_sym.set_type(vector_type);

                promoted_to_vector.insert(tl_sym);

                VECTORIZATION_DEBUG()
                {
                    fprintf(stderr,"VECTORIZER: '%s' TL::Symbol type promotion from '%s'"\
                            " to '%s'\n", n.prettyprint().c_str(),
                            tl_sym_type.get_simple_declaration(
                                n.retrieve_context(), "").c_str(),
                            vector_type.get_simple_declaration(
                                n.retrieve_context(), "").c_str());
                }
            }

            struct ReferenceInfo
            {
                bool is_uniform;
                bool is_linear;
                // bool is_iv;
            };

            ReferenceInfo request_info_of_reference(Nodecl::NodeclBase n)
            {
                ReferenceInfo result;
                result.is_uniform = Vectorizer::_vectorizer_analysis->is_uniform(
                        _environment._analysis_simd_scope, n, n);
                result.is_linear = Vectorizer::_vectorizer_analysis->
                    is_linear(_environment._analysis_simd_scope, n);
                // result.is_iv = Vectorizer::_vectorizer_analysis->
                //     is_induction_variable(_environment._analysis_simd_scope, n);

                return result;
            }

            virtual void visit (const Nodecl::Symbol& n)
            {
                TL::Symbol tl_sym = n.get_symbol();

                if (!local_symbols.contains(tl_sym))
                    return;
                if (promoted_to_vector.contains(tl_sym))
                    return;

                std::cerr << "SYMBOL ACCESS |" << n.prettyprint() << "| " << n.get_locus_str() << std::endl;

                TL::Type tl_sym_type = tl_sym.get_type().no_ref();
                bool is_mask = tl_sym_type.is_mask();

                ReferenceInfo ref_info = request_info_of_reference(n);
                if (!tl_sym_type.is_vector()
                        && !is_mask
                        && !ref_info.is_uniform
                        && !ref_info.is_linear
                        /* && !ref_info.is_induction_variable */)
                {
                    turn_local_to_vector(tl_sym, n);
                }
                else
                {
                    std::cerr << "SKIPPING |" << tl_sym.get_name() << "| at " << n.get_locus_str()
                        << " linear=" << ref_info.is_linear << " uniform=" << ref_info.is_uniform 
                        << std::endl;
                    // internal_error("Code unreachable", 0);
                    // if (!tl_sym.get_type().is_vector())
                    // {
                    //     VECTORIZATION_DEBUG()
                    //     {
                    //         fprintf(stderr,"VECTORIZER: '%s' is kept scalar", 
                    //                 nodecl_sym.prettyprint().c_str());

                    //         if (Vectorizer::_vectorizer_analysis->
                    //                 is_uniform(_environment._analysis_simd_scope,
                    //                     nodecl_sym, nodecl_sym))
                    //             fprintf(stderr," (uniform)");

                    //         if (Vectorizer::_vectorizer_analysis->
                    //                 is_linear(_environment._analysis_simd_scope,
                    //                     nodecl_sym))
                    //             fprintf(stderr," (linear)");

                    //         fprintf(stderr,"\n");
                    //     }
                    // }
                }
            }

            virtual void visit (const Nodecl::ClassMemberAccess& n)
            {
                // Get leftmost access
                // a.x.y -> a
                Nodecl::NodeclBase leftmost = n.get_lhs();
                while (leftmost.is<Nodecl::ClassMemberAccess>())
                {
                    leftmost = leftmost.as<Nodecl::ClassMemberAccess>().get_lhs();
                }
                // Too complex, give up
                if (!leftmost.is<Nodecl::Symbol>())
                {
                    std::cerr << "IN " << n.prettyprint() << " LEFTMOST IS NOT A NODECL_SYMBOL BUT A " << ast_print_node_type(leftmost.get_kind()) << std::endl;
                    return;
                }

                // Check the whole object (this is different to what we do
                // below, this walk will ignore the accessed member)
                walk(n.get_lhs());

                // Now resume with the current class member access
                TL::Symbol tl_sym = leftmost.get_symbol();

                if (!local_symbols.contains(tl_sym))
                    return;
                if (promoted_to_vector.contains(tl_sym))
                    return;

                std::cerr << "CLASS MEMBER ACCESS |" << n.prettyprint() << "| " << n.get_locus_str() << std::endl;

                TL::Type tl_sym_type = tl_sym.get_type().no_ref();

                // Here we take into account the accessed member
                ReferenceInfo ref_info = request_info_of_reference(n);
                if (!tl_sym_type.is_vector() // ???
                        && !ref_info.is_uniform
                        && !ref_info.is_linear
                        /* && !ref_info.is_induction_variable */)
                {
                    turn_local_to_vector(tl_sym, n);
                }
                else
                {
                    std::cerr << "SKIPPING |" << n.prettyprint() << "| at " << n.get_locus_str()
                        << " linear=" << ref_info.is_linear << " uniform=" << ref_info.is_uniform 
                        << std::endl;
                    // internal_error("Code unreachable", 0);
                }
            }
        };

        TL::ObjectList<TL::Symbol> local_symbols = Nodecl::Utils::get_local_symbols(n);

        LocalReferences local_refs(_environment, local_symbols);
        local_refs.walk(n);

        // objlist_nodecl_symbol_t local_symbol_list = 
        //     Nodecl::Utils::get_local_symbols_occurrences(n);

        // for(objlist_nodecl_symbol_t::iterator it = local_symbol_list.begin();
        //         it != local_symbol_list.end();
        //         it++)
        // {
        //     //std::cerr << "STUDYING: " << it->prettyprint() << " at line " << it->get_locus_str() << std::endl;
        //     const Nodecl::Symbol& nodecl_sym = *it;
        //     TL::Symbol tl_sym = nodecl_sym.get_symbol();
        //     TL::Type tl_sym_type = tl_sym.get_type();

        //     bool is_mask = tl_sym_type.is_mask();
        //     bool is_uniform = Vectorizer::_vectorizer_analysis->is_uniform(
        //             _environment._analysis_simd_scope, nodecl_sym, nodecl_sym);
        //     bool is_linear = Vectorizer::_vectorizer_analysis->
        //         is_linear(_environment._analysis_simd_scope, nodecl_sym);
        //     // bool is_iv = Vectorizer::_vectorizer_analysis->
        //     //     is_induction_variable(_environment._analysis_simd_scope, nodecl_sym);

        //     //std::cerr << "vector: " << tl_sym_type.is_vector() << std::endl;
        //     //std::cerr << "mask: " << tl_sym_type.is_mask() << std::endl;
        //     //std::cerr << "uniform: " << Vectorizer::_vectorizer_analysis->is_uniform(
        //     //        _environment._analysis_simd_scope, nodecl_sym, nodecl_sym) << std::endl;
        //     //std::cerr << "linear: " << Vectorizer::_vectorizer_analysis->
        //     //    is_linear(_environment._analysis_simd_scope, nodecl_sym) << std::endl;

        //     //std::cerr << "IV: " << Vectorizer::_vectorizer_analysis->
        //     //    is_induction_variable(_environment._analysis_simd_scope, nodecl_sym) << std::endl;
 

        //     if (!tl_sym_type.is_vector()
        //             && !is_mask
        //             && !is_uniform
        //             && !is_linear
        //             /* && !is_induction_variable */)
        //     {
        //         TL::Type vector_type;

        //         if (tl_sym_type.is_bool())
        //         {
        //             vector_type = TL::Type::get_mask_type(
        //                     _environment._vectorization_factor);
        //         }
        //         else if (tl_sym_type.is_class()
        //                 && Utils::class_type_can_be_vectorized(tl_sym_type))
        //         {
        //             vector_type = Utils::get_class_of_vector_fields(
        //                     tl_sym_type,
        //                     _environment._vectorization_factor);
        //         }
        //         else
        //         {
        //             vector_type = Utils::get_qualified_vector_to(
        //                 tl_sym_type, _environment._vectorization_factor);
        //         }

        //         tl_sym.set_type(vector_type);

        //         VECTORIZATION_DEBUG()
        //         {
        //             fprintf(stderr,"VECTORIZER: '%s' TL::Symbol type promotion from '%s'"
        //                     " to '%s'\n", nodecl_sym.prettyprint().c_str(),
        //                     tl_sym_type.get_simple_declaration(
        //                         n.retrieve_context(), "").c_str(),
        //                     vector_type.get_simple_declaration(
        //                         n.retrieve_context(), "").c_str());
        //         }
        //     }
        //     else
        //     {
        //         if (!tl_sym.get_type().is_vector())
        //         {
        //             VECTORIZATION_DEBUG()
        //             {
        //                 fprintf(stderr,"VECTORIZER: '%s' is kept scalar", 
        //                         nodecl_sym.prettyprint().c_str());

        //                 if (Vectorizer::_vectorizer_analysis->
        //                         is_uniform(_environment._analysis_simd_scope,
        //                             nodecl_sym, nodecl_sym))
        //                     fprintf(stderr," (uniform)");

        //                 if (Vectorizer::_vectorizer_analysis->
        //                         is_linear(_environment._analysis_simd_scope,
        //                             nodecl_sym))
        //                     fprintf(stderr," (linear)");

        //                 fprintf(stderr,"\n");
        //             }
        //         }
        //     }
        // }

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
