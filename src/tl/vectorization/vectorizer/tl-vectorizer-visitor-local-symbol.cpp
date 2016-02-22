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

#include "cxx-diagnostic.h"

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
                    _environment._vec_factor);

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

        struct LocalReferences : public Nodecl::ExhaustiveVisitor<bool>
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
                TL::Type tl_sym_type = tl_sym.get_type().no_ref();

                TL::Type vector_type;

                if (tl_sym_type.is_bool())
                {
                    vector_type = TL::Type::get_mask_type(
                            _environment._vec_factor);
                }
                else if (tl_sym_type.is_integral_type()
                        || tl_sym_type.is_floating_type())
                {
                    vector_type = Utils::get_qualified_vector_to(
                        tl_sym_type,
                        _environment._vec_isa_desc.get_vec_factor_for_type(
                            tl_sym_type, _environment._vec_factor));
                }
                else if (tl_sym_type.is_class()
                        && Utils::class_type_can_be_vectorized(tl_sym_type))
                {
                    bool is_new = false;
                    vector_type = Utils::get_class_of_vector_fields_for_isa(
                            tl_sym_type,
                            _environment._vec_factor,
                            is_new,
                            _environment._vec_isa_desc);
                    if (is_new
                            && IS_CXX_LANGUAGE)
                    {
                        VECTORIZATION_DEBUG()
                        {
                            std::cerr << "NEW CLASS -> " << vector_type.get_symbol().get_qualified_name() << std::endl;
                            std::cerr << "(1) ENV = " << &_environment << std::endl;
                        }
                        _environment._vectorized_classes.append(
                                std::make_pair(tl_sym_type, vector_type)
                                );
                    }
                    VECTORIZATION_DEBUG()
                    {
                        std::cerr << "CLASS -> " << vector_type.get_symbol().get_qualified_name() << std::endl;
                    }
                }
                else
                {
                    fatal_printf_at(
                            n.get_locus(),
                            "cannot vectorize '%s' of type '%s'\n",
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
                            " to '%s' (%s)\n", tl_sym.get_name().c_str(),
                            tl_sym_type.get_simple_declaration(
                                n.retrieve_context(), "").c_str(),
                            vector_type.get_simple_declaration(
                                n.retrieve_context(), "").c_str(),
                            n.get_locus_str().c_str());
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

            virtual bool join_list(TL::ObjectList<bool>& list)
            {
                bool result = false;
                for (const auto& it : list)
                {
                    result |= it;
                }

                return result;
            }

            virtual bool visit(const Nodecl::Symbol& n)
            {
                TL::Symbol tl_sym = n.get_symbol();

                if (!local_symbols.contains(tl_sym))
                    return false;
                if (promoted_to_vector.contains(tl_sym))
                    return true;

                TL::Type tl_sym_type = tl_sym.get_type().no_ref();
                bool is_mask = tl_sym_type.is_mask();

                bool result;
                ReferenceInfo ref_info = request_info_of_reference(n);
                if (!tl_sym_type.is_vector()
                        && !Utils::is_class_of_vector_fields(tl_sym_type)
                        && !is_mask
                        && !ref_info.is_uniform
                        && !ref_info.is_linear
                        /* && !ref_info.is_induction_variable */)
                {
                    turn_local_to_vector(tl_sym, n);
                    result = true;
                }
                else
                {
                    VECTORIZATION_DEBUG()
                    {
                        std::cerr << "SKIPPING |" << tl_sym.get_name() << "| at " << n.get_locus_str()
                            << " linear=" << ref_info.is_linear << " uniform=" << ref_info.is_uniform 
                            << std::endl;
                    }
                    
                    result = false;
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

                return result;
            }

            virtual bool visit(const Nodecl::ClassMemberAccess& n)
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
                    VECTORIZATION_DEBUG()
                    {
                        std::cerr << "IN " << n.prettyprint() 
                            << " LEFTMOST IS NOT A NODECL_SYMBOL BUT A " 
                            << ast_print_node_type(leftmost.get_kind()) << std::endl;
                    }
                    return false;
                }

                // Check the whole object (this is different to what we do
                // below, this walk will ignore the accessed member)
                walk(n.get_lhs());

                // Now resume with the current class member access
                TL::Symbol tl_sym = leftmost.get_symbol();

                if (!local_symbols.contains(tl_sym))
                    return false;
                if (promoted_to_vector.contains(tl_sym))
                    return true;

                VECTORIZATION_DEBUG()
                {
                    std::cerr << "CLASS MEMBER ACCESS |" << n.prettyprint() << "| " << n.get_locus_str() << std::endl;
                }

                TL::Type tl_sym_type = tl_sym.get_type().no_ref();

                bool result;
                // Here we take into account the accessed member
                ReferenceInfo ref_info = request_info_of_reference(n);
                if (!tl_sym_type.is_vector() // ???
                        && !Utils::is_class_of_vector_fields(tl_sym_type)
                        && !ref_info.is_uniform
                        && !ref_info.is_linear
                        /* && !ref_info.is_induction_variable */)
                {
                    turn_local_to_vector(tl_sym, n);
                    result = true;
                }
                else
                {
                    VECTORIZATION_DEBUG()
                    {
                        std::cerr << "SKIPPING |" << n.prettyprint() << "| at " << n.get_locus_str()
                            << " linear=" << ref_info.is_linear << " uniform=" << ref_info.is_uniform 
                            << std::endl;
                    }
                    // internal_error("Code unreachable", 0);
                    result = false;
                }

                return result;
            }

            virtual bool visit(const Nodecl::ObjectInit& n)
            {
                TL::Symbol sym = n.get_symbol();
                TL::Type scalar_type = sym.get_type().no_ref();

                bool result;
                if (!scalar_type.is_vector()
                        && !Utils::is_class_of_vector_fields(scalar_type))
                {
                    Nodecl::NodeclBase init = sym.get_value();

                    // Vectorizing initialization
                    if(!init.is_null())
                    {
                        result = walk(init);

                        if (result)
                        {
                            turn_local_to_vector(sym, n);
                        }
                    }
                }
                else
                {
                    result = true;
                }

                return result;
            }

            virtual bool visit(const Nodecl::FunctionCall& n)
            {
                TL::Type function_type = n.get_called().get_type().no_ref();

                if (!function_type.is_valid())
                {
                    // constructors...
                    walk(n.get_arguments());
                    return false; //?
                }

                if (function_type.is_pointer())
                    function_type = function_type.points_to();

                bool has_ellipsis = false;
                TL::ObjectList<TL::Type> params = function_type.parameters(has_ellipsis);
                Nodecl::List arguments = n.get_arguments().as<Nodecl::List>();

                TL::ObjectList<TL::Type>::iterator it_param = params.begin();
                Nodecl::List::iterator it_arg = arguments.begin();

                TL::Type function_target_type = n.get_type().no_ref();

                // Get the best vector version of the function available
                Nodecl::NodeclBase best_version_node;
                if (n.get_called().is<Nodecl::Symbol>())
                {
                    best_version_node = vec_func_versioning.get_best_version(
                        n.get_called().get_symbol(),
                        _environment._vec_isa_desc.get_id(),
                        _environment._vec_factor,
                        /* mask */ false);
                }

                TL::Symbol vector_function;
                if (!best_version_node.is_null())
                    vector_function = best_version_node.get_symbol();

                bool result = false;
                for (;
                        it_param != params.end() && it_arg != arguments.end();
                        it_param++, it_arg++)
                {
                    if (it_param->is_any_reference())
                    {
                        if (it_arg->is<Nodecl::Symbol>())
                        {
                            TL::Symbol tl_sym = it_arg->get_symbol();
                            if (!local_symbols.contains(tl_sym))
                                continue;
                            if (promoted_to_vector.contains(tl_sym))
                            {
                                result = true;
                                continue;
                            }

                            if (vector_function.is_valid())
                            {
                                TL::ObjectList<TL::Type> vector_params_list = vector_function.get_type().parameters();
                                int vector_param_type_idx = it_param - params.begin();
                                ERROR_CONDITION(vector_param_type_idx >= (int)vector_params_list.size(),
                                        "Invalid index", 0);
                                TL::Type vector_param_type = vector_params_list[vector_param_type_idx];
                                vector_param_type = vector_param_type.no_ref().get_unqualified_type();

                                if (vector_param_type.is_vector()
                                        || Utils::is_class_of_vector_fields(vector_param_type))
                                {
                                    turn_local_to_vector(tl_sym, n);
                                    result = true;
                                }
                                else
                                {
                                    // Do nothing
                                }
                            }
                            else
                            {
                                error_printf_at(
                                        it_arg->get_locus(),
                                        "argument '%s' bound to a reference of type '%s' but there is no vector function for it\n",
                                        it_arg->prettyprint().c_str(),
                                        it_param->get_declaration(it_arg->retrieve_context(), "").c_str());
                            }
                        }
                        else
                        {
                            error_printf_at(
                                    it_arg->get_locus(),
                                    "argument '%s' bound to a reference of type '%s' is too complex\n",
                                    it_arg->prettyprint().c_str(),
                                    it_param->get_declaration(it_arg->retrieve_context(), "").c_str());
                        }
                    }
                    else
                    {
                        // This is a usual argument evaluation
                        result = walk(*it_arg);
                    }
                }

                if (has_ellipsis)
                {
                    // In the unlikely case of ellipsis function calls,
                    // traverse the arguments as normal evaluations
                    for (; it_arg != arguments.end(); it_arg++)
                    {
                        result = walk(*it_arg);
                    }
                }

                return result;
            }
        };

        TL::ObjectList<TL::Symbol> local_symbols = Nodecl::Utils::get_local_symbols(n);

        VECTORIZATION_DEBUG()
        {
            fprintf(stderr, "VECTORIZER: Local symbols found:\n");
            for(const auto& lsym : local_symbols)
            {
                fprintf(stderr, "%s (%s), ", lsym.get_name().c_str(),
                        lsym.get_locus_str().c_str());
            }
            fprintf(stderr, "\n");
        }

        LocalReferences local_refs(_environment, local_symbols);
        local_refs.walk(n);

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
