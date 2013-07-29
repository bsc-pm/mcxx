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

#include "tl-omp-simd.hpp"
#include "tl-omp.hpp"
#include "tl-nodecl-utils.hpp"

using namespace TL::Vectorization;

namespace TL { 
    namespace OpenMP {

        Simd::Simd()
            : PragmaCustomCompilerPhase("omp-simd"),  
            _simd_enabled(false), _svml_enabled(false), _fast_math_enabled(false), _mic_enabled(false)
        {
            set_phase_name("Vectorize OpenMP SIMD parallel IR");
            set_phase_description("This phase vectorize the OpenMP SIMD parallel IR");

            register_parameter("simd_enabled",
                    "If set to '1' enables simd constructs, otherwise it is disabled",
                    _simd_enabled_str,
                    "0").connect(functor(&Simd::set_simd, *this));


            register_parameter("svml_enabled",
                    "If set to '1' enables svml math library, otherwise it is disabled",
                    _svml_enabled_str,
                    "0").connect(functor(&Simd::set_svml, *this));

            register_parameter("fast_math_enabled",
                    "If set to '1' enables fast_math operations, otherwise it is disabled",
                    _fast_math_enabled_str,
                    "0").connect(functor(&Simd::set_fast_math, *this));

            register_parameter("mic_enabled",
                    "If set to '1' enables compilation for MIC architecture, otherwise it is disabled",
                    _mic_enabled_str,
                    "0").connect(functor(&Simd::set_mic, *this));
        }

        void Simd::set_simd(const std::string simd_enabled_str)
        {
            if (simd_enabled_str == "1")
            {
                _simd_enabled = true;
            }
        }

        void Simd::set_svml(const std::string svml_enabled_str)
        {
            if (svml_enabled_str == "1")
            {
                _svml_enabled = true;
            }
        }

        void Simd::set_fast_math(const std::string fast_math_enabled_str)
        {
            if (fast_math_enabled_str == "1")
            {
                _fast_math_enabled = true;
            }
        }

        void Simd::set_mic(const std::string mic_enabled_str)
        {
            if (mic_enabled_str == "1")
            {
                _mic_enabled = true;
            }
        }

        void Simd::pre_run(TL::DTO& dto)
        {
            this->PragmaCustomCompilerPhase::pre_run(dto);
        }

        void Simd::run(TL::DTO& dto)
        {
            this->PragmaCustomCompilerPhase::run(dto);

            //RefPtr<FunctionTaskSet> function_task_set = RefPtr<FunctionTaskSet>::cast_static(dto["openmp_task_info"]);

            Nodecl::NodeclBase translation_unit = dto["nodecl"];

            if (_simd_enabled)
            {
                SimdVisitor simd_visitor(_fast_math_enabled, _svml_enabled, _mic_enabled);
                simd_visitor.walk(translation_unit);
            }
        }

        SimdVisitor::SimdVisitor(bool fast_math_enabled, bool svml_enabled, bool mic_enabled)
            : _vectorizer(TL::Vectorization::Vectorizer::get_vectorizer())
        {
            if (fast_math_enabled)
                _vectorizer.enable_fast_math();

            if (mic_enabled)
            {
                _vector_length = 64;
                _device_name = "knc";
                _support_masking = true;
                _mask_size = 16;

                if (svml_enabled)
                    _vectorizer.enable_svml_knc();
            }
            else
            {
                _vector_length = 16;
                _device_name = "smp";
                _support_masking = false;
                _mask_size = 0;

                if (svml_enabled)
                    _vectorizer.enable_svml_sse();
            }
        }

        void SimdVisitor::visit(const Nodecl::OpenMP::Simd& simd_node)
        {
            Nodecl::ForStatement for_statement = simd_node.get_statement().as<Nodecl::ForStatement>();
            Nodecl::List simd_environment = simd_node.get_environment().as<Nodecl::List>();

            // Suitable clause
            Nodecl::List suitable_expressions;
            process_suitable_clause(simd_environment, suitable_expressions);

            // Vectorlengthfor clause
            TL::Type vectorlengthfor_type;
            process_vectorlengthfor_clause(simd_environment, vectorlengthfor_type);

            // Reduction clause
            TL::ObjectList<TL::Symbol> reductions;
            Nodecl::OpenMP::Reduction omp_reductions = simd_environment.find_first<Nodecl::OpenMP::Reduction>();
            Nodecl::List omp_reduction_list;

            std::map<TL::Symbol, TL::Symbol> new_external_vector_symbol_map;
            //TODO: move this to process_reduction_clause
            if(!omp_reductions.is_null())
            {
                // Extract reduced Nodecl::Symbol from ReductionItems
                omp_reduction_list = omp_reductions.get_reductions().as<Nodecl::List>();
                for(Nodecl::List::iterator it = omp_reduction_list.begin();
                        it != omp_reduction_list.end();
                        it++ )
                {
                    TL::Symbol red_sym = (*it).as<Nodecl::OpenMP::ReductionItem>().
                        get_reduced_symbol().as<Nodecl::Symbol>().get_symbol();

                    reductions.append(red_sym);

                    // Add new vector TL::Symbol in the enclosing context
                    TL::Symbol new_red_sym =
                        for_statement.retrieve_context().new_symbol("__vred_" + red_sym.get_name());
                    new_red_sym.get_internal_symbol()->kind = SK_VARIABLE;
                    new_red_sym.get_internal_symbol()->entity_specs.is_user_declared = 1;
                    new_red_sym.set_type(red_sym.get_type().get_vector_to(_vector_length));

                    // Add new TL::Symbol to map
                    new_external_vector_symbol_map.insert(std::pair<TL::Symbol, TL::Symbol>(
                                red_sym, new_red_sym));
                }
            }

            // Add epilog before vectorization
            Nodecl::ForStatement epilog = Nodecl::Utils::deep_copy(
                    for_statement, for_statement).as<Nodecl::ForStatement>();

            simd_node.append_sibling(epilog);

            // VECTORIZE FOR
            VectorizerEnvironment for_environment(
                    _device_name,
                    _vector_length, 
                    _support_masking,
                    _mask_size,
                    vectorlengthfor_type,
                    &suitable_expressions,
                    &reductions,
                    &new_external_vector_symbol_map);

            bool needs_epilog = 
                _vectorizer.vectorize(for_statement, for_environment); 

            // Add new vector symbols
            if (!new_external_vector_symbol_map.empty())
            {
                Nodecl::List pre_for_nodecls, post_for_nodecls;

                // REDUCTIONS
                for(Nodecl::List::iterator it = omp_reduction_list.begin();
                        it != omp_reduction_list.end();
                        it++)
                {
                    // Prepare reduction information to be used in vectorizer
                    Nodecl::OpenMP::ReductionItem omp_red_item = (*it).as<Nodecl::OpenMP::ReductionItem>();
                    TL::OpenMP::Reduction omp_red = *(OpenMP::Reduction::get_reduction_info_from_symbol(
                                omp_red_item.get_reductor().get_symbol()));

                    // Symbols
                    std::map<TL::Symbol, TL::Symbol>::iterator new_external_symbol_pair = 
                        new_external_vector_symbol_map.find(omp_red_item.get_reduced_symbol().get_symbol());

                    TL::Symbol scalar_tl_symbol = new_external_symbol_pair->first;
                    TL::Symbol vector_tl_symbol = new_external_symbol_pair->second;

                    // Reduction info
                    Nodecl::NodeclBase reduction_initializer = omp_red.get_initializer();
                    std::string reduction_name = omp_red.get_name();
                    TL::Type reduction_type = omp_red.get_type();

                    // Vectorize reductions
                    if(_vectorizer.is_supported_reduction(
                                omp_red.is_builtin(reduction_name),
                                reduction_name,
                                reduction_type,
                                for_environment))
                    {
                        _vectorizer.vectorize_reduction(scalar_tl_symbol, 
                                vector_tl_symbol, 
                                reduction_initializer,
                                reduction_name,
                                reduction_type,
                                for_environment,
                                pre_for_nodecls,
                                post_for_nodecls);
                    }
                    else
                    {
                        running_error("SIMD: reduction '%s:%s' is not supported", 
                                reduction_name.c_str(), scalar_tl_symbol.get_name().c_str());
                    }
                }

                simd_node.prepend_sibling(pre_for_nodecls);
                // Final reduction after the epilog (to reduce also elements from masked epilogs)
                epilog.append_sibling(post_for_nodecls);

                // TODO: 
                // firstprivate in SIMD
            }

            // Process epilog
            if (needs_epilog)
            {
                _vectorizer.process_epilog(epilog, for_environment);
            }
            else // Remove epilog
            {
                Nodecl::Utils::remove_from_enclosing_list(epilog);
            }

            // Remove Simd node
            simd_node.replace(for_statement);
        }

        void SimdVisitor::visit(const Nodecl::OpenMP::SimdFor& simd_node)
        {
            Nodecl::OpenMP::For omp_for = simd_node.get_openmp_for().as<Nodecl::OpenMP::For>();
            Nodecl::List omp_simd_for_environment = simd_node.get_environment().as<Nodecl::List>();
            Nodecl::List omp_for_environment = omp_for.get_environment().as<Nodecl::List>();

            // Skipping AST_LIST_NODE
            Nodecl::NodeclBase loop = omp_for.get_loop();
            ERROR_CONDITION(!loop.is<Nodecl::ForStatement>(), 
                    "Unexpected node %s. Expecting a ForStatement after '#pragma omp simd for'", 
                    ast_print_node_type(loop.get_kind()));

            Nodecl::ForStatement for_statement = loop.as<Nodecl::ForStatement>();

            // Suitable clause
            Nodecl::List suitable_expressions;
            process_suitable_clause(omp_simd_for_environment, suitable_expressions);

            // Vectorlengthfor clause
            TL::Type vectorlengthfor_type;
            process_vectorlengthfor_clause(omp_simd_for_environment, vectorlengthfor_type);

            // Reduction clause
            TL::ObjectList<TL::Symbol> reductions;
            process_reduction_clause(omp_simd_for_environment, reductions);

            // Add epilog with single before vectorization
            Nodecl::ForStatement epilog = Nodecl::Utils::deep_copy(
                    for_statement, for_statement).as<Nodecl::ForStatement>();

            Nodecl::List single_environment;

            Nodecl::NodeclBase barrier = omp_for_environment.find_first<Nodecl::OpenMP::BarrierAtEnd>();
            Nodecl::NodeclBase flush = omp_for_environment.find_first<Nodecl::OpenMP::FlushAtExit>();

            if (!barrier.is_null())
            {
                // Move barrier from omp for to single
                single_environment.append(barrier.shallow_copy());
                Nodecl::Utils::remove_from_enclosing_list(barrier);
            }
            if (!flush.is_null())
            {
                // Move flush from omp for to single
                single_environment.append(flush.shallow_copy());
                Nodecl::Utils::remove_from_enclosing_list(flush);
            }

            // Mark the induction variable as a private entity in the Single construct
            Nodecl::OpenMP::Private ind_var_priv = 
                Nodecl::OpenMP::Private::make(Nodecl::List::make(
                            TL::ForStatement(for_statement).get_induction_variable().make_nodecl()));
            single_environment.append(ind_var_priv);

            Nodecl::OpenMP::Single single_epilog =
                Nodecl::OpenMP::Single::make(single_environment,
                        Nodecl::List::make(epilog), epilog.get_locus());

            simd_node.append_sibling(single_epilog);

            // VECTORIZE FOR
            std::map<TL::Symbol, TL::Symbol> new_external_vector_symbol_map;

            VectorizerEnvironment for_environment(
                    _device_name,
                    _vector_length,
                    _support_masking, 
                    _mask_size,
                    vectorlengthfor_type,
                    &suitable_expressions,
                    &reductions,
                    &new_external_vector_symbol_map);

            bool needs_epilog = 
                _vectorizer.vectorize(for_statement, for_environment); 

            // Process epilog
            if (needs_epilog)
            {
                _vectorizer.process_epilog(epilog, for_environment);
            }
            else // Remove epilog
            {
                Nodecl::Utils::remove_from_enclosing_list(single_epilog);
            }

            // Remove Simd node
            simd_node.replace(omp_for);
        }

        void SimdVisitor::visit(const Nodecl::OpenMP::SimdFunction& simd_node)
        {
            Nodecl::FunctionCode function_code = simd_node.get_statement()
                .as<Nodecl::FunctionCode>();

            Nodecl::List omp_environment = simd_node.get_environment().as<Nodecl::List>();

            // Remove SimdFunction node
            simd_node.replace(function_code);

            // Suitable clause
            Nodecl::List suitable_expressions;
            process_suitable_clause(omp_environment, suitable_expressions);

            // Vectorlengthfor clause
            TL::Type vectorlengthfor_type;
            process_vectorlengthfor_clause(omp_environment, vectorlengthfor_type);


            Nodecl::OpenMP::Mask omp_mask = omp_environment.find_first<Nodecl::OpenMP::Mask>();
            Nodecl::OpenMP::NoMask omp_nomask = omp_environment.find_first<Nodecl::OpenMP::NoMask>();

            if((!omp_mask.is_null()) && (!omp_nomask.is_null()))
            {
                running_error("SIMD: 'mask' and 'nomask' clauses are now allowed at the same time\n");
            } 

            if((!omp_mask.is_null()) && (!_support_masking))
            {
                running_error("SIMD: 'mask' clause detected. Masking is not supported by the underlying architecture\n");
            } 

            // Mask Version
            if (_support_masking && omp_nomask.is_null())
            {
                Nodecl::FunctionCode mask_func =
                    common_simd_function(simd_node, function_code, suitable_expressions, vectorlengthfor_type, true);
            }
            // Nomask Version
            if (omp_mask.is_null())
            {
                Nodecl::FunctionCode no_mask_func =
                    common_simd_function(simd_node, function_code, suitable_expressions, vectorlengthfor_type, false);
            }
        }

        Nodecl::FunctionCode SimdVisitor::common_simd_function(const Nodecl::OpenMP::SimdFunction& simd_node,
                const Nodecl::FunctionCode& function_code,
                const Nodecl::List& suitable_expressions,
                const TL::Type& vectorlengthfor_type,
                const bool masked_version)
        {
            TL::Symbol func_sym = function_code.get_symbol();
            std::string orig_func_name = func_sym.get_name();

            // Set new vector function symbol
            std::stringstream vector_func_name; 

            vector_func_name <<"__" 
                << orig_func_name
                << "_" 
                << _device_name 
                << "_" 
                << _vector_length
                ;

            if (masked_version)
            {
                vector_func_name << "_mask";
            }

            TL::Symbol new_func_sym = func_sym.get_scope().
                new_symbol(vector_func_name.str());
            new_func_sym.get_internal_symbol()->kind = SK_FUNCTION;

            Nodecl::Utils::SimpleSymbolMap func_sym_map;
            func_sym_map.add_map(func_sym, new_func_sym);

            Nodecl::FunctionCode vector_func_code = 
                Nodecl::Utils::deep_copy(function_code, 
                        function_code,
                        func_sym_map).as<Nodecl::FunctionCode>();

            FunctionDeepCopyFixVisitor fix_deep_copy_visitor(func_sym, new_func_sym);
            fix_deep_copy_visitor.walk(vector_func_code.get_statements());

            // Add SIMD version to vector function versioning
            _vectorizer.add_vector_function_version(orig_func_name, vector_func_code, 
                    _device_name, _vector_length, func_sym.get_type().returns(), masked_version, 
                    TL::Vectorization::SIMD_FUNC_PRIORITY, false);

            // Append vectorized function code to scalar function
            simd_node.append_sibling(vector_func_code);

            // Vectorize function
            VectorizerEnvironment _environment(
                    _device_name,
                    _vector_length, 
                    _support_masking,
                    _mask_size,
                    vectorlengthfor_type,
                    &suitable_expressions,
                    NULL,
                    NULL);

            _vectorizer.vectorize(vector_func_code, _environment, masked_version); 

            return vector_func_code;
        }


        void SimdVisitor::process_suitable_clause(const Nodecl::List& environment,
                Nodecl::List& suitable_expressions)
        {
            Nodecl::OpenMP::Suitable omp_suitable = 
                environment.find_first<Nodecl::OpenMP::Suitable>();

            if(!omp_suitable.is_null())
            {
                suitable_expressions = omp_suitable.get_suitable_expressions().as<Nodecl::List>();
            }
        }


        void SimdVisitor::process_vectorlengthfor_clause(const Nodecl::List& environment, 
                TL::Type& vectorlengthfor_type)
        {
            Nodecl::OpenMP::VectorLengthFor omp_vector_length_for = 
                environment.find_first<Nodecl::OpenMP::VectorLengthFor>();

            if(!omp_vector_length_for.is_null())
            {
                vectorlengthfor_type = omp_vector_length_for.get_type();
            }
            else //Float type by default //TODO
            {
                vectorlengthfor_type = TL::Type::get_float_type();
            }
        }


        void SimdVisitor::process_reduction_clause(const Nodecl::List& environment,
                TL::ObjectList<TL::Symbol>& reductions)
        {
            Nodecl::OpenMP::Reduction omp_reductions = 
                environment.find_first<Nodecl::OpenMP::Reduction>();

            if(!omp_reductions.is_null())
            {
                // Extract reduced Nodecl::Symbol from ReductionItems
                Nodecl::List omp_reduction_list = omp_reductions.get_reductions().as<Nodecl::List>();
                for(Nodecl::List::iterator it = omp_reduction_list.begin();
                        it != omp_reduction_list.end();
                        it++ )
                {
                    reductions.append((*it).as<Nodecl::OpenMP::ReductionItem>().
                            get_reduced_symbol().as<Nodecl::Symbol>().get_symbol());
                }
            }
        }


        FunctionDeepCopyFixVisitor::FunctionDeepCopyFixVisitor(const TL::Symbol& orig_symbol, const TL::Symbol& new_symbol)
            : _orig_symbol(orig_symbol), _new_symbol(new_symbol)
        {
        }

        void FunctionDeepCopyFixVisitor::visit(const Nodecl::Symbol& n)
        {
            if (n.get_symbol() == _new_symbol)
            {
                n.replace(_orig_symbol.make_nodecl(false, n.get_locus()));
            }
        }
    }
}

EXPORT_PHASE(TL::OpenMP::Simd)
