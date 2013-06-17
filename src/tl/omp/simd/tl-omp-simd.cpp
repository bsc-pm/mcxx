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
//#include "tl-nodecl-utils.hpp"
//#include "cxx-diagnostic.h"
//#include "cxx-cexpr.h"
//#include "fortran03-scope.h"
//#include "tl-predicateutils.hpp"

namespace TL { 
    namespace OpenMP {
        
        Simd::Simd()
            : PragmaCustomCompilerPhase("omp-simd"),  
            _simd_enabled(false), _svml_enabled(false), _ffast_math_enabled(false), _mic_enabled(false)
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

            register_parameter("ffast_math_enabled",
                    "If set to '1' enables ffast_math operations, otherwise it is disabled",
                    _ffast_math_enabled_str,
                    "0").connect(functor(&Simd::set_ffast_math, *this));

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

        void Simd::set_ffast_math(const std::string ffast_math_enabled_str)
        {
            if (ffast_math_enabled_str == "1")
            {
                _ffast_math_enabled = true;
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
                SimdVisitor simd_visitor(_ffast_math_enabled, _svml_enabled, _mic_enabled);
                simd_visitor.walk(translation_unit);
            }
        }

        SimdVisitor::SimdVisitor(bool ffast_math_enabled, bool svml_enabled, bool mic_enabled)
            : _vectorizer(TL::Vectorization::Vectorizer::get_vectorizer())
        {
            if (ffast_math_enabled)
                _vectorizer.enable_ffast_math();

            if (mic_enabled)
            {
                _vector_length = 64;
                _device_name = "knc";

                if (svml_enabled)
                    _vectorizer.enable_svml_knc();
            }
            else
            {
                _vector_length = 16;
                _device_name = "smp";

                if (svml_enabled)
                    _vectorizer.enable_svml_sse();
            }
        }

        void SimdVisitor::visit(const Nodecl::OpenMP::Simd& simd_node)
        {
            Nodecl::NodeclBase for_statement = simd_node.get_statement();

            // Vectorize for
            Nodecl::NodeclBase epilog = 
                    _vectorizer.vectorize(for_statement.as<Nodecl::ForStatement>(), 
                            _device_name, _vector_length, NULL); 

            // Add epilog
            if (!epilog.is_null())
            {
                simd_node.append_sibling(epilog);
            }

            // Remove Simd node
            simd_node.replace(for_statement);
        }

        void SimdVisitor::visit(const Nodecl::OpenMP::SimdFunction& simd_node)
        {
            Nodecl::FunctionCode function_code = simd_node.get_statement()
                .as<Nodecl::FunctionCode>();
            
            // Remove SimdFunction node
            simd_node.replace(function_code);

            TL::Symbol sym = function_code.get_symbol();

            Nodecl::FunctionCode vectorized_func_code = 
                Nodecl::Utils::deep_copy(function_code, function_code).as<Nodecl::FunctionCode>();

            // Vectorize function
            _vectorizer.vectorize(vectorized_func_code, 
                    _device_name, _vector_length, NULL); 

            // Set new name
            std::stringstream vectorized_func_name; 
            
            vectorized_func_name <<"__" 
                << sym.get_name() 
                << "_" 
                << _device_name 
                << "_" 
                << _vector_length;

            vectorized_func_code.get_symbol().set_name(vectorized_func_name.str());

            // Add SIMD version to vector function versioning
            _vectorizer.add_vector_function_version(sym.get_name(), vectorized_func_code, 
                    _device_name, _vector_length, NULL, TL::Vectorization::SIMD_FUNC_PRIORITY);

            // Append vectorized function code to scalar function
            simd_node.append_sibling(vectorized_func_code);
        }

        /*
        void Simd::simd_for_handler_pre(TL::PragmaCustomStatement) { }
        void Simd::simd_for_handler_post(TL::PragmaCustomStatement stmt) 
    {
        // Skipping AST_LIST_NODE 
        Nodecl::NodeclSimd statements = stmt.get_statements();

        if (_simd_enabled)
        {
            ERROR_CONDITION(!statements.is<Nodecl::List>(), 
                    "'pragma omp simd' Expecting a AST_LIST_NODE (1)", 0);
            Nodecl::List ast_list_node = statements.as<Nodecl::List>();
            ERROR_CONDITION(ast_list_node.size() != 1, 
                    "AST_LIST_NODE after '#pragma omp simd' must be equal to 1 (1)", 0);

            // Skipping NODECL_CONTEXT
            Nodecl::NodeclSimd context = ast_list_node.front();
            ERROR_CONDITION(!context.is<Nodecl::Context>(), 
                    "'pragma omp simd' Expecting a NODECL_CONTEXT", 0);

            // Skipping AST_LIST_NODE
            Nodecl::NodeclSimd in_context = context.as<Nodecl::Context>().get_in_context();
            ERROR_CONDITION(!in_context.is<Nodecl::List>(), 
                    "'pragma omp simd' Expecting a AST_LIST_NODE (2)", 0);
            Nodecl::List ast_list_node2 = in_context.as<Nodecl::List>();
            ERROR_CONDITION(ast_list_node2.size() != 1, 
                    "AST_LIST_NODE after '#pragma omp simd' must be equal to 1 (2)", 0);

            Nodecl::NodeclSimd node = ast_list_node2.front();
            ERROR_CONDITION(!node.is<Nodecl::ForStatement>(), 
                    "Unexpected node %s. Expecting a ForStatement after '#pragma omp simd'", 
                    ast_print_node_type(node.get_kind()));

            // Vectorize for
            Nodecl::NodeclSimd epilog = 
                _vectorizer.vectorize(node.as<Nodecl::ForStatement>(),
                        "smp", 16, NULL); 

            // Add epilog
            if (!epilog.is_null())
            {
                //node.append_sibling(epilog);
            }

            // for_handler_post
            PragmaCustomLine pragma_line = stmt.get_pragma_line();
            bool barrier_at_end = !pragma_line.get_clause("nowait").is_defined();

            Nodecl::NodeclSimd code = loop_handler_post(
                    stmt, node, barrier_at_end, false);

            // Removing #pragma
            stmt.replace(code);
        }
        else
        {
            // Remove #pragma
            stmt.replace(statements);
        }
    }

    void Simd::parallel_simd_for_handler_pre(TL::PragmaCustomStatement) { }
    void Simd::parallel_simd_for_handler_post(TL::PragmaCustomStatement stmt) 
    {
    }
    */
    } 
}

EXPORT_PHASE(TL::OpenMP::Simd)
