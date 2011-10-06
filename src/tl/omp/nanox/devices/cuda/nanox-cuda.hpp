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


#ifndef NANOX_CUDA_HPP
#define NANOX_CUDA_CPP

#include "tl-compilerphase.hpp"
#include "tl-devices.hpp"

namespace TL
{

    namespace Nanox
    {

        class DeviceCUDA : public DeviceProvider
        {
            private:
                std::string _cudaFilename;
                AST_t _root;
                std::set<std::string> _taskSymbols;

                void do_cuda_outline_replacements(
                        AST_t body,
                        ScopeLink scope_link,
                        const DataEnvironInfo& data_env_info,
                        Source &initial_code,
                        Source &replaced_outline);

                void do_cuda_inline_get_addresses(
                        const Scope& sc,
                        const DataEnvironInfo& data_env_info,
                        Source &copy_setup,
                        ReplaceSrcIdExpression& replace_src,
                        bool &err_declared);

                void replace_kernel_config(AST_t &kernel_call, ScopeLink sl);

                void get_output_file(std::ofstream& cudaFile);

                std::set<std::string> _fwdSymbols;

                RefPtr<OpenMP::FunctionTaskSet> _function_task_set; 
            public:

                // This phase does nothing
                virtual void pre_run(DTO& dto);
                virtual void run(DTO& dto) { }

                DeviceCUDA();

                virtual ~DeviceCUDA() { }

                virtual void create_outline(
                        const std::string& task_name,
                        const std::string& struct_typename,
                        DataEnvironInfo &data_environ,
                        const OutlineFlags& outline_flags,
                        AST_t reference_tree,
                        ScopeLink sl,
                        Source initial_setup,
                        Source outline_body);

                virtual void do_replacements(DataEnvironInfo& data_environ,
                        AST_t body,
                        ScopeLink scope_link,
                        Source &initial_setup,
                        Source &replace_src);

                virtual void get_device_descriptor(const std::string& task_name,
                        DataEnvironInfo &data_environ,
                        const OutlineFlags& outline_flags,
                        AST_t reference_tree,
                        ScopeLink sl,
                        Source &ancillary_device_description,
                        Source &device_descriptor);

                virtual void phase_cleanup(DTO& data_flow);

                virtual void insert_function_definition(PragmaCustomConstruct, bool);
                virtual void insert_declaration(PragmaCustomConstruct, bool);
        };

    }

}

#endif // NANOX_CUDA_CPP
