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

#include "codegen-cuda.hpp"

#include "cxx-diagnostic.h"

//getenv
#include<cstdlib>


namespace Codegen
{

    // Check if the given path file comes from CUDA installation directory
    bool CudaGPU::check_whether_symbol_comes_from_cuda(TL::Symbol s) const
    {
        if (_cuda_home == "")
            return false;

        std::string path(s.get_filename());
        return (path.substr(0, _cuda_home.size()) == _cuda_home);
    }

    CudaGPU::CudaGPU()
    {
        const char* path = getenv("CUDA_HOME");
        _cuda_home = path ? path : "";

        // Ideally, we should emit this warning in the driver...
        if (path == NULL)
            warn_printf_at(NULL, "The environment variable '$CUDA_HOME' is not defined\n");
    }

    void CudaGPU::visit(const Nodecl::CudaKernelCall &n)
    {
        Nodecl::NodeclBase nodecl_function_call = n.get_function_call();
        Nodecl::NodeclBase kernel_config = n.get_kernel_config();

        Nodecl::FunctionCall function_call = nodecl_function_call.as<Nodecl::FunctionCall>();
        Nodecl::NodeclBase called_expr = function_call.get_called();
        Nodecl::NodeclBase function_args = function_call.get_arguments();

        walk(called_expr);

        *file << "<<<";
        walk_list(kernel_config.as<Nodecl::List>(),", ");
        *file << ">>>";

        *file << "(";
        if (!function_args.is_null())
        {
            walk_list(function_args.as<Nodecl::List>(), ", ");
        }
        *file << ")";
    }


    void CudaGPU::do_define_symbol(TL::Symbol symbol,
            void (CxxBase::*decl_sym_fun)(TL::Symbol symbol),
            void (CxxBase::*def_sym_fun)(TL::Symbol symbol),
            TL::Scope* scope)
    {
        if (!check_whether_symbol_comes_from_cuda(symbol))
        {
            CxxBase::do_define_symbol(symbol, decl_sym_fun, def_sym_fun, scope);
        }
        else
        {
            // This symbol will be treated as if it was defined
            set_codegen_status(symbol, CODEGEN_STATUS_DEFINED);
        }
    }

    void CudaGPU::do_declare_symbol(TL::Symbol symbol,
            void (CxxBase::*decl_sym_fun)(TL::Symbol symbol),
            void (CxxBase::*def_sym_fun)(TL::Symbol symbol),
            TL::Scope* scope)
    {
        if (!check_whether_symbol_comes_from_cuda(symbol))
        {
            CxxBase::do_declare_symbol(symbol, decl_sym_fun, def_sym_fun, scope);
        }
        else
        {
            // This symbol will be treated as if it was declared
            set_codegen_status(symbol, CODEGEN_STATUS_DECLARED);
        }
    }

    bool CudaGPU::cuda_print_special_attributes()
    {
        return true;
    }

    bool CudaGPU::cuda_emit_always_extern_linkage()
    {
        return IS_C_LANGUAGE;
    }

} // Codegen

EXPORT_PHASE(Codegen::CudaGPU)
