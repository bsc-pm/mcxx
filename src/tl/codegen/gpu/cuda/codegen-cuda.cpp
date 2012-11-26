/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

// #ifdef HAVE_QUADMATH_H
// MCXX_BEGIN_DECLS
// #include <quadmath.h>
// MCXX_END_DECLS
// #endif

namespace Codegen
{
        void CudaGPU::visit(const Nodecl::CudaKernelCall &n)
        {
            Nodecl::NodeclBase nodecl_function_call = n.get_function_call();
            Nodecl::NodeclBase kernel_config = n.get_kernel_config();

            Nodecl::FunctionCall function_call = nodecl_function_call.as<Nodecl::FunctionCall>();
            Nodecl::NodeclBase called_expr = function_call.get_called();
            Nodecl::NodeclBase function_args = function_call.get_arguments();

            walk(called_expr);

            file << "<<<";
            walk_list(kernel_config.as<Nodecl::List>(),", ");
            file << ">>>";

            file << "(";
            if (!function_args.is_null())
            {
                walk_list(function_args.as<Nodecl::List>(), ", ");
            }
            file << ");\n";
        }


} // Codegen

EXPORT_PHASE(Codegen::CudaGPU)
