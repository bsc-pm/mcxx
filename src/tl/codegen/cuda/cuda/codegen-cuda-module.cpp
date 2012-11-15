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

#include "codegen-cuda-module.hpp"
#include "codegen-cxx.hpp"

namespace Codegen
{
    CudaModuleVisitor::CudaModuleVisitor(CodegenVisitor* base_codegen)
        : CodegenModuleVisitor(base_codegen)
    {
    }

    void CudaModuleVisitor::visit(const Nodecl::CudaKernelCall & node)
    {
        Nodecl::NodeclBase nodecl_function_call = node.get_function_call();
        Nodecl::NodeclBase kernel_config = node.get_kernel_config();

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

    Nodecl::NodeclVisitor<void>::Ret CudaModuleVisitor::unhandled_node(const Nodecl::NodeclBase& n)
    {
        fprintf(stderr, "CUDA Codegen: Unknown node %s at %s.\n",
                ast_print_node_type(n.get_kind()),
                n.get_locus().c_str());
        /*
           running_error("SSE Codegen: Unknown node %s at %s.",
           ast_print_node_type(n.get_kind()),
           n.get_locus().c_str()); 
         */
        return Ret();
    }

    void CudaModuleVisitor::walk_list(const Nodecl::List& list, const std::string& separator)
    {
        Nodecl::List::const_iterator it = list.begin(), begin = it;

        while (it != list.end())
        {
            if (it != begin)
            {
                file << separator;
            }

            walk(*it);
            it++;
        }
    }
}
