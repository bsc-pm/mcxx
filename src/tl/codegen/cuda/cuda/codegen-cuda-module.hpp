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

#ifndef CUDA_MODULE_HPP
#define CUDA_MODULE_HPP

#include "codegen-common.hpp"
#include "tl-nodecl-base.hpp"


namespace Codegen
{
    class CudaModuleVisitor : public CodegenModuleVisitor
    {
        public:

            CudaModuleVisitor(CodegenVisitor* base_codegen);

            void visit(const Nodecl::CudaKernelCall & node);

            Nodecl::NodeclVisitor<void>::Ret unhandled_node(const Nodecl::NodeclBase& n);

        private:

            void walk_list(const Nodecl::List& list, const std::string& separator);

    };
}

#endif // CUDA_MODULE_HPP
