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

#ifndef CODEGEN_CUDA_HPP
#define CODEGEN_CUDA_HPP

#include "codegen-phase.hpp"
#include "codegen-cxx.hpp"
// #include "tl-scope.hpp"
// #include "tl-symbol.hpp"
// 
// #include <sstream>
// #include <map>
// #include <set>

namespace Codegen
{
    class CudaGPU : public CxxBase
    {
        public:
            using CxxBase::visit;
            void visit(const Nodecl::CudaKernelCall &);

        protected:

            virtual void do_define_symbol(TL::Symbol symbol,
                    void (CxxBase::*decl_sym_fun)(TL::Symbol symbol),
                    void (CxxBase::*def_sym_fun)(TL::Symbol symbol),
                    TL::Scope* scope = NULL);


            virtual void do_declare_symbol(TL::Symbol symbol,
                    void (CxxBase::*decl_sym_fun)(TL::Symbol symbol),
                    void (CxxBase::*def_sym_fun)(TL::Symbol symbol),
                    TL::Scope* scope = NULL);

            virtual bool cuda_print_special_attributes();

            virtual bool cuda_emit_always_extern_linkage();
    };
}

#endif // CODEGEN_CUDA_HPP
