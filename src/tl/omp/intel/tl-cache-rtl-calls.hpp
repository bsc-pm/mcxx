/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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

#ifndef TL_CACHE_RTL_CALLS
#define TL_CACHE_RTL_CALLS

#include "tl-nodecl-visitor.hpp"
#include "tl-omp-intel.hpp"

namespace TL { namespace Intel {

class CacheRTLCalls : public Nodecl::ExhaustiveVisitor<void>
{
    public:
        CacheRTLCalls(Lowering*);
        ~CacheRTLCalls();

        virtual void visit(const Nodecl::FunctionCode& function_code);

        typedef void (CacheRTLCalls::* CacheRTLCallsHandler)(TL::Symbol sym,
                Nodecl::NodeclBase function_code,
                TL::ObjectList<Nodecl::NodeclBase>& ocurrences);
    private:
        Lowering* _lowering;

        void add_cacheable_function(
                TL::ObjectList<TL::Symbol>& cacheable_set,
                const std::string& str,
                std::map<TL::Symbol, CacheRTLCallsHandler>& cacheable_handler_set,
                CacheRTLCallsHandler do_cache_call);

        void cache_kmpc_global_thread(
                TL::Symbol sym,
                Nodecl::NodeclBase function_code,
                TL::ObjectList<Nodecl::NodeclBase>& occurrences);
};

} }

#endif
