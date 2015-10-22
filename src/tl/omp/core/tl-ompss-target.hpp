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




#ifndef TL_OMPSS_TARGET_HPP
#define TL_OMPSS_TARGET_HPP

#include "tl-objectlist.hpp"
#include "tl-symbol.hpp"

namespace TL { namespace OmpSs {

    struct TargetContext
    {
        ObjectList<std::string> device_list;

        ObjectList<Nodecl::NodeclBase> copy_in;
        ObjectList<Nodecl::NodeclBase> copy_out;
        ObjectList<Nodecl::NodeclBase> copy_inout;

        ObjectList<Nodecl::NodeclBase> ndrange;
        ObjectList<Nodecl::NodeclBase> shmem; // shared memory
        ObjectList<Nodecl::NodeclBase> onto;

        bool is_implicit;
        bool has_implements;
        Symbol implements;

        // The name of the file where the kernels are defined
        std::string file;

        // The real name of the kernel
        std::string name;

        enum CopyDepsValue {
            UNDEF_COPY_DEPS,
            NO_COPY_DEPS,
            COPY_DEPS,
        };
        CopyDepsValue copy_deps;

        TargetContext()
            : device_list(), copy_in(), copy_out(), copy_inout(),
            ndrange(), shmem(), onto(), is_implicit(),
            has_implements(), implements(), file(), name(), copy_deps(UNDEF_COPY_DEPS)
        {
        }
    };

} }

#endif // TL_OMPSS_TARGET_HPP
