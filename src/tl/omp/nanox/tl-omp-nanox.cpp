/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

#include "tl-omp-nanox.hpp"

using namespace TL;
using namespace TL::Nanox;


OMPTransform::OMPTransform()
{
    set_phase_name("OpenMP for nanox");
    set_phase_description("This phase implements OpenMP targeting nanox runtime");

    on_directive_post["parallel"].connect(functor(&OMPTransform::parallel_postorder, *this));

    on_directive_post["task"].connect(functor(&OMPTransform::task_postorder, *this));

    on_directive_post["taskwait"].connect(functor(&OMPTransform::taskwait_postorder, *this));

    on_directive_post["single"].connect(functor(&OMPTransform::single_postorder, *this));

    on_directive_post["for"].connect(functor(&OMPTransform::for_postorder, *this));

    on_directive_post["atomic"].connect(functor(&OMPTransform::atomic_postorder, *this));

    on_directive_post["threadprivate"].connect(functor(&OMPTransform::atomic_postorder, *this));

    on_directive_post["barrier"].connect(functor(&OMPTransform::barrier_postorder, *this));

    on_directive_pre["target"].connect(functor(&OMPTransform::target_preorder, *this));
    on_directive_post["target"].connect(functor(&OMPTransform::target_postorder, *this));

    ObjectList<std::string> smp_device_fallback;
    smp_device_fallback.append("smp");

    _target_ctx.push_back(smp_device_fallback);
}

EXPORT_PHASE(TL::Nanox::OMPTransform)
