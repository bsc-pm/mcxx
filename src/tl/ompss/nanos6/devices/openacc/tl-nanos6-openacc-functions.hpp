/*--------------------------------------------------------------------
  (C) Copyright 2020-2020 Barcelona Supercomputing Center
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

#ifndef TL_NANOS6_OPENACC_FUNCTIONS_HPP
#define TL_NANOS6_OPENACC_FUNCTIONS_HPP

#include "tl-compilerphase.hpp"
#include "tl-nodecl.hpp"

namespace TL
{
namespace Nanos6
{

//! This phase deals with the specific changes that OpenACC task functions
// need.
class OpenACCTasks : public TL::CompilerPhase
{
  private:
  // Modify function definition (sym) to append a new parameter:
  // int asyncQueue
  // Will be applied to Symbol List provided by FunctionDefinitionsVisitor
  // The FunctionCallsVisitor will then provide the apropriate variable in the call sites
  void append_async_parameter(TL::Symbol &sym);

  public:
    OpenACCTasks();

    virtual void run(DTO &dto);
};

} // namespace Nanos6
} // namespace TL

#endif
