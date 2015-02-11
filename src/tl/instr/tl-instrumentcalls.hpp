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




#ifndef TL_INSTRUMENTCALLS_HPP
#define TL_INSTRUMENTCALLS_HPP

#include "tl-compilerphase.hpp"
#include "tl-traverse.hpp"
#include "tl-instrumentfilter.hpp"
#include "tl-langconstruct.hpp"

namespace TL
{
    class InstrumentCalls : public CompilerPhase
    {
        private:
            InstrumentFilterFile _instrument_filter;
            class InstrumentCallsFunctor : public TraverseFunctor
            {
                private:
                    std::set<std::string> defined_shadows;
                    InstrumentFilterFile& _instrument_filter;
                public:
                    InstrumentCallsFunctor(InstrumentFilterFile& instrument_filter);

                    virtual ~InstrumentCallsFunctor() { }

                    virtual void preorder(Context ctx, AST_t node);
                    virtual void postorder(Context ctx, AST_t node);

                    bool define_shadow(IdExpression function_name, std::string shadow_function_name);
            };

        public:
            virtual void pre_run(DTO& data_flow);
            virtual void run(DTO& data_flow);

            virtual ~InstrumentCalls();
            InstrumentCalls(const std::string& instrument_file_name, 
                    const std::string& instrument_filter_mode);
    };
}

#endif // TL_INSTRUMENTCALLS_HPP
