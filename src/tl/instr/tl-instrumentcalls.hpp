/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
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
