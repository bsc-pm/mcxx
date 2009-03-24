/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#ifndef HLT_BLOCKING_HPP
#define HLT_BLOCKING_HPP

#include "hlt-transform.hpp"
#include "tl-langconstruct.hpp"

namespace TL
{
    namespace HLT
    {
        class LoopBlocking : public BaseTransform
        {
            protected:
                virtual Source get_source();
            private:
                TL::ForStatement _for_stmt;
                unsigned int _nesting;
                ObjectList<TL::Expression> _nest_factors;
                ObjectList<TL::ForStatement> _nest_loops;

                Source do_blocking();

                Source do_nothing();

                bool check_nesting();
                void discover_for_nest_rec(TL::AST_t tree);
                unsigned int discover_for_nest();
            public:
                LoopBlocking(TL::ForStatement for_stmt, ObjectList<TL::Expression> block_factors);
        };

        LoopBlocking block_loop(TL::ForStatement for_stmt, ObjectList<TL::Expression> block_factors);
    }
}

#endif // HLT_BLOCKING_HPP
