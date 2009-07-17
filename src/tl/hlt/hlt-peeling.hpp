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
#ifndef HLT_PEELING_HPP
#define HLT_PEELING_HPP

#include "hlt-transform.hpp"

namespace TL
{
    namespace HLT
    {
        //! \addtogroup HLT High Level Transformations
        //! @{

        //! Class that implements loop peeling
        class LIBHLT_CLASS LoopPeeling : public BaseTransform
        {
            private:
                ForStatement _for_stmt;
                int _init_peeling;
                int _end_peeling;
                Source _peeled_loop;
                void do_peeling();
            protected:
                virtual Source get_source();
            public:
                LoopPeeling(ForStatement for_stmt, 
                        int init_peeling, 
                        int end_peeling);
        };

        LIBHLT_EXTERN LoopPeeling loop_peeling(ForStatement for_stmt, int init_peeling, int end_peeling);

        //! @}
    }
}

#endif // HLT_PEELING_HPP
