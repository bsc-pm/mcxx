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
