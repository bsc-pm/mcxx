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




#ifndef HLT_BLOCKING_HPP
#define HLT_BLOCKING_HPP

#include "hlt-common.hpp"
#include "hlt-transform.hpp"
#include "tl-langconstruct.hpp"
#include "tl-for-nest.hpp"

namespace TL
{
    namespace HLT
    {
        //! \addtogroup HLT High Level Transformations
        //! @{

        //! Class that implements loop blocking
        /*! 
          This class implements blocking on a perfect loop nest.

          Input of this transformation is a perfect loop nest and a list of 
          expressions containing each the value of blocking in each loop nest.
         */
        class LIBHLT_CLASS LoopBlocking : public BaseTransform
        {
            protected:
                virtual Source get_source();
            private:
                TL::ForStatement _for_stmt;
                unsigned int _nesting;
                ObjectList<TL::Expression> _nest_factors;
                ForNestInfo _for_nest_info;

                Source do_blocking();

                Source do_nothing();

                bool check_nesting();
            public:
                //! Constructs a LoopBlocking object
                /*!
                   \param for_stmt For statement that should be a perfect loop nest
                   \param block_factors List of expressions, each one with the block size of the corresponding nest level
                   */
                LoopBlocking(TL::ForStatement for_stmt, ObjectList<TL::Expression> block_factors);
        };

        //! Returns a LoopBlocking object
        /*!
          \param for_stmt For statement that should be a perfect loop nest
          \param block_factors List of expressions, each one with the block size of the corresponding nest level
         */
        LIBHLT_EXTERN LoopBlocking block_loop(TL::ForStatement for_stmt, ObjectList<TL::Expression> block_factors);

        //! @}
    }
}

#endif // HLT_BLOCKING_HPP
