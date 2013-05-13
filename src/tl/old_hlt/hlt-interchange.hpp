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




#ifndef HLT_INTERCHANGE_HPP
#define HLT_INTERCHANGE_HPP

#include "hlt-transform.hpp"
#include "tl-for-nest.hpp"

namespace TL
{
    namespace HLT
    {
        //! \addtogroup HLT High Level Transformations
        //! @{

        //! This class implements loops interchange
        /*!
          Loop interchange is a loop transformation where a perfect loop nest
          has its loops permuted.
         */
        class LIBHLT_CLASS LoopInterchange : public BaseTransform
        {
            protected:
                Source do_interchange();
                virtual Source get_source();
                bool is_valid_permutation(ObjectList<int> permutation, bool &identity);
                ForNestInfo _for_nest;
                ObjectList<int> _permutation;
                bool _is_identity;
            public:
                //! Constructs a LoopInterchange object
                /*!
                  \param for_stmt Perfect loop nest
                  \param permutation A permutation of integers ranging from 1 to the depth of \a for_stmt loop nest
                 */
                LoopInterchange(ForStatement for_stmt, ObjectList<int> permutation);
        };

        //! Constructs a LoopInterchange object
        /*!
          \param for_stmt Perfect loop nest
          \param permutation A permutation of integers ranging from 1 to the depth of \a for_stmt loop nest
         */
        LIBHLT_EXTERN LoopInterchange loop_interchange(ForStatement for_stmt, ObjectList<int> permutation);
        //! @}
    }
}

#endif // HLT_INTERCHANGE_HPP
