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




#ifndef HLT_FUSION_HPP
#define HLT_FUSION_HPP

#include "hlt-transform.hpp"
#include "tl-langconstruct.hpp"

namespace TL
{
    namespace HLT
    {
        //! \addtogroup HLT High Level Transformations
        //! @{

        //! This class implements loop fusion 
        /*!
          Loop fusion is a loop transformation where a set of loops
          with the same iteration space is fused into a single loop.
         */
        class LIBHLT_CLASS LoopFusion : public BaseTransform
        {
            private:
                ObjectList<ForStatement> _for_stmt_list;
                Source do_fusion();
            protected:
                virtual Source get_source();
            public:
                //! Creates a LoopFusion object
                /*!
                  \param for_stmt_list List of regular for statements
                  */
                LoopFusion(ObjectList<ForStatement> for_stmt_list);
        };

        //! Creates a LoopFusion object
        /*!
          \param for_stmt_list List of regular for statements
         */
        LIBHLT_EXTERN LoopFusion loop_fusion(ObjectList<ForStatement> for_stmt_list);
        //! @}
    }
}

#endif // HLT_FUSION_HPP
