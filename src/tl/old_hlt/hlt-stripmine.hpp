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




#ifndef HLT_STRIPMINE_HPP
#define HLT_STRIPMINE_HPP


#include "tl-langconstruct.hpp"
#include "hlt-transform.hpp"

namespace TL
{
    namespace HLT
    {
        //! \addtogroup HLT High Level Transformations
        //!* @{

        class LIBHLT_CLASS StripMine : public BaseTransform
        {
            protected:
                virtual Source get_source();

            private:
                ForStatement _for_stmt;
                Source _amount;

            public:
                StripMine(ForStatement for_stmt, Source amount);
        };

        //! Creates a StripMine object
        /*!
          \param for_stmt Regular loop
          \param amount Strip mining amount
         */
        LIBHLT_EXTERN StripMine stripmine_loop(ForStatement for_stmt, Source amount);

        //!* @}
    }
}

#endif // HLT_STRIPMINE_HPP
