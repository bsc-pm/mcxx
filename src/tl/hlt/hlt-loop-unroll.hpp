/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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




#ifndef HLT_UNROLL_HPP
#define HLT_UNROLL_HPP

#include "tl-nodecl.hpp"
#include "hlt-transform.hpp"

namespace TL
{
    namespace HLT
    {
        //! \addtogroup HLT High Level Transformations
        //! @{

        //! Unrolls a regular loop a given number of times
        /*!
          This class implements loop unrolling. Loop unrolling
          repeats the body of the loop in the loop itself, adjusting
          the stride and creating, if necessary an epilog loop.
          */
        class LIBHLT_CLASS LoopUnroll : public Transform
        {
            private:
                Nodecl::NodeclBase _loop, _transformation, _unrolled, _epilog;
                int _unroll_factor;
                bool _create_epilog;
            public:
                LoopUnroll();

                // Properties
                LoopUnroll& set_loop(Nodecl::NodeclBase loop);
                LoopUnroll& set_unroll_factor(int n);
                LoopUnroll& set_create_epilog(bool b);

                // Action
                void unroll();

                // Results
                Nodecl::NodeclBase get_whole_transformation() const { return _transformation; }

                Nodecl::NodeclBase get_unrolled_loop() const { return _unrolled; }
                Nodecl::NodeclBase get_epilog_loop() const { return _epilog; }
        };

        //! @}
    }
}

#endif // HLT_UNROLL_HPP
