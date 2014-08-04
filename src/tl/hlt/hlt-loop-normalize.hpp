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



#ifndef HLT_NORMALIZE_LOOP_HPP
#define HLT_NORMALIZE_LOOP_HPP

#include "hlt-transform.hpp"

namespace TL { namespace HLT {

        //! \addtogroup HLT High Level Transformations
        //! @{

        //! Transforms a loop into a one with a step of one
    class LIBHLT_CLASS LoopNormalize
    {
        private:
            Nodecl::NodeclBase _transformation;
            Nodecl::NodeclBase _loop;
        public:
            LoopNormalize();

            // Properties
            LoopNormalize& set_loop(Nodecl::NodeclBase loop);

            // Action
            void normalize();

            // Results
            Nodecl::NodeclBase get_whole_transformation() const { return _transformation; }
    };

} }

#endif //  HLT_NORMALIZE_LOOP_HPP
