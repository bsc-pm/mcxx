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


#ifndef HLT_TRANSFORM_HPP
#define HLT_TRANSFORM_HPP

#include "tl-nodecl.hpp"
#include "hlt-common.hpp"

namespace TL
{
    //! High Level Transformations for TL
    namespace HLT
    {
        //! \addtogroup HLT High Level Transformations
        //! @{

        //! Base for all transformations
        /*!
          All HLT transformations derive from this class.
         */
        struct LIBHLT_CLASS Transform
        {
            public:

                /*! States if the passed tree is suitable for the transformation
                 *
                 * \param diagnostic If true the check routine should diagnose why the tree is not suitable
                 */
                virtual bool check(bool diagnostic) = 0;

                /*! Performs the transformation
                 * \pre Transform::check should have returned true
                 */
                virtual void perform() = 0;
                bool check() { return this->check(false); }

                Transform(Nodecl::NodeclBase n) { }

            private:
                // Not copiable
                Transform();
                Transform(const Transform&);
                Transform& operator=(const Transform&);
        };

        //! @}
    }
}

#endif // HLT_TRANSFORM_HPP

