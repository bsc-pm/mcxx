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




#ifndef HLT_COLLAPSE_HPP
#define HLT_COLLAPSE_HPP

#include "hlt-transform.hpp"
#include "tl-langconstruct.hpp"
#include "tl-for-nest.hpp"

namespace TL
{
    namespace HLT
    {
        //! \addtogroup HLT High Level Transformations
        //! @{

        //! Class that implements loop collapsing
        /*!
          This class collapses a perfect loop nest into a single
          loop that performs the same iterations as the original
          loop nest.
         */
        class LIBHLT_CLASS LoopCollapse : public BaseTransform
        {
            private:
                ForNestInfo _for_nest_info;

                int _nest_level;

                bool _induction_private;

                bool _split_transform;
                Source *_header;

                bool _keep_ancillary_names;
                ObjectList<std::string> *_ancillary_names;
            protected:
                virtual Source get_source();
            public:
                //! Constructs a LoopCollapse object
                /*!
                  \param for_stmt Perfect loop nest
                  */
                LoopCollapse(ForStatement for_stmt);

                LoopCollapse& set_nesting_level(int n);

                LoopCollapse& set_split_transform(Source &header);

                LoopCollapse& set_induction_private(bool b);

                LoopCollapse& keep_ancillary_names(ObjectList<std::string>& ancillary_names);
        };

        //! Constructs a LoopCollapse object
        /*!
          \param for_stmt Perfect loop nest
         */
        LIBHLT_EXTERN LoopCollapse loop_collapse(ForStatement for_stmt);

        //! @}
    }
}

#endif // HLT_COLLAPSE_HPP
