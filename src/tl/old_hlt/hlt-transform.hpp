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

#include "tl-ast.hpp"
#include "tl-source.hpp"
#include "hlt-common.hpp"
#include <string>
#include <iostream>

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
        struct LIBHLT_CLASS BaseTransform
        {
            private:
                AST_t _identity_tree;
                bool _identity;
                bool _allow_identity;
                Source get_source_impl();

            protected:
                //! Every subclass of HLT will implement this
                /*!
                   This function implements the transformation and returns
                   the transformed code
                 */
                virtual Source get_source() = 0;
                /*!
                  Sets that this transformation will not do anything but
                  returning \a tree as identity code

                  \param tree Returned code representing the identity of the transformation
                 */
                void set_identity(AST_t tree);

                //! Where warnings or error messages can be written
                /*!
                  By default this is std::cerr
                 */
                std::ostream &_ostream;
            public:
                 operator Source();
                 operator std::string();

                 BaseTransform();
                 //! Constructs a BaseTransform specifying the output stream
                 BaseTransform(std::ostream &o);
                 virtual ~BaseTransform() { }

                 //! Allows an identity transformation
                 BaseTransform& allow_identity();
                 //! Allows or disallows identity transformation
                 /*!
                   \param b If true allows identity, otherwise is it is disallowed
                  */
                 BaseTransform& allow_identity(bool b);

                 //! Disallows identity transformation
                 BaseTransform& disallow_identity();
        };

        enum InstrumentationEvents
        {
            TASK_INVALID = 0,
            TASK_CODE = 9000001,
            TASK_OVERHEAD,
        };

        extern bool enable_instrumentation;
        //! @}
    }
}

#endif // HLT_TRANSFORM_HPP
