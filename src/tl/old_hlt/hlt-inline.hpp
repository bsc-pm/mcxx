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




#ifndef HLT_INLINE_HPP
#define HLT_INLINE_HPP

#include "hlt-common.hpp"
#include "hlt-transform.hpp"
#include "tl-langconstruct.hpp"

namespace TL
{
    namespace HLT
    {
        //! \addtogroup HLT High Level Transformations
        //! @{

        //! Class that implements function inlining
        /*! 
          This class implements inlining for a given function call

          If for some reason the inlining is not possible, the expression
          is left untouched.
         */
        class LIBHLT_CLASS FunctionCallInline : public BaseTransform
        {
            protected:
                virtual Source get_source();
            private:
                Expression _function_call;
                Symbol _function_symbol;
                static const char* inline_prettyprint_callback(AST _a, void* data);
            public:
                //! Constructs a FunctionCallInline object
                /*!
                  \param function_call An Expression of a function call
                 */
                FunctionCallInline(TL::Expression function_call);

        };

        //! Returns a FunctionCallInline object
        /*!
          \param expr An Expression of a function call
         */
        LIBHLT_EXTERN FunctionCallInline inline_function(TL::Expression expr);

        //! @}
    }
}

#endif // HLT_INLINE_HPP
