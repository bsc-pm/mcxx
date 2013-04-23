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




#ifndef HLT_EXTENSION_HPP
#define HLT_EXTENSION_HPP

#include "hlt-transform.hpp"
#include "tl-langconstruct.hpp"

namespace TL
{
    namespace HLT
    {
        //! \addtogroup HLT High Level Transformations
        //! @{

        //! Extends a function to work on vector arguments
        class LIBHLT_CLASS FunctionExtension : public BaseTransform
        {
            private:
                FunctionDefinition _funct_def;
                Expression *_extension_amount;
                Symbol _function_symbol;
                Source _extension_code;
                std::string _extended_function_name;

                void do_extension();
            public:
                virtual Source get_source();

                FunctionExtension(FunctionDefinition funct_def);

                FunctionExtension& set_extended_function_name(const std::string name);
                FunctionExtension& set_extension_amount(Expression expr);

                virtual ~FunctionExtension()
                {
                    delete _extension_amount;
                }
        };

        FunctionExtension function_extension(FunctionDefinition funct_def,
                Expression extension_amount);
    }
}

#endif // HLT_EXTENSION_HPP
