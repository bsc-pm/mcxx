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




#ifndef TL_EXTERNALVARS_HPP
#define TL_EXTERNALVARS_HPP

#include "tl-common.hpp"

#ifndef FROM_TL_EXTERNALVARS_CPP
#warning "This is a deprecated interface and should not be used anymore"
#endif

namespace TL
{
    //! External variables interface
    /*!
     * \deprecated This class is deprecated, instead use the interface of compiler parameter phases
     * as defined in TL::CompilerPhase
     */
    class LIBTL_CLASS ExternalVars
    {
        public:
            // Gets the value of a variable passed in the command line
            /*!
             * \deprecated This function is deprecated. Instead use the interface of compiler parameter
             * phases as defined in TL::CompilerPhase
             *
             * \param name The name of the external parameter passed in the command line
             * \param default_val If the parameter is not actually passed then return this value
             */
            static std::string get(const std::string& name, const std::string& default_val = "") DEPRECATED;
    };
}

#endif // TL_EXTERNALVARS_HPP
