/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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



#include "tl-omptransform.hpp"

namespace TL
{
    namespace Nanos4
    {
        Symbol OpenMPTransform::warn_unreferenced_data(Symbol sym)
        {
            std::cerr << "Warning: Entity '" << sym.get_qualified_name() << "' "
                << "is not referenced in the body of the construct" << std::endl;
            return sym;
        }

        Symbol OpenMPTransform::warn_no_data_sharing(Symbol sym)
        {
            std::cerr << "Warning: '" << sym.get_qualified_name() << "' "
                << " does not have a data sharing attribute and 'default(none)' was specified. "
                << "It will be considered shared." << std::endl;
            return sym;
        }
    }
}
