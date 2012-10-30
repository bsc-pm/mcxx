/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#include "tl-lowering-visitor.hpp"
#include "tl-devices.hpp"

namespace TL { namespace Nanox {

        void LoweringVisitor::visit(const Nodecl::OpenMP::TargetDeclaration& construct)
        {
            Nodecl::Utils::remove_from_enclosing_list(construct);
        }

        void LoweringVisitor::visit(const Nodecl::OpenMP::TargetDefinition& construct)
        {
            bool remove_symbols = false;
            DeviceHandler device_handler = DeviceHandler::get_device_handler();
            Nodecl::List symbols = construct.get_symbols().as<Nodecl::List>();
            Nodecl::List devices = construct.get_devices().as<Nodecl::List>();
            for (Nodecl::List::iterator it = devices.begin(); it != devices.end(); ++it)
            {
                Nodecl::Text device_name = (*it).as<Nodecl::Text>();

                DeviceProvider* device = device_handler.get_device(device_name.get_text());

                ERROR_CONDITION(device == NULL,
                        "%s: device '%s' has not been loaded",
                        construct.get_locus().c_str(),
                        device_name.get_text().c_str());

                bool result = device->copy_stuff_to_device_file(symbols);
                remove_symbols = remove_symbols || result;
            }

            if (remove_symbols)
            {
                for (Nodecl::List::iterator it = symbols.begin(); it != symbols.end(); ++it)
                {
                    Symbol sym = (*it).as<Nodecl::Symbol>().get_symbol();
                    if (sym.is_function()
                            && !sym.get_function_code().is_null())
                    {
                        Nodecl::Utils::remove_from_enclosing_list(sym.get_function_code());
                    }

                }
            }

            Nodecl::Utils::remove_from_enclosing_list(construct);
        }

} }
