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

#include "tl-lowering-visitor.hpp"
#include "tl-devices.hpp"

namespace TL { namespace Nanox {

    // This Auxiliar visitor is used to traverse the tree and remove from it every
    // declaration or definition of any symbol contained in the '_symbols_to_be_removed' set.
    // Currently, we are handling the following kind of nodes:
    // - Nodecl::FunctionCode
    // - Nodecl::ObjectInit
    // - Nodecl::CxxDecl
    // - Nodecl::CxxDef
    //
    class RemoveCopiedStuffVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            const ObjectList<Symbol> & _symbols_to_be_removed;

        public:
            RemoveCopiedStuffVisitor(const ObjectList<Symbol> & symbols)
                : _symbols_to_be_removed(symbols)
            {
            }

            void visit(const Nodecl::FunctionCode& node)
            {
                if (_symbols_to_be_removed.contains(node.get_symbol()))
                {
                    Nodecl::Utils::remove_from_enclosing_list(node);
                }
            }

            void visit(const Nodecl::ObjectInit& node)
            {
                if (_symbols_to_be_removed.contains(node.get_symbol()))
                {
                    Nodecl::Utils::remove_from_enclosing_list(node);
                }
            }

            void visit(const Nodecl::CxxDecl& node)
            {
                if (_symbols_to_be_removed.contains(node.get_symbol()))
                {
                    Nodecl::Utils::remove_from_enclosing_list(node);
                }
            }

            void visit(const Nodecl::CxxDef& node)
            {
                if (_symbols_to_be_removed.contains(node.get_symbol()))
                {
                    Nodecl::Utils::remove_from_enclosing_list(node);
                }
            }
    };

    void LoweringVisitor::visit(const Nodecl::OmpSs::TargetDeclaration& construct)
    {

        DeviceHandler device_handler = DeviceHandler::get_device_handler();
        Nodecl::List symbols = construct.get_symbols().as<Nodecl::List>();
        Nodecl::List devices = construct.get_devices().as<Nodecl::List>();

        // This set stores the symbols declared/defined by this TargetDeclaration
        TL::ObjectList<Symbol> list_of_symbols;

        // Declarations/Definitions to be copied
        TL::ObjectList<Nodecl::NodeclBase> declarations;
        for (Nodecl::List::iterator it = symbols.begin(); it != symbols.end(); ++it)
        {
            Symbol symbol = it->as<Nodecl::Symbol>().get_symbol();

            // Best effort!
            if (symbol.is_function()
                    && !symbol.get_function_code().is_null())
            {
                declarations.append(symbol.get_function_code());
            }
            else if (symbol.is_variable())
            {
                declarations.append(
                        Nodecl::ObjectInit::make(
                            symbol,
                            construct.get_locus()));
            }
            else
            {
                if (symbol.is_defined())
                {
                    declarations.append(
                            Nodecl::CxxDef::make(
                                /* optative context */ nodecl_null(),
                                symbol,
                                construct.get_locus()));
                }
                else
                {
                    declarations.append(
                            Nodecl::CxxDecl::make(
                                /* optative context */ nodecl_null(),
                                symbol,
                                construct.get_locus()));
                }
            }

            list_of_symbols.append(symbol);
        }

        bool using_device_smp = false;
        for (Nodecl::List::iterator it = devices.begin(); it != devices.end(); ++it)
        {
            Nodecl::Text device_name = (*it).as<Nodecl::Text>();
            using_device_smp = using_device_smp || device_name.get_text() == "smp";

            DeviceProvider* device = device_handler.get_device(device_name.get_text());

            ERROR_CONDITION(device == NULL,
                    "%s: device '%s' has not been loaded",
                    construct.get_locus_str().c_str(),
                    device_name.get_text().c_str());

            device->copy_stuff_to_device_file(declarations);
        }

         if (!using_device_smp)
         {
             // If this target declaration does not use the 'smp' device, we
             // should remove the symbols declared or defined by this target
             // declaration from the original source
             RemoveCopiedStuffVisitor remove_visitor(list_of_symbols);
             remove_visitor.walk(CURRENT_COMPILED_FILE->nodecl);
         }

        // Finally, we can remove the pragma
        Nodecl::Utils::remove_from_enclosing_list(construct);
    }

} }
