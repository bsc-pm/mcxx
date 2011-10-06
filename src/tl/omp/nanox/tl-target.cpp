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


#include "tl-omp-nanox.hpp"
#include "tl-devices.hpp"

using namespace TL;
using namespace TL::Nanox;

void OMPTransform::target_preorder(PragmaCustomConstruct ctr)
{
}

static void remove_target_pragma(PragmaCustomConstruct ctr)
{
    // We allow this to appear both for statements and declarations
    Statement stmt = ctr.get_statement();
    if (stmt.get_ast().is_valid())
    {
        ctr.get_ast().replace(stmt.get_ast());
    }
    else
    {
        ctr.get_ast().replace(ctr.get_declaration());
    }
}

void OMPTransform::target_postorder(PragmaCustomConstruct ctr)
{
    Statement stmt = ctr.get_statement();
    if (stmt.get_ast().is_valid())
    {
        // For statements, silently remove the pragma
        remove_target_pragma(ctr);
        return;
    }

    if (ctr.get_clause("copy_deps").is_defined()
            || ctr.get_clause("copy_in").is_defined()
            || ctr.get_clause("copy_out").is_defined()
            || ctr.get_clause("copy_inout").is_defined())
    {
        std::cerr << ctr.get_ast().get_locus() << ": warning: copy clauses are not considered yet" << std::endl;
    }

    PragmaCustomClause device_clause = ctr.get_clause("device");
    if (!device_clause.is_defined())
    {
        std::cerr << ctr.get_ast().get_locus() << ": warning: '#pragma omp target' requires a device, skipping" << std::endl;
        remove_target_pragma(ctr);
        return;
    }

    ObjectList<std::string> device_list = device_clause.get_arguments(ExpressionTokenizerTrim());

    DeviceHandler &device_handler = DeviceHandler::get_device_handler();
    bool one_is_smp = false;

    for (ObjectList<std::string>::iterator it = device_list.begin();
            it != device_list.end();
            it++)
    {
        // We pass "copies" to all the device providers but 'smp'
        // SMP must be run the last since it we allow it to cause side-effects
        if (*it == "smp")
        {
            one_is_smp = 1;
            continue;
        }

        DeviceProvider* device_provider = device_handler.get_device(*it);

        if (device_provider == NULL)
        {
            internal_error("invalid device '%s' at '%s'\n",
                    it->c_str(), ctr.get_ast().get_locus().c_str());
        }

        if (FunctionDefinition::predicate(ctr.get_declaration()))
        {
            device_provider->insert_function_definition(ctr, /* is_copy */ 1);
        }
        else
        {
            device_provider->insert_declaration(ctr, /* is_copy */ 1);
        }
    }

    // Nobody removed the original tree and there was not smp e.g: 
    //      #pragma target device(cuda, opencl)
    if (one_is_smp)
    {
        DeviceProvider* device_provider = device_handler.get_device("smp");
        if (device_provider == NULL)
        {
            internal_error("invalid device 'smp' at '%s'\n",
                    ctr.get_ast().get_locus().c_str());
        }

        if (FunctionDefinition::predicate(ctr.get_declaration()))
        {
            device_provider->insert_function_definition(ctr, /* is_copy */ 0);
        }
        else
        {
            device_provider->insert_declaration(ctr, /* is_copy */ 0);
        }
    }
    else
    {
        ctr.get_ast().remove_in_list();
    }
}
