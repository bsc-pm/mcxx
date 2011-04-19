/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

#include "nanox-smp.hpp"
#include "tl-data-env.hpp"
#include "tl-counters.hpp"
#include "tl-parallel-common.hpp"
#include "tl-devices.hpp"

using namespace TL;
using namespace TL::Nanox;


static Source perform_reduction_symbol( const OpenMP::ReductionSymbol& reduction_symbol,
                                        Source reduction_var_name, Source partial_reduction, ScopeLink sl)
{
    OpenMP::UDRInfoItem2 udr2 = reduction_symbol.get_udr_2();
    Source result;

    if (udr2.is_builtin_operator())
    {
        ReplaceSrcIdExpression replace_udr_builtin(sl);
        replace_udr_builtin.add_replacement(udr2.get_out_symbol(), reduction_var_name);
        replace_udr_builtin.add_replacement(udr2.get_in_symbol(), partial_reduction);

 std::cout << "UDR2: " << udr2.get_out_symbol().get_name() << ", " << reduction_var_name.get_source() << "\n";
 std::cout << "UDR2: " << udr2.get_in_symbol().get_name() << ", " << partial_reduction.get_source() << "\n";

        result 
                << replace_udr_builtin.replace(udr2.get_combine_expr())
                << ";"
                ;
        return result;
    }
    else
    {
        std::string func_name = udr2.get_function_definition_symbol().get_qualified_name(true);
        
        C_LANGUAGE()
        {
            result
            << func_name
            << "( "
            << "&" << reduction_var_name
            << ", "
            << "&" << partial_reduction
            << " )"
            << ";"
            ;
        }
        
        CXX_LANGUAGE()
        {
            result
            << func_name
            << "( "
            << reduction_var_name
            << ", "
            << partial_reduction
            << " )"
            << ";"
            ;
        }
    }
    
    return result;
}


static Source get_reduction_gathering(ObjectList<OpenMP::ReductionSymbol> reduction_references, ScopeLink sl)
{
    Source reduction_gathering;
    
    // For every entity being reduced
    for (ObjectList<OpenMP::ReductionSymbol>::iterator it = reduction_references.begin();
            it != reduction_references.end();
            it++)
    {
        // We are lacking a scope here for the qualified name
        std::string reduction_var_name = it->get_symbol().get_qualified_name();
        
        // Get the operator involved
        Source partial_reduction;
        partial_reduction
                << "rdv_" << it->get_symbol().get_name() << "[rdv_i]"
                ;
        
        reduction_gathering
                << perform_reduction_symbol(*it, reduction_var_name, partial_reduction, sl)
                ;
    }
    
    return reduction_gathering;
}

Source DeviceSMP::get_reduction_update(ObjectList<OpenMP::ReductionSymbol> reduction_references, ScopeLink sl)
{
    Source reduction_update;
    
    if (reduction_references.empty())
    {
        return reduction_update;
    }
    
    reduction_update 
            <<    "int nanos_thread_id = omp_get_thread_num();"
            ;
    
    for (ObjectList<OpenMP::ReductionSymbol>::iterator it = reduction_references.begin();
            it != reduction_references.end();
            it++)
    {
        const std::string reduction_var_name = "_args->rdv_" + it->get_symbol().get_name() + "[nanos_thread_id]";
        const std::string partial_reduction = "rdp_" + it->get_symbol().get_name();
        OpenMP::UDRInfoItem2 udr2 = it->get_udr_2();
        
        if (udr2.is_builtin_operator())
        {
            ReplaceSrcIdExpression replace_udr_builtin(sl);
            replace_udr_builtin.add_replacement(udr2.get_out_symbol(), reduction_var_name);
            replace_udr_builtin.add_replacement(udr2.get_in_symbol(), partial_reduction);
            
            reduction_update 
                << replace_udr_builtin.replace(udr2.get_combine_expr())
                << ";"
            ;
        }
        else
        {
            std::string func_name = udr2.get_function_definition_symbol().get_qualified_name(true);
            
            C_LANGUAGE()
            {
                reduction_update
                << func_name
                << "( "
                << "&" << reduction_var_name
                << ", "
                << "&" << partial_reduction
                << " )"
                << ";"
                ;
            }
            
            CXX_LANGUAGE()
            {
                reduction_update
                << func_name
                << "( "
                << reduction_var_name
                << ", "
                << partial_reduction
                << " )"
                << ";"
                ;
            }
        }
    }
    
    return reduction_update;
}

Source DeviceSMP::get_reduction_code(ObjectList<OpenMP::ReductionSymbol> reduction_references, ScopeLink sl)
{
    Source reduction_code;
    
    if (reduction_references.empty())
    {
        return reduction_code;
    }
    
    // Source code that gathers the values computed by every thread
    Source reduction_gathering;
    Source reduction_update;
    
    reduction_code
            << comment("Reduction code performed after the join")
            << "int rdv_i;"
            << "for (rdv_i = 0; rdv_i < omp_get_num_threads(); rdv_i++)"
            << "{"
            <<    reduction_gathering
            << "}"
            ;
    
    reduction_gathering = get_reduction_gathering(reduction_references, sl);
    
    return reduction_code;
}




