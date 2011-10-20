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



#include "tl-omp-core.hpp"
#include "tl-omp-deps.hpp"

namespace TL { namespace OpenMP {

    static void add_data_sharings(ObjectList<Nodecl::NodeclBase> &expression_list, 
            DataSharingEnvironment& data_sharing, 
            DependencyDirection dep_attr)
    {
        for (ObjectList<Nodecl::NodeclBase>::iterator it = expression_list.begin();
                it != expression_list.end();
                it++)
        {
            DataReference expr(*it);
            std::string warning;
            if (!expr.is_valid(warning))
            {
                std::cerr << warning;
                std::cerr << expr.get_locus() 
                    << ": warning: skipping invalid dependency expression '" << expr.prettyprint() << "'" << std::endl;
                continue;
            }

            DependencyItem dep_item(*it, dep_attr);

            Symbol sym = expr.get_base_symbol();
            DataSharingAttribute ds_attr = data_sharing.get_data_sharing(sym);
            Type data_type = expr.get_data_type();

            // Arguable if we have T (&)[10] (a reference to array)
            if (data_type.is_reference())
            {
                data_type = data_type.references_to();
            }

            if (expr.is<Nodecl::Symbol>() || sym.is_member())
            {
                data_sharing.set_data_sharing(sym, (DataSharingAttribute)(DS_SHARED | DS_IMPLICIT));
            }
            else
            {
                Type sym_type = sym.get_type();
                if (sym_type.is_reference())
                {
                    sym_type = sym_type.references_to();
                }

                if (sym_type.is_array())
                {
                    data_sharing.set_data_sharing(sym, (DataSharingAttribute)(DS_SHARED | DS_IMPLICIT));
                }
                else
                {
                    data_sharing.set_data_sharing(sym, (DataSharingAttribute)(DS_FIRSTPRIVATE | DS_IMPLICIT));
                }
            }
            data_sharing.add_dependence(dep_item);
        }
    }

    void Core::get_dependences_info(TL::PragmaCustomLine construct, DataSharingEnvironment& data_sharing)
    {
        PragmaCustomClause input_clause = construct.get_clause("input");
        get_dependences_info_clause(input_clause, data_sharing, DEP_DIR_INPUT);

        PragmaCustomClause output_clause = construct.get_clause("output");
        get_dependences_info_clause(output_clause, data_sharing, DEP_DIR_OUTPUT);

        PragmaCustomClause inout_clause = construct.get_clause("inout");
        get_dependences_info_clause(inout_clause, data_sharing, DEP_DIR_INOUT);

        PragmaCustomClause concurrent_clause = construct.get_clause("concurrent");
        get_dependences_info_clause(concurrent_clause, data_sharing, 
                (OpenMP::DependencyDirection)(DEP_REDUCTION));

        PragmaCustomClause fp_input_clause = construct.get_clause("__fp_input");
        get_dependences_info_clause(fp_input_clause, data_sharing, 
                (OpenMP::DependencyDirection)(DEP_DIR_INPUT));

        PragmaCustomClause fp_output_clause = construct.get_clause("__fp_output");
        get_dependences_info_clause(fp_output_clause, data_sharing, 
                (OpenMP::DependencyDirection)(DEP_DIR_OUTPUT));

        PragmaCustomClause fp_inout_clause = construct.get_clause("__fp_inout");
        get_dependences_info_clause(fp_inout_clause, data_sharing, 
                (OpenMP::DependencyDirection)(DEP_DIR_INOUT));

        // Same meaning as 'concurrent'
        PragmaCustomClause fp_reduction_clause = construct.get_clause("__fp_reduction");
        get_dependences_info_clause(fp_reduction_clause, data_sharing, 
                (OpenMP::DependencyDirection)(DEP_REDUCTION));
    }

    void Core::get_dependences_info_clause(PragmaCustomClause clause,
           DataSharingEnvironment& data_sharing,
           DependencyDirection dep_attr)
    {
        if (clause.is_defined())
        {
            ObjectList<Nodecl::NodeclBase> expr_list = clause.get_arguments_as_expressions();
            add_data_sharings(expr_list, data_sharing, dep_attr);
        }
    }
} }
