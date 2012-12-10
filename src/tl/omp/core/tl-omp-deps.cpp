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
            if (!expr.is_valid())
            {
                std::cerr << expr.get_error_log();
                std::cerr << expr.get_locus()
                    << ": error: skipping invalid dependency expression '" << expr.prettyprint() << "'" << std::endl;
                continue;
            }

            DependencyItem dep_item(*it, dep_attr);

            Symbol sym = expr.get_base_symbol();

            // Note that in general a dependency should be shared
            //
            //   inout(x)    x must be shared
            //
            // But we allow more general cases. In these cases x, is not going to be shared
            // and it will be left to the default data sharing
            //
            //   inout(*x)             We do not define a specific data sharing for these
            //   inout(x[10])
            //   inout(x[1:2])
            //   inout([10][20] x)
            //
            // Note, though, that if the base symbol 'x' is an array, it will always be shared.
            //
            if (expr.is<Nodecl::Symbol>()
                    || sym.get_type().is_array()
                    || (sym.get_type().is_any_reference()
                        && sym.get_type().references_to().is_array()))
            {
                data_sharing.set_data_sharing(sym, (DataSharingAttribute)(DS_SHARED | DS_IMPLICIT));
            }

            data_sharing.add_dependence(dep_item);
        }
    }

    void Core::get_dependences_info(TL::PragmaCustomLine construct, DataSharingEnvironment& data_sharing)
    {
        PragmaCustomClause input_clause = construct.get_clause("in",
                /* deprecated */ "input");
        get_dependences_info_clause(input_clause, data_sharing, DEP_DIR_IN);

        PragmaCustomClause output_clause = construct.get_clause("out",
                /* deprecated */ "output");
        get_dependences_info_clause(output_clause, data_sharing, DEP_DIR_OUT);

        PragmaCustomClause inout_clause = construct.get_clause("inout");
        get_dependences_info_clause(inout_clause, data_sharing, DEP_DIR_INOUT);

        PragmaCustomClause concurrent_clause = construct.get_clause("concurrent");
        get_dependences_info_clause(concurrent_clause, data_sharing,
                DEP_CONCURRENT);

        PragmaCustomClause commutative_clause = construct.get_clause("commutative");
        get_dependences_info_clause(commutative_clause, data_sharing,
                DEP_COMMUTATIVE);
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
