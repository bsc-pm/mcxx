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

    static Symbol get_symbol_of_data_reference(Expression expr)
    {
        if (expr.is_id_expression())
        {
            IdExpression id_expr = expr.get_id_expression();
            return id_expr.get_symbol();
        }
        else if (expr.is_unary_operation())
        {
            if (expr.get_operation_kind() == Expression::DERREFERENCE)
            {
                Expression ref = expr.get_unary_operand();

                if (ref.is_unary_operation()
                        && ref.get_operation_kind() == Expression::REFERENCE)
                    return get_symbol_of_data_reference(ref.get_unary_operand());
                else
                    return get_symbol_of_data_reference(ref);
            }
        }
        else if (expr.is_array_subscript())
        {
            return get_symbol_of_data_reference(expr.get_subscripted_expression());
        }
        else if (expr.is_array_section_range()
                || expr.is_array_section_size())
        {
            return get_symbol_of_data_reference(expr.array_section_item());
        }
        else if (expr.is_shaping_expression())
        {
            return get_symbol_of_data_reference(expr.shaped_expression());
        }
        else
        {
            internal_error("Invalid expression kind", 0);
        }
    }

    static void add_data_sharings(ObjectList<Expression> &expression_list, 
            DataSharingEnvironment& data_sharing, 
            DependencyDirection dep_attr)
    {
        for (ObjectList<Expression>::iterator it = expression_list.begin();
                it != expression_list.end();
                it++)
        {
            DataReference expr(*it);
            if (!expr.is_valid())
            {
                std::cerr << expr.get_ast().get_locus() 
                    << ": warning: skipping invalid dependency expression '" << expr.prettyprint() << "'" << std::endl;
                continue;
            }

            DependencyItem dep_item(*it, dep_attr);

            Symbol sym = expr.get_base_symbol();
            DataSharingAttribute ds_attr = data_sharing.get_data_sharing(sym);

            if ((dep_attr & DEP_FIRSTPRIVATE) != DEP_FIRSTPRIVATE)
            {
                if (expr.is_id_expression())
                {
                    if (((ds_attr & DS_UNDEFINED) != DS_UNDEFINED)
                            && ((ds_attr & DS_IMPLICIT) != DS_IMPLICIT)
                            && ((ds_attr & DS_SHARED) != DS_SHARED))
                    {
                        std::cerr << expr.get_ast().get_locus()
                            << ": warning: symbol '" << sym.get_qualified_name() << "' has a non-shared data sharing, overwriting to shared" << std::endl;
                    }
                    data_sharing.set_data_sharing(sym, (DataSharingAttribute)(DS_SHARED | DS_IMPLICIT));
                }
            }

            data_sharing.add_dependence(dep_item);
        }
    }

    void Core::get_dependences_info(PragmaCustomConstruct construct, DataSharingEnvironment& data_sharing)
    {
        PragmaCustomClause input_clause = construct.get_clause("input");
        get_dependences_info_clause(input_clause, data_sharing, DEP_DIR_INPUT);

        PragmaCustomClause output_clause = construct.get_clause("output");
        get_dependences_info_clause(output_clause, data_sharing, DEP_DIR_OUTPUT);

        PragmaCustomClause inout_clause = construct.get_clause("inout");
        get_dependences_info_clause(inout_clause, data_sharing, DEP_DIR_INOUT);

        PragmaCustomClause fp_input_clause = construct.get_clause("__fp_input");
        get_dependences_info_clause(fp_input_clause, data_sharing, 
                (OpenMP::DependencyDirection)(DEP_DIR_INPUT | DEP_FIRSTPRIVATE));

        PragmaCustomClause fp_output_clause = construct.get_clause("__fp_output");
        get_dependences_info_clause(fp_output_clause, data_sharing, 
                (OpenMP::DependencyDirection)(DEP_DIR_OUTPUT | DEP_FIRSTPRIVATE));

        PragmaCustomClause fp_inout_clause = construct.get_clause("__fp_inout");
        get_dependences_info_clause(fp_inout_clause, data_sharing, 
                (OpenMP::DependencyDirection)(DEP_DIR_INOUT | DEP_FIRSTPRIVATE));

        PragmaCustomClause fp_reduction_clause = construct.get_clause("__fp_reduction");
        get_dependences_info_clause(fp_reduction_clause, data_sharing, 
                (OpenMP::DependencyDirection)(DEP_REDUCTION | DEP_FIRSTPRIVATE));
    }

    void Core::get_dependences_info_clause(PragmaCustomClause clause,
           DataSharingEnvironment& data_sharing,
           DependencyDirection dep_attr)
    {
        if (clause.is_defined())
        {
            ObjectList<Expression> expr_list = clause.get_expression_list();
            add_data_sharings(expr_list, data_sharing, dep_attr);
        }
    }
} }
