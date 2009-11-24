#include "tl-omp-core.hpp"

namespace TL { namespace OpenMP {

    static bool check_for_dep_expression(Expression expr, Symbol &base_sym)
    {
        if (expr.is_id_expression())
        {
            base_sym = expr.get_id_expression().get_symbol();
            return true;
        }
        else if (expr.is_array_subscript())
        {
            // Restrict the array syntax to arrays
            return expr.get_subscripted_expression().get_type().is_array()
                && check_for_dep_expression(expr.get_subscripted_expression(), base_sym);
        }
        else if (expr.is_array_section())
        {
            // Restrict the array section syntax to arrays
            return expr.get_subscripted_expression().get_type().is_array()
                && check_for_dep_expression(expr.array_section_item(), base_sym);
        }
        else if (expr.is_member_access())
        {
            return check_for_dep_expression(expr.get_accessed_entity(), base_sym);
        }
        else
        {
            return false;
        }
    }

    static void add_data_sharings(ObjectList<Expression> &expression_list, 
            DataSharing& data_sharing, 
            DependencyItem::DependencyAttribute attr)
    {
        for (ObjectList<Expression>::iterator it = expression_list.begin();
                it != expression_list.end();
                it++)
        {
            // FIXME - This is the base symbol of the dependence
            Expression& expr(*it);
            Symbol base_sym(NULL);
            if (!check_for_dep_expression(expr, base_sym))
            {
                DependencyItem dep_item(base_sym, it->get_ast(), attr);
                data_sharing.add_dependence(dep_item);
            }
            else
            {
                std::cerr << expr.get_ast().get_locus() 
                    << ": warning: '" << expr.prettyprint() << "' is not a valid dependency specification, skipping" << std::endl;
            }
        }
    }

    void Core::get_dependences_info(PragmaCustomConstruct construct, DataSharing& data_sharing)
    {
        // Input
        PragmaCustomClause input_clause = construct.get_clause("input");

        if (input_clause.is_defined())
        {
            // FIXME - We need something more than simple id-expressions
            ObjectList<Expression> input_id_expr = input_clause.get_expression_list();
            add_data_sharings(input_id_expr, data_sharing, DependencyItem::INPUT);
        }

        PragmaCustomClause output_clause = construct.get_clause("output");

        if (input_clause.is_defined())
        {
            // FIXME - We need something more than simple id-expressions
            ObjectList<Expression> input_id_expr = input_clause.get_expression_list();
            add_data_sharings(input_id_expr, data_sharing, DependencyItem::OUTPUT);
        }
    }

} }
