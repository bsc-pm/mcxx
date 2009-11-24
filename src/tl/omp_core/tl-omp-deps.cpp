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
            if (check_for_dep_expression(expr, base_sym))
            {
                DependencyItem dep_item(base_sym, it->get_ast(), attr);

                if ((data_sharing.get(base_sym) & DA_SHARED) != DA_SHARED)
                {
                    if ((data_sharing.get(base_sym) & DA_PRIVATE) != DA_PRIVATE)
                    {
                        std::cerr << expr.get_ast().get_locus() 
                            << ": warning: dependency specification '" << expr.prettyprint() 
                            << "' has a related variable whose data sharing is not shared, setting it to shared" << std::endl;
                        data_sharing.set(base_sym, DA_SHARED);
                    }
                    else
                    {
                        // std::cerr << expr.get_ast().get_locus() 
                        //     << ": warning: dependency specification '" << expr.prettyprint() 
                        //     << "' has a related variable whose data sharing is private, this is likely to fail" << std::endl;
                        running_error("%s: error: related variable '%s' of dependency specification '%s' is private, "
                                "it must be shared if referenced in an 'input' or an 'output' clause\n",
                                expr.get_ast().get_locus().c_str(),
                                expr.prettyprint().c_str(),
                                base_sym.get_name().c_str());
                    }
                }

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
        PragmaCustomClause input_clause = construct.get_clause("input");
        if (input_clause.is_defined())
        {
            ObjectList<Expression> input_expr = input_clause.get_expression_list();
            add_data_sharings(input_expr, data_sharing, DependencyItem::INPUT);
        }

        PragmaCustomClause output_clause = construct.get_clause("output");
        if (output_clause.is_defined())
        {
            ObjectList<Expression> output_expr = output_clause.get_expression_list();
            add_data_sharings(output_expr, data_sharing, DependencyItem::OUTPUT);
        }
    }

} }
