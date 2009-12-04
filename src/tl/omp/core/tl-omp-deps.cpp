#include "tl-omp-core.hpp"
#include "tl-omp-deps.hpp"

namespace TL { namespace OpenMP {

    static bool check_for_dep_expression(Expression expr)
    {
        bool is_lvalue = false;
        expr.get_type(is_lvalue);

        if (!is_lvalue)
        {
            std::cerr << expr.get_ast().get_locus() 
                << ": warning: dependency expression '" << expr.prettyprint() << "' is not a lvalue" << std::endl;
        }

        return is_lvalue;
    }

    static void add_data_sharings(ObjectList<Expression> &expression_list, 
            DataSharingEnvironment& data_sharing, 
            DependencyDirection attr)
    {
        for (ObjectList<Expression>::iterator it = expression_list.begin();
                it != expression_list.end();
                it++)
        {
            // FIXME - This is the base symbol of the dependence
            Expression& expr(*it);
            if (check_for_dep_expression(expr))
            {
                DependencyItem dep_item(*it, attr);

                if (expr.is_id_expression())
                {
                    Symbol sym = expr.get_id_expression().get_computed_symbol();
                    if (sym.is_valid())
                    {
                        DataSharingAttribute attr = data_sharing.get(sym);

                        if (((attr & DS_PRIVATE) == DS_PRIVATE)
                                && ((attr & DS_IMPLICIT) != DS_IMPLICIT))
                        {
                            std::cerr << expr.get_ast().get_locus()
                                << ": warning: dependency expression '" 
                                << expr.prettyprint() << "' names a private variable, making it shared" << std::endl;

                            data_sharing.set(sym, attr);
                        }

                        data_sharing.set(sym, (DataSharingAttribute)(DS_SHARED | DS_IMPLICIT));
                        dep_item.set_symbol_dependence(sym);
                    }
                    else
                    {
                        std::cerr << expr.get_ast().get_locus() 
                            << ": warning: skipping invalid dependency expression '" << expr.prettyprint() << "'" << std::endl;
                        continue;
                    }
                }

                data_sharing.add_dependence(dep_item);
            }
            else
            {
                std::cerr << expr.get_ast().get_locus() 
                    << ": warning: skipping invalid dependency expression '" << expr.prettyprint() << "'" << std::endl;
            }
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
