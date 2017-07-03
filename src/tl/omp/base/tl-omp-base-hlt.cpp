#include "tl-omp-base.hpp"
#include "hlt-loop-collapse.hpp"
#include "tl-pragmasupport.hpp"

#include "cxx-cexpr.h"

namespace TL { namespace OpenMP {

    void loop_hlt_handler_post(TL::PragmaCustomStatement construct)
    {
        TL::PragmaCustomLine pragma_line = construct.get_pragma_line();
        TL::PragmaCustomClause collapse = construct.get_pragma_line().get_clause("collapse");
        if (!collapse.is_defined())
            return;

        TL::ObjectList<Nodecl::NodeclBase> expr_list = collapse.get_arguments_as_expressions(construct);
        if (expr_list.size() != 1)
        {
            error_printf_at(construct.get_locus(), "'collapse' clause needs exactly one argument\n");
            return;
        }

        Nodecl::NodeclBase expr = expr_list[0];
        if (!expr.is_constant()
                || !is_any_int_type(expr.get_type().get_internal_type()))
        {
            error_printf_at(construct.get_locus(),
                    "'collapse' clause requires an integer constant expression\n");
            return;
        }

        int collapse_factor = const_value_cast_to_signed_int(expr.get_constant());

        if (collapse_factor <= 0)
        {
            error_printf_at(
                    construct.get_locus(),
                    "Non-positive factor (%d) is not allowed in the 'collapse' clause\n",
                    collapse_factor);
        }
        else if (collapse_factor == 1)
        {
            // Removing the collapse clause from the pragma
            pragma_line.remove_clause("collapse");
        }
        else if (collapse_factor > 1)
        {
            Nodecl::NodeclBase loop = get_statement_from_pragma(construct);

            HLT::LoopCollapse loop_collapse;
            loop_collapse.set_loop(loop);
            loop_collapse.set_pragma_context(construct.retrieve_context());
            loop_collapse.set_collapse_factor(collapse_factor);

            loop_collapse.collapse();

            Nodecl::NodeclBase transformed_code = loop_collapse.get_whole_transformation();
            TL::ObjectList<TL::Symbol> capture_symbols = loop_collapse.get_omp_capture_symbols();

            // We may need to add some symbols that are used to implement the collapse clause to the pragma
            std::string names;
            for (TL::ObjectList<TL::Symbol>::iterator it = capture_symbols.begin();
                    it != capture_symbols.end();
                    it++)
            {
                if (it != capture_symbols.begin())
                    names += ",";
                names += it->get_name();
            }
            Nodecl::List clauses = pragma_line.get_clauses().as<Nodecl::List>();
            clauses.append(Nodecl::PragmaCustomClause::make(Nodecl::List::make(Nodecl::PragmaClauseArg::make(names)), "firstprivate"));

            // Removing the collapse clause from the pragma
            pragma_line.remove_clause("collapse");

            // Create a new pragma over the new for stmt
            ERROR_CONDITION(!transformed_code.is<Nodecl::Context>(), "Unexpected node\n", 0);
            Nodecl::NodeclBase compound_statement =
                transformed_code.as<Nodecl::Context>().get_in_context().as<Nodecl::List>().front();

            ERROR_CONDITION(!compound_statement.is<Nodecl::CompoundStatement>(), "Unexpected node\n", 0);
            Nodecl::Context context_for_stmt =
                compound_statement.as<Nodecl::CompoundStatement>().get_statements()
                .as<Nodecl::List>().find_first<Nodecl::Context>();

            Nodecl::Utils::remove_from_enclosing_list(context_for_stmt);

            Nodecl::List stmt_list =
                compound_statement.as<Nodecl::CompoundStatement>().get_statements().as<Nodecl::List>();
            ERROR_CONDITION(stmt_list.is_null(), "Unreachable code\n", 0);

            Nodecl::PragmaCustomStatement new_pragma =
                Nodecl::PragmaCustomStatement::make(pragma_line,
                        Nodecl::List::make(context_for_stmt),
                        construct.get_text(),
                        construct.get_locus());

            stmt_list.append(new_pragma);

            construct.replace(transformed_code);
        }
    }


    void Base::apply_openmp_high_level_transformations(Nodecl::NodeclBase translation_unit)
    {
        PragmaMapDispatcher map_dispatcher;

        map_dispatcher["omp"].statement.post["taskloop"].connect(
                std::bind((void (*)(TL::PragmaCustomStatement))&loop_hlt_handler_post, std::placeholders::_1));
        map_dispatcher["omp"].statement.post["for"].connect(
                std::bind((void (*)(TL::PragmaCustomStatement))&loop_hlt_handler_post, std::placeholders::_1));
        map_dispatcher["omp"].statement.post["parallelfor"].connect(
                std::bind((void (*)(TL::PragmaCustomStatement))&loop_hlt_handler_post, std::placeholders::_1));
        map_dispatcher["omp"].statement.post["do"].connect(
                std::bind((void (*)(TL::PragmaCustomStatement))&loop_hlt_handler_post, std::placeholders::_1));
        map_dispatcher["omp"].statement.post["paralleldo"].connect(
                std::bind((void (*)(TL::PragmaCustomStatement))&loop_hlt_handler_post, std::placeholders::_1));

        map_dispatcher["oss"].statement.post["taskloop"].connect(
                std::bind((void (*)(TL::PragmaCustomStatement))&loop_hlt_handler_post, std::placeholders::_1));

        PragmaVisitor visitor(map_dispatcher, /* ignore_template_functions */ CURRENT_CONFIGURATION->explicit_instantiation);

        visitor.walk(translation_unit);
    }

}}
