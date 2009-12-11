#include "tl-ss2omp.hpp"
#include "tl-augmented-symbol.hpp"
#include "tl-region.hpp"
#include "tl-region-list.hpp"
#include "tl-parameter-region-list.hpp"
#include "cxx-utils.h"

namespace TL
{

    void SS2OpenMP::on_post_task(PragmaCustomConstruct construct)
    {
        AugmentedSymbol augmented_sym = AugmentedSymbol::invalid();
        DeclaredEntity decl_entity(NULL, construct.get_scope_link());
        AST_t context_tree = construct.get_ast();
        if (construct.is_function_definition())
        {
            FunctionDefinition task_definition(construct.get_declaration(),
                    construct.get_scope_link());
            decl_entity = task_definition.get_declared_entity();

            augmented_sym = AugmentedSymbol(task_definition.get_function_symbol());

            context_tree = task_definition.get_function_body().get_ast();
        }
        else
        {
            Declaration decl(construct.get_declaration(), construct.get_scope_link());
            decl_entity = decl.get_declared_entities()[0];

            augmented_sym = AugmentedSymbol(decl_entity.get_declared_symbol());

            context_tree = decl_entity.get_parameter_declarations()[0].get_ast();
        }

        if (!augmented_sym.is_task())
        {
            internal_error("This is not a task!", 0);
        }

        ObjectList<ParameterDeclaration> parameter_decls = decl_entity.get_parameter_declarations();

        RefPtr<ParameterRegionList> parameter_region_list = augmented_sym.get_parameter_region_list();

        Source new_pragma_construct_src, clauses;
        new_pragma_construct_src
            << "#pragma omp task " << clauses << "\n"
            << ";"
            ;

        Source input_clause_args;
        Source output_clause_args;
        Source inout_clause_args;

        ObjectList<Type> parameters = augmented_sym.get_type().parameters();
        int i = 0;
        for (ObjectList<RegionList>::iterator it = parameter_region_list->begin();
                it != parameter_region_list->end();
                it++)
        {
            RegionList &region_list(*it);

            if (region_list.size() > 1)
            {
                running_error("%s: error: regions with more than one dependence are not supported\n",
                        construct.get_ast().get_locus().c_str());
            }

            Region &region(region_list[0]);

            Source *clause_args = NULL;
            switch ((int)region.get_direction())
            {
                case Region::INPUT_DIR:
                    {
                        clause_args = &input_clause_args;
                        break;
                    }
                case Region::OUTPUT_DIR:
                    {
                        clause_args = &output_clause_args;
                        break;
                    }
                case Region::INOUT_DIR:
                    {
                        clause_args = &inout_clause_args;
                        break;
                    }
                default:
                    {
                        internal_error("Invalid directionality", 0);
                        break;
                    }
            }

            // std::cerr << "Parameter region #" << i 
            //     << " direction=" << dir_str 
            //     << " dimension_count=" << region.get_dimension_count()
            //     << " is_full=" << region.is_full()
            //     << std::endl;

            if (region.get_dimension_count() == 0)
            {
                // Two cases: a scalar or a pointer if it is a scalar there is
                // no need to state anything
                if (parameters[i].is_pointer())
                {
                    clause_args->append_with_separator(
                            "*" + parameter_decls[i].get_name().prettyprint(),
                            ",");
                }
            }
            else
            {
                Source array_sections;
                for (Region::iterator it = region.begin();
                        it != region.end();
                        it++)
                {
                    Region::DimensionSpecifier*& dim_spec(*it);

                    // array_sections += 
                    //     "[ " + dim_spec->get_dimension_start().prettyprint() + " : "
                    //     "(" + dim_spec->get_accessed_length().prettyprint() + ")-(" 
                    //            + dim_spec->get_dimension_start().prettyprint() + ") - 1 ]";

                    Source lower_bound_src, upper_bound_src;

                    array_sections
                        << "["
                        << lower_bound_src
                        << ":"
                        << upper_bound_src
                        << "]"
                        ;

                    lower_bound_src << dim_spec->get_dimension_start()
                        ;

                    // Simplify if possible the upper bound, otherwise the
                    // resulting expression is not nice
                    upper_bound_src << "(" << dim_spec->get_accessed_length() << ")-("
                        << dim_spec->get_dimension_start().prettyprint() << ") - 1";

                    AST_t upper_bound_tree = upper_bound_src.parse_expression(context_tree, 
                            construct.get_scope_link());

                    Expression upper_bound_expr(upper_bound_tree, construct.get_scope_link());

                    if (upper_bound_expr.is_constant())
                    {
                        bool valid = false;
                        int cexpr = upper_bound_expr.evaluate_constant_int_expression(valid);
                        if (valid)
                        {
                            upper_bound_src = (Source() << cexpr);
                        }
                    }
                }

                clause_args->append_with_separator(
                        parameter_decls[i].get_name().prettyprint() + array_sections.get_source(),
                        ",");
            }

            i++;
        }

        if (!input_clause_args.empty())
        {
            clauses << " input(" << input_clause_args << ")";
        }
        if (!output_clause_args.empty())
        {
            clauses << " output(" << output_clause_args << ")";
        }
        if (!inout_clause_args.empty())
        {
            clauses << " inout(" << inout_clause_args << ")";
        }

        // std::cerr << new_pragma_construct_src.get_source() << std::endl;
        AST_t new_pragma_tree_list = new_pragma_construct_src.parse_declaration(construct.get_ast(),
                construct.get_scope_link());

        ASTIterator iterator_list = new_pragma_tree_list.get_list_iterator();

        AST_t new_pragma_tree = iterator_list.item();
        PragmaCustomConstruct new_pragma_construct(new_pragma_tree, construct.get_scope_link());

        AST_t new_pragma_line = new_pragma_construct.get_pragma_line();
        construct.get_pragma_line().replace(new_pragma_line);
        construct.get_ast().replace_text("omp");
    }

    void SS2OpenMP::on_post_target(PragmaCustomConstruct construct)
    {
    }
}

EXPORT_PHASE(TL::SS2OpenMP)
