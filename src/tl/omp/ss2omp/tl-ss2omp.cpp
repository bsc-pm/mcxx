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
        if (construct.is_function_definition())
        {
            FunctionDefinition task_definition(construct.get_declaration(),
                    construct.get_scope_link());

            augmented_sym = AugmentedSymbol(task_definition.get_function_symbol());
        }
        else
        {
            Declaration decl(construct.get_declaration(), construct.get_scope_link());

            augmented_sym = AugmentedSymbol(decl.get_declared_entities()[0].get_declared_symbol());
        }

        if (!augmented_sym.is_task())
        {
            internal_error("This is not a task!", 0);
        }

        RefPtr<ParameterRegionList> parameter_region_list = augmented_sym.get_parameter_region_list();

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

            switch ((int)region.get_direction())
            {
                case Region::INPUT_DIR:
                    {
                        break;
                    }
                case Region::OUTPUT_DIR:
                    {
                        break;
                    }
                case Region::INOUT_DIR:
                    {
                        break;
                    }
                default:
                    {
                        internal_error("Invalid directionality", 0);
                        break;
                    }
            }
        }
    }

    void SS2OpenMP::on_post_target(PragmaCustomConstruct construct)
    {
    }
}

EXPORT_PHASE(TL::SS2OpenMP)
