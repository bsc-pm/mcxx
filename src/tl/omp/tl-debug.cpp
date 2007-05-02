#include "tl-omptransform.hpp"

namespace TL
{
    IdExpression OpenMPTransform::print_id_expression(IdExpression id_expression)
    {
        std::cerr << "-> " << id_expression.prettyprint() << std::endl;

        return id_expression;
    }

    Source OpenMPTransform::debug_parameter_info(
            ObjectList<ParameterInfo> parameter_info_list)
    {
        std::stringstream info;

        info << "Parameter information: " << std::endl;

        if (parameter_info_list.empty())
        {
            info << "No parameters" << std::endl;
        }
        else
            for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                    it != parameter_info_list.end();
                    it++)
            {
                info << "'" << it->parameter_name << "' ";

                if (it->kind == ParameterInfo::BY_VALUE)
                {
                    info << "Passed by value (private pointer). ";
                }
                else if (it->kind == ParameterInfo::BY_POINTER)
                {
                    info << "Passed by reference (global pointer). ";
                }

                info << "Original type: " 
                    << it->type.get_declaration(it->id_expression.get_scope(), "") << ". ";

                info << "Related id-expression: " 
                    << it->id_expression.get_ast().get_locus() << ". ";

                info << std::endl;
            }

        return comment(info.str());
    }
}
