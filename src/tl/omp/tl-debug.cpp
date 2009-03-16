/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include "tl-omptransform.hpp"

namespace TL
{
    namespace Nanos4
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
                    info << "Symbol: '" << it->parameter_name << "' ";

                    if (it->kind == ParameterInfo::BY_VALUE)
                    {
                        info << "Passed by value (private pointer). ";
                    }
                    else if (it->kind == ParameterInfo::BY_POINTER)
                    {
                        info << "Passed by reference (global pointer). ";
                    }

                    // info << "Original type: " 
                    //     << it->type.get_declaration(it->id_expression.get_scope(), "") << ". ";

                    // info << "Related id-expression: " 
                    //     << it->id_expression.get_ast().get_locus() << ". ";

                    info << std::endl;
                }

            return comment(info.str());
        }
    }
}
