/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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




#include "tl-templates.hpp"

namespace TL
{
    TemplateParameter::TemplateParameter(template_parameter_list_t *tpl_param, int index)
        : _tpl_param(tpl_param), _index(index)
    {
    }

    TemplateParameter::Kind TemplateParameter::get_kind() const
    {
        switch (_tpl_param->parameters[_index]->kind)
        {
            case TPK_TYPE:
                return TemplateParameter::TYPE;
            case TPK_NONTYPE:
                return TemplateParameter::NONTYPE;
            case TPK_TEMPLATE:
                return TemplateParameter::TEMPLATE;
            default:
                return TemplateParameter::INVALID;
        }
    }

    Symbol TemplateParameter::get_symbol() const
    {
        return Symbol(_tpl_param->parameters[_index]->entry);
    }

    bool TemplateParameter::has_default_argument() const
    {
        return _tpl_param->arguments[_index]->is_default;
    }

    std::string TemplateParameter::get_name() const
    {
        return get_symbol().get_name();
    }

    int TemplateParameter::get_position() const
    {
        return _tpl_param->parameters[_index]->entry->entity_specs.template_parameter_position;
    }

    int TemplateParameter::get_nesting() const
    {
        return _tpl_param->parameters[_index]->entry->entity_specs.template_parameter_nesting;
    }

    TemplateArgument TemplateParameter::get_default_argument() const
    {
        return TemplateArgument(_tpl_param, _index);
    }

    TemplateArgument::TemplateArgument(template_parameter_list_t* tpl_arg, int index)
        : _tpl_param(tpl_arg), _index(index)
    {
    }

    TemplateArgument::Kind TemplateArgument::get_kind() const
    {
        switch (_tpl_param->arguments[_index]->kind)
        {
            case TPK_TYPE:
                return TemplateArgument::TYPE;
            case TPK_NONTYPE:
                return TemplateArgument::NONTYPE;
            case TPK_TEMPLATE:
                return TemplateArgument::TEMPLATE;
            default:
                return TemplateArgument::INVALID;
        }
    }

    Type TemplateArgument::get_type() const
    {
        return  Type(_tpl_param->arguments[_index]->type);
    }

    AST_t TemplateArgument::get_expression() const
    {
        return AST_t();
        // return AST_t(_tpl_param->arguments[_index]->expression);
    }

    bool TemplateArgument::is_implicit() const
    {
        return false;
    }

    int TemplateArgument::get_position() const
    {
        return _index;
    }

    int TemplateArgument::get_nesting() const
    {
        return ::get_template_nesting_of_template_parameters(_tpl_param);
    }
}

