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



#include "tl-templates.hpp"

namespace TL
{
    TemplateParameter::TemplateParameter(template_parameter_t *tpl_param)
        : _tpl_param(tpl_param)
    {
    }

    TemplateParameter::Kind TemplateParameter::get_kind() const
    {
        switch (_tpl_param->kind)
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
        return Symbol(_tpl_param->entry);
    }

    bool TemplateParameter::has_default_argument() const
    {
        return _tpl_param->has_default_argument;
    }

    template_parameter_t* TemplateParameter::get_internal_template_parameter() const
    {
        return _tpl_param;
    }

    std::string TemplateParameter::get_name() const
    {
        return get_symbol().get_name();
    }

    int TemplateParameter::get_position() const
    {
        return _tpl_param->entry->entity_specs.template_parameter_position;
    }

    int TemplateParameter::get_nesting() const
    {
        return _tpl_param->entry->entity_specs.template_parameter_nesting;
    }

    TemplateArgument TemplateParameter::get_default_argument() const
    {
        return TemplateArgument(_tpl_param->default_template_argument);
    }

    TemplateArgument::TemplateArgument(template_argument_t* tpl_arg)
        : _tpl_arg(tpl_arg)
    {
    }

    TemplateArgument::Kind TemplateArgument::get_kind() const
    {
        switch (_tpl_arg->kind)
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
        return  Type(_tpl_arg->type);
    }

    AST_t TemplateArgument::get_expression() const
    {
        return AST_t(_tpl_arg->expression);
    }

    bool TemplateArgument::is_implicit() const
    {
        return _tpl_arg->implicit;
    }

    int TemplateArgument::get_position() const
    {
        return _tpl_arg->position;
    }

    int TemplateArgument::get_nesting() const
    {
        return _tpl_arg->nesting;
    }

    template_argument_t* TemplateArgument::get_internal_template_argument() const
    {
        return _tpl_arg;
    }
}

