/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

#include "tl-symbol.hpp"
#include "tl-type.hpp"

namespace TL
{
    Schema Symbol::schema(&::scope_entry_extensible_schema);

    tl_type_t* Symbol::get_extended_attribute(const std::string& name) const
    {
        return default_get_extended_attribute(
                &scope_entry_extensible_schema,
                this->_symbol->extended_data,
                name);
    }

    bool Symbol::set_extended_attribute(const std::string &str, const tl_type_t &data)
    {
        return default_set_extended_attribute(
                &scope_entry_extensible_schema,
                this->_symbol->extended_data,
                str,
                data);
    }

    Type Symbol::get_type() const
    {
        Type result(_symbol->type_information);
        return result;
    }

    std::string Symbol::get_name() const
    {
        return (_symbol->symbol_name != NULL) ? 
            std::string(_symbol->symbol_name) : 
            std::string("");
    }

    std::string Symbol::get_qualified_name() const
    {
        if (_symbol->symbol_name == NULL)
        {
            return std::string("");
        }
        else
        {
            // FIXME -> the scope should be the occurrence one
            int max_level = 0;
            char is_dependent = 0;
            const char* qualified_name = get_fully_qualified_symbol_name(_symbol, _symbol->decl_context, 
                    &is_dependent, &max_level);
            return std::string(qualified_name);
        }
    }

    std::string Symbol::get_qualified_name(Scope sc) const
    {
        if (_symbol->symbol_name == NULL)
        {
            return std::string("");
        }
        else
        {
            // FIXME -> the scope should be the occurrence one
            int max_level = 0;
            char is_dependent = 0;
            const char* qualified_name = get_fully_qualified_symbol_name(_symbol, sc._decl_context, 
                    &is_dependent, &max_level);
            return std::string(qualified_name);
        }
    }

    bool Symbol::operator<(Symbol s) const
    {
        return this->_symbol < s._symbol;
    }

    Scope Symbol::get_scope() const
    {
        Scope result(_symbol->decl_context);

        return result;
    }

    Symbol& Symbol::operator=(Symbol s)
    {
        this->_symbol = s._symbol;
        return (*this);
    }

    bool Symbol::operator==(Symbol s) const
    {
        return (this->_symbol == s._symbol);
    }

    bool Symbol::operator!=(Symbol s) const
    {
        return !(this->operator==(s));
    }

    const Symbol Symbol::invalid()
    {
        return Symbol(NULL);
    }

    bool Symbol::is_invalid() const
    {
        return (*this == invalid());
    }

    bool Symbol::is_valid() const
    {
        return !is_invalid();
    }

    // This should be subclassed since it is C/C++ specific
    bool Symbol::is_variable() const
    {
        return (this->_symbol->kind == SK_VARIABLE);
    }

    bool Symbol::is_function() const
    {
        return (this->_symbol->kind == SK_FUNCTION);
    }

    bool Symbol::is_template_function() const
    {
        return (this->_symbol->kind == SK_FUNCTION
                && is_template_specialized_type(this->_symbol->type_information));
    }

    bool Symbol::is_typedef() const
    {
        return (this->_symbol->kind == SK_TYPEDEF);
    }

    bool Symbol::is_typename() const
    {
        return (this->_symbol->kind == SK_TYPEDEF
                || this->_symbol->kind == SK_ENUM
                || this->_symbol->kind == SK_CLASS
                || this->_symbol->kind == SK_TEMPLATE_TYPE_PARAMETER);
    }

    bool Symbol::is_member() const
    {
        return _symbol->entity_specs.is_member;
    }

    bool Symbol::is_artificial() const
    {
        return (_symbol->kind == SK_OTHER);
    }

    Type Symbol::get_class_type() const
    {
        return Type(_symbol->entity_specs.class_type);
    }

    AST_t Symbol::get_point_of_declaration() const
    {
        return AST_t(_symbol->point_of_declaration);
    }

    bool Symbol::is_parameter() const
    {
        return (_symbol->entity_specs.is_parameter);
    }

    int Symbol::get_parameter_position() const
    {
        return (_symbol->entity_specs.parameter_position);
    }

    bool Symbol::is_static() const
    {
        return (_symbol->entity_specs.is_static);
    }

    bool Symbol::is_register() const
    {
        return (_symbol->entity_specs.is_register);
    }

    // FIXME : This only holds if the 'extern' qualifier was given
    // in the declaration of the symbol but global symbols
    // without it are 'extern' too. Using 'is_static' is better
    // till this gets fixed
    bool Symbol::is_extern() const
    {
        return (_symbol->entity_specs.is_extern);
    }

    bool Symbol::is_mutable() const
    {
        return (_symbol->entity_specs.is_mutable);
    }

    // States is a exported template (unused at all)
    bool Symbol::is_exported_template() const
    {
        return (_symbol->entity_specs.is_export);
    }

    // Inlined function
    bool Symbol::is_inline() const
    {
        return (_symbol->entity_specs.is_inline);
    }

    // Virtual function
    bool Symbol::is_virtual() const
    {
        return (_symbol->entity_specs.is_virtual);
    }

    bool Symbol::is_pure() const
    {
        return (_symbol->entity_specs.is_pure);
    }

    bool Symbol::is_conversion_function() const
    {
        return (_symbol->entity_specs.is_conversion);
    }

    // Is a constructor
    bool Symbol::is_constructor() const
    {
        return (_symbol->entity_specs.is_constructor);
    }

    // Is an explicit constructor
    bool Symbol::is_explicit_constructor() const
    {
        return (_symbol->entity_specs.is_explicit);
    }

    bool Symbol::has_initialization() const
    {
        return (_symbol->expression_value != NULL);
    }

    AST_t Symbol::get_initialization() const
    {
        return _symbol->expression_value;
    }

    bool Symbol::has_namespace_scope() const
    {
        return _symbol->decl_context.current_scope != NULL
            && _symbol->decl_context.current_scope->kind == NAMESPACE_SCOPE;
    }

    bool Symbol::has_block_scope() const
    {
        return _symbol->decl_context.current_scope != NULL
            && _symbol->decl_context.current_scope->kind == BLOCK_SCOPE;
    }

    bool Symbol::has_local_scope() const
    {
        return has_block_scope();
    }

    bool Symbol::has_class_scope() const
    {
        return _symbol->decl_context.current_scope != NULL
            && _symbol->decl_context.current_scope->kind == CLASS_SCOPE;
    }

    bool Symbol::has_template_scope() const
    {
        return _symbol->decl_context.current_scope != NULL
            && _symbol->decl_context.current_scope->kind == TEMPLATE_SCOPE;
    }

    bool Symbol::has_prototype_scope() const
    {
        return _symbol->decl_context.current_scope != NULL
            && _symbol->decl_context.current_scope->kind == PROTOTYPE_SCOPE;
    }

    bool Symbol::is_builtin() const
    {
        // Despite the name this applies to variables too
        return _symbol->entity_specs.is_builtin;
    }

    bool Symbol::is_created_after_typedef() const
    {
        return _symbol->entity_specs.after_typedef;
    }
    
    bool Symbol::has_gcc_attribute(const std::string &str) const
    {
        for (int i = 0; i < _symbol->entity_specs.num_gcc_attributes; i++)
        {
            std::string current_gcc_attr(_symbol->entity_specs.gcc_attributes[i].attribute_name);

            if (current_gcc_attr == str)
                return true;
        }

        return false;
    }

    AST_t Symbol::get_argument_of_gcc_attribute(const std::string &str) const
    {
        for (int i = 0; i < _symbol->entity_specs.num_gcc_attributes; i++)
        {
            std::string current_gcc_attr(_symbol->entity_specs.gcc_attributes[i].attribute_name);

            if (current_gcc_attr == str)
            {
                return _symbol->entity_specs.gcc_attributes[i].expression_list;
            }
        }

        return AST_t(NULL);
    }

    bool Symbol::is_defined() const
    {
        return _symbol->defined;
    }
}
