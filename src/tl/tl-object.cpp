/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#include "tl-builtin.hpp"
#include "tl-object.hpp"
#include "tl-ast.hpp"
#include "tl-symbol.hpp"

#include <cstdlib>
#include <cstring>

namespace TL
{

bool Object::has_attribute(const std::string& name) const
{
    return (this->get_extended_attribute(name) != NULL);
}

RefPtr<Object> Object::get_attribute(const std::string& name) const
{
    RefPtr<Object> result;
    tl_type_t* tl_value = this->get_extended_attribute(name);

    if (tl_value == NULL)
    {
        std::cerr << "Attribute '" << name << "' not found" << std::endl;
        result = RefPtr<Undefined>(new Undefined());
        return result;
    }

    switch (tl_value->kind)
    {
        case TL_INTEGER :
            {
                result = RefPtr<Integer>(new Integer(tl_value->data._integer));
                return result;
            }
        case TL_BOOL :
            {
                result = RefPtr<Bool>(new Bool(tl_value->data._boolean));
                return result;
            }
        case TL_AST :
            {
                result = RefPtr<AST_t>(new AST_t(tl_value->data._ast));
                return result;
            }
        case TL_STRING :
            {
                result = RefPtr<String>(new String(tl_value->data._string));
                return result;
            }
        case TL_SYMBOL :
            {
                result = RefPtr<Symbol>(new Symbol(tl_value->data._entry));
                return result;
            }
        case TL_TYPE:
            {
                result = RefPtr<Type>(new Type(tl_value->data._type));
                return result;
            }
        case TL_OTHER :
            {
                // Generic case
                result = RefPtr<Object>(reinterpret_cast<Object*>(tl_value->data._data));
                return result;
            }
        case TL_UNDEFINED :
            {
                break;
            }
    }

    // If we reach here simply return an undefined type
    result = RefPtr<Undefined>(new Undefined());
    return result;
}

void Object::set_attribute(const std::string &name, bool b)
{
    tl_type_t value = tl_bool(b);

    this->set_extended_attribute(name, value);
}

void Object::set_attribute(const std::string &name, int i)
{
    tl_type_t value = tl_integer(i);

    this->set_extended_attribute(name, value);
}

void Object::set_attribute(const std::string &name, RefPtr<Object> obj)
{
    tl_type_t value;
    memset(&value, 0, sizeof(value));

    value.kind = TL_OTHER;
    value.data._data = obj.get_pointer();

    // This will increase the counter if needed
    this->set_extended_attribute(name, value);
}

static tl_type_t found_but_not_set;
tl_type_t* default_get_extended_attribute(
        extensible_schema_t* extensible_schema, 
        extensible_struct_t* extensible_struct, 
        const std::string& name)
{
    //  First get the extended attribute
    char found = 0;
    void* p = extensible_struct_get_field_pointer_lazy(extensible_schema,
            extensible_struct,
            name.c_str(),
            &found);

    if (found)
    {
        if (p == NULL)
        {
            // It was found but nobody wrote on this attribute
            // Clear the static return type
            memset(&found_but_not_set, 0, sizeof(found_but_not_set));
            return &found_but_not_set;
        }
        else
        {
            return (tl_type_t*)p;
        }
    }
    else 
    {
        return NULL;
    }
}

bool default_set_extended_attribute(
        extensible_schema_t* extensible_schema, 
        extensible_struct_t* extensible_struct, 
        const std::string &str, const tl_type_t &data)
{
    extensible_schema_add_field_if_needed(extensible_schema,
            str.c_str(), sizeof(data));

    void *p = extensible_struct_get_field_pointer(extensible_schema,
            extensible_struct,
            str.c_str());
    
    // Something happened
    if (p == NULL)
        return false;

    tl_type_t* tl_value = reinterpret_cast<tl_type_t*>(p);
    
    if (tl_value->kind == TL_OTHER
            && tl_value->data._data != NULL)
    {
        // Decrease the reference if there was an Object
        reinterpret_cast<Object*>(tl_value->data._data)->obj_unreference();
    }

    // Write
    *tl_value = data;

    if (data.kind == TL_OTHER
            // What an unfortunate name
            && data.data._data != NULL)
    {
        // Increase the reference if it is an Object
        reinterpret_cast<Object*>(data.data._data)->obj_reference();
    }

    // Data was written
    return true;
}

}
