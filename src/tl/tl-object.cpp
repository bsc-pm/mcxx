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
                // When nobody else holds a reference to obj_data it will have
                // a reference of one.
                Object* obj_data = reinterpret_cast<Object*>(tl_value->data._data);

                // But we need to return a RefPtr<Object>
                // Constructing 'ref_obj_data' does not change the reference count
                // of 'obj_data'.
                RefPtr<Object> ref_obj_data(obj_data);
                // so we do it now
                obj_data->obj_reference();

                // Now we can safely copy this new reference to obj_data
                result = ref_obj_data;
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
    // Mark another reference
    obj->obj_reference();

    tl_type_t* tl_ptr_value = this->get_extended_attribute(name);
    if (tl_ptr_value != NULL)
    {
        if (tl_ptr_value->kind == TL_OTHER)
        {
            // Unreference the existing one
            ((Object*)(tl_ptr_value->data._data))->obj_unreference();
        }
    }

    tl_type_t value;
    memset(&value, 0, sizeof(value));

    value.kind = TL_OTHER;
    value.data._data = obj.get_pointer();

    this->set_extended_attribute(name, value);
}

tl_type_t* default_get_extended_attribute(
        extensible_struct_t* extensible_struct, 
        const std::string& name)
{
    tl_type_t* p = NULL;
    if (extensible_struct != NULL)
    {
        p = (tl_type_t*)extensible_struct_get_field(extensible_struct, name.c_str());
    }
    return p;
}

bool default_set_extended_attribute(
        extensible_struct_t* extensible_struct, 
        const std::string &field_name, const tl_type_t &data)
{
    tl_type_t* new_tl_data = new tl_type_t(data);
    extensible_struct_set_field(extensible_struct,
            field_name.c_str(), new_tl_data);

    return true;
}

}
