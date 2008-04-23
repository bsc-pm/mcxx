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

    // Manually increase the reference counter of this object
    this->set_extended_attribute(name, value);
}

void Object::set_attribute(const std::string &name, RefPtr<Object> obj)
{
    tl_type_t value;
    memset(&value, 0, sizeof(value));

    value.kind = TL_OTHER;
    value.data._data = obj.get_pointer();

    // Manually increase the reference counter of this object
    if (this->set_extended_attribute(name, value))
    {
        // Increase the reference counter cause now it is being referenced
        // from the guts of the extended structure
        obj->obj_reference();
    }
}

}
