/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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

namespace TL
{

bool Object::has_attribute(const std::string& name) const
{
    return (this->get_extended_attribute(name) != NULL);
}

static Undefined und;

Object& Object::get_attribute(const std::string& name) const
{
    // Fix this function because it leaks everythime you call it
    //
    //   * we want to return a reference in order to make it convertible to any
    //   derived class of Object without slicing the result
    //
    //   * returning a pointer makes the code ugly thus returning a reference works
    //   * maybe using another approach like
    //
    //     void get_attribute(const std::string& name, <derived-class-of-object>& result);
    //     
    //     would be better (one function for every type)

    tl_type_t* tl_value = this->get_extended_attribute(name);

    if (tl_value == NULL)
    {
        std::cerr << "Attribute '" << name << "' not found" << std::endl;
        return und;
    }

    switch (tl_value->kind)
    {
        case TL_INTEGER :
            {
                Integer* i = new Integer(tl_value->data._integer);
                return (*i);
                break;
            }
        case TL_BOOL :
            {
                Bool* b = new Bool(tl_value->data._boolean);
                return (*b);
                break;
            }
        case TL_AST :
            {
                AST_t* ast = new AST_t(tl_value->data._ast);
                return (*ast);
                break;
            }
        case TL_STRING :
            {
                String* str = new String(tl_value->data._string);
                return (*str);
                break;
            }
        case TL_SYMBOL :
            {
                Symbol* sym = new Symbol(tl_value->data._entry);
                return (*sym);
                break;
            }
        case TL_ARRAY :
            {
                std::cerr << "Unimplemented TL Array" << std::endl;
                break;
            }
        case TL_UNDEFINED :
            {
// #warning Implement this
                break;
            }
    }

    return und;
}

}
