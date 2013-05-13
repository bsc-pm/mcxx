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




#include "hlt-transform.hpp"
#include "hlt-exception.hpp"

using namespace TL;
using namespace TL::HLT;

bool TL::HLT::enable_instrumentation = false;

BaseTransform::BaseTransform()
    : _identity(false), _allow_identity(true), _ostream(std::cerr)
{
}

BaseTransform::BaseTransform(std::ostream &o)
    : _identity(false), _allow_identity(true), _ostream(o)
{
}

BaseTransform::operator Source()
{
    return get_source_impl();
}

BaseTransform::operator std::string()
{
    return get_source_impl();
}

TL::Source BaseTransform::get_source_impl()
{
    if (_identity)
    {
        if (_allow_identity)
        {
            return _identity_tree.prettyprint();
        }
        else
        {
            throw HLTException(_identity_tree, "invalid transformation");
        }
    }
    else
    {
        return this->get_source();
    }
}

void BaseTransform::set_identity(AST_t tree)
{
    _identity = true;
    _identity_tree = tree;
}

BaseTransform& BaseTransform::allow_identity()
{
    _allow_identity = true;
    return *this;
}

BaseTransform& BaseTransform::allow_identity(bool b)
{
    _allow_identity = b;
    return *this;
}

BaseTransform& BaseTransform::disallow_identity()
{
    _allow_identity = false;
    return *this;
}
