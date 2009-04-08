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
#include "hlt-transform.hpp"

using namespace TL::HLT;

BaseTransform::BaseTransform()
    : _identity(false), _ostream(std::cerr)
{
}

BaseTransform::BaseTransform(std::ostream &o)
    : _identity(false), _ostream(o)
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
        return _identity_src;
    }
    else
    {
        return this->get_source();
    }
}

void BaseTransform::set_identity(const Source &src)
{
    _identity = true;
    _identity_src = src;
}
