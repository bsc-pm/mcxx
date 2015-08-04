/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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


#ifndef TL_FUNCTOR_HPP
#define TL_FUNCTOR_HPP

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "tl-common.hpp"
#include <functional>
#include "cxx-macros.h"

#if !defined(HAVE_CXX11)
#include <tr1/functional>
namespace std
{
    using std::tr1::function;
    using std::tr1::bind;
    // Note: std::tr1::result_of behaves slightly different to std::result_of
    // so we prefer to make sure we acknowledge the difference
    // Note: std::tr1::result_of requires a result_type if a function object
    // is created using a class

    namespace placeholders {
        using std::tr1::placeholders::_1;
        // using std::tr1::placeholders::_2;
    }
}
#endif

template <typename T>
struct LiftPointer;

template <typename S, typename T>
struct LiftPointer<S(T)>
{
    std::function<S(const T&)> f;
    LiftPointer(const std::function<S(const T&)>& f_)
        : f(f_) { }

    S operator ()(const T* pt)
    {
        return f(*pt);
    }
};

template <typename S, typename T>
std::function<S(const T* const &)> lift_pointer(const std::function<S(const T&)>& f)
{
    LiftPointer<S(T)> lifted(f);
    return lifted;
}


#endif // TL_FUNCTOR_HPP
