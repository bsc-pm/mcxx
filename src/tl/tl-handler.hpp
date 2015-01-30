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




#ifndef TL_HANDLER_HPP
#define TL_HANDLER_HPP

#include "tl-common.hpp"
#include <vector>

#include "tl-functor.hpp"

/*
 * Very simplified signal/slot library
 */
namespace TL
{
    //! \addtogroup Functors
    //! @{

    //! Class representing a Signal with only one parameter
    template <class Param1>
    class Signal1
    {
        private:
            typedef typename std::vector<std::function<void(Param1)> > handlers_type;
            typedef typename handlers_type::iterator handlers_iterator;


            template <typename T>
                struct AdjustParam
                {
                    typedef const T& type;
                };

            template <typename T>
                struct AdjustParam<const T&>
                {
                    typedef const T& type;
                };

            template <typename T>
                struct AdjustParam<T&>
                {
                    typedef T& type;
                };

            handlers_type _handlers;
        public:
            //! Connects a functor to this signal
            template <class T>
                void connect(const T& handler)
                {
                    _handlers.push_back(handler);
                }
    
            //! Signals all the connected functors with given parameter
            void signal(typename AdjustParam<Param1>::type p1)
            {
                handlers_iterator it;
                for (it = _handlers.begin(); it != _handlers.end(); it++)
                {
                    it->operator()(p1);
                }
            }
    };
    
    //! @}
}

#endif // TL_HANDLER_HPP
