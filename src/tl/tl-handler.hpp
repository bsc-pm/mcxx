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
#ifndef TL_HANDLER_HPP
#define TL_HANDLER_HPP

#include <vector>

#include "tl-functor.hpp"

/*
 * Very simplified signal/slot library
 */
namespace TL
{

// Signal 1 parameters
template <class Param1>
class Signal1
{
    private:
        typedef typename std::vector<Functor<void, Param1>* > handlers_type;
        typedef typename handlers_type::iterator handlers_iterator;

        handlers_type _handlers;
    public:
        template <class T>
        void connect(const T& handler)
        {
            T* new_handler = new T(handler);

            _handlers.push_back(new_handler);
        }

        void signal(Param1 p1)
        {
            handlers_iterator it;
            for (it = _handlers.begin(); it != _handlers.end(); it++)
            {
                (*it)->operator()(p1);
            }
        }

        ~Signal1()
        {
        }
};

}

#endif // TL_HANDLER_HPP
