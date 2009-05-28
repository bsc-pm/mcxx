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
#ifndef TL_DTO_HPP
#define TL_DTO_HPP

#include "tl-common.hpp"
#include <string>
#include <map>
#include "tl-object.hpp"
#include "tl-objectlist.hpp"
#include "tl-refptr.hpp"

//! TL classes for compiler phases
namespace TL
{
    //! Class type of the object used to pass information along the compiler phase pipeline
    /*!
     * This class implements in some way the pattern Data Transfer Object, hence the name,
     * to pass data in a generic way among objects.
     */
    class LIBTL_CLASS DTO 
    {
        private:
            typedef std::map<std::string, RefPtr<Object> > DTO_inner;
            //! Inner representation of the data transfer object
            DTO_inner _dto;
        public :
            //! Returns a reference to a named object
            /*!
             * \param str The name to retrieve the object.
             *
             * This function can only be used to get data
             * previously registered in the DTO using
             * set_object.
             */
            RefPtr<Object> operator[](const std::string& str)
            {
                DTO_inner::iterator it = _dto.find(str);
                if (it == _dto.end())
                {
                    return RefPtr<Undefined>(new Undefined);
                }
                else
                {
                    return it->second;
                }
            }

            //! Adds an object into the DTO so it is available
            //to further phases.
            /*!
             * \param str The key name used to retrieve later this object
             * \param obj The object stored under the name \a str
             */
            void set_object(const std::string& str, RefPtr<Object> obj)
            {
                _dto[str] = obj;
            }

            //! Returns all the keys registered in this DTO
            ObjectList<std::string> get_keys() const
            {
                ObjectList<std::string> result;

                for (DTO_inner::const_iterator it = _dto.begin();
                        it != _dto.end();
                        it++)
                {
                    result.insert(it->first);
                }

                return result;
            }
    };
}

#endif // TL_DTO_HPP
