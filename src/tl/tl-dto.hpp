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




#ifndef TL_DTO_HPP
#define TL_DTO_HPP

#include "tl-common.hpp"
#include <string>
#include <map>
#include "tl-object.hpp"
#include "tl-objectlist.hpp"

#include <memory>

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
            typedef std::map<std::string, std::shared_ptr<Object> > DTO_inner;
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
            std::shared_ptr<Object> operator[](const std::string& str)
            {
                DTO_inner::iterator it = _dto.find(str);
                if (it == _dto.end())
                {
                    return std::shared_ptr<Undefined>(new Undefined);
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
            void set_object(const std::string& str, std::shared_ptr<Object> obj)
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

            //! Returns a reference to a named object
            /*!
             * \param str The name to retrieve the object.
             *
             * This function can only be used to get data
             * previously registered in the DTO using
             * set_object.
             */
            void set_value(const std::string& str, std::shared_ptr<Object> obj)
			{
				DTO_inner::iterator it = _dto.find(str);
				if (it != _dto.end())
				{
					it->second = obj;
				}
			}
    };
}

#endif // TL_DTO_HPP
