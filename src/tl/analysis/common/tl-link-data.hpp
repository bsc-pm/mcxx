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

#ifndef TL_LINK_DATA_HPP
#define TL_LINK_DATA_HPP

#include "tl-common.hpp"
#include <cstring>  // NULL
#include <tr1/unordered_map>

namespace TL {
namespace Analysis {

    //! This class allows linking arbitrary named data into any class.
    /*!
     * This class has its own memory management. It can be copied but
     * it will not duplicate its contents but increase a number of copies
     * counter. In destruction this number is decreased, when it reaches zero
     * the whole structure will be deleted.
     */
    class LIBTL_CLASS LinkData
    {
    private:
        //! Data info structure
        struct data_info
        {
            //! The data itself
            void *data;
            //! The destructor function
            void (*destructor)(void*);

            static void do_nothing(void*) { }

            data_info()
                : data(NULL), destructor(data_info::do_nothing)
            {}
        };
        
        // This is a pointer so this class can be copied
        typedef std::tr1::unordered_map<int, data_info> Dict;
        Dict *_data_list;
        int *_num_copies;

        void release_code();

    public:

        //! Creates a new LinkData object.
        /*!
         * Will set the number of copies counter to 1 and
         * create the unordered map of data.
         */
        LinkData();

        //! Copy constructor
        /*!
            * It does not duplicate the data, but increases a shared number of
            * copies counter.
            */
        LinkData(const LinkData& l);

        //! Destructor adaptor of any given type
        /*!
         * This function is used to preserve type safety in C++ and to
         * allow proper constructors be called.
         */
        template <typename _T>
        static void destroy_adapter(void* p)
        {
            _T* t = reinterpret_cast<_T*>(p);
            delete t;
        }

        //! Retrieves the data with key
        /*!
         * \param key The name of the data. If it was not retrieved never
         * before, a default construction will happen.
         * \param t Copy constructor value
         *
         * The requested type must be default constructible. Calling
         * this function with a same name and different types with same (or
         * shared) LinkData objects will fail miserably.
         */
        template <typename _T>
        _T& get_data(const int key, const _T& t = _T())
        {
            _T* result = NULL;
            if (_data_list->find(key) == _data_list->end())
            {
                result = new _T(t);

                data_info d;
                d.data = result;
                d.destructor = destroy_adapter<_T>;

                (*_data_list)[key] = d;
            }

            data_info d = (*_data_list)[key];
            result = reinterpret_cast<_T*>(d.data);

            return *result;
        }

        //! Retrieves the data with key
        /*!
         * \param key The name of the data. If it was not retrieved never
         * before, a default construction will happen.
         * \param data The data to be set.
         *
         * The requested type must be default constructible. Calling
         * this function with a same name and different types with same (or
         * shared) LinkData objects will fail miserably.
         */
        template <typename _T>
        void set_data(const int key, const _T& data)
        {
            data_info &d = (*_data_list)[key];
            d.destructor(d.data);

            d.data = new _T(data);
            d.destructor = destroy_adapter<_T>;
        }

        LinkData& operator=(const LinkData&);

        bool has_key(const int key) const
        {
            return (_data_list->find(key) != _data_list->end());
        }

        //! Destroy object
        /*!
         * This destructor decreases the number of copies counter.
         * If it reaches zero, all data information is properly freed.
         */
        ~LinkData();
    };
}
}

#endif // TL_LINK_DATA_HPP
