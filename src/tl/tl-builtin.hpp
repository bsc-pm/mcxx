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




#ifndef TL_BUILTIN_HPP
#define TL_BUILTIN_HPP

#include "tl-common.hpp"
#include <typeinfo>
#include <iostream>
#include <string>
#include <map>
#include <tr1/unordered_map>
#include <strings.h>
#include "tl-object.hpp"

namespace TL
{
    //! This class wraps an int within an Object. It behaves like a plain int.
    /*!
     * This class allows to get integer types from extended structures and pass
     * them into TL::DTO objects
     */
    class LIBTL_CLASS Integer : public Object
    {
        private:
            int _i;

        protected:
            virtual tl_type_t* get_extended_attribute(const std::string&) const
            {
                return NULL;
            }
        public:
            Integer(int i)
                : _i(i)
            {
            }

            Integer(const Integer& i)
                : Object(i), _i(i._i)
            {
            }

            virtual operator int() const
            {
                return _i;
            }

            virtual operator bool() const
            {
                return bool(_i);
            }

            Integer operator+(const Integer& j) const
            {
                return Integer(this->_i + j._i);
            }

            Integer operator-(const Integer& j) const
            {
                return Integer(this->_i - j._i);
            }

            Integer operator-() const
            {
                return Integer(-(this->_i));
            }

            Integer operator*(const Integer& j) const
            {
                return Integer(this->_i * j._i);
            }

            Integer operator/(const Integer& j) const
            {
                return Integer(this->_i / j._i);
            }

            Integer operator%(const Integer& j) const
            {
                return Integer(this->_i % j._i);
            }

            Integer& operator=(const Integer& j)
            {
                this->_i = j._i;
                return (*this);
            }

            // Prefix
            Integer& operator++()
            {
                ++(this->_i);
                return (*this);
            }

            // Postfix
            Integer operator++(int) 
            {
                Integer t(this->_i);

                (this->_i)++;

                return t;
            }

            Integer& operator--()
            {
                --(this->_i);
                return (*this);
            }

            // Postfix
            Integer operator--(int)
            {
                Integer t(this->_i);
                (this->_i)--;
                return t;
            }

            bool operator==(Integer& j)
            {
                return (this->_i == j._i);
            }

            bool operator!=(Integer& j)
            {
                return !(this->operator==(j));
            }

            ~Integer() { }
    };

    //! This class wraps a bool within an Object. It behaves like a plain bool.
    /*!
     * This class allows to get bool types from extended structures and pass
     * them into TL::DTO objects
     */
    class LIBTL_CLASS Bool : public Object
    {
        private:
            bool _b;

        protected:
            virtual tl_type_t* get_extended_attribute(const std::string&) const
            {
                return NULL;
            }
        public:

            Bool(bool b)
                : _b(b)
            {
            }

            Bool(const Bool& b)
                : Object(b), _b(b._b)
            {
            }

            virtual operator int() const
            {
                return int(_b);
            }

            virtual operator bool() const
            {
                return _b;
            }

            virtual bool is_bool() const
            {
                return true;
            }

            Bool operator&&(const Bool& b) const
            {
                return Bool(_b && b._b);
            }

            Bool operator||(const Bool& b) const
            {
                return Bool(_b || b._b);
            }

            Bool operator!() const
            {
                return Bool(!_b);
            }

            Bool& operator=(const Bool& b)
            {
                _b = b._b;
                return (*this);
            }

            ~Bool() { }
    };

    //! This class wraps a std::string within an Object. It behaves like a plain std::string.
    /*!
     * This class allows to get std::string types from extended structures and pass
     * them into TL::DTO objects
     */
    class LIBTL_CLASS String : public Object, public std::string
    {
        private:
            bool all_blanks() const
            {
                bool blanks = true;
                int len = this->size();
                for (int i = 0; (i < len) && blanks; i++)
                {
                    blanks &= (this->operator[](i) == ' ') || (this->operator[](i) == '\t');
                }
                return blanks;
            }

        protected:
            virtual tl_type_t* get_extended_attribute(const std::string&) const
            {
                return NULL;
            }
        public:
            String()
                : std::string()
            {
            }

            String( const String& s )
                : Object(s), std::string(s)
            {
            }
            String( size_type len, const char& ch )
                : std::string(len, ch)
            {
            }

            String (const std::string& str)
                : std::string(str)
            {
            }

            String( const char* str )
                : std::string(str)
            {
            }

            String( const char* str, size_type len)
                : std::string(str, len)
            {
            }

            String( const String& str, size_type index, size_type len)
                : std::string(str, index, len)
            {
            }

            virtual bool is_string() const
            {
                return true;
            }

            bool compare_case_insensitive_to(const String& str) const
            {
                return (strcasecmp(this->c_str(), str.c_str()) == 0);
            }

            String& append_with_separator(const String& str, const String& sep) 
            {
                if (this->all_blanks())
                {
                    this->operator=(str);
                }
                else
                {
                    this->operator+=(sep);
                    this->operator+=(str);
                }

                return (*this);
            }

            ~String()
            {
            }
    };

    class LIBTL_CLASS File : public Object
    {
        private:
            FILE* _file;

        protected:
            virtual tl_type_t* get_extended_attribute(const std::string&) const
            {
                return NULL;
            }
        public:
            File()
                : _file(NULL)
            {
            }

            File(FILE *file)
                : _file(file)
            {
            }

            FILE* get_file()
            {
                return _file;
            }

            ~File()
            {
            }
    };
}

#endif // TL_BUILTIN_HPP
