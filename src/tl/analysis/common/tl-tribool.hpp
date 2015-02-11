/*--------------------------------------------------------------------
 ( C) Copyright 2006-2014 Barcelona Supercomputing Center             *
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

#ifndef TL_TRIBOOL
#define TL_TRIBOOL

namespace TL
{
    // Ternary valued logic: true, false or unknown
    struct tribool
    {
        public:

            enum TriboolValue
            {
                False = 0,
                True = 1,
                Unknown = 2,

                // Convenience aliases
                No = False,
                Yes = True,
                Undefined = Unknown,
                no = False,
                yes = True,
                unknown = Unknown,
                undefined = Unknown,
            };

        private:

            TriboolValue _value;

        public:

        tribool() : _value(Unknown) { }
        tribool(bool b) : _value(b ? True : False) { }
        tribool(TriboolValue v) : _value(v) { }
        tribool(const tribool& t) : _value(t._value) { }

        bool is_false() const { return _value == False; }
        bool is_true() const { return _value == True; }
        bool is_unknown() const { return _value == Unknown; }

        TriboolValue value() const { return _value; }

        tribool& operator=(const tribool& t)
        {
            if (this != &t)
            {
                this->_value = t._value;
            }
            return *this;
        }

        bool operator==(const tribool& t) const
        {
            return this->_value == t._value;
        }

        // Not
        tribool operator!() const
        {
            if (is_unknown())
                return tribool();

            return tribool(!is_true());
        }
    };
    
    // And
    inline tribool operator&&(const tribool &lhs, const tribool& rhs)
    {
        if (!lhs.is_unknown() && !rhs.is_unknown())
            return tribool(lhs.is_true() && rhs.is_true());
        else if (lhs.is_false() || rhs.is_false())
            return tribool(false);
        else
            return tribool();
    }

    // Or
    inline tribool operator||(const tribool& lhs, const tribool& rhs)
    {
        if (!lhs.is_unknown() && !rhs.is_unknown())
            return tribool(lhs.is_true() || rhs.is_true());
        else if (lhs.is_true() || rhs.is_true())
            return tribool(true);
        else
            return tribool();
    }
}

#endif

