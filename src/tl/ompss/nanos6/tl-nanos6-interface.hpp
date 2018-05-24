/*--------------------------------------------------------------------
  (C) Copyright 2016-2016 Barcelona Supercomputing Center
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


#ifndef TL_NANOS6_INTERFACE_HPP
#define TL_NANOS6_INTERFACE_HPP

#include <map>
#include <string>

namespace TL { namespace Nanos6 {

class Interface
{
    private:

        /**
         * This map associates each family with its version. Note that families
         * that are defined in the Nanos6 headers have always a non-negative
         * version, but the type of the version in this map is 'const int'. The
         * reason to have this type is that we use the '-1' value to specify
         * that a certain family was not present in the runtime headers.
         */
        static std::map<const std::string, const int> _map;

        /** Deleted constructor. This class should never be instantiated */
        Interface() {}

    public:

        /**
         * this function returns whether the real version of the 'family' family is at least
         * the 'expected_version'. Note that it returns false if the family is not present.
         */
        static bool family_is_at_least(const std::string& family, unsigned int expected_version);

        /**
         * this function returns whether the real version of the 'family' family is the 'expected_version'.
         * Note that it returns false if the family is not present.
         */
        static bool family_is(const std::string& family, unsigned int expected_version);

        /**
         * this function emits a fatal error if the 'family' family is not at least the 'expected_version'.
         * The 'feature' string is the feature that requires at least that version of the runtime.
         */
        static void family_must_be_at_least(
                const std::string& family, unsigned int expected_version, const std::string& feature);

        /**
         * this function emits a fatal error if the 'family' family is not  the 'expected_version'.
         * The 'feature' string is the feature that requires that version of the runtime.
         */
        static void family_must_be(
                const std::string& family, unsigned int expected_version, const std::string& feature);

        /**
         * This function checks whether the Nanos6 headers are deprecated
         */
        static void check_nanos6_deprecated_headers();
};

}}
#endif // TL_NANOS6_INTERFACE_HPP
