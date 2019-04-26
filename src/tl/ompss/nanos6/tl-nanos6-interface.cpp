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

#include "tl-nanos6-interface.hpp"

#include "tl-nodecl.hpp"
#include "tl-scope.hpp"
#include "tl-symbol.hpp"
#include "tl-compilerpipeline.hpp"

#include "cxx-diagnostic.h"
#include "cxx-cexpr.h"
#include<iostream>
namespace TL { namespace Nanos6 {

    // Since _map is a static data member we have to define it here
    std::map<const std::string, const int> Interface::_map;

    int get_version_for_family(const std::string &family, std::map<const std::string, const int> &map)
    {
        int real_version;
        std::map<const std::string, const int>::iterator it = map.find(family);
        if (it != map.end())
        {
            real_version = it->second;
        }
        else
        {
            TL::Symbol family_enumerator = TL::Scope(CURRENT_COMPILED_FILE->global_decl_context).get_symbol_from_name(family);
            if (family_enumerator.is_valid())
            {
                ERROR_CONDITION(!family_enumerator.is_enumerator(), "This symbol was expected to be an enumerator\n", 0);
                Nodecl::NodeclBase value = family_enumerator.get_value();
                ERROR_CONDITION(value.is_null(), "Invalid node\n", 0);
                ERROR_CONDITION(!value.is_constant(), "This node should have a constant value\n", 0);
                real_version = const_value_cast_to_unsigned_int(value.get_constant());
            }
            else
            {
                // This is an special value that indicates that a certain
                // family was not present in the runtime headers
                real_version = -1;
            }

            map.insert(std::make_pair(family, real_version));
        }
        return real_version;
    }

    bool Interface::family_is_at_least(const std::string &family, unsigned int expected_version)
    {

        return ((int)expected_version) <= get_version_for_family(family, _map);
    }

    bool Interface::family_is(const std::string &family, unsigned int expected_version)
    {
        return ((int)expected_version) == get_version_for_family(family, _map);
    }

    void Interface::family_must_be_at_least(
            const std::string &family, unsigned int expected_version, const std::string& feature)
    {
        if (!family_is_at_least(family, expected_version))
        {
            fatal_error("Error: the version of the '%s' Nanos6 interface should be at least %d to support '%s'",
                    family.c_str(), expected_version, feature.c_str());
        }
    }

    void Interface::family_must_be(
            const std::string &family, unsigned int expected_version, const std::string& feature)
    {
        if (!family_is(family, expected_version))
        {
            fatal_error("Error: the version of the '%s' Nanos6 interface should be %d to support '%s'",
                    family.c_str(), expected_version, feature.c_str());
        }
    }

    void Interface::check_nanos6_deprecated_headers()
    {
        //IMPORTANT: every change in the major version resets all other versions to 1
        family_must_be_at_least("nanos6_major_api", 3, "any feature of OmpSs-2");
    }
}}
