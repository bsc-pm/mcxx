/*--------------------------------------------------------------------
(C) Copyright 2006-2014 Barcelona Supercomputing Center             *
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

#include "tl-ssa.hpp"

namespace TL {
namespace Analysis {

    unsigned int non_sym_constraint_id = 0;
    unsigned int get_next_id(const NBase& n)
    {
        //! This maps stores the relationship between each variable in a given node and 
        //! the last identifier used to create a constraint for that variable
        std::map<NBase, unsigned int, Nodecl::Utils::Nodecl_structural_less> var_to_last_constraint_id;

        unsigned int next_id = 0;
        if (!n.is_null())
        {
            if(var_to_last_constraint_id.find(n) != var_to_last_constraint_id.end())
                next_id = var_to_last_constraint_id[n] + 1;
            var_to_last_constraint_id[n] = next_id;
        }
        else
        {
            next_id = ++non_sym_constraint_id;
        }

        return next_id;
    }

    bool replace_substring(std::string& str, const std::string& from, const std::string& to) {
        size_t start_pos = str.find(from);
        if (start_pos == std::string::npos)
            return false;
        str.replace(start_pos, from.length(), to);
        return true;
    }

    std::string get_subscripts_string(
            const Nodecl::ArraySubscript& n,
            /*in*/ VarToConstraintMap& input_constraints)
    {
        std::string subscripts_str;

        // Concatenate all the subscripts in a string replacing symbols with the ssa symbols
        const Nodecl::List& subscripts = n.get_subscripts().as<Nodecl::List>();
        for (Nodecl::List::const_iterator it = subscripts.begin(); it != subscripts.end(); ++it)
        {
            const Nodecl::NodeclBase& s = *it;
            if (input_constraints.find(s) != input_constraints.end())
                subscripts_str += input_constraints[s].get_symbol().get_name();
            else    // The subscript is an operation
            {
                // 1.- Get all symbol_names
                ObjectList<Nodecl::Symbol> all_syms = Nodecl::Utils::get_all_symbols_first_occurrence(s);
                std::vector<std::string> all_syms_names;
                std::vector<std::string> all_syms_replacements;
                for (ObjectList<Nodecl::Symbol>::iterator its = all_syms.begin();
                     its != all_syms.end(); ++its)
                {
                    NBase its_parent = its->get_parent();
                    all_syms_names.push_back(its->get_symbol().get_name());
                    if (its_parent.is<Nodecl::Conversion>()
                        && its_parent.is_constant())
                    {   // Note: constants are not associated to symbols anymore,
                        // but to the conversion previous to the symbol occurrence
                        Nodecl::NodeclBase its_const = const_value_to_nodecl(its_parent.get_constant());
                        all_syms_replacements.push_back(its_const.prettyprint());
                    }
                    else if (input_constraints.find(*its) != input_constraints.end())
                    {
                        all_syms_replacements.push_back(input_constraints[*its].get_symbol().get_name());
                    }
                    else
                    {
                        internal_error("No input constraint found for symbol '%s' in subscript '%s'.\n",
                                       its->get_symbol().get_name().c_str(),
                                       s.prettyprint().c_str());
                    }
                }
                // Order the names by length (longer first)
                CompareString cs;
                std::sort(all_syms_names.begin(), all_syms_names.end(), cs);

                // 2.- Build the subscript string
                subscripts_str = s.prettyprint();
                // Remove blank spaces from the string (e.g. due to operations in the subscripts)
                subscripts_str.erase(remove_if(subscripts_str.begin(), subscripts_str.end(), isspace),
                                     subscripts_str.end());
                // Replace in the subscript each symbol by its corresponding ssa symbol
                std::vector<std::string>::iterator its = all_syms_names.begin();
                std::vector<std::string>::iterator itr = all_syms_replacements.begin();
                for (; its != all_syms_names.end(); ++its, ++itr)
                    replace_substring(subscripts_str, *its, *itr);
            }
            subscripts_str += "_";
        }

        return subscripts_str;
    }

}
}
