/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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

#include "tl-remove-attrs.hpp"
#include "tl-ast.hpp"
#include "tl-scopelink.hpp"
#include "tl-langconstruct.hpp"

namespace TL
{
    RemoveAttributes::RemoveAttributes()
    {
    }

    class RemoveAttributeFunctor : public TraverseASTFunctor
    {
        private:
            const ObjectList<std::string> &_attr_names;
            ScopeLink _sl;
        public:
            RemoveAttributeFunctor(const ObjectList<std::string> &attr_names, 
                    ScopeLink sl)
                 : _attr_names(attr_names), _sl(sl)
            {
            }

            ASTTraversalResult do_(const AST_t& a) const
            {
                if (GCCAttributeSpecifier::predicate(a))
                {
                    std::cerr << "Found ->" << a.get_locus() << std::endl;
                    ObjectList<GCCAttribute> gcc_attrib = GCCAttributeSpecifier(a, _sl).get_gcc_attribute_list();
                    for (ObjectList<GCCAttribute>::iterator it = gcc_attrib.begin();
                            it != gcc_attrib.end();
                            it++)
                    {
                        if (_attr_names.contains(it->get_name()))
                        {
                            std::cerr << "Match ->" << a.get_locus() << std::endl;
                            return ast_traversal_result_helper(/* match */ true, /* recurse */ false);
                        }
                    }
                }
            }
    };

    void RemoveAttributes::run(DTO& dto)
    {
        TL::AST_t tree = dto["translation_unit"];
        TL::ScopeLink sl = dto["scope_link"];

        TL::String str = dto["remove_attributes"];

        // Do nothing if no remove_attributes entity was defined
        if (str == "")
            return;

        _attr_names.clear();
        std::string::size_type i = 0;

        while (i != std::string::npos
                && (i < str.size()))
        {
            std::string::size_type j = str.find(',', i);

            if (j == std::string::npos)
            {
                _attr_names.append(str.substr(i));
                i = j;
            }
            else
            {
                _attr_names.append(str.substr(i, j - i));
                i = j + 1;
            }
        }

        std::cerr << "REMOVING : " << concat_strings(_attr_names, ", ") << std::endl;

        ObjectList<AST_t> matching_attributes = tree.depth_subtrees(RemoveAttributeFunctor(_attr_names, sl));

        for (ObjectList<AST_t>::iterator it = matching_attributes.begin();
                it != matching_attributes.end();
                it++)
        {
            std::cerr << "Removing ->" << it->get_locus() << std::endl;
            it->remove_in_list();
        }
    }
}

EXPORT_PHASE(TL::RemoveAttributes);
