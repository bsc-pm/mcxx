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


#include "tl-omp-base-instantiation.hpp"
#include "tl-pragmasupport.hpp"
#include "cxx-diagnostic.h"
#include "cxx-instantiation.h"
#include <set>
#include <algorithm>
#include <iterator>

namespace TL { namespace OpenMP {

    struct ContainsOpenMP : public Nodecl::ExhaustiveVisitor<void>
    {
        public:

            bool result;
            ContainsOpenMP()
                : result(false) { }

            virtual void visit(const Nodecl::PragmaCustomDirective& node)
            {
                result = result || TL::PragmaUtils::is_pragma_construct("omp", node);
            }

            virtual void visit(const Nodecl::PragmaCustomStatement& node)
            {
                result = result || TL::PragmaUtils::is_pragma_construct("omp", node);
                if (!result)
                {
                    this->walk(node.get_statements());
                }
            }

            virtual void visit(const Nodecl::PragmaCustomDeclaration& node)
            {
                result = result || TL::PragmaUtils::is_pragma_construct("omp", node);
            }
    };

    static std::set<TL::Symbol> instantiated_function_set;

    InstantiateVisitorOmp::InstantiateVisitorOmp(TL::DTO& dto)
        : _dto(dto)
    {
    }

    void InstantiateVisitorOmp::visit(const Nodecl::ObjectInit& node)
    {
        TL::Symbol sym = node.get_symbol();
        this->walk(sym.get_value());
    }

    void InstantiateVisitorOmp::visit(const Nodecl::Symbol& node)
    {
        TL::Symbol sym = node.get_symbol();

        if (!sym.is_function())
            return;

        // We only care about functions that have not been defined yet
        if (sym.is_defined())
            return;

        // Ignore already processed functions
        if (instantiated_function_set.find(sym) != instantiated_function_set.end())
            return;

        ContainsOpenMP c;
        if (sym.get_type().is_template_specialized_type())
        {
            // Any specialization of a function type is going to be non-dependent
            if (sym.get_type() != sym.get_type().get_related_template_type().get_primary_template())
            {
                TL::Symbol primary_function =
                    sym.get_type().get_related_template_type().get_primary_template().get_symbol();

                c.walk(primary_function.get_function_code());
            }
        }
        else if (sym.is_member()
                && sym.get_class_type().get_symbol().get_type().is_template_specialized_type()
                // Note that the compiler creates partial specializations (which are dependent)
                && !sym.get_class_type().is_dependent())
        {
            TL::Symbol emission_template = sym.get_internal_symbol()->entity_specs.emission_template;

            if (!emission_template.is_valid())
                return;

            c.walk(emission_template.get_function_code());
        }

        if (c.result)
        {
            keep_for_instantiation(sym);
        }
    }


    void InstantiateVisitorOmp::keep_for_instantiation(TL::Symbol symbol)
    {
        instantiated_function_set.insert(symbol);
    }

    void InstantiateVisitorOmp::instantiate_single_function(TL::Symbol symbol)
    {
        info_printf("%s: info: instantiating '%s' because it contains '#pragma omp'\n",
                symbol.get_locus_str().c_str(),
                symbol.get_qualified_name().c_str());

        instantiate_template_function_and_integrate_in_translation_unit(symbol.get_internal_symbol(),
                symbol.get_locus());
    }

    void InstantiateVisitorOmp::instantiate()
    {
        Nodecl::NodeclBase translation_unit = _dto["nodecl"];

        instantiated_function_set.clear();

        while (true)
        {
            std::set<TL::Symbol> prev_instantiated_function_set = instantiated_function_set;

            this->walk(translation_unit);

            if (prev_instantiated_function_set == instantiated_function_set)
                break;

            std::set<TL::Symbol> newly_added;
            std::set_difference(instantiated_function_set.begin(),
                    instantiated_function_set.end(),
                    prev_instantiated_function_set.begin(),
                    prev_instantiated_function_set.end(),
                    std::inserter(newly_added, newly_added.begin()));

            for (std::set<TL::Symbol>::iterator it = newly_added.begin();
                    it != newly_added.end();
                    it++)
            {
                instantiate_single_function(*it);
            }
        }
    }

} }
