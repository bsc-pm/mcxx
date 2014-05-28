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

    namespace {
        std::set<TL::Symbol> instantiated_function_set;
    }

    InstantiateVisitorOmp::InstantiateVisitorOmp(TL::DTO& dto)
        : _dto(dto)
    {
    }

    void InstantiateVisitorOmp::visit(const Nodecl::FunctionCode& function_code)
    {
        TL::Symbol function_symbol = function_code.get_symbol();
        if (function_symbol.is_member()
                && function_symbol.get_class_type().is_dependent())
        {
            walk_function_code(function_code);
        }
    }

    void InstantiateVisitorOmp::visit(const Nodecl::TemplateFunctionCode& function_code)
    {
        TL::Symbol function_symbol = function_code.get_symbol();
        TL::Type function_type = function_symbol.get_type();

        if (function_type.is_template_specialized_type())
        {
            TL::Type template_type = function_type.get_related_template_type();

            TL::ObjectList<TL::Type> specializations = template_type.get_specializations();
            if (specializations.size() == 1)
            {
                // This template has not been instantiated at all (there is
                // only a primary function)
                return;
            }
        }
        walk_function_code(function_code);
    }

    template <typename NodeKind>
        void InstantiateVisitorOmp::walk_function_code(const NodeKind& node)
        {
            TL::Symbol function_symbol = node.get_symbol();

            ContainsOpenMP c;
            c.walk(node.get_statements());

            if (!c.result)
                return;

            TL::Type function_type = function_symbol.get_type();

            if (function_type.is_template_specialized_type())
            {
                TL::Type template_type = function_type.get_related_template_type();
                TL::ObjectList<TL::Type> specializations = template_type.get_specializations();
                TL::Type primary_specialization = template_type.get_primary_template();

                for (TL::ObjectList<TL::Type>::iterator it = specializations.begin();
                        it != specializations.end();
                        it++)
                {
                    if (*it == primary_specialization
                            // Skip explicit specializations as well
                            || (it->get_symbol().get_scope().get_template_parameters() != NULL
                                && it->get_symbol().get_scope().get_template_parameters()->is_explicit_specialization))
                        continue;

                    TL::Symbol specialized_symbol = it->get_symbol();
                    this->keep_for_instantiation(specialized_symbol);
                }
            }
            else
            {
                this->keep_for_instantiation(function_symbol);
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
