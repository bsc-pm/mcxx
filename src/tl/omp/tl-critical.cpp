/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include "tl-omptransform.hpp"

namespace TL
{
    namespace Nanos4
    {
        void OpenMPTransform::critical_postorder(OpenMP::CriticalConstruct critical_construct)
        {
            Source critical_source;

            OpenMP::Directive directive = critical_construct.directive();
            Statement critical_body = critical_construct.body();
            ScopeLink scope_link = critical_construct.get_scope_link();

            OpenMP::Clause region_name = directive.parameter_clause();

            std::string mutex_variable;

            if (!region_name.is_defined())
            {
                mutex_variable = "_nthf_unspecified_critical";
            }
            else
            {
                ObjectList<IdExpression> id_expressions = region_name.id_expressions(TL::ALL_FOUND_SYMBOLS);
                IdExpression head = id_expressions[0];

                mutex_variable = "_nthf_"  + head.prettyprint();
            }

            critical_source
                << "{"
                //                    <<   "extern void nthf_spin_lock_(void*);"
                //                    <<   "extern void nthf_spin_unlock_(void*);"
                <<   "nthf_spin_lock_(&" << mutex_variable << ");"
                <<   critical_body.prettyprint()
                <<   "nthf_spin_unlock_(&" << mutex_variable << ");"
                << "}"
                ;

            define_global_mutex(mutex_variable, critical_construct.get_ast(),
                    critical_construct.get_scope_link());

            AST_t critical_tree = critical_source.parse_statement(critical_construct.get_ast(),
                    critical_construct.get_scope_link());

            critical_construct.get_ast().replace(critical_tree);
        }

        void OpenMPTransform::define_global_mutex(std::string mutex_variable, AST_t ref_tree, ScopeLink sl)
        {
            if (criticals_defined.find(mutex_variable) == criticals_defined.end())
            {
                // Now declare, if not done before
                Source critical_mutex_def_src, weak_attr;

                critical_mutex_def_src <<
                    "nth_word_t " << weak_attr << " " << mutex_variable << " = 0;"
                    ;

                // It seems it is a good idea to do that also in C
                // CXX_LANGUAGE()
                {
                    // We need this because of the One Definition Rule
                    weak_attr 
                        << "__attribute__((weak))"
                        ;
                }

                AST_t critical_mutex_def_tree = critical_mutex_def_src.parse_global(ref_tree, sl);

                ref_tree.prepend_sibling_global(critical_mutex_def_tree);

                criticals_defined.insert(mutex_variable);
            }
        }
    }
}
