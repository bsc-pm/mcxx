/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

#include "tl-omptransform.hpp"

namespace TL
{
    namespace Nanos4
    {
        void OpenMPTransform::critical_postorder(PragmaCustomConstruct critical_construct)
        {
            Source critical_source;

            Statement critical_body = critical_construct.get_statement();
            ScopeLink scope_link = critical_construct.get_scope_link();

            std::string mutex_variable;

            if (!critical_construct.is_parameterized())
            {
                mutex_variable = "_nthf_unspecified_critical";
            }
            else
            {
                // ObjectList<IdExpression> id_expressions = region_name.id_expressions(TL::ALL_FOUND_SYMBOLS);
                // IdExpression head = id_expressions[0];

                ObjectList<std::string> args = critical_construct.get_parameter_arguments();

                if (args.size() > 1)
                {
                    std::cerr << critical_construct.get_ast().get_locus() 
                        << ": warning: #pragma omp critical only receives one argument, using first one" 
                        << std::endl;
                }
                else if (args.size() == 0)
                {
                    running_error("%s: error: #pragma omp critical needs an argument",
                            critical_construct.get_ast().get_locus().c_str());
                }

                mutex_variable = "_nthf_"  + args[0];
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
