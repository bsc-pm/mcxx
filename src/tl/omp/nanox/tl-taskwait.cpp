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

#include "tl-omp-nanox.hpp"

namespace TL
{
    namespace Nanox
    {
        void OMPTransform::taskwait_postorder(PragmaCustomConstruct ctr)
        {
            OpenMP::DataSharingEnvironment& data_sharing = openmp_info->get_data_sharing(ctr.get_ast());
            Source src;

            ObjectList<OpenMP::DependencyItem> dependences;
            data_sharing.get_all_dependences(dependences);

            if (!dependences.empty())
            {
                Source dependences_wait;
                src << dependences_wait;

                int num_dependences = dependences.size();
                Source dependency_defs_wait, fake_struct_fields, dep_holder_init;
                dependences_wait << "{"
                    << "struct _dependence_holder {"
                    << fake_struct_fields
                    << "} _dep_holder = {" << dep_holder_init << "};"
                    << "nanos_dependence_t _wait_dependences[" << num_dependences << "] = {"
                    << dependency_defs_wait
                    << "};"
                    ;

                int dep_num = 0;
                for (ObjectList<OpenMP::DependencyItem>::iterator it = dependences.begin();
                        it != dependences.end();
                        it++)
                {
                    Source dependency_name, dependency_offset;

                    dependency_name
                        << "dep_" << dep_num;
                    
                    DataReference data_ref = it->get_dependency_expression();

                    if (data_ref.get_type().is_array())
                    {
                        fake_struct_fields << data_ref
                            .get_type()
                            .array_element()
                            .get_pointer_to()
                            .get_declaration(data_ref.get_scope(),
                                    dependency_name) << ";";

                        dep_holder_init << data_ref 
                            ;
                    }
                    else
                    {
                        fake_struct_fields << data_ref
                            .get_type()
                            .get_pointer_to()
                            .get_declaration(data_ref.get_scope(),
                                    dependency_name) << ";";

                        dep_holder_init << "&(" << data_ref << ")"
                            ;
                    }

                    dependency_defs_wait
                        << "{"
                        << "(void**)&_dep_holder." << dependency_name << ","
                        << dependency_offset << ","
                        << "{1, 0, 0},"
                        << "sizeof(" << data_ref << ")"
                        << "}"
                        ;


                    dependency_offset
                        << "((char*)(" << data_ref.get_address() << ") - " << "(char*)_dep_holder." << dependency_name << ")"
                        ;

                    if ((it + 1) != dependences.end())
                    {
                        dep_holder_init
                            << ","
                            ;
                        dependency_defs_wait
                            << ","
                            ;
                    }

                    dep_num++;
                }

                dependences_wait << "nanos_wait_on(" << num_dependences << ", _wait_dependences);";
                dependences_wait << "}";
            }
            else
            {
                src << "nanos_wg_wait_completion(nanos_current_wd());"
                    ;
            }

            AST_t tree = src.parse_statement(ctr.get_ast(), ctr.get_scope_link());
            ctr.get_ast().replace(tree);
        }
    }
}
