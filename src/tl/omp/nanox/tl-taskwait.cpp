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



#include "tl-omp-nanox.hpp"
#include "tl-nanos.hpp"

namespace TL
{
    namespace Nanox
    {
        namespace
        {
            void non_region_dependences(Source& src,
                    ObjectList<OpenMP::DependencyItem>& dependences)
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
                        << "{1, 0, 0, 0},"
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

            void fill_dimensions(int n_dims, int actual_dim, std::string* dim_sizes, Type dep_type, Source& dims_description, Scope sc)
            {
                if (actual_dim < (n_dims-1))
                {
                    fill_dimensions(n_dims, actual_dim+1, dim_sizes, dep_type.array_element(), dims_description, sc);

                    if (dep_type.array_is_region())
                    {
                        AST_t lb, ub, size;
                        dep_type.array_get_region_bounds(lb, ub);
                        size = dep_type.array_get_region_size();

                        dims_description << ", {"
                            << "(" << dim_sizes[actual_dim] << "), "
                            << "(" << lb.prettyprint() << "), "
                            << "(" << size.prettyprint() << ")"
                            << "}"
                            ;
                    }
                    else
                    {
                        std::string lb;
                        if (IS_C_LANGUAGE)
                        {
                            lb = "0";
                        }
                        else if (IS_FORTRAN_LANGUAGE)
                        {
                            lb = "1";
                        }

                        dims_description << ", {"
                            << "(" << dep_type.array_get_size().prettyprint() << "), "
                            << "(" << lb << "), "
                            << "(" << dim_sizes[actual_dim] << ")"
                            << "}"
                            ;
                    }
                }
            }

            void region_dependences(Source& src,
                    ObjectList<OpenMP::DependencyItem>& dependences)
            {
                Source num_dependences;
                num_dependences << dependences.size();

                Source dependency_regions, dependency_defs;
                src
                    << "{"
                    <<    dependency_regions
                    <<    "nanos_data_access_t _data_accesses[" << num_dependences << "] = {"
                    <<    dependency_defs
                    <<    "};"
                    ;

                int num_dep = 0;
                for (ObjectList<OpenMP::DependencyItem>::iterator it = dependences.begin();
                        it != dependences.end();
                        it++)
                {
                    // Set dependency flags as inout
                    Source dependency_flags;
                    dependency_flags << "{ 1,1, 0, 0 }";

                    Source dependency_field_name;
                    DataReference dependency_expression = it->get_dependency_expression();

                    Type dependency_type = dependency_expression.get_type();
                    Symbol base_symbol = dependency_expression.get_base_symbol();
                    Source dep_expr_addr = dependency_expression.get_address();

                    Source dims_description;

                    int num_dimensions = dependency_type.get_num_dimensions();

                    // Compute the base type of the dependency and the array containing the size of each dimension
                    Type dependency_base_type = dependency_type;
                    std::string dimension_sizes[num_dimensions + 1];
                    for (int dim = 0; dim < num_dimensions; dim++)
                    {
                        dimension_sizes[dim] = dependency_base_type.array_get_size().prettyprint();
                        dependency_base_type = dependency_base_type.array_element();
                    }
                    std::string base_type_name = dependency_base_type.get_declaration(dependency_expression.get_scope(), "");

                    // Generate the spawn
                    dependency_regions 
                        << "nanos_region_dimension_t dimensions" << num_dep << "[" << std::max(num_dimensions,1) << "] = {"
                        << dims_description << "};";

                    Source dependency_offset, dependency_base_address;

                    if (num_dimensions == 0) 
                    {
                        // A scalar
                        dims_description << "{" 
                            << "sizeof(" << base_type_name << "), " 
                            << dependency_offset << ", "
                            << "sizeof(" << base_type_name << ")" 
                            << "}"
                            ;
                    }
                    else
                    {
                        // Less significant dimension s computed in bytes
                        Type aux_type = dependency_type;
                        while (aux_type.array_element().is_array())
                        {
                            aux_type = aux_type.array_element();
                        }

                        if (aux_type.array_is_region())
                        {
                            AST_t lb, ub, size;
                            aux_type.array_get_region_bounds(lb, ub);
                            size = aux_type.array_get_region_size();

                            dims_description << "{" 
                                << "sizeof(" << base_type_name << ") * (" << dimension_sizes[num_dimensions-1] << "), " 
                                << "sizeof(" << base_type_name << ") * (" << lb.prettyprint() << "), "
                                << "sizeof(" << base_type_name << ") * (" << size.prettyprint() << ")  /**/"
                                << "}"
                                ;
                        }
                        else
                        {
                            std::string lb;
                            lb = "0";

                            dims_description << ""
                                << "sizeof(" << base_type_name << ") * (" << dimension_sizes[num_dimensions-1] << "), "
                                << lb << ", "
                                << "sizeof(" << base_type_name << ") * (" << dimension_sizes[num_dimensions-1] << ")"
                                << "}"
                                ;
                        }

                        // The rest of dimensions, if there are, are computed in terms of number of elements
                        if (num_dimensions > 1)
                        {
                            fill_dimensions(num_dimensions, 0, dimension_sizes, dependency_type,
                                    dims_description, dependency_expression.get_scope());
                        }
                    }

                    if (num_dimensions == 0)
                    {
                        num_dimensions++;
                    }

                    dependency_offset
                        << "(char*)" << dep_expr_addr << " - (char*)" << dependency_base_address
                        ;

                    Expression::OperationKind op_kind;
                    if ((op_kind = dependency_expression.get_operation_kind()) == Expression::DERREFERENCE
                            || base_symbol.get_type().is_array())
                    {
                        dependency_base_address << base_symbol.get_name();
                    }
                    else
                    {
                        dependency_base_address << "&" << base_symbol.get_name();
                    }

                    dependency_defs
                        << "{"
                        << dependency_base_address << ", "
                        << dependency_flags << ","
                        << num_dimensions << ","
                        << "dimensions" << num_dep <<","
                        << dependency_offset
                        << "}"
                        ;

                    if ((it + 1) != dependences.end())
                    {
                        dependency_defs << ",";
                    }

                    num_dep++;
                }

                src <<     "nanos_wait_on(" << num_dependences << ", _data_accesses);"
                    << "}"
                    ;
            }

        }

        void OMPTransform::taskwait_postorder(PragmaCustomConstruct ctr)
        {
            OpenMP::DataSharingEnvironment& data_sharing = openmp_info->get_data_sharing(ctr.get_ast());
            Source src;

            ObjectList<OpenMP::DependencyItem> dependences;
            data_sharing.get_all_dependences(dependences);

            if (!dependences.empty())
            {
                if (Nanos::Version::interface_is_at_least("deps_api", 1000))
                {
                    region_dependences(src, dependences);
                }
                else
                {
                    non_region_dependences(src, dependences);
                }
            }
            else
            {
                bool avoid_flush = false;

                PragmaCustomClause noflush = ctr.get_clause("noflush");
                if (noflush.is_defined())
                {
                    avoid_flush = true;
                }

                src << get_wait_completion(Source("nanos_current_wd()"), avoid_flush, ctr.get_ast())
                    ;
            }

            AST_t tree = src.parse_statement(ctr.get_ast(), ctr.get_scope_link());
            ctr.get_ast().replace(tree);
        }
    }
}
