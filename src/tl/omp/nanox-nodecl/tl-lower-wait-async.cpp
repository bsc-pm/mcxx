/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#include "tl-lowering-visitor.hpp"
#include "tl-counters.hpp"
#include "tl-source.hpp"
#include "tl-nanos.hpp"
#include "tl-datareference.hpp"

namespace TL { namespace Nanox {

void LoweringVisitor::fill_dependences_wait(
        Nodecl::NodeclBase ctr,
        OutlineInfo& outline_info,
        // out
        Source& result_src
        )
{
    Source dependency_init;

    int num_deps = count_dependences(outline_info);

    TL::ObjectList<OutlineDataItem> data_items = outline_info.get_data_items();

    if (num_deps == 0)
    {
        internal_error("Code unreachable", 0);
        return;
    }

    if (Nanos::Version::interface_is_at_least("master", 6001))
    {
        Source dependency_regions;

        result_src
            << dependency_regions
            << "nanos_data_access_t dependences[" << num_deps << "]"
            ; 
        
        if (IS_C_LANGUAGE
                || IS_CXX_LANGUAGE)
        {
            result_src << " = {"
                << dependency_init
                << "};"
                ;
        }
        result_src << ";"
            ;

        int current_dep_num = 0;
        for (TL::ObjectList<OutlineDataItem>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            OutlineDataItem::Directionality dir = it->get_directionality();
            if (dir == OutlineDataItem::DIRECTIONALITY_NONE)
                continue;

            TL::ObjectList<Nodecl::NodeclBase> deps = it->get_dependences();
            for (ObjectList<Nodecl::NodeclBase>::iterator dep_it = deps.begin();
                    dep_it != deps.end();
                    dep_it++, current_dep_num++)
            {
                TL::DataReference dep_expr(*dep_it);

                Source dependency_flags,
                       dependency_flags_in,
                       dependency_flags_out,
                       dependency_flags_concurrent,
                       dependency_size;

                Source dep_expr_addr;
                dep_expr_addr << as_expression(dep_expr.get_base_address());
                dependency_size << as_expression(dep_expr.get_sizeof());

                dependency_flags 
                    << "{" 
                    << dependency_flags_in << "," 
                    << dependency_flags_out << ", "
                    << /* renaming has not yet been implemented */ "0, " 
                    << dependency_flags_concurrent
                    << "}"
                    ;

                Type dependency_type = dep_expr.get_data_type();

                int num_dimensions = dependency_type.get_num_dimensions();

                int concurrent = ((dir & OutlineDataItem::DIRECTIONALITY_CONCURRENT) == OutlineDataItem::DIRECTIONALITY_CONCURRENT);

                dependency_flags_in << (((dir & OutlineDataItem::DIRECTIONALITY_IN) == OutlineDataItem::DIRECTIONALITY_IN) || concurrent);
                dependency_flags_out << (((dir & OutlineDataItem::DIRECTIONALITY_OUT) == OutlineDataItem::DIRECTIONALITY_OUT) || concurrent);
                dependency_flags_concurrent << concurrent;
                //
                // Compute the base type of the dependency and the array containing the size of each dimension
                Type dependency_base_type = dependency_type;

                Nodecl::NodeclBase dimension_sizes[num_dimensions];
                for (int dim = 0; dim < num_dimensions; dim++)
                {
                    dimension_sizes[dim] = dependency_base_type.array_get_size();
                    dependency_base_type = dependency_base_type.array_element();
                }

                std::string base_type_name = dependency_base_type.get_declaration(dep_expr.retrieve_context(), "");

                dependency_regions << "nanos_region_dimension_t dimensions_" << current_dep_num << "[" << std::max(num_dimensions, 1) << "]"
                    ;

                Source dims_description;

                if (IS_C_LANGUAGE
                        || IS_CXX_LANGUAGE)
                {
                    dependency_regions << "=  { " << dims_description << "}";
                }

                dependency_regions << ";"
                    ;

                if (num_dimensions == 0)
                {
                    Source dimension_size, dimension_lower_bound, dimension_accessed_length;

                    dimension_size << as_expression(dimension_sizes[num_dimensions - 1].copy()) << "* sizeof(" << base_type_name << ")";
                    dimension_lower_bound << "0";
                    dimension_accessed_length << dimension_size;

                    if (IS_C_LANGUAGE
                            || IS_CXX_LANGUAGE)
                    {
                        Source dims_description;
                        dims_description
                            << "{"
                            << dimension_size << ","
                            << dimension_lower_bound << ","
                            << dimension_accessed_length
                            << "}"
                            ;
                    }
                    else
                    {
                        dependency_regions
                            << "dimensions_" << current_dep_num << "[0].size = " << dimension_size << ";"
                            << "dimensions_" << current_dep_num << "[0].lower_bound = " << dimension_lower_bound << ";"
                            << "dimensions_" << current_dep_num << "[0].accessed_length = " << dimension_accessed_length << ";"
                            ;
                    }
                }
                else
                {
                    Source dimension_size, dimension_lower_bound, dimension_accessed_length;

                    // Compute the contiguous array type
                    Type contiguous_array_type = dependency_type;
                    while (contiguous_array_type.array_element().is_array())
                    {
                        contiguous_array_type = contiguous_array_type.array_element();
                    }

                    Nodecl::NodeclBase lb, ub, size;
                    if (contiguous_array_type.array_is_region())
                    {
                        contiguous_array_type.array_get_region_bounds(lb, ub);
                        size = contiguous_array_type.array_get_region_size();
                    }
                    else
                    {
                        contiguous_array_type.array_get_bounds(lb, ub);
                        size = contiguous_array_type.array_get_size();
                    }

                    dimension_size << "sizeof(" << base_type_name << ") * " << as_expression(dimension_sizes[num_dimensions - 1].copy());
                    dimension_lower_bound << "sizeof(" << base_type_name << ") * " << as_expression(lb.copy());
                    dimension_accessed_length << "sizeof(" << base_type_name << ") * " << as_expression(size.copy());

                    if (IS_C_LANGUAGE
                            || IS_CXX_LANGUAGE)
                    {
                        Source dims_description;
                        dims_description
                            << "{"
                            << dimension_size << ","
                            << dimension_lower_bound << ","
                            << dimension_accessed_length
                            << "}"
                            ;
                    }
                    else
                    {
                        dependency_regions
                            << "dimensions_" << current_dep_num << "[0].size = " << dimension_size << ";"
                            << "dimensions_" << current_dep_num << "[0].lower_bound = " << dimension_lower_bound << ";"
                            << "dimensions_" << current_dep_num << "[0].accessed_length = " << dimension_accessed_length << ";"
                            ;
                    }
                    
                    // All but 0 (contiguous) are handled here
                    fill_dimensions(num_dimensions, 
                            num_dimensions,
                            current_dep_num,
                            dimension_sizes, 
                            dependency_type, 
                            dims_description,
                            dependency_regions,
                            dep_expr.retrieve_context());
                }

                if (num_dimensions == 0) 
                    num_dimensions++;

                if (IS_C_LANGUAGE
                        || IS_CXX_LANGUAGE)
                {
                    dependency_init
                        << "{"
                        << "(void*)" << dep_expr_addr << ","
                        << dependency_flags << ", "
                        << num_dimensions << ", "
                        << "dimensions_" << current_dep_num
                        << "}";
                }
                else if (IS_FORTRAN_LANGUAGE)
                {
                    result_src
                        << "dependences[" << current_dep_num << "].address = (void*)" << dep_expr_addr << ";"
                        << "dependences[" << current_dep_num << "].flags.input = " << dependency_flags_in << ";"
                        << "dependences[" << current_dep_num << "].flags.output" << dependency_flags_out << ";"
                        << "dependences[" << current_dep_num << "].flags.can_rename = 0;"
                        << "dependences[" << current_dep_num << "].flags.commutative = " << dependency_flags_concurrent << ";"
                        << "dependences[" << current_dep_num << "].dimension_count = " << num_dimensions << ";"
                        << "dependences[" << current_dep_num << "].dimensions = dimensions_" << current_dep_num << ";"
                        ;
                }
            }
        }
    }
    else
    {
        Source temporaries;
        result_src
            << temporaries
            << "nanos_dependence_t dependences[" << num_deps << "]";

        // We only initialize in C/C++, in Fortran we will make a set of assignments
        if (IS_C_LANGUAGE
                || IS_CXX_LANGUAGE)
        {
            result_src << "= {"
            << dependency_init
            << "}";
        }

        result_src << ";"
            ;

        int current_dep_num = 0;
        for (TL::ObjectList<OutlineDataItem>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            OutlineDataItem::Directionality dir = it->get_directionality();
            if (dir == OutlineDataItem::DIRECTIONALITY_NONE)
                continue;

            TL::ObjectList<Nodecl::NodeclBase> deps = it->get_dependences();
            for (ObjectList<Nodecl::NodeclBase>::iterator dep_it = deps.begin();
                    dep_it != deps.end();
                    dep_it++, current_dep_num++)
            {
                TL::DataReference dep_expr(*dep_it);

                Source current_dependency_init,
                       dependency_offset,
                       dependency_flags,
                       dependency_flags_in,
                       dependency_flags_out,
                       dependency_flags_concurrent,
                       dependency_size;

                Source dep_expr_addr;
                dep_expr_addr << as_expression(dep_expr.get_base_address());
                dependency_size << as_expression(dep_expr.get_sizeof());


                if (IS_C_LANGUAGE
                        || IS_CXX_LANGUAGE)
                {
                    temporaries
                        << "void *addr_" << current_dep_num << " = (void*)(" << dep_expr_addr << ");"
                        ;
                    current_dependency_init
                        << "{"
                        << "(void**)(&addr_" << current_dep_num << "),"
                        << dependency_offset << ","
                        << dependency_flags << ","
                        << dependency_size
                        << "}"
                        ;
                }
                else if (IS_FORTRAN_LANGUAGE)
                {
                    // We use plain assignments in Fortran
                    result_src
                        << "void *addr_" << current_dep_num << " = (void*)(" << dep_expr_addr << ");"
                        << "dependences[" << current_dep_num << "].address = " << "(void**)(&addr_" << current_dep_num << ");"
                        << "dependences[" << current_dep_num << "].offset = " << dependency_offset << ";"
                        << "dependences[" << current_dep_num << "].size = " << dependency_size << ";"
                        << "dependences[" << current_dep_num << "].flags.input = " << dependency_flags_in << ";"
                        << "dependences[" << current_dep_num << "].flags.output = " << dependency_flags_out << ";"
                        << "dependences[" << current_dep_num << "].flags.can_rename = 0;"
                        ;
                    
                    // if (Nanos::Version::interface_is_at_least("master", 5001))
                    {
                        result_src
                            << "dependences[" << current_dep_num << "].flags.commutative = " << dependency_flags_concurrent << ";"
                            ;
                    }
                }

                Nodecl::NodeclBase ptr_ref = 
                    Nodecl::Reference::make(dep_expr.copy(),
                            dep_expr.get_type().get_pointer_to(),
                            dep_expr.get_filename(),
                            dep_expr.get_line());

                dependency_offset << "0";
                    // << "((char*)(" << dep_expr_addr << ") - " << "(char*)(" << as_expression(ptr_ref) << "))"
                    ;

                if (Nanos::Version::interface_is_at_least("master", 5001))
                {
                    dependency_flags 
                        << "{" 
                        << dependency_flags_in << "," 
                        << dependency_flags_out << ", "
                        << /* renaming has not yet been implemented */ "0, " 
                        << dependency_flags_concurrent
                        << "}"
                        ;
                }
                else
                {
                    dependency_flags 
                        << "{" 
                        << dependency_flags_in << "," 
                        << dependency_flags_out << ", "
                        << /* renaming has not yet been implemented */ "0, " 
                        << "}"
                        ;
                }

                int concurrent = ((dir & OutlineDataItem::DIRECTIONALITY_CONCURRENT) == OutlineDataItem::DIRECTIONALITY_CONCURRENT);

                dependency_flags_in << (((dir & OutlineDataItem::DIRECTIONALITY_IN) == OutlineDataItem::DIRECTIONALITY_IN) || concurrent);
                dependency_flags_out << (((dir & OutlineDataItem::DIRECTIONALITY_OUT) == OutlineDataItem::DIRECTIONALITY_OUT) || concurrent);
                dependency_flags_concurrent << concurrent;

                dependency_init.append_with_separator(current_dependency_init, ",");
            }
        }
    }
}

void LoweringVisitor::emit_wait_async(Nodecl::NodeclBase construct, 
        bool has_dependences,
        OutlineInfo& outline_info)
{
    Source src;

    if (!has_dependences)
    {
        src << "{"
            <<     "nanos_wd_t wd = nanos_current_wd();"
            <<     "nanos_err_t err;"
            <<     "err = nanos_wg_wait_completion(wd, 0);"
            <<     "if (err != NANOS_OK) nanos_handle_error(err);"
            << "}"
            ;
    }
    else
    {
        Source dependences;
        fill_dependences_wait(
                construct,
                outline_info,
                dependences);

        int num_dependences = count_dependences(outline_info);

        src << "{"
            <<     dependences
            <<     "nanos_err_t err = nanos_wait_on(" << num_dependences << ", dependences);"
            <<     "if (err != NANOS_OK) nanos_handle_error(err);"
            << "}"
            ;
    }


    FORTRAN_LANGUAGE()
    {
        // Parse in C
        Source::source_language = SourceLanguage::C;
    }

    Nodecl::NodeclBase n = src.parse_statement(construct);

    FORTRAN_LANGUAGE()
    {
        Source::source_language = SourceLanguage::Current;
    }

    construct.integrate(n);
}

void LoweringVisitor::visit(const Nodecl::Parallel::WaitAsyncsShallow& construct)
{
    OutlineInfo outline_info(Nodecl::NodeclBase::null());
    emit_wait_async(construct, /* has_dependences */ false, outline_info);
}

void LoweringVisitor::visit(const Nodecl::Parallel::WaitAsyncsDependences& construct)
{
    Nodecl::NodeclBase environment = construct.get_environment();

    OutlineInfo outline_info(environment);
    emit_wait_async(construct, /* has_dependences */ true, outline_info);
}

} }
