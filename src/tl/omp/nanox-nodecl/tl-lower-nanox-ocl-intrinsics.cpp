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

#include "cxx-cexpr.h"
#include "fortran03-typeutils.h"
#include "fortran03-typeenviron.h"
#include "fortran03-exprtype.h"

#include "tl-lowering-visitor.hpp"
#include "tl-nanox-ptr.hpp"


namespace TL { namespace Nanox {

    static Symbol create_new_function_opencl_allocate(
            Nodecl::NodeclBase expr_stmt,
            Symbol subscripted_symbol,
            Type element_type,
            int num_dimensions,
            bool is_allocatable)
    {
        std::string alloca_or_pointer = is_allocatable ? "ALLOCATABLE" : "POINTER";

        TL::Source dummy_arguments_bounds, dimension_attr, allocate_dims;
        dimension_attr << "DIMENSION(";
        for (int i = 1; i <= num_dimensions; ++i)
        {
            if (i != 1)
            {
                allocate_dims << ", ";
                dummy_arguments_bounds <<", ";
                dimension_attr << ", ";
            }

            dummy_arguments_bounds <<"LB" << i <<", " << "UB" << i;
            dimension_attr << ":";
            allocate_dims << "LB" << i << ":" << "UB" << i;
        }
        dimension_attr << ")";

        size_t size_of_array_descriptor =
            fortran_size_of_array_descriptor(
                    fortran_get_rank0_type(subscripted_symbol.get_type().get_internal_type()),
                    fortran_get_rank_of_type(subscripted_symbol.get_type().get_internal_type()));

        TL::Source new_function_name;
        new_function_name
            << "NANOX_OPENCL_ALLOCATE_INTERNAL_"
            << (ptrdiff_t) subscripted_symbol.get_internal_symbol()
            ;

        Nodecl::NodeclBase nodecl_body;
        TL::Source new_function;
        new_function
            << "SUBROUTINE " << new_function_name << "(ARR, " << dummy_arguments_bounds  << ")\n"
            <<      as_type(element_type) << ", " << dimension_attr << ", " << alloca_or_pointer << " :: ARR\n"
            <<      as_type(element_type) << ", " << dimension_attr << ", ALLOCATABLE :: TMP\n"
            <<      "INTEGER :: " << dummy_arguments_bounds << "\n"
            <<      "INTEGER :: ERR \n"
            <<      "ALLOCATE(TMP(" << allocate_dims << "))\n"
            <<      statement_placeholder(nodecl_body)
            <<      "DEALLOCATE(TMP)\n"
            << "END SUBROUTINE " << new_function_name << "\n"
            ;

        Nodecl::NodeclBase function_code = new_function.parse_global(expr_stmt.retrieve_context().get_global_scope());

        TL::Scope inside_function = ReferenceScope(nodecl_body).get_scope();
        TL::Symbol new_function_sym = inside_function.get_symbol_from_name(strtolower(new_function_name.get_source().c_str()));
        TL::Symbol arr_sym = inside_function.get_symbol_from_name("arr");
        TL::Symbol tmp_sym = inside_function.get_symbol_from_name("tmp");
        TL::Symbol ptr_of_arr_sym = get_function_ptr_of(arr_sym, inside_function);
        TL::Symbol ptr_of_tmp_sym = get_function_ptr_of(tmp_sym, inside_function);

        TL::Source aux;
        aux
            <<  "ERR = NANOS_MEMCPY("
            <<          ptr_of_arr_sym.get_name() << "(ARR),"
            <<          ptr_of_tmp_sym.get_name() << "(TMP),"
            <<          "INT(" << size_of_array_descriptor << "," << type_get_size(get_ptrdiff_t_type()) << "))\n"

            <<  "CALL NANOS_OPENCL_ALLOCATE_FORTRAN("
            <<          "SIZEOF(TMP),"
            <<          ptr_of_arr_sym.get_name() << "(ARR))\n"
            ;

        nodecl_body.replace(aux.parse_statement(inside_function));
        Nodecl::Utils::prepend_to_enclosing_top_level_location(expr_stmt, function_code);

        return new_function_sym;
    }

    static void handle_ompss_opencl_allocate_intrinsic(
            Nodecl::FunctionCall function_call,
            std::map<std::pair<TL::Type, std::pair<int, bool> > , Symbol> &declared_ocl_allocate_functions,
            Nodecl::NodeclBase expr_stmt)
    {
        Nodecl::List arguments = function_call.get_arguments().as<Nodecl::List>();
        ERROR_CONDITION(arguments.size() != 1, "More than one argument in 'ompss_opencl_allocate' call\n", 0);

        Nodecl::NodeclBase actual_argument = arguments[0];
        ERROR_CONDITION(!actual_argument.is<Nodecl::FortranActualArgument>(), "Unexpected tree\n", 0);

        Nodecl::NodeclBase arg = actual_argument.as<Nodecl::FortranActualArgument>().get_argument();
        ERROR_CONDITION(!arg.is<Nodecl::ArraySubscript>(), "Unreachable code\n", 0);

        Nodecl::NodeclBase subscripted = arg.as<Nodecl::ArraySubscript>().get_subscripted();
        TL::Symbol subscripted_symbol = ::fortran_data_ref_get_symbol(subscripted.get_internal_nodecl());

        ERROR_CONDITION(
                !(subscripted_symbol.get_type().is_fortran_array()
                    && subscripted_symbol.is_allocatable())
                &&
                !(subscripted_symbol.get_type().is_pointer()
                    && subscripted_symbol.get_type().points_to().is_fortran_array()),
                "The argument of 'ompss_opencl_allocate' intrinsic must be "
                "an allocatable array or a pointer to an array with all its bounds specified\n", 0);

        TL::Type array_type;
        int num_dimensions;
        bool is_allocatable;
        if (subscripted_symbol.is_allocatable())
        {
            array_type = subscripted_symbol.get_type();
            num_dimensions = subscripted_symbol.get_type().get_num_dimensions();
            is_allocatable = true;
        }
        else
        {
            array_type = subscripted_symbol.get_type().points_to();
            num_dimensions = array_type.get_num_dimensions();
            is_allocatable = false;
        }

        TL::Type element_type = array_type;
        while (element_type.is_array())
        {
            element_type = element_type.array_element();
        }

        ERROR_CONDITION(!array_type.is_array(), "This type should be an array type", 0);

        std::pair<TL::Type, std::pair<int, bool> > key =
            std::make_pair(element_type, std::make_pair(num_dimensions, is_allocatable));

        std::map<std::pair<TL::Type, std::pair<int, bool> > , Symbol>::iterator it_new_fun =
            declared_ocl_allocate_functions.find(key);

        // Reuse the auxiliar function if it already exists
        Symbol new_function_sym;
        if (it_new_fun != declared_ocl_allocate_functions.end())
        {
            new_function_sym = it_new_fun->second;
        }
        else
        {
            new_function_sym = create_new_function_opencl_allocate(
                    expr_stmt, subscripted_symbol, element_type, num_dimensions, is_allocatable);

            declared_ocl_allocate_functions[key] = new_function_sym;
        }

        // Replace the current intrinsic call by a call to the new function
        TL::Source actual_arg_array;
        Nodecl::NodeclBase subscripted_lvalue = subscripted.shallow_copy();
        subscripted_lvalue.set_type(subscripted_symbol.get_type().no_ref().get_lvalue_reference_to());

        actual_arg_array << as_expression(subscripted_lvalue);

        TL::Source actual_arg_bounds;
        Nodecl::List subscripts = arg.as<Nodecl::ArraySubscript>().get_subscripts().as<Nodecl::List>();
        for (Nodecl::List::reverse_iterator it = subscripts.rbegin();
                it != subscripts.rend();
                it++)
        {
            Nodecl::NodeclBase subscript = *it, lower, upper;

            if (it != subscripts.rbegin())
                actual_arg_bounds << ", ";

            if (subscript.is<Nodecl::Range>())
            {
                lower = subscript.as<Nodecl::Range>().get_lower();
                upper = subscript.as<Nodecl::Range>().get_upper();
            }
            else
            {
                lower = nodecl_make_integer_literal(
                        fortran_get_default_integer_type(),
                        const_value_get_signed_int(1), make_locus("", 0, 0));
                upper = subscript;
            }
            actual_arg_bounds << as_expression(lower) << "," << as_expression(upper);
        }

        TL::Source new_function_call;
        new_function_call
            << "CALL " << as_symbol(new_function_sym) << "("
            <<  actual_arg_array  << ", "
            <<  actual_arg_bounds << ")\n"
            ;

        expr_stmt.replace(new_function_call.parse_statement(expr_stmt));

    }

    static void handle_ompss_opencl_deallocate_intrinsic(
            Nodecl::FunctionCall function_call,
            Nodecl::NodeclBase expr_stmt)
    {
        Nodecl::List arguments = function_call.get_arguments().as<Nodecl::List>();
        ERROR_CONDITION(arguments.size() != 1, "More than one argument in ompss_opencl_deallocate call", 0);

        Nodecl::NodeclBase actual_argument = arguments[0];
        ERROR_CONDITION(!actual_argument.is<Nodecl::FortranActualArgument>(), "Unexpected tree", 0);

        Nodecl::NodeclBase arg = actual_argument.as<Nodecl::FortranActualArgument>().get_argument();
        TL::Symbol array_sym = ::fortran_data_ref_get_symbol(arg.get_internal_nodecl());

        ERROR_CONDITION(
                !(array_sym.get_type().is_fortran_array()
                    && array_sym.is_allocatable())
                &&
                !(array_sym.get_type().is_pointer()
                    && array_sym.get_type().points_to().is_fortran_array()),
                "The argument of 'ompss_opencl_deallocate' intrinsic must be "
                "an allocatable array or a pointer to an array\n", 0);

        // Replace the current intrinsic call by a call to the Nanos++ API
        TL::Symbol ptr_of_arr_sym = get_function_ptr_of(array_sym, expr_stmt.retrieve_context());

        TL::Source new_function_call;
        new_function_call
            << "CALL NANOS_OPENCL_DEALLOCATE_FORTRAN("
            <<      ptr_of_arr_sym.get_name() << "("<< as_expression(arg) << "))\n"
            ;

        expr_stmt.replace(new_function_call.parse_statement(expr_stmt));
    }

    void LoweringVisitor::visit(const Nodecl::ExpressionStatement& expr_stmt)
    {
        Nodecl::NodeclBase nest = expr_stmt.get_nest();
        if (IS_FORTRAN_LANGUAGE
                && nest.is<Nodecl::FunctionCall>())
        {
            Nodecl::FunctionCall function_call = nest.as<Nodecl::FunctionCall>();
            if (function_call.get_called().is<Nodecl::Symbol>())
            {
                TL::Symbol sym = function_call.get_called().as<Nodecl::Symbol>().get_symbol();
                // We are only interested in two intrinsic symbols
                if (sym.is_intrinsic())
                {
                    if(sym.get_name() == "ompss_opencl_allocate")
                    {
                        // We replace the intrinsic call by a call to a new function which:
                        //  - allocates a new temporary array with descriptor
                        //  - copies the array descriptor to the address of the array
                        //  - calls to the Nanos++ API to allocate the buffer in the shared memory
                        //  - deallocates the temporary array with descriptor
                        //
                        // Example:
                        //
                        //    ...
                        //    INTEGER, ALLOCATABLE :: V(:)
                        //    OMPSS_OPENCL_ALLOCATE(V(10))
                        //    ...
                        //
                        // Is transformed into:
                        //
                        //    SUBROUTINE NANOX_OPENCL_ALLOCATE_INTERNAL(ARR, LB1, UB1)
                        //        INTEGER, ALLOCATABLE :: ARR(:)
                        //        INTEGER :: LB1, UB1, ERR
                        //        INTEGER, ALLOCATABLE :: TMP(:)
                        //
                        //        ALLOCATE(TMP(LB1:UB1))
                        //
                        //        ERR = NANOS_MEMCPY(
                        //                MERCURIUM_GET_ADDRESS_OF(ARR),
                        //                MERCURIUM_GET_ADDRESS_OF(TMP),
                        //                48)
                        //
                        //        CALL NANOS_OPENCL_ALLOCATE_FORTRAN(
                        //            SIZEOF(TMP),
                        //            MERCURIUM_GET_ADDRESS_OF(ARR))
                        //
                        //        DEALLOCATE(TMP)
                        //    END SUBROUTINE NANOX_OPENCL_ALLOCATE_INTERNAL
                        //
                        //    ...
                        //    INTEGER, ALLOCATABLE :: V(:)
                        //    CALL NANOX_OPENCL_ALLOCATE_INTERNAL(V, 1, 10)
                        //    ...
                        //
                        // For more information: https://pm.bsc.es/projects/mcxx/ticket/1994
                        handle_ompss_opencl_allocate_intrinsic(
                                function_call,
                                _declared_ocl_allocate_functions,
                                expr_stmt);
                    }
                    else if (sym.get_name() == "ompss_opencl_deallocate")
                    {
                        // The transformation applied to this intrinsic is more
                        // simple than the other one, we only need to replace
                        // the call to the intrinsic by a call to the Nanos++
                        // API:
                        //
                        //    ...
                        //    INTEGER, ALLOCATABLE :: V(:)
                        //    ...
                        //    OMPSS_OPENCL_DEALLOCATE(V)
                        //    ...
                        //
                        // Is transformed into:
                        //
                        //    ...
                        //    INTEGER, ALLOCATABLE :: V(:)
                        //    ...
                        //    CALL NANOS_OPENCL_ALLOCATE_FORTRAN(MERCURIUM_GET_ADDRESS_OF(V))
                        //    ...
                        handle_ompss_opencl_deallocate_intrinsic(function_call, expr_stmt);
                    }
                }
            }
        }
        walk(expr_stmt.get_nest());
    }

}}
