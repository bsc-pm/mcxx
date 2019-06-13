/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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

#include "tl-checkpoint-visitor.hpp"

#include "tl-datareference.hpp"
#include "tl-omp-lowering-utils.hpp"

#include "tl-scope.hpp"

#include "cxx-diagnostic.h"
#include "cxx-cexpr.h"
#include "cxx-scope.h"
#include "fortran03-typeutils.h"

namespace TL { namespace Checkpoint {
    namespace {
        TL::Symbol get_function_symbol(const std::string& name)
        {
            TL::Symbol fun_sym = TL::Scope::get_global_scope().get_symbol_from_name(name);
            ERROR_CONDITION(!fun_sym.is_valid() || !fun_sym.is_function(), "Invalid '%s' function symbol\n", name.c_str());
            return fun_sym;
        }

        TL::Symbol get_basic_tcl_data_type(TL::Type type)
        {
            TL::Type aux_type = type.no_ref().get_unqualified_type();

            const char* enumerator_name;

            if (aux_type.is_char())
                enumerator_name ="TCL_CHAR";
            else if (aux_type.is_signed_char())
                enumerator_name ="TCL_SCHAR";
            else if (aux_type.is_unsigned_char())
                enumerator_name ="TCL_UCHAR";
            else if (aux_type.is_signed_short_int())
                enumerator_name ="TCL_SHORT";
            else if (aux_type.is_unsigned_short_int())
                enumerator_name ="TCL_USHORT";
            else if (aux_type.is_signed_int())
                enumerator_name ="TCL_INT";
            else if (aux_type.is_unsigned_int())
                enumerator_name ="TCL_UINT";
            else if (aux_type.is_signed_long_int())
                enumerator_name ="TCL_LONG";
            else if (aux_type.is_unsigned_long_int())
                enumerator_name ="TCL_ULONG";
            else if (aux_type.is_float())
                enumerator_name ="TCL_FLOAT";
            else if (aux_type.is_double())
                enumerator_name ="TCL_DOUBLE";
            else if (aux_type.is_long_double())
                enumerator_name ="TCL_LDOUBLE";
            else
                enumerator_name ="TCL_UNKNOWN";


            TL::Symbol enumerator = TL::Scope::get_global_scope().get_symbol_from_name(enumerator_name);
            ERROR_CONDITION(!enumerator.is_valid(), "Invalid '%s' enumerator\n", enumerator_name);
            return enumerator;
        }


        void compute_dimensionality_information_c(
                TL::Type type,
                // Out
                TL::ObjectList<Nodecl::NodeclBase>& arguments_list)
        {
            Nodecl::NodeclBase size, lower_bound, upper_bound;
            if (type.is_array())
            {
                TL::Type element_type = type.array_element();
                if (element_type.is_array())
                    compute_dimensionality_information_c(element_type, arguments_list);

                size = type.array_get_size().shallow_copy();

                if (type.array_is_region())
                    type.array_get_region_bounds(lower_bound, upper_bound);
                else
                    type.array_get_bounds(lower_bound, upper_bound);

                lower_bound = lower_bound.shallow_copy();
                upper_bound =
                    Nodecl::Add::make(
                            upper_bound.shallow_copy(),
                            const_value_to_nodecl(const_value_get_one(4, 1)),
                            upper_bound.get_type().no_ref());

                // Continuous dimension should be expressed in bytes
                if (!element_type.is_array())
                {
                    TL::Symbol enumerator = get_basic_tcl_data_type(element_type);
                    arguments_list.append(enumerator.make_nodecl(/*set_ref_type*/false));

                    Nodecl::NodeclBase element_type_size = Nodecl::Sizeof::make(
                            Nodecl::Type::make(element_type),
                            Nodecl::NodeclBase::null(),
                            get_size_t_type());

                    size = Nodecl::Mul::make(size, element_type_size, size.get_type().no_ref());
                    lower_bound = Nodecl::Mul::make(
                            lower_bound,
                            element_type_size.shallow_copy(),
                            lower_bound.get_type().no_ref());

                    upper_bound = Nodecl::Mul::make(
                            upper_bound,
                            element_type_size.shallow_copy(),
                            upper_bound.get_type().no_ref());
                }
            }
            else
            {
                TL::Symbol enumerator = get_basic_tcl_data_type(type);
                arguments_list.append(enumerator.make_nodecl(/*set_ref_type*/false));

                // Continuous dimension should be expressed in bytes
                size = Nodecl::Sizeof::make(
                        Nodecl::Type::make(type),
                        Nodecl::NodeclBase::null(),
                        get_size_t_type());

                lower_bound = const_value_to_nodecl(const_value_get_zero(4, 1));
                upper_bound = size.shallow_copy();
            }

            arguments_list.append(size);
            arguments_list.append(lower_bound);
            arguments_list.append(upper_bound);
        }

        void compute_dimensionality_information_fortran(
                const TL::DataReference& data_ref,
                TL::Type type,
                // Out
                TL::ObjectList<Nodecl::NodeclBase>& arguments_list)
        {
            Nodecl::NodeclBase size, lower_bound, upper_bound;
            if (type.is_array())
            {
                TL::Type element_type = type.array_element();
                if (element_type.is_array())
                    compute_dimensionality_information_fortran(data_ref, element_type, arguments_list);

                Nodecl::NodeclBase array_lb, array_ub;
                {
                    type.array_get_bounds(array_lb, array_ub);
                    if (array_lb.is_null())
                        array_lb = TL::OpenMP::Lowering::Utils::Fortran::get_lower_bound(data_ref, type.fortran_rank());
                    else
                        array_lb = array_lb.shallow_copy();

                    if (array_ub.is_null())
                        array_ub = TL::OpenMP::Lowering::Utils::Fortran::get_upper_bound(data_ref, type.fortran_rank());
                    else
                        array_ub = array_ub.shallow_copy();
                }

                Nodecl::NodeclBase region_lb, region_ub;
                {
                    if (type.array_is_region())
                    {
                        type.array_get_region_bounds(region_lb, region_ub);
                    }
                    else
                    {
                        region_lb = array_lb.shallow_copy();
                        region_ub = array_ub.shallow_copy();
                    }
                }

                size = TL::OpenMP::Lowering::Utils::Fortran::get_size_for_dimension(data_ref, type, type.fortran_rank());

                lower_bound = Nodecl::Minus::make(
                        region_lb,
                        array_lb,
                        region_lb.get_type().no_ref());

                //XXX: ADD one?
                upper_bound = Nodecl::Add::make(
                        Nodecl::Minus::make(
                            region_ub,
                            array_lb.shallow_copy(),
                            region_lb.get_type().no_ref()),
                        const_value_to_nodecl(const_value_get_one(8, 1)),
                        region_lb.get_type().no_ref());

                // Continuous dimension should be expressed in bytes
                if (!element_type.is_array())
                {
                    TL::Symbol enumerator = get_basic_tcl_data_type(element_type);
                    arguments_list.append(enumerator.make_nodecl(/*set_ref_type*/false));

                    Nodecl::NodeclBase element_type_size = Nodecl::Sizeof::make(
                            Nodecl::Type::make(element_type),
                            Nodecl::NodeclBase::null(),
                            get_size_t_type());

                    size = Nodecl::Mul::make(
                            Nodecl::ParenthesizedExpression::make(size, size.get_type().no_ref()),
                            element_type_size,
                            size.get_type().no_ref());

                    lower_bound = Nodecl::Mul::make(
                            Nodecl::ParenthesizedExpression::make(lower_bound, lower_bound.get_type().no_ref()),
                            element_type_size.shallow_copy(),
                            lower_bound.get_type().no_ref());

                    upper_bound = Nodecl::Mul::make(
                            Nodecl::ParenthesizedExpression::make(upper_bound, upper_bound.get_type().no_ref()),
                            element_type_size.shallow_copy(),
                            upper_bound.get_type().no_ref());
                }
            }
            else
            {
                TL::Symbol enumerator = get_basic_tcl_data_type(type);
                arguments_list.append(enumerator.make_nodecl(/*set_ref_type*/false));

                size = data_ref.get_sizeof().shallow_copy();
                lower_bound = const_value_to_nodecl(const_value_get_zero(8, 0));
                upper_bound = size.shallow_copy();
            }

            // Fortran is a bit picky checking the actual arguments types, for
            // this reason we may need to add some conversions
            TL::Type param_type = fortran_choose_int_type_from_kind(8);
            arguments_list.append(Nodecl::Conversion::make(size, param_type));
            arguments_list.append(Nodecl::Conversion::make(lower_bound, param_type));
            arguments_list.append(Nodecl::Conversion::make(upper_bound, param_type));
        }

        void compute_base_address_and_dimensionality_information(
                const TL::DataReference& data_ref,
                // Out
                TL::ObjectList<Nodecl::NodeclBase>& arguments_list)
        {
            Nodecl::NodeclBase base_address =
                Nodecl::Conversion::make(
                        data_ref.get_base_address().shallow_copy(),
                        TL::Type::get_void_type().get_pointer_to());

            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                base_address.set_text("C");

            arguments_list.append(base_address);

            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                compute_dimensionality_information_c(data_ref.get_data_type(), arguments_list);
            else
                compute_dimensionality_information_fortran(data_ref, data_ref.get_data_type(), arguments_list);
        }

        void register_checkpoint_data(const TL::ObjectList<Nodecl::NodeclBase>& data, int max_dims, Nodecl::List &new_stmts)
        {
            for (TL::ObjectList<Nodecl::NodeclBase>::const_iterator it = data.begin();
                    it != data.end();
                    it++)
            {
                TL::DataReference data_ref(*it);
                TL::Type data_type = data_ref.get_data_type();
                TL::Symbol register_fun;
                {
                    int num_dims_dep = data_type.is_array() ? data_type.get_num_dimensions() : 1;
                    if (num_dims_dep > max_dims)
                    {
                        error_printf_at(it->get_locus(),
                                "The current TCL API does not support '%d' dimensions", num_dims_dep);
                    }

                    std::stringstream ss;
                    ss << "tcl_register_cpinfo" << num_dims_dep;
                    register_fun = get_function_symbol(ss.str().c_str());
                }

                if (data_ref.is_multireference())
                    internal_error("Multireferences are not supported yet", 0);

                TL::ObjectList<Nodecl::NodeclBase> args;
                compute_base_address_and_dimensionality_information(data_ref, args);

                Nodecl::NodeclBase function_call = Nodecl::FunctionCall::make(
                        register_fun.make_nodecl(/*set_ref_type*/true),
                        Nodecl::List::make(args),
                        /*alternate-name*/ Nodecl::NodeclBase::null(),
                        /*function-form*/ Nodecl::NodeclBase::null(),
                        TL::Type::get_void_type());

                new_stmts.append(Nodecl::ExpressionStatement::make(function_call));
            }
        }
    }

    void CheckpointVisitor::visit(const Nodecl::Checkpoint::Init& init_construct)
    {
        CheckpointEnvironment env(init_construct.get_environment());

        TL::Symbol init_fun = get_function_symbol("tcl_init");

        Nodecl::List args;
        if (!env.communicator.is_null())
            args.append(env.communicator);

        Nodecl::NodeclBase function_call = Nodecl::FunctionCall::make(
                init_fun.make_nodecl(/*set_ref_type*/true),
                args,
                /*alternate-name*/ Nodecl::NodeclBase::null(),
                /*function-form*/ Nodecl::NodeclBase::null(),
                TL::Type::get_void_type());

        init_construct.replace(Nodecl::ExpressionStatement::make(function_call));
    }

    void CheckpointVisitor::visit(const Nodecl::Checkpoint::Shutdown& shutdown_construct)
    {
        TL::Symbol shutdown_fun = get_function_symbol("tcl_shutdown");

        Nodecl::NodeclBase function_call = Nodecl::FunctionCall::make(
                shutdown_fun.make_nodecl(/*set_ref_type*/true),
                /*args*/ Nodecl::NodeclBase::null(),
                /*alternate-name*/ Nodecl::NodeclBase::null(),
                /*function-form*/ Nodecl::NodeclBase::null(),
                TL::Type::get_void_type());

        shutdown_construct.replace(Nodecl::ExpressionStatement::make(function_call));
    }

    void CheckpointVisitor::visit(const Nodecl::Checkpoint::Load& load_construct)
    {
        CheckpointEnvironment env(load_construct.get_environment());

        Nodecl::List new_stmts;
        {
            TL::Symbol beg_load_fun = get_function_symbol("tcl_begin_load");

            Nodecl::List args;
            args.append(const_value_to_nodecl(const_value_get_zero(get_sizeof_type(get_size_t_type()), 1)));

            Nodecl::NodeclBase function_call = Nodecl::FunctionCall::make(
                    beg_load_fun.make_nodecl(/*set_ref_type*/true),
                    args,
                    /*alternate-name*/ Nodecl::NodeclBase::null(),
                    /*function-form*/ Nodecl::NodeclBase::null(),
                    TL::Type::get_void_type());

            new_stmts.append(Nodecl::ExpressionStatement::make(function_call));
        }

        register_checkpoint_data(env.data_exprs, _phase->get_tcl_max_dims(), new_stmts);

        {
            TL::Symbol end_load_fun = get_function_symbol("tcl_end_load");

            Nodecl::NodeclBase function_call = Nodecl::FunctionCall::make(
                    end_load_fun.make_nodecl(/*set_ref_type*/true),
                    /*args*/ Nodecl::NodeclBase::null(),
                    /*alternate-name*/ Nodecl::NodeclBase::null(),
                    /*function-form*/ Nodecl::NodeclBase::null(),
                    TL::Type::get_void_type());

            new_stmts.append(Nodecl::ExpressionStatement::make(function_call));
        }

        load_construct.replace(new_stmts);

    }

    void CheckpointVisitor::visit(const Nodecl::Checkpoint::Store& store_construct)
    {
        CheckpointEnvironment env(store_construct.get_environment());

        Nodecl::List new_stmts;

        {
            TL::Symbol beg_store_fun = get_function_symbol("tcl_begin_store");

            // int level, size_t id, bool is_mandatory, void (*handler)(int)
            Nodecl::List args;
            args.append(Nodecl::Conversion::make(env.level, TL::Type::get_int_type()));

            Nodecl::NodeclBase checkpoint_kind = env.kind;

            if (checkpoint_kind.is_null())
            {
                TL::Symbol default_checkpoint_kind =
                    TL::Scope::get_global_scope().get_symbol_from_name("CHK_FULL");
                ERROR_CONDITION(!default_checkpoint_kind.is_valid(), "Invalid 'CHK_FULL' enumerator\n", 0);

                checkpoint_kind = default_checkpoint_kind.make_nodecl(/*set_ref_type*/false);
            }
            args.append(checkpoint_kind);

            args.append(Nodecl::Conversion::make(env.id, TL::Type::get_size_t_type()));
            args.append(Nodecl::Conversion::make(const_value_to_nodecl(const_value_get_one(4, 1)), TL::Type::get_bool_type()));
            args.append(const_value_to_nodecl(const_value_get_zero(get_sizeof_type(get_size_t_type()), 1)));

            Nodecl::NodeclBase function_call = Nodecl::FunctionCall::make(
                    beg_store_fun.make_nodecl(/*set_ref_type*/true),
                    args,
                    /*alternate-name*/ Nodecl::NodeclBase::null(),
                    /*function-form*/ Nodecl::NodeclBase::null(),
                    TL::Type::get_void_type());

            new_stmts.append(Nodecl::ExpressionStatement::make(function_call));
        }

        register_checkpoint_data(env.data_exprs, _phase->get_tcl_max_dims(), new_stmts);

        {
            TL::Symbol end_store_fun = get_function_symbol("tcl_end_store");

            Nodecl::NodeclBase function_call = Nodecl::FunctionCall::make(
                    end_store_fun.make_nodecl(/*set_ref_type*/true),
                    /*args*/ Nodecl::NodeclBase::null(),
                    /*alternate-name*/ Nodecl::NodeclBase::null(),
                    /*function-form*/ Nodecl::NodeclBase::null(),
                    TL::Type::get_void_type());

            new_stmts.append(Nodecl::ExpressionStatement::make(function_call));
        }

        if (!env.if_expr.is_null())
        {
            TL::Scope new_scope = TL::Scope(new_block_context(store_construct.retrieve_context().get_decl_context()));


            Nodecl::NodeclBase if_stmt = Nodecl::IfElseStatement::make(
                    env.if_expr,
                    Nodecl::List::make(
                        Nodecl::Context::make(
                            Nodecl::List::make(
                                Nodecl::CompoundStatement::make(
                                    new_stmts,
                                    /*finally*/ Nodecl::NodeclBase::null())),
                            new_scope)),
                    /*else*/ Nodecl::NodeclBase::null());

            store_construct.replace(if_stmt);
        }
        else
        {
            store_construct.replace(new_stmts);
        }
    }
}}
