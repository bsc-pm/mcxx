/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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

#include "fortran03-typeutils.h"
#include "fortran03-typeenviron.h"

#include "tl-nanos.hpp"
#include "tl-nodecl-utils-fortran.hpp"
#include "tl-counters.hpp"
#include "tl-outline-info.hpp"
#include "tl-lowering-visitor.hpp"
#include "tl-nanox-ptr.hpp"

namespace TL { namespace Nanox {

    struct ReductionReplaceSymbolVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            const std::map<TL::Symbol, Nodecl::NodeclBase> & _translation_map;
        public:

            ReductionReplaceSymbolVisitor(const std::map<TL::Symbol, Nodecl::NodeclBase> & map)
                : _translation_map(map)
            {
            }

            void visit(const Nodecl::Symbol &node)
            {
                TL::Symbol sym = node.get_symbol();

                std::map<TL::Symbol, Nodecl::NodeclBase>::const_iterator it = _translation_map.find(sym);
                if (it == _translation_map.end())
                    return;

                node.replace(it->second.shallow_copy());
            }
    };


    static TL::Symbol create_reduction_function_internal(
            OpenMP::Reduction* red,
            Nodecl::NodeclBase construct,
            std::string function_name,
            TL::Type omp_out_type,
            TL::Type omp_in_type,
            bool omp_out_type_is_allocatable)
    {
        Nodecl::NodeclBase function_body;
        Source src;
        src << "void " << function_name << "("
            << as_type(omp_out_type.no_ref().get_lvalue_reference_to()) << " omp_out,"
            << as_type(omp_in_type.no_ref().get_lvalue_reference_to()) << " omp_in)"
            << "{"
            <<    statement_placeholder(function_body)
            << "}"
            ;

        FORTRAN_LANGUAGE()
        {
            Source::source_language = SourceLanguage::C;
        }

        Nodecl::NodeclBase function_code = src.parse_global(construct.retrieve_context().get_global_scope());

        FORTRAN_LANGUAGE()
        {
            Source::source_language = SourceLanguage::Current;
        }

        TL::Scope inside_function = ReferenceScope(function_body).get_scope();
        TL::Symbol param_omp_in = inside_function.get_symbol_from_name("omp_in");
        ERROR_CONDITION(!param_omp_in.is_valid(), "Symbol omp_in not found", 0);
        TL::Symbol param_omp_out = inside_function.get_symbol_from_name("omp_out");
        ERROR_CONDITION(!param_omp_out.is_valid(), "Symbol omp_out not found", 0);
        param_omp_out.get_internal_symbol()->entity_specs.is_allocatable = omp_out_type_is_allocatable;

        TL::Symbol function_sym = inside_function.get_symbol_from_name(function_name);
        ERROR_CONDITION(!function_sym.is_valid(), "Symbol %s not found", function_name.c_str());

        Nodecl::NodeclBase expanded_combiner = red->get_combiner().shallow_copy();

        std::map<TL::Symbol, Nodecl::NodeclBase> translation_map;

        if (param_omp_in.get_type().no_ref().is_pointer())
        {
            translation_map[red->get_omp_in()] = Nodecl::Dereference::make(
                    param_omp_in.make_nodecl(true), param_omp_in.get_type().no_ref().points_to());
        }
        else
        {
            translation_map[red->get_omp_in()] = param_omp_in.make_nodecl(true);
        }

        if (param_omp_out.get_type().no_ref().is_pointer())
        {
            translation_map[red->get_omp_out()] = Nodecl::Dereference::make(
                    param_omp_out.make_nodecl(true), param_omp_out.get_type().no_ref().points_to());
        }
        else
        {
            translation_map[red->get_omp_out()] = param_omp_out.make_nodecl(true);
        }

        ReductionReplaceSymbolVisitor expander_visitor(translation_map);
        expander_visitor.walk(expanded_combiner);

        function_body.replace(
                Nodecl::List::make(Nodecl::ExpressionStatement::make(expanded_combiner)));

        // As the reduction function is needed during the instantiation of
        // the task, this function should be inserted before the construct
        Nodecl::Utils::prepend_to_enclosing_top_level_location(construct, function_code);

        return function_sym;
    }

    static std::pair<TL::Symbol, TL::Symbol> create_reduction_functions_internal(
            OpenMP::Reduction* red,
            TL::Symbol reduction_item,
            Nodecl::NodeclBase construct,
            LoweringVisitor::reduction_task_map_t & reduction_map)
    {
        TL::Type reduction_type = reduction_item.get_type();

        // Reuse the reduction functions If they are already computed
        LoweringVisitor::reduction_task_map_t::iterator it = reduction_map.find(red);
        if (it != reduction_map.end())
        {
            return it->second;
        }

        std::stringstream red_fun;
        red_fun << "nanos_red_" << red << "_" << simple_hash_str(construct.get_filename().c_str());
        std::stringstream red_fun_orig_var;
        red_fun_orig_var << "nanos_red_" << red << "_" << simple_hash_str(construct.get_filename().c_str()) << "_orig_var";

        TL::Symbol reduction_function, reduction_function_original_var;
        if (IS_FORTRAN_LANGUAGE
                && reduction_type.is_array())
        {
            reduction_function = create_reduction_function_internal(
                    red,
                    construct,
                    red_fun.str(),
                    /* omp_out_type */ reduction_type.get_pointer_to(),
                    /* omp_in_type */ reduction_type.get_pointer_to(),
                    /* omp_out_is_allocatable */ false);

            reduction_function_original_var = create_reduction_function_internal(
                    red,
                    construct,
                    red_fun_orig_var.str(),
                    /* omp_out_type */ reduction_type,
                    /* omp_in_type */ reduction_type.get_pointer_to(),
                    reduction_item.get_internal_symbol()->entity_specs.is_allocatable);
        }
        else
        {
            reduction_function = create_reduction_function_internal(
                    red,
                    construct,
                    red_fun.str(),
                    /* omp_out_type */ reduction_type,
                    /* omp_in_type */ reduction_type,
                    /* omp_out_is_allocatable */ false);

            reduction_function_original_var = reduction_function;
        }

        std::pair<TL::Symbol, TL::Symbol> red_funcs = std::make_pair(reduction_function, reduction_function_original_var);
        reduction_map[red] = red_funcs;
        return red_funcs;
    }

    static void create_reduction_functions(OpenMP::Reduction* red,
            Nodecl::NodeclBase construct,
            TL::Symbol reduction_item,
            TL::Symbol& reduction_function,
            TL::Symbol& reduction_function_original_var,
            LoweringVisitor::reduction_task_map_t & reduction_map)
    {
        //std::cerr << "DEBUG: <creating reduction function> " << reduction_item.get_type().print_declarator() << std::endl;
        std::pair<TL::Symbol, TL::Symbol> red_funcs =
            create_reduction_functions_internal(red, reduction_item, construct, reduction_map);

        reduction_function = red_funcs.first;
        reduction_function_original_var = red_funcs.second;
    }

    static TL::Symbol create_initializer_function_c(
            OpenMP::Reduction* red,
            TL::Type reduction_type,
            Nodecl::NodeclBase construct,
            LoweringVisitor::reduction_map_t& initializer_map)
    {
        LoweringVisitor::reduction_map_t::iterator it = initializer_map.find(red);
        if (it != initializer_map.end())
        {
            return it->second;
        }

        std::string fun_name;
        {
            std::stringstream ss;
            ss << "nanos_ini_" << red << "_" << simple_hash_str(construct.get_filename().c_str());
            fun_name = ss.str();
        }

        Nodecl::NodeclBase function_body;
        Source src;
        src << "void " << fun_name << "("
            <<      as_type(reduction_type.no_ref().get_lvalue_reference_to()) << " omp_priv,"
            <<      as_type(reduction_type.no_ref().get_lvalue_reference_to()) << " omp_orig)"
            << "{"
            <<    statement_placeholder(function_body)
            << "}"
            ;

        Nodecl::NodeclBase function_code = src.parse_global(construct.retrieve_context().get_global_scope());

        TL::Scope inside_function = ReferenceScope(function_body).get_scope();
        TL::Symbol param_omp_priv = inside_function.get_symbol_from_name("omp_priv");
        ERROR_CONDITION(!param_omp_priv.is_valid(), "Symbol omp_priv not found", 0);

        TL::Symbol param_omp_orig = inside_function.get_symbol_from_name("omp_orig");
        ERROR_CONDITION(!param_omp_orig.is_valid(), "Symbol omp_orig not found", 0);

        TL::Symbol function_sym = inside_function.get_symbol_from_name(fun_name);
        ERROR_CONDITION(!function_sym.is_valid(), "Symbol %s not found", fun_name.c_str());

        Nodecl::NodeclBase initializer = red->get_initializer().shallow_copy();
        if (initializer.is<Nodecl::StructuredValue>())
        {
            Nodecl::StructuredValue structured_value = initializer.as<Nodecl::StructuredValue>();
            if (structured_value.get_form().is<Nodecl::StructuredValueBracedImplicit>())
            {
                structured_value.set_form(Nodecl::StructuredValueCompoundLiteral::make());
            }
        }

        Nodecl::Utils::SimpleSymbolMap translation_map;

        translation_map.add_map(red->get_omp_priv(), param_omp_priv);
        translation_map.add_map(red->get_omp_orig(), param_omp_orig);

        Nodecl::NodeclBase new_initializer = Nodecl::Utils::deep_copy(initializer, inside_function, translation_map);

        if (red->get_is_initialization())
        {
            // The original initializer was something like 'omp_priv = expr1', but the
            // new_initializer only represents the lhs expression (in our example, expr1).
            // For this reason we create manually an assignment expression.
            Nodecl::NodeclBase param_omp_priv_ref = Nodecl::Symbol::make(param_omp_priv);
            param_omp_priv_ref.set_type(param_omp_priv.get_type());

            function_body.replace(
                    Nodecl::List::make(
                        Nodecl::ExpressionStatement::make(
                            Nodecl::Assignment::make(
                                param_omp_priv_ref,
                                new_initializer,
                                param_omp_priv_ref.get_type().no_ref())
                            )));
        }
        else
        {
            function_body.replace(
                    Nodecl::List::make(Nodecl::ExpressionStatement::make(new_initializer)));
        }

        initializer_map[red] = function_sym;

        // As the initializer function is needed during the instantiation of
        // the task, this function should be inserted before the construct
        Nodecl::Utils::prepend_to_enclosing_top_level_location(construct,
                function_code);

        return function_sym;
    }

    static TL::Symbol create_initializer_function_fortran(
            OpenMP::Reduction* red,
            TL::Type reduction_type,
            Nodecl::NodeclBase construct,
            LoweringVisitor::reduction_map_t & initializer_map)
    {
        LoweringVisitor::reduction_map_t::iterator it = initializer_map.find(red);
        if (it != initializer_map.end())
        {
            return it->second;
        }

        std::string fun_name;
        {
            std::stringstream ss;
            ss << "nanos_ini_" << red << "_" << simple_hash_str(construct.get_filename().c_str());
            fun_name = ss.str();
        }

        Nodecl::NodeclBase initializer = red->get_initializer().shallow_copy();

        TL::Source omp_out_pointer_opt;
        TL::Source extra_stuff_array_red;
        if (reduction_type.is_array())
        {
            omp_out_pointer_opt << ", POINTER ";
            Source extra_dims;
            {
                TL::Type t = reduction_type;
                int rank = 0;
                if (t.is_fortran_array())
                {
                    rank = t.fortran_rank();
                }

                int i;
                for (i = 0; i < rank; i++)
                {
                    Source lbound_src;
                    lbound_src << "LBOUND(omp_orig, DIM = " << (rank - i) << ")";
                    Source ubound_src;
                    ubound_src << "UBOUND(omp_orig, DIM = " << (rank - i) << ")";

                    extra_dims << "(" << lbound_src << ":" << ubound_src << ")";

                    t = t.array_element();
                }

                extra_stuff_array_red << "ALLOCATE(omp_out" << extra_dims <<")\n";

            }
        }

        Source src;
        src << "SUBROUTINE " << fun_name << "(omp_out, omp_orig)\n"
            <<    "IMPLICIT NONE\n"
            <<    as_type(reduction_type) << omp_out_pointer_opt << " ::  omp_out\n"
            <<    as_type(reduction_type) <<  " :: omp_orig\n"
            <<    extra_stuff_array_red
            <<    "omp_out = " << as_expression(initializer) << "\n"
            << "END SUBROUTINE " << fun_name << "\n";
        ;

        TL::Scope global_scope = construct.retrieve_context().get_global_scope();
        Nodecl::NodeclBase function_code = src.parse_global(global_scope);
        TL::Symbol function_sym = global_scope.get_symbol_from_name(fun_name);

        ERROR_CONDITION(!function_sym.is_valid(), "Symbol %s not found", fun_name.c_str());

        initializer_map[red] = function_sym;

        // As the initializer function is needed during the instantiation of
        // the task, this function should be inserted before the construct
        Nodecl::Utils::prepend_to_enclosing_top_level_location(construct,
                function_code);

        return function_sym;
    }

    static void create_initializer_function(OpenMP::Reduction* red,
            Nodecl::NodeclBase construct,
            TL::Type reduction_type,
            TL::Symbol& initializer_function,
            LoweringVisitor::reduction_map_t& initializer_map)
    {
        //std::cerr << "DEBUG: <creating initialize function> " << reduction_type.print_declarator() << std::endl;
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            initializer_function = create_initializer_function_c(red, reduction_type, construct, initializer_map);
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            initializer_function = create_initializer_function_fortran(red, reduction_type, construct, initializer_map);
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }

    class ReplaceReductionSymbols : public Nodecl::ExhaustiveVisitor<void>
    {
        const std::map<TL::Symbol, Nodecl::NodeclBase>& _reduction_symbol_to_nodecl_map;

        public:

        ReplaceReductionSymbols(const std::map<TL::Symbol, Nodecl::NodeclBase>& reduction_symbol_to_nodecl_map)
            : _reduction_symbol_to_nodecl_map(reduction_symbol_to_nodecl_map)
        {
        }

        void visit(const Nodecl::ObjectInit& node)
        {
            TL::Symbol sym = node.get_symbol();
            if (sym.get_value().is_null())
                return;

            walk(sym.get_value());
        }

        void visit(const Nodecl::Symbol& node)
        {
            TL::Symbol sym = node.get_symbol();
            std::map<TL::Symbol, Nodecl::NodeclBase>::const_iterator it = _reduction_symbol_to_nodecl_map.find(sym);
            if (it == _reduction_symbol_to_nodecl_map.end())
                return;

            node.replace(it->second.shallow_copy());
        }

    };

    void LoweringVisitor::handle_reductions_on_task(
            Nodecl::NodeclBase construct, OutlineInfo& outline_info, Nodecl::NodeclBase statements,
            Nodecl::NodeclBase& final_statements)
    {
        if (Nanos::Version::interface_is_at_least("task_reduction", 1000)
                || (Nanos::Version::interface_is_at_least("reduction_on_task", 1000)))
        {
            bool there_are_reductions_on_task = false;
            TL::Source reductions_stuff, reductions_stuff_final;
            std::map<TL::Symbol, std::string> reduction_symbols_map;

            TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();
            for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
                    it != data_items.end();
                    it++)
            {
                if ((*it)->get_sharing() != OutlineDataItem::SHARING_CONCURRENT_REDUCTION)
                    continue;

                TL::Symbol reduction_item = (*it)->get_symbol();

                there_are_reductions_on_task = true;

                std::string storage_name = (*it)->get_field_name() + "_storage";

                std::pair<TL::OpenMP::Reduction*, TL::Type> red_info_pair= (*it)->get_reduction_info();
                TL::OpenMP::Reduction* reduction_info = red_info_pair.first;

                TL::Type reduction_type = reduction_item.get_type();

                TL::Symbol reduction_function, reduction_function_original_var;
                create_reduction_functions(reduction_info,
                        construct,
                        reduction_item,
                        reduction_function,
                        reduction_function_original_var,
                        _reduction_on_tasks_red_map);

                TL::Symbol initializer_function;
                create_initializer_function(reduction_info,
                        construct,
                        reduction_type,
                        initializer_function,
                        _reduction_on_tasks_ini_map);

                if (Nanos::Version::interface_is_at_least("task_reduction", 1000))
                {
                    // Mandatory TL::Sources to be filled by any reduction
                    TL::Source
                        orig_address, // address of the original reduction variable
                        storage_var, // variable which holds the address of the storage
                        final_stmts; // This source contains the stmts for the final clause support

                    // Specific TL::Sources to be filled only by Fortran array reduction
                    TL::Source extra_array_red_decl, extra_array_red_memcpy;

                    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                    {
                        storage_var << storage_name;
                        orig_address  <<  "(void *) &" << (*it)->get_field_name();
                        final_stmts
                            << "if (" << storage_var << " == 0)"
                            << "{"
                            <<     storage_name  << " = &" << (*it)->get_field_name() << ";"
                            << "}"
                            ;
                    }
                    else
                    {
                        if (reduction_type.is_array())
                        {
                            size_t size_of_array_descriptor =
                                fortran_size_of_array_descriptor(
                                        fortran_get_rank0_type(reduction_type.get_internal_type()),
                                        fortran_get_rank_of_type(reduction_type.get_internal_type()));

                            TL::Symbol ptr_of_sym = get_function_ptr_of(reduction_item, construct.retrieve_context());
                            if (reduction_item.is_allocatable())
                            {
                                orig_address << ptr_of_sym.get_name() << "(" << (*it)->get_field_name() << ")";
                            }
                            else
                            {
                                orig_address <<  "(void *) &" << (*it)->get_field_name();
                            }

                            extra_array_red_decl << "void* indirect;";
                            storage_var << "indirect";

                            extra_array_red_memcpy
                                << "err = nanos_memcpy("
                                <<      "(void **) &" << storage_name << ","
                                <<      "indirect,"
                                <<      size_of_array_descriptor << ");"
                                ;

                            final_stmts
                                << "if (" << storage_var << " == 0)"
                                << "{"
                                <<     "err = nanos_memcpy("
                                <<         "(void **) &" << storage_name << ","
                                <<         orig_address << ","
                                <<         size_of_array_descriptor << ");"
                                << "}"
                                << "else"
                                << "{"
                                <<     extra_array_red_memcpy
                                << "}"
                                ;
                        }
                        else
                        {
                            // We need to convert a void* type into a pointer to the reduction type.
                            // As a void* in FORTRAN is represented as an INTEGER(8), we cannot do this
                            // conversion directly in the FORTRAN source. For this reason we introduce
                            // a new function that will be defined in a C file.
                            TL::Symbol func = TL::Nanox::get_function_ptr_conversion(
                                    // Destination
                                    reduction_type.get_pointer_to(),
                                    // Origin
                                    TL::Type::get_void_type().get_pointer_to(),
                                    construct.retrieve_context());

                            orig_address <<  "(void *) &"<< (*it)->get_field_name();
                            storage_var << storage_name;


                            final_stmts
                                << "if (" << storage_var << " == 0)"
                                << "{"
                                <<     storage_name << " = " << func.get_name() << "(&" << (*it)->get_field_name() << ");"
                                << "}"
                                ;
                        }

                    }

                    reductions_stuff
                        << extra_array_red_decl
                        << as_type(reduction_type.get_pointer_to()) << " " << storage_name << ";"
                        << "err = nanos_task_reduction_get_thread_storage("
                        <<         orig_address  << ","
                        <<         "(void **) &" << storage_var << ");"
                        << extra_array_red_memcpy
                        ;

                    reductions_stuff_final
                        << extra_array_red_decl
                        << as_type(reduction_type.get_pointer_to()) << " " << storage_name << ";"
                        << "err = nanos_task_reduction_get_thread_storage("
                        <<         orig_address  << ","
                        <<         "(void **) &" << storage_var << ");"
                        << final_stmts
                        ;
                }
                else /* JAN's Version */
                {
                    std::string cache_storage = (*it)->get_field_name() + "_cache_storage";
                    reductions_stuff
                        << as_type(reduction_type.get_pointer_to()) << " " << storage_name << ";"
                        << "nanos_TPRS_t* " << cache_storage << ";"
                        << "err = nanos_reduction_request_tprs("
                        <<      "(void *) &" << (*it)->get_field_name() << "," // target
                        <<      "sizeof(" << as_type(reduction_type) << "),"  // size
                        <<      "(void (*)(void *, void *)) &" << reduction_function.get_name() << "," // reducer
                        <<      "(void (*)(void *, void *)) & " << initializer_function.get_name() << "," // initializer
                        <<      "(void (*)(void *, void *, void*)) reduction_reduce," // reducer atomic
                        <<      "(void (*)(void *)) reduction_flush," // flush
                        <<      "&" << cache_storage << ");"  // storage
                        ;

                    TL::Source auxiliar_final, auxiliar_final2;
                    reductions_stuff_final
                        << as_type(reduction_type.get_pointer_to()) << " " << storage_name << ";"
                        << "err = nanos_reduction_check_target((void *) &" << (*it)->get_field_name() << ", &is_registered);"
                        << "if (is_registered)"
                        << "{"
                        <<      "nanos_TPRS_t* " << cache_storage << ";"
                        <<      "err = nanos_reduction_request_tprs("
                        <<           "(void *) &" << (*it)->get_field_name() << "," // target
                        <<           "sizeof(" << as_type(reduction_type) << "),"  // size
                        <<           "(void (*)(void *, void *)) &" << reduction_function.get_name() << "," // reducer
                        <<           "(void (*)(void *, void *)) & " << initializer_function.get_name() << "," // initializer
                        <<           "(void (*)(void *, void *, void*)) reduction_reduce," // reducer atomic
                        <<           "(void (*)(void *)) reduction_flush," // flush
                        <<           "&" << cache_storage << ");"  // storage
                        <<      auxiliar_final
                        << "}"
                        << "else"
                        << "{"
                        <<     auxiliar_final2
                        << "}"
                        ;

                    if (IS_FORTRAN_LANGUAGE)
                    {
                        // We need to convert a void* type into a pointer to the reduction type.
                        // As a void* in FORTRAN is represented as an INTEGER(8), we cannot do this
                        // conversion directly in the FORTRAN source. For this reason we introduce
                        // a new function that will be defined in a C file.
                        TL::Symbol func = TL::Nanox::get_function_ptr_conversion(
                                // Destination
                                reduction_type.get_pointer_to(),
                                // Origin
                                TL::Type::get_void_type().get_pointer_to(),
                                construct.retrieve_context());


                        auxiliar_final2
                            << storage_name << " = " << func.get_name() << "(&" << (*it)->get_field_name() << ");"
                            ;

                        reductions_stuff
                            << storage_name << " = " << func.get_name() <<"(" << cache_storage << "->storage);"
                            ;

                        auxiliar_final
                            << storage_name << " = " << func.get_name() <<"(" << cache_storage << "->storage);"
                            ;
                    }
                    else
                    {
                        auxiliar_final2
                            << storage_name << " = &" << (*it)->get_field_name() << ";"
                            ;

                        auxiliar_final
                            << storage_name << " = "
                            <<   "(" << as_type(reduction_type.get_pointer_to()) << ")" << cache_storage << "->storage;"
                            ;

                        reductions_stuff
                            << storage_name << " = "
                            <<   "(" << as_type(reduction_type.get_pointer_to()) << ")" << cache_storage << "->storage;"
                            ;
                    }
                }

                reduction_symbols_map[reduction_item] = storage_name;
            }

            if (there_are_reductions_on_task)
            {
                // Generating the final code
                {
                    TL::Source extra_declarations;
                    extra_declarations << "nanos_err_t err;";

                    if (Nanos::Version::interface_is_at_least("reduction_on_task", 1000))
                        extra_declarations << as_type(TL::Type::get_bool_type()) << "is_registered;";

                    Nodecl::NodeclBase placeholder;
                    TL::Source new_statements_src;
                    new_statements_src
                        << "{"
                        <<      extra_declarations
                        <<      reductions_stuff_final
                        <<      statement_placeholder(placeholder)
                        << "}"
                        ;

                    if (IS_FORTRAN_LANGUAGE)
                        Source::source_language = SourceLanguage::C;

                    Nodecl::NodeclBase new_statements = new_statements_src.parse_statement(construct);

                    if (IS_FORTRAN_LANGUAGE)
                        Source::source_language = SourceLanguage::Current;

                    TL::Scope new_scope = ReferenceScope(placeholder).get_scope();
                    std::map<TL::Symbol, Nodecl::NodeclBase> reduction_symbol_to_nodecl_map;
                    for (std::map<TL::Symbol, std::string>::iterator it = reduction_symbols_map.begin();
                            it != reduction_symbols_map.end();
                            ++it)
                    {
                        TL::Symbol reduction_sym = it->first;
                        std::string storage_name = it->second;
                        TL::Symbol storage_sym = new_scope.get_symbol_from_name(storage_name);
                        ERROR_CONDITION(!storage_sym.is_valid(), "This symbol is not valid\n", 0);

                        Nodecl::NodeclBase deref_storage = Nodecl::Dereference::make(
                                storage_sym.make_nodecl(/* set_ref_type */ true, storage_sym.get_locus()),
                                storage_sym.get_type().points_to());

                        reduction_symbol_to_nodecl_map[reduction_sym] = deref_storage;
                    }

                    ReplaceReductionSymbols visitor(reduction_symbol_to_nodecl_map);
                    Nodecl::NodeclBase copied_statements = statements.shallow_copy();
                    visitor.walk(copied_statements);

                    placeholder.replace(copied_statements);

                    final_statements = new_statements;
                }

                // Generating the task code
                {
                    TL::Source new_statements_src;
                    Nodecl::NodeclBase placeholder;
                    new_statements_src
                        << "{"
                        <<      "nanos_err_t err;"
                        <<      reductions_stuff
                        <<      statement_placeholder(placeholder)
                        << "}"
                        ;

                    if (IS_FORTRAN_LANGUAGE)
                        Source::source_language = SourceLanguage::C;

                    Nodecl::NodeclBase new_statements = new_statements_src.parse_statement(construct);

                    if (IS_FORTRAN_LANGUAGE)
                        Source::source_language = SourceLanguage::Current;


                    TL::Scope new_scope = ReferenceScope(placeholder).get_scope();
                    std::map<TL::Symbol, Nodecl::NodeclBase> reduction_symbol_to_nodecl_map;
                    for (std::map<TL::Symbol, std::string>::iterator it = reduction_symbols_map.begin();
                            it != reduction_symbols_map.end();
                            ++it)
                    {
                        TL::Symbol reduction_sym = it->first;
                        std::string storage_name = it->second;
                        TL::Symbol storage_sym = new_scope.get_symbol_from_name(storage_name);
                        ERROR_CONDITION(!storage_sym.is_valid(), "This symbol is not valid\n", 0);

                        Nodecl::NodeclBase deref_storage = Nodecl::Dereference::make(
                                storage_sym.make_nodecl(/* set_ref_type */ true, storage_sym.get_locus()),
                                storage_sym.get_type().points_to());

                        reduction_symbol_to_nodecl_map[reduction_sym] = deref_storage;
                    }

                    ReplaceReductionSymbols visitor(reduction_symbol_to_nodecl_map);
                    Nodecl::NodeclBase copied_statements = statements.shallow_copy();
                    visitor.walk(copied_statements);

                    placeholder.replace(copied_statements);
                    statements.replace(new_statements);
                }
            }
        }
    }

    void LoweringVisitor::register_reductions(Nodecl::NodeclBase construct, OutlineInfo& outline_info, TL::Source& src)
    {
        if (!Nanos::Version::interface_is_at_least("task_reduction", 1000))
            return;

        TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();
        for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            if ((*it)->get_sharing() != OutlineDataItem::SHARING_CONCURRENT_REDUCTION)
                continue;

            std::pair<TL::OpenMP::Reduction*, TL::Type> red_info_pair= (*it)->get_reduction_info();
            TL::OpenMP::Reduction* reduction_info = red_info_pair.first;

            TL::Symbol reduction_item = (*it)->get_symbol();
            TL::Type reduction_type = reduction_item.get_type();

            TL::Symbol reduction_function, reduction_function_original_var;
            create_reduction_functions(reduction_info,
                    construct,
                    reduction_item,
                    reduction_function,
                    reduction_function_original_var,
                    _reduction_on_tasks_red_map);

            TL::Symbol initializer_function;
            create_initializer_function(reduction_info,
                    construct,
                    reduction_type,
                    initializer_function,
                    _reduction_on_tasks_ini_map);

            TL::Source size_to_be_allocated, target_address;
            if (IS_FORTRAN_LANGUAGE
                    && reduction_type.is_array())
            {
                size_t size_of_array_descriptor =
                    fortran_size_of_array_descriptor(
                            fortran_get_rank0_type(reduction_type.get_internal_type()),
                            fortran_get_rank_of_type(reduction_type.get_internal_type()));
                size_to_be_allocated << size_of_array_descriptor;

                if (reduction_item.is_allocatable())
                {
                    TL::Symbol ptr_of_sym = get_function_ptr_of(reduction_item, construct.retrieve_context());
                    target_address << ptr_of_sym.get_name() << "( " << (*it)->get_symbol().get_name() << ")";
                }
                else
                {
                    target_address << "(void *) &" << (*it)->get_field_name();
                }
            }
            else
            {
                size_to_be_allocated << "sizeof(" << as_type(reduction_type) << ")";
                target_address << "(void *) &" << (*it)->get_field_name();
            }

            src
                << "err = nanos_task_reduction_register("
                <<      target_address << "," // object address
                <<      "(void *) & " << (*it)->get_field_name() << ","
                <<      size_to_be_allocated << ","    // size
                <<      "__alignof__(" << as_type(reduction_type) << "),"
                <<      "(void (*)(void *, void *))& " << initializer_function.get_name() << ","         // initializer
                <<      "(void (*)(void *, void *)) &" << reduction_function.get_name() << "," // reducer
                <<      "(void (*)(void *, void *)) &" << reduction_function_original_var.get_name() << ");" // reducer ori
                ;
        }
    }
}}

