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

#include "cxx-cexpr.h"

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

    namespace
    {
        // Convenient implementation of the 'get_symbol_from_name' function
        // that emits an error if the symbol does not exist
        TL::Symbol get_symbol_from_name(TL::Scope sc, const std::string &name)
        {
            TL::Symbol result = sc.get_symbol_from_name(name);
            ERROR_CONDITION(result.is_invalid(), "symbol '%s' not found\n", name.c_str());
            return result;
        }
    }

    static TL::Symbol create_reduction_function_internal(
            OpenMP::Reduction* red,
            TL::Type reduction_type,
            Nodecl::NodeclBase construct,
            std::string function_name,
            TL::Type omp_out_type,
            TL::Type omp_in_type,
            bool omp_out_type_is_allocatable)
    {
        Nodecl::NodeclBase combiner_stmt_placeholder;
        Source src;
        src << "void " << function_name << "("
            << as_type(omp_out_type.no_ref().get_lvalue_reference_to()) << " omp_out,"
            << as_type(omp_in_type.no_ref().get_lvalue_reference_to()) << " omp_in)"
            << "{"
            <<    statement_placeholder(combiner_stmt_placeholder)
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

        TL::Symbol function_sym = get_symbol_from_name(TL::Scope::get_global_scope(), function_name);
        TL::Scope inner_scope = combiner_stmt_placeholder.retrieve_context();
        function_sym.set_related_scope(inner_scope);
        if (IS_FORTRAN_LANGUAGE)
        {
            Nodecl::Utils::Fortran::append_used_modules(
                    construct.retrieve_context(),
                    function_sym.get_related_scope());
        }

        TL::Symbol param_omp_in  = get_symbol_from_name(inner_scope, "omp_in");
        TL::Symbol param_omp_out = get_symbol_from_name(inner_scope, "omp_out");

        symbol_entity_specs_set_is_allocatable(param_omp_out.get_internal_symbol(), omp_out_type_is_allocatable);

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

        Nodecl::List list_stmts;
        list_stmts.append(Nodecl::ExpressionStatement::make(expanded_combiner));

        if (IS_FORTRAN_LANGUAGE &&
                reduction_type.is_array())
        {
                list_stmts.append(Nodecl::FortranDeallocateStatement::make(Nodecl::List::make(param_omp_in.make_nodecl()), nodecl_null()));
        }

        combiner_stmt_placeholder.replace(list_stmts);

        // As the reduction function is needed during the instantiation of
        // the task, this function should be inserted before the construct
        Nodecl::Utils::prepend_to_enclosing_top_level_location(construct, function_code);

        return function_sym;
    }

    static std::pair<TL::Symbol, TL::Symbol> create_reduction_functions_internal(
            OpenMP::Reduction* red,
            TL::Type reduction_type,
            TL::Symbol reduction_item,
            Nodecl::NodeclBase construct)
    {
       std::stringstream red_fun;
       red_fun << "nanos_red_" << red << "_" << reduction_type.get_internal_type() << "_" << simple_hash_str(construct.get_filename().c_str());

       std::stringstream red_fun_orig_var;
       red_fun_orig_var << "nanos_red_" << red << "_" << reduction_type.get_internal_type() << "_" << simple_hash_str(construct.get_filename().c_str()) << "_orig_var";

       TL::Symbol reduction_function, reduction_function_original_var;
       if (IS_FORTRAN_LANGUAGE
             && reduction_type.is_array())
       {
          reduction_function = create_reduction_function_internal(
                red,
                reduction_type,
                construct,
                red_fun.str(),
                /* omp_out_type */ reduction_type.get_pointer_to(),
                /* omp_in_type */ reduction_type.get_pointer_to(),
                /* omp_out_is_allocatable */ false);

          reduction_function_original_var = create_reduction_function_internal(
                red,
                reduction_type,
                construct,
                red_fun_orig_var.str(),
                /* omp_out_type */ reduction_type,
                /* omp_in_type */ reduction_type.get_pointer_to(),
                symbol_entity_specs_get_is_allocatable(reduction_item.get_internal_symbol()));
       }
       else
       {
          reduction_function = create_reduction_function_internal(
                red,
                reduction_type,
                construct,
                red_fun.str(),
                /* omp_out_type */ reduction_type,
                /* omp_in_type */ reduction_type,
                /* omp_out_is_allocatable */ false);

          reduction_function_original_var = reduction_function;
       }
       return std::make_pair(reduction_function, reduction_function_original_var);
    }

    static void create_reduction_functions(OpenMP::Reduction* red,
            Nodecl::NodeclBase construct,
            TL::Type reduction_type,
            TL::Symbol reduction_item,
            TL::Symbol& reduction_function,
            TL::Symbol& reduction_function_original_var)
    {
        //std::cerr << "DEBUG: <creating reduction function> " << reduction_item.get_type().print_declarator() << std::endl;
        std::pair<TL::Symbol, TL::Symbol> red_funcs =
            create_reduction_functions_internal(red, reduction_type, reduction_item, construct);

        reduction_function = red_funcs.first;
        reduction_function_original_var = red_funcs.second;
    }

    static TL::Symbol create_initializer_function_c(
            OpenMP::Reduction* red,
            TL::Type reduction_type,
            Nodecl::NodeclBase construct)
    {
        std::string fun_name;
        {
            std::stringstream ss;
            ss << "nanos_ini_" << red << "_" << reduction_type.get_internal_type() << "_" << simple_hash_str(construct.get_filename().c_str());
            fun_name = ss.str();
        }

        Nodecl::NodeclBase combiner_stmt_placeholder;
        Source src;
        src << "void " << fun_name << "("
            <<      as_type(reduction_type.no_ref().get_lvalue_reference_to()) << " omp_priv,"
            <<      as_type(reduction_type.no_ref().get_lvalue_reference_to()) << " omp_orig)"
            << "{"
            <<    statement_placeholder(combiner_stmt_placeholder)
            << "}"
            ;

        Nodecl::NodeclBase function_code = src.parse_global(construct.retrieve_context().get_global_scope());


        TL::Symbol function_sym = get_symbol_from_name(TL::Scope::get_global_scope(), fun_name);
        TL::Scope inner_scope = combiner_stmt_placeholder.retrieve_context();
        function_sym.set_related_scope(inner_scope);

        TL::Symbol param_omp_priv = get_symbol_from_name(inner_scope, "omp_priv");
        TL::Symbol param_omp_orig = get_symbol_from_name(inner_scope, "omp_orig");

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

        Nodecl::NodeclBase new_initializer = Nodecl::Utils::deep_copy(initializer, inner_scope, translation_map);

        if (red->get_is_initialization())
        {
            // The original initializer was something like 'omp_priv = expr1', but the
            // new_initializer only represents the lhs expression (in our example, expr1).
            // For this reason we create manually an assignment expression.
            Nodecl::NodeclBase param_omp_priv_ref = Nodecl::Symbol::make(param_omp_priv);
            param_omp_priv_ref.set_type(param_omp_priv.get_type());

            combiner_stmt_placeholder.replace(
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
            combiner_stmt_placeholder.replace(
                    Nodecl::List::make(Nodecl::ExpressionStatement::make(new_initializer)));
        }

        // As the initializer function is needed during the instantiation of
        // the task, this function should be inserted before the construct
        Nodecl::Utils::prepend_to_enclosing_top_level_location(construct,
                function_code);

        return function_sym;
    }

    static TL::Symbol create_initializer_function_fortran(
            OpenMP::Reduction* red,
            TL::Type reduction_type,
            Nodecl::NodeclBase construct)
    {
        std::string fun_name;
        {
            std::stringstream ss;
            ss << "nanos_ini_" << red << "_" << reduction_type.get_internal_type() << "_" << simple_hash_str(construct.get_filename().c_str());
            fun_name = ss.str();
        }

        Nodecl::NodeclBase initializer = red->get_initializer().shallow_copy();


        TL::Type omp_out_type = reduction_type,
                 omp_ori_type = reduction_type;

        // These sources are only used in array reductions
        TL::Source omp_out_extra_attributes,
            extra_stuff_array_red;

        if (reduction_type.is_array())
        {
            Source dims_descr;
            TL::Type t = reduction_type;
            int rank = 0;
            if (t.is_fortran_array())
            {
                rank = t.fortran_rank();
            }

            dims_descr << "(";
            omp_out_extra_attributes << ", POINTER, DIMENSION(";

            int i;
            for (i = 0; i < rank; i++)
            {
                if (i != 0)
                {
                    dims_descr << ",";
                    omp_out_extra_attributes << ",";
                }

                dims_descr << "LBOUND(omp_orig, DIM = " << (rank - i) << ")"
                    << ":"
                    << "UBOUND(omp_orig, DIM = " << (rank - i) << ")"
                    ;

                omp_out_extra_attributes << ":";
                t = t.array_element();
            }

            dims_descr << ")";
            omp_out_extra_attributes << ")";

            omp_out_type = t;

            extra_stuff_array_red << "ALLOCATE(omp_priv" << dims_descr <<")\n";
        }

        Nodecl::NodeclBase initializer_stmt;
        Source src;
        src << "SUBROUTINE " << fun_name << "(omp_priv, omp_orig)\n"
            <<    "IMPLICIT NONE\n"
            <<    as_type(omp_out_type) << omp_out_extra_attributes << " ::  omp_priv\n"
            <<    as_type(omp_ori_type) <<  " :: omp_orig\n"
            <<    extra_stuff_array_red
            <<    statement_placeholder(initializer_stmt)
            << "END SUBROUTINE " << fun_name << "\n"
            ;

        Nodecl::NodeclBase function_code = src.parse_global(TL::Scope::get_global_scope());

        TL::Scope inner_scope = initializer_stmt.retrieve_context();

        TL::Symbol function_sym = get_symbol_from_name(TL::Scope::get_global_scope(), fun_name);
        function_sym.set_related_scope(inner_scope);

        Nodecl::Utils::Fortran::append_used_modules(
                construct.retrieve_context(),
                inner_scope);

        TL::Symbol omp_priv = get_symbol_from_name(inner_scope, "omp_priv");
        TL::Symbol omp_orig = get_symbol_from_name(inner_scope, "omp_orig");

        Nodecl::Utils::SimpleSymbolMap map;
        map.add_map(red->get_omp_priv(), omp_priv);
        map.add_map(red->get_omp_orig(), omp_orig);

        Nodecl::NodeclBase init_expr =
            Nodecl::Utils::deep_copy(red->get_initializer(), inner_scope, map);

        if (!init_expr.is<Nodecl::FunctionCall>())
        {
            Nodecl::NodeclBase rhs = omp_priv.make_nodecl(/*set_ref_type*/ true);
            if (omp_priv.get_type().no_ref().is_pointer())
            {
                rhs = Nodecl::Dereference::make(rhs, rhs.get_type().no_ref().points_to());
            }

            init_expr = Nodecl::Assignment::make( rhs, init_expr, rhs.get_type());
        }
        initializer_stmt.replace(Nodecl::ExpressionStatement::make(init_expr));

        // As the initializer function is needed during the instantiation of
        // the task, this function should be inserted before the construct
        Nodecl::Utils::prepend_to_enclosing_top_level_location(construct, function_code);


        ERROR_CONDITION(!function_sym.is_valid(), "Symbol %s not found", fun_name.c_str());
        return function_sym;
    }

    static void create_initializer_function(OpenMP::Reduction* red,
            Nodecl::NodeclBase construct,
            TL::Type reduction_type,
            TL::Symbol& initializer_function)
    {
        //std::cerr << "DEBUG: <creating initialize function> " << reduction_type.print_declarator() << std::endl;
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            initializer_function = create_initializer_function_c(red, reduction_type, construct);
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            initializer_function = create_initializer_function_fortran(red, reduction_type, construct);
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

    Nodecl::NodeclBase handle_task_statements(
          Nodecl::NodeclBase construct,
          Nodecl::NodeclBase task_statements,
          Nodecl::NodeclBase& task_placeholder, // Do not remove the reference
          TL::Source &new_stmts_src,            // It should be a const reference
          const std::map<TL::Symbol, std::string> &reduction_symbols_map)
    {
       if (IS_FORTRAN_LANGUAGE)
          Source::source_language = SourceLanguage::C;

       Nodecl::NodeclBase new_statements = new_stmts_src.parse_statement(construct);

       if (IS_FORTRAN_LANGUAGE)
          Source::source_language = SourceLanguage::Current;

       TL::Scope new_scope = ReferenceScope(task_placeholder).get_scope();
       std::map<TL::Symbol, Nodecl::NodeclBase> reduction_symbol_to_nodecl_map;
       for (std::map<TL::Symbol, std::string>::const_iterator it = reduction_symbols_map.begin();
             it != reduction_symbols_map.end();
             ++it)
       {
          TL::Symbol reduction_sym = it->first;
          std::string storage_name = it->second;
          TL::Symbol storage_sym = get_symbol_from_name(new_scope, storage_name);

          Nodecl::NodeclBase deref_storage = Nodecl::Dereference::make(
                storage_sym.make_nodecl(/* set_ref_type */ true, storage_sym.get_locus()),
                storage_sym.get_type().points_to());

          reduction_symbol_to_nodecl_map[reduction_sym] = deref_storage;
       }

       ReplaceReductionSymbols visitor(reduction_symbol_to_nodecl_map);
       Nodecl::NodeclBase copied_statements = task_statements.shallow_copy();
       visitor.walk(copied_statements);
       task_placeholder.replace(copied_statements);

       return new_statements;
    }


    bool LoweringVisitor::handle_reductions_on_task(
            Nodecl::NodeclBase construct,
            OutlineInfo& outline_info,
            Nodecl::NodeclBase statements,
            bool generate_final_stmts,
            Nodecl::NodeclBase& final_statements)
    {
        int num_reductions = 0;

        TL::Source
            reductions_stuff,
            final_clause_stuff,
            extra_array_red_memcpy;

        std::map<std::string, Nodecl::NodeclBase> is_registered_stmt_placeholders_map;
        std::map<TL::Symbol, std::string> reduction_symbols_map;

        TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();
        for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
           if (!(*it)->is_reduction())
              continue;

            std::pair<TL::OpenMP::Reduction*, TL::Type> red_info_pair = (*it)->get_reduction_info();
            TL::OpenMP::Reduction* reduction_info = red_info_pair.first;
            TL::Type reduction_type = red_info_pair.second.no_ref();

            TL::Symbol reduction_item = (*it)->get_symbol();
            TL::Type reduction_item_type = reduction_item.get_type().no_ref();

            TL::Counter &counter = TL::CounterManager::get_counter((*it)->get_field_name());
            std::stringstream ss;
            ss << "_storage_" << (int)counter;
            counter++;

            std::string storage_var_name = (*it)->get_field_name() + ss.str();
            TL::Type storage_var_type = reduction_type.get_pointer_to();


            TL::Symbol reduction_function, reduction_function_original_var, initializer_function;

            // Checking if the current reduction type has been treated before
            // Note that if that happens we can reuse the combiner and
            // initializer function.
            //
            // C/C++: note that if the type of the list item is an array type,
            // we regiter the reduction over its element type
            TL::Type registered_reduction_type = reduction_type;
            while (!IS_FORTRAN_LANGUAGE
                    && registered_reduction_type.is_array())
            {
                registered_reduction_type = registered_reduction_type.array_element();
            }

            LoweringVisitor::reduction_task_map_t::iterator task_red_info =
               _task_reductions_map.find(std::make_pair(reduction_info, registered_reduction_type));

            if (task_red_info != _task_reductions_map.end())
            {
              reduction_function = task_red_info->second._reducer;
              reduction_function_original_var = task_red_info->second._reducer_orig_var;
              initializer_function = task_red_info->second._initializer;
            }
            else
            {
               create_reduction_functions(reduction_info,
                     construct,
                     registered_reduction_type,
                     reduction_item,
                     reduction_function,
                     reduction_function_original_var);

               create_initializer_function(reduction_info,
                     construct,
                     registered_reduction_type,
                     initializer_function);

               _task_reductions_map.insert(
                       std::make_pair(
                           std::make_pair(reduction_info, registered_reduction_type),
                           TaskReductionsInfo(reduction_function, reduction_function_original_var, initializer_function)
                           ));
            }

            // Mandatory TL::Sources to be filled by any reduction
            TL::Source
                orig_address, // address of the original reduction variable
                storage_var; // variable which holds the address of the storage

            // Specific TL::Sources to be filled only by Fortran array reduction
            TL::Source extra_array_red_decl;

            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            {
                storage_var << storage_var_name;
                orig_address << (reduction_item_type.is_pointer() ? "" : "&") << (*it)->get_field_name();

                final_clause_stuff
                    << "if (" << storage_var_name << "_registered)"
                    << "{"
                    <<     storage_var_name  << " = "
                    <<        "(" << as_type(storage_var_type) << ")" << orig_address << ";"
                    << "}"
                    ;
            }
            else
            {
               orig_address <<  "&" << (*it)->get_field_name();
                if (reduction_item_type.is_array())
                {
                    size_t size_of_array_descriptor =
                        fortran_size_of_array_descriptor(
                                fortran_get_rank0_type(reduction_item_type.get_internal_type()),
                                fortran_get_rank_of_type(reduction_item_type.get_internal_type()));


                    storage_var << storage_var_name << "_indirect";
                    extra_array_red_decl << "void *" << storage_var << ";";

                    extra_array_red_memcpy
                        << "nanos_err = nanos_memcpy("
                        <<      "(void **) &" << storage_var_name << ","
                        <<      storage_var << ","
                        <<      size_of_array_descriptor << ");"
                            ;

                    final_clause_stuff
                        << "if (" << storage_var_name << "_registered)"
                        << "{"
                        <<     "nanos_err = nanos_memcpy("
                        <<         "(void **) &" << storage_var_name << ","
                        <<         "(void *) "<< orig_address << ","
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
                            reduction_item_type.get_pointer_to(),
                            // Origin
                            TL::Type::get_void_type().get_pointer_to(),
                            construct.retrieve_context());

                    storage_var << storage_var_name;

                    final_clause_stuff
                        << "if (" << storage_var_name << "_registered)"
                        << "{"
                        <<     storage_var_name << " = " << func.get_name() << "(" <<  orig_address << ");"
                        << "}"
                        ;
                }
            }


            reductions_stuff
                << extra_array_red_decl
                << as_type(storage_var_type) << " " << storage_var_name << ";"
                << "nanos_err = nanos_task_reduction_get_thread_storage("
                <<         "(void *)" << orig_address  << ","
                <<         "(void **) &" << storage_var << ");"

                << as_type(TL::Type::get_bool_type()) << " " << storage_var_name << "_registered;"
                << statement_placeholder(is_registered_stmt_placeholders_map[storage_var_name]);
                ;

            reduction_symbols_map[reduction_item] = storage_var_name;
            num_reductions++;
        }

        if (num_reductions != 0)
        {
            // Generating the final code if needed
            if (generate_final_stmts)
            {
                Nodecl::NodeclBase placeholder;
                TL::Source new_statements_src;
                new_statements_src
                    << "{"
                    <<      "nanos_err_t nanos_err;"
                    <<      reductions_stuff
                    <<      final_clause_stuff
                    <<      statement_placeholder(placeholder)
                    << "}"
                    ;

                final_statements = handle_task_statements(
                      construct, statements, placeholder, new_statements_src, reduction_symbols_map);

                for (std::map<std::string, Nodecl::NodeclBase>::const_iterator it = is_registered_stmt_placeholders_map.begin();
                        it != is_registered_stmt_placeholders_map.end();
                        it++)
                {
                    const std::string & storage_var_name = it->first;
                    Nodecl::NodeclBase is_registered_stmt_placeholder = it->second;

                    TL::Scope inner_scope = is_registered_stmt_placeholder.retrieve_context();

                    TL::Symbol storage_var_sym = get_symbol_from_name(inner_scope, storage_var_name);
                    ERROR_CONDITION(storage_var_sym.is_invalid(), "Invalid symbol", 0);

                    Nodecl::NodeclBase expr_stmt;
                    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                    {
                        TL::Symbol is_registered_symbol = get_symbol_from_name(inner_scope, storage_var_name + "_registered");

                        expr_stmt = Nodecl::ExpressionStatement::make(
                                Nodecl::Assignment::make(
                                    is_registered_symbol.make_nodecl(/*set_ref_type*/ true),
                                    Nodecl::Equal::make(
                                        storage_var_sym.make_nodecl(/*set_ref_type*/ true),
                                        const_value_to_nodecl(const_value_get_signed_int(0)),
                                        TL::Type::get_bool_type()),
                                    is_registered_symbol.get_type().get_lvalue_reference_to()));
                    }
                    else // IS_FORTRAN_LANGUAGE
                    {
                        TL::Source src;
                        src << storage_var_name << "_registered = ASSOCIATED(" << as_symbol(storage_var_sym) << ")";
                        expr_stmt = src.parse_statement(inner_scope);
                    }

                    is_registered_stmt_placeholder.replace(expr_stmt);
                }
            }

            // Generating the task code
            {
                TL::Source new_statements_src;
                Nodecl::NodeclBase placeholder;
                new_statements_src
                    << "{"
                    <<      "nanos_err_t nanos_err;"
                    <<      reductions_stuff
                    <<      extra_array_red_memcpy
                    <<      statement_placeholder(placeholder)
                    << "}"
                    ;

                Nodecl::NodeclBase new_statements = handle_task_statements(
                      construct, statements, placeholder, new_statements_src, reduction_symbols_map);
                statements.replace(new_statements);
            }
        }

        ERROR_CONDITION(num_reductions != 0 &&
                !Nanos::Version::interface_is_at_least("task_reduction", 1001),
                "The version of the runtime begin used does not support task reductions", 0);

        return (num_reductions != 0);
    }

    void LoweringVisitor::register_reductions(
          Nodecl::NodeclBase construct, OutlineInfo& outline_info, TL::Source& src)
    {
        TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();
        for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
           if (!(*it)->is_reduction())
              continue;

            std::pair<TL::OpenMP::Reduction*, TL::Type> red_info_pair = (*it)->get_reduction_info();
            TL::OpenMP::Reduction* reduction_info = red_info_pair.first;
            TL::Type reduction_type = red_info_pair.second.no_ref();

            ERROR_CONDITION(!Nanos::Version::interface_is_at_least("task_reduction", 1001),
                  "The version of the runtime being used does not support task reductions", 0);

            TL::Symbol reduction_item = (*it)->get_symbol();

            ERROR_CONDITION(reduction_type.is_array()
                  && (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                  && !Nanos::Version::interface_is_at_least("task_reduction", 1002),
                  "The version of the runtime being used does not support array reductions in C/C++", 0);

            // Note that at this point all the reduction must be registered.
            // For C/C++ array reductions, the registered_reduction type is the
            // element type
            TL::Type registered_reduction_type = reduction_type;
            while (!IS_FORTRAN_LANGUAGE
                    && registered_reduction_type.is_array())
            {
                registered_reduction_type = registered_reduction_type.array_element();
            }

            LoweringVisitor::reduction_task_map_t::iterator task_red_info =
               _task_reductions_map.find(std::make_pair(reduction_info, registered_reduction_type));

            ERROR_CONDITION(task_red_info == _task_reductions_map.end(),
                  "Unregistered task reduction\n", 0);

            TL::Symbol reduction_function = task_red_info->second._reducer;
            TL::Symbol reduction_function_original_var = task_red_info->second._reducer_orig_var;
            TL::Symbol initializer_function = task_red_info->second._initializer;

            // Common case: the runtime will host the private copies of the list item
            if (!(IS_FORTRAN_LANGUAGE && reduction_type.is_array()))
            {
               // Array Reductions in C/C++ are defined over the elements of the array
               TL::Source reduction_size_src_opt;
               TL::Type element_type = registered_reduction_type;

               reduction_size_src_opt << "sizeof(" << as_type(reduction_type) <<"),";

               TL::Source item_address =
                  (reduction_item.get_type().is_pointer() ? "" : "&") + (*it)->get_field_name();

               src
                  << "nanos_err = nanos_task_reduction_register("
                  <<      "(void *) " << item_address << ","          // object address
                  <<      reduction_size_src_opt                      // whole reduction size
                  <<      "sizeof(" << as_type(element_type) << "),"  // element size
                  <<      "(void (*)(void *, void *)) &"
                  <<          initializer_function.get_name() << ","  // initializer
                  <<      "(void (*)(void *, void *)) &"
                  <<          reduction_function.get_name() << ");"   // reducer
                  ;
            }
            else
            {
               // Specific case for Fortran Array Reductions: the runtime will
               // host a private array descriptor for each thread. Later, in
               // the initializer function, this array descriptors will be
               // initialized and their array storage will be allocated
               TL::Source target_address;
               size_t size_array_descriptor =
                  fortran_size_of_array_descriptor(
                        fortran_get_rank0_type(reduction_type.get_internal_type()),
                        fortran_get_rank_of_type(reduction_type.get_internal_type()));

               if (reduction_type.array_requires_descriptor())
               {
                  TL::Symbol ptr_of_sym = get_function_ptr_of(reduction_item, construct.retrieve_context());
                  target_address << ptr_of_sym.get_name() << "( " << (*it)->get_symbol().get_name() << ")";
               }
               else
               {
                  target_address << "(void *) &" << (*it)->get_field_name();
               }

               src
                  << "nanos_err = nanos_task_fortran_array_reduction_register("
                  <<      target_address << ","                                  // Address to the array descriptor
                  <<      "(void *) & " << (*it)->get_field_name() << ","        // Address to the storage
                  <<      size_array_descriptor << ","                           // size
                  <<      "(void (*)(void *, void *)) &"
                  <<          initializer_function.get_name() << ","             // initializer
                  <<      "(void (*)(void *, void *)) &"
                  <<          reduction_function.get_name() << ","               // reducer
                  <<      "(void (*)(void *, void *)) &"
                  <<          reduction_function_original_var.get_name() << ");" // reducer ori
                  ;
            }
        }
    }
}}

