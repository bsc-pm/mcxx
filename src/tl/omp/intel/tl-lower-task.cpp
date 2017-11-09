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

#include "tl-counters.hpp"
#include "tl-lowering-visitor.hpp"
#include "tl-lowering-utils.hpp"
#include "tl-symbol-utils.hpp"

namespace TL { namespace Intel {

void LoweringVisitor::visit(const Nodecl::OpenMP::Task& construct)
{
    Nodecl::NodeclBase statements = construct.get_statements();
    Nodecl::List environment = construct.get_environment().as<Nodecl::List>();

    walk(statements);
    statements = construct.get_statements(); // Should not be necessary

    TL::ObjectList<Nodecl::OpenMP::Shared> shared_list = environment.find_all<Nodecl::OpenMP::Shared>();
    TL::ObjectList<Nodecl::OpenMP::Private> private_list = environment.find_all<Nodecl::OpenMP::Private>();
    TL::ObjectList<Nodecl::OpenMP::Firstprivate> firstprivate_list = environment.find_all<Nodecl::OpenMP::Firstprivate>();
    TL::ObjectList<Nodecl::OpenMP::Reduction> reduction_list = environment.find_all<Nodecl::OpenMP::Reduction>();

    TL::ObjectList<TL::Symbol> all_symbols_passed; // Set of all symbols passed in the outline
    TL::ObjectList<TL::Symbol> private_symbols;
    TL::ObjectList<TL::Symbol> firstprivate_symbols;
    TL::ObjectList<TL::Symbol> shared_symbols;

    if (!shared_list.empty())
    {
        TL::ObjectList<Symbol> tmp =
            shared_list  // TL::ObjectList<OpenMP::Shared>
            .map<Nodecl::NodeclBase>(&Nodecl::OpenMP::Shared::get_symbols) // TL::ObjectList<Nodecl::NodeclBase>
            .map<Nodecl::List>(&Nodecl::NodeclBase::as<Nodecl::List>) // TL::ObjectList<Nodecl::List>
            .map<TL::ObjectList<Nodecl::NodeclBase> >(&Nodecl::List::to_object_list) // TL::ObjectList<TL::ObjectList<Nodecl::NodeclBase> >
            .reduction(TL::append_two_lists<Nodecl::NodeclBase>) // TL::ObjectList<Nodecl::NodeclBase>
            .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol) // TL::ObjectList<TL::Symbol>
            ;

        shared_symbols.insert(tmp);
        all_symbols_passed.insert(tmp);
    }
    if (!private_list.empty())
    {
        TL::ObjectList<Symbol> tmp =
            private_list  // TL::ObjectList<OpenMP::Private>
            .map<Nodecl::NodeclBase>(&Nodecl::OpenMP::Private::get_symbols) // TL::ObjectList<Nodecl::NodeclBase>
            .map<Nodecl::List>(&Nodecl::NodeclBase::as<Nodecl::List>) // TL::ObjectList<Nodecl::List>
            .map<TL::ObjectList<Nodecl::NodeclBase> >(&Nodecl::List::to_object_list) // TL::ObjectList<TL::ObjectList<Nodecl::NodeclBase> >
            .reduction(TL::append_two_lists<Nodecl::NodeclBase>) // TL::ObjectList<Nodecl::NodeclBase>
            .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol) // TL::ObjectList<TL::Symbol>
            ;

        private_symbols.insert(tmp);
    }
    if (!firstprivate_list.empty())
    {
        TL::ObjectList<Symbol> tmp =
            firstprivate_list  // TL::ObjectList<OpenMP::Firstprivate>
            .map<Nodecl::NodeclBase>(&Nodecl::OpenMP::Firstprivate::get_symbols) // TL::ObjectList<Nodecl::NodeclBase>
            .map<Nodecl::List>(&Nodecl::NodeclBase::as<Nodecl::List>) // TL::ObjectList<Nodecl::List>
            .map<TL::ObjectList<Nodecl::NodeclBase> >(&Nodecl::List::to_object_list) // TL::ObjectList<TL::ObjectList<Nodecl::NodeclBase> >
            .reduction(TL::append_two_lists<Nodecl::NodeclBase>) // TL::ObjectList<Nodecl::NodeclBase>
            .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol) // TL::ObjectList<TL::Symbol>
            ;

        private_symbols.insert(tmp);
        firstprivate_symbols.insert(tmp);
        all_symbols_passed.insert(tmp);
    }

    // Add the VLA symbols
    {
        TL::ObjectList<TL::Symbol> vla_symbols;
        for (TL::ObjectList<TL::Symbol>::iterator it = all_symbols_passed.begin();
                it != all_symbols_passed.end();
                it++)
        {
            Intel::gather_vla_symbols(*it, vla_symbols);
        }

        // VLA symbols are always firstprivate
        private_symbols.insert(vla_symbols);
        firstprivate_symbols.insert(vla_symbols);

        // We want all the gathered VLA symbols be the first ones
        vla_symbols.insert(all_symbols_passed);
        all_symbols_passed = vla_symbols;
    }

    TL::ObjectList<Nodecl::OpenMP::ReductionItem> reduction_items;
    if (!reduction_list.empty())
    {
        reduction_items = reduction_list
            .map<Nodecl::NodeclBase>(&Nodecl::OpenMP::Reduction::get_reductions)
            .map<Nodecl::List>(&Nodecl::NodeclBase::as<Nodecl::List>)
            .map<TL::ObjectList<Nodecl::NodeclBase> >(&Nodecl::List::to_object_list)
            .reduction((&TL::append_two_lists<Nodecl::NodeclBase>))
            .map<Nodecl::OpenMP::ReductionItem>(&Nodecl::NodeclBase::as<Nodecl::OpenMP::ReductionItem>);
        TL::ObjectList<Symbol> reduction_symbols = reduction_items
            .map<Nodecl::NodeclBase>(&Nodecl::OpenMP::ReductionItem::get_reduced_symbol) // TL::ObjectList<Nodecl::NodeclBase>
            .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol); // TL::ObjectList<TL::Symbol>
        all_symbols_passed.insert(reduction_symbols);
    }

    // END VARS

    TL::Scope global_scope = CURRENT_COMPILED_FILE->global_decl_context;

    TL::Type kmp_task_type = global_scope
                            .get_symbol_from_name("kmp_task_t")
                            .get_user_defined_type();

    TL::Type kmp_routine_type = global_scope
                                .get_symbol_from_name("kmp_routine_entry_t")
                                .get_user_defined_type();

    TL::Symbol ident_symbol = Intel::new_global_ident_symbol(construct);

    // Crear task func
    TL::Type kmp_int32_type = global_scope
        .get_symbol_from_name("kmp_int32")
        .get_user_defined_type();
    ERROR_CONDITION(!kmp_int32_type.is_valid(), "Type kmp_int32 not in scope", 0);

    TL::Symbol enclosing_function = Nodecl::Utils::get_enclosing_function(construct);

    TL::Counter &task_num = TL::CounterManager::get_counter("intel-omp-task");
    std::stringstream outline_task_name;
    outline_task_name << "_task_" << enclosing_function.get_name() << "_" << (int)task_num;
    task_num++;

    TL::ObjectList<std::string> parameter_names;
    TL::ObjectList<TL::Type> parameter_types;

    parameter_names.append("_global_tid"); parameter_types.append(kmp_int32_type.get_pointer_to());
    parameter_names.append("_task"); parameter_types.append(kmp_task_type.get_pointer_to());

    TL::Symbol outline_task = SymbolUtils::new_function_symbol(
            enclosing_function,
            outline_task_name.str(),
            TL::Type::get_void_type(),
            parameter_names,
            parameter_types);

    Nodecl::NodeclBase outline_task_code, outline_task_stmt;
    SymbolUtils::build_empty_body_for_function(outline_task,
            outline_task_code,
            outline_task_stmt);

    // Crear task_args estructura
    std::stringstream task_args_struct_name;
    task_args_struct_name << "_args" << outline_task_name.str();

    Source src_task_args_struct;
    src_task_args_struct << "struct " << task_args_struct_name.str() << " {";
    for (TL::ObjectList<TL::Symbol>::const_iterator it = shared_symbols.begin();
            it != shared_symbols.end();
            it++)
    {
        src_task_args_struct
            << as_type(it->get_type().no_ref().get_pointer_to())
            << " "
            << it->get_name()
            << ";";
    }

    for (TL::ObjectList<TL::Symbol>::const_iterator it = firstprivate_symbols.begin();
            it != firstprivate_symbols.end();
            it++)
    {
        src_task_args_struct
            << as_type(it->get_type().no_ref())
            << " "
            << it->get_name()
            << ";";
    }

    src_task_args_struct << "};";
    Nodecl::NodeclBase tree_task_args_struct = src_task_args_struct.parse_declaration(global_scope);

    if (IS_CXX_LANGUAGE)
    {
        Nodecl::Utils::prepend_to_enclosing_top_level_location(construct, tree_task_args_struct);
    }

    std::string task_args_struct_lang_name = task_args_struct_name.str();
    if (IS_C_LANGUAGE)
        task_args_struct_lang_name  = "struct " + task_args_struct_lang_name;

    TL::Type task_args_type = global_scope
	    .get_symbol_from_name(task_args_struct_lang_name)
	    .get_user_defined_type();

    // Poner el codigo de crear task, estructuras...
    Source src_task_call_body;
    Nodecl::NodeclBase stmt_declarations, stmt_task_alloc, stmt_task_fill, stmt_task;
    src_task_call_body
    << "{"
    << statement_placeholder(stmt_declarations)
    << statement_placeholder(stmt_task_alloc)
    << statement_placeholder(stmt_task_fill)
    << statement_placeholder(stmt_task)
    << "}";
    Nodecl::NodeclBase tree_task_call_body = src_task_call_body.parse_statement(construct);

    Source src_declarations;
    src_declarations
    << as_type(kmp_task_type) << " *_ret;"
    << as_type(task_args_type) << " *_args;";
    Nodecl::NodeclBase tree_declarations = src_declarations.parse_statement(stmt_declarations);
    // Por que no puedo hacer replace en lugar de prepend_sibling?
    stmt_declarations.prepend_sibling(tree_declarations);

    Source src_task_alloc;
    src_task_alloc
    << "_ret = __kmpc_omp_task_alloc(&" << as_symbol(ident_symbol) << ","
                                 << "__kmpc_global_thread_num(&" << as_symbol(ident_symbol) << "),"
                                 << "1,"
                                 << "sizeof(" << as_type(kmp_task_type) << "),"
                                 << "sizeof(" << as_type(task_args_type) << "),"
                                 << "(" << as_type(kmp_routine_type) << ")&" << as_symbol(outline_task) << ");";
    Nodecl::NodeclBase tree_task_alloc = src_task_alloc.parse_statement(stmt_task_alloc);
    stmt_task_alloc.replace(tree_task_alloc);

    Source src_task_fill;
    src_task_fill
    << "_args = (" << as_type(task_args_type) << "*)" << "_ret->shareds;";
    TL::ObjectList<TL::Symbol> fields = task_args_type.get_fields();
    TL::ObjectList<TL::Symbol>::iterator it_fields = fields.begin();
    for (TL::ObjectList<TL::Symbol>::const_iterator it = shared_symbols.begin();
		    it != shared_symbols.end();
		    it++, it_fields++)
    {
	    src_task_fill
		    << "_args" << "->" << it_fields->get_name() << " = &" << it->get_name() << ";";
    }

//    it_fields = fields.begin();

    for (TL::ObjectList<TL::Symbol>::const_iterator it = firstprivate_symbols.begin();
		    it != firstprivate_symbols.end();
		    it++, it_fields++)
    {
        if (!it->get_type().no_ref().is_array()) {
	    src_task_fill
		    << "_args" << "->" << it_fields->get_name() << " = " << it->get_name() << ";";
        }
        else {
            src_task_fill
            << "__builtin_memcpy(_args->" << it_fields->get_name() << ","
            <<                        it->get_name()
            <<                        ", sizeof(" << as_symbol(*it_fields) << "));";
        }
    }
    Nodecl::NodeclBase tree_task_fill = src_task_fill.parse_statement(stmt_task_fill);
    stmt_task_fill.prepend_sibling(tree_task_fill);

    // Copiar codigo a la funcion substituyendo simbolos
    // TODO: Hay que rellenar symbol_map

    Nodecl::Utils::SimpleSymbolMap symbol_map;
    Source src_task_prev;
    TL::Source src_task_fp_array_copy;
    src_task_prev
    << as_type(task_args_type) << " *_args = (" << as_type(task_args_type) << "*)" << "_task->shareds;";

    it_fields = fields.begin();
    for (TL::ObjectList<TL::Symbol>::const_iterator it = shared_symbols.begin();
		    it != shared_symbols.end();
		    it++, it_fields++)
    {
        src_task_prev
        << as_type(it->get_type().no_ref().get_lvalue_reference_to())
        << " _task_"
        << it->get_name()
        << " = "
        << "*(*_args)."
        << it_fields->get_name()
        << ";";
    }

    for (TL::ObjectList<TL::Symbol>::const_iterator it = firstprivate_symbols.begin();
		    it != firstprivate_symbols.end();
		    it++, it_fields++)
    {
        src_task_prev
        << as_type(it->get_type().no_ref().get_lvalue_reference_to())
        << " _task_"
        << it->get_name()
        << " = "
        << "(*_args)."
        << it_fields->get_name()
        << ";";
    }

    for (TL::ObjectList<TL::Symbol>::const_iterator it = private_symbols.begin();
		    it != private_symbols.end();
		    it++)
    {
        if (!firstprivate_symbols.contains(*it)) {
            src_task_prev
            << as_type(it->get_type())
            << " _task_"
            << it->get_name()
            << ";";
        }
    }

    if (!src_task_prev.empty()) {
        Nodecl::NodeclBase tree_task_prev = src_task_prev.parse_statement(outline_task_stmt);
        for (TL::ObjectList<TL::Symbol>::const_iterator it = shared_symbols.begin();
                    it != shared_symbols.end();
                    it++)
        {
            // retrieve_context busca por los nodos de arriba el scope
            TL::Symbol task_sym = outline_task_stmt.retrieve_context().get_symbol_from_name("_task_" + it->get_name());
            ERROR_CONDITION(!task_sym.is_valid(), "Invalid symbol", 0);
            symbol_map.add_map(*it, task_sym);
        }
        for (TL::ObjectList<TL::Symbol>::const_iterator it = private_symbols.begin();
                    it != private_symbols.end();
                    it++)
        {
            // retrieve_context busca por los nodos de arriba el scope
            TL::Symbol task_sym = outline_task_stmt.retrieve_context().get_symbol_from_name("_task_" + it->get_name());
            ERROR_CONDITION(!task_sym.is_valid(), "Invalid symbol", 0);
            symbol_map.add_map(*it, task_sym);
       }
        outline_task_stmt.prepend_sibling(tree_task_prev);
    }

    Nodecl::NodeclBase task_func_body = Nodecl::Utils::deep_copy(statements,
            outline_task_stmt,
            symbol_map);
    outline_task_stmt.prepend_sibling(task_func_body);

    Nodecl::Utils::prepend_to_enclosing_top_level_location(construct, outline_task_code);

    Source src_task;
    src_task
    << "__kmpc_omp_task(&" << as_symbol(ident_symbol) << ","
    << "__kmpc_global_thread_num(&" << as_symbol(ident_symbol) << "),"
    << "(" << as_type(kmp_task_type) << " *)_ret);";
    Nodecl::NodeclBase tree_task = src_task.parse_statement(stmt_task);
    stmt_task.replace(tree_task);

    construct.replace(tree_task_call_body);
}

} }
