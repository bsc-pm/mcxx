/*--------------------------------------------------------------------
  (C) Copyright 2015-2015 Barcelona Supercomputing Center
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


#include "tl-nanos6-task-properties.hpp"
#include "tl-nodecl-visitor.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-symbol-utils.hpp"
#include "tl-counters.hpp"
#include "cxx-cexpr.h"
#include "cxx-diagnostic.h"


namespace TL { namespace Nanos6 {

    struct TaskPropertiesVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            TaskProperties &_task_properties;
        public:
            TaskPropertiesVisitor(TaskProperties& task_properties) 
                : _task_properties(task_properties) { } 

            virtual void visit(const Nodecl::OpenMP::Firstprivate& n)
            {
                _task_properties.firstprivate.insert(
                        n.get_symbols()
                        .as<Nodecl::List>()
                        .to_object_list()
                        .map(&Nodecl::NodeclBase::get_symbol));
            }

            virtual void visit(const Nodecl::OpenMP::Shared& n)
            {
                _task_properties.shared.insert(
                        n.get_symbols()
                        .as<Nodecl::List>()
                        .to_object_list()
                        .map(&Nodecl::NodeclBase::get_symbol));
            }

            virtual void visit(const Nodecl::OpenMP::Private& n)
            {
                _task_properties.private_.insert(
                        n.get_symbols()
                        .as<Nodecl::List>()
                        .to_object_list()
                        .map(&Nodecl::NodeclBase::get_symbol));
            }

            // virtual void visit(const Nodecl::OpenMP::TaskReduction& n)
            // {
            // }

            virtual void visit(const Nodecl::OpenMP::DepIn& n)
            {
                _task_properties.dep_in.append(n.get_in_deps().as<Nodecl::List>().to_object_list());
            }

            virtual void visit(const Nodecl::OpenMP::DepOut& n)
            {
                _task_properties.dep_out.append(n.get_out_deps().as<Nodecl::List>().to_object_list());
            }

            virtual void visit(const Nodecl::OpenMP::DepInout& n)
            {
                _task_properties.dep_inout.append(n.get_inout_deps().as<Nodecl::List>().to_object_list());
            }

            virtual void visit(const Nodecl::OpenMP::Final& n)
            {
                _task_properties.final_ = n.get_condition();
            }

            virtual void visit(const Nodecl::OpenMP::Untied& n)
            {
                _task_properties.is_tied = false;
            }

            virtual void visit(const Nodecl::OmpSs::TaskLabel& n)
            {
                _task_properties.task_label = n.get_text();
            }

            // virtual void visit(const Nodecl::OmpSs::Target& n)
            // {
            // }

            // virtual void visit(const Nodecl::OmpSs::CopyIn& n)
            // {
            // }

            // virtual void visit(const Nodecl::OmpSs::CopyOut& n)
            // {
            // }

            // virtual void visit(const Nodecl::OmpSs::CopyInout& n)
            // {
            // }

            virtual void unhandled_node(const Nodecl::NodeclBase &n)
            {
                error_printf("%s: error: unhandled node '%s'\n",
                        n.get_locus_str().c_str(),
                        ast_print_node_type(n.get_kind()));
            }

    };

    TaskProperties TaskProperties::gather_task_properties(
            LoweringPhase* phase,
            const Nodecl::OpenMP::Task& node)
    {
        TaskProperties tp(phase);

        TaskPropertiesVisitor tv(tp);
        tv.walk(node.get_environment());

        tp.related_function = Nodecl::Utils::get_enclosing_function(node);
        tp.locus_of_task = node.get_locus();
        tp.task_body = node.get_statements();

        return tp;
    }

    TaskProperties TaskProperties::gather_task_properties(
            LoweringPhase* phase,
            const Nodecl::OmpSs::TaskCall& node)
    {
        TaskProperties tp(phase);

        TaskPropertiesVisitor tv(tp);
        tv.walk(node.get_environment());

        tp.related_function = Nodecl::Utils::get_enclosing_function(node);
        tp.locus_of_task = node.get_locus();
        tp.is_function_task = true;

        return tp;
    }

    void TaskProperties::create_task_info(
            /* out */
            TL::Symbol &task_info,
            Nodecl::NodeclBase& local_init)
    {
        create_outline_function();
        create_dependences_function();
        create_copies_function();

        TL::Symbol task_info_struct =
            TL::Scope::get_global_scope().get_symbol_from_name("task_info");

        ERROR_CONDITION(!task_info_struct.is_valid()
                || !(task_info_struct.is_typedef()
                    || task_info_struct.is_class()),
                "Invalid symbol", 0);

        if (IS_FORTRAN_LANGUAGE)
        {
            // Force this symbol to be BIND(C)
            // FIXME: we want to be able to do this from the C FE
            TL::Symbol sym = task_info_struct.get_type().advance_over_typedefs().get_symbol();
            if (!sym.is_bind_c())
            {
                symbol_entity_specs_set_bind_info(
                        sym.get_internal_symbol(),
                        Nodecl::FortranBindC::make(
                            Nodecl::NodeclBase::null()
                            ).get_internal_nodecl());
            }
        }

        std::string task_info_name;
        {
            std::stringstream ss;
            TL::Counter &counter = TL::CounterManager::get_counter("nanos6-args");
            ss << "task_info_var_" << (int)counter;
            counter++;
            task_info_name = ss.str();
        }

        task_info = TL::Scope::get_global_scope().new_symbol(task_info_name);
        task_info.get_internal_symbol()->kind = SK_VARIABLE;
        symbol_entity_specs_set_is_user_declared(
                task_info.get_internal_symbol(), 1);
        task_info.set_type(task_info_struct.get_user_defined_type());

        TL::ObjectList<TL::Symbol> fields = task_info_struct.get_type().get_nonstatic_data_members();

        struct GetField
        {
            TL::ObjectList<TL::Symbol>& fields;

            GetField(TL::ObjectList<TL::Symbol>& fields_)
                : fields(fields_) { }

            Nodecl::NodeclBase operator()(const std::string& name)
            {
                TL::ObjectList<TL::Symbol> l;
                ERROR_CONDITION( ( l = fields.find(&TL::Symbol::get_name, std::string(name))).empty(),
                        "Field '%s' not found", name.c_str());
                return l[0].make_nodecl(/* set_ref_type */ true);
            }
        };

        GetField get_field(fields);

        Nodecl::NodeclBase field_run = get_field("run");
        Nodecl::NodeclBase init_run = 
            outline_function.make_nodecl(/* set_ref_type */ true);
        TL::Type run_type = TL::Type::get_void_type().get_function_returning(
                TL::ObjectList<TL::Type>(1, TL::Type::get_void_type().get_pointer_to().get_const_type()))
            .get_pointer_to();
        init_run = Nodecl::Cast::make(
                init_run,
                run_type,
                "C");

        Nodecl::NodeclBase field_register_depinfo = get_field("register_depinfo");
        Nodecl::NodeclBase init_register_depinfo;
        if (dependences_function.is_valid())
        {
            init_register_depinfo = dependences_function.make_nodecl(/* set_ref_type */ true);
            init_register_depinfo = Nodecl::Cast::make(
                    init_register_depinfo,
                    run_type,
                    "C");
        }
        else
        {
            init_register_depinfo = const_value_to_nodecl(const_value_get_signed_int(0));
        }

        Nodecl::NodeclBase field_register_copies = get_field("register_copies");
        Nodecl::NodeclBase init_register_copies;
        if (copies_function.is_valid())
        {
            init_register_copies = copies_function.make_nodecl(/* set_ref_type */ true);
            init_register_copies = Nodecl::Cast::make(
                    init_register_copies,
                    run_type,
                    "C");
        }
        else
        {
            init_register_copies = const_value_to_nodecl(const_value_get_signed_int(0));
        }

        Nodecl::NodeclBase field_task_label = get_field("task_label");
        Nodecl::NodeclBase init_task_label;
        if (!task_label.empty())
        {
            char* c = xstrdup(task_label.c_str());
            init_task_label = const_value_to_nodecl(
                    const_value_make_string_null_ended(c, strlen(c))
                    );
            DELETE(c);
        }
        else
        {
            init_task_label = const_value_to_nodecl(const_value_get_signed_int(0));
        }

        Nodecl::NodeclBase field_declaration_source = get_field("declaration_source");

        const char* c = locus_to_str(locus_of_task);
        Nodecl::NodeclBase init_declaration_source =
            const_value_to_nodecl(const_value_make_string_null_ended(c, strlen(c)));
        
        Nodecl::NodeclBase struct_init = 
            Nodecl::StructuredValue::make(
                    Nodecl::List::make(
                        Nodecl::FieldDesignator::make(
                            field_run,
                            init_run,
                            field_run.get_type()),
                        Nodecl::FieldDesignator::make(
                            field_register_depinfo,
                            init_register_depinfo,
                            field_register_depinfo.get_type()),
                        Nodecl::FieldDesignator::make(
                            field_register_copies,
                            init_register_copies,
                            field_register_copies.get_type()),
                        Nodecl::FieldDesignator::make(
                            field_task_label,
                            init_task_label,
                            field_task_label.get_type()),
                        Nodecl::FieldDesignator::make(
                            field_declaration_source,
                            init_declaration_source,
                            field_declaration_source.get_type())
                            ),
                        Nodecl::StructuredValueBracedImplicit::make(),
                        task_info.get_type());

        task_info.set_value(struct_init);

        if (IS_C_LANGUAGE
                || IS_CXX_LANGUAGE)
        {
            if (IS_CXX_LANGUAGE)
            {
                Nodecl::Utils::prepend_to_enclosing_top_level_location(
                        task_body,
                        Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), task_info));
            }

            Nodecl::Utils::prepend_to_enclosing_top_level_location(
                    task_body,
                    Nodecl::ObjectInit::make(task_info));
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            // FIXME - mangling of symbols
            phase->get_extra_c_code().append(Nodecl::ObjectInit::make(task_info));
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }

    TL::Scope TaskProperties::compute_scope_for_environment_structure()
    {
        TL::Scope sc = related_function.get_scope();
        // We are enclosed by a function because we are an internal subprogram
        if (IS_FORTRAN_LANGUAGE && related_function.is_nested_function())
        {
            // Get the enclosing function
            TL::Symbol enclosing_function = related_function.get_scope().get_related_symbol();

            // Update the scope
            sc = enclosing_function.get_scope();
        }

        if (related_function.is_member())
        {
            // Class scope
            sc = ::class_type_get_inner_context(related_function.get_class_type().get_internal_type());
        }
        else if (related_function.is_in_module())
        {
            // Scope of the module
            sc = related_function.in_module().get_related_scope();
        }

        return sc;
    }

    void TaskProperties::add_field_to_class(
            TL::Symbol new_class_symbol,
            TL::Scope class_scope,
            TL::Symbol var,
            TL::Type field_type)
    {
        TL::Type new_class_type = new_class_symbol.get_user_defined_type();

        std::string orig_field_name = var.get_name();
        TL::Type orig_field_type = var.get_type();

        TL::Symbol field = class_scope.new_symbol(orig_field_name);
        field.get_internal_symbol()->kind = SK_VARIABLE;
        symbol_entity_specs_set_is_user_declared(field.get_internal_symbol(), 1);

        field.set_type( field_type );
        field.get_internal_symbol()->locus = var.get_locus();

        symbol_entity_specs_set_is_member(field.get_internal_symbol(), 1);
        symbol_entity_specs_set_class_type(field.get_internal_symbol(),
                new_class_type.get_internal_type());
        symbol_entity_specs_set_access(field.get_internal_symbol(), AS_PUBLIC);
        class_type_add_member(
                new_class_type.get_internal_type(),
                field.get_internal_symbol(),
                /* is_definition */ 1);

        field_map[var] = field;
    }

    void TaskProperties::create_environment_structure(
            /* out */
            TL::Type& data_env_struct,
            Nodecl::NodeclBase& args_size)
    {
        field_map.clear();

        TL::Scope sc = compute_scope_for_environment_structure();

        TL::Counter &counter = TL::CounterManager::get_counter("nanos6-args");
        std::stringstream ss;
        ss << "nanos_task_args_" << (int)counter;
        counter++;

        std::string structure_name;
        if (IS_C_LANGUAGE
                || IS_FORTRAN_LANGUAGE)
        {
            // We need an extra 'struct '
            structure_name = "struct " + ss.str();
        }
        else
        {
            structure_name = ss.str();
        }

        // Create the class symbol
        TL::Symbol new_class_symbol = sc.new_symbol(structure_name);
        new_class_symbol.get_internal_symbol()->kind = SK_CLASS;
        type_t* new_class_type = get_new_class_type(sc.get_decl_context(), TT_STRUCT);

        symbol_entity_specs_set_is_user_declared(new_class_symbol.get_internal_symbol(), 1);

        const decl_context_t* class_context = new_class_context(new_class_symbol.get_scope().get_decl_context(),
                new_class_symbol.get_internal_symbol());

        TL::Scope class_scope(class_context);

        class_type_set_inner_context(new_class_type, class_context);

        new_class_symbol.get_internal_symbol()->type_information = new_class_type;

        for (TL::ObjectList<TL::Symbol>::iterator it = shared.begin();
                it != shared.end();
                it++)
        {
            TL::Type type_of_field = it->get_type().no_ref();

            if (type_of_field.is_variably_modified())
            {
                error_printf("%s: error: shared symbol '%s' of runtime-sized type is not supported\n",
                        locus_to_str(locus_of_task),
                        it->get_qualified_name().c_str());
                continue;
            }

            if (IS_FORTRAN_LANGUAGE)
            {
                type_of_field = TL::Type::get_void_type().get_pointer_to();
            }
            else
            {
                if (type_of_field.is_array())
                {
                    type_of_field = type_of_field.array_element().get_pointer_to();
                }
                else
                {
                    type_of_field = type_of_field.get_pointer_to();
                }
            }

            add_field_to_class(
                    new_class_symbol,
                    class_scope,
                    *it,
                    type_of_field);
        }

        for (TL::ObjectList<TL::Symbol>::iterator it = firstprivate.begin();
                it != firstprivate.end();
                it++)
        {
            TL::Type type_of_field = it->get_type().no_ref();

            if (type_of_field.is_variably_modified())
            {
                error_printf("%s: error: captured symbol '%s' of runtime-sized type is not supported\n",
                        locus_to_str(locus_of_task),
                        it->get_qualified_name().c_str());
                continue;
            }

            type_of_field = type_of_field.get_unqualified_type();

            add_field_to_class(
                    new_class_symbol,
                    class_scope,
                    *it,
                    type_of_field);
        }

        nodecl_t nodecl_output = nodecl_null();
        finish_class_type(new_class_type,
                ::get_user_defined_type(new_class_symbol.get_internal_symbol()),
                sc.get_decl_context(),
                locus_of_task,
                &nodecl_output);
        set_is_complete_type(new_class_type, /* is_complete */ 1);
        set_is_complete_type(get_actual_class_type(new_class_type), /* is_complete */ 1);

        info_structure
            = data_env_struct
            = new_class_symbol.get_user_defined_type();

        // FIXME - VLA
        args_size = const_value_to_nodecl_with_basic_type(
                const_value_get_integer(
                    info_structure.get_size(),
                    /* bytes */ type_get_size(get_size_t_type()),
                    /* sign */ 0),
                get_size_t_type());
    }

    void TaskProperties::create_outline_function()
    {
        // Unpacked function
        TL::ObjectList<std::string> unpack_parameter_names;
        TL::ObjectList<TL::Type> unpack_parameter_types;

        std::string unpacked_name;
        {
            TL::Counter &counter = TL::CounterManager::get_counter("nanos6-outline");
            std::stringstream ss;
            ss << "nanos6_unpack_" << (int)counter;
            counter++;
            unpacked_name = ss.str();
        }

        for (TL::ObjectList<TL::Symbol>::iterator it = shared.begin();
                it != shared.end();
                it++)
        {
            unpack_parameter_names.append(it->get_name());
            TL::Type p_type = it->get_type().no_ref();
            // FIXME - Shared VLAs
            p_type = p_type.get_lvalue_reference_to();
            unpack_parameter_types.append(p_type);
        }

        for (TL::ObjectList<TL::Symbol>::iterator it = firstprivate.begin();
                it != firstprivate.end();
                it++)
        {
            unpack_parameter_names.append(it->get_name());
            TL::Type p_type = it->get_type().no_ref();
            // FIXME - Captured VLAs
            p_type = p_type.get_lvalue_reference_to();
            unpack_parameter_types.append(p_type);
        }

        TL::Symbol unpacked_function
            = SymbolUtils::new_function_symbol(
                    related_function,
                    unpacked_name,
                    TL::Type::get_void_type(),
                    unpack_parameter_names,
                    unpack_parameter_types);

        Nodecl::NodeclBase unpacked_function_code, unpacked_empty_stmt;
        SymbolUtils::build_empty_body_for_function(
                unpacked_function,
                unpacked_function_code,
                unpacked_empty_stmt);

        TL::Scope unpacked_inside_scope = unpacked_empty_stmt.retrieve_context();

        // Prepare deep copy
        Nodecl::Utils::SimpleSymbolMap symbol_map;
        for (TL::ObjectList<TL::Symbol>::iterator it = shared.begin();
                it != shared.end();
                it++)
        {
            TL::Symbol param_sym = unpacked_inside_scope.get_symbol_from_name(it->get_name());
            ERROR_CONDITION(!param_sym.is_valid()
                    || !param_sym.is_parameter(), "Invalid symbol", 0);
            symbol_map.add_map(*it, param_sym);
        }

        for (TL::ObjectList<TL::Symbol>::iterator it = firstprivate.begin();
                it != firstprivate.end();
                it++)
        {
            TL::Symbol param_sym = unpacked_inside_scope.get_symbol_from_name(it->get_name());
            ERROR_CONDITION(!param_sym.is_valid()
                    || !param_sym.is_parameter(), "Invalid symbol", 0);
            symbol_map.add_map(*it, param_sym);
        }

        Nodecl::List extra_decls;

        for (TL::ObjectList<TL::Symbol>::iterator it = private_.begin();
                it != private_.end();
                it++)
        {
            TL::Symbol priv = unpacked_inside_scope.new_symbol(it->get_name());
            priv.get_internal_symbol()->kind = SK_VARIABLE;
            symbol_entity_specs_set_is_user_declared(priv.get_internal_symbol(), 1);
            priv.set_type(it->get_type());

            symbol_map.add_map(*it, priv);

            if (IS_CXX_LANGUAGE)
            {
                extra_decls.append(
                        Nodecl::CxxDef::make(
                            Nodecl::NodeclBase::null(),
                            priv,
                            it->get_locus()));
            }
        }

        if (!extra_decls.is_null())
            unpacked_empty_stmt.prepend_sibling(extra_decls);

        // Deep copy the body
        Nodecl::NodeclBase body = Nodecl::Utils::deep_copy(task_body, unpacked_inside_scope, symbol_map);
        unpacked_empty_stmt.prepend_sibling(body);

        Nodecl::Utils::prepend_to_enclosing_top_level_location(task_body, unpacked_function_code);

        // Outline function
        std::string ol_name;
        {
            TL::Counter &counter = TL::CounterManager::get_counter("nanos6-outline");
            std::stringstream ss;
            ss << "nanos6_ol_" << (int)counter;
            counter++;
            ol_name = ss.str();
        }

        TL::ObjectList<std::string> ol_parameter_names(1, "arg");
        TL::ObjectList<TL::Type> ol_parameter_types(1, info_structure.get_lvalue_reference_to());

        outline_function
            = SymbolUtils::new_function_symbol(
                    related_function,
                    ol_name,
                    TL::Type::get_void_type(),
                    ol_parameter_names,
                    ol_parameter_types);

        Nodecl::NodeclBase outline_function_code, outline_empty_stmt;
        SymbolUtils::build_empty_body_for_function(
                outline_function,
                outline_function_code,
                outline_empty_stmt);

        TL::Scope outline_inside_scope = outline_empty_stmt.retrieve_context();

        TL::Symbol arg = outline_inside_scope.get_symbol_from_name("arg");
        ERROR_CONDITION(!arg.is_valid(), "Invalid symbol", 0);
        ERROR_CONDITION(!arg.is_parameter(), "Invalid symbol", 0);

        Nodecl::List args;

        // Prepare the call to the unpacked function
        for (TL::ObjectList<TL::Symbol>::iterator it = shared.begin();
                it != shared.end();
                it++)
        {
            ERROR_CONDITION(field_map.find(*it) == field_map.end(), "Symbol is not mapped", 0);

            if (!it->get_type().no_ref().is_array())
            {
                args.append(
                        Nodecl::Dereference::make(
                            Nodecl::ClassMemberAccess::make(
                                // Nodecl::Dereference::make(
                                    arg.make_nodecl(/* set_ref_type */ true),
                                    // arg.get_type().points_to().get_lvalue_reference_to()),
                                field_map[*it].make_nodecl(),
                                /* member_literal */ Nodecl::NodeclBase::null(),
                                field_map[*it].get_type().get_lvalue_reference_to()),
                            field_map[*it].get_type().points_to().get_lvalue_reference_to()));
            }
            else
            {
                args.append(
                        Nodecl::ClassMemberAccess::make(
                            // Nodecl::Dereference::make(
                                arg.make_nodecl(/* set_ref_type */ true),
                            //    arg.get_type().points_to().get_lvalue_reference_to()),
                            field_map[*it].make_nodecl(),
                            /* member_literal */ Nodecl::NodeclBase::null(),
                            field_map[*it].get_type().get_lvalue_reference_to()));
            }
        }

        for (TL::ObjectList<TL::Symbol>::iterator it = firstprivate.begin();
                it != firstprivate.end();
                it++)
        {
            ERROR_CONDITION(field_map.find(*it) == field_map.end(), "Symbol is not mapped", 0);

            TL::Type expr_type = it->get_type().no_ref().get_lvalue_reference_to();
            args.append(
                    Nodecl::ClassMemberAccess::make(
                        // Nodecl::Dereference::make(
                            arg.make_nodecl(/* set_ref_type */ true),
                        //    arg.get_type().points_to().get_lvalue_reference_to()),
                        field_map[*it].make_nodecl(),
                        /* member_literal */ Nodecl::NodeclBase::null(),
                        expr_type)
                    );
        }

        Nodecl::NodeclBase call_to_unpacked =
            Nodecl::ExpressionStatement::make(
                    Nodecl::FunctionCall::make(
                        unpacked_function.make_nodecl(/* set_ref_type */ true),
                        args,
                        /* alternate_name */ Nodecl::NodeclBase::null(),
                        /* function_form */ Nodecl::NodeclBase::null(),
                        get_void_type()));

        outline_empty_stmt.prepend_sibling(call_to_unpacked);

        Nodecl::Utils::prepend_to_enclosing_top_level_location(task_body, outline_function_code);
    }

    void TaskProperties::create_dependences_function()
    {
#if 0
        TL::ObjectList<std::string> ol_parameter_names(1, "arg");
        TL::ObjectList<TL::Type> ol_parameter_types(1, info_structure);

        // Outline function
        std::string ol_name;
        {
            TL::Counter &counter = TL::CounterManager::get_counter("nanos6-outline");
            std::stringstream ss;
            ss << "nanos6_dep_" << (int)counter;
            counter++;
            ol_name = ss.str();
        }

        dependences_function
            = SymbolUtils::new_function_symbol(
                    related_function,
                    dep_name,
                    TL::Type::get_void_type(),
                    parameter_names,
                    parameter_types);

        Nodecl::NodeclBase outline_function_code, outline_empty_stmt;
        SymbolUtils::build_empty_body_for_function(
                outline_function,
                outline_function_code,
                outline_empty_stmt);

        Nodecl::Utils::prepend_to_enclosing_top_level_location(task_body, outline_function_code);
#endif
    }

    void TaskProperties::create_copies_function()
    {
#if 0
        TL::ObjectList<std::string> ol_parameter_names(1, "arg");
        TL::ObjectList<TL::Type> ol_parameter_types(1, info_structure);

        // Outline function
        std::string ol_name;
        {
            TL::Counter &counter = TL::CounterManager::get_counter("nanos6-outline");
            std::stringstream ss;
            ss << "nanos6_dep_" << (int)counter;
            counter++;
            ol_name = ss.str();
        }

        dependences_function
            = SymbolUtils::new_function_symbol(
                    related_function,
                    dep_name,
                    TL::Type::get_void_type(),
                    parameter_names,
                    parameter_types);

        Nodecl::NodeclBase outline_function_code, outline_empty_stmt;
        SymbolUtils::build_empty_body_for_function(
                outline_function,
                outline_function_code,
                outline_empty_stmt);

        Nodecl::Utils::prepend_to_enclosing_top_level_location(task_body, outline_function_code);
#endif
    }

    void TaskProperties::capture_environment(
            TL::Symbol args,
            /* out */
            Nodecl::NodeclBase& captured_env)
    {
        Nodecl::List captured_list;

        for (TL::ObjectList<TL::Symbol>::iterator it = shared.begin();
                it != shared.end();
                it++)
        {
            ERROR_CONDITION(field_map.find(*it) == field_map.end(),
                    "Symbol is not mapped", 0);

            Nodecl::NodeclBase rhs = it->make_nodecl(/* set_ref_type */ true);
            if (!it->get_type().no_ref().is_array())
            {
                rhs = Nodecl::Reference::make(
                        rhs,
                        rhs.get_type().no_ref().get_pointer_to());
            }
            else
            {
                rhs = Nodecl::Conversion::make(
                        rhs,
                        rhs.get_type().no_ref().array_element().get_pointer_to());
            }

            TL::Type lhs_type =
                field_map[*it].get_type().no_ref().get_lvalue_reference_to();

            Nodecl::NodeclBase lhs =
                Nodecl::ClassMemberAccess::make(
                        Nodecl::Dereference::make(
                            args.make_nodecl(/* set_ref_type */ true),
                            args.get_type().points_to().get_lvalue_reference_to()),
                        field_map[*it].make_nodecl(),
                        /* member_literal */ Nodecl::NodeclBase::null(),
                        lhs_type);

            captured_list.append(
                    Nodecl::ExpressionStatement::make(
                        Nodecl::Assignment::make(
                            lhs,
                            rhs,
                            lhs_type))
                    );
        }

        for (TL::ObjectList<TL::Symbol>::iterator it = firstprivate.begin();
                it != firstprivate.end();
                it++)
        {
            ERROR_CONDITION(field_map.find(*it) == field_map.end(),
                    "Symbol is not mapped", 0);

            if (!is_standard_layout_type(it->get_type().no_ref().get_internal_type()))
            {
                error_printf("%s: error: capture of symbol '%s' with non-standard layout type is not supported\n",
                        locus_to_str(locus_of_task),
                        it->get_qualified_name().c_str());
            }
            else if (!it->get_type().no_ref().is_array())
            {
                Nodecl::NodeclBase rhs = it->make_nodecl(/* set_ref_type */ true);

                TL::Type lhs_type =
                    field_map[*it].get_type().no_ref().get_lvalue_reference_to();

                Nodecl::NodeclBase lhs =
                    Nodecl::ClassMemberAccess::make(
                            Nodecl::Dereference::make(
                                args.make_nodecl(/* set_ref_type */ true),
                                args.get_type().points_to().get_lvalue_reference_to()),
                            field_map[*it].make_nodecl(),
                            /* member_literal */ Nodecl::NodeclBase::null(),
                            lhs_type);

                captured_list.append(
                        Nodecl::ExpressionStatement::make(
                            Nodecl::Assignment::make(
                                lhs,
                                rhs,
                                lhs_type))
                        );
            }
            else
            {
                TL::Symbol builtin_memcpy = TL::Scope::get_global_scope().get_symbol_from_name("__builtin_memcpy");
                ERROR_CONDITION(!builtin_memcpy.is_valid()
                        || !builtin_memcpy.is_function(), "Invalid symbol", 0);

                TL::Type lhs_type =
                    field_map[*it].get_type().no_ref().get_lvalue_reference_to();

                Nodecl::NodeclBase lhs =
                    Nodecl::Reference::make(
                            Nodecl::ClassMemberAccess::make(
                                Nodecl::Dereference::make(
                                    args.make_nodecl(/* set_ref_type */ true),
                                    args.get_type().points_to().get_lvalue_reference_to()),
                                field_map[*it].make_nodecl(),
                                /* member_literal */ Nodecl::NodeclBase::null(),
                                lhs_type),
                            lhs_type.no_ref().get_pointer_to());

                Nodecl::NodeclBase rhs = it->make_nodecl(/* set_ref_type */ true);
                rhs = Nodecl::Conversion::make(
                        rhs,
                        rhs.get_type().no_ref().array_element().get_pointer_to());

                Nodecl::NodeclBase size_of_array =
                    const_value_to_nodecl_with_basic_type(
                            const_value_get_signed_int(
                                it->get_type().no_ref().get_size()
                                ),
                            get_size_t_type());

                captured_list.append(
                        Nodecl::ExpressionStatement::make(
                            Nodecl::FunctionCall::make(
                                builtin_memcpy.make_nodecl(/* set_ref_type */ true),
                                Nodecl::List::make(lhs, rhs, size_of_array),
                                /* alternate-name */ Nodecl::NodeclBase::null(),
                                /* function-form */ Nodecl::NodeclBase::null(),
                                TL::Type::get_void_type().get_pointer_to())));

            }
        }

        captured_env = captured_list;
    }

} }
