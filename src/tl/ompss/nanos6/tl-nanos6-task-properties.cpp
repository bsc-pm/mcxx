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

#include "fortran03-mangling.h"
#include "fortran03-typeutils.h"

#include <algorithm>

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
                        .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol));
            }

            virtual void visit(const Nodecl::OpenMP::Shared& n)
            {
                _task_properties.shared.insert(
                        n.get_symbols()
                        .as<Nodecl::List>()
                        .to_object_list()
                        .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol));
            }

            virtual void visit(const Nodecl::OpenMP::Private& n)
            {
                _task_properties.private_.insert(
                        n.get_symbols()
                        .as<Nodecl::List>()
                        .to_object_list()
                        .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol));
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

            virtual void visit(const Nodecl::OpenMP::FunctionTaskParsingContext& n)
            {
                _task_properties.locus_of_task_declaration = n.get_locus();
            }
    };

    TaskProperties TaskProperties::gather_task_properties(
            LoweringPhase* phase,
            const Nodecl::OpenMP::Task& node)
    {
        TaskProperties tp(phase);

        tp.locus_of_task_creation = node.get_locus();
        tp.locus_of_task_declaration = node.get_locus();

        TaskPropertiesVisitor tv(tp);
        tv.walk(node.get_environment());

        tp.compute_captured_values();
        tp.related_function = Nodecl::Utils::get_enclosing_function(node);
        tp.task_body = node.get_statements();

        return tp;
    }

    TaskProperties TaskProperties::gather_task_properties(
            LoweringPhase* phase,
            const Nodecl::OmpSs::TaskCall& node)
    {
        TaskProperties tp(phase);

        tp.locus_of_task_creation = node.get_locus();
        Nodecl::FunctionCall call = node.get_call().as<Nodecl::FunctionCall>();
        tp.locus_of_task_declaration = call.get_called().get_symbol().get_locus();

        TaskPropertiesVisitor tv(tp);
        tv.walk(node.get_environment());

        tp.compute_captured_values();
        tp.related_function = Nodecl::Utils::get_enclosing_function(node);
        tp.is_function_task = true;

        return tp;
    }

    bool TaskProperties::is_saved_expression(Nodecl::NodeclBase n)
    {
        return (n.is<Nodecl::Symbol>()
                && n.get_symbol().is_saved_expression());
    }

    void TaskProperties::handle_array_bound(Nodecl::NodeclBase n)
    {
        if (is_saved_expression(n))
        {
            captured_value.insert(n.get_symbol());
        }
    }

    void TaskProperties::walk_type_for_saved_expressions(TL::Type t)
    {
        if (t.is_array())
        {
            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            {
                handle_array_bound(t.array_get_size());
            }
            else if (IS_FORTRAN_LANGUAGE)
            {
                Nodecl::NodeclBase lower, upper;
                t.array_get_bounds(lower, upper);
                handle_array_bound(lower);
                handle_array_bound(upper);
            }
            else
            {
                internal_error("Code unreachable", 0);
            }

            walk_type_for_saved_expressions(t.array_element());
        }
        else if (t.is_lvalue_reference()
                || t.is_rvalue_reference())
        {
            walk_type_for_saved_expressions(t.no_ref());
        }
        else if (t.is_pointer())
        {
            walk_type_for_saved_expressions(t.points_to());
        }
        else
        {
            // Do nothing
        }
    }

    void TaskProperties::compute_captured_values()
    {
        for (TL::ObjectList<TL::Symbol>::iterator it = shared.begin();
                it != shared.end();
                it++)
        {
            if (it->get_type().depends_on_nonconstant_values())
                walk_type_for_saved_expressions(it->get_type());
        }
        for (TL::ObjectList<TL::Symbol>::iterator it = firstprivate.begin();
                it != firstprivate.end();
                it++)
        {
            if (it->get_type().depends_on_nonconstant_values())
                walk_type_for_saved_expressions(it->get_type());
        }

        captured_value.insert(firstprivate);
    }

    namespace {

        struct GetField
        {
            TL::ObjectList<TL::Symbol>& fields;

            GetField(TL::ObjectList<TL::Symbol>& fields_)
                : fields(fields_) { }

            Nodecl::NodeclBase operator()(const std::string& name)
            {
                TL::ObjectList<TL::Symbol> l;
                ERROR_CONDITION( ( l = fields.find<std::string>(&TL::Symbol::get_name, std::string(name))).empty(),
                        "Field '%s' not found", name.c_str());
                return l[0].make_nodecl(/* set_ref_type */ true);
            }
        };

    }

    void TaskProperties::create_task_info(
            /* out */
            TL::Symbol &task_info,
            TL::Symbol& task_invocation_info,
            Nodecl::NodeclBase& local_init)
    {
        create_outline_function();
        create_dependences_function();
        create_copies_function();

        TL::Symbol task_info_struct =
            TL::Scope::get_global_scope().get_symbol_from_name("nanos_task_info");

        ERROR_CONDITION(!task_info_struct.is_valid()
                || !(task_info_struct.is_typedef()
                    || task_info_struct.is_class()),
                "Invalid symbol", 0);

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
        symbol_entity_specs_set_is_static(task_info.get_internal_symbol(), 1);

        TL::ObjectList<TL::Symbol> fields = task_info_struct.get_type().get_nonstatic_data_members();
        GetField get_field(fields);

        Nodecl::NodeclBase field_run = get_field("run");
        Nodecl::NodeclBase init_run;

        if (IS_FORTRAN_LANGUAGE)
        {
            init_run = outline_function_mangled.make_nodecl(/* set_ref_type */ true);
        }
        else
        {
            init_run = outline_function.make_nodecl(/* set_ref_type */ true);
        }
        TL::Type run_type = TL::Type::get_void_type().get_function_returning(
                TL::ObjectList<TL::Type>(1, TL::Type::get_void_type().get_pointer_to()))
            .get_pointer_to();
        init_run = Nodecl::Conversion::make(
                init_run,
                run_type);
        init_run.set_text("C");

        Nodecl::NodeclBase field_register_depinfo = get_field("register_depinfo");
        Nodecl::NodeclBase init_register_depinfo;
        TL::Type dep_or_copies_fun_type = TL::Type::get_void_type().get_function_returning(
                TL::ObjectList<TL::Type>(2, TL::Type::get_void_type().get_pointer_to()))
            .get_pointer_to();
        if (dependences_function.is_valid())
        {
            if (IS_FORTRAN_LANGUAGE)
            {
                init_register_depinfo = dependences_function_mangled.make_nodecl(/* set_ref_type */ true);
            }
            else
            {
                init_register_depinfo = dependences_function.make_nodecl(/* set_ref_type */ true);
            }
            init_register_depinfo = Nodecl::Conversion::make(
                    init_register_depinfo,
                    dep_or_copies_fun_type);
            init_register_depinfo.set_text("C");
        }
        else
        {
            init_register_depinfo = const_value_to_nodecl(const_value_get_signed_int(0));
        }

        Nodecl::NodeclBase field_register_copies = get_field("register_copies");
        Nodecl::NodeclBase init_register_copies;
        if (copies_function.is_valid())
        {
            if (IS_FORTRAN_LANGUAGE)
            {
                init_register_copies = copies_function_mangled.make_nodecl(/* set_ref_type */ true);
            }
            else
            {
                init_register_copies = copies_function.make_nodecl(/* set_ref_type */ true);
            }
            init_register_copies = Nodecl::Conversion::make(
                    init_register_copies,
                    run_type);
            init_register_copies.set_text("C");
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

        const char* c = locus_to_str(locus_of_task_declaration);
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

        // Task invocation info
        TL::Symbol task_invocation_info_struct =
            TL::Scope::get_global_scope().get_symbol_from_name("nanos_task_invocation_info");
        ERROR_CONDITION(!task_invocation_info_struct.is_valid(), "Invalid symbol", 0);

        std::string task_invocation_info_name;
        {
            std::stringstream ss;
            TL::Counter &counter = TL::CounterManager::get_counter("nanos6-args");
            ss << "task_invocation_info_" << (int)counter;
            counter++;
            task_invocation_info_name = ss.str();
        }

        task_invocation_info = TL::Scope::get_global_scope().new_symbol(task_invocation_info_name);
        task_invocation_info.get_internal_symbol()->kind = SK_VARIABLE;
        symbol_entity_specs_set_is_user_declared(
                task_invocation_info.get_internal_symbol(), 1);
        task_invocation_info.set_type(task_invocation_info_struct.get_user_defined_type());
        symbol_entity_specs_set_is_static(task_invocation_info.get_internal_symbol(), 1);

        TL::ObjectList<TL::Symbol> task_invocation_fields = task_invocation_info_struct.get_type().get_nonstatic_data_members();
        GetField get_field_task_invocation_info(task_invocation_fields);

        Nodecl::NodeclBase field_invocation_source = get_field_task_invocation_info("invocation_source");

        c = locus_to_str(locus_of_task_creation);
        Nodecl::NodeclBase init_invocation_source =
            const_value_to_nodecl(const_value_make_string_null_ended(c, strlen(c)));

        Nodecl::NodeclBase task_invocation_init = 
            Nodecl::StructuredValue::make(
                    Nodecl::List::make(
                        Nodecl::FieldDesignator::make(
                            field_invocation_source,
                            init_invocation_source,
                            field_invocation_source.get_type())
                            ),
                        Nodecl::StructuredValueBracedImplicit::make(),
                        task_info.get_type());
        task_invocation_info.set_value(task_invocation_init);

        if (IS_C_LANGUAGE
                || IS_CXX_LANGUAGE)
        {
            if (IS_CXX_LANGUAGE)
            {
                Nodecl::Utils::prepend_to_enclosing_top_level_location(
                        task_body,
                        Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), task_info));
                Nodecl::Utils::prepend_to_enclosing_top_level_location(
                        task_body,
                        Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), task_invocation_info));
            }

            Nodecl::Utils::prepend_to_enclosing_top_level_location(
                    task_body,
                    Nodecl::ObjectInit::make(task_info));
            Nodecl::Utils::prepend_to_enclosing_top_level_location(
                    task_body,
                    Nodecl::ObjectInit::make(task_invocation_info));
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            phase->get_extra_c_code().append(Nodecl::ObjectInit::make(task_info));
            phase->get_extra_c_code().append(Nodecl::ObjectInit::make(task_invocation_info));

            // Create a detached symbol with the same name as the real one
            // We need to do that otherwise Fortran codegen attempts to initialize this symbol
            // (We may want to fix this somehow)
            symbol_entity_specs_set_is_static(task_info.get_internal_symbol(), 0);
            //
            scope_entry_t* task_info_shim = NEW0(scope_entry_t);
            task_info_shim->symbol_name = task_info.get_internal_symbol()->symbol_name;
            task_info_shim->kind = task_info.get_internal_symbol()->kind;
            task_info_shim->decl_context = task_info.get_internal_symbol()->decl_context;
            symbol_entity_specs_set_is_user_declared(task_info_shim, 1);
            // Fake the structure size
            const int size_of_ptr = TL::Type::get_void_type().get_pointer_to().get_size();
            ERROR_CONDITION(task_info.get_type().get_size() % size_of_ptr != 0,
                    "Struct size does not divide the size of a pointer", 0);
            int num_elements = task_info.get_type().get_size() / size_of_ptr;
            task_info_shim->type_information = TL::Type(fortran_choose_int_type_from_kind(size_of_ptr))
                .get_array_to(const_value_to_nodecl(const_value_get_signed_int(num_elements)),
                        TL::Scope::get_global_scope()).get_internal_type();

            task_info = task_info_shim;

            // Ditto for task_invocation_info
            symbol_entity_specs_set_is_static(task_invocation_info.get_internal_symbol(), 0);
            //
            scope_entry_t* task_invocation_info_shim = NEW0(scope_entry_t);
            task_invocation_info_shim->symbol_name = task_invocation_info.get_internal_symbol()->symbol_name;
            task_invocation_info_shim->kind = task_invocation_info.get_internal_symbol()->kind;
            task_invocation_info_shim->decl_context = task_invocation_info.get_internal_symbol()->decl_context;
            symbol_entity_specs_set_is_user_declared(task_invocation_info_shim, 1);
            ERROR_CONDITION(task_invocation_info.get_type().get_size() % size_of_ptr != 0,
                    "Struct size does not divide the size of a pointer", 0);
            num_elements = task_invocation_info.get_type().get_size() / size_of_ptr;
            task_invocation_info_shim->type_information = TL::Type(fortran_choose_int_type_from_kind(size_of_ptr))
                .get_array_to(const_value_to_nodecl(const_value_get_signed_int(num_elements)),
                        TL::Scope::get_global_scope()).get_internal_type();

            task_invocation_info = task_invocation_info_shim;
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
                field.get_internal_symbol()->decl_context,
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

        for (TL::ObjectList<TL::Symbol>::iterator it = captured_value.begin();
                it != captured_value.end();
                it++)
        {
            TL::Type type_of_field = it->get_type().no_ref();

            if (type_of_field.depends_on_nonconstant_values())
            {
                if (type_of_field.no_ref().is_array())
                {
                    error_printf_at(
                            locus_of_task_creation,
                            "capturing the value of the runtime-sized array '%s' is not supported\n",
                            it->get_qualified_name().c_str());
                }
                type_of_field = TL::Type::get_void_type().get_pointer_to();
            }
            else
            {
                type_of_field = type_of_field.get_unqualified_type();

            }

            add_field_to_class(
                    new_class_symbol,
                    class_scope,
                    *it,
                    type_of_field);
        }

        for (TL::ObjectList<TL::Symbol>::iterator it = shared.begin();
                it != shared.end();
                it++)
        {
            TL::Type type_of_field = it->get_type().no_ref();

            if (IS_FORTRAN_LANGUAGE)
            {
                type_of_field = TL::Type::get_void_type().get_pointer_to();
            }
            else
            {
                if (type_of_field.depends_on_nonconstant_values())
                {
                    type_of_field = TL::Type::get_void_type().get_pointer_to();
                }
                else if (type_of_field.is_array())
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

        nodecl_t nodecl_output = nodecl_null();
        finish_class_type(new_class_type,
                ::get_user_defined_type(new_class_symbol.get_internal_symbol()),
                sc.get_decl_context(),
                locus_of_task_creation,
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

        if (IS_CXX_LANGUAGE)
        {
            Nodecl::Utils::prepend_to_enclosing_top_level_location(
                    task_body,
                    Nodecl::CxxDef::make(
                        Nodecl::NodeclBase::null(),
                        new_class_symbol));
        }
    }

    namespace {
        struct GenerateParamsNames
        {
            int x;
            GenerateParamsNames() : x(0) { }
            std::string operator()()
            {
                std::stringstream ss;
                ss << "p_" << x;
                x++;
                return ss.str();
            }
        };

        struct SolveParamNames
        {
            TL::Scope sc;
            SolveParamNames(TL::Scope sc_)
                : sc(sc_) { }
            Nodecl::NodeclBase operator()(const std::string& str)
            {
                TL::Symbol sym = sc.get_symbol_from_name(str);
                ERROR_CONDITION(!sym.is_valid(), "Symbol '%s' not found", sym.get_name().c_str());

                return sym.make_nodecl(/* set_ref_type */ true);
            }
        };
    }

    TL::Type TaskProperties::rewrite_type_for_outline(TL::Type t, Nodecl::Utils::SymbolMap& symbol_map)
    {
        if (t.is_array())
        {
            TL::Type elem_type = rewrite_type_for_outline(t.array_element(), symbol_map);
            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            {
                Nodecl::NodeclBase new_size = t.array_get_size();
                if (is_saved_expression(new_size))
                {
                    new_size = symbol_map.map(new_size.get_symbol()).make_nodecl(/* set_ref_type */ true);
                }

                return elem_type.get_array_to(new_size, TL::Scope::get_global_scope());
            }
            else if (IS_FORTRAN_LANGUAGE)
            {
                Nodecl::NodeclBase new_lower, new_upper;
                t.array_get_bounds(new_lower, new_upper);

                if (is_saved_expression(new_lower))
                {
                    new_lower = symbol_map.map(new_lower.get_symbol()).make_nodecl(/* set_ref_type */ true);
                }
                if (is_saved_expression(new_upper))
                {
                    new_upper = symbol_map.map(new_upper.get_symbol()).make_nodecl(/* set_ref_type */ true);
                }

                return elem_type.get_array_to(new_lower, new_upper, TL::Scope::get_global_scope());
            }
            else
            {
                internal_error("Code unreachable", 0);
            }

        }
        else if (t.is_lvalue_reference())
        {
            return rewrite_type_for_outline(t.no_ref(), symbol_map).get_lvalue_reference_to();
        }
        else if (t.is_rvalue_reference())
        {
            return rewrite_type_for_outline(t.no_ref(), symbol_map).get_rvalue_reference_to();
        }
        else if (t.is_pointer())
        {
            return rewrite_type_for_outline(t.points_to(), symbol_map)
                .get_pointer_to()
                .get_as_qualified_as(t);
        }
        else
        {
            return t;
        }
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

        for (TL::ObjectList<TL::Symbol>::iterator it = captured_value.begin();
                it != captured_value.end();
                it++)
        {
            unpack_parameter_names.append(it->get_name());
            TL::Type p_type = it->get_type().no_ref();

            if (p_type.is_array())
            {
                p_type = p_type.array_element().get_pointer_to();
            }
            else
            {
                p_type = p_type.get_lvalue_reference_to();
            }

            unpack_parameter_types.append(p_type);
        }

        for (TL::ObjectList<TL::Symbol>::iterator it = shared.begin();
                it != shared.end();
                it++)
        {
            unpack_parameter_names.append(it->get_name());
            TL::Type p_type = it->get_type().no_ref();

            if (p_type.is_array())
            {
                p_type = p_type.array_element().get_pointer_to();
            }
            else
            {
                p_type = p_type.get_lvalue_reference_to();
            }
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

        // Prepare deep copy and remember those parameters that need fixup
        Nodecl::Utils::SimpleSymbolMap symbol_map;

        struct ParameterToSymbol
        {
            TL::Scope _sc;
            ParameterToSymbol(TL::Scope sc)
                : _sc(sc) { }

            TL::Symbol operator()(const std::string& str)
            {
                TL::Symbol param_sym = _sc.get_symbol_from_name(str);
                ERROR_CONDITION(!param_sym.is_valid()
                        || !param_sym.is_parameter(), "Invalid symbol", 0);
                return param_sym;
            }
        };

        ParameterToSymbol param_to_symbol(unpacked_inside_scope);

        TL::ObjectList<TL::Symbol> parameters_to_update_type;
        for (TL::ObjectList<TL::Symbol>::iterator it = captured_value.begin();
                it != captured_value.end();
                it++)
        {
            TL::Symbol param_sym = param_to_symbol(it->get_name());
            symbol_map.add_map(*it, param_sym);

            if (param_sym.get_type().depends_on_nonconstant_values())
                parameters_to_update_type.append(param_sym);
        }

        for (TL::ObjectList<TL::Symbol>::iterator it = shared.begin();
                it != shared.end();
                it++)
        {
            TL::Symbol param_sym = param_to_symbol(it->get_name());
            symbol_map.add_map(*it, param_sym);

            if (param_sym.get_type().depends_on_nonconstant_values())
                parameters_to_update_type.append(param_sym);
        }

        // Now fix the types of runtime sized types prior anything else
        for (TL::ObjectList<TL::Symbol>::iterator it = parameters_to_update_type.begin();
                it != parameters_to_update_type.end();
                it++)
        {
            it->set_type(rewrite_type_for_outline(it->get_type(), symbol_map));
        }
        if (!parameters_to_update_type.empty())
        {
            // Sync the function type if needed

            TL::ObjectList<TL::Type> updated_param_types =
                unpack_parameter_names
                .map<TL::Symbol>(param_to_symbol)
                .map<TL::Type>(&TL::Symbol::get_type);
            unpacked_function.set_type(
                    TL::Type::get_void_type().get_function_returning(updated_param_types)
                    );
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

        if (IS_CXX_LANGUAGE
                && !related_function.is_member())
        {
            Nodecl::Utils::prepend_to_enclosing_top_level_location(
                    task_body,
                    Nodecl::CxxDecl::make(
                        Nodecl::Context::make(
                            Nodecl::NodeclBase::null(),
                            related_function.get_scope()),
                        unpacked_function));
        }

        Nodecl::Utils::append_to_enclosing_top_level_location(task_body, unpacked_function_code);

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

        if (IS_FORTRAN_LANGUAGE)
        {
            // For Fortran we will generate a global variable in C, but this
            // requires us to use the proper mangling.
            const char* mangled_name = ::fortran_mangle_symbol(outline_function.get_internal_symbol());

            // Create a detached symbol that looks like it comes from the global scope
            outline_function_mangled = NEW0(scope_entry_t);
            outline_function_mangled.get_internal_symbol()->symbol_name = mangled_name;
            outline_function_mangled.get_internal_symbol()->decl_context
                = TL::Scope::get_global_scope().get_decl_context();
            outline_function_mangled.get_internal_symbol()->kind = SK_FUNCTION;
            // fake type
            outline_function_mangled.get_internal_symbol()->type_information
                = TL::Type::get_void_type().get_function_returning(TL::ObjectList<TL::Type>())
                .get_internal_type();
            symbol_entity_specs_set_is_user_declared(
                    outline_function_mangled.get_internal_symbol(),
                    1);
        }

        Nodecl::NodeclBase outline_function_code, outline_empty_stmt;
        SymbolUtils::build_empty_body_for_function(
                outline_function,
                outline_function_code,
                outline_empty_stmt);

        TL::Scope outline_inside_scope = outline_empty_stmt.retrieve_context();
        TL::Symbol arg = outline_inside_scope.get_symbol_from_name("arg");
        ERROR_CONDITION(!arg.is_valid(), "Invalid symbol", 0);
        ERROR_CONDITION(!arg.is_parameter(), "Invalid symbol", 0);

        // Prepare the call to the unpacked function
        if (!IS_FORTRAN_LANGUAGE)
        {
            Nodecl::List args;

            for (TL::ObjectList<TL::Symbol>::iterator it = captured_value.begin();
                    it != captured_value.end();
                    it++)
            {
                ERROR_CONDITION(field_map.find(*it) == field_map.end(), "Symbol is not mapped", 0);

                if (it->get_type().depends_on_nonconstant_values())
                {
                    if (it->get_type().is_array())
                    {
                        internal_error("Capture of array values not implemented yet", 0);
                    }
                    else
                    {
                        TL::Type param_type = rewrite_type_using_args(
                                arg,
                                it->get_type().no_ref()
                                );

                        Nodecl::NodeclBase cast;
                        args.append(
                                Nodecl::Dereference::make(
                                    cast = Nodecl::Conversion::make(
                                        Nodecl::Reference::make(
                                            Nodecl::ClassMemberAccess::make(
                                                arg.make_nodecl(/* set_ref_type */ true),
                                                field_map[*it].make_nodecl(),
                                                /* member_literal */ Nodecl::NodeclBase::null(),
                                                field_map[*it].get_type().get_lvalue_reference_to()),
                                            field_map[*it].get_type().get_pointer_to()),
                                        param_type.get_pointer_to()),
                                    param_type.get_lvalue_reference_to()
                                    )
                                );
                        cast.set_text("C");
                    }
                }
                else
                {
                    TL::Type expr_type = it->get_type().no_ref().get_lvalue_reference_to();
                    args.append(
                            Nodecl::ClassMemberAccess::make(
                                arg.make_nodecl(/* set_ref_type */ true),
                                field_map[*it].make_nodecl(),
                                /* member_literal */ Nodecl::NodeclBase::null(),
                                expr_type)
                            );
                }
            }

            for (TL::ObjectList<TL::Symbol>::iterator it = shared.begin();
                    it != shared.end();
                    it++)
            {
                ERROR_CONDITION(field_map.find(*it) == field_map.end(), "Symbol is not mapped", 0);

                if (it->get_type().depends_on_nonconstant_values())
                {
                    // FIXME: there is a bit of duplication here with the code
                    // that computes the type in the outline
                    if (it->get_type().no_ref().is_array())
                    {
                        TL::Type type_in_outline = it->get_type().no_ref();
                        type_in_outline = type_in_outline.array_element().get_pointer_to();
                        type_in_outline = rewrite_type_using_args(arg, type_in_outline);

                        TL::Type pointer_type = type_in_outline;

                        // The field is void*, cast it to the type of the
                        // pointer (coming from array-to-pointer) type of the
                        // outline
                        Nodecl::NodeclBase cast;
                        args.append(
                                cast = Nodecl::Conversion::make(
                                    Nodecl::ClassMemberAccess::make(
                                        arg.make_nodecl(/* set_ref_type */ true),
                                        field_map[*it].make_nodecl(),
                                        /* member_literal */ Nodecl::NodeclBase::null(),
                                        field_map[*it].get_type().get_lvalue_reference_to()),
                                    pointer_type)
                                );
                        cast.set_text("C");
                    }
                    else
                    {
                        TL::Type type_in_outline = it->get_type().no_ref();
                        type_in_outline = type_in_outline.get_lvalue_reference_to();
                        type_in_outline = rewrite_type_using_args(arg, type_in_outline);

                        TL::Type pointer_type = type_in_outline.no_ref().get_pointer_to();
                        TL::Type arg_type = type_in_outline;

                        // The field is void*, cast it to the type of the
                        // argument and then derreference
                        Nodecl::NodeclBase cast;
                        args.append(
                                Nodecl::Dereference::make(
                                    cast = Nodecl::Conversion::make(
                                        Nodecl::ClassMemberAccess::make(
                                            arg.make_nodecl(/* set_ref_type */ true),
                                            field_map[*it].make_nodecl(),
                                            /* member_literal */ Nodecl::NodeclBase::null(),
                                            field_map[*it].get_type().get_lvalue_reference_to()),
                                        pointer_type),
                                    arg_type
                                    )
                                );
                        cast.set_text("C");
                    }

                }
                else if (!it->get_type().no_ref().is_array())
                {
                    // in the struct we have T* and the function expects T&
                    // make a derrefence
                    args.append(
                            Nodecl::Dereference::make(
                                Nodecl::ClassMemberAccess::make(
                                    arg.make_nodecl(/* set_ref_type */ true),
                                    field_map[*it].make_nodecl(),
                                    /* member_literal */ Nodecl::NodeclBase::null(),
                                    field_map[*it].get_type().get_lvalue_reference_to()),
                                field_map[*it].get_type().points_to().get_lvalue_reference_to())
                            );
                }
                else
                {
                    // the variable was T a[N][M]
                    // in the struct we have T (*a)[M] and the function expects T (*a)[M]
                    // so only make a lvalue-to-rvalue conversion
                    args.append(
                            Nodecl::Conversion::make(
                                Nodecl::ClassMemberAccess::make(
                                    arg.make_nodecl(/* set_ref_type */ true),
                                field_map[*it].make_nodecl(),
                                /* member_literal */ Nodecl::NodeclBase::null(),
                                field_map[*it].get_type().get_lvalue_reference_to()),
                                field_map[*it].get_type())
                        );
                }
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
        }
        else
        {
            std::string forwarded_name;
            {
                TL::Counter &counter = TL::CounterManager::get_counter("nanos6-outline");
                std::stringstream ss;
                ss << "nanos6_fwd_" << (int)counter;
                counter++;
                forwarded_name = ss.str();
            }

            TL::ObjectList<std::string> forwarded_parameter_names;
            TL::ObjectList<TL::Type> forwarded_parameter_types;

            forwarded_parameter_names.append("ol");
            forwarded_parameter_types.append(TL::Type::get_void_type().get_pointer_to());

            Nodecl::List args;

            args.append(
                    Nodecl::Reference::make(
                        unpacked_function.make_nodecl(/* set_ref_type */ true),
                        unpacked_function.get_type().get_pointer_to()));

            for (TL::ObjectList<TL::Symbol>::iterator it = captured_value.begin();
                    it != captured_value.end();
                    it++)
            {
                ERROR_CONDITION(field_map.find(*it) == field_map.end(), "Symbol is not mapped", 0);
                TL::Symbol field = field_map[*it];

                forwarded_parameter_names.append(field.get_name());
                forwarded_parameter_types.append(field.get_type().get_lvalue_reference_to());

                args.append(
                            Nodecl::ClassMemberAccess::make(
                                // Nodecl::Dereference::make(
                                arg.make_nodecl(/* set_ref_type */ true),
                                //    arg.get_type().points_to().get_lvalue_reference_to()),
                                field_map[*it].make_nodecl(),
                                /* member_literal */ Nodecl::NodeclBase::null(),
                                field_map[*it].get_type().get_lvalue_reference_to()));
            }

            for (TL::ObjectList<TL::Symbol>::iterator it = shared.begin();
                    it != shared.end();
                    it++)
            {
                ERROR_CONDITION(field_map.find(*it) == field_map.end(), "Symbol is not mapped", 0);
                TL::Symbol field = field_map[*it];

                forwarded_parameter_names.append(field.get_name());
                forwarded_parameter_types.append(field.get_type());

                args.append(
                            Nodecl::ClassMemberAccess::make(
                                // Nodecl::Dereference::make(
                                arg.make_nodecl(/* set_ref_type */ true),
                                //    arg.get_type().points_to().get_lvalue_reference_to()),
                                field_map[*it].make_nodecl(),
                                /* member_literal */ Nodecl::NodeclBase::null(),
                                field_map[*it].get_type().get_lvalue_reference_to()));
            }

            TL::Symbol forwarded_function = SymbolUtils::new_function_symbol(
                    TL::Scope::get_global_scope(),
                    forwarded_name,
                    TL::Type::get_void_type(),
                    forwarded_parameter_names,
                    forwarded_parameter_types);
            // Make this symbol global
            symbol_entity_specs_set_is_static(forwarded_function.get_internal_symbol(), 0);

            Nodecl::NodeclBase call_to_forward =
                Nodecl::ExpressionStatement::make(
                        Nodecl::FunctionCall::make(
                            forwarded_function.make_nodecl(/* set_ref_type */ true),
                            args,
                            /* alternate_name */ Nodecl::NodeclBase::null(),
                            /* function_form */ Nodecl::NodeclBase::null(),
                            get_void_type()));

            outline_empty_stmt.prepend_sibling(call_to_forward);

            TL::ObjectList<std::string> c_forwarded_parameter_names(forwarded_parameter_names.size(), "");

            c_forwarded_parameter_names[0] = "ol";

            GenerateParamsNames generate_params_names;
            std::generate(c_forwarded_parameter_names.begin() + 1, c_forwarded_parameter_names.end(), generate_params_names);

            TL::ObjectList<TL::Type> c_forwarded_parameter_types (
                forwarded_parameter_types.size(), TL::Type::get_void_type().get_pointer_to()
                );
            // fix first
            c_forwarded_parameter_types[0] = TL::Type::get_void_type().get_function_returning(
                    TL::ObjectList<TL::Type>(forwarded_parameter_types.size() - 1, TL::Type::get_void_type().get_pointer_to())
                    );

            std::string c_forwarded_name = ::fortran_mangle_symbol(
                    forwarded_function.get_internal_symbol());

            // Now generate the C counterpart
            TL::Symbol c_forwarded_function = SymbolUtils::new_function_symbol(
                    TL::Scope::get_global_scope(),
                    c_forwarded_name,
                    TL::Type::get_void_type(),
                    c_forwarded_parameter_names,
                    c_forwarded_parameter_types);
            symbol_entity_specs_set_is_static(c_forwarded_function.get_internal_symbol(), 0);

            Nodecl::NodeclBase c_forwarded_function_code, c_forwarded_empty_stmt;
            SymbolUtils::build_empty_body_for_function(
                    c_forwarded_function,
                    c_forwarded_function_code,
                    c_forwarded_empty_stmt);

            SolveParamNames solve_param_names(c_forwarded_empty_stmt.retrieve_context());

            TL::ObjectList<Nodecl::NodeclBase> refs_to_params = c_forwarded_parameter_names.map<Nodecl::NodeclBase>(
                    solve_param_names
                    );

            Nodecl::NodeclBase c_ol_arg = refs_to_params[0];
            Nodecl::List c_args = Nodecl::List::make(
                    TL::ObjectList<Nodecl::NodeclBase>(refs_to_params.begin()+1, refs_to_params.end())
                    );

            // Note, since we are in Fortran, the empty statement must be wholly replaced by a CompoundStatement
            c_forwarded_empty_stmt.replace(
                    Nodecl::CompoundStatement::make(
                        Nodecl::List::make(
                            Nodecl::ExpressionStatement::make(
                                Nodecl::FunctionCall::make(
                                    c_ol_arg,
                                    c_args,
                                    /* alternate-symbol */ Nodecl::NodeclBase::null(),
                                    /* function-form */ Nodecl::NodeclBase::null(),
                                    TL::Type::get_void_type()))),
                        /* finalize */ Nodecl::NodeclBase::null()));

            phase->get_extra_c_code().append(c_forwarded_function_code);
        }

        if (IS_CXX_LANGUAGE
                && !related_function.is_member())
        {
            Nodecl::Utils::prepend_to_enclosing_top_level_location(task_body,
                    Nodecl::CxxDecl::make(
                        Nodecl::Context::make(
                            Nodecl::NodeclBase::null(),
                            related_function.get_scope()),
                        outline_function));
        }

        Nodecl::Utils::append_to_enclosing_top_level_location(task_body, outline_function_code);
    }

    void TaskProperties::register_linear_dependence(
            TL::DataReference& data_ref,
            TL::Symbol handler,
            TL::Symbol arg,
            TL::Symbol register_fun,
            Nodecl::List& register_statements)
    {
        Nodecl::NodeclBase base_addr = data_ref.get_base_address();
        base_addr = rewrite_expression_using_args(arg, base_addr);

        TL::Type data_type = data_ref.get_data_type();

        /// void nanos_register_XXX_dep(void *handler, void *start, size_t length);

        Nodecl::List arg_list;
        // handler
        arg_list.append(
                Nodecl::Conversion::make(
                    handler.make_nodecl(/* set_ref_type */ true),
                    handler.get_type()));
        // start
        arg_list.append(
                Nodecl::Conversion::make(
                    base_addr,
                    TL::Type::get_void_type().get_pointer_to()));

        // length
        if (data_type.depends_on_nonconstant_values())
        {
            arg_list.append(
                    rewrite_expression_using_args(arg,
                        data_ref.get_sizeof().shallow_copy()));
        }
        else
        {
            arg_list.append(
                    const_value_to_nodecl_with_basic_type(
                        const_value_get_integer(
                            data_type.get_size(),
                            type_get_size(get_size_t_type()),
                            /* sign */ 0),
                        get_size_t_type()));
        }

        Nodecl::NodeclBase function_call = Nodecl::ExpressionStatement::make(
                Nodecl::FunctionCall::make(
                    register_fun.make_nodecl(/* set_ref_type */ true),
                    arg_list,
                    /* alternate-symbol */ Nodecl::NodeclBase::null(),
                    /* function-form */ Nodecl::NodeclBase::null(),
                    TL::Type::get_void_type()));

        register_statements.append(function_call);
    }

    void TaskProperties::register_dependence_for_array(
            TL::DataReference& data_ref,
            TL::Symbol handler,
            TL::Symbol arg,
            TL::Symbol register_fun,
            Nodecl::List& register_statements)
    {
        Nodecl::NodeclBase base_addr = data_ref.get_base_address();
        base_addr = rewrite_expression_using_args(arg, base_addr);

        TL::Type data_type = data_ref.get_data_type();

        ERROR_CONDITION(!data_type.is_array(), "Invalid data type here", 0);
        if (data_type.array_is_region())
        {
            internal_error("Regions not implemented yet", 0);
        }
        else
        {
            register_linear_dependence(
                    data_ref,
                    handler,
                    arg,
                    register_fun,
                    register_statements);
        }
    }

    void TaskProperties::create_dependences_function()
    {
        TL::ObjectList<std::string> dep_parameter_names(2);
        dep_parameter_names[0] = "handler";
        dep_parameter_names[1] = "arg";
        TL::ObjectList<TL::Type> dep_parameter_types(2);
        dep_parameter_types[0] = TL::Type::get_void_type().get_pointer_to();
        dep_parameter_types[1] = info_structure.get_lvalue_reference_to();

        // Outline function
        std::string dep_name;
        {
            TL::Counter &counter = TL::CounterManager::get_counter("nanos6-outline");
            std::stringstream ss;
            ss << "nanos6_dep_" << (int)counter;
            counter++;
            dep_name = ss.str();
        }

        dependences_function
            = SymbolUtils::new_function_symbol(
                    related_function,
                    dep_name,
                    TL::Type::get_void_type(),
                    dep_parameter_names,
                    dep_parameter_types);

        if (IS_FORTRAN_LANGUAGE)
        {
            // For Fortran we will generate a global variable in C, but this
            // requires us to use the proper mangling.
            const char* mangled_name = ::fortran_mangle_symbol(dependences_function.get_internal_symbol());

            // Create a detached symbol that looks like it comes from the global scope
            dependences_function_mangled = NEW0(scope_entry_t);
            dependences_function_mangled.get_internal_symbol()->symbol_name = mangled_name;
            dependences_function_mangled.get_internal_symbol()->decl_context
                = TL::Scope::get_global_scope().get_decl_context();
            dependences_function_mangled.get_internal_symbol()->kind = SK_FUNCTION;
            // fake type
            dependences_function_mangled.get_internal_symbol()->type_information
                = TL::Type::get_void_type().get_function_returning(TL::ObjectList<TL::Type>())
                .get_internal_type();
            symbol_entity_specs_set_is_user_declared(
                    dependences_function_mangled.get_internal_symbol(),
                    1);
        }

        Nodecl::NodeclBase dependences_function_code, dependences_empty_stmt;
        SymbolUtils::build_empty_body_for_function(
                dependences_function,
                dependences_function_code,
                dependences_empty_stmt);

        TL::Scope dependences_inside_scope = dependences_empty_stmt.retrieve_context();
        TL::Symbol handler = dependences_inside_scope.get_symbol_from_name("handler");
        TL::Symbol arg = dependences_inside_scope.get_symbol_from_name("arg");
        ERROR_CONDITION(!arg.is_valid(), "Invalid symbol", 0);

        TL::Scope global_context = TL::Scope::get_global_scope();
        TL::Symbol register_dep_in = global_context.get_symbol_from_name("nanos_register_read_depinfo");
        ERROR_CONDITION(!register_dep_in.is_valid(), "Invalid symbol", 0);

        TL::Symbol register_dep_out = global_context.get_symbol_from_name("nanos_register_write_depinfo");
        ERROR_CONDITION(!register_dep_out.is_valid(), "Invalid symbol", 0);

        TL::Symbol register_dep_inout = global_context.get_symbol_from_name("nanos_register_readwrite_depinfo");
        ERROR_CONDITION(!register_dep_inout.is_valid(), "Invalid symbol", 0);

        struct DependencesSet
        {
            TL::ObjectList<Nodecl::NodeclBase> &dep_list;
            TL::Symbol register_fun;
        } deps[] = {
            { dep_in, register_dep_in },
            { dep_out, register_dep_out },
            { dep_inout, register_dep_inout },
        };


        for (DependencesSet *dep_set = deps;
                dep_set != (DependencesSet*)(&deps + 1);
                dep_set++)
        {
            TL::ObjectList<Nodecl::NodeclBase> &dep_list = dep_set->dep_list;

            for (TL::ObjectList<Nodecl::NodeclBase>::iterator
                    it = dep_list.begin();
                    it != dep_list.end();
                    it++)
            {
                TL::DataReference data_ref = *it;
                TL::Type data_type = data_ref.get_data_type();

                Nodecl::List register_statements;
                if (!data_type.is_array())
                {
                    register_linear_dependence(
                            data_ref,
                            handler,
                            arg,
                            dep_set->register_fun,
                            register_statements);
                }
                else
                {
                    register_dependence_for_array(
                            data_ref,
                            handler,
                            arg,
                            dep_set->register_fun,
                            register_statements);
                }

                dependences_empty_stmt.prepend_sibling(register_statements);
            }
        }

        if (IS_CXX_LANGUAGE
                && !related_function.is_member())
        {
            Nodecl::Utils::prepend_to_enclosing_top_level_location(task_body,
                    Nodecl::CxxDecl::make(
                        Nodecl::Context::make(
                            Nodecl::NodeclBase::null(),
                            related_function.get_scope()),
                        dependences_function));
        }

        Nodecl::Utils::append_to_enclosing_top_level_location(task_body, dependences_function_code);
    }

    Nodecl::NodeclBase TaskProperties::rewrite_expression_using_args(TL::Symbol arg, Nodecl::NodeclBase expr)
    {
        Nodecl::NodeclBase result = expr.shallow_copy();

        struct RewriteExpression : public Nodecl::ExhaustiveVisitor<void>
        {
            TL::Symbol arg;
            field_map_t &field_map;
            const TL::ObjectList<TL::Symbol>& shared;

            RewriteExpression(TL::Symbol arg_,
                    field_map_t& field_map_,
                    const TL::ObjectList<TL::Symbol> &shared_)
                : arg(arg_), field_map(field_map_), shared(shared_)
            {
            }

            virtual void visit(const Nodecl::Symbol& node)
            {
                TL::Symbol sym = node.get_symbol();
                ERROR_CONDITION(field_map.find(sym) == field_map.end(),
                        "Symbol '%s' not found in the field map!",
                        sym.get_name().c_str());

                TL::Symbol field = field_map[sym];

                Nodecl::NodeclBase new_expr =
                    Nodecl::ClassMemberAccess::make(
                            arg.make_nodecl(/* set_ref_type */ true, node.get_locus()),
                            field.make_nodecl(node.get_locus()),
                            /* form */ Nodecl::NodeclBase::null(),
                            field.get_type(),
                            node.get_locus());

                // FIXME: what about Fortran?
                // if (IS_C_LANGUAGE
                //         || IS_CXX_LANGUAGE)
                {
                    if (shared.contains(sym)
                            && !sym.get_type().no_ref().is_array())
                    {
                        new_expr = Nodecl::Dereference::make(
                                new_expr,
                                sym.get_type().no_ref().get_lvalue_reference_to(),
                                new_expr.get_locus());
                    }
                }
                // else if (IS_FORTRAN_LANGUAGE)
                // {
                //     // internal_error("Not yet implemented", 0);
                // }

                node.replace(new_expr);
            }
        };

        RewriteExpression r(arg, field_map, shared);
        r.walk(result);

        struct RemoveRedundantRefDerref : public Nodecl::ExhaustiveVisitor<void>
        {
            virtual void visit(const Nodecl::Reference& node)
            {
                if (node.get_rhs().is<Nodecl::Dereference>())
                {
                    node.replace(node.get_rhs().as<Nodecl::Dereference>().get_rhs());
                }
            }

            virtual void visit(const Nodecl::Dereference& node)
            {
                if (node.get_rhs().is<Nodecl::Reference>())
                {
                    node.replace(node.get_rhs().as<Nodecl::Reference>().get_rhs());
                }
            }
        };

        RemoveRedundantRefDerref c;
        c.walk(result);

        return result;
    }

    TL::Type TaskProperties::rewrite_type_using_args(TL::Symbol arg, TL::Type t)
    {
        if (t.is_array())
        {
            TL::Type elem_type = rewrite_type_using_args(arg, t.array_element());
            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            {
                Nodecl::NodeclBase new_size = t.array_get_size();
                if (is_saved_expression(new_size))
                {
                    new_size = rewrite_expression_using_args(arg, new_size);
                }

                return elem_type.get_array_to(new_size, TL::Scope::get_global_scope());
            }
            else if (IS_FORTRAN_LANGUAGE)
            {
                Nodecl::NodeclBase new_lower, new_upper;
                t.array_get_bounds(new_lower, new_upper);

                if (is_saved_expression(new_lower))
                {
                    new_lower = rewrite_expression_using_args(arg, new_lower);
                }
                if (is_saved_expression(new_upper))
                {
                    new_upper = rewrite_expression_using_args(arg, new_upper);
                }

                return elem_type.get_array_to(new_lower, new_upper, TL::Scope::get_global_scope());
            }
            else
            {
                internal_error("Code unreachable", 0);
            }

        }
        else if (t.is_lvalue_reference())
        {
            return rewrite_type_using_args(arg, t.no_ref()).get_lvalue_reference_to();
        }
        else if (t.is_rvalue_reference())
        {
            return rewrite_type_using_args(arg, t.no_ref()).get_rvalue_reference_to();
        }
        else if (t.is_pointer())
        {
            return rewrite_type_using_args(arg, t.points_to())
                .get_pointer_to()
                .get_as_qualified_as(t);
        }
        else
        {
            return t;
        }
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

        for (TL::ObjectList<TL::Symbol>::iterator it = captured_value.begin();
                it != captured_value.end();
                it++)
        {
            ERROR_CONDITION(field_map.find(*it) == field_map.end(),
                    "Symbol is not mapped", 0);

            if (!is_standard_layout_type(it->get_type().no_ref().get_internal_type()))
            {
                error_printf_at(locus_of_task_creation,
                        "capture of symbol '%s' with non-standard layout type is not supported\n",
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

                Nodecl::NodeclBase size_of_array;

                if (it->get_type().depends_on_nonconstant_values())
                {
                    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                    {
                        size_of_array =
                            Nodecl::Sizeof::make(
                                    Nodecl::Type::make(it->get_type()),
                                    Nodecl::NodeclBase::null(),
                                    get_size_t_type());

                    }
                    else if (IS_FORTRAN_LANGUAGE)
                    {
                        internal_error("Capture of VLA not yet implemented", 0);
                    }
                }
                else
                {
                    size_of_array =
                        const_value_to_nodecl_with_basic_type(
                                const_value_get_signed_int(
                                    it->get_type().no_ref().get_size()
                                    ),
                                get_size_t_type());
                }

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


        captured_env = captured_list;
    }

} }
