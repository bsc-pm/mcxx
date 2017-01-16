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
#include "tl-nanos6-support.hpp"
#include "tl-nanos6-fortran-support.hpp"
#include "tl-nodecl-visitor.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-nodecl-utils-fortran.hpp"
#include "tl-symbol-utils.hpp"
#include "tl-counters.hpp"
#include "cxx-cexpr.h"
#include "cxx-diagnostic.h"

#include "fortran03-mangling.h"
#include "fortran03-typeutils.h"
#include "fortran03-typeenviron.h"
#include "fortran03-intrinsics.h"
#include "fortran03-scope.h"

#include <algorithm>
#include <set>

namespace TL { namespace Nanos6 {

    struct TaskPropertiesVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
      private:
        TaskProperties &_task_properties;

        void not_supported(const std::string &feature, Nodecl::NodeclBase n)
        {
            error_printf_at(n.get_locus(),
                            "%s is not supported in Nanos 6\n",
                            feature.c_str());
        }

        void not_supported_seq(const std::string &feature, Nodecl::List l)
        {
            for (Nodecl::List::iterator it = l.begin(); it != l.end(); it++)
            {
                not_supported(feature, *it);
            }
        }

        void ignored(const std::string &feature, Nodecl::NodeclBase n)
        {
            error_printf_at(
                n.get_locus(), "%s is ignored in Nanos 6\n", feature.c_str());
        }

        void ignored_seq(const std::string &feature, Nodecl::List l)
        {
            for (Nodecl::List::iterator it = l.begin(); it != l.end(); it++)
            {
                ignored(feature, *it);
            }
        }

      public:
        TaskPropertiesVisitor(TaskProperties &task_properties)
            : _task_properties(task_properties)
        {
        }

        virtual void visit(const Nodecl::OpenMP::Firstprivate &n)
        {
            _task_properties.firstprivate.insert(
                n.get_symbols()
                    .as<Nodecl::List>()
                    .to_object_list()
                    .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol));
        }

        virtual void visit(const Nodecl::OpenMP::Shared &n)
        {
            _task_properties.shared.insert(
                n.get_symbols()
                    .as<Nodecl::List>()
                    .to_object_list()
                    .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol));
        }

        virtual void visit(const Nodecl::OpenMP::Private &n)
        {
            _task_properties.private_.insert(
                n.get_symbols()
                    .as<Nodecl::List>()
                    .to_object_list()
                    .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol));
        }

        virtual void visit(const Nodecl::OpenMP::If &n)
        {
            ignored("if clause", n);
            Nodecl::Utils::remove_from_enclosing_list(n);
        }

        virtual void visit(const Nodecl::OpenMP::TaskReduction &n)
        {
            not_supported("task reductions",
                          n.get_reductions().as<Nodecl::List>());
        }

        virtual void visit(const Nodecl::OmpSs::Concurrent &n)
        {
            not_supported_seq("concurrent dependences",
                              n.get_exprs().as<Nodecl::List>());
        }

        template < typename T >
        void handle_dependences(const T& n, TL::ObjectList<Nodecl::NodeclBase>& dep_list)
        {
            _task_properties.any_task_dependence = true;
            dep_list.append(n.get_exprs().template as<Nodecl::List>().to_object_list());
        }

        virtual void visit(const Nodecl::OpenMP::DepIn &n)
        {
            handle_dependences(n, _task_properties.dep_in);
        }

        virtual void visit(const Nodecl::OpenMP::DepOut &n)
        {
            handle_dependences(n, _task_properties.dep_out);
        }

        virtual void visit(const Nodecl::OpenMP::DepInout &n)
        {
            handle_dependences(n, _task_properties.dep_inout);
        }

        virtual void visit(const Nodecl::OmpSs::DepWeakIn &n)
        {
            handle_dependences(n, _task_properties.dep_weakin);
        }

        virtual void visit(const Nodecl::OmpSs::DepWeakOut &n)
        {
            handle_dependences(n, _task_properties.dep_weakout);
        }

        virtual void visit(const Nodecl::OmpSs::DepWeakInout &n)
        {
            handle_dependences(n, _task_properties.dep_weakinout);
        }

        virtual void visit(const Nodecl::OmpSs::Commutative &n)
        {
            handle_dependences(n, _task_properties.dep_commutative);
        }

        virtual void visit(const Nodecl::OpenMP::Final &n)
        {
            _task_properties.final_ = n.get_condition();
        }

        virtual void visit(const Nodecl::OpenMP::Untied &n)
        {
            _task_properties.is_tied = false;
        }

        virtual void visit(const Nodecl::OmpSs::TaskLabel &n)
        {
            _task_properties.task_label = n.get_text();
        }

        virtual void visit(const Nodecl::OmpSs::Target &n)
        {
            Nodecl::List devices = n.get_devices().as<Nodecl::List>();
            for (Nodecl::List::iterator it = devices.begin();
                 it != devices.end();
                 it++)
            {
                if (std::string(strtolower(it->get_text().c_str())) != "smp")
                {
                    not_supported("devices other than smp", *it);
                }
            }
        }

        virtual void visit(const Nodecl::OmpSs::CopyIn &n)
        {
            ignored_seq("copy_in clause",
                        n.get_input_copies().as<Nodecl::List>());
        }

        virtual void visit(const Nodecl::OmpSs::CopyOut &n)
        {
            ignored_seq("copy_out clause",
                        n.get_output_copies().as<Nodecl::List>());
        }

        virtual void visit(const Nodecl::OmpSs::CopyInout &n)
        {
            ignored_seq("copy_inout clause",
                        n.get_inout_copies().as<Nodecl::List>());
        }

        virtual void visit(const Nodecl::OpenMP::FunctionTaskParsingContext &n)
        {
            _task_properties.locus_of_task_declaration = n.get_locus();
        }

        virtual void visit(const Nodecl::OmpSs::Alloca &n)
        {
            not_supported_seq("alloca captures",
                              n.get_exprs().as<Nodecl::List>());
        }

        virtual void visit(const Nodecl::OmpSs::SharedAndAlloca &n)
        {
            not_supported_seq("shared-and-alloca captures",
                              n.get_exprs().as<Nodecl::List>());
        }

        virtual void visit(const Nodecl::OmpSs::NDRange &n)
        {
            not_supported("ndrange clause", n);
        }

        virtual void visit(const Nodecl::OmpSs::ShMem &n)
        {
            not_supported("shmem clause", n);
        }

        virtual void visit(const Nodecl::OmpSs::Name &n)
        {
            not_supported("name clause", n);
        }

        virtual void visit(const Nodecl::OmpSs::Onto &n)
        {
            not_supported("onto clause", n);
        }

        virtual void visit(const Nodecl::OmpSs::Implements &n)
        {
            not_supported("implements clause", n);
        }

        virtual void visit(const Nodecl::OmpSs::DepInPrivate &n)
        {
            not_supported_seq("(private) value input dependences",
                              n.get_exprs().as<Nodecl::List>());
        }

        virtual void visit(const Nodecl::OmpSs::Cost &n)
        {
            _task_properties.cost = n.get_cost();
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
        tp.fix_data_sharing_of_this();
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
        tp.remove_data_sharing_of_this();
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

    void TaskProperties::compute_captured_saved_expressions()
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
    }

    bool TaskProperties::symbol_has_data_sharing_attribute(TL::Symbol sym) const
    {
        return shared.contains(sym)       ||
               private_.contains(sym)     ||
               firstprivate.contains(sym) ||
               captured_value.contains(sym);
    }

    void TaskProperties::compute_captured_symbols_without_data_sharings(
            Nodecl::NodeclBase n)
    {
        struct CaptureExtraVariables : public Nodecl::ExhaustiveVisitor<void>
        {
            TaskProperties& _tp;
            TL::ObjectList<TL::Symbol> _ignore_symbols;

            CaptureExtraVariables(TaskProperties& tp) : _tp(tp) { }

            void visit(const Nodecl::MultiExpression& node)
            {
                // The iterator of a MultiExpression has to be ignored!
                _ignore_symbols.append(node.get_symbol());
                this->Nodecl::ExhaustiveVisitor<void>::visit(node);
            }

            void visit(const Nodecl::Symbol& node)
            {
                TL::Symbol sym = node.get_symbol();
                if (!sym.is_variable())
                    return;

                if (sym.is_member())
                    return;

                if (_ignore_symbols.contains(sym))
                    return;

                if(!_tp.symbol_has_data_sharing_attribute(sym))
                    _tp.captured_value.insert(sym);
            }
        };

        CaptureExtraVariables visitor(*this);
        visitor.walk(n);
    }

    void TaskProperties::compute_captured_symbols_without_data_sharings(
            const TL::ObjectList<Nodecl::NodeclBase>& list)
    {
        for (TL::ObjectList<Nodecl::NodeclBase>::const_iterator it =  list.begin();
                it != list.end();
                ++it)
        {
            compute_captured_symbols_without_data_sharings(*it);
        }
    }

    void TaskProperties::compute_captured_symbols_without_data_sharings()
    {
        // Dependences
        compute_captured_symbols_without_data_sharings(dep_in);
        compute_captured_symbols_without_data_sharings(dep_out);
        compute_captured_symbols_without_data_sharings(dep_inout);
        compute_captured_symbols_without_data_sharings(dep_weakin);
        compute_captured_symbols_without_data_sharings(dep_weakout);
        compute_captured_symbols_without_data_sharings(dep_weakinout);
        compute_captured_symbols_without_data_sharings(dep_commutative);

        // Other task clauses
        compute_captured_symbols_without_data_sharings(final_);
        compute_captured_symbols_without_data_sharings(cost);
    }

    void TaskProperties::compute_captured_values()
    {
        compute_captured_saved_expressions();
        captured_value.insert(firstprivate);
        compute_captured_symbols_without_data_sharings();

    }

    namespace {
    struct SymbolThis
    {
      private:
        TL::Symbol &_found_this;

      public:
        SymbolThis(TL::Symbol &found_this) : _found_this(found_this)
        {
        }

        bool operator()(TL::Symbol s)
        {
            if (s.get_name() == "this")
            {
                _found_this = s;
                return true;
            }
            else
                return false;
        }
    };
    }

    void TaskProperties::remove_data_sharing_of_this()
    {
        if (!IS_CXX_LANGUAGE)
            return;

        TL::Symbol dummy;
        TL::ObjectList<TL::Symbol>::iterator it
            = std::remove_if(shared.begin(), shared.end(), SymbolThis(dummy));

        shared.erase(it, shared.end());
    }

    void TaskProperties::fix_data_sharing_of_this()
    {
        if (!IS_CXX_LANGUAGE)
            return;

        // Ideally we should not have to do this, but OpenMP::Base marks that
        // C++'s 'this' is shared: which concepually is if it weren't because
        // 'this' is an rvalue pointer
        TL::Symbol found_this;
        TL::ObjectList<TL::Symbol>::iterator it = std::remove_if(
            shared.begin(), shared.end(), SymbolThis(found_this));

        shared.erase(it, shared.end());
        if (found_this.is_valid())
            captured_value.insert(found_this);
    }

    namespace
    {

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

        void add_static_member_definition(TL::Symbol class_symbol,
                                          TL::Symbol field_member)
        {
            Nodecl::TopLevel top_level = CURRENT_COMPILED_FILE->nodecl;

            bool found = false;
            Nodecl::List l = top_level.get_top_level().as<Nodecl::List>();
            for (Nodecl::List::iterator it = l.begin(); it != l.end(); it++)
            {
                if (it->is<Nodecl::CxxDef>()
                    && it->get_symbol() == class_symbol)
                {
                    it->append_sibling(Nodecl::List::make(
                        Nodecl::ObjectInit::make(field_member),
                        Nodecl::CxxDef::make(
                            Nodecl::Context::make(Nodecl::NodeclBase::null(),
                                                  class_symbol.get_scope()),
                            field_member)));
                    found = true;
                    break;
                }
            }
            ERROR_CONDITION(!found,
                            "Definition of class '%s' not found\n",
                            class_symbol.get_qualified_name().c_str());
        }
    }

    void TaskProperties::create_task_info_regular_function(
        TL::Symbol task_info_struct,
        const std::string &task_info_name,
        /* out */
        TL::Symbol &task_info,
        TL::Symbol &task_invocation_info,
        Nodecl::NodeclBase &local_init)
    {
        // task info goes to the global scope
        task_info = TL::Scope::get_global_scope().new_symbol(task_info_name);
        task_info.get_internal_symbol()->kind = SK_VARIABLE;
        symbol_entity_specs_set_is_user_declared(
            task_info.get_internal_symbol(), 1);
        task_info.set_type(task_info_struct.get_user_defined_type());
        symbol_entity_specs_set_is_static(task_info.get_internal_symbol(), 1);

        // Task invocation info
        create_task_invocation_info(task_info, task_invocation_info);

        // Add required declarations to the tree
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            if (IS_CXX_LANGUAGE)
            {
                Nodecl::Utils::prepend_to_enclosing_top_level_location(
                    task_body,
                    Nodecl::CxxDef::make(Nodecl::NodeclBase::null(),
                                         task_info));
                Nodecl::Utils::prepend_to_enclosing_top_level_location(
                    task_body,
                    Nodecl::CxxDef::make(Nodecl::NodeclBase::null(),
                                         task_invocation_info));
            }

            Nodecl::Utils::prepend_to_enclosing_top_level_location(
                task_body, Nodecl::ObjectInit::make(task_info));
            Nodecl::Utils::prepend_to_enclosing_top_level_location(
                task_body, Nodecl::ObjectInit::make(task_invocation_info));
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            phase->get_extra_c_code().append(
                Nodecl::ObjectInit::make(task_info));
            phase->get_extra_c_code().append(
                Nodecl::ObjectInit::make(task_invocation_info));
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }

    void TaskProperties::create_task_info_nondependent_function(
        TL::Symbol task_info_struct,
        const std::string &task_info_name,
        /* out */
        TL::Symbol &task_info,
        TL::Symbol &task_invocation_info,
        Nodecl::NodeclBase &local_init)
    {
        if (!related_function.is_member())
            return create_task_info_regular_function(task_info_struct,
                                                     task_info_name,
                                                     task_info,
                                                     task_invocation_info,
                                                     local_init);

        // Member
        ERROR_CONDITION(!IS_CXX_LANGUAGE, "This is only for C++", 0);

        // task_info is a static member of the class
        TL::Type class_type = related_function.get_class_type();
        TL::Scope class_scope
            = ::class_type_get_inner_context(class_type.get_internal_type());

        task_info = class_scope.new_symbol(task_info_name);
        task_info.get_internal_symbol()->kind = SK_VARIABLE;
        symbol_entity_specs_set_is_user_declared(
            task_info.get_internal_symbol(), 1);
        task_info.set_type(task_info_struct.get_user_defined_type());
        symbol_entity_specs_set_is_member(task_info.get_internal_symbol(), 1);
        symbol_entity_specs_set_class_type(task_info.get_internal_symbol(),
                                           class_type.get_internal_type());
        symbol_entity_specs_set_is_static(task_info.get_internal_symbol(), 1);
        symbol_entity_specs_set_access(task_info.get_internal_symbol(),
                                       AS_PUBLIC);
        class_type_add_member(class_type.get_internal_type(),
                              task_info.get_internal_symbol(),
                              task_info.get_internal_symbol()->decl_context,
                              /* is_definition */ 0);

        set_is_dependent_type(class_type.get_internal_type(),
                              related_function.get_class_type().is_dependent());

        // Task invocation info
        create_task_invocation_info(task_info, task_invocation_info);

        // Add required declarations to the tree

        // Since we use a static member we need to generate it, but
        // this requires finding the class definition in the top
        // level so we can append it afterwards
        add_static_member_definition(task_info.get_class_type().get_symbol(),
                                     task_info);

        Nodecl::Utils::prepend_to_enclosing_top_level_location(
            task_body,
            Nodecl::CxxDef::make(Nodecl::NodeclBase::null(),
                                 task_invocation_info));
        Nodecl::Utils::prepend_to_enclosing_top_level_location(
            task_body, Nodecl::ObjectInit::make(task_info));
        Nodecl::Utils::prepend_to_enclosing_top_level_location(
            task_body, Nodecl::ObjectInit::make(task_invocation_info));

        Nodecl::Utils::prepend_to_enclosing_top_level_location(
            task_body,
            Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), task_info));
        Nodecl::Utils::prepend_to_enclosing_top_level_location(
            task_body,
            Nodecl::CxxDef::make(Nodecl::NodeclBase::null(),
                                 task_invocation_info));

        Nodecl::Utils::prepend_to_enclosing_top_level_location(
            task_body, Nodecl::ObjectInit::make(task_info));
        Nodecl::Utils::prepend_to_enclosing_top_level_location(
            task_body, Nodecl::ObjectInit::make(task_invocation_info));
    }

    void TaskProperties::create_task_info_dependent_function(
        TL::Symbol task_info_struct,
        const std::string &task_info_name,
        /* out */
        TL::Symbol &task_info,
        TL::Symbol &task_invocation_info,
        Nodecl::NodeclBase &local_init)
    {
        ERROR_CONDITION(!IS_CXX_LANGUAGE, "This is only for C++", 0);

        TL::Scope scope_of_template_class;
        if (!related_function.is_member())
        {

            // We want task_info symbol be in the anonymous namespace of the
            // global
            // scope, so make sure it has been created
            Source src;
            src << "namespace { }";
            src.parse_global(TL::Scope::get_global_scope());
            //

            TL::Symbol anonymous_namespace
                = TL::Scope::get_global_scope().get_symbol_from_name(
                    "(unnamed)");
            ERROR_CONDITION(!anonymous_namespace.is_valid(),
                            "Missing unnamed namespace",
                            0);
            scope_of_template_class = anonymous_namespace.get_internal_symbol()
                                          ->related_decl_context;
        }
        else
        {
            scope_of_template_class = ::class_type_get_inner_context(
                related_function.get_class_type().get_internal_type());
        }

        std::string task_info_tpl_name;
        {
            std::stringstream ss;
            TL::Counter &counter
                = TL::CounterManager::get_counter("nanos6-args");
            ss << "task_info_tpl_" << (int)counter;
            counter++;
            task_info_tpl_name = ss.str();
        }

        template_parameter_list_t *tpl
            = template_specialized_type_get_template_parameters(
                related_function.get_type().get_internal_type());

        TL::Symbol new_class_symbol
            = SymbolUtils::new_class_template(task_info_tpl_name,
                                              tpl,
                                              scope_of_template_class,
                                              locus_of_task_creation);

        if (related_function.is_member())
        {
            type_t *current_class
                = related_function.get_class_type().get_internal_type();
            symbol_entity_specs_set_is_member(
                new_class_symbol.get_internal_symbol(), 1);
            symbol_entity_specs_set_class_type(
                new_class_symbol.get_internal_symbol(), current_class);
            symbol_entity_specs_set_is_defined_inside_class_specifier(
                new_class_symbol.get_internal_symbol(), 1);
            symbol_entity_specs_set_access(
                new_class_symbol.get_internal_symbol(), AS_PUBLIC);
            class_type_add_member(
                current_class,
                new_class_symbol.get_internal_symbol(),
                new_class_symbol.get_internal_symbol()->decl_context,
                /* is_definition */ 1);
            class_type_set_enclosing_class_type(
                new_class_symbol.get_type().get_internal_type(), current_class);
        }

        TL::Scope class_scope(class_type_get_inner_context(
            new_class_symbol.get_type().get_internal_type()));


        // Now add the field
        task_info = class_scope.new_symbol(task_info_name);
        task_info.get_internal_symbol()->kind = SK_VARIABLE;
        symbol_entity_specs_set_is_user_declared(
            task_info.get_internal_symbol(), 1);
        task_info.set_type(task_info_struct.get_user_defined_type());
        symbol_entity_specs_set_is_member(task_info.get_internal_symbol(), 1);
        symbol_entity_specs_set_class_type(
            task_info.get_internal_symbol(),
            new_class_symbol.get_user_defined_type().get_internal_type());
        symbol_entity_specs_set_is_static(task_info.get_internal_symbol(), 1);
        symbol_entity_specs_set_access(task_info.get_internal_symbol(),
                                       AS_PUBLIC);
        class_type_add_member(new_class_symbol.get_type().get_internal_type(),
                              task_info.get_internal_symbol(),
                              task_info.get_internal_symbol()->decl_context,
                              /* is_definition */ 0);

        // Finish the template class
        nodecl_t nodecl_output = nodecl_null();
        finish_class_type(
            new_class_symbol.get_type().get_internal_type(),
            ::get_user_defined_type(new_class_symbol.get_internal_symbol()),
            new_class_symbol.get_scope().get_decl_context(),
            locus_of_task_creation,
            &nodecl_output);
        set_is_complete_type(new_class_symbol.get_type().get_internal_type(),
                             /* is_complete */ 1);
        set_is_complete_type(
            get_actual_class_type(
                new_class_symbol.get_type().get_internal_type()),
            /* is_complete */ 1);

        // Task invocation
        create_task_invocation_info(task_info, task_invocation_info);

        // Add required declarations to the tree
        Nodecl::Utils::prepend_to_enclosing_top_level_location(
            task_body,
            Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), new_class_symbol));
        add_static_member_definition(new_class_symbol, task_info);

        Nodecl::Utils::prepend_to_enclosing_top_level_location(
            task_body,
            Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), task_info));
        Nodecl::Utils::prepend_to_enclosing_top_level_location(
            task_body,
            Nodecl::CxxDef::make(Nodecl::NodeclBase::null(),
                                 task_invocation_info));

        Nodecl::Utils::prepend_to_enclosing_top_level_location(
            task_body, Nodecl::ObjectInit::make(task_info));
        Nodecl::Utils::prepend_to_enclosing_top_level_location(
            task_body, Nodecl::ObjectInit::make(task_invocation_info));
    }

    void TaskProperties::create_task_invocation_info(
        TL::Symbol task_info,
        /* out */ TL::Symbol &task_invocation_info)
    {
        TL::Symbol task_invocation_info_struct
            = TL::Scope::get_global_scope().get_symbol_from_name(
                "nanos_task_invocation_info");
        ERROR_CONDITION(
            !task_invocation_info_struct.is_valid(), "Invalid symbol", 0);

        std::string task_invocation_info_name;
        {
            std::stringstream ss;
            TL::Counter &counter
                = TL::CounterManager::get_counter("nanos6-args");
            ss << "task_invocation_info_" << (int)counter;
            counter++;
            task_invocation_info_name = ss.str();
        }

        task_invocation_info = TL::Scope::get_global_scope().new_symbol(
            task_invocation_info_name);
        task_invocation_info.get_internal_symbol()->kind = SK_VARIABLE;
        symbol_entity_specs_set_is_user_declared(
            task_invocation_info.get_internal_symbol(), 1);
        task_invocation_info.set_type(
            task_invocation_info_struct.get_user_defined_type());
        symbol_entity_specs_set_is_static(
            task_invocation_info.get_internal_symbol(), 1);

        TL::ObjectList<TL::Symbol> task_invocation_fields
            = task_invocation_info_struct.get_type()
                  .get_nonstatic_data_members();
        GetField get_field_task_invocation_info(task_invocation_fields);

        Nodecl::NodeclBase field_invocation_source
            = get_field_task_invocation_info("invocation_source");

        const char *c = locus_to_str(locus_of_task_creation);
        Nodecl::NodeclBase init_invocation_source = const_value_to_nodecl(
            const_value_make_string_null_ended(c, strlen(c)));

        Nodecl::NodeclBase task_invocation_init = Nodecl::StructuredValue::make(
            Nodecl::List::make(Nodecl::FieldDesignator::make(
                field_invocation_source,
                init_invocation_source,
                field_invocation_source.get_type())),
            Nodecl::StructuredValueBracedImplicit::make(),
            task_info.get_type());
        task_invocation_info.set_value(task_invocation_init);
    }

    Nodecl::NodeclBase create_final_task_flags(TL::Symbol task_flags, Nodecl::NodeclBase condition)
    {
        Nodecl::NodeclBase ret;

        if (condition.is_null())
        {
            if (IS_FORTRAN_LANGUAGE)
                condition = Nodecl::BooleanLiteral::make(get_bool_type(), const_value_get_zero(type_get_size(get_bool_type()), /* sign */ 1));
            else
                condition = const_value_to_nodecl(const_value_get_signed_int(0));

        }

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            // In this case we only need an expression
            ret = Nodecl::BitwiseShl::make(
                    /* lhs */
                    Nodecl::Different::make(
                        condition,
                        const_value_to_nodecl(const_value_get_signed_int(0)),
                        TL::Type::get_bool_type()),
                    /* rhs */
                    const_value_to_nodecl(const_value_get_signed_int(0)),
                    /* type */
                    get_size_t_type());
        }
        else // IS_FORTRAN_LANGUAGE
        {
            TL::Scope sc = TL::Scope::get_global_scope();

            Nodecl::NodeclBase arg1 = Nodecl::FortranActualArgument::make(task_flags.make_nodecl());
            Nodecl::NodeclBase arg2 = Nodecl::FortranActualArgument::make(const_value_to_nodecl(const_value_get_signed_int(0)));
            Nodecl::NodeclBase arguments_list = Nodecl::List::make(arg1, arg2);

            nodecl_t actual_arguments[2] = { arg1.get_internal_nodecl(), arg2.get_internal_nodecl() };

            TL::Symbol intrinsic_ibset(
                    fortran_solve_generic_intrinsic_call(
                        fortran_query_intrinsic_name_str(sc.get_decl_context(), "ibset"),
                        actual_arguments,
                        /* explicit_num_actual_arguments */ 2,
                        /* is_call */ 0));

            Nodecl::FunctionCall ibset_function_call =
                Nodecl::FunctionCall::make(
                    intrinsic_ibset.make_nodecl(),
                    arguments_list,
                    /* alternate_name */ Nodecl::NodeclBase::null(),
                    /* function_form */ Nodecl::NodeclBase::null(),
                    intrinsic_ibset.get_type().returns(),
                    task_flags.get_locus());

            Nodecl::NodeclBase actual_arguments_ibclr =
                Nodecl::List::make(arg1.shallow_copy(), arg2.shallow_copy());

            TL::Symbol intrinsic_ibclr(
                    fortran_solve_generic_intrinsic_call(
                        fortran_query_intrinsic_name_str(sc.get_decl_context(), "ibclr"),
                        actual_arguments,
                        /* explicit_num_actual_arguments */ 2,
                        /* is_call */ 0));

            Nodecl::FunctionCall ibclr_function_call = Nodecl::FunctionCall::make(
                    intrinsic_ibclr.make_nodecl(),
                    arguments_list.shallow_copy(),
                    /* alternate_name */ Nodecl::NodeclBase::null(),
                    /* function_form */ Nodecl::NodeclBase::null(),
                    intrinsic_ibclr.get_type().returns(),
                    task_flags.get_locus());

            ret = Nodecl::IfElseStatement::make(
                /* condition */
                condition,
                /* then */
                Nodecl::List::make(
                    Nodecl::ExpressionStatement::make(
                        Nodecl::Assignment::make(
                               /* lhs */ task_flags.make_nodecl(),
                               /* rhs */ ibset_function_call,
                               /* type */ task_flags.get_type().get_lvalue_reference_to()
                        ),
                        task_flags.get_locus()
                    )
                ),
                /* else */
                Nodecl::List::make(
                    Nodecl::ExpressionStatement::make(
                        Nodecl::Assignment::make(
                               /* lhs */ task_flags.make_nodecl(),
                               /* rhs */ ibclr_function_call,
                               /* type */ task_flags.get_type().get_lvalue_reference_to()
                        ),
                        /* locus */
                        task_flags.get_locus()
                    )
                )
            );
        }
        return ret;
    }

    Nodecl::NodeclBase TaskProperties::create_task_flags(TL::Symbol task_flags)
    {
        Nodecl::NodeclBase ret;
        Nodecl::NodeclBase final_nodecl;
        // Nodecl::NodeclBase if_nodecl;

        // Final flag
        final_nodecl = create_final_task_flags(task_flags, this->final_);
        ret = final_nodecl;

        // Nodecl::NodeclBase if_flag = create_if_task_flags(task_flags);

        // if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        // {
        //     ret = Nodecl::LogicalOr::make(final_flag, if_flag);
        // }
        // else // if (IS_FORTRAN_LANGUAGE)
        // {
        //     // Just a set of IfElseStatement that need to be generated one
        //     // after the other
        //     ret = Nodecl::List();
        //     ret.append(final_flag);
        //     ret.append(if_flag);
        // }

        return ret;
    }

    void TaskProperties::create_task_info(
            /* out */
            TL::Symbol &task_info,
            TL::Symbol &task_invocation_info,
            Nodecl::NodeclBase &local_init)
    {
        create_outline_function();
        create_dependences_function();
        create_copies_function();
        create_cost_function();

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

        if (IS_C_LANGUAGE || IS_FORTRAN_LANGUAGE)
        {
            create_task_info_regular_function(task_info_struct,
                                              task_info_name,
                                              task_info,
                                              task_invocation_info,
                                              local_init);
        }
        else if (IS_CXX_LANGUAGE)
        {
            if (!related_function.get_type().is_template_specialized_type()
                || (!related_function.get_type().is_dependent()
                    && (!related_function.is_member()
                        || !related_function.get_class_type().is_dependent())))
            {
                create_task_info_nondependent_function(task_info_struct,
                                                       task_info_name,
                                                       task_info,
                                                       task_invocation_info,
                                                       local_init);
            }
            else
            {
                create_task_info_dependent_function(task_info_struct,
                                                    task_info_name,
                                                    task_info,
                                                    task_invocation_info,
                                                    local_init);
            }
        }
        else
        {
            internal_error("Code unreachable", 0);
        }

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

        Nodecl::NodeclBase init_run_final;

        if (IS_FORTRAN_LANGUAGE)
        {
            init_run_final = outline_function_mangled.make_nodecl(/* set_ref_type */ true);
        }
        else
        {
            init_run_final = outline_function.make_nodecl(/* set_ref_type */ true);
        }
        init_run_final = Nodecl::Conversion::make(
                init_run_final,
                run_type);
        init_run_final.set_text("C");

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


        Nodecl::NodeclBase field_get_cost = get_field("get_cost");
        Nodecl::NodeclBase init_get_cost;
        if (cost_function.is_valid())
        {
            TL::Type cost_fun_type
                = TL::Type::get_size_t_type()
                      .get_function_returning(TL::ObjectList<TL::Type>(
                          1, TL::Type::get_void_type().get_pointer_to()))
                      .get_pointer_to();

            init_get_cost = cost_function.make_nodecl(/* set_ref_type */ true);
            init_get_cost
                = Nodecl::Conversion::make(init_get_cost, cost_fun_type);
            init_get_cost.set_text("C");
        }
        else
        {
            init_get_cost
                = const_value_to_nodecl(const_value_get_signed_int(0));
        }

        TL::ObjectList<Nodecl::NodeclBase> field_init;
        field_init.append(Nodecl::FieldDesignator::make(
            field_run, init_run, field_run.get_type()));
        field_init.append(
            Nodecl::FieldDesignator::make(field_register_depinfo,
                                          init_register_depinfo,
                                          field_register_depinfo.get_type()));
        field_init.append(
            Nodecl::FieldDesignator::make(field_register_copies,
                                          init_register_copies,
                                          field_register_copies.get_type()));
        field_init.append(Nodecl::FieldDesignator::make(
            field_task_label, init_task_label, field_task_label.get_type()));
        field_init.append(
            Nodecl::FieldDesignator::make(field_declaration_source,
                                          init_declaration_source,
                                          field_declaration_source.get_type()));
        field_init.append(Nodecl::FieldDesignator::make(
            field_get_cost, init_get_cost, field_get_cost.get_type()));

        Nodecl::NodeclBase struct_init = Nodecl::StructuredValue::make(
            Nodecl::List::make(field_init),
            Nodecl::StructuredValueBracedImplicit::make(),
            task_info.get_type());

        task_info.set_value(struct_init);

        if (IS_FORTRAN_LANGUAGE)
        {
            // Create a detached symbol with the same name as the real one
            // We need to do that otherwise Fortran codegen attempts to
            // initialize this symbol
            // (We may want to fix this somehow)
            symbol_entity_specs_set_is_static(task_info.get_internal_symbol(),
                                              0);
            //
            scope_entry_t *task_info_shim = NEW0(scope_entry_t);
            task_info_shim->symbol_name
                = task_info.get_internal_symbol()->symbol_name;
            task_info_shim->kind = task_info.get_internal_symbol()->kind;
            task_info_shim->decl_context
                = task_info.get_internal_symbol()->decl_context;
            symbol_entity_specs_set_is_user_declared(task_info_shim, 1);
            // Fake the structure size
            const int size_of_ptr
                = TL::Type::get_void_type().get_pointer_to().get_size();
            ERROR_CONDITION(task_info.get_type().get_size() % size_of_ptr != 0,
                            "Struct size does not divide the size of a pointer",
                            0);
            int num_elements = task_info.get_type().get_size() / size_of_ptr;
            task_info_shim->type_information
                = TL::Type(fortran_choose_int_type_from_kind(size_of_ptr))
                      .get_array_to(
                           const_value_to_nodecl(
                               const_value_get_signed_int(num_elements)),
                           TL::Scope::get_global_scope())
                      .get_internal_type();

            task_info = task_info_shim;

            // Ditto for task_invocation_info
            symbol_entity_specs_set_is_static(
                task_invocation_info.get_internal_symbol(), 0);
            //
            scope_entry_t *task_invocation_info_shim = NEW0(scope_entry_t);
            task_invocation_info_shim->symbol_name
                = task_invocation_info.get_internal_symbol()->symbol_name;
            task_invocation_info_shim->kind
                = task_invocation_info.get_internal_symbol()->kind;
            task_invocation_info_shim->decl_context
                = task_invocation_info.get_internal_symbol()->decl_context;
            symbol_entity_specs_set_is_user_declared(task_invocation_info_shim,
                                                     1);
            ERROR_CONDITION(
                task_invocation_info.get_type().get_size() % size_of_ptr != 0,
                "Struct size does not divide the size of a pointer",
                0);
            num_elements = task_invocation_info.get_type().get_size()
                           / size_of_ptr;
            task_invocation_info_shim->type_information
                = TL::Type(fortran_choose_int_type_from_kind(size_of_ptr))
                      .get_array_to(
                           const_value_to_nodecl(
                               const_value_get_signed_int(num_elements)),
                           TL::Scope::get_global_scope())
                      .get_internal_type();

            task_invocation_info = task_invocation_info_shim;
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

    TL::Symbol TaskProperties::add_field_to_class(TL::Symbol new_class_symbol,
                                                  TL::Scope class_scope,
                                                  const std::string &var_name,
                                                  const locus_t *var_locus,
                                                  bool is_allocatable,
                                                  TL::Type field_type)
    {
        TL::Type new_class_type = new_class_symbol.get_user_defined_type();

        std::string orig_field_name = var_name;

        if (IS_CXX_LANGUAGE && orig_field_name == "this")
        {
            orig_field_name = "_this";
        }

        TL::Symbol field = class_scope.new_symbol(orig_field_name);
        field.get_internal_symbol()->kind = SK_VARIABLE;
        symbol_entity_specs_set_is_user_declared(field.get_internal_symbol(), 1);

        field.set_type( field_type );
        field.get_internal_symbol()->locus = var_locus;

        symbol_entity_specs_set_is_member(field.get_internal_symbol(), 1);
        symbol_entity_specs_set_class_type(field.get_internal_symbol(),
                new_class_type.get_internal_type());
        symbol_entity_specs_set_access(field.get_internal_symbol(), AS_PUBLIC);

        symbol_entity_specs_set_is_allocatable(
                field.get_internal_symbol(), is_allocatable);

        class_type_add_member(
                new_class_type.get_internal_type(),
                field.get_internal_symbol(),
                field.get_internal_symbol()->decl_context,
                /* is_definition */ 1);

        return field;
    }

    namespace
    {
    TL::Type fortran_storage_type_array_descriptor(TL::Type array_type)
    {
        TL::Type void_pointer = TL::Type::get_void_type().get_pointer_to();
        TL::Type suitable_integer
            = fortran_choose_int_type_from_kind(void_pointer.get_size());

        size_t size_of_array_descriptor = fortran_size_of_array_descriptor(
            fortran_get_rank0_type(array_type.get_internal_type()),
            fortran_get_rank_of_type(array_type.get_internal_type()));

        ERROR_CONDITION(
            (size_of_array_descriptor % suitable_integer.get_size()) != 0,
            "The size of the descriptor is not a multiple of the integer type",
            0);

        int num_items = size_of_array_descriptor / suitable_integer.get_size();

        return get_array_type_bounds(
            suitable_integer.get_internal_type(),
            const_value_to_nodecl(const_value_get_signed_int(1)),
            const_value_to_nodecl(const_value_get_signed_int(num_items)),
            TL::Scope::get_global_scope().get_decl_context());
    }

    std::string get_name_for_descriptor(const std::string &var_name)
    {
        Counter &counter
            = CounterManager::get_counter("array-descriptor-copies");
        std::stringstream ss;
        ss << var_name << "_descriptor_" << (int)counter;
        counter++;
        return ss.str();
    }
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
        symbol_entity_specs_set_is_user_declared(
            new_class_symbol.get_internal_symbol(), 1);

        type_t *new_class_type
            = get_new_class_type(sc.get_decl_context(), TT_STRUCT);

        if (related_function.get_type().is_template_specialized_type()
            && related_function.get_type().is_dependent())
        {
            template_parameter_list_t *tpl
                = related_function.get_type()
                      .template_specialized_type_get_template_parameters()
                      .get_internal_template_parameter_list();
            ERROR_CONDITION(
                tpl == NULL, "There must be template parameters", 0);

            new_class_symbol.get_internal_symbol()->kind = SK_TEMPLATE;
            type_t *template_type = get_new_template_type(
                tpl,
                new_class_type,
                uniquestr(structure_name.c_str()),
                related_function.get_scope().get_decl_context(),
                locus_of_task_creation);
            new_class_symbol.set_type(template_type);

            ::template_type_set_related_symbol(
                template_type, new_class_symbol.get_internal_symbol());

            symbol_entity_specs_set_is_user_declared(
                new_class_symbol.get_internal_symbol(), 1);

            new_class_symbol
                = TL::Type(::template_type_get_primary_type(
                               new_class_symbol.get_type().get_internal_type()))
                      .get_symbol();
            new_class_type = new_class_symbol.get_type().get_internal_type();
        }
        else
        {
            new_class_symbol.get_internal_symbol()->kind = SK_CLASS;
        }

        if (sc.is_class_scope())
        {
            symbol_entity_specs_set_is_member(
                new_class_symbol.get_internal_symbol(), 1);
            symbol_entity_specs_set_access(
                new_class_symbol.get_internal_symbol(), AS_PUBLIC);
            symbol_entity_specs_set_class_type(
                new_class_symbol.get_internal_symbol(),
                related_function.get_class_type().get_internal_type());
            symbol_entity_specs_set_is_defined_inside_class_specifier(
                new_class_symbol.get_internal_symbol(), 1);

            class_type_add_member(
                related_function.get_class_type().get_internal_type(),
                new_class_symbol.get_internal_symbol(),
                new_class_symbol.get_internal_symbol()->decl_context,
                /* is_definition */ 1);

            class_type_set_enclosing_class_type(new_class_type,
                                                sc.get_related_symbol()
                                                    .get_user_defined_type()
                                                    .get_internal_type());
            set_is_dependent_type(
                new_class_type,
                is_dependent_type(new_class_type)
                    || sc.get_related_symbol().get_type().is_dependent());
        }

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
                // FIXME: this has been designed thinking in C/C++, does it work for Fortran????
                type_of_field = TL::Type::get_void_type().get_pointer_to();
            }
            else if (type_of_field.is_function())
            {
                if (IS_FORTRAN_LANGUAGE)
                {
                    type_of_field = TL::Type::get_void_type().get_pointer_to();
                }
                else
                {
                    type_of_field = type_of_field.get_pointer_to();
                }
            }
            else
            {
                type_of_field = type_of_field.get_unqualified_type();

            }

            TL::Symbol field = add_field_to_class(
                    new_class_symbol,
                    class_scope,
                    it->get_name(),
                    it->get_locus(),
                    symbol_entity_specs_get_is_allocatable(it->get_internal_symbol()),
                    type_of_field);

            field_map[*it] = field;
        }

        for (TL::ObjectList<TL::Symbol>::iterator it = shared.begin();
                it != shared.end();
                it++)
        {
            TL::Type type_of_field = it->get_type().no_ref();

            if (IS_FORTRAN_LANGUAGE)
            {
                type_of_field = TL::Type::get_void_type().get_pointer_to();
                if (it->get_type().no_ref().is_array()
                    && it->get_type().no_ref().array_requires_descriptor())
                {
                    TL::Symbol field = add_field_to_class(
                        new_class_symbol,
                        class_scope,
                        get_name_for_descriptor(it->get_name()),
                        it->get_locus(),
                        /* is_allocatable */ false,
                        fortran_storage_type_array_descriptor(
                            it->get_type().no_ref()));

                    array_descriptor_map[*it] = field;
                }
            }
            else
            {
                if (type_of_field.depends_on_nonconstant_values())
                {
                    type_of_field = TL::Type::get_void_type().get_pointer_to();
                }
                else if (type_of_field.is_array()
                         && !it->get_type().is_any_reference())
                {
                    type_of_field
                        = type_of_field.array_element().get_pointer_to();
                }
                else
                {
                    type_of_field = type_of_field.get_pointer_to();
                }
            }

            TL::Symbol field = add_field_to_class(
                    new_class_symbol,
                    class_scope,
                    it->get_name(),
                    it->get_locus(),
                    /* is_allocatable */ false,
                    type_of_field);

            field_map[*it] = field;
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


        // Computing the size of the arguments structure
        {

            // This nodecl represents the size of the structure of arguments
            Nodecl::NodeclBase basic_size;
            if (new_class_symbol.get_type().is_dependent())
            {
                basic_size = Nodecl::Sizeof::make(
                        Nodecl::Type::make(info_structure, locus_of_task_creation),
                        Nodecl::NodeclBase::null(),
                        TL::Type::get_size_t_type(),
                        locus_of_task_creation);
            }
            else
            {
                basic_size = const_value_to_nodecl_with_basic_type(
                        const_value_get_integer(
                            info_structure.get_size(),
                            /* bytes */ type_get_size(get_size_t_type()),
                            /* sign */ 0),
                        get_size_t_type());
            }

            // This nodecl represents the extra storage that the runtime has to
            // allocate contiguosly to the arguments structure to support VLAs
            Nodecl::NodeclBase extra_storage = const_value_to_nodecl(const_value_get_signed_int(0));
            for (TL::ObjectList<TL::Symbol>::iterator it = captured_value.begin();
                    it != captured_value.end();
                    it++)
            {
                if (it->get_type().depends_on_nonconstant_values())
                {
                    Nodecl::NodeclBase size_of_array = Nodecl::Add::make(
                            const_value_to_nodecl(const_value_get_signed_int(VLA_OVERALLOCATION_ALIGN)),
                            Nodecl::Sizeof::make(
                                Nodecl::Type::make(it->get_type()),
                                Nodecl::NodeclBase::null(),
                                get_size_t_type()),
                            get_size_t_type());

                    extra_storage = Nodecl::Add::make(
                            extra_storage,
                            size_of_array,
                            size_of_array.get_type());
                }
            }

            // Finally, we compute the real size of our arguments
            args_size = Nodecl::Add::make(basic_size, extra_storage, basic_size.get_type());
        }

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

        struct AddParameter
        {
          private:
            TL::ObjectList<std::string> &_unpack_parameter_names;
            TL::ObjectList<TL::Type> &_unpack_parameter_types;
            std::map<TL::Symbol, std::string> &_symbols_to_param_names;

          public:
            AddParameter(
                TL::ObjectList<std::string> &unpack_parameter_names,
                TL::ObjectList<TL::Type> &unpack_parameter_types,
                std::map<TL::Symbol, std::string> &symbols_to_param_names)
                : _unpack_parameter_names(unpack_parameter_names),
                  _unpack_parameter_types(unpack_parameter_types),
                  _symbols_to_param_names(symbols_to_param_names)
            {
            }

            void operator()(TL::Symbol sym)
            {
                std::string name = sym.get_name();
                if (IS_CXX_LANGUAGE && sym.get_name() == "this")
                {
                    name = "_this";
                }

                _symbols_to_param_names[sym] = name;
                _unpack_parameter_names.append(name);

                TL::Type p_type = sym.get_type().no_ref();

                if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                {
                    if (p_type.is_array())
                    {
                        p_type = p_type.array_element().get_pointer_to();
                    }
                    else
                    {
                        p_type = p_type.get_lvalue_reference_to();
                    }
                }
                else if (IS_FORTRAN_LANGUAGE)
                {
                    p_type = p_type.get_lvalue_reference_to();
                }
                else
                {
                    internal_error("Code unreachable", 0);
                }

                _unpack_parameter_types.append(p_type);
            }
        };

        struct ParameterToSymbol
        {
            TL::Scope _sc;
            ParameterToSymbol(TL::Scope sc) : _sc(sc)
            {
            }

            TL::Symbol operator()(const std::string &str) const
            {
                TL::Symbol param_sym = _sc.get_symbol_from_name(str);
                ERROR_CONDITION(!param_sym.is_valid()
                                    || (!param_sym.is_parameter()
                                        && !param_sym.is_saved_expression()),
                                "Invalid symbol for name '%s'",
                                str.c_str());
                return param_sym;
            }
        };


        struct MapSymbols
        {
          private:
            const std::map<TL::Symbol, std::string> &_symbols_to_param_names;
            TL::ObjectList<TL::Symbol> &_parameters_to_update_type;
            Nodecl::Utils::SimpleSymbolMap &_symbol_map;

            const ParameterToSymbol &_param_to_symbol;

            static bool type_is_runtime_sized(TL::Type t)
            {
                if (!t.is_valid())
                    return false;

                if (t.is_any_reference())
                    return type_is_runtime_sized(t.no_ref());
                else if (t.is_pointer())
                    return type_is_runtime_sized(t.points_to());
                else if (t.is_array())
                {
                    if (type_is_runtime_sized(t.array_element()))
                        return true;

                    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                    {
                        Nodecl::NodeclBase size = t.array_get_size();
                        return !size.is_null() && !size.is_constant();
                    }
                    else if (IS_FORTRAN_LANGUAGE)
                    {
                        Nodecl::NodeclBase lbound, ubound;
                        t.array_get_bounds(lbound, ubound);

                        return (!lbound.is_null() && !lbound.is_constant())
                               || (!ubound.is_null() && !ubound.is_constant());
                    }
                    else
                    {
                        internal_error("Code unreachable", 0);
                    }
                }
                return false;
            }

          public:
            MapSymbols(
                const ParameterToSymbol &param_to_symbol,
                const std::map<TL::Symbol, std::string> &symbols_to_param_names,
                /* out */ Nodecl::Utils::SimpleSymbolMap &symbol_map,
                /* out */ TL::ObjectList<TL::Symbol> &parameter_to_update_type)
                : _symbols_to_param_names(symbols_to_param_names),
                  _parameters_to_update_type(parameter_to_update_type),
                  _symbol_map(symbol_map),
                  _param_to_symbol(param_to_symbol)
            {
            }

            void operator()(TL::Symbol sym)
            {
                std::map<TL::Symbol, std::string>::const_iterator it_param_name
                    = _symbols_to_param_names.find(sym);
                ERROR_CONDITION(it_param_name == _symbols_to_param_names.end(),
                                "Symbol '%s' not mapped",
                                sym.get_name().c_str());

                TL::Symbol param_sym = _param_to_symbol(it_param_name->second);
                _symbol_map.add_map(sym, param_sym);

                // Propagate TARGET attribute
                if (sym.is_target())
                  symbol_entity_specs_set_is_target(
                      param_sym.get_internal_symbol(), 1);

                // Propagate ALLOCATABLE attribute
                if (sym.is_allocatable())
                    symbol_entity_specs_set_is_allocatable(
                        param_sym.get_internal_symbol(), 1);

                if (type_is_runtime_sized(param_sym.get_type()))
                    _parameters_to_update_type.append(param_sym);
            }
        };
    }

    TL::Type TaskProperties::rewrite_type_for_outline(
        TL::Type t, TL::Scope scope, Nodecl::Utils::SymbolMap &symbol_map)
    {
        return type_deep_copy(
            t.get_internal_type(),
            scope.get_decl_context(),
            symbol_map.get_symbol_map());
    }

    void TaskProperties::create_outline_function()
    {
        // Unpacked function
        TL::ObjectList<std::string> unpack_parameter_names;
        TL::ObjectList<TL::Type> unpack_parameter_types;
        std::map<TL::Symbol, std::string> symbols_to_param_names;

        std::string unpacked_name;
        {
            TL::Counter &counter = TL::CounterManager::get_counter("nanos6-outline");
            std::stringstream ss;
            ss << "nanos6_unpack_" << (int)counter;
            counter++;
            unpacked_name = ss.str();
        }

        captured_value.map(AddParameter(unpack_parameter_names,
                                        unpack_parameter_types,
                                        symbols_to_param_names));
        shared.map(AddParameter(unpack_parameter_names,
                                unpack_parameter_types,
                                symbols_to_param_names));

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

        ParameterToSymbol param_to_symbol(unpacked_inside_scope);

        TL::ObjectList<TL::Symbol> parameters_to_update_type;
        captured_value.map(MapSymbols(param_to_symbol,
                                      symbols_to_param_names,
                                      /* out */ symbol_map,
                                      /* out */ parameters_to_update_type));
        shared.map(MapSymbols(param_to_symbol,
                              symbols_to_param_names,
                              /* out */ symbol_map,
                              /* out */ parameters_to_update_type));

        // Add extra mappings for VLAs
        TL::ObjectList<TL::Symbol> new_vlas;
        for (TL::ObjectList<TL::Symbol>::iterator it
             = parameters_to_update_type.begin();
             it != parameters_to_update_type.end();
             it++)
        {
            add_extra_mappings_for_vla_types(it->get_type(),
                                             unpacked_inside_scope,
                                             /* out */
                                             symbol_map,
                                             new_vlas);
        }

        // Now fix the types of runtime sized types prior anything else
        for (TL::ObjectList<TL::Symbol>::iterator it = parameters_to_update_type.begin();
                it != parameters_to_update_type.end();
                it++)
        {
            it->set_type(rewrite_type_for_outline(it->get_type(), unpacked_inside_scope, symbol_map));
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
            priv.set_type(rewrite_type_for_outline(it->get_type().no_ref(), unpacked_inside_scope, symbol_map));

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

        Nodecl::List nested_functions;
        if (IS_FORTRAN_LANGUAGE)
        {
            Nodecl::Utils::Fortran::ExtraDeclsVisitor fun_visitor(
                    symbol_map,
                    unpacked_inside_scope,
                    related_function);
            fun_visitor.insert_extra_symbols(task_body);

            if (related_function.is_in_module())
            {
                TL::Symbol in_module = related_function.in_module();
                Nodecl::Utils::Fortran::append_used_modules(
                        task_body.retrieve_context(),
                        in_module.get_related_scope());
            }

            Nodecl::Utils::Fortran::append_used_modules(
                    task_body.retrieve_context(),
                    unpacked_inside_scope);

            if (related_function.is_nested_function())
            {
                TL::Symbol enclosing_function = related_function.get_scope().get_related_symbol();

                ERROR_CONDITION(!enclosing_function.is_valid()
                        || !(enclosing_function.is_function()
                            || enclosing_function.is_fortran_main_program()),
                        "Invalid enclosing symbol of nested function", 0);

                Nodecl::Utils::Fortran::append_used_modules(
                        enclosing_function.get_related_scope(),
                        unpacked_inside_scope);
            }

            fortran_add_types(unpacked_inside_scope);

            // Now get all the needed internal functions and duplicate them in the outline
            Nodecl::Utils::Fortran::InternalFunctions internal_functions;
            internal_functions.walk(task_body);

            nested_functions = duplicate_internal_subprograms(internal_functions.function_codes,
                    unpacked_inside_scope,
                    symbol_map);
        }

        // Deep copy the body
        Nodecl::NodeclBase body = Nodecl::Utils::deep_copy(task_body, unpacked_inside_scope, symbol_map);
        unpacked_empty_stmt.prepend_sibling(body);


        unpacked_empty_stmt.append_sibling(nested_functions);

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

        Nodecl::Utils::append_to_top_level_nodecl(unpacked_function_code);

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
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            Nodecl::List args;

            for (TL::ObjectList<TL::Symbol>::iterator it = captured_value.begin();
                    it != captured_value.end();
                    it++)
            {
                ERROR_CONDITION(field_map.find(*it) == field_map.end(), "Symbol is not mapped", 0);

                if (it->get_type().depends_on_nonconstant_values())
                {
                    //if (it->get_type().is_array())
                    //{
                    //    internal_error("Capture of array values not implemented yet", 0);
                    //}
                    //else
                    {
                        TL::Type param_type = rewrite_type_using_args(
                                arg,
                                it->get_type().no_ref(),
                                TL::ObjectList<TL::Symbol>()
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
                        type_in_outline = rewrite_type_using_args(arg, type_in_outline, TL::ObjectList<TL::Symbol>());

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
                        type_in_outline = rewrite_type_using_args(arg, type_in_outline, TL::ObjectList<TL::Symbol>());

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
                else if (!it->get_type().is_array())
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

            // Make sure we explicitly pass template arguments to the
            // unpacked function
            Nodecl::NodeclBase function_form;
            if (unpacked_function.get_type().is_template_specialized_type())
            {
                function_form = Nodecl::CxxFunctionFormTemplateId::make();
                function_form.set_template_parameters(
                    unpacked_function.get_type()
                        .template_specialized_type_get_template_arguments());
            }

            Nodecl::NodeclBase call_to_unpacked
                = Nodecl::ExpressionStatement::make(Nodecl::FunctionCall::make(
                    unpacked_function.make_nodecl(/* set_ref_type */ true),
                    args,
                    /* alternate_name */ Nodecl::NodeclBase::null(),
                    function_form,
                    get_void_type()));

            outline_empty_stmt.prepend_sibling(call_to_unpacked);
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            fortran_add_types(outline_inside_scope);

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

            Nodecl::List deallocate_exprs;
            for (TL::ObjectList<TL::Symbol>::iterator it = captured_value.begin();
                    it != captured_value.end();
                    it++)
            {
                ERROR_CONDITION(field_map.find(*it) == field_map.end(), "Symbol is not mapped", 0);
                TL::Symbol field = field_map[*it];

                forwarded_parameter_names.append(field.get_name());
                forwarded_parameter_types.append(field.get_type().get_lvalue_reference_to());

                Nodecl::NodeclBase class_member_access =  Nodecl::ClassMemberAccess::make(
                        arg.make_nodecl(/* set_ref_type */ true),
                        field_map[*it].make_nodecl(),
                        /* member_literal */ Nodecl::NodeclBase::null(),
                        field_map[*it].get_type().get_lvalue_reference_to());
                args.append(class_member_access);

                // Finally, if the current symbol is allocatable, we have to deallocate it.
                // Note that this copy was created when we captured its value.
                if (symbol_entity_specs_get_is_allocatable(it->get_internal_symbol()))
                {
                    deallocate_exprs.append(class_member_access.shallow_copy());
                }
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

            if(!deallocate_exprs.is_null())
                outline_empty_stmt.append_sibling(Nodecl::FortranDeallocateStatement::make(deallocate_exprs, nodecl_null()));

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
        else
        {
            internal_error("Code unreachable", 0);
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

        Nodecl::Utils::append_to_top_level_nodecl(outline_function_code);
    }

    void TaskProperties::register_linear_dependence(
        TL::DataReference &data_ref,
        TL::Symbol handler,
        TL::Symbol arg,
        TL::Symbol register_fun,
        TL::Scope scope,
        const TL::ObjectList<TL::Symbol> &local,
        Nodecl::List &register_statements)
    {
        if (!data_ref.is_multireference())
        {
            Nodecl::NodeclBase conv;
            Nodecl::NodeclBase base_addr = Nodecl::Add::make(
                conv = Nodecl::Conversion::make(
                    data_ref.get_base_address().shallow_copy(),
                    TL::Type::get_char_type().get_pointer_to()),
                data_ref.get_offsetof_dependence().shallow_copy(),
                TL::Type::get_char_type().get_pointer_to());
            conv.set_text("C");

            base_addr = rewrite_expression_using_args(arg, base_addr, local);

            TL::Type data_type = data_ref.get_data_type();

            /// void nanos_register_XXX_dep(void *handler, void *start, size_t
            /// length);

            Nodecl::List arg_list;
            // handler
            arg_list.append(Nodecl::Conversion::make(
                handler.make_nodecl(/* set_ref_type */ true),
                handler.get_type()));
            // start
            arg_list.append(Nodecl::Conversion::make(
                base_addr, TL::Type::get_void_type().get_pointer_to()));

            // length
            if (data_type.depends_on_nonconstant_values()
                || data_type.is_incomplete())
            {
                arg_list.append(rewrite_expression_using_args(
                    arg, data_ref.get_sizeof().shallow_copy(), local));
            }
            else if (data_type.is_dependent())
            {
                arg_list.append(data_ref.get_sizeof().shallow_copy());
            }
            else
            {
                arg_list.append(const_value_to_nodecl_with_basic_type(
                    const_value_get_integer(data_type.get_size(),
                                            type_get_size(get_size_t_type()),
                                            /* sign */ 0),
                    get_size_t_type()));
            }

            Nodecl::NodeclBase function_call
                = Nodecl::ExpressionStatement::make(Nodecl::FunctionCall::make(
                    register_fun.make_nodecl(/* set_ref_type */ true),
                    arg_list,
                    /* alternate-symbol */ Nodecl::NodeclBase::null(),
                    /* function-form */ Nodecl::NodeclBase::null(),
                    TL::Type::get_void_type()));

            register_statements.append(function_call);
        }
        else
        {
            TL::ObjectList<TL::DataReference::MultiRefIterator> multireferences = data_ref.multireferences();

            Nodecl::Utils::SimpleSymbolMap symbol_map;
            TL::ObjectList<TL::Symbol> current_locals;
            TL::Counter &ctr = TL::CounterManager::get_counter("nanos6-multideps");
            for (TL::ObjectList<TL::DataReference::MultiRefIterator>::iterator it = multireferences.begin();
                    it != multireferences.end();
                    it++)
            {
                std::stringstream ss;
                ss << it->first.get_name() << "_tmp_" << (int)ctr;
                ctr++;
                std::string ind_var_name = ss.str();

                TL::Symbol local_sym = scope.new_symbol(ind_var_name);
                local_sym.get_internal_symbol()->kind = SK_VARIABLE;
                local_sym.get_internal_symbol()->type_information =
                    ::get_signed_int_type();
                symbol_entity_specs_set_is_user_declared(local_sym.get_internal_symbol(),
                        1);

                symbol_map.add_map(it->first, local_sym);
                current_locals.append(local_sym);

                CXX_LANGUAGE()
                {
                    register_statements.append(
                            Nodecl::CxxDef::make(Nodecl::NodeclBase::null(),
                                local_sym));
                }
            }

            Source src;
            for (TL::ObjectList<TL::DataReference::MultiRefIterator>::iterator it = multireferences.begin();
                    it != multireferences.end();
                    it++)
            {
                ERROR_CONDITION(
                    !it->second.is<Nodecl::Range>(), "Invalid Node", 0);
                Nodecl::Range range = it->second.as<Nodecl::Range>();
                // expression :
                // NODECL_MULTI_EXPRESSION([range]multi-expr-range-expression,
                // [base]expression) symbol type const-value-opt
                Source lower, upper, stride;
                lower << as_expression(rewrite_expression_using_args(arg,
                            range.get_lower(), current_locals));
                upper << as_expression(rewrite_expression_using_args(arg,
                            range.get_upper(), current_locals));
                stride << as_expression(rewrite_expression_using_args(arg,
                            range.get_stride(), current_locals));

                TL::Source ind_var;
                ind_var << as_symbol(symbol_map.map(it->first));
                src << "for (" << ind_var << " = " << lower << "; "
                    << ind_var << " <= " << upper << "; " << ind_var
                    << " += " << stride << ") {";
            }
            Nodecl::NodeclBase body_of_loop;
            src << statement_placeholder(body_of_loop);

            for (TL::ObjectList<TL::DataReference::MultiRefIterator>::iterator it = multireferences.begin();
                    it != multireferences.end();
                    it++)
            {
                src << "}";
            }

            Nodecl::NodeclBase loop = src.parse_statement(scope);


            Nodecl::NodeclBase base_exp = data_ref;
            while (base_exp.is<Nodecl::MultiExpression>())
                base_exp = base_exp.as<Nodecl::MultiExpression>().get_base();

            TL::Scope scope_of_body_of_loop = body_of_loop.retrieve_context();
            base_exp = Nodecl::Utils::deep_copy(
                base_exp, scope_of_body_of_loop, symbol_map);

            TL::DataReference base_data_ref = base_exp;
            Nodecl::List base_reg;
            register_linear_dependence(base_data_ref,
                                       handler,
                                       arg,
                                       register_fun,
                                       scope,
                                       current_locals,
                                       base_reg);

            body_of_loop.replace(base_reg);

            register_statements.append(loop);
        }
    }

    void TaskProperties::register_fortran_linear_dependence(
        TL::DataReference &data_ref,
        TL::Symbol handler,
        Nodecl::Utils::SymbolMap &symbol_map,
        TL::Symbol register_fun,
        Nodecl::List &register_statements)
    {
        Nodecl::NodeclBase base_addr
            = data_ref.get_base_address().shallow_copy();

        base_addr = Nodecl::Utils::deep_copy(
            Nodecl::Add::make(
                Nodecl::Conversion::make(
                    base_addr, TL::Type::get_void_type().get_pointer_to()),
                data_ref.get_offsetof_dependence(),
                TL::Type::get_char_type().get_pointer_to()),
            TL::Scope::get_global_scope(),
            symbol_map);

        TL::Type data_type = data_ref.get_data_type();

        /// void nanos_register_XXX_dep(void *handler, void *start, size_t
        /// length);

        Nodecl::List arg_list;
        // handler
        arg_list.append(Nodecl::Conversion::make(
            handler.make_nodecl(/* set_ref_type */ true), handler.get_type()));
        // start
        arg_list.append(Nodecl::Conversion::make(
            base_addr, TL::Type::get_void_type().get_pointer_to()));

        // length
        if (data_type.depends_on_nonconstant_values()
            || data_type.is_incomplete())
        {
            arg_list.append(
                Nodecl::Utils::deep_copy(data_ref.get_sizeof(),
                                         TL::Scope::get_global_scope(),
                                         symbol_map));
        }
        else if (data_type.is_dependent())
        {
            arg_list.append(data_ref.get_sizeof().shallow_copy());
        }
        else
        {
            arg_list.append(const_value_to_nodecl_with_basic_type(
                const_value_get_integer(data_type.get_size(),
                                        type_get_size(get_size_t_type()),
                                        /* sign */ 0),
                get_size_t_type()));
        }

        Nodecl::NodeclBase function_call
            = Nodecl::ExpressionStatement::make(Nodecl::FunctionCall::make(
                register_fun.make_nodecl(/* set_ref_type */ true),
                arg_list,
                /* alternate-symbol */ Nodecl::NodeclBase::null(),
                /* function-form */ Nodecl::NodeclBase::null(),
                TL::Type::get_void_type()));

        register_statements.append(function_call);
    }

    namespace
    {

    bool same_value(const_value_t *v1, const_value_t *v2)
    {
        if (v1 == NULL || v2 == NULL)
            return false;
        return const_value_is_nonzero(const_value_eq(v1, v2));
    }

    bool structurally_equal_bounds(Nodecl::NodeclBase n1, Nodecl::NodeclBase n2)
    {
        if (n1.is<Nodecl::Symbol>() && n1.get_symbol().is_saved_expression())
            n1 = n1.get_symbol().get_value();
        if (n2.is<Nodecl::Symbol>() && n2.get_symbol().is_saved_expression())
            n2 = n2.get_symbol().get_value();

        return same_value(n1.get_constant(), n2.get_constant())
               || Nodecl::Utils::structurally_equal_nodecls(n1, n2);
    }

    bool is_contiguous_region_(TL::Type data_type, int n)
    {
        if (n == 0)
        {
            ERROR_CONDITION(!data_type.array_is_region(),
                            "We expect an array region here",
                            0);
            return is_contiguous_region_(data_type.array_element(), n + 1);
        }
        else
        {
            if (data_type.is_array() && data_type.array_is_region())
            {
                // If it is a region it might designate the whole array
                Nodecl::NodeclBase lower_bound;
                Nodecl::NodeclBase upper_bound;
                data_type.array_get_bounds(lower_bound, upper_bound);

                Nodecl::NodeclBase lower_region_bound;
                Nodecl::NodeclBase upper_region_bound;
                data_type.array_get_region_bounds(lower_region_bound,
                                                  upper_region_bound);
                /* whole array a[0:N-1] */
                if (structurally_equal_bounds(lower_bound, lower_region_bound) &&
                        structurally_equal_bounds(upper_bound, upper_region_bound))
                    return is_contiguous_region_(data_type.array_element(), n + 1);

                return 0;
            }
            else
                return 1;
        }
    }

    bool is_single_item(TL::Type data_type)
    {
        if (data_type.is_array() && data_type.array_is_region())
        {
            Nodecl::NodeclBase lower_region_bound;
            Nodecl::NodeclBase upper_region_bound;
            data_type.array_get_region_bounds(lower_region_bound,
                                              upper_region_bound);

            if (/* a[r] expanded into a[r:r] */
                structurally_equal_bounds(lower_region_bound,
                                          upper_region_bound))
                return is_single_item(data_type.array_element());
            return 0;
        }
        else
            return 1;
    }

    bool is_contiguous_region(TL::DataReference &data_ref)
    {
        return is_contiguous_region_(data_ref.get_data_type(), 0)
               || is_single_item(data_ref.get_data_type());
    }
    }

    void TaskProperties::register_region_dependence(
        TL::DataReference &data_ref,
        TL::Symbol handler,
        TL::Symbol arg,
        TL::Symbol register_fun,
        TL::Scope scope,
        const TL::ObjectList<TL::Symbol> &local,
        Nodecl::List &register_statements)
    {
        if (!is_contiguous_region(data_ref))
        {
            register_noncontiguous_region_dependence(
                data_ref, handler, arg, register_fun, scope, local,
                register_statements);
        }
        else
        {
            register_linear_dependence(
                data_ref, handler, arg, register_fun, scope, local,
                register_statements);
        }
    }

    void TaskProperties::register_noncontiguous_region_dependence(
        TL::DataReference &data_ref,
        TL::Symbol handler,
        TL::Symbol arg,
        TL::Symbol register_fun,
        TL::Scope scope,
        const TL::ObjectList<TL::Symbol> &local,
        Nodecl::List &register_statements)
    {
        ERROR_CONDITION(
            !data_ref.is<Nodecl::ArraySubscript>(), "Invalid node", 0);
        Nodecl::ArraySubscript arr = data_ref.as<Nodecl::ArraySubscript>();
        Nodecl::List subscripts = arr.get_subscripts().as<Nodecl::List>();
        ERROR_CONDITION(subscripts.size() <= 1, "Invalid subcript list", 0);

        TL::ObjectList<TL::Symbol> ind_vars;
        TL::ObjectList<Nodecl::Range> ranges;

        // For each Range node we create a new variable that will be used as
        // the induction variable of a new loop. Note that we skip the last
        // subscript since it is always contiguous
        for (Nodecl::List::iterator it = subscripts.begin();
             it + 1 != subscripts.end();
             it++)
        {
            if (it->is<Nodecl::Range>())
            {
                TL::Counter &ctr
                    = TL::CounterManager::get_counter("nanos6-noncontiguous");
                std::stringstream ss;
                ss << "x_" << (int)ctr;
                ctr++;
                std::string ind_var_name = ss.str();

                TL::Symbol sym = scope.new_symbol(ind_var_name);
                sym.get_internal_symbol()->kind = SK_VARIABLE;
                sym.get_internal_symbol()->type_information = get_signed_int_type();
                symbol_entity_specs_set_is_user_declared(sym.get_internal_symbol(), 1);
                ind_vars.append(sym);
                ranges.append(it->as<Nodecl::Range>());

                if (IS_CXX_LANGUAGE)
                {
                    register_statements.append(
                            Nodecl::CxxDef::make(/*context*/ nodecl_null(), sym));
                }
            }
        }

        // For each induction variable we create a new loop where the
        // lower and upper values are obtained from the Range node
        TL::Source src;
        for (size_t i = 0; i < ind_vars.size(); i++)
        {
            Source lower, upper, stride;
            Nodecl::Range& range = ranges[i];
            lower << as_expression(rewrite_expression_using_args(
                        arg, range.get_lower(), ind_vars));
            upper << as_expression(rewrite_expression_using_args(
                        arg, range.get_upper(), ind_vars));
            stride << as_expression(rewrite_expression_using_args(
                        arg, range.get_stride(), ind_vars));

            std::string name = ind_vars[i].get_name();
            src << "for (" << name << " = " << lower << "; " << name
                << " <= " << upper << "; " << name << " += " << stride << ") {"
                ;
        }

        Nodecl::NodeclBase body_of_loop;
        src << statement_placeholder(body_of_loop);

        for (size_t i = 0; i < ind_vars.size(); i++)
        {
            src << "}";
        }

        Nodecl::NodeclBase loop = src.parse_statement(scope);

        Nodecl::NodeclBase new_data_ref = data_ref.shallow_copy();
        Nodecl::List new_subscripts = new_data_ref.as<Nodecl::ArraySubscript>()
                                          .get_subscripts()
                                          .as<Nodecl::List>();

        // Finally, we update the subscripts of the copy of the original
        // dependence. Basically, we replace the lower and upper bounds of each
        // Range
        size_t range_count = 0;
        for (Nodecl::List::iterator it = new_subscripts.begin();
             it + 1 != new_subscripts.end();
             it++)
        {
            if (it->is<Nodecl::Range>())
            {
                TL::Symbol&  ind_var = ind_vars[range_count++];

                Nodecl::Range r = it->as<Nodecl::Range>();
                r.get_lower().replace(ind_var.make_nodecl(/* set ref */ true));
                r.get_upper().replace(ind_var.make_nodecl(/* set ref */ true));
            }
        }

        Nodecl::List linear_reg;
        TL::DataReference updated_data_ref(new_data_ref);
        register_linear_dependence(updated_data_ref,
                                   handler,
                                   arg,
                                   register_fun,
                                   scope,
                                   ind_vars,
                                   linear_reg);
        body_of_loop.replace(linear_reg);

        register_statements.append(loop);
    }


    void TaskProperties::register_fortran_region_dependence(
        TL::DataReference &data_ref,
        TL::Symbol handler,
        Nodecl::Utils::SymbolMap &symbol_map,
        TL::Symbol register_fun,
        Nodecl::List &register_statements)
    {
        if (!is_contiguous_region(data_ref))
        {
            error_printf_at(
                data_ref.get_locus(),
                "dependence '%s' has a region not known to be contiguous, this "
                "is not supported yet\n",
                data_ref.prettyprint().c_str());
            return;
        }

        // This is a contiguous region, so it can be easily mapped onto a lineal
        // one
        return register_fortran_linear_dependence(
            data_ref, handler, symbol_map, register_fun, register_statements);
    }

    void TaskProperties::register_dependence_for_array(
            TL::DataReference& data_ref,
            TL::Symbol handler,
            TL::Symbol arg,
            TL::Symbol register_fun,
            TL::Scope scope,
            const TL::ObjectList<TL::Symbol> &local,
            Nodecl::List& register_statements)
    {
        TL::Type data_type = data_ref.get_data_type();

        ERROR_CONDITION(!data_type.is_array(), "Invalid data type here", 0);
        if (data_type.array_is_region())
        {
            register_region_dependence(
                data_ref, handler, arg, register_fun, scope, local,
                register_statements);
        }
        else
        {
            register_linear_dependence(
                data_ref, handler, arg, register_fun, scope, local,
                register_statements);
        }
    }

    void TaskProperties::register_fortran_dependence_for_array(
        TL::DataReference &data_ref,
        TL::Symbol handler,
        Nodecl::Utils::SymbolMap &symbol_map,
        TL::Symbol register_fun,
        Nodecl::List &register_statements)
    {
        TL::Type data_type = data_ref.get_data_type();

        ERROR_CONDITION(!data_type.is_array(), "Invalid data type here", 0);
        if (data_type.array_is_region())
        {
            register_fortran_region_dependence(data_ref,
                                               handler,
                                               symbol_map,
                                               register_fun,
                                               register_statements);
        }
        else
        {
            register_fortran_linear_dependence(data_ref,
                                               handler,
                                               symbol_map,
                                               register_fun,
                                               register_statements);
        }
    }

    void TaskProperties::create_dependences_function_c()
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

        Nodecl::NodeclBase dependences_function_code, dependences_empty_stmt;
        SymbolUtils::build_empty_body_for_function(
                dependences_function,
                dependences_function_code,
                dependences_empty_stmt);

        TL::Scope dependences_inside_scope = dependences_empty_stmt.retrieve_context();
        TL::Symbol handler = dependences_inside_scope.get_symbol_from_name("handler");
        ERROR_CONDITION(!handler.is_valid(), "Invalid symbol", 0);
        TL::Symbol arg = dependences_inside_scope.get_symbol_from_name("arg");
        ERROR_CONDITION(!arg.is_valid(), "Invalid symbol", 0);

        TL::Scope global_context = TL::Scope::get_global_scope();

        struct DependencesSet
        {
            TL::ObjectList<Nodecl::NodeclBase> &dep_list;
            std::string func_name;
        } deps[] = {
            { dep_in, "nanos_register_read_depinfo" },
            { dep_out, "nanos_register_write_depinfo" },
            { dep_inout, "nanos_register_readwrite_depinfo" },

            { dep_weakin, "nanos_register_weak_read_depinfo" },
            { dep_weakout, "nanos_register_weak_write_depinfo" },
            { dep_weakinout, "nanos_register_weak_readwrite_depinfo" },

            { dep_commutative, "nanos_register_commutative_depinfo" },
        };

        for (DependencesSet *dep_set = deps;
                dep_set != (DependencesSet*)(&deps + 1);
                dep_set++)
        {
            TL::ObjectList<Nodecl::NodeclBase> &dep_list = dep_set->dep_list;

            if (dep_list.empty())
                continue;

            TL::Symbol register_fun = global_context.get_symbol_from_name(dep_set->func_name);
            if (!register_fun.is_valid())
            {
                fatal_error("Function '%s' not found while trying to register dependences of its kind\n",
                        dep_set->func_name.c_str());
            }

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
                            register_fun,
                            dependences_inside_scope,
                            /* Local */ TL::ObjectList<TL::Symbol>(),
                            register_statements);
                }
                else
                {
                    register_dependence_for_array(
                            data_ref,
                            handler,
                            arg,
                            register_fun,
                            dependences_inside_scope,
                            /* Local */ TL::ObjectList<TL::Symbol>(),
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

    void TaskProperties::create_dependences_function_fortran_proper()
    {
        // This is similar to the dep_fun function
        TL::ObjectList<std::string> unpack_parameter_names;
        TL::ObjectList<TL::Type> unpack_parameter_types;
        std::map<TL::Symbol, std::string> symbols_to_param_names;

        std::string dep_fun_name;
        {
            TL::Counter &counter
                = TL::CounterManager::get_counter("nanos6-outline");
            std::stringstream ss;
            ss << "nanos6_dep_1_" << (int)counter;
            counter++;
            dep_fun_name = ss.str();
        }

        unpack_parameter_names.append("handler");
        unpack_parameter_types.append(
            TL::Type::get_void_type().get_pointer_to());
        captured_value.map(AddParameter(unpack_parameter_names,
                                        unpack_parameter_types,
                                        symbols_to_param_names));
        shared.map(AddParameter(unpack_parameter_names,
                                unpack_parameter_types,
                                symbols_to_param_names));

        dependences_function
            = SymbolUtils::new_function_symbol(related_function,
                                               dep_fun_name,
                                               TL::Type::get_void_type(),
                                               unpack_parameter_names,
                                               unpack_parameter_types);

        Nodecl::NodeclBase dep_fun_function_code, dep_fun_empty_stmt;
        SymbolUtils::build_empty_body_for_function(
            dependences_function, dep_fun_function_code, dep_fun_empty_stmt);

        TL::Scope dep_fun_inside_scope = dep_fun_empty_stmt.retrieve_context();

        fortran_add_types(dep_fun_inside_scope);

        // Prepare deep copy and remember those parameters that need fixup
        Nodecl::Utils::SimpleSymbolMap symbol_map;

        ParameterToSymbol param_to_symbol(dep_fun_inside_scope);

        TL::ObjectList<TL::Symbol> parameters_to_update_type;
        captured_value.map(MapSymbols(param_to_symbol,
                                      symbols_to_param_names,
                                      /* out */ symbol_map,
                                      /* out */ parameters_to_update_type));
        shared.map(MapSymbols(param_to_symbol,
                              symbols_to_param_names,
                              /* out */ symbol_map,
                              /* out */ parameters_to_update_type));

        // Now fix the types of runtime sized types prior anything else
        for (TL::ObjectList<TL::Symbol>::iterator it
             = parameters_to_update_type.begin();
             it != parameters_to_update_type.end();
             it++)
        {
            it->set_type(rewrite_type_for_outline(
                it->get_type(), dep_fun_inside_scope, symbol_map));
        }

        if (!parameters_to_update_type.empty())
        {
            // Sync the function type if needed
            TL::ObjectList<TL::Type> updated_param_types
                = unpack_parameter_names.map<TL::Symbol>(param_to_symbol)
                      .map<TL::Type>(&TL::Symbol::get_type);
            dependences_function.set_type(
                TL::Type::get_void_type().get_function_returning(
                    updated_param_types));
        }

        TL::Symbol handler
            = dep_fun_inside_scope.get_symbol_from_name("handler");
        ERROR_CONDITION(!handler.is_valid(), "Invalid symbol", 0);

        TL::Scope global_context = TL::Scope::get_global_scope();

        struct DependencesSet
        {
            TL::ObjectList<Nodecl::NodeclBase> &dep_list;
            std::string func_name;
        } deps[] = {
            { dep_in, "nanos_register_read_depinfo" },
            { dep_out, "nanos_register_write_depinfo" },
            { dep_inout, "nanos_register_readwrite_depinfo" },

            { dep_weakin, "nanos_register_weak_read_depinfo" },
            { dep_weakout, "nanos_register_weak_write_depinfo" },
            { dep_weakinout, "nanos_register_weak_readwrite_depinfo" },

            { dep_commutative, "nanos_register_commutative_depinfo" },
        };

        for (DependencesSet *dep_set = deps;
             dep_set != (DependencesSet *)(&deps + 1);
             dep_set++)
        {
            TL::ObjectList<Nodecl::NodeclBase> &dep_list = dep_set->dep_list;

            if (dep_list.empty())
                continue;

            TL::Symbol register_fun
                = global_context.get_symbol_from_name(dep_set->func_name);
            if (!register_fun.is_valid())
            {
                fatal_error(
                    "Function '%s' not found while trying to register "
                    "dependences of its kind\n",
                    dep_set->func_name.c_str());
            }

            for (TL::ObjectList<Nodecl::NodeclBase>::iterator it
                 = dep_list.begin();
                 it != dep_list.end();
                 it++)
            {
                TL::DataReference data_ref = *it;
                TL::Type data_type = data_ref.get_data_type();

                Nodecl::List register_statements;
                if (!data_type.is_array())
                {
                    register_fortran_linear_dependence(data_ref,
                                                       handler,
                                                       symbol_map,
                                                       register_fun,
                                                       register_statements);
                }
                else
                {
                    register_fortran_dependence_for_array(data_ref,
                                                          handler,
                                                          symbol_map,
                                                          register_fun,
                                                          register_statements);
                }

                dep_fun_empty_stmt.prepend_sibling(register_statements);
            }
        }

        Nodecl::Utils::append_to_enclosing_top_level_location(
            task_body, dep_fun_function_code);
    }

    void TaskProperties::create_dependences_function_fortran_forward()
    {
        /* ===================== */
        /* Dependences function that simply calls the forward function */
        /* ===================== */
        TL::ObjectList<std::string> dep_parameter_names(2);
        dep_parameter_names[0] = "handler";
        dep_parameter_names[1] = "arg";
        TL::ObjectList<TL::Type> dep_parameter_types(2);
        dep_parameter_types[0] = TL::Type::get_void_type().get_pointer_to();
        dep_parameter_types[1] = info_structure.get_lvalue_reference_to();

        std::string dep_name;
        {
            TL::Counter &counter
                = TL::CounterManager::get_counter("nanos6-outline");
            std::stringstream ss;
            ss << "nanos6_dep_0_" << (int)counter;
            counter++;
            dep_name = ss.str();
        }

        TL::Symbol proper_dependences_function = dependences_function;

        dependences_function
            = SymbolUtils::new_function_symbol(related_function,
                                               dep_name,
                                               TL::Type::get_void_type(),
                                               dep_parameter_names,
                                               dep_parameter_types);

        Nodecl::NodeclBase dependences_function_code, dependences_empty_stmt;
        SymbolUtils::build_empty_body_for_function(dependences_function,
                                                   dependences_function_code,
                                                   dependences_empty_stmt);

        TL::Scope dependences_inside_scope
            = dependences_empty_stmt.retrieve_context();
        TL::Symbol handler
            = dependences_inside_scope.get_symbol_from_name("handler");
        ERROR_CONDITION(!handler.is_valid(), "Invalid symbol", 0);
        TL::Symbol arg = dependences_inside_scope.get_symbol_from_name("arg");
        ERROR_CONDITION(!arg.is_valid(), "Invalid symbol", 0);

        Nodecl::Utils::append_to_enclosing_top_level_location(
            task_body, dependences_function_code);

        fortran_add_types(dependences_inside_scope);

        /* ===================== */
        /* Dependences function that simply calls the forward function */
        /* Fortran side */
        /* ===================== */
        std::string forwarded_name;
        {
            TL::Counter &counter
                = TL::CounterManager::get_counter("nanos6-outline");
            std::stringstream ss;
            ss << "nanos6_fwd_dep_" << (int)counter;
            counter++;
            forwarded_name = ss.str();
        }

        TL::ObjectList<std::string> forwarded_parameter_names;
        TL::ObjectList<TL::Type> forwarded_parameter_types;

        forwarded_parameter_names.append("ol");
        forwarded_parameter_types.append(
            TL::Type::get_void_type().get_pointer_to());
        forwarded_parameter_names.append("handler");
        forwarded_parameter_types.append(
            TL::Type::get_void_type().get_pointer_to());

        Nodecl::List args;

        // ol
        args.append(Nodecl::Reference::make(
            proper_dependences_function.make_nodecl(/* set_ref_type */ true),
            proper_dependences_function.get_type().get_pointer_to()));
        // handler
        args.append(handler.make_nodecl(/* set_ref_type */ true));

        for (TL::ObjectList<TL::Symbol>::iterator it = captured_value.begin();
             it != captured_value.end();
             it++)
        {
            ERROR_CONDITION(field_map.find(*it) == field_map.end(),
                            "Symbol is not mapped",
                            0);
            TL::Symbol field = field_map[*it];

            forwarded_parameter_names.append(field.get_name());
            forwarded_parameter_types.append(
                field.get_type().get_lvalue_reference_to());

            args.append(Nodecl::ClassMemberAccess::make(
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
            ERROR_CONDITION(field_map.find(*it) == field_map.end(),
                            "Symbol is not mapped",
                            0);
            TL::Symbol field = field_map[*it];

            forwarded_parameter_names.append(field.get_name());
            forwarded_parameter_types.append(field.get_type());

            args.append(Nodecl::ClassMemberAccess::make(
                // Nodecl::Dereference::make(
                arg.make_nodecl(/* set_ref_type */ true),
                //    arg.get_type().points_to().get_lvalue_reference_to()),
                field_map[*it].make_nodecl(),
                /* member_literal */ Nodecl::NodeclBase::null(),
                field_map[*it].get_type().get_lvalue_reference_to()));
        }

        TL::Symbol forwarded_function
            = SymbolUtils::new_function_symbol(TL::Scope::get_global_scope(),
                                               forwarded_name,
                                               TL::Type::get_void_type(),
                                               forwarded_parameter_names,
                                               forwarded_parameter_types);
        // Make this symbol global
        symbol_entity_specs_set_is_static(
            forwarded_function.get_internal_symbol(), 0);

        Nodecl::NodeclBase call_to_forward
            = Nodecl::ExpressionStatement::make(Nodecl::FunctionCall::make(
                forwarded_function.make_nodecl(/* set_ref_type */ true),
                args,
                /* alternate_name */ Nodecl::NodeclBase::null(),
                /* function_form */ Nodecl::NodeclBase::null(),
                get_void_type()));

        dependences_empty_stmt.prepend_sibling(call_to_forward);

        /* ===================== */
        /* Dependences function that simply calls the forward function */
        /* C side */
        /* ===================== */
        std::string c_forwarded_name
            = ::fortran_mangle_symbol(forwarded_function.get_internal_symbol());

        // Now generate the C counterpart
        TL::ObjectList<std::string> c_forwarded_parameter_names(
            forwarded_parameter_names.size(), "");

        c_forwarded_parameter_names[0] = "ol";

        GenerateParamsNames generate_params_names;
        std::generate(c_forwarded_parameter_names.begin() + 1,
                      c_forwarded_parameter_names.end(),
                      generate_params_names);

        TL::ObjectList<TL::Type> c_forwarded_parameter_types(
            forwarded_parameter_types.size(),
            TL::Type::get_void_type().get_pointer_to());
        c_forwarded_parameter_types[0]
            = TL::Type::get_void_type().get_function_returning(
                TL::ObjectList<TL::Type>(
                    forwarded_parameter_types.size() - 1,
                    TL::Type::get_void_type().get_pointer_to()));

        TL::Symbol c_forwarded_function
            = SymbolUtils::new_function_symbol(TL::Scope::get_global_scope(),
                                               c_forwarded_name,
                                               TL::Type::get_void_type(),
                                               c_forwarded_parameter_names,
                                               c_forwarded_parameter_types);
        symbol_entity_specs_set_is_static(
            c_forwarded_function.get_internal_symbol(), 0);

        Nodecl::NodeclBase c_forwarded_function_code, c_forwarded_empty_stmt;
        SymbolUtils::build_empty_body_for_function(c_forwarded_function,
                                                   c_forwarded_function_code,
                                                   c_forwarded_empty_stmt);

        SolveParamNames solve_param_names(
            c_forwarded_empty_stmt.retrieve_context());

        TL::ObjectList<Nodecl::NodeclBase> refs_to_params
            = c_forwarded_parameter_names.map<Nodecl::NodeclBase>(
                solve_param_names);

        Nodecl::NodeclBase c_ol_arg = refs_to_params[0];
        Nodecl::List c_args
            = Nodecl::List::make(TL::ObjectList<Nodecl::NodeclBase>(
                refs_to_params.begin() + 1, refs_to_params.end()));

        c_forwarded_empty_stmt.replace(Nodecl::CompoundStatement::make(
            Nodecl::List::make(
                Nodecl::ExpressionStatement::make(Nodecl::FunctionCall::make(
                    c_ol_arg,
                    c_args,
                    /* alternate-symbol */ Nodecl::NodeclBase::null(),
                    /* function-form */ Nodecl::NodeclBase::null(),
                    TL::Type::get_void_type()))),
            /* finalize */ Nodecl::NodeclBase::null()));

        phase->get_extra_c_code().append(c_forwarded_function_code);
    }

    void TaskProperties::create_dependences_function_fortran_mangled()
    {
        // For Fortran we will generate a global function in C, but this
        // requires us to use the proper mangling.
        const char *mangled_name = ::fortran_mangle_symbol(
            dependences_function.get_internal_symbol());

        // Create a detached symbol that looks like it comes from the global
        // scope
        dependences_function_mangled = NEW0(scope_entry_t);
        dependences_function_mangled.get_internal_symbol()->symbol_name
            = mangled_name;
        dependences_function_mangled.get_internal_symbol()->decl_context
            = TL::Scope::get_global_scope().get_decl_context();
        dependences_function_mangled.get_internal_symbol()->kind = SK_FUNCTION;
        // fake type
        dependences_function_mangled.get_internal_symbol()->type_information
            = TL::Type::get_void_type()
                  .get_function_returning(TL::ObjectList<TL::Type>())
                  .get_internal_type();
        symbol_entity_specs_set_is_user_declared(
            dependences_function_mangled.get_internal_symbol(), 1);
    }

    void TaskProperties::create_dependences_function_fortran()
    {
        create_dependences_function_fortran_proper();
        create_dependences_function_fortran_forward();
        create_dependences_function_fortran_mangled();
    }

    void TaskProperties::create_dependences_function()
    {
        // Skip this function if the current task doesn't have any task dependence
        if (!any_task_dependence)
            return;

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            create_dependences_function_c();
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            create_dependences_function_fortran();
        }
    }

    Nodecl::NodeclBase TaskProperties::rewrite_expression_using_args(
            TL::Symbol arg,
            Nodecl::NodeclBase expr,
            const TL::ObjectList<TL::Symbol>& local)
    {
        Nodecl::NodeclBase result = expr.shallow_copy();

        struct RewriteExpression : public Nodecl::ExhaustiveVisitor<void>
        {
            TL::Symbol arg;
            field_map_t &field_map;
            const TL::ObjectList<TL::Symbol>& shared;
            const TL::ObjectList<TL::Symbol> local;

            RewriteExpression(TL::Symbol arg_,
                    field_map_t& field_map_,
                    const TL::ObjectList<TL::Symbol> &shared_,
                    const TL::ObjectList<TL::Symbol> &local_)
                : arg(arg_), field_map(field_map_), shared(shared_), local(local_)
            {
            }

            virtual void visit(const Nodecl::Symbol& node)
            {
                TL::Symbol sym = node.get_symbol();

                // Ignoring local symbols
                if (local.contains(sym))
                    return;

                // Ignoring symbols that are not variables
                if (!sym.is_variable())
                    return;

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

            virtual void visit(const Nodecl::ClassMemberAccess &node)
            {
                walk(node.get_lhs());
            }
        };

        RewriteExpression r(arg, field_map, shared, local);
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

    TL::Type TaskProperties::rewrite_type_using_args(TL::Symbol arg, TL::Type t,
            const TL::ObjectList<TL::Symbol> &local)
    {
        if (t.is_array())
        {
            TL::Type elem_type = rewrite_type_using_args(arg, t.array_element(), local);
            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            {
                Nodecl::NodeclBase new_size = t.array_get_size();
                new_size = rewrite_expression_using_args(arg, new_size, local);

                return elem_type.get_array_to(new_size, TL::Scope::get_global_scope());
            }
            else if (IS_FORTRAN_LANGUAGE)
            {
                Nodecl::NodeclBase new_lower, new_upper;
                t.array_get_bounds(new_lower, new_upper);

                if (is_saved_expression(new_lower))
                {
                    new_lower = rewrite_expression_using_args(arg, new_lower, local);
                }
                if (is_saved_expression(new_upper))
                {
                    new_upper = rewrite_expression_using_args(arg, new_upper, local);
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
            return rewrite_type_using_args(arg, t.no_ref(), local).get_lvalue_reference_to();
        }
        else if (t.is_rvalue_reference())
        {
            return rewrite_type_using_args(arg, t.no_ref(), local).get_rvalue_reference_to();
        }
        else if (t.is_pointer())
        {
            return rewrite_type_using_args(arg, t.points_to(), local)
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

    void TaskProperties::create_cost_function()
    {
        if (cost.is_null())
            return;

        TL::ObjectList<std::string> parameter_names(1);
        parameter_names[0] = "arg";
        TL::ObjectList<TL::Type> parameter_types(1);
        parameter_types[0] = info_structure.get_lvalue_reference_to();

        std::string cost_name;
        {
            TL::Counter &counter
                = TL::CounterManager::get_counter("nanos6-outline");
            std::stringstream ss;
            ss << "nanos6_cost_" << (int)counter;
            counter++;
            cost_name = ss.str();
        }

        cost_function
            = SymbolUtils::new_function_symbol(related_function,
                                               cost_name,
                                               TL::Type::get_size_t_type(),
                                               parameter_names,
                                               parameter_types);

        Nodecl::NodeclBase cost_function_code, cost_empty_stmt;
        SymbolUtils::build_empty_body_for_function(
            cost_function, cost_function_code, cost_empty_stmt);

        TL::Scope scope_inside_cost = cost_empty_stmt.retrieve_context();
        TL::Symbol arg = scope_inside_cost.get_symbol_from_name("arg");
        ERROR_CONDITION(!arg.is_valid(), "Invalid symbol", 0);

        Nodecl::NodeclBase computed_cost
            = rewrite_expression_using_args(arg, cost, /* locals */
                    TL::ObjectList<TL::Symbol>());

        if (!computed_cost.get_type().is_same_type(TL::Type::get_size_t_type()))
        {
            computed_cost
                = Nodecl::Conversion::make(computed_cost,
                                           TL::Type::get_size_t_type(),
                                           computed_cost.get_locus());
        }

        Nodecl::NodeclBase return_stmt = Nodecl::ReturnStatement::make(
            computed_cost, computed_cost.get_locus());

        cost_empty_stmt.replace(return_stmt);

        Nodecl::Utils::prepend_to_enclosing_top_level_location(
            task_body, cost_function_code);
    }

    void TaskProperties::capture_environment(
            TL::Symbol args,
            /* out */
            Nodecl::NodeclBase& captured_env)
    {
        Nodecl::List captured_list;

        Nodecl::NodeclBase vla_offset;
        // 1. Traversing captured variables (firstprivate + other captures)
        for (TL::ObjectList<TL::Symbol>::iterator it = captured_value.begin();
                it != captured_value.end();
                it++)
        {
            ERROR_CONDITION(field_map.find(*it) == field_map.end(),
                    "Symbol is not mapped", 0);

            if (it->get_type().is_dependent() &&
                    !is_standard_layout_type(it->get_type().no_ref().get_internal_type()))
            {
                error_printf_at(locus_of_task_creation,
                        "capture of symbol '%s' with non-standard layout type is not supported\n",
                        it->get_qualified_name().c_str());
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

            Nodecl::List current_captured_stmts;
            if (!it->get_type().no_ref().is_array()
                    && !it->get_type().no_ref().is_function())
            {
                Nodecl::NodeclBase rhs = it->make_nodecl(/* set_ref_type */ true);

                Nodecl::NodeclBase assignment_stmt =
                    Nodecl::ExpressionStatement::make(
                            Nodecl::Assignment::make(
                                lhs,
                                rhs,
                                lhs_type));

                current_captured_stmts.append(assignment_stmt);
            }
            else if (it->get_type().no_ref().is_function())
            {
                Nodecl::NodeclBase rhs = Nodecl::Reference::make(
                        it->make_nodecl(/* set_ref_type */ true),
                        it->get_type().no_ref().get_pointer_to());

                Nodecl::NodeclBase assignment_stmt =
                    Nodecl::ExpressionStatement::make(
                            Nodecl::Assignment::make(
                                lhs,
                                rhs,
                                lhs_type));

                current_captured_stmts.append(assignment_stmt);
            }
            else
            {
                if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                {
                    if (it->get_type().depends_on_nonconstant_values())
                    {
                        if (vla_offset.is_null())
                        {
                            // Skipping the arguments structure
                            Nodecl::NodeclBase cast = Nodecl::Conversion::make(
                                    Nodecl::Add::make(
                                        args.make_nodecl(/* ser_ref_type */ true),
                                        /* 1, */ const_value_to_nodecl(const_value_get_signed_int(1)),
                                        args.get_type().no_ref()),
                                    TL::Type::get_char_type().get_pointer_to());

                            cast.set_text("C");
                            vla_offset = cast;
                        }

                        // Skipping the extra space allocated for each vla
                        Nodecl::NodeclBase mask_align =
                            const_value_to_nodecl(const_value_get_signed_int(VLA_OVERALLOCATION_ALIGN - 1));

                        // expr = (size_t)(vla_offset + mask_align)
                        Nodecl::NodeclBase cast_expr;
                        cast_expr = Nodecl::Conversion::make(
                                Nodecl::Add::make(
                                    vla_offset,
                                    mask_align,
                                    vla_offset.get_type()),
                                get_size_t_type());
                        cast_expr.set_text("C");

                        // expr = (void *)((size_t)(vla_offset + mask_align) & ~mask_align)
                        cast_expr = Nodecl::Conversion::make(
                                Nodecl::BitwiseAnd::make(
                                    cast_expr,
                                    Nodecl::BitwiseNot::make(
                                        mask_align.shallow_copy(),
                                        mask_align.get_type()),
                                    get_size_t_type()),
                                TL::Type::get_void_type().get_pointer_to());
                        cast_expr.set_text("C");

                        Nodecl::NodeclBase rhs = cast_expr;
                        Nodecl::NodeclBase assignment_stmt = Nodecl::ExpressionStatement::make(
                                Nodecl::Assignment::make(
                                    lhs.shallow_copy(),
                                    rhs,
                                    lhs_type));

                        current_captured_stmts.append(assignment_stmt);

                        // Compute the offset for the next vla symbol (current member + its size)
                        vla_offset = Nodecl::Conversion::make(
                                Nodecl::Add::make(
                                    lhs.shallow_copy(),
                                    Nodecl::Sizeof::make(
                                        Nodecl::Type::make(it->get_type()),
                                        Nodecl::NodeclBase::null(),
                                        get_size_t_type()),
                                    get_size_t_type()),
                                TL::Type::get_char_type().get_pointer_to());
                        vla_offset.set_text("C");
                    }

                    TL::Symbol builtin_memcpy =
                        TL::Scope::get_global_scope().get_symbol_from_name("__builtin_memcpy");

                    ERROR_CONDITION(!builtin_memcpy.is_valid()
                            || !builtin_memcpy.is_function(), "Invalid symbol", 0);

                    Nodecl::NodeclBase ref_lhs =
                        Nodecl::Reference::make(lhs, lhs_type.no_ref().get_pointer_to());

                    Nodecl::NodeclBase rhs = Nodecl::Conversion::make(
                            it->make_nodecl(/* set_ref_type */ true),
                            it->get_type().no_ref().array_element().get_pointer_to());

                    Nodecl::NodeclBase size_of_array;
                    if (it->get_type().depends_on_nonconstant_values())
                    {
                        size_of_array =
                            Nodecl::Sizeof::make(
                                    Nodecl::Type::make(it->get_type()),
                                    Nodecl::NodeclBase::null(),
                                    get_size_t_type());
                    }
                    else
                    {
                        size_of_array =
                            const_value_to_nodecl_with_basic_type(
                                    const_value_get_signed_int(
                                        it->get_type().no_ref().get_size()),
                                    get_size_t_type());
                    }

                    Nodecl::NodeclBase function_call_stmt = Nodecl::ExpressionStatement::make(
                            Nodecl::FunctionCall::make(
                                builtin_memcpy.make_nodecl(/* set_ref_type */ true),
                                Nodecl::List::make(ref_lhs, rhs, size_of_array),
                                /* alternate-name */ Nodecl::NodeclBase::null(),
                                /* function-form */ Nodecl::NodeclBase::null(),
                                TL::Type::get_void_type().get_pointer_to()));

                    current_captured_stmts.append(function_call_stmt);
                }
                else // IS_FORTRAN_LANGUAGE
                {
                    if (!it->get_type().depends_on_nonconstant_values())
                    {
                        Nodecl::NodeclBase rhs = it->make_nodecl(/* set_ref_type */ true);

                        Nodecl::NodeclBase assignment_stmt =
                            Nodecl::ExpressionStatement::make(
                                    Nodecl::Assignment::make(
                                        lhs,
                                        rhs,
                                        lhs_type));

                        current_captured_stmts.append(assignment_stmt);
                    }
                    else
                    {
                        internal_error("Arrays with nonconstant values are not supported yet.", 0);
                    }
                }
            }

            if (IS_FORTRAN_LANGUAGE
                    && it->is_parameter()
                    && it->is_optional())
            {
                Source conditional_capture_src;

                Nodecl::NodeclBase capture_null =
                    Nodecl::ExpressionStatement::make(
                            Nodecl::Assignment::make(
                                lhs.shallow_copy(),
                                Source("MERCURIUM_NULL()").parse_expression(related_function.get_related_scope()),
                                TL::Type::get_void_type().get_pointer_to()));

                conditional_capture_src
                    << "IF (PRESENT(" << as_symbol(*it) << ")) THEN\n"
                    <<    as_statement(current_captured_stmts)
                    << "ELSE\n"
                    <<    as_statement(capture_null)
                    << "END IF\n"
                    ;

                Nodecl::NodeclBase if_else_stmt =
                    conditional_capture_src.parse_statement(related_function.get_related_scope());

               current_captured_stmts = Nodecl::List::make(if_else_stmt);
            }

            captured_list.append(current_captured_stmts);
        }

        // Since we compute the offsets in advance, once all the capture
        // symbols have been treated we can safely free this tree
        if (!vla_offset.is_null())
            nodecl_free(vla_offset.get_internal_nodecl());

        // 2. Traversing SHARED variables
        for (TL::ObjectList<TL::Symbol>::iterator it = shared.begin();
                it != shared.end();
                it++)
        {
            ERROR_CONDITION(field_map.find(*it) == field_map.end(),
                    "Symbol is not mapped", 0);

            Nodecl::NodeclBase rhs = it->make_nodecl(/* set_ref_type */ true);

            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            {
                if (!it->get_type().is_array())
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
            }
            else if (IS_FORTRAN_LANGUAGE)
            {
                if (it->get_type().no_ref().is_pointer()
                    || it->is_allocatable())
                {
                    TL::Symbol ptr_of_sym = fortran_get_function_ptr_of(
                        *it,
                        related_function.get_related_scope(),
                        phase->get_extra_c_code());

                    rhs = Nodecl::FunctionCall::make(
                        ptr_of_sym.make_nodecl(/* set_ref_type */ true),
                        Nodecl::List::make(
                            it->make_nodecl(/* set_ref_type */ true)),
                        /* alternate_name */ Nodecl::NodeclBase::null(),
                        /* function_form */ Nodecl::NodeclBase::null(),
                        ptr_of_sym.get_type().returns());
                }
                else if (it->get_type().no_ref().is_array()
                         && it->get_type().no_ref().array_requires_descriptor())
                {
                    TL::Symbol descriptor_field = array_descriptor_map[*it];
                    ERROR_CONDITION(!descriptor_field.is_valid(),
                                    "Array descriptor field not found",
                                    0);

                    TL::Symbol copy_function
                        = fortran_get_copy_descriptor_function(
                            /* dest */ descriptor_field,
                            /* source */ *it,
                            related_function.get_related_scope(),
                            phase->get_extra_c_code());

                    Nodecl::NodeclBase dest = Nodecl::ClassMemberAccess::make(
                        Nodecl::Dereference::make(
                            args.make_nodecl(/* set_ref_type */ true),
                            args.get_type()
                                .points_to()
                                .get_lvalue_reference_to()),
                        descriptor_field.make_nodecl(),
                        /* member_literal */ Nodecl::NodeclBase::null(),
                        descriptor_field.get_type().get_lvalue_reference_to());

                    Nodecl::NodeclBase capture_descriptor_stmt
                        = Nodecl::ExpressionStatement::make(
                            Nodecl::FunctionCall::make(
                                copy_function.make_nodecl(/* set_ref */ true),
                                Nodecl::List::make(
                                    dest, it->make_nodecl(/* set_ref */ true)),
                                Nodecl::NodeclBase::null(),
                                Nodecl::NodeclBase::null(),
                                get_void_type()));

                    captured_list.append(capture_descriptor_stmt);

                    rhs = Nodecl::Reference::make(
                        dest.shallow_copy(),
                        descriptor_field.get_type().get_pointer_to());
                }
                else
                {
                    rhs = Nodecl::Reference::make(
                        rhs, rhs.get_type().no_ref().get_pointer_to());
                }
            }
            else
            {
                internal_error("Code unreachable", 0);
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

            Nodecl::NodeclBase current_captured_stmt = Nodecl::ExpressionStatement::make(
                    Nodecl::Assignment::make(
                        lhs,
                        rhs,
                        lhs_type));

            if (IS_FORTRAN_LANGUAGE
                    && it->is_parameter()
                    && it->is_optional())
            {
                Nodecl::NodeclBase capture_null = 
                    Nodecl::ExpressionStatement::make(
                            Nodecl::Assignment::make(
                                lhs.shallow_copy(),
                                Source("MERCURIUM_NULL()").parse_expression(related_function.get_related_scope()),
                                TL::Type::get_void_type().get_pointer_to()));

                Source conditional_capture_src;

                conditional_capture_src
                    << "IF (PRESENT(" << as_symbol(*it) << ")) THEN\n"
                    <<    as_statement(current_captured_stmt)
                    << "ELSE\n"
                    <<    as_statement(capture_null)
                    << "END IF\n"
                    ;

                current_captured_stmt =
                    conditional_capture_src.parse_statement(related_function.get_related_scope());
            }

            captured_list.append(current_captured_stmt);
        }


        captured_env = captured_list;
    }

    void TaskProperties::fortran_add_types(TL::Scope dest_scope)
    {
        TL::ObjectList<TL::Symbol> all_syms;
        all_syms.append(shared);
        all_syms.append(captured_value);

        TL::Nanos6::fortran_add_types(all_syms, dest_scope);
    }
} }
