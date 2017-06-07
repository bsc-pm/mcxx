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
#include "tl-nanos6-lower.hpp"
#include "tl-nanos6-support.hpp"
#include "tl-nanos6-fortran-support.hpp"
#include "tl-nanos6-interface.hpp"

#include "tl-nodecl-visitor.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-nodecl-utils-fortran.hpp"
#include "tl-symbol-utils.hpp"
#include "tl-counters.hpp"

#include "codegen-phase.hpp"

#include "tl-lowering-utils.hpp"
#include "tl-atomics.hpp"

#include "cxx-typeutils.h"
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
            _task_properties.if_clause = n.get_condition();
        }

        virtual void visit(const Nodecl::OpenMP::TaskReduction &n)
        {
            if(TL::Nanos6::Interface::family_is_at_least("nanos6_multidimensional_dependencies_api", 2))
            {
                Nodecl::List reductions = n.get_reductions().as<Nodecl::List>();
                for (Nodecl::List::iterator it = reductions.begin();
                        it != reductions.end();
                        it++)
                {
                    Nodecl::OpenMP::ReductionItem red_item = it->as<Nodecl::OpenMP::ReductionItem>();

                    TL::Symbol reductor_sym = red_item.get_reductor().get_symbol();
                    TL::Symbol reduction_symbol = red_item.get_reduced_symbol().get_symbol();
                    TL::Type reduction_type = red_item.get_reduction_type().get_type();

                    OpenMP::Reduction* red = OpenMP::Reduction::get_reduction_info_from_symbol(reductor_sym);
                    ERROR_CONDITION(red == NULL, "Invalid value for red_item", 0);

                    _task_properties.reduction.insert(TaskProperties::ReductionItem(reduction_symbol, reduction_type, red));
                }
            }
            else
            {
                not_supported("task reductions",
                        n.get_reductions().as<Nodecl::List>());
            }
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

        virtual void visit(const Nodecl::OmpSs::Concurrent &n)
        {
            handle_dependences(n, _task_properties.dep_concurrent);
        }

        virtual void visit(const Nodecl::OmpSs::DepReduction &n)
        {
            if(TL::Nanos6::Interface::family_is_at_least("nanos6_multidimensional_dependencies_api", 2))
            {
                handle_dependences(n, _task_properties.dep_reduction);
            }
            else
            {
                not_supported("task reductions",
                        n.get_exprs().as<Nodecl::List>());
            }
        }

        virtual void visit(const Nodecl::OpenMP::Final &n)
        {
            _task_properties.final_clause = n.get_condition();
        }

        virtual void visit(const Nodecl::OpenMP::Priority &n)
        {
            _task_properties.priority_clause = n.get_priority();
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

        virtual void visit(const Nodecl::OpenMP::TaskwaitDep &n)
        {
            _task_properties.is_taskwait_dep = true;
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
            _task_properties.cost_clause = n.get_cost();
        }
    };

    TaskProperties TaskProperties::gather_task_properties(
            LoweringPhase* phase,
            Lower* lower,
            const Nodecl::OpenMP::Task& node)
    {
        TaskProperties tp(phase, lower);

        tp.locus_of_task_creation = node.get_locus();
        tp.locus_of_task_declaration = node.get_locus();

        TaskPropertiesVisitor tv(tp);
        tv.walk(node.get_environment());
        tp.remove_redundant_data_sharings();

        tp.compute_captured_values();

        tp.fix_data_sharing_of_this();
        tp.related_function = Nodecl::Utils::get_enclosing_function(node);
        tp.task_body = node.get_statements();

        return tp;
    }

    TaskProperties TaskProperties::gather_task_properties(
            LoweringPhase* phase,
            Lower* lower,
            const Nodecl::OmpSs::TaskCall& node)
    {
        TaskProperties tp(phase, lower);

        tp.locus_of_task_creation = node.get_locus();
        Nodecl::FunctionCall call = node.get_call().as<Nodecl::FunctionCall>();
        tp.locus_of_task_declaration = call.get_called().get_symbol().get_locus();

        TaskPropertiesVisitor tv(tp);
        tv.walk(node.get_environment());
        tp.remove_redundant_data_sharings();

        tp.compute_captured_values();

        tp.remove_data_sharing_of_this();
        tp.related_function = Nodecl::Utils::get_enclosing_function(node);
        tp.is_function_task = true;

        return tp;
    }

    void TaskProperties::remove_redundant_data_sharings()
    {
        TL::ObjectList<TL::Symbol> new_shared_list;
        for (TL::ObjectList<TL::Symbol>::iterator it = shared.begin();
                it != shared.end();
                it++)
        {
            if (!reduction.contains(*it))
                new_shared_list.insert(*it);
        }
        shared = new_shared_list;
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
        return shared.contains(sym)         ||
               private_.contains(sym)       ||
               firstprivate.contains(sym)   ||
               captured_value.contains(sym) ||
               reduction.contains(sym);
    }
    namespace {
    struct FirstprivateSymbolsWithoutDataSharing : public Nodecl::ExhaustiveVisitor<void>
    {
        TaskProperties& _tp;
        TL::ObjectList<TL::Symbol> _ignore_symbols;

        FirstprivateSymbolsWithoutDataSharing(TaskProperties& tp) : _tp(tp)
        {}

        void operator()(Nodecl::NodeclBase node)
        {
            walk(node);
        }

        void visit(const Nodecl::MultiExpression& node)
        {
            // The iterator of a MultiExpression has to be ignored!
            _ignore_symbols.push_back(node.get_symbol());
            Nodecl::ExhaustiveVisitor<void>::visit(node);
            _ignore_symbols.pop_back();
        }

        void visit(const Nodecl::Symbol& node)
        {
            TL::Symbol sym = node.get_symbol();
            if (!sym.is_variable()
                    || sym.is_member()
                    || _ignore_symbols.contains(sym)
                    || _tp.symbol_has_data_sharing_attribute(sym))
                return;

            _tp.firstprivate.insert(sym);
        }

        void visit(const Nodecl::Conversion& node)
        {
            // int *v;
            // #pragma omp task inout( ((int (*)[N]) v)[0;M])
            Nodecl::ExhaustiveVisitor<void>::visit(node);

            TL::Type type = node.get_type();
            if (type.depends_on_nonconstant_values())
                _tp.walk_type_for_saved_expressions(type);
        }
    };
    }

    void TaskProperties::firstprivatize_symbols_without_data_sharing()
    {
        FirstprivateSymbolsWithoutDataSharing fp_syms_without_data_sharing(*this);

        // Dependences
        dep_in.map(fp_syms_without_data_sharing);
        dep_out.map(fp_syms_without_data_sharing);
        dep_inout.map(fp_syms_without_data_sharing);
        dep_weakin.map(fp_syms_without_data_sharing);
        dep_weakout.map(fp_syms_without_data_sharing);
        dep_weakinout.map(fp_syms_without_data_sharing);
        dep_commutative.map(fp_syms_without_data_sharing);
        dep_concurrent.map(fp_syms_without_data_sharing);
        dep_reduction.map(fp_syms_without_data_sharing);

        // Other task clauses
        fp_syms_without_data_sharing(cost_clause);
        fp_syms_without_data_sharing(priority_clause);
    }

    void TaskProperties::compute_captured_values()
    {
        // Do not reorder these statements
        firstprivatize_symbols_without_data_sharing();
        compute_captured_saved_expressions();
        captured_value.insert(firstprivate);
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
    namespace {

        void compute_generic_flag_c(
                Nodecl::NodeclBase opt_expr,
                int default_value,
                int bit,
                // Out
                Nodecl::NodeclBase& flags_expr)
        {
            // If the opt_expr is not present, we create a new expression using the default value
            if (opt_expr.is_null())
                opt_expr = const_value_to_nodecl(const_value_get_signed_int(default_value));

            // Building the expression for the current flag
            Nodecl::NodeclBase current_flag_expr = Nodecl::BitwiseShl::make(
                    Nodecl::Different::make(
                        opt_expr,
                        const_value_to_nodecl(const_value_get_signed_int(0)),
                        TL::Type::get_bool_type()),
                    const_value_to_nodecl(const_value_get_unsigned_int(bit)),
                    get_size_t_type());

            // Finally, we have to combine the expression fo the current flag with the previous ones
            if (!flags_expr.is_null())
            {
                flags_expr = Nodecl::BitwiseOr::make(
                        flags_expr,
                        current_flag_expr,
                        flags_expr.get_type());
            }
            else flags_expr = current_flag_expr;
        }

        Nodecl::NodeclBase compute_generic_flag_fortran(
                TL::Symbol task_flags,
                Nodecl::NodeclBase opt_expr,
                int default_value,
                int bit)
        {
            // If the opt_expr is not present, we create a new expression using the default value
            if (opt_expr.is_null())
                opt_expr = Nodecl::BooleanLiteral::make(
                        TL::Type::get_bool_type(),
                        const_value_get_unsigned_int(default_value));

            Nodecl::NodeclBase arg1 = Nodecl::FortranActualArgument::make(task_flags.make_nodecl());
            Nodecl::NodeclBase arg2 = Nodecl::FortranActualArgument::make(const_value_to_nodecl(const_value_get_signed_int(bit)));
            Nodecl::NodeclBase arguments_list = Nodecl::List::make(arg1, arg2);

            nodecl_t actual_arguments[2] = { arg1.get_internal_nodecl(), arg2.get_internal_nodecl() };

            TL::Symbol intrinsic_ibset(
                    fortran_solve_generic_intrinsic_call(
                        fortran_query_intrinsic_name_str(TL::Scope::get_global_scope().get_decl_context(), "ibset"),
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

            Nodecl::NodeclBase flag_stmt = Nodecl::IfElseStatement::make(
                    /* condition */
                    opt_expr,
                    /* then */
                    Nodecl::List::make(
                        Nodecl::ExpressionStatement::make(
                            Nodecl::Assignment::make(
                                /* lhs */ task_flags.make_nodecl(),
                                /* rhs */ ibset_function_call,
                                /* type */ task_flags.get_type().get_lvalue_reference_to()),
                            task_flags.get_locus())),
                    Nodecl::NodeclBase::null());

            return flag_stmt;
        }

        // This function negates the condition if it's not null
        Nodecl::NodeclBase negate_condition_if_possible(Nodecl::NodeclBase cond)
        {
            if (cond.is_null())
                return cond;

            return Nodecl::LogicalNot::make(cond, TL::Type::get_bool_type());
        }
    }

    void TaskProperties::compute_task_flags(TL::Symbol task_flags, Nodecl::NodeclBase& out_stmts)
    {
        Nodecl::List new_stmts;

        // Note that depending on the base language we compute the flags of a task a bit different:
        //      * C/C++: we compute a new expression that contains all the flags
        //
        //              taskflags = ((final_expr != 0) << 0) | ((!if_expr != 0) << 1);
        //
        //      * Fortran: since Fortran doesn't have a simple way to work with
        //        bit fields, we generate several statements:
        //
        //              taskflags = 0;
        //              if (final_expr) call ibset(taskflags, 0);
        //              if (!if_expr)    call ibset(taskflags, 1);
        //
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            Nodecl::NodeclBase task_flags_expr;

            compute_generic_flag_c(final_clause, /* default value */ 0, /* bit */ 0, /* out */ task_flags_expr);
            compute_generic_flag_c(negate_condition_if_possible(if_clause), /* default value */ 0, /* bit */ 1, /* out */ task_flags_expr);

            new_stmts.append(
                    Nodecl::ExpressionStatement::make(
                        Nodecl::Assignment::make(
                            task_flags.make_nodecl(/* set_ref_type */ 1),
                            task_flags_expr,
                            task_flags.get_type().no_ref().get_lvalue_reference_to()
                            )));
        }
        else // IS_FORTRAN_LANGUAGE
        {
            // Initialize the task_flags variable to zero
            new_stmts.append(
                    Nodecl::ExpressionStatement::make(
                        Nodecl::Assignment::make(
                            task_flags.make_nodecl(/* set_ref_type */ 1),
                            Nodecl::IntegerLiteral::make(
                                TL::Type::get_size_t_type(),
                                const_value_get_signed_int(0)),
                            task_flags.get_type().no_ref().get_lvalue_reference_to()
                            )));

            Nodecl::NodeclBase final_stmts =
                compute_generic_flag_fortran(task_flags, final_clause, /* default value */ 0, /* bit */ 0);

            Nodecl::NodeclBase if_stmts =
                compute_generic_flag_fortran(task_flags, negate_condition_if_possible(if_clause), /* default value */ 0, /* bit */ 1);

            new_stmts.append(final_stmts);
            new_stmts.append(if_stmts);
        }
        out_stmts = new_stmts;
    }

    void TaskProperties::create_task_info(
            /* out */
            TL::Symbol &task_info,
            TL::Symbol &task_invocation_info,
            Nodecl::NodeclBase &local_init)
    {
        create_outline_function();
        create_dependences_function();
        create_cost_function();
        create_priority_function();

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

        TL::Type run_type = TL::Type::get_void_type().get_function_returning(
                TL::ObjectList<TL::Type>(1, TL::Type::get_void_type().get_pointer_to()))
            .get_pointer_to();


        Nodecl::NodeclBase init_run;
        if (outline_function.is_valid())
        {
            if (IS_FORTRAN_LANGUAGE)
            {
                init_run = outline_function_mangled.make_nodecl(/* set_ref_type */ true);
            }
            else
            {
                init_run = outline_function.make_nodecl(/* set_ref_type */ true);
            }
            init_run = Nodecl::Conversion::make(
                    init_run,
                    run_type);
            init_run.set_text("C");
        }
        else
        {
            init_run = const_value_to_nodecl(const_value_get_signed_int(0));
        }

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

        Nodecl::NodeclBase field_get_priority = get_field("get_priority");
        Nodecl::NodeclBase init_get_priority;
        if (priority_function.is_valid())
        {
            TL::Type priority_fun_type =
                TL::Type::get_size_t_type().get_function_returning(
                        TL::ObjectList<TL::Type>(
                            1, TL::Type::get_void_type().get_pointer_to()))
                .get_pointer_to();

            init_get_priority = Nodecl::Conversion::make(
                    priority_function.make_nodecl(/* set_ref_type */ true),
                    priority_fun_type);

            init_get_priority.set_text("C");
        }
        else
        {
            init_get_priority =
                const_value_to_nodecl(const_value_get_signed_int(0));
        }

        TL::ObjectList<Nodecl::NodeclBase> field_init;
        field_init.append(
            Nodecl::FieldDesignator::make(field_run,
                                          init_run,
                                          field_run.get_type()));
        field_init.append(
            Nodecl::FieldDesignator::make(field_register_depinfo,
                                          init_register_depinfo,
                                          field_register_depinfo.get_type()));
        field_init.append(
            Nodecl::FieldDesignator::make(field_get_priority,
                                          init_get_priority,
                                          field_get_priority.get_type()));
        field_init.append(
            Nodecl::FieldDesignator::make(field_task_label,
                                          init_task_label,
                                          field_task_label.get_type()));
        field_init.append(
            Nodecl::FieldDesignator::make(field_declaration_source,
                                          init_declaration_source,
                                          field_declaration_source.get_type()));
        field_init.append(
            Nodecl::FieldDesignator::make(field_get_cost,
                                          init_get_cost,
                                          field_get_cost.get_type()));

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
    TL::Type rewrite_type(TL::Type t, TL::Scope scope, Nodecl::Utils::SymbolMap &symbol_map)
    {
        return type_deep_copy(
            t.get_internal_type(),
            scope.get_decl_context(),
            symbol_map.get_symbol_map());
    }

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

        for (TL::ObjectList<ReductionItem>::const_iterator it = reduction.begin();
                it != reduction.end();
                it++)
        {
            const ReductionItem& curr_red_item = *it;

            // First we add the pointer to the original variable
            {
                TL::Type type_of_field = curr_red_item.reduction_type.no_ref();
                if (IS_FORTRAN_LANGUAGE)
                    type_of_field = TL::Type::get_void_type().get_pointer_to();
                else
                    type_of_field = type_of_field.get_pointer_to();

                TL::Symbol field = add_field_to_class(
                        new_class_symbol,
                        class_scope,
                        curr_red_item.symbol.get_name(),
                        curr_red_item.symbol.get_locus(),
                        /* is_allocatable */ false,
                        type_of_field);

                field_map[curr_red_item.symbol] = field;
            }

            // Second, we add the local variable
            {
                TL::Type type_of_field = curr_red_item.reduction_type;
                TL::Symbol field = add_field_to_class(
                        new_class_symbol,
                        class_scope,
                        curr_red_item.symbol.get_name() + "_local_red",
                        curr_red_item.symbol.get_locus(),
                        /* is_allocatable */ false,
                        type_of_field);

                // Note that we do not explicitly map this field to the original list item!
            }

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

            void handle_symbol(TL::Symbol sym, const std::string& name)
            {
                std::string fixed_name = name;
                if (IS_CXX_LANGUAGE && name == "this")
                    fixed_name = "_this";

                _symbols_to_param_names.insert(std::make_pair(sym, fixed_name));
                _unpack_parameter_names.append(fixed_name);
                _unpack_parameter_types.append(sym.get_type().no_ref().get_lvalue_reference_to());
            }

            void operator()(TL::Symbol sym)
            {
                handle_symbol(sym, sym.get_name());
            }

            void operator()(const TaskProperties::ReductionItem& red_item)
            {
                TL::Symbol sym = red_item.symbol;
                handle_symbol(sym, sym.get_name());
                handle_symbol(sym, sym.get_name() + "_local_red");
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

            void compute_mapping(TL::Symbol sym)
            {
                std::map<TL::Symbol, std::string>::const_iterator it_param_name = _symbols_to_param_names.find(sym);
                ERROR_CONDITION(it_param_name == _symbols_to_param_names.end(),
                                "Symbol '%s' not mapped",sym.get_name().c_str());

                TL::Symbol param_sym = _param_to_symbol(it_param_name->second);
                _symbol_map.add_map(sym, param_sym);

                // Propagate TARGET attribute
                if (sym.is_target())
                  symbol_entity_specs_set_is_target(param_sym.get_internal_symbol(), 1);

                // Propagate ALLOCATABLE attribute
                if (sym.is_allocatable())
                    symbol_entity_specs_set_is_allocatable(param_sym.get_internal_symbol(), 1);

                if (type_is_runtime_sized(param_sym.get_type()))
                    _parameters_to_update_type.append(param_sym);
            }

            void operator()(TL::Symbol sym)
            {
                compute_mapping(sym);
            }

            void operator()(const TaskProperties::ReductionItem& red_item)
            {
                compute_mapping(red_item.symbol);
            }
        };
    }


    void TaskProperties::create_outline_function()
    {
        // Skip this function if the current task comes from a taskwait depend
        if (is_taskwait_dep)
            return;

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

        AddParameter add_params_functor(
                /* out */ unpack_parameter_names,
                /* out */ unpack_parameter_types,
                /* out */ symbols_to_param_names);

        captured_value.map(add_params_functor);
        shared.map(add_params_functor);
        reduction.map(add_params_functor);

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
        Nodecl::Utils::append_to_top_level_nodecl(unpacked_function_code);

        TL::Scope unpacked_inside_scope = unpacked_empty_stmt.retrieve_context();

        // Prepare deep copy and remember those parameters that need fixup
        Nodecl::Utils::SimpleSymbolMap symbol_map;

        ParameterToSymbol param_to_symbol(unpacked_inside_scope);

        TL::ObjectList<TL::Symbol> parameters_to_update_type;

        MapSymbols map_symbols_functor(
                param_to_symbol,
                symbols_to_param_names,
                /* out */ symbol_map,
                /* out */ parameters_to_update_type);

        captured_value.map(map_symbols_functor);
        shared.map(map_symbols_functor);
        reduction.map(map_symbols_functor);


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
            it->set_type(rewrite_type(it->get_type(), unpacked_inside_scope, symbol_map));
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
            priv.set_type(rewrite_type(it->get_type().no_ref(), unpacked_inside_scope, symbol_map));

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
        unpacked_empty_stmt.append_sibling(nested_functions);

        handle_task_reductions(unpacked_inside_scope, unpacked_empty_stmt);

        // Deep copy the body
        Nodecl::NodeclBase body = Nodecl::Utils::deep_copy(task_body, unpacked_inside_scope, symbol_map);
        unpacked_empty_stmt.replace(body);

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

                Nodecl::NodeclBase argument = Nodecl::ClassMemberAccess::make(
                        arg.make_nodecl(/* set_ref_type */ true),
                        field_map[*it].make_nodecl(),
                        /* member_literal */ Nodecl::NodeclBase::null(),
                        field_map[*it].get_type().no_ref().get_lvalue_reference_to());

                if (it->get_type().no_ref().is_array()
                        && it->get_type().depends_on_nonconstant_values())
                {
                    TL::Type param_type = rewrite_type_using_args(
                            arg, it->get_type().no_ref(), TL::ObjectList<TL::Symbol>());

                    Nodecl::NodeclBase cast;
                    argument = Nodecl::Dereference::make(
                            cast = Nodecl::Conversion::make(
                                Nodecl::Reference::make(
                                    argument,
                                    field_map[*it].get_type().get_pointer_to()),
                                param_type.get_pointer_to()),
                            param_type.get_lvalue_reference_to());

                    cast.set_text("C");
                }

                args.append(argument);
            }

            for (TL::ObjectList<TL::Symbol>::iterator it = shared.begin();
                    it != shared.end();
                    it++)
            {
                ERROR_CONDITION(field_map.find(*it) == field_map.end(), "Symbol is not mapped", 0);

                ERROR_CONDITION(it->get_type().depends_on_nonconstant_values()
                        && !it->get_type().no_ref().is_array(), "Unexpected type\n", 0);

                //  Depending on the type of the list item we may have two different scenarios:
                //  * If the list item is an array (i.e. T a[N][M]),
                //    in the struct we have 'T (*p)[N]' and the unpacked function expects a 'T (&a)[N][M]'
                //  * otherwise, if the list item is not an array (i.e. T x),
                //    in the struct we have 'T*' and the unpacked function expects 'T&'

                Nodecl::NodeclBase argument = Nodecl::ClassMemberAccess::make(
                        arg.make_nodecl(/* set_ref_type */ true),
                        field_map[*it].make_nodecl(),
                        /* member_literal */ Nodecl::NodeclBase::null(),
                        field_map[*it].get_type().get_lvalue_reference_to());

                if (it->get_type().no_ref().is_array())
                {
                    TL::Type cast_type_type =
                        rewrite_type_using_args(arg, it->get_type().no_ref().get_pointer_to(), TL::ObjectList<TL::Symbol>());

                    // (T (*)[N][M]) arg.v
                    argument = Nodecl::Conversion::make(argument, cast_type_type);

                    argument.set_text("C");
                }

                args.append(Nodecl::Dereference::make(
                            argument,
                            argument.get_type().no_ref().points_to().get_lvalue_reference_to()));
            }

            for (TL::ObjectList<ReductionItem>::const_iterator it = reduction.begin();
                    it != reduction.end();
                    it++)
            {
                ERROR_CONDITION(field_map.find(it->symbol) == field_map.end(), "Symbol is not mapped", 0);

                // 1st. Adding the original reduction variable as an argument
                {
                    args.append(
                            Nodecl::Dereference::make(
                                Nodecl::ClassMemberAccess::make(
                                    arg.make_nodecl(/* set_ref_type */ true),
                                    field_map[it->symbol].make_nodecl(),
                                    /* member_literal */ Nodecl::NodeclBase::null(),
                                    field_map[it->symbol].get_type().get_lvalue_reference_to()),
                                field_map[it->symbol].get_type().points_to().get_lvalue_reference_to())
                            );
                }

                // 2nd. Adding the task local variable as an argument
                {
                    TL::Scope inner_class_context(
                            class_type_get_inner_context(info_structure.get_internal_type()));

                    TL::Symbol local_symbol = inner_class_context.get_symbol_from_name(it->symbol.get_name() + "_local_red");
                    TL::Type expr_type = local_symbol.get_type().no_ref().get_lvalue_reference_to();
                    args.append(
                            Nodecl::ClassMemberAccess::make(
                                arg.make_nodecl(/* set_ref_type */ true),
                                local_symbol.make_nodecl(),
                                /* member_literal */ Nodecl::NodeclBase::null(),
                                expr_type));
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

            for (TL::ObjectList<ReductionItem>::const_iterator it = reduction.begin();
                    it != reduction.end();
                    it++)
            {
                ERROR_CONDITION(field_map.find(it->symbol) == field_map.end(), "Symbol is not mapped", 0);

                {
                    TL::Symbol field = field_map[it->symbol];

                    forwarded_parameter_names.append(field.get_name());
                    forwarded_parameter_types.append(field.get_type());

                    args.append(
                            Nodecl::ClassMemberAccess::make(
                                // Nodecl::Dereference::make(
                        arg.make_nodecl(/* set_ref_type */ true),
                        //    arg.get_type().points_to().get_lvalue_reference_to()),
                                field_map[it->symbol].make_nodecl(),
                                /* member_literal */ Nodecl::NodeclBase::null(),
                                field_map[it->symbol].get_type().get_lvalue_reference_to()));
                }

                {
                    TL::Scope inner_class_context(
                            class_type_get_inner_context(info_structure.get_internal_type()));

                    TL::Symbol local_symbol = inner_class_context.get_symbol_from_name(it->symbol.get_name() + "_local_red");
                    TL::Type expr_type = local_symbol.get_type().no_ref().get_lvalue_reference_to();

                    forwarded_parameter_names.append(local_symbol.get_name());
                    forwarded_parameter_types.append(expr_type);

                    args.append(
                            Nodecl::ClassMemberAccess::make(
                                arg.make_nodecl(/* set_ref_type */ true),
                                local_symbol.make_nodecl(),
                                /* member_literal */ Nodecl::NodeclBase::null(),
                                expr_type));
                }
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

    class ReplaceSymbolsVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
        const std::map<TL::Symbol, TL::Symbol> _symbols_to_be_replaced;

        public:
        ReplaceSymbolsVisitor(const std::map<TL::Symbol, TL::Symbol>& map)
            : _symbols_to_be_replaced(map)
        { }

        void visit(const Nodecl::Symbol& node)
        {
            TL::Symbol current_symbol = node.get_symbol();
            std::map<TL::Symbol, TL::Symbol>::const_iterator it = _symbols_to_be_replaced.find(current_symbol);

            // Do nothing if the symbol is not present
            if (it == _symbols_to_be_replaced.end())
                return;

            TL::Symbol mapped_symbol(it->second);
            node.replace(mapped_symbol.make_nodecl(/* set_ref_type */ true));
        }
    };

    Nodecl::NodeclBase make_mutual_exclusive_expr(Nodecl::ExpressionStatement expr_stmt)
    {
        Nodecl::NodeclBase expr = expr_stmt.as<Nodecl::ExpressionStatement>().get_nest();
        bool builtin_atomic = false;
        bool nanox_api_atomic = false; // Feasible atomic transformation using Nanox API

        if (!allowed_expression_atomic(
                    expr, builtin_atomic, nanox_api_atomic)
                || nanox_api_atomic)
        {
            return Nodecl::OpenMP::Critical::make(
                    /* environment */ Nodecl::NodeclBase::null(),
                    Nodecl::List::make(Nodecl::ExpressionStatement::make(expr)));
        }

        if (builtin_atomic)
            return builtin_atomic_int_op(expr);

        return compare_and_exchange(expr);
    }

    void TaskProperties::handle_task_reductions(
            const TL::Scope& unpacked_inside_scope,
            Nodecl::NodeclBase unpacked_empty_stmt)
    {
        for (TL::ObjectList<ReductionItem>::const_iterator it = reduction.begin();
                it != reduction.end();
                it++)
        {
            const ReductionItem& red_item(*it);

            TL::Symbol orig_red_var = unpacked_inside_scope.get_symbol_from_name(red_item.symbol.get_name());
            TL::Symbol priv_red_var = unpacked_inside_scope.get_symbol_from_name(red_item.symbol.get_name() + "_local_red");

            // Computing the expression statement that initializes our task local variable
            Nodecl::NodeclBase initializer = red_item.reduction_info->get_initializer().shallow_copy();
            std::map<TL::Symbol, TL::Symbol> initializer_translation_map;

            TL::Symbol omp_orig  = red_item.reduction_info->get_omp_orig();
            initializer_translation_map[omp_orig] = orig_red_var;

            ReplaceSymbolsVisitor initializer_visitor(initializer_translation_map);
            initializer_visitor.walk(initializer);

            Nodecl::NodeclBase assignment = Nodecl::Assignment::make(
                    priv_red_var.make_nodecl(/* set_ref_type */ true),
                    initializer,
                    priv_red_var.get_type().no_ref().get_lvalue_reference_to());

            unpacked_empty_stmt.prepend_sibling(Nodecl::ExpressionStatement::make(assignment));


            // Computing the combiner statements
            Nodecl::NodeclBase combiner = red_item.reduction_info->get_combiner().shallow_copy();
            std::map<TL::Symbol, TL::Symbol> combiner_translation_map;

            TL::Symbol omp_in  = red_item.reduction_info->get_omp_in();
            TL::Symbol omp_out = red_item.reduction_info->get_omp_out();
            combiner_translation_map[omp_in]  = priv_red_var;
            combiner_translation_map[omp_out] = orig_red_var;

            ReplaceSymbolsVisitor combiner_visitor(combiner_translation_map);
            combiner_visitor.walk(combiner);
            combiner = Nodecl::ExpressionStatement::make(combiner);
            unpacked_empty_stmt.append_sibling(combiner);

            combiner.replace(make_mutual_exclusive_expr(combiner.as<Nodecl::ExpressionStatement>()));
            lower_visitor->walk(combiner);
        }
    }


    // FIXME: MOVE THIS FUNCTION TO ANOTHER PLACE
    void get_reduction_type(const TL::Type& data_type, Nodecl::NodeclBase &type_node)
    {
        std::string type_name;

        if (data_type.is_char())
            type_name = "RED_TYPE_CHAR";
        else if (data_type.is_signed_char())
            type_name = "RED_TYPE_SIGNED_CHAR";
        else if (data_type.is_unsigned_char())
            type_name = "RED_TYPE_UNSIGNED_CHAR";
        else if (data_type.is_signed_short_int())
            type_name = "RED_TYPE_SHORT";
        else if (data_type.is_unsigned_short_int())
            type_name = "RED_TYPE_UNSIGNED_SHORT";
        else if (data_type.is_signed_integral())
            type_name = "RED_TYPE_INT";
        else if (data_type.is_unsigned_integral())
            type_name = "RED_TYPE_UNSIGNED_INT";
        else if (data_type.is_signed_long_int())
            type_name = "RED_TYPE_LONG";
        else if (data_type.is_unsigned_long_int())
            type_name = "RED_TYPE_UNSIGNED_LONG";
        else if (data_type.is_signed_long_long_int())
            type_name = "RED_TYPE_LONG_LONG";
        else if (data_type.is_unsigned_long_long_int())
            type_name = "RED_TYPE_UNSIGNED_LONG_LONG";
        else if (data_type.is_float())
        {
            if (data_type.is_complex())
                type_name = "RED_TYPE_COMPLEX_FLOAT";
            else
                type_name = "RED_TYPE_FLOAT";
        }
        else if (data_type.is_double())
        {
            if (data_type.is_complex())
                type_name = "RED_TYPE_COMPLEX_DOUBLE";
            else
                type_name = "RED_TYPE_DOUBLE";
        }
        else if (data_type.is_long_double())
        {
            if (data_type.is_complex())
                type_name = "RED_TYPE_COMPLEX_LONG_DOUBLE";
            else
                type_name = "RED_TYPE_LONG_DOUBLE";
        }
        else if (data_type.is_bool())
            type_name = "RED_TYPE_BOOLEAN";

        TL::Symbol type_sym = TL::Scope::get_global_scope().get_symbol_from_name(type_name);
        ERROR_CONDITION(type_sym.is_invalid(), "'%s' symbol not found", type_name.c_str());

        type_node = type_sym.make_nodecl(/* set_ref_type */ false);
    }


    // FIXME: MOVE THIS FUNCTION TO ANOTHER PLACE
    void get_reduction_operation(const TL::OpenMP::Reduction& red_info, Nodecl::NodeclBase& operation_node)
    {
        std::string reduction_name = red_info.get_name();
        std::string operation_name;

        if (reduction_name == "+"       || reduction_name == "operator +")
            operation_name = "RED_OP_ADDITION";
        if (reduction_name == "-"       || reduction_name == "operator -")
            operation_name = "RED_OP_ADDITION";
        else if (reduction_name == "*"  || reduction_name == "operator *")
            operation_name = "RED_OP_PRODUCT";
        else if (reduction_name == "&"  || reduction_name == "operator &"  || reduction_name == "iand")
            operation_name = "RED_OP_BITWISE_AND";
        else if (reduction_name == "|"  || reduction_name == "operator |"  || reduction_name == "ior")
            operation_name = "RED_OP_BITWISE_OR";
        else if (reduction_name == "^"  || reduction_name == "operator ^"  || reduction_name == "ieor")
            operation_name = "RED_OP_BITWISE_XOR";
        else if (reduction_name == "&&" || reduction_name == "operator &&" || reduction_name == ".and.")
            operation_name = "RED_OP_LOGICAL_AND";
        else if (reduction_name == "||" || reduction_name == "operator ||" || reduction_name == ".or.")
            operation_name = "RED_OP_LOGICAL_OR";
        else if (reduction_name == ".neqv.")
            operation_name = "RED_OP_LOGICAL_XOR";
        else if (reduction_name == ".eqv.")
            operation_name = "RED_OP_LOGICAL_NXOR";
        else if (reduction_name == "max")
            operation_name = "RED_OP_MAXIMUM";
        else if (reduction_name == "min")
            operation_name = "RED_OP_MINIMUM";

        TL::Symbol operation_sym = TL::Scope::get_global_scope().get_symbol_from_name(operation_name);
        ERROR_CONDITION(operation_sym.is_invalid(), "'%s' symbol not found", operation_name.c_str());

        operation_node = operation_sym.make_nodecl(/* set_ref_type */ false);
    }

    void TaskProperties::compute_reduction_arguments_dependence_c(
            TL::DataReference& data_ref,
            TL::Symbol arg,
            const TL::ObjectList<TL::Symbol>& local_symbols,
            // Out
            Nodecl::List& arguments_list)
    {
        // 1st argument: type operation identifier
        TL::ObjectList<ReductionItem> red_items =
            reduction.find(ReductionItem(data_ref.get_base_symbol()));

        ERROR_CONDITION(red_items.empty(), "No reduction item for symbol '%s'",
                data_ref.get_base_symbol().get_name().c_str());

        TL::OpenMP::Reduction *reduction_info = red_items.begin()->reduction_info;

        Nodecl::NodeclBase type_node;
        Nodecl::NodeclBase operation_node;
        get_reduction_type(reduction_info->get_type(), type_node);
        get_reduction_operation(*reduction_info, operation_node);

        Nodecl::NodeclBase arg1_type_op = Nodecl::Add::make(
                type_node,
                operation_node,
                reduction_info->get_type()
                );

        // 2nd argument: reduction identifier within task
        Nodecl::NodeclBase arg2_id = const_value_to_nodecl(
                const_value_get_unsigned_int(num_reductions));

        arguments_list.append(rewrite_expression_using_args(arg, arg1_type_op, local_symbols));
        arguments_list.append(rewrite_expression_using_args(arg, arg2_id, local_symbols));
    }

    void TaskProperties::compute_dimensions_dependence_c(
            TL::Type array_type,
            TL::Symbol arg,
            const TL::ObjectList<TL::Symbol>& local_symbols,
            // Out
            Nodecl::List& arguments_list)
    {
        ERROR_CONDITION(!array_type.is_array(), "Unexpected type", 0);

        TL::Type element_type = array_type.array_element();
        if (element_type.is_array())
            compute_dimensions_dependence_c(element_type, arg, local_symbols, arguments_list);

        Nodecl::NodeclBase size, lower_bound, upper_bound;

        size = array_type.array_get_size().shallow_copy();

        if (array_type.array_is_region())
        {
            array_type.array_get_region_bounds(lower_bound, upper_bound);
            lower_bound = lower_bound.shallow_copy();
            upper_bound =
                Nodecl::Add::make(
                        upper_bound.shallow_copy(),
                        const_value_to_nodecl(const_value_get_one(8, 0)),
                        upper_bound.get_type().no_ref());
        }
        else
        {
            array_type.array_get_bounds(lower_bound, upper_bound);
            lower_bound = lower_bound.shallow_copy();
            upper_bound =
                Nodecl::Add::make(
                        upper_bound.shallow_copy(),
                        const_value_to_nodecl(const_value_get_one(8, 0)),
                        upper_bound.get_type().no_ref());
        }

        // Continuous dimension should be expressed in bytes
        if (!element_type.is_array())
        {
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

        arguments_list.append(rewrite_expression_using_args(arg, size, local_symbols));
        arguments_list.append(rewrite_expression_using_args(arg, lower_bound, local_symbols));
        arguments_list.append(rewrite_expression_using_args(arg, upper_bound, local_symbols));
    }

    void TaskProperties::compute_arguments_dependence_c(
            TL::DataReference& data_ref,
            TL::Symbol handler,
            TL::Symbol arg,
            const TL::ObjectList<TL::Symbol>& local_symbols,
            // Out
            Nodecl::List& arguments_list)
    {
        Nodecl::NodeclBase arg1_handler;
        Nodecl::NodeclBase arg2_sym_identifier;
        Nodecl::NodeclBase arg3_dep_text;
        Nodecl::NodeclBase arg4_base_address;

        // 1st argument: task handler
        arg1_handler = handler.make_nodecl(/* set_ref_type */ true);

        // 2nd argument: sym identifier
        arg2_sym_identifier = const_value_to_nodecl(const_value_get_minus_one(4, 1));

        // 3rd argument: dependence text
        std::string dependence_text = Codegen::get_current().codegen_to_str(data_ref, data_ref.retrieve_context());
        arg3_dep_text = const_value_to_nodecl(
                const_value_make_string_null_ended(
                    dependence_text.c_str(),
                    strlen(dependence_text.c_str())));

        // 4rd argument: base address of the expression
        arg4_base_address = Nodecl::Conversion::make(
                data_ref.get_base_address().shallow_copy(),
                TL::Type::get_void_type().get_pointer_to());
        arg4_base_address.set_text("C");
        arg4_base_address = rewrite_expression_using_args(
                arg, arg4_base_address, local_symbols);

        arguments_list.append(arg1_handler);
        arguments_list.append(arg2_sym_identifier);
        arguments_list.append(arg3_dep_text);
        arguments_list.append(arg4_base_address);

        TL::Type data_type = data_ref.get_data_type();

        if (!data_type.is_array())
        {
            // The current dependence is not an array

            Nodecl::NodeclBase arg5_size, arg6_lower_bound, arg7_upper_bound;

            // 5th argument: size = sizeof(data_type)
            arg5_size = data_ref.get_sizeof().shallow_copy();

            // 6th argument: lower_bound = 0
            arg6_lower_bound = const_value_to_nodecl(const_value_get_zero(8, 0));

            // 7th argument: upper_bound = sizeof(data_type)
            arg7_upper_bound = arg5_size.shallow_copy();

            arguments_list.append(arg5_size);
            arguments_list.append(arg6_lower_bound);
            arguments_list.append(arg7_upper_bound);
        }
        else
        {
            compute_dimensions_dependence_c(
                    data_type, arg, local_symbols, arguments_list);
        }
    }

    void TaskProperties::register_dependence_c(
            TL::DataReference& data_ref,
            TL::Symbol handler,
            TL::Symbol arg,
            TL::Symbol register_fun,
            const TL::ObjectList<TL::Symbol>& local_symbols,
            // Out
            Nodecl::List& register_statements)
    {
        std::string reduction_register_fun_name = "nanos_register_region_reduction_depinfo";
        bool is_reduction = reduction_register_fun_name.compare(
                0, std::string::npos, register_fun.get_name(),
                0, reduction_register_fun_name.length()) == 0;

        Nodecl::List args;

        if (is_reduction)
        {
            ERROR_CONDITION(data_ref.get_data_type().is_array(), "Array reductions not supported", 0);

            compute_reduction_arguments_dependence_c(
                    data_ref, arg, local_symbols, args);

            // Increment number of registered reductions for the task (used as id when registering)
            num_reductions++;
        }

        compute_arguments_dependence_c(
                data_ref, handler, arg, local_symbols, args);

        Nodecl::NodeclBase function_call =
            Nodecl::ExpressionStatement::make(
                    Nodecl::FunctionCall::make(
                        register_fun.make_nodecl(/* set_ref_type */ true),
                        args,
                        /* alternate_name */ Nodecl::NodeclBase::null(),
                        /* function_form */ Nodecl::NodeclBase::null(),
                        get_void_type()));

        register_statements.append(function_call);
    }


    void TaskProperties::register_multidependence_c(
            TL::DataReference& data_ref,
            TL::Symbol handler,
            TL::Symbol arg,
            TL::Symbol register_fun,
            const TL::ObjectList<TL::Symbol>& local_symbols,
            TL::Scope scope,
            // Out
            Nodecl::List& register_statements)
    {
        TL::ObjectList<TL::DataReference::MultiRefIterator> multireferences = data_ref.multireferences();

        Nodecl::Utils::SimpleSymbolMap symbol_map;
        TL::ObjectList<TL::Symbol> current_locals;
        TL::Counter &ctr = TL::CounterManager::get_counter("nanos6-multideps");
        for (TL::ObjectList<TL::DataReference::MultiRefIterator>::iterator it2 = multireferences.begin();
                it2 != multireferences.end();
                it2++)
        {
            std::stringstream ss;
            ss << it2->first.get_name() << "_tmp_" << (int)ctr;
            ctr++;
            std::string ind_var_name = ss.str();

            TL::Symbol local_sym = scope.new_symbol(ind_var_name);
            local_sym.get_internal_symbol()->kind = SK_VARIABLE;
            local_sym.get_internal_symbol()->type_information =
                ::get_signed_int_type();
            symbol_entity_specs_set_is_user_declared(local_sym.get_internal_symbol(),
                    1);

            symbol_map.add_map(it2->first, local_sym);
            current_locals.append(local_sym);

            CXX_LANGUAGE()
            {
                register_statements.append(
                        Nodecl::CxxDef::make(Nodecl::NodeclBase::null(),
                            local_sym));
            }
        }


        // This empty stmt will be replaced by the function call to the runtime
        Nodecl::NodeclBase empty_stmt = Nodecl::EmptyStatement::make();

        Nodecl::NodeclBase body = Nodecl::List::make(empty_stmt);
        for (TL::ObjectList<TL::DataReference::MultiRefIterator>::reverse_iterator it2 = multireferences.rbegin();
                it2 != multireferences.rend();
                it2++)
        {
            ERROR_CONDITION(!it2->second.is<Nodecl::Range>(), "Invalid Node", 0);

            Nodecl::Range range = it2->second.as<Nodecl::Range>();
            Nodecl::NodeclBase lower = rewrite_expression_using_args(arg, range.get_lower(), current_locals);
            Nodecl::NodeclBase upper = rewrite_expression_using_args(arg, range.get_upper(), current_locals);
            Nodecl::NodeclBase stride = rewrite_expression_using_args(arg, range.get_stride(), current_locals);

            TL::Symbol ind_var = symbol_map.map(it2->first);

            Nodecl::NodeclBase loop_control =
                Nodecl::LoopControl::make(
                    // init-expr-list
                        Nodecl::List::make(
                            Nodecl::Assignment::make(
                                ind_var.make_nodecl(/* set_ref_type */ true),
                                lower,
                                ind_var.get_type().no_ref().get_lvalue_reference_to())),
                    // test-expr
                    Nodecl::LowerOrEqualThan::make(
                        ind_var.make_nodecl(/* set_ref_type */ true),
                        upper,
                        TL::Type::get_bool_type()),
                    // incr-exp
                    Nodecl::AddAssignment::make(
                        ind_var.make_nodecl(/* set_ref_type */ true),
                        stride,
                        ind_var.get_type().no_ref().get_lvalue_reference_to()));

            // Note that we are not creating any context / compound stmt. Thus, the body has to be always an statement
            Nodecl::NodeclBase for_stmt =
                Nodecl::ForStatement::make(loop_control, body, /* loop-name */ Nodecl::NodeclBase::null());

            body = Nodecl::List::make(for_stmt);
        }

        Nodecl::NodeclBase base_exp = data_ref;
        while (base_exp.is<Nodecl::MultiExpression>())
            base_exp = base_exp.as<Nodecl::MultiExpression>().get_base();

        base_exp = Nodecl::Utils::deep_copy(base_exp, scope, symbol_map);

        TL::DataReference base_data_ref = base_exp;
        Nodecl::List base_reg;

        register_dependence_c(
                base_data_ref,
                handler,
                arg,
                register_fun,
                current_locals,
                base_reg);

        empty_stmt.replace(base_reg);

        register_statements.append(body);
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
            { dep_in,    "nanos_register_region_read_depinfo"              },
            { dep_out,   "nanos_register_region_write_depinfo"             },
            { dep_inout, "nanos_register_region_readwrite_depinfo"         },

            { dep_weakin,    "nanos_register_region_weak_read_depinfo"      },
            { dep_weakout,   "nanos_register_region_weak_write_depinfo"     },
            { dep_weakinout, "nanos_register_region_weak_readwrite_depinfo" },

            { dep_commutative, "nanos_register_region_commutative_depinfo" },
            { dep_concurrent,  "nanos_register_region_concurrent_depinfo"  },

            { dep_reduction, "nanos_register_region_reduction_depinfo" },
        };

        for (DependencesSet *dep_set = deps;
                dep_set != (DependencesSet*)(&deps + 1);
                dep_set++)
        {
            TL::ObjectList<Nodecl::NodeclBase> &dep_list = dep_set->dep_list;
            if (dep_list.empty())
                continue;

            for (TL::ObjectList<Nodecl::NodeclBase>::iterator
                    it = dep_list.begin();
                    it != dep_list.end();
                    it++)
            {
                TL::DataReference data_ref = *it;
                TL::Type data_type = data_ref.get_data_type();

                TL::Symbol register_fun;
                {
                    int max_dimensions = phase->get_deps_max_dimensions();
                    ERROR_CONDITION(data_type.is_array() &&
                            (data_type.get_num_dimensions() > max_dimensions),
                            "Maximum number of data dimensions allowed is %d",
                            max_dimensions);

                    int num_dims_dep = data_type.is_array() ? data_type.get_num_dimensions() : 1;
                    std::stringstream ss;
                    ss << dep_set->func_name << num_dims_dep;

                    register_fun = global_context.get_symbol_from_name(ss.str());
                    if (!register_fun.is_valid())
                    {
                        fatal_error(
                                "'%s' function not found while trying to register dependences\n",
                                ss.str().c_str());
                    }
                }

                Nodecl::List register_statements;
                if (!data_ref.is_multireference())
                {
                    register_dependence_c(
                            data_ref,
                            handler,
                            arg,
                            register_fun,
                            /* local_syms */ TL::ObjectList<TL::Symbol>(),
                            register_statements);
                }
                else
                {
                    register_multidependence_c(
                            data_ref,
                            handler,
                            arg,
                            register_fun,
                            /* local_syms */ TL::ObjectList<TL::Symbol>(),
                            dependences_inside_scope,
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

    void TaskProperties::compute_reduction_arguments_dependence_fortran(
            TL::DataReference& data_ref,
            // Out
            Nodecl::List& arguments_list)
    {
        // 1st argument: type operation identifier
        TL::ObjectList<ReductionItem> red_items =
            reduction.find(ReductionItem(data_ref.get_base_symbol()));

        ERROR_CONDITION(red_items.empty(), "No reduction item for symbol '%s'",
                data_ref.get_base_symbol().get_name().c_str());

        TL::OpenMP::Reduction *reduction_info = red_items.begin()->reduction_info;

        Nodecl::NodeclBase type_node;
        Nodecl::NodeclBase operation_node;
        get_reduction_type(reduction_info->get_type(), type_node);
        get_reduction_operation(*reduction_info, operation_node);

        Nodecl::NodeclBase arg1_type_op = Nodecl::Add::make(
                type_node,
                operation_node,
                reduction_info->get_type()
                );

        // 2nd argument: reduction identifier within task
        Nodecl::NodeclBase arg2_id = const_value_to_nodecl(
                const_value_get_unsigned_int(num_reductions));

        arguments_list.append(arg1_type_op);
        arguments_list.append(arg2_id);
    }

    void TaskProperties::compute_dimensions_dependence_fortran(
            const TL::DataReference& data_ref,
            TL::Type array_type,
            // Out
            Nodecl::List& arguments_list)
    {
        ERROR_CONDITION(!array_type.is_array(), "Unexpected type", 0);

        TL::Type element_type = array_type.array_element();
        if (element_type.is_array())
            compute_dimensions_dependence_fortran(data_ref, element_type, arguments_list);

        Nodecl::NodeclBase array_lb, array_ub;
        {
            array_type.array_get_bounds(array_lb, array_ub);
            if (array_lb.is_null())
                array_lb = TL::Lowering::Utils::Fortran::get_lower_bound(data_ref, array_type.fortran_rank());
            else
                array_lb = array_lb.shallow_copy();

            if (array_ub.is_null())
                array_ub = TL::Lowering::Utils::Fortran::get_upper_bound(data_ref, array_type.fortran_rank());
            else
                array_ub = array_ub.shallow_copy();
        }

        Nodecl::NodeclBase region_lb, region_ub;
        {
            if (array_type.array_is_region())
            {
                array_type.array_get_region_bounds(region_lb, region_ub);
            }
            else
            {
                region_lb = array_lb.shallow_copy();
                region_ub = array_ub.shallow_copy();
            }
        }

        Nodecl::NodeclBase arg_size =
                TL::Lowering::Utils::Fortran::get_size_for_dimension(data_ref, array_type, array_type.fortran_rank());

        Nodecl::NodeclBase arg_adj_lb, arg_adj_ub;
        arg_adj_lb = Nodecl::Minus::make(
                    region_lb,
                    array_lb,
                    region_lb.get_type().no_ref());

        //XXX: ADD one?
        arg_adj_ub = Nodecl::Add::make(
            Nodecl::Minus::make(
                    region_ub,
                    array_lb,
                    region_lb.get_type().no_ref()),
                const_value_to_nodecl(const_value_get_one(8, 0)),
                region_lb.get_type().no_ref());

        // Continuous dimension should be expressed in bytes
        if (!element_type.is_array())
        {
            Nodecl::NodeclBase element_type_size = Nodecl::Sizeof::make(
                    Nodecl::Type::make(element_type),
                    Nodecl::NodeclBase::null(),
                    get_size_t_type());

            arg_size = Nodecl::Mul::make(
                    Nodecl::ParenthesizedExpression::make(arg_size, arg_size.get_type().no_ref()),
                    element_type_size,
                    arg_size.get_type().no_ref());

            arg_adj_lb = Nodecl::Mul::make(
                    Nodecl::ParenthesizedExpression::make(arg_adj_lb, arg_adj_lb.get_type().no_ref()),
                    element_type_size.shallow_copy(),
                    arg_adj_lb.get_type().no_ref());

            arg_adj_ub = Nodecl::Mul::make(
                    Nodecl::ParenthesizedExpression::make(arg_adj_ub, arg_adj_ub.get_type().no_ref()),
                    element_type_size.shallow_copy(),
                    arg_adj_ub.get_type().no_ref());
        }

        // Fortran is a bit repellent checking the actual arguments types, for
        // this reason we may need to add some conversions
         TL::Type param_type = fortran_choose_int_type_from_kind(8);
        // if(!arg_size.get_type().no_ref().is_same_type(param_type))
            arg_size = Nodecl::Conversion::make(arg_size, param_type);

        //if(!arg_adj_lb.get_type().no_ref().is_same_type(param_type))
            arg_adj_lb = Nodecl::Conversion::make(arg_adj_lb, param_type);

        //if(!arg_adj_ub.get_type().no_ref().is_same_type(param_type))
            arg_adj_ub = Nodecl::Conversion::make(arg_adj_ub, param_type);


        arguments_list.append(arg_size);
        arguments_list.append(arg_adj_lb);
        arguments_list.append(arg_adj_ub);
    }

    void TaskProperties::compute_arguments_dependence_fortran(
            TL::DataReference& data_ref,
            TL::Symbol handler,
            // Out
            Nodecl::List& arguments_list)
    {
        Nodecl::NodeclBase arg1_handler;
        Nodecl::NodeclBase arg2_sym_identifier;
        Nodecl::NodeclBase arg3_dep_text;
        Nodecl::NodeclBase arg4_base_address;

        // 1st argument: task handler
         arg1_handler = Nodecl::Conversion::make(
                handler.make_nodecl(/* set_ref_type */ true),
                handler.get_type());

        // 2nd argument: sym identifier
        arg2_sym_identifier = const_value_to_nodecl(const_value_get_minus_one(4, 1));

        // 3rd argument: dependence text
        std::string dependence_text = Codegen::get_current().codegen_to_str(data_ref, data_ref.retrieve_context());
        arg3_dep_text = const_value_to_nodecl(
                const_value_make_string_null_ended(
                    dependence_text.c_str(),
                    strlen(dependence_text.c_str())));

        // 4rd argument: base address of the expression
        arg4_base_address = Nodecl::Conversion::make(
                data_ref.get_base_address().shallow_copy(),
                TL::Type::get_void_type().get_pointer_to());

        arguments_list.append(arg1_handler);
        arguments_list.append(arg2_sym_identifier);
        arguments_list.append(arg3_dep_text);
        arguments_list.append(arg4_base_address);

        TL::Type data_type = data_ref.get_data_type();

        if (!data_type.is_array())
        {
            // The current dependence is not an array

            Nodecl::NodeclBase arg5_size, arg6_lower_bound, arg7_upper_bound;

            // 5th argument: size = sizeof(data_type)
            arg5_size = data_ref.get_sizeof().shallow_copy();

            // 6th argument: lower_bound = 0
            arg6_lower_bound = const_value_to_nodecl(const_value_get_zero(8, 0));

            // 7th argument: upper_bound = sizeof(data_type)
            arg7_upper_bound = arg5_size.shallow_copy();

            arguments_list.append(arg5_size);
            arguments_list.append(arg6_lower_bound);
            arguments_list.append(arg7_upper_bound);
        }
        else
        {
            compute_dimensions_dependence_fortran(
                    data_ref, data_type, arguments_list);
        }
    }

    void TaskProperties::register_dependence_fortran(
        TL::DataReference &data_ref,
        TL::Symbol handler,
        Nodecl::Utils::SymbolMap &symbol_map,
        TL::Symbol register_fun,
        Nodecl::List &register_statements)
    {
        std::string reduction_register_fun_name = "nanos_register_region_reduction_depinfo";
        bool is_reduction = reduction_register_fun_name.compare(
                0, std::string::npos, register_fun.get_name(),
                0, reduction_register_fun_name.length()) == 0;

        Nodecl::List args;

        if (is_reduction)
        {
            ERROR_CONDITION(data_ref.get_data_type().is_array(), "Array reductions not supported", 0);

            compute_reduction_arguments_dependence_fortran(
                    data_ref, args);

            // Increment number of registered reductions for the task (used as id when registering)
            num_reductions++;
        }

        compute_arguments_dependence_fortran(
                data_ref, handler, args);

        Nodecl::NodeclBase function_call
            = Nodecl::ExpressionStatement::make(Nodecl::FunctionCall::make(
                register_fun.make_nodecl(/* set_ref_type */ true),
                args,
                /* alternate-symbol */ Nodecl::NodeclBase::null(),
                /* function-form */ Nodecl::NodeclBase::null(),
                TL::Type::get_void_type()));

        register_statements.append(Nodecl::Utils::deep_copy(function_call, TL::Scope::get_global_scope(), symbol_map));
    }

    void TaskProperties::register_multidependence_fortran(
        TL::DataReference &data_ref,
        TL::Symbol handler,
        Nodecl::Utils::SymbolMap &symbol_map,
        TL::Symbol register_fun,
        TL::Scope scope,
        // Out
        Nodecl::List &register_statements)
    {
        TL::ObjectList<TL::DataReference::MultiRefIterator> multireferences = data_ref.multireferences();

        Nodecl::Utils::SimpleSymbolMap extended_symbol_map(&symbol_map);
        TL::ObjectList<TL::Symbol> current_locals;
        TL::Counter &ctr = TL::CounterManager::get_counter("nanos6-multideps");
        for (TL::ObjectList<TL::DataReference::MultiRefIterator>::iterator it2 = multireferences.begin();
                it2 != multireferences.end();
                it2++)
        {
            std::stringstream ss;
            ss << it2->first.get_name() << "_tmp_" << (int)ctr;
            ctr++;
            std::string ind_var_name = ss.str();

            TL::Symbol local_sym = scope.new_symbol(ind_var_name);
            local_sym.get_internal_symbol()->kind = SK_VARIABLE;
            local_sym.get_internal_symbol()->type_information =
                ::get_signed_int_type();
            symbol_entity_specs_set_is_user_declared(local_sym.get_internal_symbol(), 1);

            extended_symbol_map.add_map(it2->first, local_sym);
            current_locals.append(local_sym);

            CXX_LANGUAGE()
            {
                register_statements.append(
                        Nodecl::CxxDef::make(Nodecl::NodeclBase::null(),
                            local_sym));
            }
        }

        Nodecl::NodeclBase empty_stmt = Nodecl::EmptyStatement::make();
        Nodecl::NodeclBase body = Nodecl::List::make(empty_stmt);

        for (TL::ObjectList<TL::DataReference::MultiRefIterator>::reverse_iterator it2 = multireferences.rbegin();
                it2 != multireferences.rend();
                it2++)
        {
            ERROR_CONDITION(!it2->second.is<Nodecl::Range>(), "Invalid Node", 0);
            Nodecl::Range range = it2->second.as<Nodecl::Range>();
            Nodecl::NodeclBase lower = range.get_lower().shallow_copy();
            Nodecl::NodeclBase upper = range.get_upper().shallow_copy();
            Nodecl::NodeclBase stride = range.get_stride().shallow_copy();

            TL::Symbol ind_var = extended_symbol_map.map(it2->first);

            Nodecl::NodeclBase loop_control =
                Nodecl::RangeLoopControl::make(
                        // ind-var
                        ind_var.make_nodecl(/* set_ref_type */ true),
                        lower,
                        upper,
                        stride);

            Nodecl::NodeclBase for_stmt =
                Nodecl::ForStatement::make(loop_control, body, /* loop-name */ Nodecl::NodeclBase::null());
            body = Nodecl::List::make(for_stmt);
        }

        Nodecl::NodeclBase base_exp = data_ref;
        while (base_exp.is<Nodecl::MultiExpression>())
            base_exp = base_exp.as<Nodecl::MultiExpression>().get_base();

        TL::DataReference base_data_ref = base_exp;
        Nodecl::List base_reg;

        register_dependence_fortran(
                base_data_ref,
                handler,
                extended_symbol_map,
                register_fun,
                // Out
                base_reg);

        empty_stmt.replace(base_reg);

        register_statements.append(body);
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
        unpack_parameter_types.append(TL::Type::get_void_type().get_pointer_to());

        AddParameter add_params_functor(
                /* out */ unpack_parameter_names,
                /* out */ unpack_parameter_types,
                /* out */ symbols_to_param_names);

        captured_value.map(add_params_functor);
        shared.map(add_params_functor);
        reduction.map(add_params_functor);

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

        MapSymbols map_symbols_functor(
                param_to_symbol,
                symbols_to_param_names,
                /* out */ symbol_map,
                /* out */ parameters_to_update_type);

        captured_value.map(map_symbols_functor);
        shared.map(map_symbols_functor);
        reduction.map(map_symbols_functor);


        // Now fix the types of runtime sized types prior anything else
        for (TL::ObjectList<TL::Symbol>::iterator it
             = parameters_to_update_type.begin();
             it != parameters_to_update_type.end();
             it++)
        {
            it->set_type(rewrite_type(
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
            { dep_in,    "nanos_register_region_read_depinfo"              },
            { dep_out,   "nanos_register_region_write_depinfo"             },
            { dep_inout, "nanos_register_region_readwrite_depinfo"         },

            { dep_weakin,    "nanos_register_region_weak_read_depinfo"      },
            { dep_weakout,   "nanos_register_region_weak_write_depinfo"     },
            { dep_weakinout, "nanos_register_region_weak_readwrite_depinfo" },

            { dep_commutative, "nanos_register_region_commutative_depinfo" },
            { dep_concurrent,  "nanos_register_region_concurrent_depinfo"  },

            { dep_reduction, "nanos_register_region_reduction_depinfo" },
        };

        for (DependencesSet *dep_set = deps;
             dep_set != (DependencesSet *)(&deps + 1);
             dep_set++)
        {
            TL::ObjectList<Nodecl::NodeclBase> &dep_list = dep_set->dep_list;

            if (dep_list.empty())
                continue;

            for (TL::ObjectList<Nodecl::NodeclBase>::iterator it
                 = dep_list.begin();
                 it != dep_list.end();
                 it++)
            {
                TL::DataReference data_ref = *it;
                TL::Type data_type = data_ref.get_data_type();

                TL::Symbol register_fun;
                {
                    int max_dimensions = phase->get_deps_max_dimensions();
                    ERROR_CONDITION(data_type.is_array() &&
                            (data_type.get_num_dimensions() > max_dimensions),
                            "Maximum number of data dimensions allowed is %d",
                            max_dimensions);

                    int num_dims_dep = data_type.is_array() ? data_type.get_num_dimensions() : 1;
                    std::stringstream ss;
                    ss << dep_set->func_name << num_dims_dep;

                    register_fun = global_context.get_symbol_from_name(ss.str());
                    if (!register_fun.is_valid())
                    {
                        fatal_error(
                                "'%s' function not found while trying to register dependences\n",
                                ss.str().c_str());
                    }
                }

                Nodecl::List register_statements;
                if (!data_ref.is_multireference())
                {
                    register_dependence_fortran(
                            data_ref,
                            handler,
                            symbol_map,
                            register_fun,
                            // Out
                            register_statements);
                }
                else
                {
                    register_multidependence_fortran(
                            data_ref,
                            handler,
                            symbol_map,
                            register_fun,
                            dep_fun_inside_scope,
                            // Out
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
            const TL::ObjectList<ReductionItem>& reduction;
            const TL::ObjectList<TL::Symbol> local;

            RewriteExpression(TL::Symbol arg_,
                    field_map_t& field_map_,
                    const TL::ObjectList<TL::Symbol> &shared_,
                    const TL::ObjectList<ReductionItem> &reduction_,
                    const TL::ObjectList<TL::Symbol> &local_)
                : arg(arg_), field_map(field_map_), shared(shared_),
                  reduction(reduction_), local(local_)
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
                    if ((shared.contains(sym) || reduction.contains(sym))
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

        RewriteExpression r(arg, field_map, shared, reduction, local);
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

    void TaskProperties::create_cost_function()
    {
        if (cost_clause.is_null())
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
            = rewrite_expression_using_args(arg, cost_clause, /* locals */
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

    void TaskProperties::create_priority_function()
    {
        // Skip this function if the current task doesn't have a priority clause
        if (priority_clause.is_null())
            return;

        TL::ObjectList<std::string> parameter_names(1, "arg");
        TL::ObjectList<TL::Type> parameter_types(
                1, info_structure.get_lvalue_reference_to());

        std::string priority_name;
        {
            TL::Counter &counter
                = TL::CounterManager::get_counter("nanos6-outline");
            std::stringstream ss;
            ss << "nanos6_priority_" << (int)counter;
            counter++;
            priority_name = ss.str();
        }

        priority_function = SymbolUtils::new_function_symbol(
                related_function,
                priority_name,
                TL::Type::get_size_t_type(),
                parameter_names,
                parameter_types);

        Nodecl::NodeclBase priority_function_code;
        Nodecl::NodeclBase priority_empty_stmt;

        SymbolUtils::build_empty_body_for_function(
                priority_function,
                priority_function_code,
                priority_empty_stmt);

        TL::Scope scope_inside_priority =
            priority_empty_stmt.retrieve_context();

        TL::Symbol arg = scope_inside_priority.get_symbol_from_name("arg");
        ERROR_CONDITION(!arg.is_valid(), "Invalid symbol", 0);

        Nodecl::NodeclBase priority_expr = rewrite_expression_using_args(
                arg,
                priority_clause,
                /* local_symbols */ TL::ObjectList<TL::Symbol>());

        if (!priority_expr.get_type()
                .is_same_type(TL::Type::get_size_t_type()))
        {
            priority_expr = Nodecl::Conversion::make(
                    priority_expr,
                    TL::Type::get_size_t_type(),
                    priority_expr.get_locus());
        }

        Nodecl::NodeclBase return_stmt = Nodecl::ReturnStatement::make(
                priority_expr,
                priority_expr.get_locus());

        priority_empty_stmt.replace(return_stmt);

        Nodecl::Utils::prepend_to_enclosing_top_level_location(
                task_body,
                priority_function_code);
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

        // 3. Traversing REDUCTION variables
        for (TL::ObjectList<ReductionItem>::iterator it = reduction.begin();
                it != reduction.end();
                it++)
        {
            ERROR_CONDITION(field_map.find(it->symbol) == field_map.end(),
                    "Symbol is not mapped", 0);

            Nodecl::NodeclBase rhs = it->symbol.make_nodecl(/* set_ref_type */ true);
            rhs = Nodecl::Reference::make(
                    rhs,
                    rhs.get_type().no_ref().get_pointer_to());

            std::string update_address_fun_name = "nanos_get_original_reduction_address";

            TL::Symbol update_address_fun =
                TL::Scope::get_global_scope().get_symbol_from_name(update_address_fun_name);
            if (!update_address_fun.is_valid())
            {
                fatal_error(
                        "'%s' function not found while trying to register dependences\n",
                        update_address_fun_name.c_str());
            }

            rhs = Nodecl::Conversion::make(
                    Nodecl::FunctionCall::make(
                        update_address_fun.make_nodecl(/* set_ref_type */ true),
                        Nodecl::List::make(rhs),
                        /* alternate_name */ Nodecl::NodeclBase::null(),
                        /* function_form */ Nodecl::NodeclBase::null(),
                        TL::Type::get_void_type().get_pointer_to()),
                    rhs.get_type());

            rhs.set_text("C");

            TL::Type lhs_type =
                field_map[it->symbol].get_type().no_ref().get_lvalue_reference_to();

            Nodecl::NodeclBase lhs =
                Nodecl::ClassMemberAccess::make(
                        Nodecl::Dereference::make(
                            args.make_nodecl(/* set_ref_type */ true),
                            args.get_type().points_to().get_lvalue_reference_to()),
                        field_map[it->symbol].make_nodecl(),
                        /* member_literal */ Nodecl::NodeclBase::null(),
                        lhs_type);

            Nodecl::NodeclBase current_captured_stmt = Nodecl::ExpressionStatement::make(
                    Nodecl::Assignment::make(
                        lhs,
                        rhs,
                        lhs_type));

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
