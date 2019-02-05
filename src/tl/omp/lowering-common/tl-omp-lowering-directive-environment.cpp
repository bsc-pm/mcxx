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


#include "tl-omp-lowering-directive-environment.hpp"
//#include "tl-nanos6-interface.hpp"

#include "tl-nodecl-visitor.hpp"

#include "cxx-diagnostic.h"


namespace TL { namespace OpenMP { namespace Lowering {

    //! This visitors traverses the environment of a directive and fills the DirectiveEnvironment structure
    struct DirectiveEnvironmentVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            DirectiveEnvironment& _env;

            void not_supported(const std::string &feature, Nodecl::NodeclBase n)
            {
                error_printf_at(n.get_locus(), "%s is not supported\n", feature.c_str());
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
                warn_printf_at(n.get_locus(), "%s is ignored\n", feature.c_str());
            }

            void ignored_seq(const std::string &feature, Nodecl::List l)
            {
                for (Nodecl::List::iterator it = l.begin(); it != l.end(); it++)
                {
                    ignored(feature, *it);
                }
            }

        public:
            DirectiveEnvironmentVisitor(DirectiveEnvironment& env) : _env(env)
            {}

            virtual void visit(const Nodecl::OpenMP::Firstprivate &n)
            {
                _env._firstprivate.insert(
                        n.get_symbols()
                        .as<Nodecl::List>()
                        .to_object_list()
                        .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol));
            }

            virtual void visit(const Nodecl::OpenMP::Shared &n)
            {
                _env.shared.insert(
                        n.get_symbols()
                        .as<Nodecl::List>()
                        .to_object_list()
                        .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol));
            }

            virtual void visit(const Nodecl::OpenMP::Private &n)
            {
                _env.private_.insert(
                        n.get_symbols()
                        .as<Nodecl::List>()
                        .to_object_list()
                        .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol));
            }

            virtual void visit(const Nodecl::OpenMP::Lastprivate &n)
            {
                _env.lastprivate.insert(
                        n.get_symbols()
                        .as<Nodecl::List>()
                        .to_object_list()
                        .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol));
            }

            virtual void visit(const Nodecl::OpenMP::FirstLastprivate &n)
            {
                _env.firstlastprivate.insert(
                        n.get_symbols()
                        .as<Nodecl::List>()
                        .to_object_list()
                        .map<TL::Symbol>(&Nodecl::NodeclBase::get_symbol));
            }

            virtual void visit(const Nodecl::OpenMP::If &n)
            {
                _env.if_clause = n.get_condition();
            }

            virtual void visit(const Nodecl::OpenMP::Reduction &n)
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

                    _env.reduction.insert(ReductionItem(reduction_symbol, reduction_type, red, /* isWeak */ false));
                }
            }

            virtual void visit(const Nodecl::OpenMP::InReduction &n)
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

                    _env.in_reduction.insert(ReductionItem(reduction_symbol, reduction_type, red, /* isWeak */ false));
                }
            }

            virtual void visit(const Nodecl::OmpSs::WeakReduction &n)
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

                    _env.reduction.insert(ReductionItem(reduction_symbol, reduction_type, red, /* isWeak */ true));
                }
            }

            template < typename T >
                void handle_dependences(const T& n, TL::ObjectList<Nodecl::NodeclBase>& dep_list)
                {
                    _env.any_task_dependence = true;
                    dep_list.append(n.get_exprs().template as<Nodecl::List>().to_object_list());
                }

            virtual void visit(const Nodecl::OpenMP::DepIn &n)
            {
                handle_dependences(n, _env.dep_in);
            }

            virtual void visit(const Nodecl::OpenMP::DepOut &n)
            {
                handle_dependences(n, _env.dep_out);
            }

            virtual void visit(const Nodecl::OpenMP::DepInout &n)
            {
                handle_dependences(n, _env.dep_inout);
            }

            virtual void visit(const Nodecl::OmpSs::DepWeakIn &n)
            {
                handle_dependences(n, _env.dep_weakin);
            }

            virtual void visit(const Nodecl::OmpSs::DepWeakOut &n)
            {
                handle_dependences(n, _env.dep_weakout);
            }

            virtual void visit(const Nodecl::OmpSs::DepWeakInout &n)
            {
                handle_dependences(n, _env.dep_weakinout);
            }

            virtual void visit(const Nodecl::OmpSs::DepCommutative &n)
            {
                handle_dependences(n, _env.dep_commutative);
            }

            virtual void visit(const Nodecl::OmpSs::DepWeakCommutative &n)
            {
                handle_dependences(n, _env.dep_weakcommutative);
            }

            virtual void visit(const Nodecl::OmpSs::DepConcurrent &n)
            {
                handle_dependences(n, _env.dep_concurrent);
            }

            virtual void visit(const Nodecl::OmpSs::DepReduction &n)
            {
                handle_dependences(n, _env.dep_reduction);
            }

            virtual void visit(const Nodecl::OmpSs::DepWeakReduction &n)
            {
                handle_dependences(n, _env.dep_weakreduction);
            }

            virtual void visit(const Nodecl::OpenMP::Final &n)
            {
                _env.final_clause = n.get_condition();
            }

            virtual void visit(const Nodecl::OpenMP::Priority &n)
            {
                _env.priority_clause = n.get_priority();
            }

            virtual void visit(const Nodecl::OpenMP::Untied &n)
            {
                _env.is_tied = false;
            }

            virtual void visit(const Nodecl::OmpSs::Wait &n)
            {
                _env.wait_clause = true;
            }

            virtual void visit(const Nodecl::OmpSs::TaskLabel &n)
            {
                _env.task_label = n.get_text();
            }

            virtual void visit(const Nodecl::OmpSs::Target &n)
            {
                Nodecl::List devices = n.get_devices().as<Nodecl::List>();
                for (Nodecl::List::iterator it = devices.begin();
                        it != devices.end();
                        it++)
                {
                    _env.device_names.append(it->get_text().c_str());
                }
                walk(n.get_items());
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
                _env.locus_of_task_declaration = n.get_locus();
            }

            virtual void visit(const Nodecl::OpenMP::TaskIsTaskwait &n)
            {
                _env.task_is_taskwait_with_deps = true;
            }

            virtual void visit(const Nodecl::OmpSs::TaskIsLoop &n)
            {
                _env.task_is_loop = true;
            }

            virtual void visit(const Nodecl::OmpSs::TaskIsTaskCall &n)
            {
                _env.task_is_taskcall = true;
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
                _env.ndrange = n.get_ndrange_expressions().as<Nodecl::List>().to_object_list();
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
                _env.cost_clause = n.get_cost();
            }

            virtual void visit(const Nodecl::OmpSs::Chunksize &n)
            {
                _env.chunksize = n.get_chunksize();
            }
    };

    DirectiveEnvironment::DirectiveEnvironment(Nodecl::NodeclBase environment) :
        is_tied(true), task_is_loop(false), task_is_taskwait_with_deps(false),
        task_is_taskcall(false), wait_clause(false),
        any_task_dependence(false), locus_of_task_declaration(NULL)
    {
        // Traversing & filling the directive environment
        DirectiveEnvironmentVisitor visitor(*this);
        visitor.walk(environment);

        // Fixing some data-sharings + capturing some special symbols
        compute_captured_values();
        fix_data_sharing_of_this();

        // Empty the '_firstprivate' list, since it won't be use from this point on
        _firstprivate.erase(_firstprivate.begin(), _firstprivate.end());
    }

    void DirectiveEnvironment::compute_captured_values()
    {
        // We need to guarrantee some order in the 'captured_value' list: The
        // captured expression symbols that appear in a type need to appear
        // before the symbols of that type. Do not reorder these statements.

        // First, add missing symbols to '_firstprivate' list
        firstprivatize_symbols_without_data_sharing();

        // At this point all symbols should have a data-sharing, so we look for
        // saved expressions in their type and add them to the 'captured_value' list
        compute_captured_saved_expressions();

        // Due to default data sharing rules, saved expressions may end up in the
        // shared set after expanding an inner construct
        fix_data_sharing_of_captured_saved_expressions();

        // Insert the remaining symbols in '_firstprivate' at the end of the
        // 'captured_value' list
        captured_value.insert(_firstprivate);
    }

    bool DirectiveEnvironment::symbol_has_data_sharing_attribute(TL::Symbol sym) const
    {
        return shared.contains(sym)         ||
               private_.contains(sym)       ||
               _firstprivate.contains(sym)  ||
               captured_value.contains(sym);
    }

    struct FirstprivateSymbolsWithoutDataSharing : public Nodecl::ExhaustiveVisitor<void>
    {
        DirectiveEnvironment& _env;
        TL::ObjectList<TL::Symbol> _ignore_symbols;

        FirstprivateSymbolsWithoutDataSharing(DirectiveEnvironment& tp) : _env(tp)
        {}

        void operator()(Nodecl::NodeclBase node)
        {
            walk(node);
        }

        void visit(const Nodecl::MultiExpression& node)
        {
            Nodecl::List iterators = node.get_iterators().as<Nodecl::List>();
            for (Nodecl::List::const_iterator it = iterators.begin();
                    it != iterators.end();
                    it++)
            {
                _ignore_symbols.push_back(it->as<Nodecl::MultiExpressionIterator>().get_symbol());
            }
            // The iterator of a MultiExpression has to be ignored!
            Nodecl::ExhaustiveVisitor<void>::visit(node);

            for (Nodecl::List::const_iterator it = iterators.begin();
                    it != iterators.end();
                    it++)
            {
                _ignore_symbols.pop_back();
            }
        }

        void visit(const Nodecl::Symbol& node)
        {
            TL::Symbol sym = node.get_symbol();
            if (!sym.is_variable()
                    || sym.is_member()
                    || _ignore_symbols.contains(sym)
                    || _env.symbol_has_data_sharing_attribute(sym))
                return;

            _env._firstprivate.insert(sym);
        }

        void visit(const Nodecl::Conversion& node)
        {
            // FIXME: This should be done in Core, see issue #2766

            // int *v;
            // #pragma omp task inout( ((int (*)[N]) v)[0;M])
            Nodecl::ExhaustiveVisitor<void>::visit(node);

            TL::Type type = node.get_type();
            if (type.depends_on_nonconstant_values())
                _env.walk_type_for_saved_expressions(type);
        }
    };

    void DirectiveEnvironment::firstprivatize_symbols_without_data_sharing()
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
        dep_weakcommutative.map(fp_syms_without_data_sharing);
        dep_concurrent.map(fp_syms_without_data_sharing);
        dep_reduction.map(fp_syms_without_data_sharing);
        dep_weakreduction.map(fp_syms_without_data_sharing);

        // Other task clauses
        fp_syms_without_data_sharing(cost_clause);
        fp_syms_without_data_sharing(priority_clause);
    }

    void DirectiveEnvironment::handle_array_bound(Nodecl::NodeclBase n)
    {
        if (n.is<Nodecl::Symbol>()
                && n.get_symbol().is_saved_expression())
        {
            captured_value.insert(n.get_symbol());
        }
    }

    void DirectiveEnvironment::walk_type_for_saved_expressions(TL::Type t)
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

    void DirectiveEnvironment::compute_captured_saved_expressions()
    {
        // This code computes a list of captured expressions' symbols comming
        // from symbol types that depend on non-constant values

        // FIXME: Most of the times, the datasharing of such captured
        // expressions' symbols has already been computed in the previous Core
        // phases, so they can be found in the '_firstprivate' list. However,
        // some situations are still missing (see issues #2766 & #2767), and
        // therefore their datasharing is computed here. Whenever this is
        // fixed, the behaviour can change and the previous condition can be
        // enforced.

        for (TL::ObjectList<TL::Symbol>::iterator it = shared.begin();
                it != shared.end();
                it++)
        {
            if (it->get_type().depends_on_nonconstant_values())
                walk_type_for_saved_expressions(it->get_type());
        }

        for (TL::ObjectList<TL::Symbol>::iterator it = _firstprivate.begin();
                it != _firstprivate.end();
                it++)
        {
            if (it->get_type().depends_on_nonconstant_values())
                walk_type_for_saved_expressions(it->get_type());
        }
    }

    namespace
    {
        bool only_saved_expressions(const TL::Symbol& s)
        {
            return s.is_saved_expression();
        }
    }

    void DirectiveEnvironment::fix_data_sharing_of_captured_saved_expressions()
    {
        TL::ObjectList<TL::Symbol> saved_expressions = captured_value.filter(only_saved_expressions);

        TL::ObjectList<TL::Symbol> filtered_shared;
        for (TL::ObjectList<TL::Symbol>::iterator it = shared.begin();
                it != shared.end();
                it++)
        {
            if (!saved_expressions.contains(*it))
                filtered_shared.append(*it);
        }

        shared = filtered_shared;
    }

    namespace  {
    struct SymbolThis
    {
        private:
            TL::Symbol &_found_this;

        public:
            SymbolThis(TL::Symbol &found_this) : _found_this(found_this)
        { }

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
    void DirectiveEnvironment::fix_data_sharing_of_this()
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
}}}
