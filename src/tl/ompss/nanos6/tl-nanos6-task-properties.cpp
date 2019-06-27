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
#include "tl-nanos6-device-manager.hpp"

#include "tl-omp-lowering-utils.hpp"
#include "tl-omp-lowering-atomics.hpp"
#include "tl-omp-reduction.hpp"

#include "tl-nodecl-visitor.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-nodecl-utils-fortran.hpp"
#include "tl-symbol-utils.hpp"
#include "tl-counters.hpp"

#include "codegen-phase.hpp"

#include "cxx-typeutils.h"
#include "cxx-cexpr.h"
#include "cxx-diagnostic.h"

#include "fortran03-mangling.h"
#include "fortran03-typeutils.h"
#include "fortran03-typeenviron.h"
#include "fortran03-intrinsics.h"
#include "fortran03-scope.h"


#include <algorithm>
#include <string>
#include <set>

namespace TL { namespace Nanos6 {

    using TL::OpenMP::Lowering::ReductionItem;

    TaskProperties::TaskProperties(
            const Nodecl::OpenMP::Task& node,
            Nodecl::NodeclBase &serial_context,
            LoweringPhase* lowering_phase,
            Lower* lower)
        : _env(node.get_environment()), _serial_context(serial_context),
        _phase(lowering_phase), _lower_visitor(lower), _num_reductions(0)
    {
        TL::Counter &counter = TL::CounterManager::get_counter("nanos6-task");
        _nanos6_task_counter = (int) counter;
        counter++;

        _locus_of_task_creation = node.get_locus();

        if (_env.locus_of_task_declaration)
            _locus_of_task_declaration = _env.locus_of_task_declaration;
        else
            _locus_of_task_declaration = node.get_locus();

        _related_function = Nodecl::Utils::get_enclosing_function(node);
        _task_body = node.get_statements();

        ERROR_CONDITION(_env.device_names.size() == 0, "A task without a device name was detected", 0);

        for (TL::ObjectList<std::string>::const_iterator it = _env.device_names.begin();
                it != _env.device_names.end();
                it++)
        {
            _implementations.insert(lower->get_device_manager().get_device(*it));
        }

        if (_env.task_is_loop)
        {
            ERROR_CONDITION(!_task_body.is<Nodecl::List>(), "Unexpected node\n", 0);
            Nodecl::NodeclBase stmt = _task_body.as<Nodecl::List>().front();
            ERROR_CONDITION(!stmt.is<Nodecl::Context>(), "Unexpected node\n", 0);
            stmt = stmt.as<Nodecl::Context>().get_in_context().as<Nodecl::List>().front();
            ERROR_CONDITION(!stmt.is<Nodecl::ForStatement>(), "Unexpected node\n", 0);

            TL::ForStatement for_stmt(stmt.as<Nodecl::ForStatement>());
            _taskloop_bounds.lower_bound = for_stmt.get_lower_bound();
            _taskloop_bounds.upper_bound =
                Nodecl::Add::make(
                        for_stmt.get_upper_bound(),
                        const_value_to_nodecl_with_basic_type(
                            const_value_get_signed_int(1), get_size_t_type()),
                        TL::Type::get_size_t_type());
            _taskloop_bounds.step = for_stmt.get_step();
        }

        {
            int id = 0;
            struct DependencesSet
            {
                TL::ObjectList<Nodecl::NodeclBase> &dep_list;
            } deps[] = {
                { _env.dep_in },
                { _env.dep_out },
                { _env.dep_inout },

                { _env.dep_weakin },
                { _env.dep_weakout },
                { _env.dep_weakinout },

                { _env.dep_commutative },
                { _env.dep_concurrent },

                { _env.dep_weakcommutative },

                { _env.dep_reduction },
                { _env.dep_weakreduction }
            };

            for (DependencesSet *dep_set = deps;
                    dep_set != (DependencesSet *)(&deps + 1);
                    dep_set++)
            {
                TL::ObjectList<Nodecl::NodeclBase> &dep_list = dep_set->dep_list;
                for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = dep_list.begin();
                        it != dep_list.end();
                        it++)
                {
                    TL::DataReference data_ref = *it;
                    TL::Type data_type = data_ref.get_data_type();

                    TL::Symbol base_symbol = data_ref.get_base_symbol();
                    std::map<TL::Symbol, unsigned int>::const_iterator map_it = _dep_symbols_to_id.find(base_symbol);
                    if (map_it == _dep_symbols_to_id.end())
                    {
                        _dep_symbols_to_id[base_symbol] = id++;
                    }
                }
            }
        }
    }

    std::string TaskProperties::get_new_name(const std::string& prefix) const
    {
        std::string fixed_fun_name = _related_function.get_name();
        std::replace(fixed_fun_name.begin(), fixed_fun_name.end(), ' ', '_');

        std::stringstream ss;
        ss << prefix << "_" << fixed_fun_name << "_" << _nanos6_task_counter;
        return ss.str();
    }

    namespace {
    //! Given a name (parameter) and a list of symbols (non-static data
    //! member), this functor constructs a Nodecl::Symbol if there is a symbol
    //! whose name is exactly the same as the parameter. Otherwise, it emits an
    //! error.
    struct GetField
    {
        const TL::ObjectList<TL::Symbol>& fields;

        GetField(const TL::ObjectList<TL::Symbol>& fields_) : fields(fields_) {}


        Nodecl::NodeclBase operator()(const std::string& name) const
        {
            TL::ObjectList<TL::Symbol> l;
            ERROR_CONDITION( ( l = fields.find<std::string>(&TL::Symbol::get_name, name)).empty(),
                    "Field '%s' not found", name.c_str());
            return l[0].make_nodecl(/* set_ref_type */ true);
        }
    };
    }
    void TaskProperties::create_task_invocation_info(
        /* out */ TL::Symbol &task_invocation_info)
    {
        TL::Symbol task_invocation_info_struct = get_nanos6_class_symbol("nanos6_task_invocation_info_t");

        std::string task_invocation_info_name = get_new_name("task_invocation_info");
        task_invocation_info = TL::Scope::get_global_scope().new_symbol(task_invocation_info_name);

        task_invocation_info.get_internal_symbol()->kind = SK_VARIABLE;
        symbol_entity_specs_set_is_user_declared(task_invocation_info.get_internal_symbol(), 1);
        task_invocation_info.set_type( task_invocation_info_struct.get_user_defined_type());
        symbol_entity_specs_set_is_static(task_invocation_info.get_internal_symbol(), 1);

        TL::ObjectList<TL::Symbol> task_invocation_fields =
            task_invocation_info_struct.get_type().get_nonstatic_data_members();
        GetField get_field_task_invocation_info(task_invocation_fields);

        Nodecl::NodeclBase field_invocation_source = get_field_task_invocation_info("invocation_source");

        const char *c = locus_to_str(_locus_of_task_creation);
        Nodecl::NodeclBase init_invocation_source = const_value_to_nodecl(
            const_value_make_string_null_ended(c, strlen(c)));

        Nodecl::NodeclBase task_invocation_init = Nodecl::StructuredValue::make(
            Nodecl::List::make(Nodecl::FieldDesignator::make(
                field_invocation_source,
                init_invocation_source,
                field_invocation_source.get_type())),
            Nodecl::StructuredValueBracedImplicit::make(),
            task_invocation_info.get_type());

        task_invocation_info.set_value(task_invocation_init);

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            if (IS_CXX_LANGUAGE)
            {
                Nodecl::Utils::prepend_to_enclosing_top_level_location(
                        _task_body,
                        Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), task_invocation_info));
            }
            Nodecl::Utils::prepend_to_enclosing_top_level_location(
                    _task_body,
                    Nodecl::ObjectInit::make(task_invocation_info));
        }
        else // IS_FORTRAN_LANGUAGE
        {
            _phase->get_extra_c_code().append(Nodecl::ObjectInit::make(task_invocation_info));
            task_invocation_info = fortran_create_detached_symbol_from_static_symbol(task_invocation_info);
        }
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

            Nodecl::List arguments_list = Nodecl::List::make(
                    Nodecl::FortranActualArgument::make(task_flags.make_nodecl()),
                    Nodecl::FortranActualArgument::make(const_value_to_nodecl(const_value_get_signed_int(bit))));

            TL::Symbol intrinsic_ibset = get_fortran_intrinsic_symbol<2>("ibset", arguments_list, /* is_call */ 0);

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
        Nodecl::NodeclBase negate_condition_if_valid(Nodecl::NodeclBase cond)
        {
            if (cond.is_null())
                return cond;

            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            {
                return Nodecl::LogicalNot::make(cond, TL::Type::get_bool_type());
            }
            else // IS_FORTRAN_LANGUAGE
            {
                return Nodecl::LogicalNot::make(
                    Nodecl::ParenthesizedExpression::make(cond, TL::Type::get_bool_type()),
                    TL::Type::get_bool_type());
            }
        }
    }

    void TaskProperties::compute_task_flags(TL::Symbol task_flags, Nodecl::NodeclBase& out_stmts)
    {
        Nodecl::List new_stmts;

        // Note that depending on the base language we compute the flags of a task a bit different:
        //      * C/C++: we compute a new expression that contains all the flags
        //
        //              taskflags = ((final_expr != 0)  << 0) |
        //                          ((!if_expr != 0)    << 1) |
        //                          ((is_loop != 0)     << 2) |
        //                          ((wait_clause != 0) << 3) |
        //                          ((preallocated_args_struct != 0) << 4)
        //
        //      * Fortran: since Fortran doesn't have a simple way to work with
        //        bit fields, we generate several statements:
        //
        //              taskflags = 0;
        //              if (final_expr)  call ibset(taskflags, 0);
        //              if (!if_expr)    call ibset(taskflags, 1);
        //              if (is_loop)     call ibset(taskflags, 2);
        //              if (wait_clause) call ibset(taskflags, 3);
        //              if (preallocated_args_struct) call ibset(taskflags, 4);
        //

        bool preallocated_args_struct = IS_FORTRAN_LANGUAGE;

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            Nodecl::NodeclBase task_flags_expr;

            compute_generic_flag_c(_env.final_clause,
                    /* default value */ 0, /* bit */ 0, /* out */ task_flags_expr);

            compute_generic_flag_c(negate_condition_if_valid(_env.if_clause),
                    /* default value */ 0, /* bit */ 1, /* out */ task_flags_expr);

            compute_generic_flag_c(Nodecl::NodeclBase::null(),
                    _env.task_is_loop, /* bit */ 2, /* out */ task_flags_expr);

            compute_generic_flag_c(Nodecl::NodeclBase::null(),
                    _env.wait_clause, /* bit */ 3, /* out */ task_flags_expr);

            compute_generic_flag_c(Nodecl::NodeclBase::null(),
                    preallocated_args_struct, /* bit */ 4, /* out */ task_flags_expr);

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
                            task_flags.get_type().no_ref().get_lvalue_reference_to())));

            new_stmts.append(
                    compute_generic_flag_fortran(task_flags, _env.final_clause, /* default value */ 0, /* bit */ 0));

            new_stmts.append(
                    compute_generic_flag_fortran(task_flags, negate_condition_if_valid(_env.if_clause), /* default value */ 0, /* bit */ 1));

            new_stmts.append(
                    compute_generic_flag_fortran(task_flags, Nodecl::NodeclBase::null(), _env.task_is_loop, /* bit */ 2));

            new_stmts.append(
                    compute_generic_flag_fortran(task_flags, Nodecl::NodeclBase::null(), _env.wait_clause, /* bit */ 3));

            new_stmts.append(
                    compute_generic_flag_fortran(task_flags, Nodecl::NodeclBase::null(), preallocated_args_struct, /* bit */ 4));
        }
        out_stmts = new_stmts;
    }



    namespace {
        //! It returns whether our iterators are not independent. That is, it returns whether the
        //! values of at least one iterator depends on another iterator
        bool iterators_are_not_independent(
                const TL::ObjectList<TL::DataReference::MultiRefIterator> &multireferences)
        {
            class ExprReferencesAnIterator : public Nodecl::ExhaustiveVisitor<void>
            {
                const TL::ObjectList<TL::Symbol> &_list;
                bool _found = false;

                public:

                ExprReferencesAnIterator(const TL::ObjectList<TL::Symbol> &list) : _list(list) {}

                void visit(const Nodecl::Symbol &node)
                {
                    TL::Symbol symbol = node.get_symbol();
                    if (!symbol.is_valid() || _found)
                        return;

                    _found = _list.contains(symbol);
                }

                bool expr_references_an_iterator() const
                {
                    return _found;
                }
            };

            bool found = false;
            TL::ObjectList<TL::Symbol> iterators;
            ExprReferencesAnIterator visitor(iterators);

            for (TL::ObjectList<TL::DataReference::MultiRefIterator>::const_iterator it = multireferences.begin();
                    it != multireferences.end() && !found;
                    it++)
            {
                ERROR_CONDITION(!it->second.is<Nodecl::Range>(), "Invalid Node", 0);
                Nodecl::Range range = it->second.as<Nodecl::Range>();

                visitor.walk(range);
                found = visitor.expr_references_an_iterator();

                iterators.append(it->first);
            }
            return found;
        }

        //! This function creates a new induction variable for each iterator
        //! It also computes a map between the original iterator and the new induction variable
        void create_induction_variables_for_iterators(
                const TL::ObjectList<TL::DataReference::MultiRefIterator> &multireferences,
                TL::Scope enclosing_scope,
                // Out
                Nodecl::Utils::SimpleSymbolMap &map,
                Nodecl::List &new_stmts)
        {
            TL::Counter &ctr = TL::CounterManager::get_counter("nanos6-ind-vars");
            for (TL::ObjectList<TL::DataReference::MultiRefIterator>::const_iterator it = multireferences.begin();
                    it != multireferences.end();
                    it++)
            {
                std::stringstream ss;
                ss << it->first.get_name() << "_tmp_" << (int)ctr;
                ctr++;
                std::string ind_var_name = ss.str();

                TL::Symbol local_sym = enclosing_scope.new_symbol(ind_var_name);
                local_sym.get_internal_symbol()->kind = SK_VARIABLE;
                local_sym.get_internal_symbol()->type_information = ::get_signed_int_type();
                symbol_entity_specs_set_is_user_declared(local_sym.get_internal_symbol(), 1);

                map.add_map(it->first, local_sym);

                CXX_LANGUAGE()
                {
                    new_stmts.append(Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), local_sym));
                }
            }
        }

        //! It returns as many loop stmts as iterators we have. Note that the
        //! innermost loop has the inner_stmts as its body.
        Nodecl::List create_loop_stmts_for_iterators(
                const TL::ObjectList<TL::DataReference::MultiRefIterator> &multireferences,
                Nodecl::Utils::SimpleSymbolMap &map,
                Nodecl::List inner_stmts,
                TL::Scope scope,
                TL::Symbol related_function)
        {
            Nodecl::List result = inner_stmts;
            for (TL::ObjectList<TL::DataReference::MultiRefIterator>::const_reverse_iterator it = multireferences.rbegin();
                    it != multireferences.rend();
                    it++)
            {
                ERROR_CONDITION(!it->second.is<Nodecl::Range>(), "Invalid Node", 0);

                Nodecl::Range range = it->second.as<Nodecl::Range>();

                if (IS_FORTRAN_LANGUAGE)
                {
                    // Insert extra symbol declarations and add them to the symbol map
                    // (e.g. functions and subroutines declared in other scopes)
                    Nodecl::Utils::Fortran::ExtraDeclsVisitor fun_visitor(
                            map,
                            scope,
                            related_function);
                    fun_visitor.insert_extra_symbols(range.get_lower());
                    fun_visitor.insert_extra_symbols(range.get_upper());
                    fun_visitor.insert_extra_symbols(range.get_stride());
                }

                Nodecl::NodeclBase lower = Nodecl::Utils::deep_copy(range.get_lower(), scope, map);
                Nodecl::NodeclBase upper = Nodecl::Utils::deep_copy(range.get_upper(), scope, map);
                Nodecl::NodeclBase stride = Nodecl::Utils::deep_copy(range.get_stride(), scope, map);

                TL::Symbol ind_var = map.map(it->first);

                Nodecl::NodeclBase loop_control;
                if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                {
                    loop_control = Nodecl::LoopControl::make(
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
                }
                else /* IS_FORTRAN_LANGUAGE */
                {
                    loop_control = Nodecl::RangeLoopControl::make(
                            ind_var.make_nodecl(/* set_ref_type */ true),
                            lower,
                            upper,
                            stride);
                }

                // Note that we are not creating any context / compound stmt.
                // Thus, the body has to be always an statement
                Nodecl::NodeclBase for_stmt =
                    Nodecl::ForStatement::make(
                            loop_control, result, /* loop-name */ Nodecl::NodeclBase::null());

                result = Nodecl::List::make(for_stmt);
            }
            return result;
        }

        //! It returns an expression that evaluates to the total number of dependences
        //! of a multidependence whose iterators are independent
        Nodecl::NodeclBase compute_num_of_dependences_for_independent_iterators(
                const TL::ObjectList<TL::DataReference::MultiRefIterator> &multireferences)
        {
            TL::Type size_t_type = TL::Type::get_size_t_type();
            Nodecl::NodeclBase curr_dyn_num_deps =
                const_value_to_nodecl(const_value_get_one(size_t_type.get_size(), 0));

            for (TL::ObjectList<TL::DataReference::MultiRefIterator>::const_reverse_iterator it = multireferences.rbegin();
                    it != multireferences.rend();
                    it++)
            {
                ERROR_CONDITION(!it->second.is<Nodecl::Range>(), "Invalid Node", 0);
                Nodecl::Range range = it->second.as<Nodecl::Range>();

                // curr_num_deps = (curr_num_deps * ((upper-lower+1)/stride));
                curr_dyn_num_deps = Nodecl::ParenthesizedExpression::make(
                        Nodecl::Mul::make(
                            curr_dyn_num_deps,
                            Nodecl::ParenthesizedExpression::make(
                                Nodecl::Div::make(
                                    Nodecl::ParenthesizedExpression::make(
                                        Nodecl::Add::make(
                                            Nodecl::Minus::make(
                                                range.get_upper().shallow_copy(),
                                                range.get_lower().shallow_copy(),
                                                size_t_type),
                                            const_value_to_nodecl(const_value_get_one(size_t_type.get_size(), 1)),
                                            size_t_type),
                                        size_t_type),
                                    range.get_stride().shallow_copy(),
                                    size_t_type),
                                size_t_type),
                            size_t_type),
                        size_t_type);
            }
            return curr_dyn_num_deps;
        }
    }

    void TaskProperties::compute_number_of_dependences(
            TL::Symbol num_deps,
            TL::Scope enclosing_scope,
            /* out */
            Nodecl::NodeclBase &num_deps_stmts)
    {
        struct DependencesSet
        {
            TL::ObjectList<Nodecl::NodeclBase> &dep_list;
        } deps[] = {
            { _env.dep_in    },
            { _env.dep_out   },
            { _env.dep_inout },

            { _env.dep_weakin    },
            { _env.dep_weakout   },
            { _env.dep_weakinout },

            { _env.dep_commutative },
            { _env.dep_concurrent  },

            { _env.dep_weakcommutative },

            { _env.dep_reduction     },
            { _env.dep_weakreduction },
        };

        // Common dependences
        int static_num_deps = 0;

        // Multideps where iterators don't depend on other iterators
        Nodecl::NodeclBase dyn_num_deps = const_value_to_nodecl(const_value_get_zero(TL::Type::get_size_t_type().get_size(), 0));

        // Statements that compute the number of dependences of multideps where at
        // least one iterator depends on the value of another iterator
        Nodecl::List new_stmts;
        for (DependencesSet *dep_set = deps;
                dep_set != (DependencesSet *)(&deps + 1);
                dep_set++)
        {
            TL::ObjectList<Nodecl::NodeclBase> &dep_list = dep_set->dep_list;

            if (dep_list.empty())
                continue;

            for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = dep_list.begin();
                    it != dep_list.end();
                    it++)
            {
                TL::DataReference data_ref = *it;
                TL::Type data_type = data_ref.get_data_type();

                Nodecl::NodeclBase curr_num_deps;
                Nodecl::List register_statements;
                if (!data_ref.is_multireference())
                {
                    ++static_num_deps;
                }
                else
                {
                    TL::ObjectList<TL::DataReference::MultiRefIterator> multireferences =
                        data_ref.get_iterators_of_multireference();

                    if (iterators_are_not_independent(multireferences))
                    {
                        Nodecl::Utils::SimpleSymbolMap extended_symbol_map;
                        create_induction_variables_for_iterators(
                                multireferences, enclosing_scope, extended_symbol_map, new_stmts);

                        Nodecl::List inner_stmts =
                            Nodecl::List::make(
                                Nodecl::ExpressionStatement::make(
                                    Nodecl::Postincrement::make(num_deps.make_nodecl(), TL::Type::get_size_t_type())));

                        Nodecl::List loop_stmts =
                            create_loop_stmts_for_iterators(
                                    multireferences, extended_symbol_map, inner_stmts, enclosing_scope, _related_function);

                        new_stmts.append(loop_stmts);
                    }
                    else
                    {
                        Nodecl::NodeclBase curr_dyn_num_deps =
                            compute_num_of_dependences_for_independent_iterators(multireferences);
                        dyn_num_deps = Nodecl::Add::make(dyn_num_deps, curr_dyn_num_deps, dyn_num_deps.get_type());
                    }
                }
            }
        }

        Nodecl::NodeclBase value =
            Nodecl::Add::make(
                    const_value_to_nodecl(
                        const_value_get_integer(
                            static_num_deps,
                            TL::Type::get_size_t_type().get_size(),
                            0)),
                    dyn_num_deps,
                    TL::Type::get_size_t_type());

        num_deps.set_value(value);

        num_deps_stmts = new_stmts;
    }

void TaskProperties::create_task_implementations_info(
        /* out */
        TL::Symbol &implementations)
{
    TL::Symbol task_implementation_info_struct = get_nanos6_class_symbol("nanos6_task_implementation_info_t");

    TL::ObjectList<TL::Symbol> fields = task_implementation_info_struct.get_type().get_nonstatic_data_members();
    GetField get_field(fields);

    // This stuff at some point should be implementation dependant
    TL::Symbol constraints_function = create_constraints_function();

    TL::ObjectList<Nodecl::NodeclBase> implementations_init;
    for (TL::ObjectList< std::shared_ptr<Device> >::const_iterator it = _implementations.begin();
            it != _implementations.end();
            it++)
    {
        TL::ObjectList<Nodecl::NodeclBase> field_init;
        // .device_type_id
        {
            Nodecl::NodeclBase field = get_field("device_type_id");
            Nodecl::NodeclBase value = (*it)->get_device_type_id().make_nodecl(/*ref_type*/ true);
            field_init.append(
                    Nodecl::FieldDesignator::make(field, value, value.get_type()));
        }

            // .run
            {
                Nodecl::NodeclBase field = get_field("run");
                Nodecl::NodeclBase value;
                TL::Symbol task_region_function = create_task_region_function(*it);

                if (task_region_function.is_valid())
                {
                    value = Nodecl::Conversion::make(task_region_function.make_nodecl(/*ref_type*/ true), field.get_type().no_ref());
                    value.set_text("C");
                }
                else
                    value = const_value_to_nodecl(const_value_get_signed_int(0));

                field_init.append(
                        Nodecl::FieldDesignator::make(field, value, value.get_type()));
            }

            // .get_constraints
            {
                Nodecl::NodeclBase field = get_field("get_constraints");
                Nodecl::NodeclBase value;
                if (constraints_function.is_valid())
                {
                    value = Nodecl::Conversion::make(constraints_function.make_nodecl(/*ref_type*/ true), field.get_type().no_ref());
                    value.set_text("C");
                }
                else
                    value = const_value_to_nodecl(const_value_get_signed_int(0));

                field_init.append(
                        Nodecl::FieldDesignator::make(field, value, value.get_type()));
            }

            // .task_label
            {
                Nodecl::NodeclBase field = get_field("task_label");
                Nodecl::NodeclBase value;

                if (!_env.task_label.empty())
                {
                    char* c = xstrdup(_env.task_label.c_str());
                    value = const_value_to_nodecl(const_value_make_string_null_ended(c, strlen(c)));
                    DELETE(c);
                }
                else
                    value = const_value_to_nodecl(const_value_get_signed_int(0));

                field_init.append(
                        Nodecl::FieldDesignator::make(field, value, value.get_type()));
            }

            // .declaration_source
            {
                Nodecl::NodeclBase field = get_field("declaration_source");
                const char* c = locus_to_str(_locus_of_task_declaration);
                Nodecl::NodeclBase value = const_value_to_nodecl(const_value_make_string_null_ended(c, strlen(c)));

                field_init.append(
                        Nodecl::FieldDesignator::make(field, value, value.get_type()));
            }

            // .run_wrapper
            {
                Nodecl::NodeclBase field = get_field("run_wrapper");

                Nodecl::NodeclBase value = const_value_to_nodecl(const_value_get_signed_int(0));

                field_init.append(
                        Nodecl::FieldDesignator::make(field, value, value.get_type()));
            }

            implementations_init.append(
                    Nodecl::StructuredValue::make(
                        Nodecl::List::make(field_init),
                        Nodecl::StructuredValueBracedImplicit::make(),
                    task_implementation_info_struct.get_user_defined_type()));
        }

        int num_impl = _implementations.size();
        TL::Type array_type = task_implementation_info_struct
            .get_user_defined_type()
            .get_array_to(const_value_to_nodecl(const_value_get_signed_int(num_impl)), TL::Scope::get_global_scope());

        std::string implementations_name = get_new_name("implementations_var");

        create_static_variable_depending_on_function_context(
            implementations_name,
            array_type,
            _task_body,
            _phase,
            implementations);

        Nodecl::NodeclBase implementations_value = Nodecl::StructuredValue::make(
                Nodecl::List::make(implementations_init),
                Nodecl::StructuredValueBracedImplicit::make(),
                implementations.get_type().no_ref());

        implementations.set_value(implementations_value);
    }

    void TaskProperties::create_task_info(
            TL::Symbol implementations,
            /* out */
            TL::Symbol &task_info)
    {
        // Note: Reduction functions need to be created before dependences functions, for UDRs
        TL::Symbol reduction_initializers, reduction_combiners;
        create_reduction_functions(reduction_initializers, reduction_combiners);

        TL::Symbol deps_function = create_dependences_function();
        TL::Symbol priority_function = create_priority_function();
        TL::Symbol destroy_function = create_destroy_function();
        TL::Symbol duplicate_function = create_duplicate_function();

        TL::Symbol task_info_struct = get_nanos6_class_symbol("nanos6_task_info_t");
        std::string task_info_name = get_new_name("task_info_var");

        create_static_variable_depending_on_function_context(
            task_info_name,
            task_info_struct.get_user_defined_type(),
            _task_body,
            _phase,
            task_info);

        TL::ObjectList<TL::Symbol> fields = task_info_struct.get_type().get_nonstatic_data_members();
        GetField get_field(fields);

        TL::ObjectList<Nodecl::NodeclBase> field_init;

        // .num_symbols
        {
            Nodecl::NodeclBase field_num_symbols = get_field("num_symbols");
            Nodecl::NodeclBase init_num_symbols =
                const_value_to_nodecl(const_value_get_unsigned_int(_dep_symbols_to_id.size()));
            field_init.append(
                    Nodecl::FieldDesignator::make(field_num_symbols,
                        init_num_symbols,
                        field_num_symbols.get_type()));
        }

        // .register_depinfo
        {
            Nodecl::NodeclBase field_register_depinfo = get_field("register_depinfo");
            Nodecl::NodeclBase init_register_depinfo;
            if (deps_function.is_valid())
            {
                init_register_depinfo = deps_function.make_nodecl(/* set_ref_type */ true);
                init_register_depinfo = Nodecl::Conversion::make(
                        init_register_depinfo,
                        field_register_depinfo.get_type().no_ref());
                init_register_depinfo.set_text("C");
            }
            else
            {
                init_register_depinfo = const_value_to_nodecl(const_value_get_signed_int(0));
            }

            field_init.append(
                    Nodecl::FieldDesignator::make(field_register_depinfo,
                        init_register_depinfo,
                        field_register_depinfo.get_type()));
        }

        // .get_priority
        {
            Nodecl::NodeclBase field_get_priority = get_field("get_priority");
            Nodecl::NodeclBase init_get_priority;
            if (priority_function.is_valid())
            {
                init_get_priority = priority_function.make_nodecl(/* set_ref_type */ true);
                init_get_priority = Nodecl::Conversion::make(
                        init_get_priority,
                        field_get_priority.get_type().no_ref());
                init_get_priority.set_text("C");
            }
            else
            {
                init_get_priority = const_value_to_nodecl(const_value_get_signed_int(0));
            }

            field_init.append(
                    Nodecl::FieldDesignator::make(field_get_priority,
                        init_get_priority,
                        field_get_priority.get_type()));
        }

        // .type_identifier
        {
            Nodecl::NodeclBase field_type_identifier = get_field("type_identifier");
            Nodecl::NodeclBase init_type_identifier = const_value_to_nodecl(const_value_get_signed_int(0));

            field_init.append(
                    Nodecl::FieldDesignator::make(field_type_identifier,
                        init_type_identifier,
                        field_type_identifier.get_type()));
        }

        // .implementation_count
        {
            int num_impl = _implementations.size();
            Nodecl::NodeclBase field_implementation_count = get_field("implementation_count");
            Nodecl::NodeclBase init_implementation_count  = const_value_to_nodecl(const_value_get_signed_int(num_impl));

            field_init.append(
                    Nodecl::FieldDesignator::make(field_implementation_count,
                        init_implementation_count,
                        field_implementation_count.get_type()));
        }

        // .implementations
        {
            Nodecl::NodeclBase field_implementations = get_field("implementations");
            Nodecl::NodeclBase init_implementations  = implementations.make_nodecl(/*ref_type*/ true);

            field_init.append(
                    Nodecl::FieldDesignator::make(field_implementations,
                        init_implementations,
                        field_implementations.get_type()));
        }

        // .destroy
        {
            Nodecl::NodeclBase field_destroy = get_field("destroy_args_block");
            Nodecl::NodeclBase init_destroy;
            if (destroy_function.is_valid())
            {
                init_destroy = destroy_function.make_nodecl(/* set_ref_type */ true);
                init_destroy = Nodecl::Conversion::make(
                        init_destroy,
                        field_destroy.get_type().no_ref());
                init_destroy.set_text("C");
            }
            else
            {
                init_destroy = const_value_to_nodecl(const_value_get_signed_int(0));
            }

            field_init.append(
                    Nodecl::FieldDesignator::make(field_destroy,
                        init_destroy,
                        field_destroy.get_type()));
        }

        // .duplicate_args_block
        {
            Nodecl::NodeclBase field_duplicate = get_field("duplicate_args_block");
            Nodecl::NodeclBase init_duplicate;
            if (duplicate_function.is_valid())
            {
                init_duplicate = duplicate_function.make_nodecl(/* set_ref_type */ true);
                init_duplicate = Nodecl::Conversion::make(
                        init_duplicate,
                        field_duplicate.get_type().no_ref());
                init_duplicate.set_text("C");
            }
            else
            {
                init_duplicate = const_value_to_nodecl(const_value_get_signed_int(0));
            }

            field_init.append(
                    Nodecl::FieldDesignator::make(field_duplicate,
                        init_duplicate,
                        field_duplicate.get_type()));
        }

        // .reduction_initializers
        {
            Nodecl::NodeclBase field_reduction_initializers = get_field("reduction_initializers");
            Nodecl::NodeclBase init_reduction_initializers;

            if (reduction_initializers.is_valid())
            {
                init_reduction_initializers =
                    reduction_initializers.make_nodecl(/* set_ref_type */ true);
            }
            else
            {
                init_reduction_initializers =
                    const_value_to_nodecl(const_value_get_signed_int(0));
            }

            field_init.append(
                    Nodecl::FieldDesignator::make(field_reduction_initializers,
                        init_reduction_initializers,
                        field_reduction_initializers.get_type()));
        }

        // .reduction_combiners
        {
            Nodecl::NodeclBase field_reduction_combiners = get_field("reduction_combiners");
            Nodecl::NodeclBase init_reduction_combiners;

            if (reduction_combiners.is_valid())
            {
                init_reduction_combiners =
                    reduction_combiners.make_nodecl(/* set_ref_type */ true);
            }
            else
            {
                init_reduction_combiners =
                    const_value_to_nodecl(const_value_get_signed_int(0));
            }

            field_init.append(
                    Nodecl::FieldDesignator::make(field_reduction_combiners,
                        init_reduction_combiners,
                        field_reduction_combiners.get_type()));
        }

        Nodecl::NodeclBase struct_init = Nodecl::StructuredValue::make(
            Nodecl::List::make(field_init),
            Nodecl::StructuredValueBracedImplicit::make(),
            task_info.get_type());

        task_info.set_value(struct_init);

        if (IS_FORTRAN_LANGUAGE)
        {
            task_info = fortran_create_detached_symbol_from_static_symbol(task_info);
        }
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
    }

    void TaskProperties::create_environment_structure(
            /* out */
            TL::Type& data_env_struct,
            Nodecl::NodeclBase& args_size,
            bool &requires_initialization)
    {
        std::string structure_name = get_new_name("nanos_task_args");
        _environment_capture.begin_type_setup(structure_name, _related_function, _locus_of_task_creation, _task_body);

        // 1. Create fields for captured & private symbols
        TL::ObjectList<TL::Symbol> captured_and_private_symbols = append_two_lists(_env.captured_value, _env.private_);
        for (TL::ObjectList<TL::Symbol>::iterator it = captured_and_private_symbols.begin();
                it != captured_and_private_symbols.end();
                it++)
        {
            _environment_capture.add_storage_for_private_symbol(*it);
        }

        // 2. Create fields for shared symbols
        for (TL::ObjectList<TL::Symbol>::iterator it = _env.shared.begin();
                it != _env.shared.end();
                it++)
        {
            _environment_capture.add_storage_for_shared_symbol(*it);
        }

        _info_structure = data_env_struct = _environment_capture.end_type_setup();

        if (IS_FORTRAN_LANGUAGE)
            args_size = const_value_to_nodecl(
                    const_value_get_zero(
                        /* bytes */ type_get_size(get_size_t_type()),
                        /* sign */ 0));
        else
            args_size = _environment_capture.get_size();

        requires_initialization = _environment_capture.requires_initialization();
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
                EnvironmentCapture &_environment_capture;
                TL::ObjectList<std::string> &_parameter_names;
                TL::ObjectList<TL::Type> &_parameter_types;
                std::map<TL::Symbol, std::string> &_symbols_to_param_names;

            public:
                AddParameter(
                        EnvironmentCapture &environment_capture,
                        TL::ObjectList<std::string> &parameter_names,
                        TL::ObjectList<TL::Type> &parameter_types,
                        std::map<TL::Symbol, std::string> &symbols_to_param_names)
                    : _environment_capture(environment_capture),
                    _parameter_names(parameter_names), _parameter_types(parameter_types),
                    _symbols_to_param_names(symbols_to_param_names)
                {}

                void operator()(TL::Symbol sym)
                {
                    std::string fixed_name = _environment_capture.get_registered_symbol_name(sym);

                    _symbols_to_param_names[sym] = fixed_name;

                    _parameter_names.append(fixed_name);
                    _parameter_types.append(sym.get_type().no_ref().get_lvalue_reference_to());
                }
        };

        bool type_is_runtime_sized(TL::Type t)
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

        struct MapSymbols
        {
            private:
                const TL::Scope &_inner_function_scope;
                const std::map<TL::Symbol, std::string> &_symbols_to_param_names;

                TL::ObjectList<TL::Symbol> &_parameters_to_update_type;
                Nodecl::Utils::SimpleSymbolMap &_symbol_map;

            public:
                MapSymbols(
                        const TL::Scope &function_scope,
                        const std::map<TL::Symbol, std::string> &symbols_to_param_names,
                        // Out
                        TL::ObjectList<TL::Symbol> &parameter_to_update_type,
                        Nodecl::Utils::SimpleSymbolMap &symbol_map)
                    : _inner_function_scope(function_scope),
                    _symbols_to_param_names(symbols_to_param_names),
                    _parameters_to_update_type(parameter_to_update_type),
                    _symbol_map(symbol_map)
                {}

                void operator()(TL::Symbol sym)
                {
                    std::map<TL::Symbol, std::string>::const_iterator it_param_name = _symbols_to_param_names.find(sym);
                    ERROR_CONDITION(it_param_name == _symbols_to_param_names.end(),
                            "Symbol '%s' not mapped", sym.get_name().c_str());

                    TL::Symbol param_sym = _inner_function_scope.get_symbol_from_name(it_param_name->second);


                    ERROR_CONDITION(!param_sym.is_valid()
                            || (!param_sym.is_parameter() && !param_sym.is_saved_expression()),
                            "Invalid symbol for name '%s'", it_param_name->second.c_str());

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
        };

        // It updates the the type of the 'function' symbol if the type of any of its parameters is runtime sized
        void update_function_type_if_needed(
                TL::Symbol function,
                TL::ObjectList<TL::Symbol>& parameters_to_update_type,
                Nodecl::Utils::SimpleSymbolMap& symbol_map)
        {
            if (parameters_to_update_type.empty())
                return;

            TL::Scope function_inside_scope = function.get_related_scope();

            // Now fix the types of runtime sized types prior anything else
            for (TL::ObjectList<TL::Symbol>::iterator it = parameters_to_update_type.begin();
                    it != parameters_to_update_type.end();
                    it++)
            {
                it->set_type(rewrite_type(it->get_type(), function_inside_scope, symbol_map));
            }

            TL::ObjectList<TL::Type> updated_param_types
                = function.get_related_symbols().map<TL::Type>(&TL::Symbol::get_type);

            function.set_type(
                    TL::Type::get_void_type().get_function_returning(updated_param_types));
        }


        TL::Symbol compute_mangled_function_symbol_from_symbol(const TL::Symbol &sym)
        {
            ERROR_CONDITION(sym.get_internal_symbol()->kind != SK_FUNCTION,
                    "Unexpected symbol kind", 0);

            // For Fortran we will generate a global variable in C, but this
            // requires us to use the proper mangling.
            const char* mangled_name =
                ::fortran_mangle_symbol(sym.get_internal_symbol());

            // Create a detached symbol that looks like it comes from the global scope
            TL::Symbol mangled_symbol = NEW0(scope_entry_t);
            mangled_symbol.get_internal_symbol()->symbol_name = mangled_name;
            mangled_symbol.get_internal_symbol()->decl_context
                = TL::Scope::get_global_scope().get_decl_context();
            mangled_symbol.get_internal_symbol()->kind =
                sym.get_internal_symbol()->kind;

            // Fake symbol type as "void fun(void)" to avoid matching problems with argument types,
            // we will need to cast it back to original type when used
            mangled_symbol.get_internal_symbol()->type_information =
                TL::Type::get_void_type().get_function_returning(TL::ObjectList<TL::Type>())
                .get_internal_type();

            symbol_entity_specs_set_is_user_declared(
                    mangled_symbol.get_internal_symbol(), 1);

            return mangled_symbol;
        }

        // This function computes the loop control that we should emit for a loop construct. It should be
        //
        //      for (induction_variable = taskloop_bounds.lower_bound;
        //              induction_variable < taskloop_bounds.upper_bound;
        //              induction_variable++)
        //      {}
        Nodecl::NodeclBase compute_taskloop_loop_control(
                const TL::Symbol& taskloop_bounds,
                TL::Symbol induction_variable,
                bool define_induction_variable)
        {
            Nodecl::NodeclBase loop_control;

            TL::ObjectList<TL::Symbol> nonstatic_data_members = taskloop_bounds.get_type().no_ref().get_nonstatic_data_members();
            GetField get_field(nonstatic_data_members);

            Nodecl::NodeclBase field = get_field("lower_bound");
            Nodecl::NodeclBase taskloop_lower_bound =
                Nodecl::ClassMemberAccess::make(
                        taskloop_bounds.make_nodecl(/*set_ref_type*/ true),
                        field,
                        /*member literal*/ Nodecl::NodeclBase::null(),
                        field.get_type());

            field = get_field("upper_bound");
            Nodecl::NodeclBase taskloop_upper_bound =
                Nodecl::ClassMemberAccess::make(
                        taskloop_bounds.make_nodecl(/*set_ref_type*/ true),
                        field,
                        /*member literal*/ Nodecl::NodeclBase::null(),
                        field.get_type());

            if (IS_C_LANGUAGE
                    || IS_CXX_LANGUAGE)
            {
                Nodecl::NodeclBase init;
                if (define_induction_variable)
                {
                    induction_variable.set_value(taskloop_lower_bound);
                    init = Nodecl::ObjectInit::make(induction_variable);
                }
                else
                {
                    init =
                        Nodecl::Assignment::make(
                                induction_variable.make_nodecl(/* set_ref_type */ true),
                                taskloop_lower_bound,
                                taskloop_lower_bound.get_type());
                }

                Nodecl::NodeclBase cond =
                    Nodecl::LowerThan::make(
                            induction_variable.make_nodecl(/* set_ref_type */ true),
                            taskloop_upper_bound,
                            taskloop_upper_bound.get_type());

                Nodecl::NodeclBase step =
                    Nodecl::Preincrement::make(
                            induction_variable.make_nodecl(/*set_ref_type*/ true),
                            induction_variable.get_type());

                loop_control = Nodecl::LoopControl::make(Nodecl::List::make(init), cond, step);
            }
            else // IS_FORTRAN_LANGUAGE
            {
                loop_control = Nodecl::RangeLoopControl::make(
                        induction_variable.make_nodecl(/*set_ref_type*/ true),
                        taskloop_lower_bound,
                        Nodecl::Minus::make(
                            taskloop_upper_bound,
                            const_value_to_nodecl(const_value_get_signed_int(1)),
                            taskloop_upper_bound.get_type().no_ref()),
                        /* step */ Nodecl::NodeclBase::null());
            }
            return loop_control;
        }
    }

    void TaskProperties::unpack_datasharing_arguments(
            const TL::Symbol &arg,
            // Out
            Nodecl::List &args,
            TL::ObjectList<std::string> *parameter_names,
            ObjectList<TL::Type> *parameter_types,
            std::map<std::string, std::pair<TL::Symbol, TL::Symbol>> *name_to_pair_orig_field_map) const
    {
        TL::ObjectList<TL::Symbol> captured_and_private_symbols =
            append_two_lists(_env.captured_value, _env.private_);
        for (TL::ObjectList<TL::Symbol>::const_iterator it = captured_and_private_symbols.begin();
                it != captured_and_private_symbols.end();
                it++)
        {
            TL::Symbol symbol = *it;
            EnvironmentCapture::Accessor symbol_accessor =
                _environment_capture.get_private_symbol_accessor(
                    arg, symbol, /* actual_storage_if_vla */ true);

            // Note: This is similar to AddParameter functor
            if (parameter_names != NULL)
                parameter_names->append(symbol_accessor._environment_name);
            if (parameter_types != NULL)
                parameter_types->append(symbol_accessor._environment_type);

            args.append(std::move(symbol_accessor._environment_access));

            if (name_to_pair_orig_field_map != NULL)
                (*name_to_pair_orig_field_map)[symbol_accessor._environment_name] = std::make_pair(symbol, symbol_accessor._environment_symbol);
        }

        for (TL::ObjectList<TL::Symbol>::const_iterator it = _env.shared.begin();
                it != _env.shared.end();
                it++)
        {
            TL::Symbol symbol = *it;
            EnvironmentCapture::Accessor symbol_accessor =
                _environment_capture.get_shared_symbol_accessor(arg, symbol, /* reference_to_pointer */ false);

            // Note: This is similar to AddParameter functor
            if (parameter_names != NULL)
                parameter_names->append(symbol_accessor._environment_name);
            if (parameter_types != NULL)
                parameter_types->append(symbol_accessor._environment_type);

            args.append(std::move(symbol_accessor._environment_access));

            if (name_to_pair_orig_field_map != NULL)
                (*name_to_pair_orig_field_map)[symbol_accessor._environment_name] = std::make_pair(symbol, symbol_accessor._environment_symbol);
        }
    }

    class ComputeUnpackedArgumentFromSymbolName
    {
        private:
            const TL::Nanos6::TaskProperties &_task_properties;
            const TL::Scope &_symbol_scope;
            Nodecl::List &_unpacked_args;
            TL::ObjectList<std::string> *_parameter_names;
            TL::ObjectList<TL::Type> *_parameter_types;
            std::map<std::string, std::pair<TL::Symbol, TL::Symbol>> *_name_to_pair_orig_field_map;

        public:
            ComputeUnpackedArgumentFromSymbolName(
                    const TL::Nanos6::TaskProperties &task_properties,
                    const TL::Scope &symbol_scope,
                    // Out
                    Nodecl::List &unpacked_args,
                    TL::ObjectList<std::string> *parameter_names,
                    ObjectList<TL::Type> *parameter_types,
                    std::map<std::string, std::pair<TL::Symbol, TL::Symbol>> *name_to_pair_orig_field_map)
                : _task_properties(task_properties),
                _symbol_scope(symbol_scope), _unpacked_args(unpacked_args),
                _parameter_names(parameter_names), _parameter_types(parameter_types),
                _name_to_pair_orig_field_map(name_to_pair_orig_field_map)
            {}

            void operator()(const std::string &symbol_name)
            {
                TL::Symbol symbol = _symbol_scope.get_symbol_from_name(symbol_name);
                ERROR_CONDITION(!symbol.is_valid(), "Invalid symbol '%s'", symbol_name.c_str());

                if (symbol_name == "arg")
                {
                    // Expand 'arg' symbol with task args members
                    _task_properties.unpack_datasharing_arguments(
                            symbol,
                            _unpacked_args,
                            _parameter_names,
                            _parameter_types,
                            _name_to_pair_orig_field_map);
                }
                else
                {
                    if (_parameter_names != NULL)
                        _parameter_names->append(symbol_name);
                    if (_parameter_types != NULL)
                        _parameter_types->append(symbol.get_type());
                    _unpacked_args.append(
                            symbol.make_nodecl(/* set_ref_type */ true));
                }
            }
    };

    void TaskProperties::create_forward_function_fortran(
            const TL::Symbol &unpacked_function,
            const std::string &common_name,
            const TL::ObjectList<std::string> &outline_fun_param_names,
            const TL::Scope &outline_fun_inside_scope,
            // Out
            Nodecl::NodeclBase &forwarded_function_call)
    {
        /*
         * Fortran side
         */

        std::string forwarded_name = get_new_name("nanos6_fwd_" + common_name);

        // Prepare function parameters and function call

        TL::ObjectList<std::string> forwarded_parameter_names;
        TL::ObjectList<TL::Type> forwarded_parameter_types;

        Nodecl::List forwarded_fun_call_args;

        // Add 'unpacked' function parameter/arg
        forwarded_parameter_names.append("unpacked");
        forwarded_parameter_types.append(
            TL::Type::get_void_type().get_pointer_to());

        forwarded_fun_call_args.append(
                Nodecl::Reference::make(
                    unpacked_function.make_nodecl(/* set_ref_type */ true),
                    unpacked_function.get_type().get_pointer_to()));

        // This map associates a symbol name with a pair that represents the original symbol and the field
        std::map<std::string, std::pair<TL::Symbol, TL::Symbol>> name_to_pair_orig_field_map;

        // Add remaining parameters/args
        ComputeUnpackedArgumentFromSymbolName compute_unpacked_argument_from_symbol_name(
                *this,
                outline_fun_inside_scope,
                forwarded_fun_call_args,
                &forwarded_parameter_names,
                &forwarded_parameter_types,
                &name_to_pair_orig_field_map);

        outline_fun_param_names.map(compute_unpacked_argument_from_symbol_name);

        TL::Symbol forwarded_function = SymbolUtils::new_function_symbol(
                TL::Scope::get_global_scope(),
                forwarded_name,
                TL::Type::get_void_type(),
                forwarded_parameter_names,
                forwarded_parameter_types);

        // Make this symbol global
        symbol_entity_specs_set_is_static(
            forwarded_function.get_internal_symbol(), 0);

        // Propagate attributes and update function type
        TL::ObjectList<TL::Symbol> forwarded_parameters_to_update_type;
        Nodecl::Utils::SimpleSymbolMap field_to_forwarded_symbol_map;

        const TL::ObjectList<TL::Symbol> &forwarded_params = forwarded_function.get_related_symbols();
        for (TL::ObjectList<TL::Symbol>::const_iterator it = forwarded_params.begin();
                it != forwarded_params.end();
                it++)
        {
            TL::Symbol forwarded_param(*it);
            TL::Symbol field(name_to_pair_orig_field_map[forwarded_param.get_name()].second);

            // Only parameters that correspond to fields need to be dealt with
            if (field.is_valid())
            {
                if (type_is_runtime_sized(forwarded_param.get_type()))
                {
                    forwarded_parameters_to_update_type.append(forwarded_param);
                    field_to_forwarded_symbol_map.add_map(field, forwarded_param);
                }

                //FIXME: Propagate TARGET attribute

                // Propagate ALLOCATABLE attribute
                TL::Symbol original_symbol(name_to_pair_orig_field_map[forwarded_param.get_name()].first);
                if (field.is_allocatable()
                        // VLAs in Fortran are represented as allocatable variables. However, when we call
                        // to the fwd function we convert them to VLAs again. For this reason we need the
                        // original symbol at this point
                        && original_symbol.is_allocatable())
                    symbol_entity_specs_set_is_allocatable(forwarded_param.get_internal_symbol(), 1);
            }
        }

        update_function_type_if_needed(
                forwarded_function, forwarded_parameters_to_update_type, field_to_forwarded_symbol_map);

        // Add call to forward
        forwarded_function_call = Nodecl::ExpressionStatement::make(
                Nodecl::FunctionCall::make(
                    forwarded_function.make_nodecl(/* set_ref_type */ true),
                    forwarded_fun_call_args,
                    /* alternate_name */ Nodecl::NodeclBase::null(),
                    /* function_form */ Nodecl::NodeclBase::null(),
                    get_void_type()));

        /*
         * C side
         */

        std::string c_forwarded_name =
            ::fortran_mangle_symbol(forwarded_function.get_internal_symbol());

        // Now generate the C counterpart, reuse parameter names and set types to void*

        TL::ObjectList<std::string> c_forwarded_parameter_names =
            forwarded_parameter_names;
        TL::ObjectList<TL::Type> c_forwarded_parameter_types(
            forwarded_parameter_types.size(),
            TL::Type::get_void_type().get_pointer_to());

        // Fix 'unpacked' function parameter type (always first parameter)
        c_forwarded_parameter_types[0] =
            TL::Type::get_void_type().get_function_returning(
                    TL::ObjectList<TL::Type>(
                        forwarded_parameter_types.size() - 1,
                        TL::Type::get_void_type().get_pointer_to()));

        TL::Symbol c_forwarded_function = SymbolUtils::new_function_symbol(
                TL::Scope::get_global_scope(),
                c_forwarded_name,
                TL::Type::get_void_type(),
                c_forwarded_parameter_names,
                c_forwarded_parameter_types);

        // Make this symbol global
        symbol_entity_specs_set_is_static(
                c_forwarded_function.get_internal_symbol(), 0);

        Nodecl::NodeclBase c_forwarded_function_code, c_forwarded_empty_stmt;
        SymbolUtils::build_empty_body_for_function(
                c_forwarded_function,
                c_forwarded_function_code,
                c_forwarded_empty_stmt);

        // Prepare function call arguments and add function call

        SolveParamNames solve_param_names(
                c_forwarded_empty_stmt.retrieve_context());

        TL::ObjectList<Nodecl::NodeclBase> c_forwarded_parameters =
            c_forwarded_parameter_names.map<Nodecl::NodeclBase>(
                    solve_param_names);

        Nodecl::NodeclBase c_unpacked_fun_parameter = c_forwarded_parameters[0];

        Nodecl::List c_unpacked_fun_call_args =
            Nodecl::List::make(TL::ObjectList<Nodecl::NodeclBase>(
                        c_forwarded_parameters.begin() + 1, c_forwarded_parameters.end()));

        c_forwarded_empty_stmt.replace(
                Nodecl::CompoundStatement::make(
                    Nodecl::List::make(
                        Nodecl::ExpressionStatement::make(
                            Nodecl::FunctionCall::make(
                                c_unpacked_fun_parameter,
                                c_unpacked_fun_call_args,
                                /* alternate-symbol */ Nodecl::NodeclBase::null(),
                                /* function-form */ Nodecl::NodeclBase::null(),
                                TL::Type::get_void_type()))),
                    /* finalize */ Nodecl::NodeclBase::null()));

        _phase->get_extra_c_code().append(c_forwarded_function_code);
    }

    void TaskProperties::create_outline_function_common(
            const std::string &common_name,
            const TL::ObjectList<std::string> &outline_fun_param_names,
            const ObjectList<TL::Type> &outline_fun_param_types,
            //Out
            TL::Symbol &outline_function,
            Nodecl::NodeclBase &outline_fun_empty_stmt)
    {
        std::string outline_fun_name = get_new_name("nanos6_ol_" + common_name);

        ERROR_CONDITION(outline_fun_param_names.size() != outline_fun_param_types.size(),
                "Unexpected parameter lists", 0);

        outline_function = SymbolUtils::new_function_symbol(
                _related_function,
                outline_fun_name,
                TL::Type::get_void_type(),
                outline_fun_param_names,
                outline_fun_param_types);

        Nodecl::NodeclBase outline_fun_code;
        SymbolUtils::build_empty_body_for_function(
                outline_function,
                outline_fun_code,
                outline_fun_empty_stmt);

        Nodecl::Utils::append_to_enclosing_top_level_location(
                _task_body, outline_fun_code);

        if (IS_CXX_LANGUAGE && !_related_function.is_member())
        {
            Nodecl::Utils::prepend_to_enclosing_top_level_location(
                    _task_body,
                    Nodecl::CxxDecl::make(
                        Nodecl::Context::make(
                            Nodecl::NodeclBase::null(),
                            _related_function.get_scope()),
                        outline_function));
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            outline_function =
                compute_mangled_function_symbol_from_symbol(outline_function);

            TL::Scope outline_fun_inside_scope =
                outline_fun_empty_stmt.retrieve_context();

            fortran_add_types(outline_fun_inside_scope);
        }
    }

    TL::Symbol TaskProperties::create_outline_function(
            const TL::Symbol &unpacked_function,
            const std::string &common_name,
            const TL::ObjectList<std::string> &outline_fun_param_names,
            const ObjectList<TL::Type> &outline_fun_param_types)
    {
        return create_outline_function(
                unpacked_function,
                common_name,
                outline_fun_param_names,
                outline_fun_param_types,
                /* compute_stmts_pre_fun_call_fun */ NULL);
    }

    TL::Symbol TaskProperties::create_outline_function(
            const TL::Symbol &unpacked_function,
            const std::string &common_name,
            const TL::ObjectList<std::string> &outline_fun_param_names,
            const ObjectList<TL::Type> &outline_fun_param_types,
            void (TaskProperties::*compute_stmts_pre_fun_call_fun)
            (const TL::Scope &outline_fun_inside_scope, Nodecl::List &stmts) const)
    {
        TL::Symbol outline_function;
        Nodecl::NodeclBase outline_fun_empty_stmt;
        create_outline_function_common(
                common_name,
                outline_fun_param_names,
                outline_fun_param_types,
                outline_function,
                outline_fun_empty_stmt);

        TL::Scope outline_fun_inside_scope = outline_fun_empty_stmt.retrieve_context();

        if (compute_stmts_pre_fun_call_fun != NULL)
        {
            Nodecl::List pre_stmts;
            (this->*compute_stmts_pre_fun_call_fun)(outline_fun_inside_scope, pre_stmts);
            outline_fun_empty_stmt.prepend_sibling(pre_stmts);
        }

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            // 'Unpacked' function call

            // Compute function call arguments
            // Note: optional parameters are not passed, as at this point no
            // function will be generated and such information needn't be computed
            Nodecl::List unpacked_fun_call_args;
            ComputeUnpackedArgumentFromSymbolName compute_unpacked_argument_from_symbol_name(
                    *this,
                    outline_fun_inside_scope,
                    unpacked_fun_call_args,
                    /* parameter_names */ NULL,
                    /* parameter_types */ NULL,
                    /* name_to_pair_orig_field_map */ NULL);

            outline_fun_param_names.map(compute_unpacked_argument_from_symbol_name);

            // Make sure we explicitly pass template arguments to the 'unpacked' function
            Nodecl::NodeclBase function_form;
            if (unpacked_function.get_type().is_template_specialized_type())
            {
                function_form = Nodecl::CxxFunctionFormTemplateId::make();
                function_form.set_template_parameters(
                        unpacked_function.get_type()
                        .template_specialized_type_get_template_arguments());
            }

            Nodecl::NodeclBase unpacked_fun_call =
                Nodecl::ExpressionStatement::make(
                        Nodecl::FunctionCall::make(
                            unpacked_function.make_nodecl(/* set_ref_type */ true),
                            unpacked_fun_call_args,
                            /* alternate_name */ Nodecl::NodeclBase::null(),
                            function_form,
                            get_void_type()));


            outline_fun_empty_stmt.replace(unpacked_fun_call);
        }
        if (IS_FORTRAN_LANGUAGE)
        {
            Nodecl::NodeclBase forwarded_function_call;
            create_forward_function_fortran(
                    unpacked_function,
                    common_name,
                    outline_fun_param_names,
                    outline_fun_inside_scope,
                    forwarded_function_call);

            outline_fun_empty_stmt.replace(forwarded_function_call);
        }

        return outline_function;
    }

    namespace
    {
        TL::Symbol generate_reduction_storage_symbol(
            const TL::DataReference &reduction_expr,
            TL::Scope &scope,
            Nodecl::List &extra_stmts)
        {
            // Obtain function symbol
            TL::Symbol get_red_storage_fun = get_nanos6_function_symbol("nanos6_get_reduction_storage1");

            TL::Symbol reduction_sym = reduction_expr.get_base_symbol();
            TL::Type reduction_type = reduction_expr.get_type().no_ref();

            TL::Symbol storage_sym = scope.new_symbol(reduction_sym.get_name() + "_storage");
            symbol_entity_specs_set_is_user_declared(
                    storage_sym.get_internal_symbol(), 1);
            storage_sym.get_internal_symbol()->kind = SK_VARIABLE;
            storage_sym.set_type(reduction_type.get_lvalue_reference_to());

            // Compute arguments
            TL::ObjectList<Nodecl::NodeclBase> multidim_arguments_list;
            compute_base_address_and_dimensionality_information(reduction_expr, multidim_arguments_list);

            TL::ObjectList<Nodecl::NodeclBase>::reverse_iterator it = multidim_arguments_list.rbegin();

            Nodecl::NodeclBase upper_bound = *(it++);
            Nodecl::NodeclBase lower_bound = *(it++);
            Nodecl::NodeclBase size = *(it++);

            for (; it != multidim_arguments_list.rend() - 1; it++)
            {
                Nodecl::NodeclBase dim_upper_bound = *(it++);
                Nodecl::NodeclBase dim_lower_bound = *(it++);
                Nodecl::NodeclBase dim_size = *it;

                upper_bound = Nodecl::Add::make(
                        Nodecl::Minus::make(
                            Nodecl::Mul::make(
                                dim_size,
                                Nodecl::ParenthesizedExpression::make(
                                    upper_bound, reduction_type),
                                reduction_type),
                            dim_size,
                            reduction_type),
                        dim_upper_bound,
                        reduction_type);

                lower_bound = Nodecl::Add::make(
                        Nodecl::Mul::make(
                            dim_size,
                            Nodecl::ParenthesizedExpression::make(
                                lower_bound, reduction_type),
                            reduction_type),
                        dim_lower_bound,
                        reduction_type);

                size = Nodecl::Mul::make(dim_size, size, reduction_type);
            }

            Nodecl::NodeclBase base_address = *it;

            Nodecl::List arguments_list =
                Nodecl::List::make(base_address, size, lower_bound, upper_bound);

            // Function call
            Nodecl::NodeclBase cast;
            Nodecl::NodeclBase function_call = Nodecl::Dereference::make(
                    cast = Nodecl::Conversion::make(
                        Nodecl::FunctionCall::make(
                            get_red_storage_fun.make_nodecl(/* set_ref_type */ true),
                            arguments_list,
                            /* alternate_name */ Nodecl::NodeclBase::null(),
                            /* function_form */ Nodecl::NodeclBase::null(),
                            TL::Type::get_void_type().get_pointer_to()),
                        reduction_type.get_pointer_to()),
                    reduction_type.get_lvalue_reference_to());
            cast.set_text("C");

            storage_sym.set_value(function_call);

            extra_stmts.prepend(
                    Nodecl::ObjectInit::make(storage_sym));
            if (IS_CXX_LANGUAGE)
            {
                extra_stmts.prepend(
                        Nodecl::CxxDef::make(
                            /* context */ Nodecl::NodeclBase::null(),
                            storage_sym));
            }

            return storage_sym;
        }

        void generate_extra_reduction_statements(
                const TL::ObjectList<Nodecl::NodeclBase> &expr_list,

                TL::Scope& unpacked_fun_inside_scope,
                const Nodecl::NodeclBase& unpacked_fun_empty_stmt,
                Nodecl::Utils::SimpleSymbolMap& symbol_map,

                TL::Scope& final_scope,
                Nodecl::List& final_stmts,
                Nodecl::Utils::SimpleSymbolMap& final_symbol_map,

                bool has_final_stmts,
                bool is_weakreduction)
        {
            for (TL::ObjectList<Nodecl::NodeclBase>::const_iterator it = expr_list.begin();
                    it != expr_list.end();
                    it++)
            {
                Nodecl::NodeclBase red_expr = it->shallow_copy();

                // Get original symbol, keep it, and rewrite registered expression in terms of unpacked function parameter

                TL::DataReference orig_data_ref = red_expr;
                TL::Symbol orig_sym = orig_data_ref.get_base_symbol();

                Nodecl::NodeclBase unpacked_fun_red_expr =
                    Nodecl::Utils::deep_copy(red_expr, unpacked_fun_inside_scope, symbol_map);

                // 1. Build function call that computes the value of the reduction storage

                if (!is_weakreduction)
                {
                    Nodecl::List unpacked_fun_extra_stmts;
                    TL::Symbol storage_sym = generate_reduction_storage_symbol(
                            unpacked_fun_red_expr,
                            unpacked_fun_inside_scope,
                            unpacked_fun_extra_stmts);

                    unpacked_fun_empty_stmt.prepend_sibling(
                            unpacked_fun_extra_stmts);

                    // (Re)map original symbol to new local symbol so that the
                    // posterior deep copy over the task body replaces all its references
                    ERROR_CONDITION(symbol_map.get_simple_symbol_map()->find(orig_sym) ==
                            symbol_map.get_simple_symbol_map()->end(),
                            "Symbol '%s' not found in map", orig_sym.get_name().c_str());
                    symbol_map.add_map(orig_sym, storage_sym);
                }

                // 2. Fix serial statements for 'final' construct

                if (has_final_stmts)
                {
                    Nodecl::List final_extra_stmts;
                    TL::Symbol final_storage_sym = generate_reduction_storage_symbol(
                            orig_data_ref,
                            final_scope,
                            final_extra_stmts);

                    final_stmts.prepend(final_extra_stmts);

                    final_symbol_map.add_map(orig_sym, final_storage_sym);
                }
            }
        }
    }

    void TaskProperties::handle_task_reductions(
            TL::Scope& unpacked_fun_inside_scope,
            Nodecl::NodeclBase unpacked_fun_empty_stmt,
            Nodecl::Utils::SimpleSymbolMap &symbol_map)
    {
        bool has_final_stmts = !_serial_context.is_null();
        TL::Scope final_scope;

        // Check serial statements structure and obtain statement list

        Nodecl::List serial_stmts_list;
        Nodecl::NodeclBase original_final_stmts;

        if (has_final_stmts)
        {
            ERROR_CONDITION(!_serial_context.is<Nodecl::Context>(),
                    "Unexpected node '%s'", ast_print_node_type(_serial_context.get_kind()));

            Nodecl::NodeclBase serial_compound_stmt =
                _serial_context.as<Nodecl::Context>()
                .get_in_context().as<Nodecl::List>().front();

            final_scope =
                _serial_context.retrieve_context().get_decl_context();

            ERROR_CONDITION(!serial_compound_stmt.is<Nodecl::CompoundStatement>(),
                    "Unexpected node '%s'",
                    ast_print_node_type(serial_compound_stmt.get_kind()));

            serial_stmts_list =
                serial_compound_stmt.as<Nodecl::CompoundStatement>()
                .get_statements().as<Nodecl::List>();

            ERROR_CONDITION(serial_stmts_list.size() != 1,
                    "Unexpected list size '%d'", serial_stmts_list.size());

            original_final_stmts = serial_stmts_list.front();

            ERROR_CONDITION(!(original_final_stmts.is<Nodecl::Context>() ||
                        original_final_stmts.is<Nodecl::ExpressionStatement>()),
                    "Unexpected node '%s'",
                    ast_print_node_type(original_final_stmts.get_kind()));
        }

        // Symbol map to replace original symbols for storage symbols
        Nodecl::Utils::SimpleSymbolMap final_symbol_map;

        // Generate extra reduction statements for 'reduction' expressions

        generate_extra_reduction_statements(
                _env.dep_reduction,
                unpacked_fun_inside_scope,
                unpacked_fun_empty_stmt,
                symbol_map,
                final_scope,
                serial_stmts_list,
                final_symbol_map,
                has_final_stmts,
                /* is_weakreduction */ false);

        // Generate extra reduction statements for 'weakreduction' expressions

        generate_extra_reduction_statements(
                _env.dep_weakreduction,
                unpacked_fun_inside_scope,
                unpacked_fun_empty_stmt,
                symbol_map,
                final_scope,
                serial_stmts_list,
                final_symbol_map,
                has_final_stmts,
                /* is_weakreduction */ true);

        // Deep copy serial statements to replace original symbol references for new final symbols

        if (!(_env.dep_reduction.empty() && _env.dep_weakreduction.empty()) &&
                has_final_stmts)
        {
            original_final_stmts.replace(
                    deep_copy(
                        original_final_stmts,
                        original_final_stmts.retrieve_context(),
                        final_symbol_map));
        }
    }

    TL::Symbol TaskProperties::create_task_region_unpacked_function(
            const std::string &common_name,
            const std::shared_ptr<Device> &device)
    {
        TL::ObjectList<std::string> unpacked_fun_param_names;
        TL::ObjectList<TL::Type> unpacked_fun_param_types;
        std::map<TL::Symbol, std::string> symbols_to_param_names;

        std::string unpacked_fun_name = get_new_name("nanos6_unpacked_" + common_name);

        AddParameter add_params_functor(
            /* in */ _environment_capture,
            /* out */ unpacked_fun_param_names,
            /* out */ unpacked_fun_param_types,
            /* out */ symbols_to_param_names);

        _env.captured_value.map(add_params_functor);
        _env.private_.map(add_params_functor);
        _env.shared.map(add_params_functor);

        // Extra arguments: device_env and address_translation_table
        const char *device_env_name;
        TL::Type type_arg;
        if (_env.task_is_loop)
        {
            device_env_name = "taskloop_bounds";
            TL::Symbol class_sym = get_nanos6_class_symbol("nanos6_taskloop_bounds_t");
            type_arg = class_sym.get_user_defined_type().get_lvalue_reference_to();
        }
        else
        {
            device_env_name = "device_env";
            type_arg = TL::Type::get_void_type().get_pointer_to();
        }

        unpacked_fun_param_names.append(device_env_name);
        unpacked_fun_param_types.append(type_arg);

        unpacked_fun_param_names.append("address_translation_table");
        TL::Type address_translation_type =
            get_nanos6_class_symbol("nanos6_address_translation_entry_t").get_user_defined_type();
        unpacked_fun_param_types.append(address_translation_type.get_pointer_to());

        TL::Symbol unpacked_function = SymbolUtils::new_function_symbol(
                _related_function,
                unpacked_fun_name,
                TL::Type::get_void_type(),
                unpacked_fun_param_names,
                unpacked_fun_param_types);

        Nodecl::NodeclBase unpacked_fun_code, unpacked_fun_empty_stmt;
        SymbolUtils::build_empty_body_for_function(
                unpacked_function,
                unpacked_fun_code,
                unpacked_fun_empty_stmt);

        device->root_unpacked_function(unpacked_function, unpacked_fun_code);

        TL::Scope unpacked_fun_inside_scope = unpacked_function.get_related_scope();
        // Prepare deep copy and remember those parameters that need fixup
        Nodecl::Utils::SimpleSymbolMap symbol_map;
        TL::ObjectList<TL::Symbol> parameters_to_update_type;

        MapSymbols map_symbols_functor(
                unpacked_fun_inside_scope,
                symbols_to_param_names,
                // Out
                parameters_to_update_type,
                symbol_map);

        _env.captured_value.map(map_symbols_functor);
        _env.private_.map(map_symbols_functor);
        _env.shared.map(map_symbols_functor);

        update_function_type_if_needed(
                unpacked_function, parameters_to_update_type, symbol_map);

        Nodecl::List nested_functions;
        if (IS_FORTRAN_LANGUAGE)
        {
            Nodecl::Utils::Fortran::ExtraDeclsVisitor fun_visitor(
                    symbol_map,
                    unpacked_fun_inside_scope,
                    _related_function);
            fun_visitor.insert_extra_symbols(_task_body);

            if (_related_function.is_in_module())
            {
                TL::Symbol in_module = _related_function.in_module();
                Nodecl::Utils::Fortran::append_used_modules(
                        _task_body.retrieve_context(),
                        in_module.get_related_scope());
            }

            Nodecl::Utils::Fortran::append_used_modules(
                    _task_body.retrieve_context(),
                    unpacked_fun_inside_scope);

            if (_related_function.is_nested_function())
            {
                TL::Symbol enclosing_function = _related_function.get_scope().get_related_symbol();

                ERROR_CONDITION(!enclosing_function.is_valid()
                        || !(enclosing_function.is_function()
                            || enclosing_function.is_fortran_main_program()),
                        "Invalid enclosing symbol of nested function", 0);

                Nodecl::Utils::Fortran::append_used_modules(
                        enclosing_function.get_related_scope(),
                        unpacked_fun_inside_scope);
            }

            fortran_add_types(unpacked_fun_inside_scope);

            // Now get all the needed internal functions and duplicate them in the outline
            Nodecl::Utils::Fortran::InternalFunctions internal_functions;
            internal_functions.walk(_task_body);

            nested_functions = duplicate_internal_subprograms(internal_functions.function_codes,
                    unpacked_fun_inside_scope,
                    symbol_map);
        }
        unpacked_fun_empty_stmt.append_sibling(nested_functions);

        handle_task_reductions(unpacked_fun_inside_scope, unpacked_fun_empty_stmt, symbol_map);

        if (_env.task_is_loop)
        {
            ERROR_CONDITION(!_task_body.is<Nodecl::List>(), "Unexpected node\n", 0);
            Nodecl::NodeclBase stmt = _task_body.as<Nodecl::List>().front();
            ERROR_CONDITION(!stmt.is<Nodecl::Context>(), "Unexpected node\n", 0);
            stmt = stmt.as<Nodecl::Context>().get_in_context().as<Nodecl::List>().front();
            ERROR_CONDITION(!stmt.is<Nodecl::ForStatement>(), "Unexpected node\n", 0);

            TL::ForStatement for_stmt(stmt.as<Nodecl::ForStatement>());

            TL::Symbol ind_var = for_stmt.get_induction_variable();
            // FIXME: The taskloop bounds are passed as if they were the device environment...
            TL::Symbol taskloop_bounds =
                unpacked_fun_inside_scope.get_symbol_from_name(device_env_name);

            for_stmt.set_loop_header(
                    compute_taskloop_loop_control(
                        taskloop_bounds,
                        ind_var,
                        for_stmt.induction_variable_in_separate_scope()));
        }

        // Deep copy device-specific task body
        unpacked_fun_empty_stmt.replace(
                device->compute_specific_task_body(
                    _task_body,
                    _env,
                    unpacked_fun_code,
                    unpacked_fun_inside_scope,
                    symbol_map));

        if (IS_CXX_LANGUAGE
                && !_related_function.is_member())
        {
            Nodecl::Utils::prepend_to_enclosing_top_level_location(
                    _task_body,
                    Nodecl::CxxDecl::make(
                        Nodecl::Context::make(
                            Nodecl::NodeclBase::null(),
                            _related_function.get_scope()),
                        unpacked_function));
        }

        return unpacked_function;
    }

    TL::Symbol TaskProperties::create_task_region_function(std::shared_ptr<Device> device)
    {
        // Skip this function if the current task comes from a taskwait depend
        if (_env.task_is_taskwait_with_deps)
            return TL::Symbol::invalid();

        const std::string common_name = "task_region";
        TL::Symbol unpacked_function =
            create_task_region_unpacked_function(common_name, device);

        // Second argument is different for a loop task
        const char *device_env_name;
        TL::Type type_arg;
        if (_env.task_is_loop)
        {
            device_env_name = "taskloop_bounds";
            TL::Symbol class_sym = get_nanos6_class_symbol("nanos6_taskloop_bounds_t");
            type_arg = class_sym.get_user_defined_type().get_lvalue_reference_to();
        }
        else
        {
            device_env_name = "device_env";
            type_arg = TL::Type::get_void_type().get_pointer_to();
        }

        TL::ObjectList<std::string> parameter_names(3);
        TL::ObjectList<TL::Type> parameter_types(3);

        parameter_names[0] = "arg";
        parameter_types[0] = _info_structure.get_lvalue_reference_to();

        parameter_names[1] = device_env_name;
        parameter_types[1] = type_arg;

        parameter_names[2] = "address_translation_table";
        TL::Type address_translation_type =
            get_nanos6_class_symbol("nanos6_address_translation_entry_t").get_user_defined_type();
        parameter_types[2] = address_translation_type.get_pointer_to();

        TL::Symbol outline_function = create_outline_function(
                unpacked_function,
                common_name,
                parameter_names,
                parameter_types,
                (device->requires_arguments_translation() ? &TaskProperties::compute_arguments_translation : NULL));

        return outline_function;
    }

    TL::Symbol TaskProperties::create_constraints_unpacked_function(
            const std::string& common_name)
    {
        TL::ObjectList<std::string> unpacked_fun_param_names;
        TL::ObjectList<TL::Type> unpacked_fun_param_types;
        std::map<TL::Symbol, std::string> symbols_to_param_names;

        std::string unpacked_fun_name = get_new_name("nanos6_unpacked_" + common_name);

        AddParameter add_params_functor(
            /* in */ _environment_capture,
            /* out */ unpacked_fun_param_names,
            /* out */ unpacked_fun_param_types,
            /* out */ symbols_to_param_names);

        _env.captured_value.map(add_params_functor);
        _env.private_.map(add_params_functor);
        _env.shared.map(add_params_functor);

        unpacked_fun_param_names.append("constraints");
        unpacked_fun_param_types.append(get_nanos6_class_symbol("nanos6_task_constraints_t")
                .get_user_defined_type().get_lvalue_reference_to());

        TL::Symbol unpacked_function = SymbolUtils::new_function_symbol(
                _related_function,
                unpacked_fun_name,
                TL::Type::get_void_type(),
                unpacked_fun_param_names,
                unpacked_fun_param_types);

        Nodecl::NodeclBase unpacked_fun_code, unpacked_fun_empty_stmt;
        SymbolUtils::build_empty_body_for_function(
            unpacked_function, unpacked_fun_code, unpacked_fun_empty_stmt);

        TL::Scope unpacked_fun_inside_scope = unpacked_function.get_related_scope();

        fortran_add_types(unpacked_fun_inside_scope);

        // Prepare deep copy and remember those parameters that need fixup
        Nodecl::Utils::SimpleSymbolMap symbol_map;
        TL::ObjectList<TL::Symbol> parameters_to_update_type;

        MapSymbols map_symbols_functor(
                unpacked_fun_inside_scope,
                symbols_to_param_names,
                // Out
                parameters_to_update_type,
                symbol_map);

        _env.captured_value.map(map_symbols_functor);
        _env.private_.map(map_symbols_functor);
        _env.shared.map(map_symbols_functor);

        update_function_type_if_needed(
                unpacked_function, parameters_to_update_type, symbol_map);

        TL::Symbol constraints =
            unpacked_fun_inside_scope.get_symbol_from_name("constraints");
        ERROR_CONDITION(!constraints.is_valid(), "Invalid symbol", 0);

        TL::ObjectList<TL::Symbol> fields =
            constraints.get_type().no_ref().get_nonstatic_data_members();
        GetField get_field(fields);

        // Cost
        {
            Nodecl::NodeclBase cost_member = get_field("cost");

            Nodecl::NodeclBase lhs_expr =
                Nodecl::ClassMemberAccess::make(
                        constraints.make_nodecl(/* set_ref_type */ true),
                        cost_member,
                        /* member_literal */ Nodecl::NodeclBase::null(),
                        cost_member.get_type());

            if (IS_FORTRAN_LANGUAGE)
            {
                // Insert extra symbol declarations and add them to the symbol map
                // (e.g. functions and subroutines declared in other scopes)
                Nodecl::Utils::Fortran::ExtraDeclsVisitor fun_visitor(
                        symbol_map,
                        unpacked_fun_inside_scope,
                        _related_function);
                fun_visitor.insert_extra_symbols(_env.cost_clause);
            }

            Nodecl::NodeclBase cost_expr =
                Nodecl::Utils::deep_copy(_env.cost_clause, unpacked_fun_inside_scope, symbol_map);

            if (!cost_expr.get_type().is_same_type(cost_member.get_type()))
            {
                cost_expr = Nodecl::Conversion::make(
                        cost_expr,
                        cost_member.get_type().no_ref(),
                        cost_expr.get_locus());
                cost_expr.set_text("C");
            }

            Nodecl::NodeclBase assignment_stmt =
                Nodecl::ExpressionStatement::make(
                        Nodecl::Assignment::make(
                            lhs_expr,
                            cost_expr,
                            lhs_expr.get_type()));

            unpacked_fun_empty_stmt.replace(assignment_stmt);
        }

        Nodecl::Utils::prepend_to_enclosing_top_level_location(
                _task_body, unpacked_fun_code);

        return unpacked_function;
    }

    TL::Symbol TaskProperties::create_constraints_function()
    {
        // Do not generate this function if the current task doesn't have any restriction
        if (_env.cost_clause.is_null())
            return TL::Symbol::invalid();

        const std::string common_name = "constraints";
        TL::Symbol unpacked_function =
            create_constraints_unpacked_function(common_name);

        TL::ObjectList<std::string> unpacked_fun_param_names(2);
        TL::ObjectList<TL::Type> unpacked_fun_param_types(2);

        unpacked_fun_param_names[0] = "arg";
        unpacked_fun_param_types[0] = _info_structure.get_lvalue_reference_to();

        unpacked_fun_param_names[1] = "constraints";
        unpacked_fun_param_types[1] = get_nanos6_class_symbol("nanos6_task_constraints_t")
            .get_user_defined_type().get_lvalue_reference_to();

        TL::Symbol outline_function = create_outline_function(
                unpacked_function,
                common_name,
                unpacked_fun_param_names,
                unpacked_fun_param_types);

        return outline_function;
    }

    namespace
    {
        struct TranslateReductionExpr: public Nodecl::ExhaustiveVisitor<void>
        {
            private:
                std::map<TL::Symbol, Nodecl::NodeclBase>& _translation_map;

            public:
                TranslateReductionExpr(std::map<TL::Symbol, Nodecl::NodeclBase>& translation_map)
                    : _translation_map(translation_map)
                {}

            virtual void visit(const Nodecl::Symbol& node)
            {
                TL::Symbol symbol = node.get_symbol();

                std::map<TL::Symbol, Nodecl::NodeclBase>::iterator it = _translation_map.find(symbol);
                if (it != _translation_map.end())
                {
                    node.replace(it->second.shallow_copy());
                }
            }
        };

        TL::Type get_base_element_type(TL::Type type)
        {
            while (type.is_array())
                type = type.array_element();

            return type;
        }

        TL::Symbol create_reduction_initializer_function(
                const ReductionItem& red,
                const TL::Symbol& related_function,
                int function_identifier)
        {
            OpenMP::Reduction& reduction_info = *red._reduction_info;
            TL::Symbol omp_priv = reduction_info.get_omp_priv();
            TL::Symbol omp_orig = reduction_info.get_omp_orig();

            // 1. Create function symbol

            std::string red_init_fun_name;
            {
                std::stringstream ss;
                ss << "nanos6_reduction_init_" << function_identifier;
                red_init_fun_name = ss.str();
            }

            TL::ObjectList<std::string> red_init_parameter_names(3);
            red_init_parameter_names[0] = omp_priv.get_name();
            red_init_parameter_names[1] = omp_orig.get_name();
            red_init_parameter_names[2] = "size";

            TL::Type base_element_type =
                get_base_element_type(red._reduction_type.no_ref());

            TL::ObjectList<TL::Type> red_init_parameter_types(3);
            red_init_parameter_types[0] = base_element_type.get_pointer_to();
            red_init_parameter_types[1] = base_element_type.get_pointer_to();
            red_init_parameter_types[2] = TL::Type::get_size_t_type();

            TL::Symbol red_init_fun_sym = SymbolUtils::new_function_symbol(
                    related_function,
                    red_init_fun_name,
                    TL::Type::get_void_type(),
                    red_init_parameter_names,
                    red_init_parameter_types);

            TL::Scope red_init_inside_scope = red_init_fun_sym.get_related_scope();

            // 2. Build function code from OpenMP::Reduction initializer

            // 2.1. Obtain and prepare the initializer statements

            Nodecl::NodeclBase initializer_expr =
                reduction_info.get_initializer().shallow_copy();

            if (reduction_info.get_is_initialization())
            {
                // Initializer expressions like 'omp_priv = expr'. The initializer
                // only represents the rhs expression (in our example, 'expr'). We
                // need to build the assignment manually.

                // We don't mind if it's an array reduction, as we are using
                // omp_priv symbol here, which will be of the base type
                initializer_expr = Nodecl::Assignment::make(
                        omp_priv.make_nodecl(/* set_ref_type */ true),
                        initializer_expr,
                        omp_priv.get_type().no_ref());
            }

            Nodecl::NodeclBase initializer_stmts =
                Nodecl::ExpressionStatement::make(
                        initializer_expr);

            // 2.2. Obtain function parameter symbols

            TL::Symbol param_omp_priv =
                red_init_inside_scope.get_symbol_from_name(omp_priv.get_name());
            ERROR_CONDITION(!param_omp_priv.is_valid(), "Symbol %s not found",
                    omp_priv.get_name().c_str());

            TL::Symbol param_omp_orig =
                red_init_inside_scope.get_symbol_from_name(omp_orig.get_name());
            ERROR_CONDITION(!param_omp_orig.is_valid(), "Symbol %s not found",
                    omp_orig.get_name().c_str());

            TL::Symbol param_size =
                red_init_inside_scope.get_symbol_from_name("size");
            ERROR_CONDITION(!param_size.is_valid(), "Symbol %s not found",
                    param_size.get_name().c_str());

            // 2.3. Prepare translation map from original symbols in
            // OpenMP::Reduction initializer to expressions using function
            // parameters

            std::map<TL::Symbol, Nodecl::NodeclBase> translation_map;

            if (red._reduction_type.is_array())
            {
                // 2.3.A. For array reductions, we need to map an expression
                // written in terms of scalars onto an array section

                // 2.3.A.1. Generate loops to iterate through the array section

                std::string num_elements_sym_name = "num_elements";
                TL::Symbol num_elements_sym = red_init_inside_scope.new_symbol(num_elements_sym_name);
                symbol_entity_specs_set_is_user_declared(num_elements_sym.get_internal_symbol(), 1);
                num_elements_sym.get_internal_symbol()->kind = SK_VARIABLE;
                num_elements_sym.set_type(param_size.get_type());

                num_elements_sym.set_value(
                        Nodecl::Div::make(
                            param_size.make_nodecl(/* set_ref_type */ true),
                            Nodecl::Sizeof::make(
                                Nodecl::Type::make(base_element_type),
                                /* expr */ Nodecl::NodeclBase::null(),
                                TL::Type::get_size_t_type()),
                            param_size.get_type()));

                std::string induction_sym_name = "i";
                TL::Symbol induction_sym = red_init_inside_scope.new_symbol(induction_sym_name);
                symbol_entity_specs_set_is_user_declared(induction_sym.get_internal_symbol(), 1);
                induction_sym.get_internal_symbol()->kind = SK_VARIABLE;
                induction_sym.set_type(param_size.get_type());
                induction_sym.set_value(const_value_to_nodecl(const_value_get_signed_int(0)));

                Nodecl::NodeclBase initialization =
                    Nodecl::ObjectInit::make(induction_sym);

                Nodecl::NodeclBase condition = Nodecl::LowerThan::make(
                        induction_sym.make_nodecl(/* set_ref_type */ true),
                        num_elements_sym.make_nodecl(/* set_ref_type */ true),
                        TL::Type::get_bool_type());

                Nodecl::NodeclBase step = Nodecl::Preincrement::make(
                        induction_sym.make_nodecl(/* set_ref_type */ true),
                        induction_sym.get_type());

                Nodecl::NodeclBase loop_control = Nodecl::LoopControl::make(
                        Nodecl::List::make(initialization),
                        condition,
                        step);

                // Note that we are not creating any context / compound stmt
                // Thus, the body has to be always a single statement
                Nodecl::NodeclBase loop = Nodecl::ForStatement::make(
                        loop_control,
                        Nodecl::List::make(
                            initializer_stmts),
                        /* loop-name */ Nodecl::NodeclBase::null());

                // 2.3.A.2. Create array subscript node using induction
                // variables and add it to translation map

                Nodecl::NodeclBase omp_priv_array_subscript =
                    Nodecl::ArraySubscript::make(
                            param_omp_priv.make_nodecl(/* set_ref_type */ true),
                            Nodecl::List::make(
                                induction_sym.make_nodecl(/* set_ref_type */ true)),
                            base_element_type);

                Nodecl::NodeclBase omp_orig_array_subscript =
                    Nodecl::ArraySubscript::make(
                            param_omp_orig.make_nodecl(/* set_ref_type */ true),
                            Nodecl::List::make(
                                induction_sym.make_nodecl(/* set_ref_type */ true)),
                            base_element_type);

                translation_map[omp_priv] = omp_priv_array_subscript;
                translation_map[omp_orig] = omp_orig_array_subscript;

                // 2.3.A.3. Set the initializer statements as the symbols
                // initializations and the loop itself

                initializer_stmts = Nodecl::List::make(
                        Nodecl::ObjectInit::make(num_elements_sym),
                        loop);
            }
            else
            {
                // 2.3.B. For scalar reductions we only need to map the
                // expression symbols to parameters

                translation_map[omp_priv] = Nodecl::Dereference::make(
                        param_omp_priv.make_nodecl(/* set_ref_type */ true),
                        param_omp_priv.get_type().get_lvalue_reference_to());

                translation_map[omp_orig] = Nodecl::Dereference::make(
                        param_omp_orig.make_nodecl(/* set_ref_type */ true),
                        param_omp_orig.get_type().get_lvalue_reference_to());
            }

            // 2.4. translate initializer

            TranslateReductionExpr translate_reduction_expr_visitor(translation_map);

            translate_reduction_expr_visitor.walk(initializer_expr);

            // 3. Build function and append to top level

            Nodecl::NodeclBase red_init_fun_code, red_init_empty_stmt;
            SymbolUtils::build_empty_body_for_function(
                    red_init_fun_sym,
                    red_init_fun_code,
                    red_init_empty_stmt);
            Nodecl::Utils::append_to_top_level_nodecl(red_init_fun_code);

            red_init_empty_stmt.replace(initializer_stmts);

            return red_init_fun_sym;
        }

        TL::Symbol create_reduction_combiner_function(
                const ReductionItem& red,
                const TL::Symbol& related_function,
                int function_identifier)
        {
            OpenMP::Reduction& reduction_info = *red._reduction_info;
            TL::Symbol omp_out = reduction_info.get_omp_out();
            TL::Symbol omp_in = reduction_info.get_omp_in();

            // 1. Create function symbol

            std::string red_comb_fun_name;
            {
                std::stringstream ss;
                ss << "nanos6_reduction_comb_" << function_identifier;
                red_comb_fun_name = ss.str();
            }

            TL::ObjectList<std::string> red_comb_parameter_names(3);
            red_comb_parameter_names[0] = omp_out.get_name();
            red_comb_parameter_names[1] = omp_in.get_name();
            red_comb_parameter_names[2] = "size";

            TL::Type base_element_type =
                get_base_element_type(red._reduction_type.no_ref());

            TL::ObjectList<TL::Type> red_comb_parameter_types(3);
            red_comb_parameter_types[0] = base_element_type.get_pointer_to();
            red_comb_parameter_types[1] = base_element_type.get_pointer_to();
            red_comb_parameter_types[2] = TL::Type::get_size_t_type();

            TL::Symbol red_comb_fun_sym = SymbolUtils::new_function_symbol(
                    related_function,
                    red_comb_fun_name,
                    TL::Type::get_void_type(),
                    red_comb_parameter_names,
                    red_comb_parameter_types);

            TL::Scope red_comb_inside_scope = red_comb_fun_sym.get_related_scope();

            // 2. Build function code from OpenMP::Reduction combiner

            // 2.1. Obtain function parameter symbols

            TL::Symbol param_omp_out =
                red_comb_inside_scope.get_symbol_from_name(omp_out.get_name());
            ERROR_CONDITION(!param_omp_out.is_valid(), "Symbol %s not found",
                    omp_out.get_name().c_str());

            TL::Symbol param_omp_in =
                red_comb_inside_scope.get_symbol_from_name(omp_in.get_name());
            ERROR_CONDITION(!param_omp_in.is_valid(), "Symbol %s not found",
                    omp_in.get_name().c_str());

            TL::Symbol param_size =
                red_comb_inside_scope.get_symbol_from_name("size");
            ERROR_CONDITION(!param_size.is_valid(), "Symbol %s not found",
                    param_size.get_name().c_str());

            // 2.2. Obtain and prepare the combiner statements

            Nodecl::NodeclBase combiner_expr =
                reduction_info.get_combiner().shallow_copy();

            Nodecl::NodeclBase combiner_stmts =
                Nodecl::ExpressionStatement::make(
                        combiner_expr);

            // 2.3. Prepare translation map from original symbols in
            // OpenMP::Reduction combiner to expressions using function parameters

            std::map<TL::Symbol, Nodecl::NodeclBase> translation_map;

            if (red._reduction_type.is_array())
            {
                // 2.3.A. For array reductions, we need to map an expression
                // written in terms of scalars onto an array section

                // 2.3.A.1. Generate loops to iterate through the array section

                std::string num_elements_sym_name = "num_elements";
                TL::Symbol num_elements_sym = red_comb_inside_scope.new_symbol(num_elements_sym_name);
                symbol_entity_specs_set_is_user_declared(num_elements_sym.get_internal_symbol(), 1);
                num_elements_sym.get_internal_symbol()->kind = SK_VARIABLE;
                num_elements_sym.set_type(param_size.get_type());

                num_elements_sym.set_value(
                        Nodecl::Div::make(
                            param_size.make_nodecl(/* set_ref_type */ true),
                            Nodecl::Sizeof::make(
                                Nodecl::Type::make(base_element_type),
                                /* expr */ Nodecl::NodeclBase::null(),
                                TL::Type::get_size_t_type()),
                            param_size.get_type()));

                std::string induction_sym_name = "i";
                TL::Symbol induction_sym = red_comb_inside_scope.new_symbol(induction_sym_name);
                symbol_entity_specs_set_is_user_declared(induction_sym.get_internal_symbol(), 1);
                induction_sym.get_internal_symbol()->kind = SK_VARIABLE;
                induction_sym.set_type(param_size.get_type());
                induction_sym.set_value(const_value_to_nodecl(const_value_get_signed_int(0)));

                Nodecl::NodeclBase initialization =
                    Nodecl::ObjectInit::make(induction_sym);

                Nodecl::NodeclBase condition = Nodecl::LowerThan::make(
                        induction_sym.make_nodecl(/* set_ref_type */ true),
                        num_elements_sym.make_nodecl(/* set_ref_type */ true),
                        TL::Type::get_bool_type());

                Nodecl::NodeclBase step = Nodecl::Preincrement::make(
                        induction_sym.make_nodecl(/* set_ref_type */ true),
                        induction_sym.get_type());

                Nodecl::NodeclBase loop_control = Nodecl::LoopControl::make(
                        Nodecl::List::make(initialization),
                        condition,
                        step);

                // Note that we are not creating any context / compound stmt
                // Thus, the body has to be always a single statement
                Nodecl::NodeclBase loop = Nodecl::ForStatement::make(
                        loop_control,
                        Nodecl::List::make(
                            combiner_stmts),
                        /* loop-name */ Nodecl::NodeclBase::null());

                // 2.3.A.2. Create array subscript node using induction
                // variables and add it to translation map

                Nodecl::NodeclBase omp_out_array_subscript =
                    Nodecl::ArraySubscript::make(
                            param_omp_out.make_nodecl(/* set_ref_type */ true),
                            Nodecl::List::make(
                                induction_sym.make_nodecl(/* set_ref_type */ true)),
                            base_element_type);

                Nodecl::NodeclBase omp_in_array_subscript =
                    Nodecl::ArraySubscript::make(
                            param_omp_in.make_nodecl(/* set_ref_type */ true),
                            Nodecl::List::make(
                                induction_sym.make_nodecl(/* set_ref_type */ true)),
                            base_element_type);

                translation_map[omp_out] = omp_out_array_subscript;
                translation_map[omp_in] = omp_in_array_subscript;

                // 2.3.A.3. Set the initializer statements as the symbols
                // initializations and the loop itself

                combiner_stmts = Nodecl::List::make(
                        Nodecl::ObjectInit::make(num_elements_sym),
                        loop);
            }
            else
            {
                // 2.3.B. For scalar reductions we only need to map the
                // expression symbols to parameters

                translation_map[omp_out] = Nodecl::Dereference::make(
                        param_omp_out.make_nodecl(/* set_ref_type */ true),
                        param_omp_out.get_type().get_lvalue_reference_to());

                translation_map[omp_in] = Nodecl::Dereference::make(
                        param_omp_in.make_nodecl(/* set_ref_type */ true),
                        param_omp_in.get_type().get_lvalue_reference_to());
            }

            // 2.4. translate combiner

            TranslateReductionExpr translate_reduction_expr_visitor(translation_map);

            translate_reduction_expr_visitor.walk(combiner_expr);

            // 3. Build function and append to top level

            Nodecl::NodeclBase red_comb_fun_code, red_comb_empty_stmt;
            SymbolUtils::build_empty_body_for_function(
                    red_comb_fun_sym,
                    red_comb_fun_code,
                    red_comb_empty_stmt);
            Nodecl::Utils::append_to_top_level_nodecl(red_comb_fun_code);

            red_comb_empty_stmt.replace(combiner_stmts);

            return red_comb_fun_sym;
        }
    }

    void TaskProperties::create_reduction_functions(
            TL::Symbol &reduction_initializers,
            TL::Symbol &reduction_combiners)
    {
        if (_env.reduction.empty())
            return;

        // 1. Build lists of reduction functions

        Nodecl::List reduction_initializer_functions;
        Nodecl::List reduction_combiner_functions;

        TL::ObjectList<TL::Type> parameter_types;
        parameter_types.append(TL::Type::get_void_type().get_pointer_to());
        parameter_types.append(TL::Type::get_void_type().get_pointer_to());
        parameter_types.append(TL::Type::get_size_t_type());

        TL::Type reduction_function_type =
            TL::Type::get_void_type().get_function_returning(
                    parameter_types)
            .get_pointer_to();

        for (TL::ObjectList<ReductionItem>::const_iterator it = _env.reduction.begin();
                it != _env.reduction.end();
                it++)
        {
            // 1.1. Obtain reduction function symbols

            TL::Symbol reduction_initializer_function_sym;
            TL::Symbol reduction_combiner_function_sym;

            // Note: Find in terms of reduction info & type, it doesn't consider the symbol
            TL::Nanos6::Lower::reduction_functions_map_t::iterator reduction_functions_it =
                _lower_visitor->_reduction_functions_map.find(*it);
            if (reduction_functions_it == _lower_visitor->_reduction_functions_map.end())
            {
                // Create new reduction functions

                TL::Counter &counter =
                    TL::CounterManager::get_counter("nanos6-reduction");

                reduction_initializer_function_sym =
                    create_reduction_initializer_function(*it, _related_function, counter);

                reduction_combiner_function_sym =
                    create_reduction_combiner_function(*it, _related_function, counter);

                counter++;

                if (IS_FORTRAN_LANGUAGE)
                {
                    reduction_initializer_function_sym =
                        compute_mangled_function_symbol_from_symbol(reduction_initializer_function_sym);
                    reduction_combiner_function_sym =
                        compute_mangled_function_symbol_from_symbol(reduction_combiner_function_sym);
                }
                else if (IS_CXX_LANGUAGE)
                {
                    Nodecl::Utils::prepend_to_enclosing_top_level_location(
                            _task_body,
                            Nodecl::CxxDef::make(
                                Nodecl::NodeclBase::null(),
                                reduction_initializer_function_sym));

                    Nodecl::Utils::prepend_to_enclosing_top_level_location(
                            _task_body,
                            Nodecl::CxxDef::make(
                                Nodecl::NodeclBase::null(),
                                reduction_combiner_function_sym));
                }

                _lower_visitor->_reduction_functions_map[*it] =
                    TL::Nanos6::Lower::ReductionFunctions(reduction_initializer_function_sym, reduction_combiner_function_sym);
            }
            else
            {
                // Reuse reduction functions

                TL::Nanos6::Lower::ReductionFunctions reduction_functions =
                    reduction_functions_it->second;

                reduction_initializer_function_sym = reduction_functions.initializer;
                reduction_combiner_function_sym = reduction_functions.combiner;
            }

            // 1.2. Add reduction functions nodes (with type casts) to their
            // corresponding list

            Nodecl::NodeclBase reduction_initializer_function =
                Nodecl::Conversion::make(
                        reduction_initializer_function_sym.make_nodecl(/* set_ref_type */ true),
                        reduction_function_type);
            reduction_initializer_function.set_text("C");

            Nodecl::NodeclBase reduction_combiner_function =
                Nodecl::Conversion::make(
                        reduction_combiner_function_sym.make_nodecl(/* set_ref_type */ true),
                        reduction_function_type);
            reduction_combiner_function.set_text("C");

            reduction_initializer_functions.append(
                    reduction_initializer_function);

            reduction_combiner_functions.append(
                    reduction_combiner_function);
        }

        // 2. Create global symbols to hold reduction functions

        TL::Scope scope;
        if (IS_CXX_LANGUAGE && _related_function.is_member())
        {
            TL::Type class_type = _related_function.get_class_type();
            scope =
                ::class_type_get_inner_context(class_type.get_internal_type());
        }
        else
        {
            scope = TL::Scope::get_global_scope();
        }

        // 2.1. Reduction initializers

        std::string reduction_initializers_name =
            get_new_name("nanos6_reduction_initializers");

        create_static_variable_depending_on_function_context(
                reduction_initializers_name,
                reduction_function_type.get_array_to(),
                _task_body,
                _phase,
                reduction_initializers);

        // 2.2. Reduction combiners

        std::string reduction_combiners_name =
            get_new_name("nanos6_reduction_combiners");

        create_static_variable_depending_on_function_context(
                reduction_combiners_name,
                reduction_function_type.get_array_to(),
                _task_body,
                _phase,
                reduction_combiners);

        // 3. Build brace initializers (ie {A, B, C}) for the global symbols
        // and set them as their value

        Nodecl::NodeclBase reduction_initializers_value =
            Nodecl::StructuredValue::make(
                    reduction_initializer_functions,
                    /* form */ Nodecl::StructuredValueBracedImplicit::make(),
                    reduction_function_type.get_array_to());

        Nodecl::NodeclBase reduction_combiners_value =
            Nodecl::StructuredValue::make(
                    reduction_combiner_functions,
                    /* form */ Nodecl::StructuredValueBracedImplicit::make(),
                    reduction_function_type.get_array_to());

        reduction_initializers.set_value(reduction_initializers_value);
        reduction_combiners.set_value(reduction_combiners_value);
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

    void TaskProperties::compute_reduction_arguments_register_dependence(
            TL::DataReference& data_ref,
            // Out
            TL::ObjectList<Nodecl::NodeclBase>& arguments_list)
    {
        // 1st argument: type operation identifier
        TL::ObjectList<ReductionItem> red_items =
            _env.reduction.find<TL::Symbol>(&ReductionItem::get_symbol, data_ref.get_base_symbol());

        ERROR_CONDITION(red_items.empty(), "No reduction item for symbol '%s'",
                data_ref.get_base_symbol().get_name().c_str());

        ReductionItem& reduction_item = *red_items.begin();
        TL::OpenMP::Reduction *reduction_info = reduction_item._reduction_info;

        Nodecl::NodeclBase arg1_type_op;

        if (reduction_info->is_builtin())
        {
            Nodecl::NodeclBase type_node;
            Nodecl::NodeclBase operation_node;
            get_reduction_type(reduction_info->get_type(), type_node);
            get_reduction_operation(*reduction_info, operation_node);

            arg1_type_op = Nodecl::Add::make(
                    type_node,
                    operation_node,
                    reduction_info->get_type());
        }
        else
        {
            // Note: For UDRs, we are using the combiner function address as
            // the 'type_op' argument. Note that this behaviour doesn't allow
            // reduction tasks on the same UDR declared on different compile
            // units to run concurrently. Also, we need to ensure the
            // combination function is defined at this point.

            // Note: Find in terms of reduction info & type, it doesn't consider the symbol
            TL::Nanos6::Lower::reduction_functions_map_t::iterator it =
                _lower_visitor->_reduction_functions_map.find(reduction_item);

            ERROR_CONDITION(it == _lower_visitor->_reduction_functions_map.end(),
                    "No reduction functions found", 0);

            arg1_type_op = Nodecl::Conversion::make(
                    it->second.combiner.make_nodecl(/* set_ref_type */ true),
                    TL::Type::get_long_int_type());
            arg1_type_op.set_text("C");
        }

        // 2nd argument: reduction identifier within task
        Nodecl::NodeclBase arg2_id = const_value_to_nodecl(
                const_value_get_unsigned_int(_num_reductions));

        // Increment number of registered reductions for the task (used as id when registering)
        _num_reductions++;

        arguments_list.append(arg1_type_op);
        arguments_list.append(arg2_id);
    }

    namespace {
        void compute_arguments_register_dependence(
                TL::DataReference& data_ref,
                TL::Symbol handler,
                std::map<TL::Symbol, unsigned int> &dep_symbols_to_id,
                // Out
                TL::ObjectList<Nodecl::NodeclBase>& arguments_list)
        {
            // task handler
            arguments_list.append(handler.make_nodecl(/* set_ref_type */ true));

            // sym identifier
            std::map<TL::Symbol, unsigned int>::const_iterator it;
            it = dep_symbols_to_id.find(data_ref.get_base_symbol());
            ERROR_CONDITION(it == dep_symbols_to_id.end(),
                    "Unexpected symbol '%s'", data_ref.get_base_symbol().get_name().c_str());

            arguments_list.append(
                    const_value_to_nodecl(const_value_get_unsigned_int(it->second)));

            // dependence text
            std::string dependence_text = Codegen::get_current().codegen_to_str(data_ref, data_ref.retrieve_context());
            arguments_list.append(const_value_to_nodecl(
                        const_value_make_string_null_ended(
                            dependence_text.c_str(),
                            strlen(dependence_text.c_str()))));

            compute_base_address_and_dimensionality_information(data_ref, arguments_list);
        }
    }

    void TaskProperties::register_dependence(
        TL::DataReference &data_ref,
        TL::Symbol handler,
        Nodecl::Utils::SymbolMap &symbol_map,
        TL::Symbol register_fun,
        // Out
        Nodecl::List &register_statements)
    {
        bool is_reduction =
            register_fun.get_name().find("reduction") != std::string::npos;

        TL::ObjectList<Nodecl::NodeclBase> arguments_list;
        if (is_reduction)
        {
            if (IS_FORTRAN_LANGUAGE)
            {
                error_printf_at(data_ref.get_locus(),
                        "Task reductions are not yet supported in Fortran\n");
            }
            compute_reduction_arguments_register_dependence(data_ref, arguments_list);
        }
        compute_arguments_register_dependence(data_ref, handler, _dep_symbols_to_id, arguments_list);

        Nodecl::NodeclBase function_call =
            Nodecl::ExpressionStatement::make(
                    Nodecl::FunctionCall::make(
                        register_fun.make_nodecl(/* set_ref_type */ true),
                        Nodecl::List::make(arguments_list),
                        /* alternate-symbol */ Nodecl::NodeclBase::null(),
                        /* function-form */ Nodecl::NodeclBase::null(),
                        TL::Type::get_void_type()));

        register_statements.append(
                Nodecl::Utils::deep_copy(function_call, TL::Scope::get_global_scope(), symbol_map));
    }



    void TaskProperties::register_multidependence(
        TL::DataReference &data_ref,
        TL::Symbol handler,
        Nodecl::Utils::SymbolMap &symbol_map,
        TL::Symbol register_fun,
        TL::Scope scope,
        // Out
        Nodecl::List &register_statements)
    {
        TL::ObjectList<TL::DataReference::MultiRefIterator> multireferences = data_ref.get_iterators_of_multireference();

        Nodecl::Utils::SimpleSymbolMap extended_symbol_map(&symbol_map);
        create_induction_variables_for_iterators(
                multireferences, scope, extended_symbol_map, register_statements);

        // This empty stmt will be replaced by the function call to the runtime
        Nodecl::NodeclBase empty_stmt  = Nodecl::EmptyStatement::make();
        Nodecl::List inner_stmts = Nodecl::List::make(empty_stmt);

        Nodecl::List loop_stmts = create_loop_stmts_for_iterators(
                multireferences, extended_symbol_map, inner_stmts, scope, _related_function);

        Nodecl::NodeclBase base_exp = data_ref.get_expression_of_multireference();
        TL::DataReference base_data_ref = base_exp;
        Nodecl::List base_reg;

        register_dependence(
                base_data_ref,
                handler,
                extended_symbol_map,
                register_fun,
                // Out
                base_reg);

        empty_stmt.replace(base_reg);

    register_statements.append(loop_stmts);
    }

    TL::Symbol TaskProperties::create_dependences_unpacked_function(
            const std::string &common_name)
    {
        TL::ObjectList<std::string> unpacked_fun_param_names;
        TL::ObjectList<TL::Type> unpacked_fun_param_types;
        std::map<TL::Symbol, std::string> symbols_to_param_names;

        std::string unpacked_fun_name = get_new_name("nanos6_unpacked_" + common_name);

        AddParameter add_params_functor(
            /* in */ _environment_capture,
            /* out */ unpacked_fun_param_names,
            /* out */ unpacked_fun_param_types,
            /* out */ symbols_to_param_names);

        _env.captured_value.map(add_params_functor);
        _env.private_.map(add_params_functor);
        _env.shared.map(add_params_functor);

        unpacked_fun_param_names.append("handler");
        unpacked_fun_param_types.append(TL::Type::get_void_type().get_pointer_to());

        TL::Symbol unpacked_function = SymbolUtils::new_function_symbol(
                _related_function,
                unpacked_fun_name,
                TL::Type::get_void_type(),
                unpacked_fun_param_names,
                unpacked_fun_param_types);

        Nodecl::NodeclBase unpacked_fun_function_code, unpacked_fun_empty_stmt;
        SymbolUtils::build_empty_body_for_function(
            unpacked_function, unpacked_fun_function_code, unpacked_fun_empty_stmt);

        TL::Scope unpacked_fun_inside_scope = unpacked_function.get_related_scope();

        fortran_add_types(unpacked_fun_inside_scope);

        // Prepare deep copy and remember those parameters that need fixup
        Nodecl::Utils::SimpleSymbolMap symbol_map;
        TL::ObjectList<TL::Symbol> parameters_to_update_type;

        MapSymbols map_symbols_functor(
                unpacked_fun_inside_scope,
                symbols_to_param_names,
                // Out
                parameters_to_update_type,
                symbol_map);

        _env.captured_value.map(map_symbols_functor);
        _env.private_.map(map_symbols_functor);
        _env.shared.map(map_symbols_functor);

        update_function_type_if_needed(
                unpacked_function, parameters_to_update_type, symbol_map);

        TL::Symbol handler = unpacked_fun_inside_scope.get_symbol_from_name("handler");
        ERROR_CONDITION(!handler.is_valid(), "Invalid symbol", 0);

        TL::Scope global_context = TL::Scope::get_global_scope();

        struct DependencesSet
        {
            TL::ObjectList<Nodecl::NodeclBase> &dep_list;
            std::string func_name;
            unsigned int min_api_vers;
            std::string api_feature;
        } deps[] = {
            { _env.dep_in,    "nanos6_register_region_read_depinfo", 5, "input dependences"      },
            { _env.dep_out,   "nanos6_register_region_write_depinfo", 5, "output dependences"    },
            { _env.dep_inout, "nanos6_register_region_readwrite_depinfo", 5, "inout dependences" },

            { _env.dep_weakin,    "nanos6_register_region_weak_read_depinfo", 5, "weak input dependences"      },
            { _env.dep_weakout,   "nanos6_register_region_weak_write_depinfo", 5, "weak output dependences"    },
            { _env.dep_weakinout, "nanos6_register_region_weak_readwrite_depinfo", 5, "weak inout dependences" },

            { _env.dep_commutative, "nanos6_register_region_commutative_depinfo", 5, "commutative dependences" },
            { _env.dep_concurrent,  "nanos6_register_region_concurrent_depinfo", 5, "concurrent dependences"   },

            { _env.dep_weakcommutative, "nanos6_register_region_weak_commutative_depinfo", 6, "weak commutative dependences" },

            { _env.dep_reduction,     "nanos6_register_region_reduction_depinfo", 5, "reduction dependences"           },
            { _env.dep_weakreduction, "nanos6_register_region_weak_reduction_depinfo", 5, "weak reduction dependences" },
        };

        for (DependencesSet *dep_set = deps;
             dep_set != (DependencesSet *)(&deps + 1);
             dep_set++)
        {
            TL::ObjectList<Nodecl::NodeclBase> &dep_list = dep_set->dep_list;

            if (dep_list.empty())
                continue;

            Interface::family_must_be_at_least("nanos6_multidimensional_dependencies_api", dep_set->min_api_vers, dep_set->api_feature);

            for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = dep_list.begin();
                 it != dep_list.end();
                 it++)
            {
                TL::DataReference data_ref = *it;
                TL::Type data_type = data_ref.get_data_type();

                TL::Symbol register_fun;
                {
                    int max_dimensions = _phase->nanos6_api_max_dimensions();
                    ERROR_CONDITION(data_type.is_array() &&
                            (data_type.get_num_dimensions() > max_dimensions),
                            "Maximum number of data dimensions allowed is %d",
                            max_dimensions);

                    int num_dims_dep = data_type.is_array() ? data_type.get_num_dimensions() : 1;
                    std::stringstream ss;
                    ss << dep_set->func_name << num_dims_dep;

                    register_fun = get_nanos6_function_symbol(ss.str());
                }

                Nodecl::NodeclBase curr_num_deps;
                Nodecl::List register_statements;
                if (!data_ref.is_multireference())
                {
                    curr_num_deps = const_value_to_nodecl(const_value_get_one(TL::Type::get_size_t_type().get_size(), 1));

                    register_dependence(
                            data_ref,
                            handler,
                            symbol_map,
                            register_fun,
                            // Out
                            register_statements);
                }
                else
                {
                    register_multidependence(
                            data_ref,
                            handler,
                            symbol_map,
                            register_fun,
                            unpacked_fun_inside_scope,
                            // Out
                            register_statements);
                }

                unpacked_fun_empty_stmt.prepend_sibling(register_statements);
            }
        }

        if (IS_CXX_LANGUAGE && !_related_function.is_member())
        {
            Nodecl::Utils::prepend_to_enclosing_top_level_location(_task_body,
                    Nodecl::CxxDecl::make(
                        Nodecl::Context::make(
                            Nodecl::NodeclBase::null(),
                            _related_function.get_scope()),
                        unpacked_function));
        }

        Nodecl::Utils::append_to_enclosing_top_level_location(
            _task_body, unpacked_fun_function_code);

        return unpacked_function;
    }

    TL::Symbol TaskProperties::create_dependences_function()
    {
        // Skip this function if the current task doesn't have any task dependence
        if (!_env.any_task_dependence)
            return TL::Symbol::invalid();

        const std::string common_name = "deps";
        TL::Symbol unpacked_function = create_dependences_unpacked_function(common_name);

        TL::ObjectList<std::string> unpacked_fun_param_names(2);
        TL::ObjectList<TL::Type> unpacked_fun_param_types(2);

        unpacked_fun_param_names[0] = "arg";
        unpacked_fun_param_types[0] = _info_structure.get_lvalue_reference_to();

        unpacked_fun_param_names[1] = "handler";
        unpacked_fun_param_types[1] = TL::Type::get_void_type().get_pointer_to();

        TL::Symbol dependences_function = create_outline_function(
                unpacked_function,
                common_name,
                unpacked_fun_param_names,
                unpacked_fun_param_types);

        return dependences_function;
    }

    TL::Symbol TaskProperties::create_priority_unpacked_function(
            const std::string& common_name)
    {
        TL::ObjectList<std::string> unpacked_fun_param_names;
        TL::ObjectList<TL::Type> unpacked_fun_param_types;
        std::map<TL::Symbol, std::string> symbols_to_param_names;

        std::string unpacked_fun_name = get_new_name("nanos6_unpacked_" + common_name);

        AddParameter add_params_functor(
            /* in */ _environment_capture,
            /* out */ unpacked_fun_param_names,
            /* out */ unpacked_fun_param_types,
            /* out */ symbols_to_param_names);

        _env.captured_value.map(add_params_functor);
        _env.private_.map(add_params_functor);
        _env.shared.map(add_params_functor);

        unpacked_fun_param_names.append("priority");
        unpacked_fun_param_types.append(get_nanos6_class_symbol("nanos6_priority_t")
                .get_user_defined_type().get_lvalue_reference_to());

        TL::Symbol unpacked_function = SymbolUtils::new_function_symbol(
                _related_function,
                unpacked_fun_name,
                TL::Type::get_void_type(),
                unpacked_fun_param_names,
                unpacked_fun_param_types);

        Nodecl::NodeclBase unpacked_function_code, unpacked_empty_stmt;
        SymbolUtils::build_empty_body_for_function(
            unpacked_function, unpacked_function_code, unpacked_empty_stmt);

        TL::Scope unpacked_fun_inside_scope = unpacked_function.get_related_scope();

        fortran_add_types(unpacked_fun_inside_scope);

        // Prepare deep copy and remember those parameters that need fixup
        Nodecl::Utils::SimpleSymbolMap symbol_map;
        TL::ObjectList<TL::Symbol> parameters_to_update_type;

        MapSymbols map_symbols_functor(
                unpacked_fun_inside_scope,
                symbols_to_param_names,
                // Out
                parameters_to_update_type,
                symbol_map);

        _env.captured_value.map(map_symbols_functor);
        _env.private_.map(map_symbols_functor);
        _env.shared.map(map_symbols_functor);

        update_function_type_if_needed(
                unpacked_function, parameters_to_update_type, symbol_map);

        if (IS_FORTRAN_LANGUAGE)
        {
            // Insert extra symbol declarations and add them to the symbol map
            // (e.g. functions and subroutines declared in other scopes)
            Nodecl::Utils::Fortran::ExtraDeclsVisitor fun_visitor(
                    symbol_map,
                    unpacked_fun_inside_scope,
                    _related_function);
            fun_visitor.insert_extra_symbols(_env.priority_clause);
        }

        Nodecl::NodeclBase priority_expr =
            Nodecl::Utils::deep_copy(_env.priority_clause, unpacked_fun_inside_scope, symbol_map);

        TL::Symbol result_sym = unpacked_fun_inside_scope.get_symbol_from_name("priority");

        if (!priority_expr.get_type().is_same_type(result_sym.get_type()))
        {
            priority_expr = Nodecl::Conversion::make(
                    priority_expr,
                    result_sym.get_type().no_ref(),
                    priority_expr.get_locus());
            priority_expr.set_text("C");
        }

        Nodecl::NodeclBase expr_stmt = Nodecl::ExpressionStatement::make(
                Nodecl::Assignment::make(
                    result_sym.make_nodecl(/* set_ref_type */ true),
                    priority_expr,
                    result_sym.get_type().no_ref().get_lvalue_reference_to()));

        unpacked_empty_stmt.replace(expr_stmt);

        Nodecl::Utils::prepend_to_enclosing_top_level_location(
                _task_body,
                unpacked_function_code);

        return unpacked_function;
    }

    TL::Symbol TaskProperties::create_priority_function()
    {
        // Skip this function if the current task doesn't have a priority clause
        if (_env.priority_clause.is_null())
            return TL::Symbol::invalid();

        const std::string common_name = "priority";
        TL::Symbol unpacked_function =
            create_priority_unpacked_function(common_name);

        TL::ObjectList<std::string> unpacked_fun_param_names(2);
        TL::ObjectList<TL::Type> unpacked_fun_param_types(2);

        unpacked_fun_param_names[0] = "arg";
        unpacked_fun_param_types[0] = _info_structure.get_lvalue_reference_to();

        unpacked_fun_param_names[1] = "priority";
        unpacked_fun_param_types[1] = get_nanos6_class_symbol("nanos6_priority_t")
            .get_user_defined_type().get_lvalue_reference_to();

        TL::Symbol outline_function = create_outline_function(
                unpacked_function,
                common_name,
                unpacked_fun_param_names,
                unpacked_fun_param_types);

        return outline_function;
    }

    TL::Symbol TaskProperties::create_destroy_function()
    {
        // Do not generate an empty function
        if (!_environment_capture.requires_destruction_function())
            return TL::Symbol::invalid();

        TL::ObjectList<std::string> destroy_param_names(1);
        TL::ObjectList<TL::Type> destroy_param_types(1);

        destroy_param_names[0] = "arg";
        destroy_param_types[0] = _info_structure.get_lvalue_reference_to();

        TL::Symbol destroy_function;
        Nodecl::NodeclBase destroy_empty_stmt;
        create_outline_function_common("destroy",
                destroy_param_names,
                destroy_param_types,
                destroy_function,
                destroy_empty_stmt);

        // Compute destroy statements
        TL::Scope destroy_inside_scope = destroy_empty_stmt.retrieve_context();
        TL::Symbol arg = destroy_inside_scope.get_symbol_from_name("arg");
        if (IS_FORTRAN_LANGUAGE)
            symbol_entity_specs_set_is_target(arg.get_internal_symbol(), 1);

        ERROR_CONDITION(!arg.is_valid() || !arg.is_parameter(), "Invalid symbol", 0);

        Nodecl::List destroy_stmts;

        // In Fortran we have to deallocate the arguments structure. We do the following hack:
        //
        //      subroutine destroy_fun(arg)
        //          type(T), target :: arg
        //          type(T), pointer :: ptr_to_arg
        //
        //          ptr_to_arg => arg
        //
        //          deallocate(ptr_to_arg)
        //      end subroutine destroy_fun
        TL::Symbol ptr_to_arg;
        if (IS_FORTRAN_LANGUAGE)
        {
            ptr_to_arg = destroy_inside_scope.new_symbol("ptr_to_arg");
            ptr_to_arg.get_internal_symbol()->kind = SK_VARIABLE;
            ptr_to_arg.get_internal_symbol()->type_information = _info_structure.get_pointer_to().get_lvalue_reference_to().get_internal_type();
            symbol_entity_specs_set_is_user_declared(ptr_to_arg.get_internal_symbol(), 1);

            destroy_stmts.append(
                    Nodecl::ExpressionStatement::make(
                        Nodecl::Assignment::make(
                            ptr_to_arg.make_nodecl(/* set_ref_type */ true),
                            arg.make_nodecl(/* set_ref_type */ true),
                            ptr_to_arg.get_type())));
        }

        TL::ObjectList<TL::Symbol> captured_and_private_symbols = append_two_lists(_env.captured_value, _env.private_);
        for (TL::ObjectList<TL::Symbol>::const_iterator it = captured_and_private_symbols.begin();
                it != captured_and_private_symbols.end();
                it++)
        {
            Nodecl::List current_destroy_stmts =
                _environment_capture.emit_symbol_destruction(
                    destroy_inside_scope,
                    arg,
                    *it);
            destroy_stmts.append(current_destroy_stmts);
        }

        if (IS_FORTRAN_LANGUAGE)
        {
            Nodecl::NodeclBase deallocate_stmt =
                Nodecl::FortranDeallocateStatement::make(
                        Nodecl::List::make(ptr_to_arg.make_nodecl(/*set_ref_type*/ true)),
                        /* options */ Nodecl::NodeclBase::null());

            destroy_stmts.append(deallocate_stmt);
        }


        ERROR_CONDITION(destroy_stmts.empty(), "Unexpected list", 0);
        destroy_empty_stmt.replace(destroy_stmts);

        return destroy_function;
    }

    TL::Symbol TaskProperties::create_duplicate_function()
    {
        //Only meaningful for loop constructs!
        if (!_env.task_is_loop)
            return TL::Symbol::invalid();

        if (!_environment_capture.requires_duplication_function())
            return TL::Symbol::invalid();

        TL::ObjectList<std::string> duplicate_param_names;
        TL::ObjectList<TL::Type> duplicate_param_types;

        duplicate_param_names.append("src");
        duplicate_param_types.append(_info_structure.get_lvalue_reference_to());

        duplicate_param_names.append("dst");
        duplicate_param_types.append(_info_structure.get_pointer_to().get_lvalue_reference_to());

        TL::Symbol duplicate_function;
        Nodecl::NodeclBase duplicate_empty_stmt;
        create_outline_function_common("duplicate",
                duplicate_param_names,
                duplicate_param_types,
                duplicate_function,
                duplicate_empty_stmt);

        TL::Scope dup_fun_inner_scope = duplicate_empty_stmt.retrieve_context();

        TL::Symbol src_data_env = dup_fun_inner_scope.get_symbol_from_name("src");
        TL::Symbol dst_data_env = dup_fun_inner_scope.get_symbol_from_name("dst");

        Nodecl::List captured_stmts;
        if (IS_FORTRAN_LANGUAGE)
        {
            Nodecl::NodeclBase allocate_stmt = Nodecl::FortranAllocateStatement::make(
                    Nodecl::List::make(dst_data_env.make_nodecl(/*set_ref_type*/ true)),
                    /* options */ Nodecl::NodeclBase::null(),
                    /* alloc-type */ Nodecl::NodeclBase::null());

            captured_stmts.append(allocate_stmt);
        }

        Nodecl::NodeclBase vla_offset;
        // 1. Traversing captured variables (firstprivate + other captures)
        for (TL::ObjectList<TL::Symbol>::iterator it = _env.captured_value.begin();
                it != _env.captured_value.end();
                it++)
        {
            Nodecl::List current_captured_stmts = _environment_capture.emit_copy_of_captured_symbol(
                dup_fun_inner_scope,
                src_data_env, dst_data_env,
                *it,
                /* inout */ vla_offset);

            captured_stmts.append(current_captured_stmts);
        }

        // 2. Traversing private symbols
        for (TL::ObjectList<TL::Symbol>::iterator it = _env.private_.begin();
                it != _env.private_.end();
                it++)
        {
            Nodecl::List current_captured_stmts = _environment_capture.emit_copy_of_private_symbol_allocation(
                dup_fun_inner_scope,
                src_data_env, dst_data_env,
                *it,
                /* inout */ vla_offset);

            captured_stmts.append(current_captured_stmts);
        }

        // Since we compute the offsets in advance, once all the capture
        // symbols have been treated we can safely free this tree
        if (!vla_offset.is_null())
            nodecl_free(vla_offset.get_internal_nodecl());

        // 3. Traversing SHARED variables
        for (TL::ObjectList<TL::Symbol>::iterator it = _env.shared.begin();
                it != _env.shared.end();
                it++)
        {
            Nodecl::List current_captured_stmts = _environment_capture.emit_copy_of_shared_symbol_location(
                src_data_env, dst_data_env,
                *it);

            captured_stmts.append(current_captured_stmts);
        }

        if (!captured_stmts.is_null())
            duplicate_empty_stmt.replace(captured_stmts);

        return duplicate_function;
    }

    void TaskProperties::capture_environment(
            TL::Symbol args,
            TL::Scope task_enclosing_scope,
            /* out */
            Nodecl::NodeclBase& captured_env)
    {
        Nodecl::List captured_list;

        Nodecl::NodeclBase vla_offset;

        // 1. Traversing captured variables (firstprivate + other captures)
        for (TL::ObjectList<TL::Symbol>::iterator it = _env.captured_value.begin();
                it != _env.captured_value.end();
                it++)
        {
            Nodecl::List current_captured_stmts =
                _environment_capture.emit_capture_of_captured_symbol(
                    task_enclosing_scope,
                    args,
                    *it,
                    /* inout */
                    vla_offset);

            captured_list.append(current_captured_stmts);
        }

        // 2. Traversing private symbols
        for (TL::ObjectList<TL::Symbol>::iterator it = _env.private_.begin();
                it != _env.private_.end();
                it++)
        {
            Nodecl::List current_captured_stmts =
                _environment_capture.emit_private_symbol_allocation(
                    task_enclosing_scope,
                    args,
                    *it,
                    /* inout */
                    vla_offset);

            captured_list.append(current_captured_stmts);
        }

        // Since we compute the offsets in advance, once all the capture
        // symbols have been treated we can safely free this tree
        if (!vla_offset.is_null())
            nodecl_free(vla_offset.get_internal_nodecl());

        // 3. Traversing SHARED variables
        for (TL::ObjectList<TL::Symbol>::iterator it = _env.shared.begin();
                it != _env.shared.end();
                it++)
        {
            Nodecl::List current_captured_stmts =
                _environment_capture.emit_caputure_of_shared_symbol_location(
                    task_enclosing_scope,
                    args,
                    *it,
                    _related_function.get_related_scope(),
                    /* inout */
                    _phase->get_extra_c_code());

            captured_list.append(current_captured_stmts);
        }

        captured_env = captured_list;
    }

    void TaskProperties::fortran_add_types(TL::Scope dest_scope)
    {
        TL::ObjectList<TL::Symbol> all_syms;
        all_syms.append(_env.shared);
        all_syms.append(_env.captured_value);

        TL::Nanos6::fortran_add_types(all_syms, dest_scope);
    }

    bool TaskProperties::task_is_loop() const
    {
        return _env.task_is_loop;
    }

    Nodecl::NodeclBase TaskProperties::get_lower_bound() const
    {
        return _taskloop_bounds.lower_bound;
    }

    Nodecl::NodeclBase TaskProperties::get_upper_bound() const
    {
        return _taskloop_bounds.upper_bound;
    }

    Nodecl::NodeclBase TaskProperties::get_step() const
    {
        return _taskloop_bounds.step;
    }

    Nodecl::NodeclBase TaskProperties::get_chunksize() const
    {
        return _env.chunksize;
    }

    void TaskProperties::compute_arguments_translation(
            const TL::Scope &outline_fun_inside_scope, Nodecl::List &stmts) const
    {
        ERROR_CONDITION(IS_FORTRAN_LANGUAGE, "The arguments translation is not implemented for Fortran yet", 0);

        TL::Symbol arg = outline_fun_inside_scope.get_symbol_from_name("arg");
        TL::Symbol atp = outline_fun_inside_scope.get_symbol_from_name("address_translation_table");
        TL::ObjectList<TL::Symbol> arg_struct_members = arg.get_type().no_ref().get_nonstatic_data_members();
        TL::ObjectList<TL::Symbol> atp_struct_members = atp.get_type().no_ref().points_to().get_nonstatic_data_members();
        GetField arg_struct_get_field(arg_struct_members);
        GetField atp_struct_get_field(atp_struct_members);

        Nodecl::List inner_stmts;
        for (std::map<TL::Symbol, unsigned int>::const_iterator it = _dep_symbols_to_id.begin();
                it != _dep_symbols_to_id.end();
                ++it)
        {
            TL::Symbol current_symbol(it->first);

            // args->p = (int*)(addr_trans_map[i].device_addr + ((size_t)args->p - addr_trans_map[i].local_addr))
            Nodecl::NodeclBase rhs, lhs;
            {
                // Compute LHS!
                Nodecl::NodeclBase arg_field = arg_struct_get_field(EnvironmentCapture::get_field_name(current_symbol.get_name()));
                Nodecl::NodeclBase member_access = Nodecl::ClassMemberAccess::make(
                        arg.make_nodecl(/*set_ref_type*/ true),
                        arg_field,
                        /*member literal*/ Nodecl::NodeclBase::null(),
                        arg_field.get_type().no_ref());

                lhs = member_access;

                // Compute RHS!
                Nodecl::NodeclBase array_access = Nodecl::ArraySubscript::make(
                        atp.make_nodecl(/* set_ref_type */true),
                        Nodecl::List::make(const_value_to_nodecl(const_value_get_unsigned_int(it->second))),
                        atp.get_type().no_ref().points_to());

                Nodecl::NodeclBase atp_device_addr_field = atp_struct_get_field("device_address");
                Nodecl::NodeclBase device_addr_expr = Nodecl::ClassMemberAccess::make(
                        array_access,
                        atp_device_addr_field,
                        /*member literal*/ Nodecl::NodeclBase::null(),
                        atp_device_addr_field.get_type().no_ref());

                Nodecl::NodeclBase atp_local_addr_field  = atp_struct_get_field("local_address");
                Nodecl::NodeclBase local_addr_expr = Nodecl::ClassMemberAccess::make(
                        array_access.shallow_copy(),
                        atp_local_addr_field,
                        /*member literal*/ Nodecl::NodeclBase::null(),
                        atp_local_addr_field.get_type().no_ref());

                Nodecl::NodeclBase casted_expr = Nodecl::Conversion::make(
                        member_access.shallow_copy(),
                        atp_local_addr_field.get_type().no_ref());

                casted_expr.set_text("C");

                rhs = Nodecl::Conversion::make(
                        Nodecl::Add::make(
                            device_addr_expr,
                            Nodecl::Minus::make(
                                casted_expr,
                                local_addr_expr,
                                local_addr_expr.get_type()),
                            device_addr_expr.get_type()),
                        arg_field.get_type().no_ref());

                rhs.set_text("C");
            }

            inner_stmts.append(Nodecl::ExpressionStatement::make(Nodecl::Assignment::make(lhs, rhs, lhs.get_type())));
        }

        Scope new_block_context_sc = new_block_context(outline_fun_inside_scope.get_decl_context());
        stmts.append(
                Nodecl::IfElseStatement::make(
                    atp.make_nodecl(/*set_ref_type*/ false),
                    Nodecl::List::make(
                        Nodecl::Context::make(
                            Nodecl::List::make(
                                Nodecl::CompoundStatement::make(
                                    inner_stmts,
                                    /* finalize */ Nodecl::NodeclBase::null())),
                            new_block_context_sc)),
                    Nodecl::NodeclBase::null()));
                }
} }
