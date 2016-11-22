/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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




#include "tl-ompss-target.hpp"
#include "tl-omp-core.hpp"
#include "cxx-diagnostic.h"

namespace TL
{
    namespace OpenMP
    {
        void Core::ompss_common_target_handler_pre(TL::PragmaCustomLine pragma_line,
                OmpSs::TargetContext& target_ctx,
                TL::Scope scope,
                bool is_pragma_task)
        {
            PragmaCustomClause onto = pragma_line.get_clause("onto");
            if (onto.is_defined())
            {
                target_ctx.onto = onto.get_arguments_as_expressions(scope);
            }

            PragmaCustomClause device = pragma_line.get_clause("device");
            if (device.is_defined())
            {
                ObjectList<std::string> device_list =
                    device.get_tokenized_arguments()
                          .map<const char *>(&std::string::c_str)
                          .map<std::string>(&strtolower);

                target_ctx.device_list.insert(device_list);
            }
            else
            {
                // In #pragma omp target a device is mandatory, for #pragma omp task
                // add it only if not empty
                std::string default_device = "smp";
                bool set_default_device = false;
                if (!is_pragma_task)
                {
                    warn_printf_at(pragma_line.get_locus(),
                            "'#pragma omp target' without 'device' clause. Assuming 'device(smp)'\n");
                    set_default_device = true;
                }
                else if (target_ctx.device_list.empty())
                {
                    set_default_device = true;
                    //If onto is defined and there is no device, default device is MPI
                    if (onto.is_defined()) default_device = "mpi";
                }

                if (set_default_device)
                {
                    target_ctx.device_list.clear();
                    target_ctx.device_list.append(default_device);
                }
            }

            PragmaCustomClause copy_in = pragma_line.get_clause("copy_in");
            if (copy_in.is_defined())
            {
                target_ctx.copy_in = parse_dependences_ompss_clause(copy_in, scope);
            }

            PragmaCustomClause copy_out = pragma_line.get_clause("copy_out");
            if (copy_out.is_defined())
            {
                target_ctx.copy_out = parse_dependences_ompss_clause(copy_out, scope);
            }

            PragmaCustomClause copy_inout = pragma_line.get_clause("copy_inout");
            if (copy_inout.is_defined())
            {
                target_ctx.copy_inout = parse_dependences_ompss_clause(copy_inout, scope);
            }

            PragmaCustomClause ndrange = pragma_line.get_clause("ndrange");
            if (ndrange.is_defined())
            {
                target_ctx.ndrange = ndrange.get_arguments_as_expressions(scope);
            }

            PragmaCustomClause shmem = pragma_line.get_clause("shmem");
            if (shmem.is_defined())
            {
                if (ndrange.is_defined())
                {
                    target_ctx.shmem = shmem.get_arguments_as_expressions(scope);
                }
                else
                {
                    warn_printf_at(pragma_line.get_locus(), "'shmem' clause cannot be used without the 'ndrange' clause, skipping\n");
                }
            }

            PragmaCustomClause file = pragma_line.get_clause("file");
            if (file.is_defined())
            {
                ObjectList<std::string> file_list = file.get_tokenized_arguments();
                if (file_list.size() != 1)
                {
                    warn_printf_at(pragma_line.get_locus(), "clause 'file' expects one identifier, skipping\n");
                }
                else
                {
                    target_ctx.file = file_list[0];
                }
            }

            PragmaCustomClause name = pragma_line.get_clause("name");
            if (name.is_defined())
            {
                ObjectList<std::string> name_list = name.get_tokenized_arguments();
                if (name_list.size() != 1)
                {
                    warn_printf_at(pragma_line.get_locus(), "clause 'name' expects one identifier, skipping\n");
                }
                else
                {
                    target_ctx.name = name_list[0];
                }
            }

            PragmaCustomClause copy_deps = pragma_line.get_clause("copy_deps");
            PragmaCustomClause no_copy_deps = pragma_line.get_clause("no_copy_deps");

            if (target_ctx.copy_deps == OmpSs::TargetContext::UNDEF_COPY_DEPS)
            {
                target_ctx.copy_deps = OmpSs::TargetContext::NO_COPY_DEPS;

                if (!copy_deps.is_defined()
                        && !no_copy_deps.is_defined())
                {
                    if (this->in_ompss_mode()
                            && this->copy_deps_by_default())
                    {
                        // Copy deps is true only if there is no copy_in, copy_out
                        // or copy_inout
                        if ( !copy_in.is_defined()
                                && !copy_out.is_defined()
                                && !copy_inout.is_defined())
                        {
                            target_ctx.copy_deps = OmpSs::TargetContext::COPY_DEPS;

                            if (!_already_informed_new_ompss_copy_deps)
                            {
                                info_printf_at(pragma_line.get_locus(),
                                        "unless 'no_copy_deps' is specified, "
                                        "the default in OmpSs is now 'copy_deps'\n");
                                info_printf_at(pragma_line.get_locus(),
                                        "this diagnostic is only shown for the "
                                        "first task found\n");

                                _already_informed_new_ompss_copy_deps = true;
                            }
                        }
                    }
                }
                else if (copy_deps.is_defined())
                {
                    target_ctx.copy_deps = OmpSs::TargetContext::COPY_DEPS;
                }
                else if (no_copy_deps.is_defined())
                {
                    target_ctx.copy_deps = OmpSs::TargetContext::NO_COPY_DEPS;
                }
                else
                {
                    internal_error("Code unreachable", 0);
                }
            }
            else if (target_ctx.copy_deps == OmpSs::TargetContext::NO_COPY_DEPS
                    || target_ctx.copy_deps == OmpSs::TargetContext::COPY_DEPS)
            {
                if (copy_deps.is_defined())
                {
                    warn_printf_at(pragma_line.get_locus(),
                            "ignoring 'copy_deps' clause because this context is already '%s'\n",
                            target_ctx.copy_deps == OmpSs::TargetContext::NO_COPY_DEPS ? "no_copy_deps" : "copy_deps");
                }
                if (no_copy_deps.is_defined())
                {
                    warn_printf_at(pragma_line.get_locus(),
                            "ignoring 'no_copy_deps' clause because this context is already '%s'\n",
                            target_ctx.copy_deps == OmpSs::TargetContext::NO_COPY_DEPS ? "no_copy_deps" : "copy_deps");
                }
            }
            else
            {
                internal_error("Code unreachable", 0);
            }
            ERROR_CONDITION(target_ctx.copy_deps == OmpSs::TargetContext::UNDEF_COPY_DEPS,
                    "Invalid value for copy_deps at this point", 0)

            PragmaCustomClause implements = pragma_line.get_clause("implements");
            if (implements.is_defined())
            {
                Symbol function_symbol (NULL);
                if (IS_C_LANGUAGE
                        || IS_CXX_LANGUAGE)
                {
                    ObjectList<Nodecl::NodeclBase> implements_list = implements.get_arguments_as_expressions(scope);

                    ERROR_CONDITION(implements_list.size() != 1, "clause 'implements' expects one identifier", 0);

                    Nodecl::NodeclBase implements_name = implements_list[0];

                    if (implements_name.is<Nodecl::Symbol>())
                    {
                        function_symbol = implements_name.get_symbol();
                    }
                    else if (implements_name.is<Nodecl::CxxDepNameSimple>())
                    {
                        ObjectList<TL::Symbol> symbols = scope.get_symbols_from_name(implements_name.get_text());

                        ERROR_CONDITION(symbols.size() != 1,
                                "The argument of the clause 'implements' cannot be an overloaded function identifier", 0);

                        function_symbol = symbols[0];
                    }
                    else
                    {
                        internal_error("Unexpected node", 0);
                    }
                }
                else if (IS_FORTRAN_LANGUAGE)
                {
                    ObjectList<std::string> implements_list = implements.get_tokenized_arguments();

                    ERROR_CONDITION(implements_list.size() != 1, "clause 'implements' expects one identifier", 0);

                    // Restore the scope chain we broke in an INTERFACE block
                    const decl_context_t* decl_context = scope.get_decl_context();
                    TL::Symbol current_procedure = scope.get_related_symbol();
                    decl_context->current_scope->contained_in = current_procedure.get_internal_symbol()->decl_context->current_scope;

                    TL::Scope fixed_scope = TL::Scope(decl_context);

                    ObjectList<TL::Symbol> symbols = fixed_scope.get_symbols_from_name(strtolower(implements_list[0].c_str()));

                    ERROR_CONDITION(symbols.size() != 1,"Unreachable code", 0);

                    function_symbol = symbols[0];
                }
                else
                {
                    internal_error("Unreachable code", 0);
                }

                if (function_symbol.is_valid()
                        && function_symbol.is_function())
                {
                    target_ctx.has_implements = true;
                    target_ctx.implements = function_symbol;
                }
                else
                {
                    warn_printf_at(pragma_line.get_locus(), "the argument of the clause 'implements' is not a valid identifier, skipping\n");
                }
            }
        }

        // #pragma omp target on top of a #pragma omp task outline
        void Core::ompss_target_handler_pre(TL::PragmaCustomDeclaration ctr)
        {
            PragmaCustomLine pragma_line = ctr.get_pragma_line();
            OmpSs::TargetContext target_ctx;

            ompss_common_target_handler_pre(pragma_line, target_ctx,
                    ctr.get_context_of_parameters().retrieve_context(),
                    /* is_pragma_task */ false);

            ompss_handle_implements_clause(
                    target_ctx, ctr.get_symbol(), ctr.get_locus());

            _target_context.push(target_ctx);
        }

        void Core::ompss_target_handler_post(TL::PragmaCustomDeclaration)
        {
            // It might be empty due to early exits in the preorder routine
            if (!_target_context.empty())
            {
                _target_context.pop();
            }
        }

        void Core::ompss_target_handler_pre(TL::PragmaCustomStatement ctr)
        {
            Nodecl::NodeclBase nested_pragma = ctr.get_statements();
            if (!nested_pragma.is_null()
                    && nested_pragma.is<Nodecl::List>())
            {
                nested_pragma = nested_pragma.as<Nodecl::List>().front();
                ERROR_CONDITION(!nested_pragma.is<Nodecl::Context>(), "Invalid node\n", 0);
                nested_pragma = nested_pragma.as<Nodecl::Context>().get_in_context().as<Nodecl::List>().front();
            }

            if (nested_pragma.is_null()
                    || !PragmaUtils::is_pragma_construct("omp", "task", nested_pragma))
            {
                warn_printf_at(ctr.get_locus(), "'#pragma omp target' must precede a '#pragma omp task' in this context\n");
                warn_printf_at(ctr.get_locus(), "skipping the whole '#pragma omp target'\n");
                return;
            }

            PragmaCustomLine pragma_line = ctr.get_pragma_line();
            OmpSs::TargetContext target_ctx;

            if (target_ctx.has_implements)
            {
                warn_printf_at(ctr.get_locus(), "'#pragma omp target' cannot have an 'implements' clause in this context\n");
                warn_printf_at(ctr.get_locus(), "skipping the whole '#pragma omp target'\n");
                return;
            }

            ompss_common_target_handler_pre(pragma_line,
                    target_ctx,
                    ctr.retrieve_context(),
                    /* is_pragma_task */ false);

            _target_context.push(target_ctx);
        }

        void Core::ompss_target_handler_post(TL::PragmaCustomStatement)
        {
            // It can already be empty if it has been consumed by a task
            if (!_target_context.empty())
            {
                _target_context.pop();
            }
        }

        static void add_copy_items(PragmaCustomLine construct, 
                DataEnvironment& data_sharing_environment,
                const ObjectList<Nodecl::NodeclBase>& list,
                TL::OmpSs::CopyDirection copy_direction,
                TL::OmpSs::TargetInfo& target_info,
                bool in_ompss_mode)
        {
            TL::ObjectList<TL::OmpSs::CopyItem> items;

            for (ObjectList<Nodecl::NodeclBase>::const_iterator it = list.begin();
                    it != list.end();
                    it++)
            {
                DataReference expr(*it);

                std::string warning;
                if (!expr.is_valid())
                {
                    expr.commit_diagnostic();
                    warn_printf_at(construct.get_locus(), "'%s' is not a valid copy data-reference, skipping\n",
                            expr.prettyprint().c_str());
                    continue;
                }

                // In OmpSs copies we may fix the data-sharing to something more natural
                if (in_ompss_mode)
                {
                    Symbol sym = expr.get_base_symbol();
                    // In OmpSs, the storage of a copy is always SHARED. Note that with this
                    // definition we aren't defining the data-sharings of the variables involved
                    // in that expression.
                    //
                    // About the data-sharings of the variables involved in the copy expression:
                    // - Fortran: the base symbol of the copy expression is always SHARED
                    // - C/C++:
                    //  * The base symbol of a trivial copy (i.e the expression is a symbol) must always be SHARED:
                    //          int x, a[10];
                    //          copy_inout(x) -> shared(x)
                    //          copy_inout(a) -> shared(a)
                    //  * The base symbol of an array expression or a reference to an array must be SHARED too:
                    //          copy_int a[10];
                    //          copy_inout(a[4])   -> shared(a)
                    //          copy_inout(a[1:2]) -> shared(a)
                    //  * The base symbol of a class member access must be shared too:
                    //          struct C { int z; } c;
                    //          copy_inout(c.z)       -> shared(c)
                    //  * Otherwise, the data-sharing of the base symbol is FIRSTPRIVATE:
                    //          int* p;
                    //          copy_inout(*p)     -> firstprivate(p)
                    //          copy_inout(p[10])  -> firstprivate(p)
                    //          copy_inout(p[1:2]) -> firstprivate(p)
                    //          copy_inout([10][20] p) -> firstprivate(p)
                    if (IS_FORTRAN_LANGUAGE)
                    {
                        data_sharing_environment.set_data_sharing(sym, DS_SHARED, DSK_IMPLICIT,
                                "the variable is mentioned in a copy and it did not have an explicit data-sharing");
                    }
                    else if (expr.is<Nodecl::Symbol>())
                    {
                        data_sharing_environment.set_data_sharing(sym, DS_SHARED, DSK_IMPLICIT,
                                "the variable is mentioned in a copy and it did not have an explicit data-sharing");
                    }
                    else if (sym.get_type().is_array()
                            || (sym.get_type().is_any_reference()
                                && sym.get_type().references_to().is_array()))
                    {
                        data_sharing_environment.set_data_sharing(sym, DS_SHARED, DSK_IMPLICIT,
                                "the variable is an array mentioned in a non-trivial copy "
                                "and it did not have an explicit data-sharing");
                    }
                    else if (sym.get_type().is_class())
                    {
                        data_sharing_environment.set_data_sharing(sym, DS_SHARED, DSK_IMPLICIT,
                                "the variable is an object mentioned in a non-trivial dependence "
                                "and it did not have an explicit data-sharing");
                    }
                    else
                    {
                        data_sharing_environment.set_data_sharing(sym, DS_FIRSTPRIVATE, DSK_IMPLICIT,
                                "the variable is a non-array mentioned in a non-trivial copy "
                                "and it did not have an explicit data-sharing");
                    }
                }

                TL::OmpSs::CopyItem copy_item(expr, copy_direction);
                items.append(copy_item);
            }

            switch (copy_direction)
            {
                case TL::OmpSs::COPY_DIR_IN:
                    {
                        target_info.append_to_copy_in(items);
                        break;
                    }
                case TL::OmpSs::COPY_DIR_OUT:
                    {
                        target_info.append_to_copy_out(items);
                        break;
                    }
                case TL::OmpSs::COPY_DIR_INOUT:
                    {
                        target_info.append_to_copy_inout(items);
                        break;
                    }
                default:
                    {
                        internal_error("Unreachable code", 0);
                    }
            }
        }

        // If the current target_ctx contains information about the
        // 'implements' clause, we should try to add the current outlined task
        // as a valid implementation of another task.
        //
        // Note that we do that in the second part of this function, once all
        // the sanity checks have been perfomed.
        void Core::ompss_handle_implements_clause(
                const OmpSs::TargetContext& target_ctx,
                Symbol function_sym,
                const locus_t* locus)
        {
            // If the 'implements' clause was not present, skip this code
            if (!target_ctx.has_implements)
                return;

            // If the 'implements' clause was defined over a symbol that is not
            // a function, skip it
            if (!function_sym.is_function())
            {
                warn_printf_at(locus, "'#pragma omp target' with an 'implements' clause must "
                        "precede a single function declaration or a function definition\n");
                warn_printf_at(locus, "skipping the whole '#pragma omp target'\n");
                return;
            }

            // If the 'implements' clause refers to a function symbol that is
            // not an outlined task, skip it
            if (!_function_task_set->is_function_task(target_ctx.implements))
            {
                warn_printf_at(locus, "invalid argument in the 'implements' clause: "
                        "'%s' is not an outlined task, skipping\n",
                        target_ctx.implements.get_qualified_name().c_str());
                return;
            }

            // At this point we can guarantee that the current use of the
            // 'implements' clause is valid

            OmpSs::TargetInfo &target_info =
                _function_task_set->get_function_task(target_ctx.implements).get_target_info();

            OmpSs::TargetInfo::implementation_table_t implementation_table = target_info.get_implementation_table();
            for (ObjectList<std::string>::const_iterator it = target_ctx.device_list.begin();
                    it != target_ctx.device_list.end();
                    it++)
            {
                std::string device(*it);
                OmpSs::TargetInfo::implementation_table_t::iterator it2 = implementation_table.find(device);
                // If the current device hasn't an entry in the map
                if (it2 == implementation_table.end()
                        // Or it has but the current symbol is not in the list
                        ||  !it2->second.contains(function_sym))
                {
                    info_printf_at(locus, "adding function '%s' as the implementation of '%s' for device '%s'\n",
                            function_sym.get_qualified_name().c_str(),
                            target_ctx.implements.get_qualified_name().c_str(),
                            device.c_str());

                    target_info.add_implementation(device, function_sym);
                }
            }
        }

        // This function is invoked only for inline tasks (and some other
        // constructs though target info is unused for them)
        void Core::ompss_get_target_info(TL::PragmaCustomLine construct,
                DataEnvironment& data_sharing_environment)
        {
            if (_target_context.empty())
                return;

            TL::OmpSs::TargetInfo target_info;

            TL::Symbol enclosing_function = Nodecl::Utils::get_enclosing_function(construct);
            ERROR_CONDITION(!enclosing_function.is_valid(), "This symbol is not valid", 0);
            target_info.set_target_symbol(enclosing_function);
            OmpSs::TargetContext& target_ctx = _target_context.top();

            add_copy_items(construct, data_sharing_environment,
                    target_ctx.copy_in,
                    TL::OmpSs::COPY_DIR_IN,
                    target_info,
                    in_ompss_mode());

            add_copy_items(construct, data_sharing_environment,
                    target_ctx.copy_out,
                    TL::OmpSs::COPY_DIR_OUT,
                    target_info,
                    in_ompss_mode());

            add_copy_items(construct, data_sharing_environment,
                    target_ctx.copy_inout,
                    TL::OmpSs::COPY_DIR_INOUT,
                    target_info,
                    in_ompss_mode());

            target_info.set_file(target_ctx.file);
            target_info.set_name(target_ctx.name);
            target_info.append_to_ndrange(target_ctx.ndrange);
            target_info.append_to_shmem(target_ctx.shmem);
            target_info.append_to_onto(target_ctx.onto);
            target_info.append_to_device_list(target_ctx.device_list);

            // Set data sharings for referenced entities in copies
            if (target_ctx.copy_deps == OmpSs::TargetContext::COPY_DEPS)
            {
                // Copy the dependences, as well

                ObjectList<DependencyItem> dependences;
                data_sharing_environment.get_all_dependences(dependences);

                ObjectList<Nodecl::NodeclBase> dep_list_in;
                ObjectList<Nodecl::NodeclBase> dep_list_out;
                ObjectList<Nodecl::NodeclBase> dep_list_inout;
                ObjectList<Nodecl::NodeclBase> dep_list_weakin;
                ObjectList<Nodecl::NodeclBase> dep_list_weakout;
                ObjectList<Nodecl::NodeclBase> dep_list_weakinout;
                ObjectList<Nodecl::NodeclBase> dep_list_reductions;
                for (ObjectList<DependencyItem>::iterator it = dependences.begin();
                        it != dependences.end();
                        it++)
                {
                    ObjectList<Nodecl::NodeclBase>* p = NULL;
                    switch (it->get_kind())
                    {
                        case DEP_DIR_IN:
                        case DEP_OMPSS_DIR_IN_PRIVATE:
                            {
                                p = &dep_list_in;
                                break;
                            }
                        case DEP_DIR_OUT:
                            {
                                p = &dep_list_out;
                                break;
                            }
                        case DEP_DIR_INOUT:
                            // OmpSs
                        case DEP_OMPSS_CONCURRENT:
                        case DEP_OMPSS_COMMUTATIVE:
                        case DEP_OMPSS_REDUCTION:
                            {
                                p = &dep_list_inout;
                                break;
                            }
                        case DEP_OMPSS_WEAK_IN:
                            {
                                p = &dep_list_weakin;
                                break;
                            }
                        case DEP_OMPSS_WEAK_OUT:
                            {
                                p = &dep_list_weakout;
                                break;
                            }
                        case DEP_OMPSS_WEAK_INOUT:
                            {
                                p = &dep_list_weakinout;
                                break;
                            }
                        default:
                            {
                                internal_error("Invalid dependency kind", 0);
                            }
                    }

                    p->append(it->get_dependency_expression());
                }

                add_copy_items(construct, data_sharing_environment,
                        dep_list_in,
                        TL::OmpSs::COPY_DIR_IN,
                        target_info,
                        in_ompss_mode());

                add_copy_items(construct, data_sharing_environment,
                        dep_list_out,
                        TL::OmpSs::COPY_DIR_OUT,
                        target_info,
                        in_ompss_mode());

                add_copy_items(construct, data_sharing_environment,
                        dep_list_inout,
                        TL::OmpSs::COPY_DIR_INOUT,
                        target_info,
                        in_ompss_mode());

                if (!dep_list_weakin.empty()
                        || !dep_list_weakout.empty()
                        || !dep_list_weakinout.empty())
                {
                    warn_printf_at(construct.get_locus(),
                            "weak dependences are not considered yet for copy_deps\n");
                }
            }

            if (this->in_ompss_mode()
                    && (target_ctx.copy_deps == OmpSs::TargetContext::COPY_DEPS
                        || !target_ctx.copy_in.empty()
                        || !target_ctx.copy_out.empty()
                        || !target_ctx.copy_inout.empty())
                    && !_allow_shared_without_copies)
            {
                ObjectList<TL::OmpSs::CopyItem> all_copies;
                all_copies.append(target_info.get_copy_in());
                all_copies.append(target_info.get_copy_out());
                all_copies.append(target_info.get_copy_inout());

                ObjectList<Symbol> all_copied_syms = all_copies
                    .map<DataReference>(&TL::OmpSs::CopyItem::get_copy_expression)
                    .map<TL::Symbol>(&DataReference::get_base_symbol);

                // In devices with disjoint memory, it may be wrong to use a
                // global variables inside a pragma task without copying it.
                ObjectList<Symbol> ds_syms;
                data_sharing_environment.get_all_symbols(DS_SHARED, ds_syms);

                for(ObjectList<Symbol>::iterator io_it = ds_syms.begin(); 
                        io_it != ds_syms.end(); 
                        io_it++)
                {
                    // Ignore 'this'
                    if (IS_CXX_LANGUAGE
                            && io_it->get_name() == "this")
                    {
                        continue;
                    }

                    if (!all_copied_syms.contains(*io_it))
                    {
                        warn_printf_at(
                                construct.get_locus(),
                                "symbol '%s' has shared data-sharing but does not have"
                                " copy directionality. This may cause problems at run-time\n",
                                io_it->get_qualified_name().c_str());
                    }
                }
            }

            // Store the target information in the current data sharing
            data_sharing_environment.set_target_info(target_info);
        }
    }
}
