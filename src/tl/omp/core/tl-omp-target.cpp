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




#include "tl-omp-target.hpp"
#include "tl-omp-core.hpp"
#include "cxx-diagnostic.h"

namespace TL
{
    namespace OpenMP
    {
        void Core::common_target_handler_pre(TL::PragmaCustomLine pragma_line,
                TargetContext& target_ctx,
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
                target_ctx.device_list.insert(device.get_tokenized_arguments());
            }
            else
            {
                // In #pragma omp target a device is mandatory, for #pragma omp task
                // add it only if not empty
                std::string default_device = "smp";
                bool set_default_device = false;
                if (!is_pragma_task)
                {
                    warn_printf("%s: warning: '#pragma omp target' without 'device' clause. Assuming 'device(smp)'\n",
                            pragma_line.get_locus_str().c_str());
                    set_default_device = true;
                }
                else if (target_ctx.device_list.empty())
                {
                    set_default_device = true;
                    //If onto is defined and there is no device, default device is MPI
                    if (onto.is_defined()) default_device="mpi";
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
                target_ctx.copy_in = copy_in.get_arguments_as_expressions(scope);
            }

            PragmaCustomClause copy_out = pragma_line.get_clause("copy_out");
            if (copy_out.is_defined())
            {
                target_ctx.copy_out = copy_out.get_arguments_as_expressions(scope);
            }

            PragmaCustomClause copy_inout = pragma_line.get_clause("copy_inout");
            if (copy_inout.is_defined())
            {
                target_ctx.copy_inout = copy_inout.get_arguments_as_expressions(scope);
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
                    warn_printf("%s: warning: 'shmem' clause cannot be used without the 'ndrange' clause, skipping\n",
                            pragma_line.get_locus_str().c_str());
                }
            }

            PragmaCustomClause file = pragma_line.get_clause("file");
            if (file.is_defined())
            {
                ObjectList<std::string> file_list = file.get_tokenized_arguments();
                if (file_list.size() != 1)
                {
                    warn_printf("%s: warning: clause 'file' expects one identifier, skipping\n",
                            pragma_line.get_locus_str().c_str());
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
                    warn_printf("%s: warning: clause 'name' expects one identifier, skipping\n",
                            pragma_line.get_locus_str().c_str());
                }
                else
                {
                    target_ctx.name = name_list[0];
                }
            }

            PragmaCustomClause copy_deps = pragma_line.get_clause("copy_deps");
            PragmaCustomClause no_copy_deps = pragma_line.get_clause("no_copy_deps");

            target_ctx.copy_deps = false;

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
                        target_ctx.copy_deps = true;

                        if (!_already_informed_new_ompss_copy_deps)
                        {
                            info_printf("%s: info: unless 'no_copy_deps' is specified, "
                                    "the default in OmpSs is now 'copy_deps'\n",
                                    pragma_line.get_locus_str().c_str());
                            info_printf("%s: info: this diagnostic is only shown for the "
                                    "first task found\n",
                                    pragma_line.get_locus_str().c_str());

                            _already_informed_new_ompss_copy_deps = true;
                        }
                    }
                }
            }
            else if (copy_deps.is_defined())
            {
                target_ctx.copy_deps = true;
            }
            else if (no_copy_deps.is_defined())
            {
                target_ctx.copy_deps = false;
            }
            else
            {
                internal_error("Code unreachable", 0);
            }


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
                    decl_context_t decl_context = scope.get_decl_context();
                    TL::Symbol current_procedure = scope.get_related_symbol();
                    decl_context.current_scope->contained_in = current_procedure.get_internal_symbol()->decl_context.current_scope;

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
                    warn_printf("%s: warning: the argument of the clause 'implements' is not a valid identifier, skipping\n",
                            pragma_line.get_locus_str().c_str());
                }
            }
        }

        // #pragma omp target on top of a #pragma omp task outline
        void Core::target_handler_pre(TL::PragmaCustomDeclaration ctr)
        {
            PragmaCustomLine pragma_line = ctr.get_pragma_line();
            TargetContext target_ctx;

            common_target_handler_pre(pragma_line, target_ctx,
                    ctr.get_context_of_parameters().retrieve_context(),
                    /* is_pragma_task */ false);

            if (target_ctx.has_implements)
            {
                Symbol function_sym = ctr.get_symbol();

                if (!function_sym.is_function())
                {
                    warn_printf("%s: warning: '#pragma omp target' with an 'implements' clause must "
                        "precede a single function declaration or a function definition\n",
                        ctr.get_locus_str().c_str());
                    warn_printf("%s: warning: skipping the whole '#pragma omp target'\n",
                        ctr.get_locus_str().c_str());
                    return;
                }

                // Now lookup a FunctionTaskInfo
                if (!_function_task_set->is_function_task(target_ctx.implements))
                {
                    warn_printf("%s: warning: '%s' is not a '#pragma omp task' function, skipping\n",
                            ctr.get_locus_str().c_str(),
                            target_ctx.implements.get_qualified_name().c_str());
                }
                else
                {
                    FunctionTaskInfo& function_task_info = _function_task_set->get_function_task(target_ctx.implements);
                    ObjectList<FunctionTaskInfo::implementation_pair_t> devices_with_impl = 
                        function_task_info.get_devices_with_implementation();

                    for (ObjectList<std::string>::iterator it = target_ctx.device_list.begin();
                            it != target_ctx.device_list.end();
                            it++)
                    {
                        const char* current_device_lowercase = strtolower(it->c_str());
                        if (!devices_with_impl.contains(std::make_pair(current_device_lowercase, function_sym)))
                        {
                            info_printf("%s: note: adding function '%s' as the implementation of '%s' for device '%s'\n",
                                    ctr.get_locus_str().c_str(),
                                    function_sym.get_qualified_name().c_str(),
                                    target_ctx.implements.get_qualified_name().c_str(),
                                    current_device_lowercase);
                            function_task_info.add_device_with_implementation(current_device_lowercase, function_sym);
                        }
                    }
                }
            }

            _target_context.push(target_ctx);
        }

        void Core::target_handler_post(TL::PragmaCustomDeclaration)
        {
            // It might be empty due to early exits in the preorder routine
            if (!_target_context.empty())
            {
                _target_context.pop();
            }
        }

        // #pragma omp target on top of a #pragma omp task inline
        void Core::target_handler_pre(TL::PragmaCustomStatement ctr)
        {
            Nodecl::NodeclBase nested_pragma = ctr.get_statements();
            if (!nested_pragma.is_null()
                    && nested_pragma.is<Nodecl::List>())
            {
                nested_pragma = nested_pragma.as<Nodecl::List>().front();
            }

            if (nested_pragma.is_null()
                    || !PragmaUtils::is_pragma_construct("omp", "task", nested_pragma))
            {
                warn_printf("%s: warning: '#pragma omp target' must precede a '#pragma omp task' in this context\n",
                        ctr.get_locus_str().c_str());
                warn_printf("%s: warning: skipping the whole '#pragma omp target'\n",
                        ctr.get_locus_str().c_str());
                return;
            }

            PragmaCustomLine pragma_line = ctr.get_pragma_line();
            TargetContext target_ctx;

            if (target_ctx.has_implements)
            {
                warn_printf("%s: warning: '#pragma omp target' cannot have an 'implements' clause in this context\n",
                        ctr.get_locus_str().c_str());
                warn_printf("%s: warning: skipping the whole '#pragma omp target'\n",
                        ctr.get_locus_str().c_str());
                return;
            }

            common_target_handler_pre(pragma_line,
                    target_ctx,
                    ctr.retrieve_context(),
                    /* is_pragma_task */ false);

            _target_context.push(target_ctx);
        }

        void Core::target_handler_post(TL::PragmaCustomStatement)
        {
            // It might be empty due to early exits in the preorder routine
            if (!_target_context.empty())
            {
                _target_context.pop();
            }
        }

        static void add_copy_items(PragmaCustomLine construct, 
                DataSharingEnvironment& data_sharing,
                const ObjectList<Nodecl::NodeclBase>& list,
                CopyDirection copy_direction,
                TargetInfo& target_info)
        {
            TL::ObjectList<CopyItem> items;

            for (ObjectList<Nodecl::NodeclBase>::const_iterator it = list.begin();
                    it != list.end();
                    it++)
            {
                DataReference expr(*it);

                std::string warning;
                if (!expr.is_valid())
                {
                    // FIXME - Make this more consistent
                    warn_printf("%s", expr.get_error_log().c_str());
                    warn_printf("%s: error: '%s' is not a valid copy data-reference, skipping\n",
                            construct.get_locus_str().c_str(),
                            expr.prettyprint().c_str());
                    continue;
                }

                Symbol sym = expr.get_base_symbol();
                OpenMP::DataSharingAttribute data_sharing_attr = data_sharing.get_data_sharing(sym);

                if (expr.is<Nodecl::Symbol>())
                {
                    if (data_sharing_attr == DS_UNDEFINED)
                    {
                        warn_printf("%s: warning: symbol '%s' does not have any data sharing, assuming 'shared'\n",
                                construct.get_locus_str().c_str(),
                                sym.get_name().c_str());
                        // Make it shared if we know nothing about this entity
                        data_sharing.set_data_sharing(sym, DS_SHARED,
                                "specified in copy_in/copy_out/copy_inout but no data-sharing was defined for it");
                    }

                    if ((data_sharing_attr & DS_PRIVATE) == DS_PRIVATE)
                    {
                        if ((data_sharing_attr & DS_IMPLICIT) != DS_IMPLICIT)
                        {
                            // This is an explicit data sharing of a private
                            // entity, which is being copied, this is wrong
                            running_error("%s: error: invalid non-shared data-sharing for copied entity '%s'\n",
                                    construct.get_locus_str().c_str(),
                                    sym.get_name().c_str());
                        }
                        else
                        {
                            // Otherwise just override the sharing attribute with shared
                            data_sharing.set_data_sharing(sym, (OpenMP::DataSharingAttribute)(DS_SHARED | DS_IMPLICIT),
                                    "entity was privatized but it appears in copy_in/copy_out/copy_inout, "
                                    "so it has been coerced to shared");
                        }
                    }
                }
                else
                {
                    Type sym_type = sym.get_type();
                    if (sym_type.is_any_reference())
                    {
                        sym_type = sym_type.references_to();
                    }

                    if (sym_type.is_array())
                    {
                        data_sharing.set_data_sharing(sym, (DataSharingAttribute)(DS_SHARED | DS_IMPLICIT),
                                "it is an array mentioned in a non-trivial way in a copy_in/copy_out/copy_inout clause");
                    }
                    else
                    {
                        data_sharing.set_data_sharing(sym, (DataSharingAttribute)(DS_FIRSTPRIVATE | DS_IMPLICIT),
                                "it is an object mentioned in a non-trivial way in a copy_in/copy_out/copy_inout clause");
                    }
                }

                CopyItem copy_item(expr, copy_direction);
                items.append(copy_item);
            }

            switch (copy_direction)
            {
                case COPY_DIR_IN:
                    {
                        target_info.append_to_copy_in(items);
                        break;
                    }
                case COPY_DIR_OUT:
                    {
                        target_info.append_to_copy_out(items);
                        break;
                    }
                case COPY_DIR_INOUT:
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

        // This function is invoked only for inline tasks (and some other
        // constructs though target info is unused for them)
        void Core::get_target_info(TL::PragmaCustomLine construct, DataSharingEnvironment& data_sharing)
        {
            if (_target_context.empty())
                return;

            TargetInfo target_info;
            target_info.set_target_symbol(construct.get_symbol());
            TargetContext& target_ctx = _target_context.top();

            add_copy_items(construct, data_sharing,
                    target_ctx.copy_in,
                    COPY_DIR_IN,
                    target_info);

            add_copy_items(construct, data_sharing,
                    target_ctx.copy_out,
                    COPY_DIR_OUT,
                    target_info);

            add_copy_items(construct, data_sharing,
                    target_ctx.copy_inout,
                    COPY_DIR_INOUT,
                    target_info);

            target_info.set_file(target_ctx.file);
            target_info.set_name(target_ctx.name);
            target_info.append_to_ndrange(target_ctx.ndrange);
            target_info.append_to_shmem(target_ctx.shmem);
            target_info.append_to_onto(target_ctx.onto);
            target_info.append_to_device_list(target_ctx.device_list);

            // Set data sharings for referenced entities in copies
            if (target_ctx.copy_deps)
            {
                // Copy the dependences, as well

                ObjectList<DependencyItem> dependences;
                data_sharing.get_all_dependences(dependences);

                ObjectList<Nodecl::NodeclBase> dep_list_in;
                ObjectList<Nodecl::NodeclBase> dep_list_out;
                ObjectList<Nodecl::NodeclBase> dep_list_inout;
                for (ObjectList<DependencyItem>::iterator it = dependences.begin();
                        it != dependences.end();
                        it++)
                {
                    ObjectList<Nodecl::NodeclBase>* p = NULL;
                    switch (it->get_kind())
                    {
                        case DEP_DIR_IN:
                        case DEP_DIR_IN_PRIVATE:
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
                        case DEP_CONCURRENT:
                        case DEP_COMMUTATIVE:
                            {
                                p = &dep_list_inout;
                                break;
                            }
                        default:
                            {
                                internal_error("Invalid dependency kind", 0);
                            }
                    }

                    p->append(it->get_dependency_expression());
                }

                add_copy_items(construct, data_sharing,
                        dep_list_in,
                        COPY_DIR_IN,
                        target_info);

                add_copy_items(construct, data_sharing,
                        dep_list_out,
                        COPY_DIR_OUT,
                        target_info);

                add_copy_items(construct, data_sharing,
                        dep_list_inout,
                        COPY_DIR_INOUT,
                        target_info);
            }

            if (this->in_ompss_mode()
                    && (target_ctx.copy_deps
                        || !target_ctx.copy_in.empty()
                        || !target_ctx.copy_out.empty()
                        || !target_ctx.copy_inout.empty())
                    && !_allow_shared_without_copies)
            {
                ObjectList<CopyItem> all_copies;
                all_copies.append(target_info.get_copy_in());
                all_copies.append(target_info.get_copy_out());
                all_copies.append(target_info.get_copy_inout());

                ObjectList<Symbol> all_copied_syms = all_copies
                    .map(functor(&CopyItem::get_copy_expression))
                    .map(functor(&DataReference::get_base_symbol));

                // In devices with disjoint memory, it may be wrong to use a
                // global variables inside a pragma task without copying it.
                ObjectList<Symbol> ds_syms;
                data_sharing.get_all_symbols(DS_SHARED, ds_syms);

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
                        warn_printf("%s: warning: symbol '%s' has shared data-sharing but does not have"
                                " copy directionality. This may cause problems at run-time\n",
                                construct.get_locus_str().c_str(),
                                io_it->get_qualified_name().c_str());

                        Nodecl::Symbol new_symbol_ref =
                            Nodecl::Symbol::make(*io_it, construct.get_locus());
                        new_symbol_ref.set_type(io_it->get_type().no_ref().get_lvalue_reference_to());
                    }
                }
            }

            // Store the target information in the current data sharing
            data_sharing.set_target_info(target_info);
        }
    }
}
