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

namespace TL
{
    namespace OpenMP
    {
        void Core::common_target_handler_pre(TL::PragmaCustomLine pragma_line,
                TargetContext& target_ctx,
                TL::Scope scope,
                bool is_pragma_task)
        {
            PragmaCustomClause device = pragma_line.get_clause("device");
            if (device.is_defined())
            {
                target_ctx.device_list = device.get_tokenized_arguments();
            }
            else
            {
                // In #pragma omp target a device is mandatory, for #pragma omp task
                // add it only if not empty
                bool set_smp_device = false;
                if (!is_pragma_task)
                {
                    std::cerr << pragma_line.get_locus_str() << ": "
                        << "warning: '#pragma omp target' without 'device' clause. Assuming 'device(smp)'"
                        << std::endl;
                    set_smp_device = true;
                }
                else if (target_ctx.device_list.empty())
                {
                    set_smp_device = true;
                }

                if (set_smp_device)
                {
                    target_ctx.device_list.clear();
                    target_ctx.device_list.append("smp");
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
                    std::cerr << pragma_line.get_locus_str()
                        << ": warning: 'shmem' clause cannot be used without the 'ndrange' clause, skipping" << std::endl;
                }
            }

            PragmaCustomClause onto = pragma_line.get_clause("onto");
            if (onto.is_defined())
            {
                target_ctx.onto = onto.get_arguments_as_expressions(scope);
            }

            PragmaCustomClause file = pragma_line.get_clause("file");
            if (file.is_defined())
            {
                ObjectList<std::string> file_list = file.get_tokenized_arguments();
                if (file_list.size() != 1)
                {
                    std::cerr << pragma_line.get_locus_str() << ": warning: clause 'file' expects one identifier, skipping" << std::endl;
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
                    std::cerr << pragma_line.get_locus_str() << ": warning: clause 'name' expects one identifier, skipping" << std::endl;
                }
                else
                {
                    target_ctx.name = name_list[0];
                }
            }

            PragmaCustomClause copy_deps = pragma_line.get_clause("copy_deps");
            PragmaCustomClause no_copy_deps = pragma_line.get_clause("no_copy_deps");

            if (this->in_ompss_mode())
            {
                if (!copy_deps.is_defined()
                        && !no_copy_deps.is_defined())
                {
                    target_ctx.copy_deps = true;

                    // Warn only if copy_in, copy_inout and copy_out are unspecified
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
            }
            else
            {
                target_ctx.copy_deps = false;
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
                    std::cerr << pragma_line.get_locus_str() << ": warning: '"
                        << "' The argument of the clause 'implements' is not a valid identifier, skipping"
                        << std::endl;
                }
            }
        }

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
                    std::cerr << ctr.get_locus_str() 
                        << ": warning: '#pragma omp target' with an 'implements' clause must "
                        "precede a single function declaration or a function definition"
                        << std::endl;
                    std::cerr << ctr.get_locus_str() << ": warning: skipping the whole '#pragma omp target'" << std::endl;
                    return;
                }

                // Now lookup a FunctionTaskInfo
                if (!_function_task_set->is_function_task(target_ctx.implements))
                {
                    std::cerr << ctr.get_locus_str() << ": warning: '" 
                        << target_ctx.implements.get_qualified_name()
                        << "' is not a '#pragma omp task' function, skipping"
                        << std::endl;
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
                            std::cerr << ctr.get_locus_str() <<
                                ": note: adding function '" << function_sym.get_qualified_name() << "'"
                                << " as the implementation of '" << target_ctx.implements.get_qualified_name() << "'"
                                << " for device '" << current_device_lowercase << "'" << std::endl;

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
                std::cerr << ctr.get_locus_str()
                    << ": warning: '#pragma omp target' must precede a '#pragma omp task' in this context" << std::endl;
                std::cerr << ctr.get_locus_str() << ": warning: skipping the whole '#pragma omp target'" << std::endl;
                return;
            }

            PragmaCustomLine pragma_line = ctr.get_pragma_line();
            TargetContext target_ctx;

            if (target_ctx.has_implements)
            {
                std::cerr << ctr.get_locus_str()
                    << ": warning: '#pragma omp target' cannot have 'implements' clause in this context" << std::endl;
                std::cerr << ctr.get_locus_str() << ": warning: skipping the whole '#pragma omp target'" << std::endl;
                return;
            }

            common_target_handler_pre(pragma_line, target_ctx, ctr.retrieve_context(),
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
                    std::cerr << expr.get_error_log();
                    std::cerr << construct.get_locus_str() 
                        << ": error: '" << expr.prettyprint() << "' is not a valid copy data-reference, skipping" 
                        << std::endl;
                    continue;
                }

                Symbol sym = expr.get_base_symbol();
                OpenMP::DataSharingAttribute data_sharing_attr = data_sharing.get_data_sharing(sym);

                if (expr.is<Nodecl::Symbol>())
                {
                    if (data_sharing_attr == DS_UNDEFINED)
                    {
                        std::cerr 
                            << construct.get_locus_str()
                            << ": warning: symbol '" << sym.get_name() << "' does not have any data sharing, assuming SHARED" 
                            << std::endl;
                        // Make it shared if we know nothing about this entity
                        data_sharing.set_data_sharing(sym, DS_SHARED);
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
                            data_sharing.set_data_sharing(sym, (OpenMP::DataSharingAttribute)(DS_SHARED | DS_IMPLICIT));
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
                        data_sharing.set_data_sharing(sym, (DataSharingAttribute)(DS_SHARED | DS_IMPLICIT));
                    }
                    else
                    {
                        data_sharing.set_data_sharing(sym, (DataSharingAttribute)(DS_FIRSTPRIVATE | DS_IMPLICIT));
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
                    DependencyDirection dir = DependencyDirection(it->get_kind() & DEP_DIR_INOUT);
                    if (dir == DEP_DIR_IN)
                    {
                        p = &dep_list_in;
                    }
                    else if (dir == DEP_DIR_OUT)
                    {
                        p = &dep_list_out;
                    }
                    else if (dir == DEP_DIR_INOUT)
                    {
                        p = &dep_list_inout;
                    }
                    else
                    {
                        internal_error("Invalid dependency kind", 0);
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

            if (!_allow_shared_without_copies)
            {

                ObjectList<CopyItem> all_copies;
                all_copies.append(target_info.get_copy_in());
                all_copies.append(target_info.get_copy_out());
                all_copies.append(target_info.get_copy_inout());

                ObjectList<Symbol> all_copied_syms = all_copies
                    .map(functor(&CopyItem::get_copy_expression))
                    .map(functor(&DataReference::get_base_symbol));

                // In devices with disjoint memory, it is forbidden to use a global
                // variables inside a pragma task without copying it
                // If there is no copy defined by the user, we will assume the
                // variable is shared and then we will copy_inout it
                ObjectList<Symbol> ds_syms;
                data_sharing.get_all_symbols(DS_SHARED, ds_syms);

                ObjectList<Nodecl::NodeclBase> shared_to_inout;
                for(ObjectList<Symbol>::iterator io_it = ds_syms.begin(); 
                        io_it != ds_syms.end(); 
                        io_it++)
                {
                    if (!all_copied_syms.contains(*io_it))
                    {
                        // FIXME 
                        //
                        // if (construct.get_show_warnings())
                        // {
                        std::cerr << construct.get_locus_str() 
                            << ": warning: symbol '" << io_it->get_qualified_name()
                            << "' has shared data-sharing but does not have copy directionality. Assuming copy_inout. "
                            << std::endl;
                        // }

                        Nodecl::Symbol new_symbol_ref =
                            Nodecl::Symbol::make(*io_it, construct.get_locus());
                        new_symbol_ref.set_type(io_it->get_type().no_ref().get_lvalue_reference_to());
                        shared_to_inout.append(
                                new_symbol_ref
                                );
                    }
                }

                add_copy_items(construct, data_sharing,
                        shared_to_inout,
                        COPY_DIR_INOUT,
                        target_info);
            }

            // Store the target information in the current data sharing
            data_sharing.set_target_info(target_info);
        }
    }
}
