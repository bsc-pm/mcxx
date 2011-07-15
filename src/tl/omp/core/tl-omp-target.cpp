/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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
        void Core::target_handler_pre(PragmaCustomConstruct ctr)
        {
            PragmaCustomClause device = ctr.get_clause("device");

            if (!device.is_defined())
            {
                std::cerr << ctr.get_ast().get_locus() << ": warning: '#pragma omp target' needs a 'device' clause" << std::endl;
            }

            TargetContext target_ctx;

            target_ctx.device_list = device.get_arguments(ExpressionTokenizerTrim());

            PragmaCustomClause copy_in = ctr.get_clause("copy_in");
            if (copy_in.is_defined())
            {
                target_ctx.copy_in = copy_in.get_arguments(ExpressionTokenizer());
            }

            PragmaCustomClause copy_out = ctr.get_clause("copy_out");
            if (copy_out.is_defined())
            {
                target_ctx.copy_out = copy_out.get_arguments(ExpressionTokenizer());
            }

            PragmaCustomClause copy_inout = ctr.get_clause("copy_inout");
            if (copy_inout.is_defined())
            {
                target_ctx.copy_inout = copy_inout.get_arguments(ExpressionTokenizer());
            }

            PragmaCustomClause copy_deps = ctr.get_clause("copy_deps");
            if (copy_deps.is_defined())
            {
                target_ctx.copy_deps = true;
            }

            PragmaCustomClause implements = ctr.get_clause("implements");
            if (implements.is_defined())
            {
                ObjectList<Expression> implements_list = implements.get_expression_list();

                if (implements_list.size() != 1)
                {
                    std::cerr << ctr.get_ast().get_locus() << ": warning: clause 'implements' expects one identifier, skipping" << std::endl;
                }
                else
                {
                    Expression implements_name = implements_list[0];

                    bool valid = false;

                    Symbol sym (NULL);
                    if (implements_name.is_id_expression())
                    {
                        IdExpression id_expr = implements_name.get_id_expression();
                        sym = id_expr.get_computed_symbol();

                        if (sym.is_valid()
                                && sym.is_function())
                            valid = true;
                    }

                    if (!valid)
                    {
                        std::cerr << ctr.get_ast().get_locus() << ": warning: '" 
                            << implements_name.prettyprint() 
                            << "' is not a valid identifier, skipping" 
                            << std::endl;
                    }
                    else
                    {
                        target_ctx.has_implements = true;
                        target_ctx.implements = sym;
                    }
                }
            }

            if (target_ctx.has_implements)
            {
                // We need to check this #pragma omp target precedes a function-decl or function-def
                DeclaredEntity decl_entity(AST_t(), ctr.get_scope_link());
                bool valid_target = true;

                AST_t current_decl = ctr.get_declaration();
                // Advance pragmae task or target
                while (current_decl.is_valid()
                        && (is_pragma_custom_construct("omp", "task", current_decl, ctr.get_scope_link())
                            || is_pragma_custom_construct("omp", "target", current_decl, ctr.get_scope_link())))
                {
                    PragmaCustomConstruct current_pragma(current_decl, ctr.get_scope_link());
                    current_decl = current_pragma.get_declaration();
                }

                if (Declaration::predicate(current_decl))
                {
                    Declaration decl(current_decl, ctr.get_scope_link());
                    ObjectList<DeclaredEntity> declared_entities = decl.get_declared_entities();

                    if (declared_entities.size() != 1)
                    {
                        valid_target = false;
                    }
                    else
                    {
                        decl_entity = declared_entities[0];
                    }
                }
                else if (FunctionDefinition::predicate(current_decl))
                {
                    FunctionDefinition funct_def(current_decl, ctr.get_scope_link());
                    decl_entity = funct_def.get_declared_entity();
                }
                else
                {
                    valid_target = false;
                }

                if (valid_target
                        && !decl_entity.is_functional_declaration())
                {
                    valid_target = false;
                }

                if (!valid_target)
                {
                    std::cerr << ctr.get_ast().get_locus() 
                        << ": warning: '#pragma omp target' with an 'implements' clause must "
                        "precede a single function declaration or a function definition"
                        << std::endl;
                    std::cerr << ctr.get_ast().get_locus() << ": warning: skipping the whole '#pragma omp target'" << std::endl;
                    return;
                }

                Symbol function_sym = decl_entity.get_declared_symbol();

                // Now lookup a FunctionTaskInfo
                if (!_function_task_set->is_function_task(target_ctx.implements))
                {
                    std::cerr << ctr.get_ast().get_locus() << ": warning: '" 
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
                        if (!devices_with_impl.contains(std::make_pair(*it, function_sym)))
                        {
                            std::cerr << ctr.get_ast().get_locus() << 
                                ": note: adding function '" << function_sym.get_qualified_name() << "'"
                                << " as the implementation of '" << target_ctx.implements.get_qualified_name() << "'"
                                << " for device '" << *it << "'" << std::endl;
                            function_task_info.add_device_with_implementation(*it, function_sym);
                        }
                    }
                }
            }
            else
            {
                // Check it precedes a function declaration/definition or a task 
                DeclaredEntity decl_entity(AST_t(), ctr.get_scope_link());
                bool valid_target = false;
                if (Declaration::predicate(ctr.get_declaration()))
                {
                    Declaration decl(ctr.get_declaration(), ctr.get_scope_link());
                    ObjectList<DeclaredEntity> declared_entities = decl.get_declared_entities();

                    valid_target = true;
                    // if (declared_entities.size() == 1
                    //         && declared_entities[0].is_functional_declaration())
                    // {
                    //     valid_target = true;
                    // }
                }
                else if (FunctionDefinition::predicate(ctr.get_declaration()))
                {
                    valid_target = true;
                }
                // FIXME - Two cases one for "declaration scope" task
                else if (is_pragma_custom_construct("omp", "task", ctr.get_declaration(), ctr.get_scope_link()))
                {
                    valid_target = true;
                }
                // FIXME - and another one for statement scope task
                else if (is_pragma_custom_construct("omp", "task", ctr.get_statement().get_ast(), ctr.get_scope_link()))
                {
                    valid_target = true;
                }
                // FIXME - Verbatim should have its own LangConstruct
                else if (ctr.get_declaration().internal_ast_type_() == AST_VERBATIM)
                {
                    valid_target = true;
                }

                if (!valid_target)
                {
                    std::cerr << ctr.get_ast().get_locus() 
                        << ": warning: '#pragma omp target' must "
                        "precede a function declaration, a function definition or a '#pragma omp task'"
                        << std::endl;
                    std::cerr << ctr.get_ast().get_locus() << ": warning: skipping the whole '#pragma omp target'" << std::endl;
                    return;
                }
            }

            _target_context.push(target_ctx);
        }

        void Core::target_handler_post(PragmaCustomConstruct ctr)
        {
            // It might be empty due to early exits in the preorder routine
            if (!_target_context.empty())
            {
                _target_context.pop();
            }
        }

        static void add_copy_items(PragmaCustomConstruct construct, 
                DataSharingEnvironment& data_sharing,
                const ObjectList<std::string>& list,
                CopyDirection copy_direction)
        {
            for (ObjectList<std::string>::const_iterator it = list.begin();
                    it != list.end();
                    it++)
            {
                Source src;
                src 
                    << "#line " << construct.get_ast().get_line() << " \"" << construct.get_ast().get_file() << "\"\n"
                    << *it;

                AST_t ast = src.parse_expression(construct.get_ast(),
                        construct.get_scope_link());

                DataReference expr(ast, construct.get_scope_link());

                std::string warning;
                if (!expr.is_valid(warning))
                {
                    std::cerr << warning;
                    std::cerr << construct.get_ast().get_locus() 
                        << ": warning: '" << expr.prettyprint() << "' is not a valid copy data-reference, skipping" 
                        << std::endl;
                    continue;
                }

                Symbol sym = expr.get_base_symbol();
                OpenMP::DataSharingAttribute data_sharing_attr = data_sharing.get_data_sharing(sym);

                if (expr.is_id_expression())
                {
                    if (data_sharing_attr == DS_UNDEFINED)
                    {
                        std::cerr 
                            << construct.get_ast().get_locus()
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
                                    construct.get_ast().get_locus().c_str(),
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
                    if (sym_type.is_reference())
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
                data_sharing.add_copy(copy_item);
            }
        }


		// XXX - Fixme, maybe is not necessary ask about 
        void Core::get_target_info(PragmaCustomConstruct construct, DataSharingEnvironment& data_sharing)
        {
            if (_target_context.empty())
                return;

            TargetContext& target_ctx = _target_context.top();

            add_copy_items(construct, data_sharing,
                    target_ctx.copy_in,
                    COPY_DIR_IN);
            add_copy_items(construct, data_sharing,
                    target_ctx.copy_out,
                    COPY_DIR_OUT);
            add_copy_items(construct, data_sharing,
                    target_ctx.copy_inout,
                    COPY_DIR_INOUT);

            for (ObjectList<std::string>::iterator it = target_ctx.device_list.begin();
                    it != target_ctx.device_list.end();
                    it++)
            {
                data_sharing.add_device(*it);
            }

            // Set data sharings for referenced entities in copies
            if (target_ctx.copy_deps)
            {
                // Copy the dependences, as well

                ObjectList<DependencyItem> dependences;
                data_sharing.get_all_dependences(dependences);

                ObjectList<std::string> dep_list_in;
                ObjectList<std::string> dep_list_out;
                ObjectList<std::string> dep_list_inout;
                for (ObjectList<DependencyItem>::iterator it = dependences.begin();
                        it != dependences.end();
                        it++)
                {
                    ObjectList<std::string>* p = NULL;
                    DependencyDirection dir = DependencyDirection(it->get_kind() & DEP_DIR_INOUT);
                    if (dir == DEP_DIR_INPUT)
                    {
                        p = &dep_list_in;
                    }
                    else if (dir == DEP_DIR_OUTPUT)
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

                add_copy_items(construct, 
                        data_sharing,
                        dep_list_in,
                        COPY_DIR_IN);
                add_copy_items(construct, 
                        data_sharing,
                        dep_list_out,
                        COPY_DIR_OUT);
                add_copy_items(construct, 
                        data_sharing,
                        dep_list_inout,
                        COPY_DIR_INOUT);
            }

            ObjectList<CopyItem> all_copies;
            data_sharing.get_all_copies(all_copies);

            ObjectList<Symbol> all_copied_syms = all_copies
                .map(functor(&CopyItem::get_copy_expression))
                .map(functor(&DataReference::get_base_symbol));

            // In devices with disjoint memory, it is forbidden to use a global
            // variables inside a pragma task without copying it
            // If there is no copy defined by the user, we will assume the
            // variable is shared and then we will copy_inout it
			ObjectList<Symbol> ds_syms;
			data_sharing.get_all_symbols(DS_SHARED, ds_syms);

            ObjectList<std::string> shared_to_inout;
			for(ObjectList<Symbol>::iterator io_it = ds_syms.begin(); 
					io_it != ds_syms.end(); 
					io_it++)
			{
				if (!all_copied_syms.contains(*io_it))
				{
					if (construct.get_show_warnings())
					{
		                std::cerr << construct.get_ast().get_locus() 
		                    << ": warning: symbol '" << io_it->get_qualified_name(construct.get_scope())
		                    << "' does not have copy directionality. Assuming copy_inout. "
		                    << std::endl;
					}
					shared_to_inout.append(io_it->get_qualified_name(construct.get_scope()));
				}
			}
		    add_copy_items(construct, data_sharing,
		            shared_to_inout,
		            COPY_DIR_INOUT);
        }
    }
}
