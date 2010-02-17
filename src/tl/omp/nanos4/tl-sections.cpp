/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

#include "tl-omptransform.hpp"

namespace TL
{
    namespace Nanos4
    {
        void OpenMPTransform::sections_preorder(PragmaCustomConstruct sections_construct)
        {
            Statement construct_body = sections_construct.get_statement();

            // We push a new level of sections with zero "section" counted
            // so far
            num_sections_stack.push(0);
            //
            // They will hold the entities as they appear in the clauses
            ObjectList<Symbol>& shared_references = 
                sections_construct.get_data<ObjectList<Symbol> >("shared_references");
            ObjectList<Symbol>& private_references = 
                sections_construct.get_data<ObjectList<Symbol> >("private_references");
            ObjectList<Symbol>& firstprivate_references = 
                sections_construct.get_data<ObjectList<Symbol> >("firstprivate_references");
            ObjectList<Symbol>& lastprivate_references = 
                sections_construct.get_data<ObjectList<Symbol> >("lastprivate_references");
            ObjectList<OpenMP::ReductionSymbol>& reduction_references =
                sections_construct.get_data<ObjectList<OpenMP::ReductionSymbol> >("reduction_references");
            ObjectList<Symbol>& copyin_references = 
                sections_construct.get_data<ObjectList<Symbol> >("copyin_references");
            ObjectList<Symbol>& copyprivate_references = 
                sections_construct.get_data<ObjectList<Symbol> >("copyprivate_references");
            
            OpenMP::DataSharingEnvironment& data_sharing = openmp_info->get_data_sharing(sections_construct.get_ast());

            data_sharing.get_all_symbols(OpenMP::DS_SHARED, shared_references);
            data_sharing.get_all_symbols(OpenMP::DS_PRIVATE, private_references);
            data_sharing.get_all_symbols(OpenMP::DS_FIRSTPRIVATE, firstprivate_references);
            data_sharing.get_all_symbols(OpenMP::DS_LASTPRIVATE, lastprivate_references);

            // As usual reductions are special
            data_sharing.get_all_reduction_symbols(reduction_references);

            data_sharing.get_all_symbols(OpenMP::DS_COPYIN, copyin_references);
            data_sharing.get_all_symbols(OpenMP::DS_COPYPRIVATE, copyprivate_references);
        }

        void OpenMPTransform::sections_postorder(PragmaCustomConstruct sections_construct)
        {
            // Get the construct_body of the statement
            Statement construct_body = sections_construct.get_statement();

            // Get the enclosing function definition
            FunctionDefinition function_definition = sections_construct.get_enclosing_function();
            // its scope
            Scope function_scope = function_definition.get_scope();

            ObjectList<Symbol>& shared_references = 
                sections_construct.get_data<ObjectList<Symbol> >("shared_references");
            ObjectList<Symbol>& private_references = 
                sections_construct.get_data<ObjectList<Symbol> >("private_references");
            ObjectList<Symbol>& firstprivate_references = 
                sections_construct.get_data<ObjectList<Symbol> >("firstprivate_references");
            ObjectList<Symbol>& lastprivate_references = 
                sections_construct.get_data<ObjectList<Symbol> >("lastprivate_references");
            ObjectList<OpenMP::ReductionSymbol>& reduction_references =
                sections_construct.get_data<ObjectList<OpenMP::ReductionSymbol> >("reduction_references");
            ObjectList<Symbol>& copyin_references = 
                sections_construct.get_data<ObjectList<Symbol> >("copyin_references");
            ObjectList<Symbol>& copyprivate_references = 
                sections_construct.get_data<ObjectList<Symbol> >("copyprivate_references");

            ObjectList<ParameterInfo> parameter_info_list;

            ObjectList<OpenMP::ReductionSymbol> reduction_empty;

            ReplaceIdExpression replace_references = 
                set_replacements(function_definition,
                        construct_body,
                        shared_references,
                        private_references,
                        firstprivate_references,
                        lastprivate_references,
                        reduction_references,
                        reduction_empty,
                        copyin_references,
                        copyprivate_references,
                        parameter_info_list);

            int num_sections = num_sections_stack.top();

            Source loop_distribution_code;

            loop_distribution_code = get_loop_distribution_in_sections(num_sections,
                    construct_body,
                    replace_references);

            // In fact we are not passing anything by parameters since
            // there is no outline here
            parameter_info_list.clear();

            Source private_declarations = get_privatized_declarations_inline(
                    sections_construct,
                    private_references,
                    firstprivate_references,
                    lastprivate_references,
                    reduction_references,
                    copyin_references
                    ); 

            Source lastprivate_code;

            if (!lastprivate_references.empty())
            {
                Source lastprivate_assignments = get_lastprivate_assignments_inline(
                        lastprivate_references,
                        copyprivate_references);

                lastprivate_code 
                    << "if (intone_last != 0)"
                    << "{"
                    <<    lastprivate_assignments
                    << "}"
                    ;
            }

            PragmaCustomClause nowait_clause = sections_construct.get_clause("nowait");
            Source loop_finalization = get_loop_finalization(/*do_barrier=*/!(nowait_clause.is_defined()));

            Source reduction_code;

            bool orphaned = (parallel_nesting == 0);
            if (orphaned)
            {
                reduction_code = get_critical_reduction_code(reduction_references);
            }
            else
            {
                reduction_code = get_noncritical_inlined_reduction_code(reduction_references, construct_body);
            }

            Source sections_source;
            sections_source 
                << "{"
                <<    private_declarations
                <<    loop_distribution_code
                <<    lastprivate_code
                <<    reduction_code
                <<    loop_finalization
                << "}"
                ;

            num_sections_stack.pop();

            AST_t sections_tree = sections_source.parse_statement(sections_construct.get_ast(),
                    sections_construct.get_scope_link());

            sections_construct.get_ast().replace(sections_tree);
        }
    }
}
