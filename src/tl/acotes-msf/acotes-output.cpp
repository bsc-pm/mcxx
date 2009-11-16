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

#include <fstream>

#include "tl-langconstruct.hpp"
#include "tl-declarationclosure.hpp"
#include "tl-multifile.hpp"

#include "acotes-output.hpp"

namespace TL
{
    void AcotesOutputPhase::pre_run(DTO& dto)
    {
    }

    void AcotesOutputPhase::run(DTO& dto)
    {
        _output_tasks = RefPtr<OutputTasks>::cast_static(dto["outline_info"]);

        ObjectList<std::string> files = _output_tasks->get_files();

        for (ObjectList<std::string>::iterator it = files.begin();
                it != files.end();
                it++)
        {
            Source output_tasks_src;
            Source output_code_src;
            Source declaration_closure_src;

            output_tasks_src
                << declaration_closure_src
                << output_code_src;


            std::cerr << "Generating SPU file: '" << *it << "'" << std::endl;

            ObjectList<OutputTask> current_file_tasks = _output_tasks->get_file_tasks(*it);

            DeclarationClosure declaration_closure(current_file_tasks[0].scope_link);

            for (ObjectList<OutputTask>::iterator it2 = current_file_tasks.begin();
                    it2 != current_file_tasks.end();
                    it2++)
			{

				LangConstruct lang_construct(it2->code, it2->scope_link);
				// Code of every task
				output_code_src << it2->code.prettyprint_external() << "\n";

				// Gather all symbols for the declaration closure
				ObjectList<IdExpression> id_expresions =
					lang_construct.all_symbol_occurrences(LangConstruct::ONLY_VARIABLES);
				for (ObjectList<IdExpression>::iterator it3 = id_expresions.begin();
						it3 != id_expresions.end();
						it3++)
				{
					declaration_closure.add(it3->get_symbol());
				}

				// Gather all declared names
				ObjectList<AST_t> declared_names = 
					lang_construct.get_ast().depth_subtrees(Declaration::predicate);
				for(ObjectList<AST_t>::iterator it3 = declared_names.begin();
						it3 != declared_names.end();
						it3++)
				{
					Declaration decl(*it3, it2->scope_link);
					ObjectList<DeclaredEntity> declared_entities = decl.get_declared_entities();
					for (ObjectList<DeclaredEntity>::iterator it4 = declared_entities.begin();
							it4 != declared_entities.end();
							it4++)
					{
						declaration_closure.add(it4->get_declared_symbol());
					}
				}
			}

            // Declaration closure
            declaration_closure_src = declaration_closure.closure();

#if 1
            // Now create the file
            bool is_new_file = 0;
            if ((*it)[0] == 's') {
		         printf ("File is SPU\n");
               CompiledFile new_file = CompilationProcess::add_file(*it, "acotes-spucellcc", 
                    is_new_file);
            }
            else {
		         printf ("File is PPU\n");
               CompiledFile new_file = CompilationProcess::add_file(*it, "acotes-ppucellcc",
                    is_new_file);
            }
#endif

            // And write the source into it
            std::fstream f(it->c_str());
            if (f.good())
            {
                f << output_tasks_src.get_source() << std::endl;
                f.close();
            }
        }
    }
}

EXPORT_PHASE(TL::AcotesOutputPhase);
