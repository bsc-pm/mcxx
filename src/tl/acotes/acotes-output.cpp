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
				output_code_src << it2->code.prettyprint_external();

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

            // Now create the file
            bool is_new_file = 0;
            CompiledFile new_file = CompilationProcess::add_file(*it, "spucellcc", 
                    is_new_file);

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
