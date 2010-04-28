#include "tl-ss-valgrind.hpp"
#include "tl-augmented-symbol.hpp"

namespace TL
{
    SSValgrind::SSValgrind()
        : PragmaCustomCompilerPhase("css")
    {
        on_directive_post["start"].connect(functor(&SSValgrind::pragma_start, *this));
        on_directive_post["finish"].connect(functor(&SSValgrind::pragma_finish, *this));
        on_directive_post["barrier"].connect(functor(&SSValgrind::pragma_barrier, *this));
    }

    void SSValgrind::run(DTO& dto)
    {
        PragmaCustomCompilerPhase::run(dto);

        // Now look for all function calls that we know are CSS functions

        ScopeLink sl = dto["scope_link"];
        AST_t a = dto["translation_unit"];

        ObjectList<AST_t> all_function_calls = a.depth_subtrees(PredicateAttr(LANG_IS_FUNCTION_CALL));

        for (ObjectList<AST_t>::iterator it = all_function_calls.begin();
                it != all_function_calls.end();
                it++)
        {
            Expression function_call(*it, sl);

            Expression function_called_expresion = function_call.get_called_expression();
            if (!function_called_expresion.is_id_expression())
                // We do not handle indirect calls (through variables)
                continue;

            AugmentedSymbol symbol = function_called_expresion.get_id_expression().get_symbol();
            // This is a CSS task
            if (!symbol.is_task())
                continue;

            Source new_code;

            new_code
                << "{"
                << "start_task_valgrind(\"" << symbol.get_name() << "\");"
                << function_call.prettyprint()
                << "end_task_valgrind();"
                << "}"
                ;

            AST_t new_tree = new_code.parse_statement(function_call.get_ast(), function_call.get_scope_link());
            function_call.get_ast().replace(new_tree);
        }
    }

    void SSValgrind::pragma_start(PragmaCustomConstruct ctr)
    {
        Source src;
        src
            << "start_css_valgrind();"
            ;

        AST_t new_code = src.parse_statement(ctr.get_ast(), ctr.get_scope_link());
        ctr.get_ast().replace(new_code);
    }

    void SSValgrind::pragma_finish(PragmaCustomConstruct ctr)
    {
        Source src;
        src
            << "end_css_valgrind();"
            ;

        AST_t new_code = src.parse_statement(ctr.get_ast(), ctr.get_scope_link());
        ctr.get_ast().replace(new_code);
    }

    void SSValgrind::pragma_barrier(PragmaCustomConstruct ctr)
    {
        Source src;
        src
            << "barrier_css_valgrind();"
            ;

        AST_t new_code = src.parse_statement(ctr.get_ast(), ctr.get_scope_link());
        ctr.get_ast().replace(new_code);
    }
}

EXPORT_PHASE(TL::SSValgrind);
