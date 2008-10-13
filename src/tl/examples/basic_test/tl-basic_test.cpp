#include "tl-basic_test.hpp"
#include "tl-ast.hpp"
#include "tl-scopelink.hpp"
#include "tl-source.hpp"
#include <iostream>

namespace TL
{
    BasicTestPhase::BasicTestPhase()
    {
        std::cerr << "Basic test phase created" << std::endl;
    }

    void BasicTestPhase::pre_run(TL::DTO& dto)
    {
        std::cerr << "Basic test phase pre_run" << std::endl;

        AST_t ast = dto["translation_unit"];
        ScopeLink sl = dto["scope_link"];

        Source src;

        src << "extern void f(int n);"
            ;

        src.parse_global(ast, sl);
    }

    void BasicTestPhase::run(TL::DTO& dto)
    {
        std::cerr << "Basic test phase run" << std::endl;
    }
}

EXPORT_PHASE(TL::BasicTestPhase);
