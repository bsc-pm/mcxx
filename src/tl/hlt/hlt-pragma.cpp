#include "hlt-pragma.hpp"
#include "hlt-exception.hpp"

using namespace TL::HLT;

HLTPragmaPhase::HLTPragmaPhase()
    : PragmaCustomCompilerPhase("hlt")
{
    set_phase_name("High Level Transformations");
    set_phase_description("This phase implement several high level "
            "transformations available through the usage of #pragma hlt");

    register_construct("unroll");
    on_directive_post["unroll"].connect(functor(&HLTPragmaPhase::unroll_loop, *this));
}

void HLTPragmaPhase::run(TL::DTO& dto)
{
    try
    {
        PragmaCustomCompilerPhase::run(dto);
    }
    catch (HLTException e)
    {
        std::cerr << e << std::endl;
        set_phase_status(PHASE_STATUS_ERROR);
    }
    catch (...)
    {
        std::cerr << "(hlt-phase): error: unknown exception" << std::endl;
        set_phase_status(PHASE_STATUS_ERROR);
    }
}

void HLTPragmaPhase::unroll_loop(PragmaCustomConstruct construct)
{
    Statement statement = construct.get_statement();

    AST_t statement_ast = statement.get_ast();
    if (!ForStatement::predicate(statement_ast))
    {
        throw HLTException(construct, "'#pragma hlt unroll' can only be used with for statements");
    }
}
