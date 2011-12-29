#include "tl-lowering-visitor.hpp"
#include "tl-counters.hpp"
#include "tl-source.hpp"

namespace TL { namespace Nanox {

void LoweringVisitor::visit(const Nodecl::Parallel::WaitAsyncsShallow& construct)
{
    Counter& current_wd = CounterManager::get_counter("nanos++-taskwait");
    Counter& err_counter = CounterManager::get_counter("nanos++-err");

    Source src, wd_name, err_name;

    src << "{"
        <<     "nanos_wd_t " << wd_name << ";"
        <<     wd_name << " = nanos_current_wd();"
        <<     "nanos_err_t " << err_name << ";"
        <<     err_name << "= nanos_wg_wait_completion(" << wd_name << ", 0);"
        << "}"
        ;

    wd_name << "current_wd_" << (int)current_wd;
    current_wd++;

    err_name << "err_" << (int)err_counter;
    err_counter++;

    FORTRAN_LANGUAGE()
    {
        // Parse in C
        Source::source_language = SourceLanguage::C;
    }

    Nodecl::NodeclBase n = src.parse_statement(construct);

    FORTRAN_LANGUAGE()
    {
        Source::source_language = SourceLanguage::Current;
    }

    construct.integrate(n);
}

} }
