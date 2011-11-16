#include "tl-omp-base.hpp"
#include "cxx-diagnostic.h"

namespace TL { namespace OpenMP {

    Base::Base()
        : PragmaCustomCompilerPhase("omp"), _core()
    {
#define OMP_DIRECTIVE(_directive, _name) \
                { \
                    dispatcher().directive.pre[_directive].connect(functor(&Base::_name##_handler_pre, *this)); \
                    dispatcher().directive.post[_directive].connect(functor(&Base::_name##_handler_post, *this)); \
                }
#define OMP_CONSTRUCT_COMMON(_directive, _name, _noend) \
                { \
                    dispatcher().declaration.pre[_directive].connect(functor((void (Base::*)(TL::PragmaCustomDeclaration))&Base::_name##_handler_pre, *this)); \
                    dispatcher().declaration.post[_directive].connect(functor((void (Base::*)(TL::PragmaCustomDeclaration))&Base::_name##_handler_post, *this)); \
                    dispatcher().statement.pre[_directive].connect(functor((void (Base::*)(TL::PragmaCustomStatement))&Base::_name##_handler_pre, *this)); \
                    dispatcher().statement.post[_directive].connect(functor((void (Base::*)(TL::PragmaCustomStatement))&Base::_name##_handler_post, *this)); \
                }
#define OMP_CONSTRUCT(_directive, _name) OMP_CONSTRUCT_COMMON(_directive, _name, false)
#define OMP_CONSTRUCT_NOEND(_directive, _name) OMP_CONSTRUCT_COMMON(_directive, _name, true)
#include "tl-omp-constructs.def"
#undef OMP_DIRECTIVE
#undef OMP_CONSTRUCT_COMMON
#undef OMP_CONSTRUCT
#undef OMP_CONSTRUCT_NOEND
    }

    void Base::pre_run(TL::DTO& dto)
    {
        _core.pre_run(dto);
        this->PragmaCustomCompilerPhase::pre_run(dto);
    }

    void Base::run(TL::DTO& dto)
    {
        _core.run(dto);
        this->PragmaCustomCompilerPhase::run(dto);
    }

#define INVALID_STATEMENT_HANDLER(_name) \
        void Base::_name##_handler_pre(TL::PragmaCustomStatement ctr) { \
            error_printf("%s: error: invalid '#pragma %s %s'\n",  \
                    ctr.get_locus().c_str(), \
                    ctr.get_text().c_str(), \
                    ctr.get_pragma_line().get_text().c_str()); \
        } \
        void Base::_name##_handler_post(TL::PragmaCustomStatement) { } 

#define INVALID_DECLARATION_HANDLER(_name) \
        void Base::_name##_handler_pre(TL::PragmaCustomDeclaration ctr) { \
            error_printf("%s: error: invalid '#pragma %s %s'\n",  \
                    ctr.get_locus().c_str(), \
                    ctr.get_text().c_str(), \
                    ctr.get_pragma_line().get_text().c_str()); \
        } \
        void Base::_name##_handler_post(TL::PragmaCustomDeclaration) { } 

        INVALID_DECLARATION_HANDLER(parallel)
        INVALID_DECLARATION_HANDLER(parallel_for)
        INVALID_DECLARATION_HANDLER(for)
        INVALID_DECLARATION_HANDLER(parallel_sections)
        INVALID_DECLARATION_HANDLER(section)
        INVALID_DECLARATION_HANDLER(sections)
        INVALID_DECLARATION_HANDLER(single)

#define EMPTY_HANDLERS_CONSTRUCT(_name) \
        void Base::_name##_handler_pre(TL::PragmaCustomStatement) { } \
        void Base::_name##_handler_post(TL::PragmaCustomStatement) { } \
        void Base::_name##_handler_pre(TL::PragmaCustomDeclaration) { } \
        void Base::_name##_handler_post(TL::PragmaCustomDeclaration) { } \

#define EMPTY_HANDLERS_DIRECTIVE(_name) \
        void Base::_name##_handler_pre(TL::PragmaCustomDirective) { } \
        void Base::_name##_handler_post(TL::PragmaCustomDirective) { } 

        EMPTY_HANDLERS_CONSTRUCT(atomic)
        EMPTY_HANDLERS_CONSTRUCT(master)
        EMPTY_HANDLERS_CONSTRUCT(critical)
        EMPTY_HANDLERS_CONSTRUCT(ordered)
#ifdef FORTRAN_SUPPORT
        EMPTY_HANDLERS_CONSTRUCT(parallel_do)
        EMPTY_HANDLERS_CONSTRUCT(do)
#endif

    void Base::barrier_handler_pre(TL::PragmaCustomDirective) { } 
    void Base::barrier_handler_post(TL::PragmaCustomDirective directive) 
    {
        directive.replace(
                Nodecl::Parallel::BarrierFull::make(directive.get_filename(), directive.get_line())
                );
    }

    void Base::flush_handler_pre(TL::PragmaCustomDirective) { }
    void Base::flush_handler_post(TL::PragmaCustomDirective directive) 
    {
        PragmaClauseArgList parameter = directive.get_pragma_line().get_parameter();
        directive.replace(
                Nodecl::Parallel::FlushMemory::make(
                    parameter.copy(),
                    directive.get_filename(), directive.get_line())
                );
    }

    void Base::taskwait_handler_pre(TL::PragmaCustomDirective) { }
    void Base::taskwait_handler_post(TL::PragmaCustomDirective directive)
    {
    }

    // Inline tasks
    void Base::task_handler_pre(TL::PragmaCustomStatement) { }
    void Base::task_handler_post(TL::PragmaCustomStatement directive)
    {
        OpenMP::DataSharingEnvironment ds = _core.get_openmp_info()->get_data_sharing(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        Nodecl::List execution_environment = this->make_execution_environment(ds, pragma_line);

        directive.replace(
                Nodecl::Parallel::Async::make(execution_environment, 
                    directive.get_statements().copy(),
                    directive.get_filename(),
                    directive.get_line()));
    }

    void Base::parallel_handler_pre(TL::PragmaCustomStatement) { }
    void Base::parallel_handler_post(TL::PragmaCustomStatement directive)
    {
        OpenMP::DataSharingEnvironment ds = _core.get_openmp_info()->get_data_sharing(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        Nodecl::List execution_environment = this->make_execution_environment(ds, pragma_line);

        Nodecl::NodeclBase number_of_replicas;
        PragmaCustomClause clause = pragma_line.get_clause("num_threads");
        if (clause.is_defined())
        {
            ObjectList<Nodecl::NodeclBase> args = clause.get_arguments_as_expressions();

            // Let core check this for us
            ERROR_CONDITION (args.size() != 1, "num_threads wrong clause", 0);

            number_of_replicas = args[0];
        }

        directive.replace(
                Nodecl::Parallel::Replicate::make(
                    Nodecl::NodeclBase::null(),
                    number_of_replicas,
                    Nodecl::Parallel::Async::make(
                        execution_environment,
                        directive.get_statements().copy(),
                        directive.get_filename(),
                        directive.get_line()),
                    directive.get_filename(),
                    directive.get_line()));
    }

    void Base::single_handler_pre(TL::PragmaCustomStatement) { }
    void Base::single_handler_post(TL::PragmaCustomStatement directive)
    {
        OpenMP::DataSharingEnvironment ds = _core.get_openmp_info()->get_data_sharing(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        Nodecl::List execution_environment = this->make_execution_environment(ds, pragma_line);

        directive.replace(
                Nodecl::Parallel::Serialize::make(
                    execution_environment,
                    Nodecl::Parallel::Async::make(
                        execution_environment,
                        directive.get_statements().copy(),
                        directive.get_filename(),
                        directive.get_line()),
                    directive.get_filename(),
                    directive.get_line()));
    }

    void Base::for_handler_pre(TL::PragmaCustomStatement) { }
    void Base::for_handler_post(TL::PragmaCustomStatement)
    {
        internal_error("Not yet implemented", 0);
    }

    // Function tasks
    void Base::task_handler_pre(TL::PragmaCustomDeclaration declaration) { }
    void Base::task_handler_post(TL::PragmaCustomDeclaration)
    {
        internal_error("Not yet implemented", 0);
    }

    void Base::target_handler_pre(TL::PragmaCustomStatement) { }
    void Base::target_handler_pre(TL::PragmaCustomDeclaration) { }

    void Base::target_handler_post(TL::PragmaCustomStatement) { }
    void Base::target_handler_post(TL::PragmaCustomDeclaration) { }

    void Base::sections_handler_pre(TL::PragmaCustomStatement) { }
    void Base::sections_handler_post(TL::PragmaCustomStatement directive)
    {
        OpenMP::DataSharingEnvironment ds = _core.get_openmp_info()->get_data_sharing(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        Nodecl::List execution_environment = this->make_execution_environment(ds, pragma_line);

        Nodecl::List tasks = directive.get_statements().as<Nodecl::List>();
        ObjectList<Nodecl::NodeclBase> new_tasks;

        for (Nodecl::List::iterator it = tasks.begin();
                it != tasks.end();
                it++)
        {
            // We should check that they are pragma section
            ERROR_CONDITION((!it->is<Nodecl::PragmaCustomStatement>()), "Invalid node found", 0);

            Nodecl::PragmaCustomStatement p = it->as<Nodecl::PragmaCustomStatement>();

            new_tasks.append( Nodecl::Parallel::Async::make(
                        execution_environment.copy(),
                        p.get_statements().copy(),
                        it->get_filename(),
                        it->get_line()) );
        }

        Nodecl::List set_of_tasks = Nodecl::List::make(new_tasks);

        directive.replace(
                Nodecl::Parallel::Composite::make(
                    Nodecl::NodeclBase::null(),
                    set_of_tasks,
                    directive.get_filename(),
                    directive.get_line()));
    }

    // Keep these (they are handled later in parallel_sections_handler_post or
    // sections_handler_post)
    void Base::section_handler_pre(TL::PragmaCustomStatement) { }
    void Base::section_handler_post(TL::PragmaCustomStatement directive) { }

    void Base::parallel_for_handler_pre(TL::PragmaCustomStatement)
    {
    }

    void Base::parallel_for_handler_post(TL::PragmaCustomStatement)
    {
    }

    // Keep these
    void Base::threadprivate_handler_pre(TL::PragmaCustomDirective) { }
    void Base::threadprivate_handler_post(TL::PragmaCustomDirective) { }

    // Remove
    void Base::declare_reduction_handler_pre(TL::PragmaCustomDirective) 
    { 
    }

    void Base::parallel_sections_handler_pre(TL::PragmaCustomStatement)
    {
    }

    void Base::declare_reduction_handler_post(TL::PragmaCustomDirective)
    {
    }

    void Base::parallel_sections_handler_post(TL::PragmaCustomStatement)
    {
    }

    Nodecl::List Base::make_execution_environment(OpenMP::DataSharingEnvironment, PragmaCustomLine)
    {
        return Nodecl::List();
    }
} }

EXPORT_PHASE(TL::OpenMP::Base)
