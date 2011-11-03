#include "tl-omp-base.hpp"
#include "cxx-diagnostic.h"

namespace TL { namespace OpenMP {

    bool Base::_already_registered = false;

    Base::Base()
        : PragmaCustomCompilerPhase("omp")
    {
    }

    static void register_directive(const std::string& str)
    {
        register_new_directive("omp", str.c_str(), 0, 0);
    }

    static void register_construct(const std::string& str, bool bound_to_statement = false)
    {
        if (IS_FORTRAN_LANGUAGE)
        {
            register_new_directive("omp", str.c_str(), 1, bound_to_statement);
        }
        else
        {
            register_new_directive("omp", str.c_str(), 1, 0);
        }
    }

    void Base::register_omp_constructs()
    {
#define OMP_DIRECTIVE(_directive, _name) \
        { \
            if (!_already_registered) \
            { \
                register_directive(_directive); \
            } \
            dispatcher().directive.pre[_directive].connect(functor(&Base::_name##_handler_pre, *this)); \
            dispatcher().directive.post[_directive].connect(functor(&Base::_name##_handler_post, *this)); \
        }
#define OMP_CONSTRUCT_COMMON(_directive, _name, _noend) \
        { \
            if (!_already_registered) \
            { \
                register_construct(_directive, _noend); \
            } \
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
        _already_registered = true;
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

        EMPTY_HANDLERS_CONSTRUCT(section)
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
        PragmaCustomLine pragma_line = directive.get_pragma_line();
        PragmaCustomClause on_clause = pragma_line.get_clause("on");

        TL::ObjectList<Nodecl::NodeclBase> on_list = on_clause.get_arguments_as_expressions();

        directive.replace(
                Nodecl::Parallel::FlushMemory::make(
                    Nodecl::List::make(on_list),
                    directive.get_filename(), 
                    directive.get_line())
                );
    }

    void Base::task_handler_pre(TL::PragmaCustomStatement) { }
    void Base::task_handler_post(TL::PragmaCustomStatement directive)
    {
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        Nodecl::List execution_environment = 
            compute_execution_environment(pragma_line, 
                    directive.get_statements(), 
                    /* is_task */ true);

        directive.replace(
                Nodecl::Parallel::Async::make(execution_environment,
                    directive.get_statements().copy(),
                    directive.get_filename(),
                    directive.get_line()));
    }

    void Base::parallel_handler_pre(TL::PragmaCustomStatement) { }
    void Base::parallel_handler_post(TL::PragmaCustomStatement directive)
    {
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        Nodecl::List execution_environment = 
            compute_execution_environment(pragma_line, 
                    directive.get_statements(), 
                    /* is_task */ false);

        Nodecl::NodeclBase num_replicas;
        PragmaCustomClause num_threads_clause = pragma_line.get_clause("num_threads");
        if (num_threads_clause.is_defined())
        {
            TL::ObjectList<Nodecl::NodeclBase> arg_list = num_threads_clause.get_arguments_as_expressions();
            if (arg_list.size() == 1)
            {
                num_replicas = arg_list[0];
            }
            else
            {
                // TODO : Signal error here
            }
        }

        directive.replace(
                Nodecl::Parallel::Replicate::make(execution_environment,
                    num_replicas,
                    directive.get_statements().copy(),
                    directive.get_filename(),
                    directive.get_line()));
    }

    void Base::single_handler_pre(TL::PragmaCustomStatement) { }
    void Base::single_handler_post(TL::PragmaCustomStatement directive)
    {
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        Nodecl::List execution_environment = 
            compute_execution_environment(pragma_line, 
                    directive.get_statements(), 
                    /* is_task */ false);

        directive.replace(
                Nodecl::Parallel::Serialize::make(execution_environment,
                    directive.get_statements(),
                    directive.get_filename(),
                    directive.get_line()));
    }

    void Base::for_handler_pre(TL::PragmaCustomStatement)
    {
    }

    void Base::for_handler_post(TL::PragmaCustomStatement)
    {
    }

    void Base::task_handler_pre(TL::PragmaCustomDeclaration)
    {
    }

    void Base::task_handler_post(TL::PragmaCustomDeclaration)
    {
    }

    void Base::target_handler_pre(TL::PragmaCustomStatement)
    {
    }

    void Base::target_handler_pre(TL::PragmaCustomDeclaration)
    {
    }

    void Base::target_handler_post(TL::PragmaCustomStatement)
    {
    }

    void Base::target_handler_post(TL::PragmaCustomDeclaration)
    {
    }

    void Base::sections_handler_pre(TL::PragmaCustomStatement)
    {
    }


    void Base::sections_handler_post(TL::PragmaCustomStatement)
    {
    }

    void Base::parallel_for_handler_pre(TL::PragmaCustomStatement)
    {
    }

    void Base::parallel_for_handler_post(TL::PragmaCustomStatement)
    {
    }

    void Base::threadprivate_handler_pre(TL::PragmaCustomDirective)
    {
    }

    void Base::threadprivate_handler_post(TL::PragmaCustomDirective)
    {
    }

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
} }
