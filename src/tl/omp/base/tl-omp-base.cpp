#include "tl-omp-base.hpp"
#include "tl-nodecl-alg.hpp"
#include "cxx-diagnostic.h"

namespace TL { namespace OpenMP {

    template <typename T,
             typename List>
    static void make_dependency_list(
            List& dependences,
            DependencyDirection kind,
            const std::string& filename, int line,
            ObjectList<Nodecl::NodeclBase>& result_list)
    {
        TL::ObjectList<Nodecl::NodeclBase> data_ref_list;
        for (typename List::iterator it = dependences.begin();
                it != dependences.end();
                it++)
        {
            if (it->get_kind() != kind)
                continue;

            data_ref_list.append(it->get_dependency_expression());
        }

        if (!data_ref_list.empty())
        {
            result_list.append(T::make(Nodecl::List::make(data_ref_list), filename, line));
        }
    }

    class FunctionCallVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            RefPtr<FunctionTaskSet> _function_task_set;
        public:
            FunctionCallVisitor(RefPtr<FunctionTaskSet> function_task_set)
                : _function_task_set(function_task_set)
            {
            }

            virtual void visit(const Nodecl::FunctionCall& call)
            {
                Nodecl::NodeclBase called = call.get_called();

                if (called.is<Nodecl::Symbol>() 
                        && _function_task_set->is_function_task(called.as<Nodecl::Symbol>().get_symbol()))
                {
                    // Nodecl::NodeclBase exec_env = compute_
                    FunctionTaskInfo& task_info = _function_task_set->get_function_task(called.as<Nodecl::Symbol>().get_symbol());

                    Nodecl::NodeclBase exec_env = this->make_exec_environment(call, task_info);

                    Nodecl::Parallel::AsyncCall async_call = Nodecl::Parallel::AsyncCall::make(
                            exec_env,
                            // Do we need to copy this?
                            call.copy(),
                            call.get_filename(),
                            call.get_line());

                    call.replace(async_call);
                }
            }

        private:
            Nodecl::NodeclBase make_exec_environment(const Nodecl::FunctionCall &call, FunctionTaskInfo& function_task_info)
            {
                TL::ObjectList<Nodecl::NodeclBase> result_list;

                TL::ObjectList<FunctionTaskDependency> task_dependences = function_task_info.get_parameter_info();

                make_dependency_list<Nodecl::Parallel::DepIn>(
                        task_dependences, 
                        OpenMP::DEP_DIR_IN,
                        call.get_filename(), 
                        call.get_line(),
                        result_list);

                make_dependency_list<Nodecl::Parallel::DepOut>(
                        task_dependences, 
                        OpenMP::DEP_DIR_OUT,
                        call.get_filename(), 
                        call.get_line(),
                        result_list);

                make_dependency_list<Nodecl::Parallel::DepInout>(
                        task_dependences, 
                        OpenMP::DEP_DIR_INOUT,
                        call.get_filename(), 
                        call.get_line(),
                        result_list);

                return Nodecl::List::make(result_list);
            }
    };

    Base::Base()
        : PragmaCustomCompilerPhase("omp"), _core()
    {
        set_phase_name("OpenMP directive to parallel IR");
        set_phase_description("This phase lowers the semantics of OpenMP into the parallel IR of Mercurium");

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

        RefPtr<FunctionTaskSet> function_task_set = RefPtr<FunctionTaskSet>::cast_static(dto["openmp_task_info"]);

        Nodecl::NodeclBase translation_unit = dto["nodecl"];

        FunctionCallVisitor function_call_visitor(function_task_set);
        function_call_visitor.walk(translation_unit);
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
        EMPTY_HANDLERS_CONSTRUCT(parallel_do)
        EMPTY_HANDLERS_CONSTRUCT(do)

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

        if (on_clause.is_defined())
        {
            directive.replace(
                    Nodecl::Parallel::WaitAsyncsDependences::make(
                        Nodecl::List::make(on_clause.get_arguments_as_expressions()),
                        directive.get_filename(), directive.get_line())
                    );
        }
        else
        {
            directive.replace(
                    Nodecl::Parallel::WaitAsyncsShallow::make(
                        directive.get_filename(), 
                        directive.get_line())
                    );
        }
    }

    // Inline tasks
    void Base::task_handler_pre(TL::PragmaCustomStatement) { }
    void Base::task_handler_post(TL::PragmaCustomStatement directive)
    {
        OpenMP::DataSharingEnvironment &ds = _core.get_openmp_info()->get_data_sharing(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        Nodecl::List execution_environment = this->make_execution_environment(ds, pragma_line);

        PragmaCustomClause untied = pragma_line.get_clause("untied");
        if (untied.is_defined())
        {
            execution_environment.push_back(
                    Nodecl::Parallel::Untied::make(
                        directive.get_filename(),
                        directive.get_line()));
        }

        PragmaCustomClause priority = pragma_line.get_clause("priority");
        if (priority.is_defined())
        {
            TL::ObjectList<Nodecl::NodeclBase> expr_list = priority.get_arguments_as_expressions(directive);

            if (expr_list.size() != 1)
            {
                warn_printf("%s: warning: ignoring invalid 'priority' clause in 'task' construct\n",
                        directive.get_locus().c_str());
            }
            else
            {
                execution_environment.push_back(
                        Nodecl::Parallel::Priority::make(
                            expr_list[0],
                            directive.get_filename(),
                            directive.get_line()));
            }
        }

        PragmaCustomClause if_clause = pragma_line.get_clause("if");

        Nodecl::NodeclBase async_code = 
                    Nodecl::Parallel::Async::make(execution_environment, 
                        directive.get_statements().copy(),
                        directive.get_filename(),
                        directive.get_line());

        // Honour if-clause
        if (if_clause.is_defined())
        {
            TL::ObjectList<Nodecl::NodeclBase> expr_list = if_clause.get_arguments_as_expressions(directive);
            if (expr_list.size() != 1)
            {
                warn_printf("%s: warning: ignoring invalid 'if' clause in 'task' construct\n",
                        directive.get_locus().c_str());
            }
            else
            {
                async_code = Nodecl::IfElseStatement::make(
                        expr_list[0],
                        Nodecl::List::make(async_code),
                        directive.get_statements().copy(),
                        directive.get_filename(),
                        directive.get_line());
            }
        }
        directive.replace(async_code);
    }

    void Base::parallel_handler_pre(TL::PragmaCustomStatement) { }
    void Base::parallel_handler_post(TL::PragmaCustomStatement directive)
    {
        OpenMP::DataSharingEnvironment &ds = _core.get_openmp_info()->get_data_sharing(directive);
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
    void Base::task_handler_post(TL::PragmaCustomDeclaration decl)
    {
        Nodecl::Utils::remove_from_enclosing_list(decl);
    }

    void Base::target_handler_pre(TL::PragmaCustomStatement) { }
    void Base::target_handler_pre(TL::PragmaCustomDeclaration) { }

    void Base::target_handler_post(TL::PragmaCustomStatement) { }
    void Base::target_handler_post(TL::PragmaCustomDeclaration) { }

    void Base::sections_handler_pre(TL::PragmaCustomStatement) { }
    void Base::sections_handler_post(TL::PragmaCustomStatement directive)
    {
        OpenMP::DataSharingEnvironment &ds = _core.get_openmp_info()->get_data_sharing(directive);
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

    struct SymbolBuilder : TL::Functor<Nodecl::NodeclBase, Symbol>
    {
        private:
            std::string _filename;
            int _line;

        public:
            SymbolBuilder(const std::string& filename, int line)
                : _filename(filename), _line(line)
            {
            }

            virtual Nodecl::NodeclBase do_(ArgType arg) const
            {
                return Nodecl::Symbol::make(arg, _filename, _line);
            }
    };

    template <typename T>
    static void make_data_sharing_list(
            OpenMP::DataSharingEnvironment &data_sharing_env,
            OpenMP::DataSharingAttribute data_attr,
            const std::string& filename, int line,
            ObjectList<Nodecl::NodeclBase>& result_list)
    {
        TL::ObjectList<Symbol> symbols;
        data_sharing_env.get_all_symbols(data_attr, symbols);

        if (!symbols.empty())
        {
            TL::ObjectList<Nodecl::NodeclBase> nodecl_symbols = symbols.map(SymbolBuilder(filename, line));

            result_list.append(T::make(Nodecl::List::make(nodecl_symbols), filename, line));
        }
    }

    Nodecl::List Base::make_execution_environment(OpenMP::DataSharingEnvironment &data_sharing_env, PragmaCustomLine pragma_line)
    {
        TL::ObjectList<Nodecl::NodeclBase> result_list;

        make_data_sharing_list<Nodecl::Parallel::Shared>(
                data_sharing_env, OpenMP::DS_SHARED, 
                pragma_line.get_filename(), pragma_line.get_line(),
                result_list);
        make_data_sharing_list<Nodecl::Parallel::Private>(
                data_sharing_env, OpenMP::DS_PRIVATE, 
                pragma_line.get_filename(), pragma_line.get_line(),
                result_list);
        make_data_sharing_list<Nodecl::Parallel::Capture>(
                data_sharing_env, OpenMP::DS_FIRSTPRIVATE, 
                pragma_line.get_filename(), pragma_line.get_line(),
                result_list);

        TL::ObjectList<OpenMP::DependencyItem> dependences;
        data_sharing_env.get_all_dependences(dependences);

        make_dependency_list<Nodecl::Parallel::DepIn>(
                dependences, 
                OpenMP::DEP_DIR_IN,
                pragma_line.get_filename(), pragma_line.get_line(),
                result_list);

        make_dependency_list<Nodecl::Parallel::DepOut>(
                dependences, 
                OpenMP::DEP_DIR_OUT,
                pragma_line.get_filename(), pragma_line.get_line(),
                result_list);

        make_dependency_list<Nodecl::Parallel::DepInout>(
                dependences, OpenMP::DEP_DIR_INOUT,
                pragma_line.get_filename(), pragma_line.get_line(),
                result_list);

        return Nodecl::List::make(result_list);
    }
    } }

EXPORT_PHASE(TL::OpenMP::Base)
