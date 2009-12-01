#include "tl-omp-core.hpp"
#include "tl-omp-tasks.hpp"

namespace TL
{
    namespace OpenMP
    {
        FunctionTaskParameter::FunctionTaskParameter(Symbol sym, DependencyAttribute direction)
            : _sym(sym), _direction(direction)
        {
        }

        Symbol FunctionTaskParameter::get_symbol() const
        {
            return _sym;
        }

        FunctionTaskParameter::DependencyAttribute FunctionTaskParameter::get_direction() const
        {
            return _direction;
        }

        FunctionTaskInfo::FunctionTaskInfo(Symbol sym,
                ObjectList<FunctionTaskParameter> parameter_info)
            : _sym(sym), _parameters(parameter_info)
        {
        }

        ObjectList<FunctionTaskParameter> FunctionTaskInfo::get_parameter_info() const
        {
            return _parameters;
        }

        FunctionTaskSet::FunctionTaskSet()
        {
        }

        bool FunctionTaskSet::is_function_task(Symbol sym) const
        {
            return (_map.find(sym) != _map.end());
        }

        FunctionTaskInfo& FunctionTaskSet::get_function_task(Symbol sym)
        {
            return _map.find(sym)->second;
        }

        const FunctionTaskInfo& FunctionTaskSet::get_function_task(Symbol sym) const
        {
            return _map.find(sym)->second;
        }

        bool FunctionTaskSet::add_function_task(Symbol sym, const FunctionTaskInfo& function_info)
        {
            std::pair<Symbol, FunctionTaskInfo> pair(sym, function_info);
            _map.insert(pair);
        }

        struct FunctionTaskParameterGenerator : public Functor<FunctionTaskParameter, std::string>
        {
            private:
                FunctionTaskParameter::DependencyAttribute _direction;
                AST_t _ref_tree;
                ScopeLink _sl;

            public:
                FunctionTaskParameterGenerator(FunctionTaskParameter::DependencyAttribute direction,
                        AST_t ref_tree, ScopeLink sl)
                    : _direction(direction), _ref_tree(ref_tree), _sl(sl)
                {
                }

                FunctionTaskParameter do_(std::string& str) const
                {
                    Source src(str);

                    AST_t id_expr_tree = src.parse_id_expression(_ref_tree, _sl);
                    IdExpression id_expr(id_expr_tree, _sl);

                    Symbol sym = id_expr.get_symbol();

                    if (!sym.is_valid())
                    {
                        running_error("%s: error: invalid name '%s'\n",
                                _ref_tree.get_locus().c_str(),
                                id_expr_tree.prettyprint().c_str());
                    }

                    return FunctionTaskParameter(sym, _direction);
                }
        };

        void Core::task_function_handler_pre(PragmaCustomConstruct construct)
        {
            // Generic warning so nobody gets fooled by what the compiler will do
            ObjectList<std::string> clauses = construct.get_clause_names();

            for (ObjectList<std::string>::iterator it = clauses.begin();
                    it != clauses.end();
                    it++)
            {
                if (*it != "input"
                        && *it != "output"
                        && *it != "inout")
                {
                    std::cerr << construct.get_ast().get_locus() << ": warning: clause '" << (*it) << "' will be ignored" << std::endl;
                }
            }

            PragmaCustomClause input_clause = construct.get_clause("input");
            ObjectList<std::string> input_arguments;
            if (input_clause.is_defined())
            {
                input_arguments = input_clause.get_arguments(ExpressionTokenizer());
            }

            PragmaCustomClause output_clause = construct.get_clause("output");
            ObjectList<std::string> output_arguments;
            if (output_clause.is_defined())
            {
                output_arguments = output_clause.get_arguments(ExpressionTokenizer());
            }

            PragmaCustomClause inout_clause = construct.get_clause("inout");
            ObjectList<std::string> inout_arguments;
            if (inout_clause.is_defined())
            {
                inout_arguments = inout_clause.get_arguments(ExpressionTokenizer());
            }

            // Now discover whether this is a function definition or a declaration
            DeclaredEntity decl_entity(AST_t(), construct.get_scope_link());
            if (Declaration::predicate(construct.get_declaration()))
            {
                Declaration decl(construct.get_declaration(), construct.get_scope_link());
                ObjectList<DeclaredEntity> declared_entities = decl.get_declared_entities();

                if (declared_entities.size() != 1)
                {
                    std::cerr << construct.get_ast().get_locus() 
                        << ": warning: 'task' directive applied to non suitable declaration, skipping" << std::endl;
                    return;
                }

                decl_entity = declared_entities[0];
            }
            else if (FunctionDefinition::predicate(construct.get_declaration()))
            {
                FunctionDefinition funct_def(construct.get_declaration(), construct.get_scope_link());
                decl_entity = funct_def.get_declared_entity();
            }
            else
            {
                std::cerr << construct.get_ast().get_locus() 
                        << ": warning: invalid use of 'task' directive, skipping" << std::endl;
                return;
            }

            if (!decl_entity.is_functional_declaration())
            {
                std::cerr << construct.get_ast().get_locus() 
                    << ": warning: declaration must be a function declaration, skipping" << std::endl;
                return;
            }

            bool has_ellipsis = false;
            ObjectList<ParameterDeclaration> parameter_decl = decl_entity.get_parameter_declarations(has_ellipsis);

            if (has_ellipsis)
            {
                std::cerr << construct.get_ast().get_locus() 
                    << ": warning: task directive cannot be applied to functions declarations with ellipsis, skipping" << std::endl;
                return;
            }

            if (parameter_decl.empty()
                    || (parameter_decl.size() == 1 && parameter_decl[0].get_type().is_void()))
            {
                std::cerr << construct.get_ast().get_locus()
                    << ": warning: task directive cannot be applied to functions with no parameters, skipping" << std::endl;
                return;
            }

            // Use the first parameter as a reference tree so we can parse the specifications
            AST_t param_ref_tree = parameter_decl[0].get_ast();

            ObjectList<FunctionTaskParameter> input_info = input_arguments.map(
                    FunctionTaskParameterGenerator(DependencyItem::INPUT,
                        param_ref_tree,
                        construct.get_scope_link())
                    );

            ObjectList<FunctionTaskParameter> output_info = output_arguments.map(
                    FunctionTaskParameterGenerator(DependencyItem::OUTPUT,
                        param_ref_tree,
                        construct.get_scope_link())
                    );

            ObjectList<FunctionTaskParameter> inout_info = inout_arguments.map(
                    FunctionTaskParameterGenerator(DependencyItem::INOUT,
                        param_ref_tree,
                        construct.get_scope_link())
                    );
        }
    }
}
