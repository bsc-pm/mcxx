#ifndef TL_OMP_TASKS_HPP
#define TL_OMP_TASKS_HPP

#include "tl-object.hpp"
#include "tl-symbol.hpp"
#include "tl-omp-deps.hpp"
#include "tl-langconstruct.hpp"
#include <map>

namespace TL
{
    namespace OpenMP
    {
        class FunctionTaskDependency
        {
            private:
                DependencyDirection _direction;
                Expression _expr;
            public:
                FunctionTaskDependency(Expression expr, DependencyDirection direction);
                DependencyDirection get_direction() const; 
                Expression get_expression() const;
        };

        class FunctionTaskInfo 
        {
            private:
                Symbol _sym;
                ObjectList<FunctionTaskDependency> _parameters;
            public:
                FunctionTaskInfo(Symbol sym,
                        ObjectList<FunctionTaskDependency> parameter_info);

                ObjectList<FunctionTaskDependency> get_parameter_info() const;

                ObjectList<Symbol> get_involved_parameters() const;
        };

        class FunctionTaskSet : public TL::Object
        {
            private:
                std::map<Symbol, FunctionTaskInfo> _map;
            public:
                FunctionTaskSet();

                bool is_function_task(Symbol sym) const;

                FunctionTaskInfo& get_function_task(Symbol sym);
                const FunctionTaskInfo& get_function_task(Symbol sym) const;
                bool add_function_task(Symbol sym, const FunctionTaskInfo&);

                bool empty() const;
        };

    }
}

#endif // TL_OMP_TASKS_HPP
