#ifndef TL_OMP_TASKS_HPP
#define TL_OMP_TASKS_HPP

#include "tl-object.hpp"
#include "tl-symbol.hpp"
#include "tl-omp.hpp"
#include <map>

namespace TL
{
    namespace OpenMP
    {
        class FunctionTaskParameter 
        {
            public:
                typedef DependencyItem::DependencyAttribute DependencyAttribute;
            private:
                DependencyAttribute _direction;
                Symbol _sym;
            public:
                FunctionTaskParameter(Symbol param_sym, DependencyAttribute direction);
                DependencyAttribute get_direction() const; 
                Symbol get_symbol() const;
        };

        class FunctionTaskInfo 
        {
            private:
                Symbol _sym;
                ObjectList<FunctionTaskParameter> _parameters;
            public:
                FunctionTaskInfo(Symbol sym,
                        ObjectList<FunctionTaskParameter> parameter_info);

                ObjectList<FunctionTaskParameter> get_parameter_info() const;
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
        };

    }
}

#endif // TL_OMP_TASKS_HPP
