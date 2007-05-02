#ifndef TL_PARAMETERINFO_HPP
#define TL_PARAMETERINFO_HPP

#include "tl-type.hpp"
#include "tl-langconstruct.hpp"
#include "tl-symbol.hpp"

namespace TL
{
    class ParameterInfo
    {
        public:
            typedef enum 
            {
                UNKNOWN = 0,
                BY_VALUE,
                BY_POINTER,
                BY_REFERENCE // Unused
            } parameter_kind_t;

            std::string parameter_name;
            std::string argument_name;
            Type type;
            parameter_kind_t kind;
            IdExpression id_expression;
            Symbol symbol;

            ParameterInfo(const std::string& _parameter_name, 
                    const std::string& _argument_name, 
                    IdExpression _id_expression, 
                    Type _type, 
                    parameter_kind_t _kind)
                : parameter_name(_parameter_name), 
                argument_name(_argument_name), 
                type(_type), 
                kind(_kind), 
                id_expression(_id_expression), 
                symbol(_id_expression.get_symbol())
            {
            }
    };
}

#endif // TL_PARAMETERINFO_HPP
