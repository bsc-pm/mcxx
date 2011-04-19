#ifndef NANOX_FIND_COMMON_HPP
#define NANOX_FIND_COMMON_HPP

#include "tl-devices.hpp"

namespace TL
{
    namespace Nanox
    {
        class FindFunction : public TL::Predicate<AST_t>
        {
            private:
                ScopeLink _sl;
                std::string _func_name;

            public:
                FindFunction(ScopeLink sl, std::string func_name) : _sl(sl), _func_name(func_name){};
                virtual bool do_(const AST_t& ast) const;
        };

        class FindAttribute : public TL::Predicate<AST_t>
        {
            private:
                ScopeLink _sl;
                std::string _attr_name;

            public:
                FindAttribute(ScopeLink sl, std::string attr_name) : _sl (sl), _attr_name(attr_name) {};
                virtual bool do_(const AST_t& ast) const;
        };
    }
}

#endif //NANOX_FIND_COMMON_HPP
