#ifndef HLT_EXTENSION_HPP
#define HLT_EXTENSION_HPP

#include "hlt-transform.hpp"
#include "tl-langconstruct.hpp"

namespace TL
{
    namespace HLT
    {
        //! \addtogroup HLT High Level Transformations
        //! @{

        //! Extends a function to work on vector arguments
        class LIBHLT_CLASS FunctionExtension : public BaseTransform
        {
            private:
                FunctionDefinition _funct_def;
                Expression _extension_amount;
                Symbol _function_symbol;
                Source _extension_code;
                std::string _extended_function_name;

                void do_extension();
            public:
                virtual Source get_source();

                FunctionExtension(FunctionDefinition funct_def, 
                        Expression extension_amount);

                FunctionExtension& set_extended_function_name(const std::string name);
        };

        FunctionExtension function_extension(FunctionDefinition funct_def,
                Expression extension_amount);
    }
}

#endif // HLT_EXTENSION_HPP
