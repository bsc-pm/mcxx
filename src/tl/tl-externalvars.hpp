#ifndef TL_EXTERNALVARS_HPP
#define TL_EXTERNALVARS_HPP

namespace TL
{
    class ExternalVars
    {
        public:
            static std::string get(const std::string& name, const std::string& default_val = "");
    };
}

#endif // TL_EXTERNALVARS_HPP
