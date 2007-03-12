#ifndef TL_INSTRUMENTFILTER_HPP
#define TL_INSTRUMENTFILTER_HPP

#include <set>
#include <string>

namespace TL
{
    class InstrumentFilterFile 
    {
        private:
            bool _filter_inverted;
            std::set<std::string> _filter_set;
        public:
            InstrumentFilterFile();
            bool match(const std::string& function_name);
    };
}

#endif // TL_INSTRUMENTFILTER_HPP
