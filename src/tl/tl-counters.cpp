#include "tl-counters.hpp"
#include <map>
#include <algorithm>
#include <iterator>

namespace TL
{
    std::ostream& operator<<(std::ostream& os, Counter c)
    {
        return (os << c._n);
    }

    typedef std::map<std::string, Counter*> counter_map_t;
    typedef counter_map_t* counter_map_ptr_t;

    static counter_map_ptr_t _counter_map = NULL;

    Counter& CounterManager::get_counter(const std::string& str)
    {
        if (_counter_map == NULL)
        {
            _counter_map = new counter_map_t();
        }

        counter_map_t& counter_map(*_counter_map);
        Counter* p_counter = counter_map[str];
        if (p_counter == NULL)
        {
            p_counter = counter_map[str] = new Counter();
        }

        return *p_counter;
    }

    static std::string get_first(std::pair<std::string, Counter*> p)
    {
        return p.first;
    }

    ObjectList<std::string> CounterManager::counters()
    {
        ObjectList<std::string> result;
        if (_counter_map == NULL)
            return result;

        counter_map_t& counter_map(*_counter_map);

        std::transform(counter_map.begin(), counter_map.end(), 
                std::back_inserter(result), get_first);

        return result;
    }
}
