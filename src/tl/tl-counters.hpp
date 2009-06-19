#ifndef TL_COUNTERS_HPP
#define TL_COUNTERS_HPP

#include <iostream>
#include <string>
#include "tl-objectlist.hpp"

namespace TL
{
    struct Counter
    {
        private:
            int _n;

            Counter(const Counter& c)
                : _n(c._n)
            {
            }
        public:

            Counter()
                : _n(0)
            {
            }

            explicit Counter(int n) 
                : _n(n)
            {
            }

            Counter& operator=(int n)
            {
                _n = n;
                return *this;
            }

            Counter& operator+=(int n)
            {
                _n += n;
                return *this;
            }

            Counter& operator++(int)
            {
                return this->operator++();
            }

            Counter& operator++()
            {
                ++_n;
                return *this;
            }

            operator int() const
            {
                return _n;
            }

            friend std::ostream& operator<<(std::ostream& os, Counter c);
    };

    struct CounterManager
    {
        public:
            static Counter& get_counter(const std::string& str);
            static ObjectList<std::string> counters();
    };
}

#endif // TL_COUNTERS_HPP
