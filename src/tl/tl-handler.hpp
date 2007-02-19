#ifndef TL_HANDLER_HPP
#define TL_HANDLER_HPP

#include <vector>

#include "tl-functor.hpp"

/*
 * Very simplified signal/slot library
 */
namespace TL
{

// Signal 1 parameters
template <class Param1>
class Signal1
{
    private:
        typedef typename std::vector<Functor<void, Param1>* > handlers_type;
        typedef typename handlers_type::iterator handlers_iterator;

        handlers_type _handlers;
    public:
        template <class T>
        void connect(const T& handler)
        {
            T* new_handler = new T(handler);

            _handlers.push_back(new_handler);
        }

        void signal(Param1 p1)
        {
            handlers_iterator it;
            for (it = _handlers.begin(); it != _handlers.end(); it++)
            {
                (*it)->operator()(p1);
            }
        }

        ~Signal1()
        {
        }
};

}

#endif // TL_HANDLER_HPP
