/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
struct hook { };
struct slot_rep { };

template<class T_functor, class T_return >
struct slot_call0
{
    static T_return call_it(slot_rep * rep)
    {
    }
    static hook address()
    {
        return reinterpret_cast<hook >(&call_it);
    }
};

