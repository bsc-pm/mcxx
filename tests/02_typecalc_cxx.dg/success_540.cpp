/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct True
{
    char x[2];
};

struct False
{
    char x;
};

template <typename Orig, typename Dest>
struct is_convertible {

    static False dispatch(...);
    static True dispatch(Dest);
    static Orig& trigger();

    enum { value = sizeof(dispatch(trigger())) == sizeof(True) };
};

struct A
{

};

struct B
{
    B();
    B(const A& );
};

template <bool> struct Test;
template <> struct Test<true> { typedef int Check; };

Test<is_convertible<A, B>::value > :: Check c;
