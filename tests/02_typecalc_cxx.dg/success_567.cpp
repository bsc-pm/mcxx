/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct my_ostream
{
};

namespace nest {
struct Time;
}

my_ostream& operator<<(my_ostream&, const nest::Time&);

namespace nest {
struct Time
{
    friend my_ostream& (::operator<<)(my_ostream&, const Time&);
};

void foo()
{
    Time t;
    my_ostream msg;
    msg << t;
}
}
