void f(void);

namespace A
{
    typedef int T;
};

using namespace A;

void f(void)
{
    T t;
}
