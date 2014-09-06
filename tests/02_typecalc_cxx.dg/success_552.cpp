/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T, typename S>
void f(T, S);

template <typename T>
void f(T, T);

void g()
{
    f(1, 2);
}
