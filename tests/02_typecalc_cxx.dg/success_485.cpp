/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A
{
    template <typename F>
        A& add(const char*, F f);
};

template <typename T>
void Quux();

void g()
{
    A<int>()
        .add("myFoo", Quux<float>)
        .add("myBar", Quux<double>);
}
