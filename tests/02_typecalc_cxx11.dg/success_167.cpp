/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/
template <typename ...T> struct A { A(T... t) { } };
int main()
{
        A<int, float, double> a(1, 2.0f, 3.0);
}
