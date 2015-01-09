/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename ...T>
struct A
{
};

A<float, double> *b;
A<float, double> *c;

void g()
{
    b = c;
}
