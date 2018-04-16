/*
<testinfo>
test_generator="config/mercurium"
</testinfo>
*/
template <typename T>
const T& foo(T&) { }

double bar(int i, int j) {}

typedef double (&Ret)(int, int);

Ret quux()
{
    return foo(bar);
}
