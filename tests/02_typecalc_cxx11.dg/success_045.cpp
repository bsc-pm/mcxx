/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T>
void f(T&&);

void g(int &i)
{
    f(i);
}
