/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

const int foo();

void f()
{
    int&& y = (const int)5;
    int&& z = foo();
}
