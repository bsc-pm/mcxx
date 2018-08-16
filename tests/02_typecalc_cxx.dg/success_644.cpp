/*
<testinfo>
test_generator="config/mercurium"
test_nolink=yes
</testinfo>
*/
template < typename T>
struct A;


void foo(A<int>& var);
void foo(A<double>& var);

void bar(A<double> & v)
{
    foo(v);
}
