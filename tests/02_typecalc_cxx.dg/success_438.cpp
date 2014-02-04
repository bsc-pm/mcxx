/*
<testinfo>
test_generator="config/mercurium"
</testinfo>
*/
template <typename T>
struct S { };

void foo()
{
   struct S<int> a;
}

template <int Q>
void bar()
{
   struct S<float> a;
}
