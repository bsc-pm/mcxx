/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

void f1() noexcept
{
}

void f2() noexcept(false)
{
}

void f3() noexcept(true)
{
}

template <typename T>
void f4() noexcept(sizeof(T) >= 4)
{
}
