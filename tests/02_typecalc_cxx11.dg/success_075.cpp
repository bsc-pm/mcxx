/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

void fun1(int& i) = delete;

template <typename T>
void fun2(T& t) = delete;

template <typename T>
void fun3(T& t);

template <>
void fun3(int&)  = delete;

void h()
{
    // int x;
    // fun1(x);
    // fun2(x);
    // fun3(x);

    float y;
    // fun2(y);
    fun3(y);
}
