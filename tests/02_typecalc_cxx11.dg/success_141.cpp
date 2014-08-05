/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename T>
void f(const T &t)
{
    for (auto&& x : t)
    {
    }
}

template <typename T>
struct my_vector
{
};

template <typename T>
void f(my_vector<int>& t)
{
    for (T&& x : t)
    {
    }
}
