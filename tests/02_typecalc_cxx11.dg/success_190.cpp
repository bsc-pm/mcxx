/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

struct A;

struct A_iterator
{
    A_iterator& operator++();
    A& operator*();
    bool operator!=(const A_iterator&);
};

struct A
{
    A_iterator begin();
    A_iterator end();
};

void f(void)
{
    A a;
    auto foo = [&]() {
        for (auto& p: a)
        {
        }
    };
    foo();
}
