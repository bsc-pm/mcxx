#include <list>

struct A
{
    virtual void foo() = 0;
};

template < class _T>
struct B : public A
{
     void foo()
     {
     }
};

template < class _T>
struct D
{
    std::list<A*> _list;
    void bar()
    {
        std::list<A*>::iterator p = _list.begin();
        while(p != _list.end())
            (*p++)->foo();
    }

};

int main(int, char**)
{
    D<int> d;
    d.bar();

    return 0;
}
