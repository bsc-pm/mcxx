/*
<testinfo>
test_generator=config/mercurium-omp
</testinfo>
*/
#include <stdlib.h>

template < typename T>
void foo()
{
  typename T::P beta = 42, copy(beta);
#pragma omp task
  {
    beta++;
  }

#pragma omp taskwait

  if (copy != beta)
      abort();
}

struct A
{
    int x;

    A() : x(0) { }
    A(int n) : x(n) { }
    A(const A& a) : x(a.x) { }

    bool operator==(A& a) const
    {
        return this->x == a.x;
    }

    bool operator !=(A& a) const
    {
        return !this->operator==(a);
    }

    A& operator++(int)
    {
        this->x++;
        return *this;
    }
};

struct B
{
    typedef int P;
};

struct C
{
    typedef A P;
};

int main(int argc, char *argv[])
{
    foo<B>();
    foo<C>();
}
