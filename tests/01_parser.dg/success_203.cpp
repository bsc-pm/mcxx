/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct C
{
    C(const C&);
    C(int);
};

template <typename T>
void f(T& x, int& y)
{
  ( typename T::iterator(x) );
  ( typename T::iterator(y) );
  ( C(x) );
  ( C(y) );
}
