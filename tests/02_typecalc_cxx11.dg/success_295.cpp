/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template < typename T >
struct  B
{
  B(T*);
};
template < typename T >
struct  A : B<T*>
{
  public:
    A(T*);
    typedef B<T*> Base;
    using Base::Base;
};

void foo()
{
  int x;
  A<int> a1(&x);

  int *p;
  A<int> a2(&p);
}
