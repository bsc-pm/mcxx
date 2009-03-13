namespace L
{
  template <class T>
  struct A
  {
  };
}

namespace K
{
   struct M { };
   void f(L::A<M>);
};

void g()
{
  L::A<K::M> a;
  f(a);
}
