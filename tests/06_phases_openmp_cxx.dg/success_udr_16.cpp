template<typename T>
struct A {
  T  t;

  void operator+(A);
};

#pragma omp declare reduction (template<typename T> +:A<T>)

void foo ( )
{
   A<int> a;

   #pragma omp parallel reduction(+:a)
   {
   }

}
