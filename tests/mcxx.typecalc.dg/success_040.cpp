typedef int T1 __attribute__((vector_size(16)));
typedef short int T2 __attribute__((vector_size(16)));

void g(T1 t1);
void g(T2 t1);

void f()
{
   T1 t1;
   T2 t2;

   t1 + t1;
   t1 = t1;

   t1 = t2;
   t2 = t1;

   t2 += t2;

   g(t1);
   g(t2);
}
