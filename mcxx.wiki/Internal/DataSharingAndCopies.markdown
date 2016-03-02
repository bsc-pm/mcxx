
 Rule of thumb

 * If the symbol of the data reference is firstprivate the copy is `NANOS_PRIVATE`
 * Otherwise `NANOS_SHARED`
 * If it appears in a dependence clause *it will always be shared*

Examples:

## Examples


        int a[10];
        void f()
        {
         int b[10];
        
         #pragma omp target copy_deps
         #pragma omp task inout(a) inout(b)
         {
           // a is NANOS_SHARED (because its data-sharing is shared)
           // b is NANOS_SHARED (because its data-sharing is shared as it appears in a dependence clause)
         }
        }


        #pragma omp task inout([10]a)
        void f(int *a);
        
        void g()
        {
          int a[10];
          f(a);
        }