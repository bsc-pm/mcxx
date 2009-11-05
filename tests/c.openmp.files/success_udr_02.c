typedef struct {
  double real;
  double imaginary;
} my_complex_t;

my_complex_t complex_add (my_complex_t a, my_complex_t b);
my_complex_t complex_sub (my_complex_t a, my_complex_t b);
my_complex_t complex_mul (my_complex_t a, my_complex_t b);

#pragma omp declare reduction(complex_add, complex_sub:my_complex_t) identity({0,0}) order(right)

#pragma omp declare reduction(complex_mul:my_complex_t) identity({1,0}) order(right)

#define N 100
my_complex_t vector[N];

void f(my_complex_t x, my_complex_t y)
{
    int i;
#pragma omp parallel for reduction(complex_add:x) reduction(complex_mul:y)
    for ( i = 0; i < N ; i++ ) 
    {
        x = complex_add(x,vector[i]);
        y = complex_mul(y,vector[i]);
    }
}
