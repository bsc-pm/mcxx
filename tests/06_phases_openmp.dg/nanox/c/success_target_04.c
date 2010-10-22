/*
<testinfo>
test_generator=config/mercurium-nanox
</testinfo>
*/

void f(void)
{
int my_global;
int cpinout;
  #pragma omp target device(smp_numa)
  #pragma omp task shared(my_global,cpinout)
  {
    my_global;
	cpinout;
  }
}

