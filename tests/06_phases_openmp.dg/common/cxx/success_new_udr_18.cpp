/*
<testinfo>
test_generator=config/mercurium-omp
</testinfo>
*/

#pragma omp declare reduction (add: int: _out=_in+_out)

int main (int argc, char* argv[])
{
  int a = 0;
  #pragma omp parallel firstprivete(a) reuction (add: a)
  a = a + 5;
  printf("reduction done");
  return 0;
}
