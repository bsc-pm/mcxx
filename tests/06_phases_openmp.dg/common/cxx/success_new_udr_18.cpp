/*
<testinfo>
test_generator=config/mercurium-omp

test_compile_fail_nanox_plain=yes
test_compile_faulty_nanox_plain=yes

test_compile_fail_nanox_instrument=yes
test_compile_faulty_nanox_instrument=yes
</testinfo>
*/

#pragma omp declare reduction (add: int: _out=_in+_out)

int main (int argc, char* argv[])
{
  int a = 0;
  #pragma omp parallel firstprivate(a) reduction (add: a)
  a = a + 5;
  printf("reduction done");
  return 0;
}
