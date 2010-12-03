/*
<testinfo>
test_generator=config/mercurium-omp

test_compile_fail_nanox_plain=yes
test_compile_faulty_nanox_plain=yes

test_compile_fail_nanox_instrument=yes
test_compile_faulty_nanox_instrument=yes
</testinfo>
*/

namespace n
{
  struct A {};
  #pragma omp declare reduction (foo: A: _out=_in)
}

int main (int argc, char* argv[])
{
  n::A a;
  #pragma omp parallel reduction (foo: a)
  a;

  return 0;
}
