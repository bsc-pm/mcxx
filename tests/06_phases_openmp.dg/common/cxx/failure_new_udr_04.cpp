/*
<testinfo>
test_generator=config/mercurium-omp

test_compile_fail_nanos4_plain=yes

test_compile_fail_nanox_plain=yes
test_compile_faulty_nanox_plain=yes

test_compile_fail_nanox_instrument=yes
test_compile_faulty_nanox_instrument=yes
</testinfo>
*/


class A {
  private:
     int x;
};

#pragma omp declare reduction ( + : A : _out.x += _in.x )

void main (int argc, char* argv[])
{
   A a;

   // fail
   #pragma omp parallel reduction( + : a )
   return 0;
}
