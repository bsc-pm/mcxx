/*
<testinfo>
test_generator=config/mercurium-omp

test_compile_fail_nanos4_plain=yes
test_compile_faulty_nano4_plain=yes
test_compile_fail_nanox_plain=yes
test_compile_faulty_nanox_plain=yes

test_compile_fail_nanox_instrument=yes
test_compile_faulty_nanox_instrument=yes
</testinfo>
*/

// fail because we need to lookup in all nested classes

class A {
  public :
    A & operator+= ( const A & );
};

class B : public A {
};

#pragma omp declare reduction( + : A : _out += _in )

int main (int argc, char* argv[])
{
  A a;
  B b;
  #pragma omp parallel reduction ( + : b )
  b;
  return 0;
}
