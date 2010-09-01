/*
<testinfo>
test_generator=config/mercurium-omp
test_noexec=yes

test_compile_fail_nanos4_plain=yes
test_compile_faulty_nanos4_plain=yes
test_compile_fail_nanox_plain=yes
test_compile_faulty_nanox_plain=yes
</testinfo>
*/

struct A
{
  int x;
};

void foo ( struct A *a,  struct A *b );

#pragma omp declare reduction ( foo : struct A : foo(&_out,&_in) )

void bar ()
{
  // illegal re-declaration
  #pragma omp declare reduction ( foo : struct A : foo(&_out,&_in) )
}
