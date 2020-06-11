/*
<testinfo>
test_generator="config/mercurium-c11"
</testinfo>
*/

// This is not actually a C11 test but using _Generic here is convenient

#define istype(x, type) _Generic(x, type: 1, default: 0)
#define checktype(x, type) do { int c[istype(x, type) ? 1 : -1]; } while(0)

void foo(void)
{
  __typeof__(0 ? (void*)0 : (double*)0) m1;
  checktype(m1, double*);

  __typeof__(0 ? (void*)0 : (double*)1) m2;
  checktype(m2, double*);

  __typeof__(0 ? (void*)1 : (double*)0) m3;
  checktype(m3, void*);

  __typeof__(0 ? (void*)1 : (double*)1) m4;
  checktype(m4, void*);
}

void bar(void)
{
  __typeof__(0 ? (void*)!1 : (double*)!1) m1;
  checktype(m1, double*);

  __typeof__(0 ? (void*)!1 : (double*)!0) m2;
  checktype(m2, double*);

  __typeof__(0 ? (void*)!0 : (double*)!1) m3;
  checktype(m3, void*);

  __typeof__(0 ? (void*)!0 : (double*)!0) m4;
  checktype(m4, void*);
}
