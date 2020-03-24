/*
<testinfo>
test_generator="config/mercurium run"
</testinfo>
*/

namespace A {
  extern int x;
};

int A::x;

extern "C" {
extern int printf(const char *format, ...);
}

int main(int argc, char *argv[])
{
  printf("%p\n", &A::x);
  return 0;
}
