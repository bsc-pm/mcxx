/*
<testinfo>
test_generator="config/mercurium"
test_compile_fail=yes
</testinfo>
*/

void foo(int x, int y) {  }

void bar(int i) {
  void **vfuncs;

  vfuncs[i] = foo;
}

