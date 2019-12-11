/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T, typename U> int apply(int a, T op1, U op2) { }

void foo();

void fn() {
  apply(10,
        [](int a) { },
        [](float a) { foo(); return; });
}

