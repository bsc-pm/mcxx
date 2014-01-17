/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

int a;
struct A
{
    int m;
    A() : m(1) { }

    void f(void)
    {
        extern int b;
        static int c;
        [this] { m = 42; a = 1; b = 2; c = 3; }();
    }
};

int main(int, char**)
{
     A a;
     a.f();
}
