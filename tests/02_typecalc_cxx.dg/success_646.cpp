/*
<testinfo>
test_generator="config/mercurium"
</testinfo>
*/

struct S
{
    void foo(int x) {}
};

template <typename T, void (S::*Func)(T)>
void bar(S s, T arg)
{
    (s.*Func)(arg);
}

int main() {
    S s;
    bar<int, &S::foo>(s, 1);
}
