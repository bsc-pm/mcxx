/*
<testinfo>
test_generator="config/mercurium-extensions c++11"
test_compile_fail=yes
</testinfo>
*/
int main() {
    const int N = 9;
    int v[N+1];
    int *p = v;
    static_assert(sizeof([N]p) == sizeof(v), "The sizeofs are not equal!!");
}
