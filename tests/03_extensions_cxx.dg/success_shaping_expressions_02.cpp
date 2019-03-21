/*
<testinfo>
test_generator="config/mercurium-extensions c++11"
</testinfo>
*/
int main() {
    const int N = 10;
    int v[N];
    int *p = v;
    static_assert(sizeof([N]p) == sizeof(v), "The sizeofs are not equal!!");
}
