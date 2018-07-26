/*
<testinfo>
test_generator=config/mercurium-fe-only
test_compile_fail=yes
</testinfo>
*/
struct S {
    int a;
    int *p;
};

int main() {
    S var;
    var.p = new int[a];
}
