/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_CXXFLAGS=-Werror
test_compile_fail=yes
</testinfo>
*/
struct C { int x; };
int main() {
C c [[deprecated]];
c.x;
}
