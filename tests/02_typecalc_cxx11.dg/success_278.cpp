/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
template < typename T >
void foo() {
    static_assert((T().x+1) == 3, "");
}
