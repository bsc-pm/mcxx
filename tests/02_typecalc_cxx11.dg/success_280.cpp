/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
template < typename T>
void foo() {

    auto f = [](T t) {
        __PRETTY_FUNCTION__;
    };

    f();
}
