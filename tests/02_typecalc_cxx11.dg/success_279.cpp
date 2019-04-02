/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
int main() {

    auto f = []() {
        __PRETTY_FUNCTION__;
    };

    f();
}
