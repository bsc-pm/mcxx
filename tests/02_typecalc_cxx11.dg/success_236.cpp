/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/
template<typename... _Elements>
void bar(_Elements&... __args)
{}

bool foo(const int& a1) {
    bar(a1);
}
