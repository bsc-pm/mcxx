/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
struct A
{
    static int arr[20];
};

decltype(A::arr) A::arr;
