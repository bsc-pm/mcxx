/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

struct A
{
    int x;
};

struct C
{
    int k;
    A w;
} a;

C c( { 1, { 4 } } );
