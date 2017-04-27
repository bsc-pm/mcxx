/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
struct Node
{
    void foo()  { }
};

struct A : public Node
{
    using Node::foo;
    void foo();
};

void A::foo() { }
