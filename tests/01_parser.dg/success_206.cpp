/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct Node
{
   void handle();
};


template <typename T>
struct SubNode : public Node
{
   using Node::handle;
};
