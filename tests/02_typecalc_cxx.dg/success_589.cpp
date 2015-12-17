/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
struct Node
{
    int x;
};

struct Map
{
    struct iterator { int x; };
    struct const_iterator { int x; };

    iterator find(const Node * const &);
    const_iterator find(const Node * const &) const;
};

void g1(Map &m, Node *n)
{
    Map::iterator it = m.find(n);
}

void g2(const Map &m, Node *n)
{
    Map::const_iterator it = m.find(n);
}
