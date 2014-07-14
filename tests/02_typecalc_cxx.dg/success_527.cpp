/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct A
{
    class iterator;
    class const_iterator
    {
        private:
            int z;
        public:
        const_iterator& operator=(const iterator& frob);
    };

    class iterator
    {
        private:
            int x;
        public:
        friend const_iterator& const_iterator::operator=(const iterator& bleh);
    };
};
