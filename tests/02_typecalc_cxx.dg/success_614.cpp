/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template < typename T >
struct my_iterator
{
    typedef int value_type;
};

template <typename V>
class iterator_base : public my_iterator<V> {
 public:
  typedef typename iterator_base::value_type value_type;

};

void foo() {
    iterator_base<int> a;
}
