/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template< typename T > struct add_const { public: typedef T const type; };
template< typename T > struct add_const<T&> { public: typedef T& type; };

struct prova { };

typedef const prova ConstProva;
typedef const prova* PointerConstProva;
typedef add_const<prova>::type ConstProva;


template <typename Iterator>
struct iterator_traits;

template <typename Iterator>
struct iterator_traits<const Iterator*>
{
    typedef Iterator value_type;
};

template <class Iterator>
struct iterator_pointee
{
    typedef typename iterator_traits<Iterator>::value_type value_type;
    typedef typename add_const<value_type>::type type;
};

typedef iterator_pointee<PointerConstProva>::type ConstProva;
