template <typename _T>
struct type_trait
{
    typedef _T* pointer;
};

template <class T_base, class T_derived>
struct is_base_and_derived
{
private:
  struct big {
    char memory[64];
  };
  static big is_base_class_(...);
  static char is_base_class_(typename type_trait<T_base>::pointer);
public:
  static const bool value =
    sizeof(is_base_class_(reinterpret_cast<typename type_trait<T_derived>::pointer>(0))) ==
    sizeof(char);
  void avoid_gcc3_warning_();
};

struct A
{
};

struct B : A
{
};

template <bool _B>
struct test { };

template <>
struct test<true>
{ 
    typedef int T;
};

typedef test<is_base_and_derived<A, B>::value>::T P;
typedef int P;
