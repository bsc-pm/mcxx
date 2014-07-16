/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
class Temp_Item {
public:
  static Temp_Item& obtain();
  static void release(Temp_Item& p);
  T& item();
private:
  T item_;
  Temp_Item* next;
  static Temp_Item* free_list_head;
  Temp_Item();
  Temp_Item(const Temp_Item&);
  Temp_Item& operator=(const Temp_Item&);
};
template <typename T>
class Temp_Reference_Holder {
public:
  Temp_Reference_Holder();
  ~Temp_Reference_Holder();
  T& item();
private:
  Temp_Reference_Holder(const Temp_Reference_Holder&);
  Temp_Reference_Holder& operator=(const Temp_Reference_Holder&);
  Temp_Item<T>& held;
};
template <typename T>
class Temp_Value_Holder {
public:
  Temp_Value_Holder();
  T& item();
private:
  Temp_Value_Holder(const Temp_Value_Holder&);
  Temp_Value_Holder& operator=(const Temp_Value_Holder&);
  T item_;
};

template <bool b>
struct Bool {
  enum const_bool_value {
    value = b
  };
};
struct True : public Bool<true> {
};
struct False : public Bool<false> {
};

struct mpz_class
{
    int x;
};

struct mpq_class
{
    int x;
};

template <bool b, typename T = void>
struct Enable_If {
};
template <typename T>
struct Enable_If<true, T> {
  typedef T type;
};

template <typename T>
struct Slow_Copy : public False {
};
template <>
struct Slow_Copy<mpz_class> : public True {
};
template <>
struct Slow_Copy<mpq_class> : public True {
};

template <typename T, typename Enable = void>
class Dirty_Temp;
template <typename T>
class Dirty_Temp<T, typename Enable_If<Slow_Copy<T>::value>::type>
  : public Temp_Reference_Holder<T> {
};
template <typename T>
class Dirty_Temp<T, typename Enable_If<!Slow_Copy<T>::value>::type>
  : public Temp_Value_Holder<T> {
};

void foo()
{
    // {
    //     typedef Enable_If<Slow_Copy<mpz_class>::value, True>::type T1;
    //     typedef True T1;
    // }

    Dirty_Temp<mpz_class> a;
}
