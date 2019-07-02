/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T> class B {};

template <typename T> class A : public B<T> {
public:
  typedef B<T> Base;
  using Base::Base;
};
