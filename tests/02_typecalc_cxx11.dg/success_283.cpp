/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
namespace std
{
  class exception
  {
  public:
    virtual const char* what() const noexcept;
  };
}


class  exception : public ::std::exception
{
    const char* what() const noexcept override
    {}
};
