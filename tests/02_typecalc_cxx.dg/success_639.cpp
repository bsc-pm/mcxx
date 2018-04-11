/*
<testinfo>
test_generator="config/mercurium"
</testinfo>
*/

template<typename ArgType>
struct unary_evaluator
{
    class InnerVectorInnerIterator;
    typedef ArgType  EvalIterator;
};

template<typename ArgType>
class unary_evaluator<ArgType>::InnerVectorInnerIterator
{
public:
  inline InnerVectorInnerIterator()
  {
    while( (EvalIterator::operator bool()))
      EvalIterator::operator++();
  }

  inline operator bool() const { return EvalIterator::operator bool(); }
};
