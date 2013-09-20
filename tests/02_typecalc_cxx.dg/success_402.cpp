/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template<int D>
struct Mask
{
   virtual bool inside(int) = 0;
   virtual bool inside(double) = 0;
};

template<int D>
struct BallMask : public Mask<D>
{
   using Mask<D>::inside;
   bool inside(int);
   bool inside(double);
};

template<int D>
bool BallMask<D>::inside(int)
{
   return true;
}

template<>
bool BallMask<2>::inside(double)
{
   return inside(1);
}
