/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
class C
{
    public:
	template<typename D>
	D* get_ptr() const
	{
	    return 0;
	}
};

template<class D>
D * get_ptr()
{
    C *c_ptr;
    c_ptr->::C::get_ptr<D>();
    return 0;
}
