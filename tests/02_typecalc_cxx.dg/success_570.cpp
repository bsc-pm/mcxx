/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
void free(void*);

template <void (*dealloc)(void *)>
struct sd_ptr_generic {
        sd_ptr_generic() {}
};

typedef sd_ptr_generic< ::free> sd_ptr;

sd_ptr y;
