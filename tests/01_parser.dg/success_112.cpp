/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
namespace mpl_ { 
    namespace aux { 
        typedef int T; 
    } 
}

namespace boost {
    namespace mpl {
        using namespace mpl_;
        namespace aux {
            using namespace mpl_::aux;
            typedef float T; 
        }
    }
}

namespace boost {
    namespace mpl {
        typedef aux::T P;
        typedef float P;
    }
}
