/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

namespace Implementation
{
    namespace WatchDog
    {
        template <typename T>
        struct EList { };

        template <typename T>
        struct Pending_List
        {
            typedef typename EList<T>::iterator iterator;
        };

        struct Handler { };
    }
}

template <typename Traits>
struct Threshold_Watcher
{
    typedef Implementation::WatchDog::Pending_List<Traits> TW_Pending_List1;
    typedef TW_Pending_List1 TW_Pending_List2;
    typedef typename TW_Pending_List2::iterator TW_Pending_List3;
    typedef Implementation::WatchDog::Handler TW_Handler;

    typename TW_Pending_List1::iterator foo11(typename Traits::Threshold,
            const TW_Handler& handler,
            bool &expired_flag);
    typename TW_Pending_List1::iterator foo12(typename Traits::Threshold,
            const TW_Handler& handler,
            bool &expired_flag);
    typename TW_Pending_List1::iterator foo13(typename Traits::Threshold,
            const TW_Handler& handler,
            bool &expired_flag);

    typename TW_Pending_List2::iterator foo21(typename Traits::Threshold,
            const TW_Handler& handler,
            bool &expired_flag);
    typename TW_Pending_List2::iterator foo22(typename Traits::Threshold,
            const TW_Handler& handler,
            bool &expired_flag);
    typename TW_Pending_List2::iterator foo23(typename Traits::Threshold,
            const TW_Handler& handler,
            bool &expired_flag);

    TW_Pending_List3 foo31(typename Traits::Threshold,
            const TW_Handler& handler,
            bool &expired_flag);
    TW_Pending_List3 foo32(typename Traits::Threshold,
            const TW_Handler& handler,
            bool &expired_flag);
    TW_Pending_List3 foo33(typename Traits::Threshold,
            const TW_Handler& handler,
            bool &expired_flag);
};

template <typename Traits>
typename Threshold_Watcher<Traits>::TW_Pending_List1::iterator
Threshold_Watcher<Traits>::foo11(typename Traits::Threshold,
            const TW_Handler& handler,
            bool &expired_flag)
{
}
template <typename Traits>
typename Threshold_Watcher<Traits>::TW_Pending_List2::iterator
Threshold_Watcher<Traits>::foo12(typename Traits::Threshold,
            const TW_Handler& handler,
            bool &expired_flag)
{
}
template <typename Traits>
typename Threshold_Watcher<Traits>::TW_Pending_List3
Threshold_Watcher<Traits>::foo13(typename Traits::Threshold,
            const TW_Handler& handler,
            bool &expired_flag)
{
}


template <typename Traits>
typename Threshold_Watcher<Traits>::TW_Pending_List1::iterator
Threshold_Watcher<Traits>::foo21(typename Traits::Threshold,
            const TW_Handler& handler,
            bool &expired_flag)
{
}
template <typename Traits>
typename Threshold_Watcher<Traits>::TW_Pending_List2::iterator
Threshold_Watcher<Traits>::foo22(typename Traits::Threshold,
            const TW_Handler& handler,
            bool &expired_flag)
{
}
template <typename Traits>
typename Threshold_Watcher<Traits>::TW_Pending_List3
Threshold_Watcher<Traits>::foo23(typename Traits::Threshold,
            const TW_Handler& handler,
            bool &expired_flag)
{
}

template <typename Traits>
typename Threshold_Watcher<Traits>::TW_Pending_List1::iterator
Threshold_Watcher<Traits>::foo31(typename Traits::Threshold,
            const TW_Handler& handler,
            bool &expired_flag)
{
}
template <typename Traits>
typename Threshold_Watcher<Traits>::TW_Pending_List2::iterator
Threshold_Watcher<Traits>::foo32(typename Traits::Threshold,
            const TW_Handler& handler,
            bool &expired_flag)
{
}
template <typename Traits>
typename Threshold_Watcher<Traits>::TW_Pending_List3
Threshold_Watcher<Traits>::foo33(typename Traits::Threshold,
            const TW_Handler& handler,
            bool &expired_flag)
{
}
