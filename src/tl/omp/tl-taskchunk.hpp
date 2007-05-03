#ifndef TL_TASKCHUNK_HPP
#define TL_TASKCHUNK_HPP

#include "tl-omptransform.hpp"
namespace TL
{
    struct TaskWhileInfo
    {
        public:
            std::string chunking;
            Source& pre_src;
            Source& post_src;

            TaskWhileInfo()
                : pre_src(*(new Source)),
                post_src(*(new Source))
            {
            }
    };

    extern std::stack<TaskWhileInfo> task_while_stack;
}
#endif // TL_TASKCHUNK_HPP
