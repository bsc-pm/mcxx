#include "tl-omp-nanox.hpp"
#include "tl-data-env.hpp"

namespace TL
{
    namespace Nanox
    {
        Source common_parallel_spawn_code(Source num_devices,
                Source device_descriptor, 
                Source struct_arg_type_name,
                Source num_threads,
                const DataEnvironInfo& data_environ_info);
    }
}
