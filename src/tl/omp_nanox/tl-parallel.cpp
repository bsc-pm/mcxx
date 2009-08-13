#include "tl-omp-nanox.hpp"

namespace TL
{
    namespace Nanox
    {
        void OMPTransform::parallel_postorder(PragmaCustomConstruct ctr)
        {

        }

        Source common_parallel_spawn_code()
        {
            Source result;

            result
                << "{"
                // FIXME - How to get the default number of threads?
                << "unsigned int _nanos_num_threads = 4;"
                << "nanos_team_t _nanos_team;"
                << "nanos_thread_t _nanos_threads[_nanos_num_threads];"
                << "err = nth_create_team(&team, (nanos_sched_t)0, &_nanos_num_threads,"
                <<            "(nanos_constraint_t*)0, /* reuse */ false, _nanos_threads);"
                << "if (err != NANOS_OK) nanos_error_handler(err);"

                << "int _i;"
                << "for (_i = 0; _i < _nanos_num_threads; _i++)"
                << "{"
                << "}"
                ;

            return result;
        }
    }
}
