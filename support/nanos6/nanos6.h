#ifndef NANOS6_H
#define NANOS6_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif


typedef struct
{
	void (*run)(void *args_block);
	void (*register_depinfo)(void *task, void *args_block);
	void (*register_copies)(void *task, void *args_block);

	void* task_label;
	void* declaration_source;
} nanos_task_info;

void nanos_create_task(
	nanos_task_info *taskinfo,
	size_t argsblocksize,
	/* out */ void **argsblockpointer,
	/* out */ void **taskpointer
);

void nanos_submit_task(void *task);

void nanos_taskwait(void);

typedef
struct {
    size_t size;
    size_t lower_bound;
    size_t accessed_length;
} nanos_rank_info;

void nanos_register_dep_in(
        void *addr,
        int rank,
        nanos_rank_info* info);
void nanos_register_dep_out(
        void *addr,
        int rank,
        nanos_rank_info* info);
void nanos_register_dep_inout(
        void *addr,
        int rank,
        nanos_rank_info* info);

#ifdef __cplusplus
}
#endif

#endif // NANOS6_H
