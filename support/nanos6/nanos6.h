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
} task_info;

void nanos_create_task(
	task_info *taskinfo,
	size_t argsblocksize,
	/* out */ void **argsblockpointer,
	/* out */ void **taskpointer
);

void nanos_submit_task(void *task);

void nanos_taskwait(void);

#ifdef __cplusplus
}
#endif

#endif // NANOS6_H
