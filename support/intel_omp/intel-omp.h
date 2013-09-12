#ifndef INTEL_OMP_H
#define INTEL_OMP_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdlib.h>

/*
 * Public interface for the Intel OpenMP RTL
 * used by Mercurium
 */

/* Basic types */

enum {
 KMP_IDENT_IMB = 0x01,
 KMP_IDENT_KMPC = 0x02,
 KMP_IDENT_AUTOPAR = 0x08,
 KMP_IDENT_ATOMIC_REDUCE = 0x10,
 KMP_IDENT_BARRIER_EXPL = 0x20,
 KMP_IDENT_BARRIER_IMPL = 0x40,
};

typedef int32_t kmp_int32;
typedef int64_t kmp_int64;
typedef uint32_t kmp_uint32;
typedef uint64_t kmp_uint64;

typedef struct ident {
 kmp_int32 reserved_1;
 kmp_int32 flags;
 kmp_int32 reserved_2;
 kmp_int32 reserved_3;
 const char *psource; // Note: Intel defines this as char*
} ident_t;

/* Atomic */

#include "intel-omp-atomics.h"

/* Startup and shutdown */

void __kmpc_begin(ident_t* loc, kmp_int32 flags);
void __kmpc_end(ident_t* loc);

/* Parallel fork/join */

typedef void (*kmpc_micro)(kmp_int32* global_tid, kmp_int32* bound_tid, ...);

void __kmpc_push_num_threads (ident_t *loc, kmp_int32 global_tid, kmp_int32 num_threads);
void __kmpc_fork_call (ident_t *loc, kmp_int32 argc, kmpc_micro microtask,...);
void __kmpc_push_num_teams (ident_t *loc, kmp_int32 global_tid, kmp_int32 num_teams, kmp_int32 num_threads);
void __kmpc_fork_teams (ident_t *loc, kmp_int32 argc, kmpc_micro microtask,...);
void __kmpc_serialized_parallel (ident_t *loc, kmp_int32 global_tid);
void __kmpc_end_serialized_parallel (ident_t *loc, kmp_int32 global_tid);

/* Thread information */

kmp_int32 __kmpc_global_thread_num (ident_t *loc);
kmp_int32 __kmpc_global_num_threads (ident_t *loc);
kmp_int32 __kmpc_bound_thread_num (ident_t *loc);
kmp_int32 __kmpc_bound_num_threads (ident_t *loc);
kmp_int32 __kmpc_in_parallel (ident_t *loc);

/* Worksharing */

enum sched_type {
 kmp_sch_lower = 32 , kmp_sch_static = 34 , kmp_sch_guided_chunked = 36 , kmp_sch_auto = 38 ,
 kmp_sch_static_steal = 44, kmp_sch_upper = 45, kmp_ord_lower = 64 , kmp_ord_static = 66 ,
 kmp_ord_auto = 70 , kmp_ord_upper = 72, kmp_distribute_static_chunked = 91, kmp_distribute_static = 92,
 kmp_nm_lower = 160 , kmp_nm_static = 162 , kmp_nm_guided_chunked = 164 , kmp_nm_auto = 166 ,
 kmp_nm_ord_static = 194 , kmp_nm_ord_auto = 198 , kmp_nm_upper = 200, kmp_sch_default = kmp_sch_static
};

typedef kmp_int32 kmp_critical_name[8];

kmp_int32 __kmpc_master (ident_t *loc, kmp_int32 global_tid);
void __kmpc_end_master (ident_t *loc, kmp_int32 global_tid);
void __kmpc_ordered (ident_t *loc, kmp_int32 gtid);
void __kmpc_end_ordered (ident_t *loc, kmp_int32 gtid);
void __kmpc_critical (ident_t *loc, kmp_int32 global_tid, kmp_critical_name *crit);
void __kmpc_end_critical (ident_t *loc, kmp_int32 global_tid, kmp_critical_name *crit);
kmp_int32 __kmpc_single (ident_t *loc, kmp_int32 global_tid);
void __kmpc_end_single (ident_t *loc, kmp_int32 global_tid);
void __kmpc_for_static_fini (ident_t *loc, kmp_int32 global_tid);
void __kmpc_dispatch_init_4 (ident_t *loc, kmp_int32 gtid, enum sched_type schedule, kmp_int32 lb, kmp_int32 ub, kmp_int32 st, kmp_int32 chunk);
void __kmpc_dispatch_init_4u (ident_t *loc, kmp_int32 gtid, enum sched_type schedule, kmp_uint32 lb, kmp_uint32 ub, kmp_int32 st, kmp_int32 chunk);
void __kmpc_dispatch_init_8 (ident_t *loc, kmp_int32 gtid, enum sched_type schedule, kmp_int64 lb, kmp_int64 ub, kmp_int64 st, kmp_int64 chunk);
void __kmpc_dispatch_init_8u (ident_t *loc, kmp_int32 gtid, enum sched_type schedule, kmp_uint64 lb, kmp_uint64 ub, kmp_int64 st, kmp_int64 chunk);
int __kmpc_dispatch_next_4 (ident_t *loc, kmp_int32 gtid, kmp_int32 *p_last, kmp_int32 *p_lb, kmp_int32 *p_ub, kmp_int32 *p_st);
int __kmpc_dispatch_next_4u (ident_t *loc, kmp_int32 gtid, kmp_int32 *p_last, kmp_uint32 *p_lb, kmp_uint32 *p_ub, kmp_int32 *p_st);
int __kmpc_dispatch_next_8 (ident_t *loc, kmp_int32 gtid, kmp_int32 *p_last, kmp_int64 *p_lb, kmp_int64 *p_ub, kmp_int64 *p_st);
int __kmpc_dispatch_next_8u (ident_t *loc, kmp_int32 gtid, kmp_int32 *p_last, kmp_uint64 *p_lb, kmp_uint64 *p_ub, kmp_int64 *p_st);
void __kmpc_dispatch_fini_4 (ident_t *loc, kmp_int32 gtid);
void __kmpc_dispatch_fini_8 (ident_t *loc, kmp_int32 gtid);
void __kmpc_dispatch_fini_4u (ident_t *loc, kmp_int32 gtid);
void __kmpc_dispatch_fini_8u (ident_t *loc, kmp_int32 gtid);
void __kmpc_for_static_init_4 (ident_t *loc, kmp_int32 gtid, kmp_int32 schedtype, kmp_int32 *plastiter, kmp_int32 *plower, kmp_int32 *pupper, kmp_int32 *pstride, kmp_int32 incr, kmp_int32 chunk);
void __kmpc_for_static_init_4u (ident_t *loc, kmp_int32 gtid, kmp_int32 schedtype, kmp_int32 *plastiter, kmp_uint32 *plower, kmp_uint32 *pupper, kmp_int32 *pstride, kmp_int32 incr, kmp_int32 chunk);
void __kmpc_for_static_init_8 (ident_t *loc, kmp_int32 gtid, kmp_int32 schedtype, kmp_int32 *plastiter, kmp_int64 *plower, kmp_int64 *pupper, kmp_int64 *pstride, kmp_int64 incr, kmp_int64 chunk);
void __kmpc_for_static_init_8u (ident_t *loc, kmp_int32 gtid, kmp_int32 schedtype, kmp_int32 *plastiter, kmp_uint64 *plower, kmp_uint64 *pupper, kmp_int64 *pstride, kmp_int64 incr, kmp_int64 chunk);

/* Synchronization */

void __kmpc_flush (ident_t *loc,...);
void __kmpc_barrier (ident_t *loc, kmp_int32 global_tid);
kmp_int32 __kmpc_barrier_master (ident_t *loc, kmp_int32 global_tid);
void __kmpc_end_barrier_master (ident_t *loc, kmp_int32 global_tid);
kmp_int32 __kmpc_barrier_master_nowait (ident_t *loc, kmp_int32 global_tid);
kmp_int32 __kmpc_reduce_nowait (ident_t *loc, kmp_int32 global_tid, kmp_int32 num_vars, size_t reduce_size, void *reduce_data, void(*reduce_func)(void *lhs_data, void *rhs_data), kmp_critical_name *lck);
void __kmpc_end_reduce_nowait (ident_t *loc, kmp_int32 global_tid, kmp_critical_name *lck);
kmp_int32 __kmpc_reduce (ident_t *loc, kmp_int32 global_tid, kmp_int32 num_vars, size_t reduce_size, void *reduce_data, void(*reduce_func)(void *lhs_data, void *rhs_data), kmp_critical_name *lck);
void __kmpc_end_reduce (ident_t *loc, kmp_int32 global_tid, kmp_critical_name *lck);

/* Threadprivate data support */

typedef void *(* kmpc_ctor )(void *);
typedef void(* kmpc_dtor )(void *);
typedef void *(* kmpc_cctor )(void *, void *);
typedef void *(* kmpc_ctor_vec )(void *, size_t);
typedef void(* kmpc_dtor_vec )(void *, size_t);
typedef void *(* kmpc_cctor_vec )(void *, void *, size_t);

void __kmpc_copyprivate (ident_t *loc, kmp_int32 gtid, size_t cpy_size, void *cpy_data, void(*cpy_func)(void *, void *), kmp_int32 didit);
void __kmpc_threadprivate_register (ident_t *loc, void *data, kmpc_ctor ctor, kmpc_cctor cctor, kmpc_dtor dtor);
void * __kmpc_threadprivate_cached (ident_t *loc, kmp_int32 global_tid, void *data, size_t size, void ***cache);
void __kmpc_threadprivate_register_vec (ident_t *loc, void *data, kmpc_ctor_vec ctor, kmpc_cctor_vec cctor, kmpc_dtor_vec dtor, size_t vector_length);

#ifdef __cplusplus
}
#endif

#endif // INTEL_OMP_H
