#include <stdint.h>
#include <stdio.h>
#include <omp.h>

typedef void (*work_fn_t)(int64_t);

void dispatch_parallel_for(int64_t start, int64_t end, void* work_fn_ptr) {
    work_fn_t work = (work_fn_t)work_fn_ptr;

    // printf("[Runtime] Starting parallel dispatch from %lld to %lld...\n", start, end);

    #pragma omp parallel for
    for (int64_t i = start; i < end; ++i) {
        // Get the unique ID of the executing thread
        int thread_id = omp_get_thread_num();
        
        // Log the thread ID and the index it's working on
        // printf("[Thread %d] Executing work for index %lld\n", thread_id, i);
        
        work(i);
    }

    // printf("[Runtime] Parallel dispatch finished.\n");
}