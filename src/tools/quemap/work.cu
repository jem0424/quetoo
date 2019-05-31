#include <SDL_Timer.h>
#include "work.h"
#include "common.h"

__global__
void Work_c(const char *name, WorkFunc func, int32_t count, thread_t &threadpool) {
	cudaMemset(&work, 0, sizeof(work));

	work.lock = SDL_CreateMutex(); // is the mutex needed in this case?
	work.name = name;
	work.count = count;
	work.func = func;
	work.index = 0;
	work.percent = -1;

	const uint32_t start = SDL_GetTicks();
	threadpool[threadIdx.x] = Thread_Create(RunFunc, NULL, 0);
	Thread_Wait(threads[threadId.x]);

	const uint32_t end = SDL_GetTicks();

	if (work.name) {
		Com_Print(" %d ms\n", end - start);
	}
}

void dispatchWork(const char *name, WorkFunc func, int32_t count) {
	cudaGetDeviceProp *prop;
	cudaGetDeviceProperties ( &prop );
	int numThreads = prop.maxThreadsPerBlock;
	thread_t *threads[thread_count];
	Work_c<<<1, numThreads>>>(*name, func, count, &threads);
	cudaDeviceSynchonize();
}
