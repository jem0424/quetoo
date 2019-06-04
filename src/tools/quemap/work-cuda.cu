/*
 * Copyright(c) 1997-2001 id Software, Inc.
 * Copyright(c) 2002 The Quakeforge Project.
 * Copyright(c) 2006 Quetoo.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

#include <cuda.h>
#include <stdio.h>

#include "work.h"

__global__ void GetWork(void) {

}

extern "C" {

/**
 * @brief
 */
int32_t WorkCuda(work_t *work) {

	cuInit(0);

	int count;
	cuDeviceGetCount(&count);

	if (count > 0) {

		CUdevice device;
		cuDeviceGet(&device, 0);

		int threads;
		cuDeviceGetAttribute(&threads, CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_BLOCK, device);

		printf("Let's run some threads.. %d\n", threads);
	}

	return 0;
}

}
