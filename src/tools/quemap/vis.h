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

#pragma once

#include "bsp.h"

#define	ON_EPSILON	0.1

typedef struct {
	vec3_t normal;
	vec_t dist;
} plane_t;

#define MAX_POINTS_ON_WINDING	64
#define	MAX_POINTS_ON_STACK_WINDING	12

typedef struct {
	int32_t num_points;
	vec3_t points[MAX_POINTS_ON_STACK_WINDING]; // variable sized
} winding_t;

typedef enum {
	STATUS_NONE, STATUS_WORKING, STATUS_DONE
} status_t;

typedef struct {
	plane_t plane; // normal pointing into neighbor
	int32_t leaf; // neighbor

	vec3_t origin; // for fast clip testing
	vec_t radius;

	winding_t *winding;
	status_t status;
	byte *front; // [portals], preliminary
	byte *flood; // [portals], intermediate
	byte *vis; // [portals], final

	size_t num_might_see; // bit count on flood for sort
} portal_t;

#define	MAX_PORTALS_ON_LEAF		128

typedef struct {
	int32_t num_portals;
	portal_t *portals[MAX_PORTALS_ON_LEAF];
} leaf_t;

typedef struct pstack_s {
	byte might_see[MAX_BSP_PORTALS / 8]; // bit string
	leaf_t *leaf;
	portal_t *portal; // portal exiting
	winding_t *source;
	winding_t *pass;

	winding_t windings[3]; // source, pass, temp in any order
	_Bool free_windings[3];

	plane_t portalplane;
	struct pstack_s *next;
} pstack_t;

typedef struct {
	portal_t *base;
	int32_t c_chains;
	pstack_t pstack_head;
} thread_data_t;

typedef struct map_vis_s {
	int32_t num_portals;
	int32_t portal_clusters;

	portal_t *portals;
	portal_t *sorted_portals[MAX_BSP_PORTALS * 2];

	leaf_t *leafs;

	int32_t leaf_bytes; // (portal_clusters + 63) >> 3
	int32_t portal_bytes; // (num_portals * 2 + 63) >> 3

	int32_t uncompressed_size;
	byte *uncompressed;

	byte *base;
	byte *pointer;
	byte *end;
} map_vis_t;

extern map_vis_t map_vis;

extern _Bool fast_vis;
extern _Bool no_sort;

void BaseVis(int32_t portal_num);
void FinalVis(int32_t portal_num);

int32_t CountBits(const byte *bits, int32_t max);