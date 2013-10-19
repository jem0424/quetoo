/*
 * Copyright(c) 1997-2001 Id Software, Inc.
 * Copyright(c) 2002 The Quakeforge Project.
 * Copyright(c) 2006 Quake2World.
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

#ifndef __PMOVE_H__
#define __PMOVE_H__

#include "shared.h"

/*
 * @brief Acceleration constants.
 */
#define PM_ACCEL_AIR			1.66
#define PM_ACCEL_GROUND			10.0
#define PM_ACCEL_GROUND_SLICK	4.375
#define PM_ACCEL_SPECTATOR		4.5
#define PM_ACCEL_WATER			4.0

/*
 * @brief Bounce constant when clipping against solids.
 */
#define PM_CLIP_BOUNCE			1.001

/*
 * @brief If set, debug messages will be generated by Pm_Move.
 */
#define PM_DEBUG 1

/*
 * @brief Friction constants.
 */
#define PM_FRICT_AIR			0.45
#define PM_FRICT_GROUND			6.0
#define PM_FRICT_GROUND_SLICK	2.0
#define PM_FRICT_LADDER			10.0
#define PM_FRICT_SPECTATOR		3.0
#define PM_FRICT_WATER			1.0

/*
 * @brief Water gravity constant.
 */
#define PM_GRAVITY_WATER		0.55

/*
 * @brief Distances traced when seeking ground.
 */
#define PM_GROUND_DIST			1.0
#define PM_GROUND_DIST_TRICK	16.0

/*
 * @brief Speed constants; intended velocities are clipped to these.
 */
#define PM_SPEED_AIR			380.0
#define PM_SPEED_CURRENT		100.0
#define PM_SPEED_DUCK_STAND		225.0
#define PM_SPEED_DUCKED			140.0
#define PM_SPEED_FALL			-540.0
#define PM_SPEED_FALL_FAR		-720.0
#define PM_SPEED_JUMP			262.5
#define PM_SPEED_LADDER			125.0
#define PM_SPEED_LAND			-300.0
#define PM_SPEED_RUN			300.0
#define PM_SPEED_SPECTATOR		350.0
#define PM_SPEED_STOP			100.0
#define PM_SPEED_UP				0.1
#define PM_SPEED_TRICK_JUMP		60.0
#define PM_SPEED_WATER			125.0
#define PM_SPEED_WATER_JUMP		450.0
#define PM_SPEED_WATER_SINK		50.0

/*
 * @brief The vertical distance afforded in step climbing.
 */
#define PM_STEP_HEIGHT			16.0

/*
 * @brief The minimum Z plane normal component required for standing.
 */
#define PM_STEP_NORMAL			0.7

/*
 * @brief Velocity is cleared when less than this.
 */
#define PM_STOP_EPSILON			0.1

/*
 * @brief Player bounding box scaling. VectorScale(PM_MINS, PM_SCALE, mins)..
 */
#define PM_SCALE 1.0

extern vec3_t PM_MINS;
extern vec3_t PM_MAXS;

/*
 * @brief Game-specific flags for pm_state_t.flags.
 */
#define PMF_DUCKED				0x2 // player is ducked
#define PMF_JUMPED				0x4 // player just jumped
#define PMF_JUMP_HELD			0x8 // player's jump key is down
#define PMF_ON_GROUND			0x10 // player is on ground
#define PMF_ON_STAIRS			0x20 // player just traversed step
#define PMF_ON_LADDER			0x40 // player is on ladder
#define PMF_UNDER_WATER			0x80 // player is under water
#define PMF_PUSHED				0x100 // disable stair checking (?)
#define PMF_TIME_TRICK_JUMP		0x200 // time eligible for trick jump
#define PMF_TIME_WATER_JUMP		0x400 // time before control
#define PMF_TIME_LAND			0x800 // time before jump eligible
#define PMF_TIME_TELEPORT		0x1000 // time frozen in place

/*
 * @brief The mask of pm_state_t.flags affecting pm_state_t.time.
 */
#define PMF_TIME_MASK (PMF_TIME_TRICK_JUMP | PMF_TIME_WATER_JUMP | PMF_TIME_LAND | PMF_TIME_TELEPORT)

/*
 * @brief Performs one discrete movement of the player through the world.
 */
void Pm_Move(pm_move_t *pm_move);

#endif /* __PMOVE_H__ */
