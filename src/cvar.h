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

#ifndef __CVAR_H__
#define __CVAR_H__

#include "filesystem.h"

extern bool cvar_user_info_modified;

typedef void (*cvar_enumerate_func)(cvar_t *var, void *data);

cvar_t *Cvar_Get(const char *name, const char *value, uint32_t flags, const char *description);
cvar_t *Cvar_Set(const char *name, const char *value);
cvar_t *Cvar_ForceSet(const char *name, const char *value);
cvar_t *Cvar_FullSet(const char *name, const char *value, uint32_t flags);
void Cvar_SetValue(const char *name, float value);
void Cvar_Toggle(const char *name);
float Cvar_GetValue(const char *name);
char *Cvar_GetString(const char *name);
void Cvar_Enumerate(cvar_enumerate_func func, void *data);
void Cvar_CompleteVar(const char *pattern, GList **matches);
void Cvar_ResetLocalVars(void);
bool Cvar_PendingLatchedVars(void);
void Cvar_UpdateLatchedVars(void);
bool Cvar_PendingVars(uint32_t flags);
void Cvar_ClearVars(uint32_t flags);
bool Cvar_Command(void);
char *Cvar_UserInfo(void);
char *Cvar_ServerInfo(void);
void Cvar_WriteVariables(file_t *f);
void Cvar_Init(void);
void Cvar_Shutdown(void);

#endif /* __CVAR_H__ */
