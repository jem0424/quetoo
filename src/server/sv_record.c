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

#include "sv_local.h"
#include "collision/cmodel.h"

/**
 * @brief
 */
void Sv_RecordDemoMulticast(void) {

	if (!sv.demo_record) {
		return;
	}

	Fs_Write(sv.demo_file, sv.multicast.data, sv.multicast.size, 1);
}

/**
 * @brief Writes a single frame of server data.
 */
void Sv_RecordDemoFrame(void) {
	if (!sv.demo_record) {
		return;
	}

	static mem_buf_t msg;
	static byte buffer[MAX_MSG_SIZE * 16];
	static entity_state_t null_state;

	// write out messages to hold the startup information
	Mem_InitBuffer(&msg, buffer, sizeof(buffer));

	// write a frame message that doesn't contain a player_state_t
	Net_WriteByte(&msg, SV_CMD_FRAME);
	Net_WriteLong(&msg, sv.frame_num);
	Net_WriteLong(&msg, 0); // what we are delta'ing from
	Net_WriteByte(&msg, 0); // rate dropped packets
	
	// send over the area bits (all visible)
	byte area_bits[MAX_BSP_AREAS >> 3];
	const _Bool old_no_areas = cm_no_areas;
	cm_no_areas = true;
	const int32_t area_bytes = Cm_WriteAreaBits(0, area_bits);
	Net_WriteByte(&msg, area_bytes);
	Net_WriteData(&msg, area_bits, area_bytes);
	cm_no_areas = old_no_areas;

	// write null player state
	static player_state_t null_player_state;
	Net_WriteDeltaPlayerState(&msg, &null_player_state, &null_player_state);

	// write entities
	for (int32_t i = 1; i < svs.game->num_entities; i++) {

		g_entity_t *ent = ENTITY_FOR_NUM(i);

		if (!ent->in_use) {
			continue;
		}

		if (!ent->s.model1 && !ent->s.sound && !ent->s.effects) {
			continue;
		}

		if (ent->sv_flags & SVF_NO_CLIENT) {
			continue;
		}

		Net_WriteDeltaEntity(&msg, &null_state, &ent->s, true);
	}

	// write end of entities
	Net_WriteShort(&msg, 0);

	Fs_Write(sv.demo_file, msg.data, msg.size, 1);
}

/**
 * @brief Writes server_data, config_strings, and baselines.
 */
static void Sv_WriteDemoHeader(void) {
	static byte buffer[MAX_MSG_SIZE];
	static mem_buf_t msg;

	// write out messages to hold the startup information
	Mem_InitBuffer(&msg, buffer, sizeof(buffer));

	// write the server data
	Net_WriteByte(&msg, SV_CMD_SERVER_DATA);
	Net_WriteShort(&msg, PROTOCOL_MAJOR);
	Net_WriteShort(&msg, svs.game->protocol);
	Net_WriteByte(&msg, 2); // server_record byte
	Net_WriteString(&msg, game->string);
	Net_WriteShort(&msg, -1);
	Net_WriteString(&msg, sv.config_strings[CS_NAME]);

	// and config_strings
	for (int32_t i = 0; i < MAX_CONFIG_STRINGS; i++) {
		if (*sv.config_strings[i] != '\0') {
			if (msg.size + strlen(sv.config_strings[i]) + 32 > msg.max_size) { // write it out
				const int32_t len = LittleLong((int32_t) msg.size);
				Fs_Write(sv.demo_file, &len, sizeof(len), 1);
				Fs_Write(sv.demo_file, msg.data, msg.size, 1);
				msg.size = 0;
			}

			Net_WriteByte(&msg, SV_CMD_CONFIG_STRING);
			Net_WriteShort(&msg, i);
			Net_WriteString(&msg, sv.config_strings[i]);
		}
	}

	Net_WriteByte(&msg, SV_CMD_CBUF_TEXT);
	Net_WriteString(&msg, "precache 0\n");

	// write it to the demo file

	const int32_t len = LittleLong((int32_t) msg.size);

	Fs_Write(sv.demo_file, &len, sizeof(len), 1);
	Fs_Write(sv.demo_file, msg.data, msg.size, 1);

	Com_Debug(DEBUG_SERVER, "Serverrecord started\n");
}

#if 0
/**
 * @brief Dumps the current net message, prefixed by the length.
 */
void Cl_WriteDemoMessage(void) {

	if (!cls.demo_file) {
		return;
	}

	if (!Fs_Tell(cls.demo_file)) {
		if (cl.frame.delta_frame_num < 0) {
			Com_Debug(DEBUG_CLIENT, "Received uncompressed frame, writing demo header..\n");
			Cl_WriteDemoHeader();
		} else {
			return; // wait for an uncompressed packet
		}
	}

	// the first eight bytes are just packet sequencing stuff
	const int32_t len = LittleLong((int32_t) (net_message.size - 8));

	Fs_Write(cls.demo_file, &len, sizeof(len), 1);
	Fs_Write(cls.demo_file, net_message.data + 8, len, 1);
}
#endif

/**
 * @brief Stop recording a server demo
 */
void Sv_Stop_f(void) {
	int32_t len = -1;

	if (!sv.demo_record) {
		Com_Print("Not recording a server demo\n");
		return;
	}

	// finish up
	Fs_Write(sv.demo_file, &len, sizeof(len), 1);
	Fs_Close(sv.demo_file);

	sv.demo_file = NULL;
	sv.demo_record = false;
	Com_Print("Stopped demo\n");
}

/**
 * @brief sv_record <demo name>
 *
 * Begin recording a server demo from the current frame until `sv_stop` is issued.
 */
void Sv_Record_f(void) {

	if (Cmd_Argc() != 2) {
		Com_Print("Usage: %s <demo name>\n", Cmd_Argv(0));
		return;
	}

	if (sv.state != SV_ACTIVE_GAME) {
		Com_Print("You must be running a game server to record\n");
		return;
	}

	if (sv.demo_record) {
		Com_Print("Already recording\n");
		return;
	}

	char demo_filename[MAX_QPATH];

	g_snprintf(demo_filename, sizeof(demo_filename), "demos/%s.svdemo", Cmd_Argv(1));

	// open the demo file
	if (!(sv.demo_file = Fs_OpenWrite(demo_filename))) {
		Com_Warn("Couldn't open %s\n", demo_filename);
		return;
	}

	sv.demo_record = true;

	Com_Print("Recording to %s\n", demo_filename);

	Sv_WriteDemoHeader();
}