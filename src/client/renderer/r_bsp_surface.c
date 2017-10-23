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

#include "r_local.h"

/**
 * @brief
 */
static void R_SetBspSurfaceState_default(const r_bsp_surface_t *surf) {

	if (r_state.blend_enabled) { // alpha blend
		vec4_t color = { 1.0, 1.0, 1.0, 1.0 };

		switch (surf->texinfo->flags & (SURF_BLEND_33 | SURF_BLEND_66)) {
			case SURF_BLEND_33:
				color[3] = 0.40;
				break;
			case SURF_BLEND_66:
				color[3] = 0.80;
				break;
			default: // both flags mean use the texture's alpha channel
				color[3] = 1.0;
				break;
		}

		R_Color(color);
	}

	if (texunit_diffuse->enabled) { // diffuse texture
		R_BindDiffuseTexture(surf->texinfo->material->diffuse->texnum);
	}

	if (texunit_lightmap->enabled) { // lightmap texture

		if (r_draw_bsp_lightmaps->value == 2) {
			R_BindLightmapTexture(surf->deluxemap->texnum);
		} else {
			R_BindLightmapTexture(surf->lightmap->texnum);
		}

		if (texunit_stainmap->enabled) {
			if (surf->stainmap.image) {
				R_BindStainmapTexture(surf->stainmap.image->texnum);
			}
		}
	}

	if (r_state.lighting_enabled) { // hardware lighting

		R_BindDeluxemapTexture(surf->deluxemap->texnum);

		if (surf->light_frame == r_locals.light_frame) { // dynamic light sources
			R_EnableLights(surf->light_mask);
		} else {
			R_EnableLights(0);
		}

		R_EnableCaustic(surf->flags & R_SURF_UNDERLIQUID);
	} else {
		R_EnableCaustic(false);
	}

	R_UseMaterial(surf->texinfo->material);

	if (r_state.stencil_test_enabled) { // write to stencil buffer to clip shadows
		if (r_model_state.world->bsp->plane_shadows[surf->plane->num]) {
			R_StencilFunc(GL_ALWAYS, R_STENCIL_REF(surf->plane->num), ~0);
		} else {
			R_StencilFunc(GL_ALWAYS, 0, 0);
		}
	}
}

/**
 * @brief
 */
static void R_DrawBspSurface_default(const r_bsp_surface_t *surf) {

	R_DrawArrays(GL_TRIANGLE_FAN, surf->index, surf->num_edges);

	r_view.num_bsp_surfaces++;
}

/**
 * @brief
 */
static void R_ResetBspSurfacesState_default(void) {

	if (r_state.lighting_enabled) {

		R_EnableLights(0);

		R_EnableCaustic(false);
	}

	R_UseMaterial(NULL);

	R_Color(NULL);
}

/**
 * @brief
 */
static void R_DrawBspSurfaces_default(const r_bsp_surfaces_t *surfs) {

	R_EnableTexture(texunit_diffuse, true);

	R_SetArrayState(r_model_state.world);

	// draw the surfaces
	for (size_t i = 0; i < surfs->count; i++) {

		if (surfs->surfaces[i]->texinfo->flags & SURF_MATERIAL) {
			continue;
		}

		if (surfs->surfaces[i]->frame != r_locals.frame) {
			continue;
		}

		R_SetBspSurfaceState_default(surfs->surfaces[i]);

		R_DrawBspSurface_default(surfs->surfaces[i]);
	}

	// reset state
	R_ResetBspSurfacesState_default();
}

/**
 * @brief
 */
static void R_DrawBspSurfacesLines_default(const r_bsp_surfaces_t *surfs) {

	R_EnableTexture(texunit_diffuse, false);

	R_BindDiffuseTexture(r_image_state.null->texnum);

	R_SetArrayState(r_model_state.world);

	glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);

	for (size_t i = 0; i < surfs->count; i++) {

		if (surfs->surfaces[i]->frame != r_locals.frame) {
			continue;
		}

		R_DrawBspSurface_default(surfs->surfaces[i]);
	}

	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

	R_EnableTexture(texunit_diffuse, true);
}

/**
 * @brief
 */
static void R_BeginDrawOpaqueBspSurfaces_default(void) {

	if (r_draw_bsp_lightmaps->value) {
		R_EnableTexture(texunit_diffuse, false);

		R_BindDiffuseTexture(r_image_state.null->texnum);
	}

	R_EnableTexture(texunit_lightmap, true);

	if (r_deluxemap->value) {
		R_EnableTexture(texunit_deluxemap, true);
	}

	if (r_stainmaps->value) {
		R_EnableTexture(texunit_stainmap, true);
	}

	R_EnableLighting(program_default, true);

	if (r_shadows->value) {
		R_EnableStencilTest(GL_REPLACE, true);
	}
}

/**
 * @brief
 */
static void R_EndDrawOpaqueBspSurfaces_default(void) {

	if (r_shadows->value) {
		R_EnableStencilTest(GL_KEEP, false);
	}

	R_EnableLighting(NULL, false);

	R_EnableTexture(texunit_lightmap, false);
	R_EnableTexture(texunit_deluxemap, false);
	R_EnableTexture(texunit_stainmap, false);

	if (r_draw_bsp_lightmaps->value) {
		R_EnableTexture(texunit_diffuse, true);
	}
}

/**
 * @brief
 */
static void R_BeginDrawWarpBspSurfaces_default(void) {

	R_EnableWarp(program_warp, true);
}

/**
 * @brief
 */
static void R_EndDrawWarpBspSurfaces_default(void) {

	R_EnableWarp(NULL, false);
}

/**
 * @brief
 */
static void R_BeginDrawAlphaTestBspSurfaces_default(void) {

	R_EnableAlphaTest(ALPHA_TEST_ENABLED_THRESHOLD);

	R_EnableTexture(texunit_lightmap, true);

	if (r_deluxemap->value) {
		R_EnableTexture(texunit_deluxemap, true);
	}

	if (r_stainmaps->value) {
		R_EnableTexture(texunit_stainmap, true);
	}

	R_EnableLighting(program_default, true);
}


/**
 * @brief
 */
static void R_EndDrawAlphaTestBspSurfaces_default(void) {
	
	R_EnableLighting(NULL, false);

	R_EnableTexture(texunit_lightmap, false);
	R_EnableTexture(texunit_deluxemap, false);
	R_EnableTexture(texunit_stainmap, false);

	R_EnableAlphaTest(ALPHA_TEST_DISABLED_THRESHOLD);
}

/**
 * @brief
 */
static void R_BeginDrawBlendBspSurfaces_default() {

	if (r_draw_bsp_lightmaps->value) {
		R_EnableTexture(texunit_diffuse, false);

		R_BindDiffuseTexture(r_image_state.null->texnum);
	}

	// blend is already enabled when this is called

	R_EnableTexture(texunit_lightmap, true);

	if (r_deluxemap->value) {
		R_EnableTexture(texunit_deluxemap, true);
	}

	if (r_stainmaps->value) {
		R_EnableTexture(texunit_stainmap, true);
	}

	R_EnableLighting(program_default, true);
}

/**
 * @brief
 */
static void R_EndDrawBlendBspSurfaces_default() {

	R_EnableLighting(NULL, false);

	R_EnableTexture(texunit_lightmap, false);
	R_EnableTexture(texunit_deluxemap, false);
	R_EnableTexture(texunit_stainmap, false);

	if (r_draw_bsp_lightmaps->value) {
		R_EnableTexture(texunit_diffuse, true);
	}
}

/**
 * @brief
 */
static void R_BeginDrawFromSurfaceType_default(const r_bsp_surface_type_t type) {

	switch (type) {
	case R_SURFTYPE_OPAQUE:
		R_BeginDrawOpaqueBspSurfaces_default();
		break;
	case R_SURFTYPE_WARP:
		R_BeginDrawWarpBspSurfaces_default();
		break;
	case R_SURFTYPE_ALPHA:
		R_BeginDrawAlphaTestBspSurfaces_default();
		break;
	default:
		Com_Error(ERROR_DROP, "This type of surface should not be batched\n");
	}
}

/**
 * @brief
 */
static void R_EndDrawFromSurfaceType_default(const r_bsp_surface_type_t type) {

	switch (type) {
	case R_SURFTYPE_OPAQUE:
		R_EndDrawOpaqueBspSurfaces_default();
		break;
	case R_SURFTYPE_WARP:
		R_EndDrawWarpBspSurfaces_default();
		break;
	case R_SURFTYPE_ALPHA:
		R_EndDrawAlphaTestBspSurfaces_default();
		break;
	default:
		Com_Error(ERROR_DROP, "This type of surface should not be batched\n");
	}
}

/**
 * @brief
 */
void R_DrawBatchedOpaqueBspSurfaces(void) {
	
	R_Color(NULL);

	if (r_draw_wireframe->value) { // surface outlines
		//R_DrawBspSurfacesLines_default(surfs);
		return;
	}

	R_EnableTexture(texunit_diffuse, true);

	R_SetArrayState(r_model_state.world);
	
	R_BindAttributeBuffer(R_ATTRIB_ELEMENTS, &r_model_state.world->bsp->visible_element_buffer);
	
	const r_bsp_surface_batch_t *batch = (const r_bsp_surface_batch_t * ) r_model_state.world->bsp->surface_batches->data;
	r_bsp_surface_type_t type = (r_bsp_surface_type_t) -1;

	for (size_t i = 0; i < r_model_state.world->bsp->surface_batches->len; i++, batch++)
	{
		const r_bsp_surface_t *surf = batch->surf;

		if (surf->surftype == R_SURFTYPE_BLEND ||
			surf->surftype == R_SURFTYPE_BLENDWARP ||
			surf->surftype == R_SURFTYPE_SKY) {
			continue;
		}

		type = surf->surftype;

		R_BeginDrawFromSurfaceType_default(surf->surftype);
		
		R_SetBspSurfaceState_default(surf);

		R_DrawArrays(GL_TRIANGLE_FAN, batch->start, batch->count);
		
		R_EndDrawFromSurfaceType_default(surf->surftype);
	}
	
	R_BindAttributeBuffer(R_ATTRIB_ELEMENTS, &r_model_state.world->bsp->element_buffer);

	R_ResetBspSurfacesState_default();

	R_EndDrawFromSurfaceType_default(type);
}

/**
 * @brief
 */
void R_DrawOpaqueBspSurfaces_default(const r_bsp_surfaces_t *surfs) {

	if (!surfs->count) {
		return;
	}

	if (r_draw_wireframe->value) { // surface outlines
		R_DrawBspSurfacesLines_default(surfs);
		return;
	}

	R_BeginDrawOpaqueBspSurfaces_default();

	R_DrawBspSurfaces_default(surfs);

	R_EndDrawOpaqueBspSurfaces_default();
}

/**
 * @brief
 */
void R_DrawOpaqueWarpBspSurfaces_default(const r_bsp_surfaces_t *surfs) {

	if (!surfs->count) {
		return;
	}

	if (r_draw_wireframe->value) { // surface outlines
		R_DrawBspSurfacesLines_default(surfs);
		return;
	}

	R_BeginDrawWarpBspSurfaces_default();

	R_DrawBspSurfaces_default(surfs);

	R_EndDrawWarpBspSurfaces_default();
}

/**
 * @brief
 */
void R_DrawAlphaTestBspSurfaces_default(const r_bsp_surfaces_t *surfs) {

	if (!surfs->count) {
		return;
	}

	if (r_draw_wireframe->value) { // surface outlines
		R_DrawBspSurfacesLines_default(surfs);
		return;
	}

	R_BeginDrawAlphaTestBspSurfaces_default();

	R_DrawBspSurfaces_default(surfs);

	R_EndDrawAlphaTestBspSurfaces_default();
}

/**
 * @brief
 */
void R_DrawBlendBspSurfaces_default(const r_bsp_surfaces_t *surfs) {

	if (!surfs->count) {
		return;
	}

	if (r_draw_wireframe->value) { // surface outlines
		R_DrawBspSurfacesLines_default(surfs);
		return;
	}

	R_BeginDrawBlendBspSurfaces_default();

	R_DrawBspSurfaces_default(surfs);

	R_EndDrawBlendBspSurfaces_default();
}

/**
 * @brief
 */
void R_DrawBlendWarpBspSurfaces_default(const r_bsp_surfaces_t *surfs) {

	if (!surfs->count) {
		return;
	}

	if (r_draw_wireframe->value) { // surface outlines
		R_DrawBspSurfacesLines_default(surfs);
		return;
	}

	R_BeginDrawWarpBspSurfaces_default();

	R_DrawBspSurfaces_default(surfs);
	
	R_EndDrawWarpBspSurfaces_default();
}

/**
 * @brief
 */
void R_DrawBackBspSurfaces_default(const r_bsp_surfaces_t *surfs) {
	// no-op
}
