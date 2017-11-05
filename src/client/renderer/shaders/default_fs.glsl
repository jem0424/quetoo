/**
 * @brief Default fragment shader.
 */

#version 330

precision lowp int;
precision lowp float;

#define FRAGMENT_SHADER

#include "include/uniforms.glsl"
#include "include/fog.glsl"
#include "include/noise3d.glsl"
#include "include/tint.glsl"
#include "include/gamma.glsl"
#include "include/hdr.glsl"

#define MAX_LIGHTS $r_max_lights
#define LIGHT_SCALE $r_modulate

#if MAX_LIGHTS
struct LightParameters {
	vec3 ORIGIN[MAX_LIGHTS];
	vec3 COLOR[MAX_LIGHTS];
	float RADIUS[MAX_LIGHTS];
};

uniform LightParameters LIGHTS;
#endif

struct CausticParameters {
	bool ENABLE;
	vec3 COLOR;
};

uniform CausticParameters CAUSTIC;

uniform bool DIFFUSE;
uniform bool LIGHTMAP;
uniform bool DELUXEMAP;
uniform bool NORMALMAP;
uniform bool GLOSSMAP;
uniform bool STAINMAP;

uniform float BUMP;
uniform float PARALLAX;
uniform float HARDNESS;
uniform float SPECULAR;

uniform sampler2D SAMPLER0;
uniform sampler2D SAMPLER1;
uniform sampler2D SAMPLER2;
uniform sampler2D SAMPLER3;
uniform sampler2D SAMPLER4;
uniform sampler2D SAMPLER8;

#define tex_diffuse  SAMPLER0
#define tex_light    SAMPLER1
#define tex_normal   SAMPLER3
#define tex_deluxe   SAMPLER2
#define tex_specular SAMPLER4
#define tex_stain    SAMPLER8

uniform float ALPHA_THRESHOLD;

in VertexData {
	vec3 vtx_modelpoint;
	vec2 vtx_texcoords[2];
	vec4 vtx_color;
	vec3 vtx_point;
	vec3 vtx_normal;
	vec3 vtx_tangent;
	vec3 vtx_bitangent;
	vec3 vtx_eye;
};

#define uv_materials vtx_texcoords[0]
#define uv_lightmap  vtx_texcoords[1]

out vec4 fragColor;

vec3 eyeDir = normalize(vtx_eye);

/**
 * @brief Yield the diffuse modulation from bump-mapping.
 */
void BumpFragment(in vec3 deluxemap, in vec3 normalmap, in vec3 glossmap, out float lightmapBumpScale, out float lightmapSpecularScale) {
	float glossFactor = clamp(dot(glossmap, vec3(0.299, 0.587, 0.114)), 0.0078125, 1.0);

	lightmapBumpScale = clamp(dot(deluxemap, normalmap), 0.0, 1.0);
	lightmapSpecularScale = (HARDNESS * glossFactor) * pow(clamp(-dot(eyeDir, reflect(deluxemap, normalmap)), 0.0078125, 1.0), (16.0 * glossFactor) * SPECULAR);
}

/**
 * @brief Yield the final sample color after factoring in dynamic light sources.
 */
void LightFragment(in vec4 diffuse, in vec3 lightmap, in vec3 normal, in float lightmapBumpScale, in float lightmapSpecularScale) {

	vec3 light = vec3(0.0);

#if MAX_LIGHTS
	/*
	 * Iterate the hardware light sources, accumulating dynamic lighting for
	 * this fragment. A light radius of 0.0 means break.
	 */
	for (int i = 0; i < MAX_LIGHTS; i++) {

		if (LIGHTS.RADIUS[i] == 0.0) { break; }

		vec3 delta = LIGHTS.ORIGIN[i] - vtx_point;
		float dist = length(delta);

		// Don't shade points outside of the radius.
		if (dist > LIGHTS.RADIUS[i]) { continue; }

		float NdotL = dot(normal, normalize(delta));

		// Don't shade normals turned away from the light.
		if (NdotL <= 0.0) { continue; }

		dist = 1.0 - dist / LIGHTS.RADIUS[i];
		light += LIGHTS.COLOR[i] * LIGHT_SCALE * NdotL * dist * dist;

	}
#endif

	// now modulate the diffuse sample with the modified lightmap
	float lightmapLuma = dot(lightmap.rgb, vec3(0.299, 0.587, 0.114));

	float blackPointLuma = 0.015625;
	float l = exp2(lightmapLuma) - blackPointLuma;
	float lightmapDiffuseBumpedLuma = l * lightmapBumpScale;
	float lightmapSpecularBumpedLuma = l * lightmapSpecularScale;

	vec3 diffuseLightmapColor = lightmap.rgb * lightmapDiffuseBumpedLuma;
	vec3 specularLightmapColor = (lightmapLuma + lightmap.rgb) * 0.5 * lightmapSpecularBumpedLuma;

	fragColor.rgb = diffuse.rgb * ((diffuseLightmapColor + specularLightmapColor) * LIGHT_SCALE + light);

	// lastly modulate the alpha channel by the color
	fragColor.a = diffuse.a * vtx_color.a;
}

/**
 * @brief Shader entry point.
 */
void main(void) {

	vec4 diffuse = vec4(1.0);

	if (DIFFUSE) {
		diffuse = texture(tex_diffuse, uv_materials);

		// see if diffuse can be discarded because of alpha test
		if (diffuse.a < ALPHA_THRESHOLD)
			discard;

		TintFragment(diffuse, uv_materials);
	}

	// first resolve the flat shading
	vec3 lightmap = vtx_color.rgb;

	if (LIGHTMAP) {
		lightmap = texture(tex_light, uv_lightmap).rgb;

		if (STAINMAP) {
			vec4 stain = texture(tex_stain, uv_lightmap);
			lightmap = mix(lightmap.rgb, stain.rgb, stain.a).rgb;
		}
	}

	// then resolve any bump mapping
	vec3 normal = vtx_normal;
	
	float lightmapBumpScale = 1.0;
	float lightmapSpecularScale = 0.0;

	if (NORMALMAP) {

		vec3 deluxemap = DELUXEMAP ? texture(tex_deluxe, uv_lightmap).rgb : vec3(0.0, 0.0, 1.0);
		deluxemap = normalize((deluxemap - 0.5) * 2.0);

		// resolve the initial normalmap sample
		normal = texture(tex_normal, uv_materials).xyz;
		normal = (normal - 0.5) * 2.0;
		normal.xy *= BUMP;
		normal = normalize(normal);

		vec3 glossmap = GLOSSMAP ? texture(tex_specular, uv_materials).rgb : vec3(0.5);

		// resolve the bumpmap modulation
		BumpFragment(deluxemap, normal, glossmap, lightmapBumpScale, lightmapSpecularScale);

		// and then transform the normalmap to model space for lighting
		normal = normalize(
			normal.x * normalize(vtx_tangent) +
			normal.y * normalize(vtx_bitangent) +
			normal.z * normalize(vtx_normal)
		);
	}

	// add any dynamic lighting to yield the final fragment color
	LightFragment(diffuse, lightmap, normal, lightmapBumpScale, lightmapSpecularScale);

	fragColor.rgb = tonemap_ldr(fragColor.rgb);

	// and fog
	FogFragment(length(vtx_point), fragColor);
}
