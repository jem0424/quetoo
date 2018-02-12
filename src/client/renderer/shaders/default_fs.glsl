/**
 * @brief Default fragment shader.
 */

#version 330

#define FRAGMENT_SHADER

#include "include/uniforms.glsl"
#include "include/fog.glsl"
#include "include/noise3d.glsl"
#include "include/tint.glsl"

#define MAX_LIGHTS $r_max_lights
#define LIGHT_SCALE $r_modulate
#define DEBUG_LIGHTMAP_LAYER_INDEX $r_draw_bsp_lightmaps

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
uniform sampler2DArray SAMPLER1;
uniform sampler2D SAMPLER2;
uniform sampler2D SAMPLER3;
uniform sampler2D SAMPLER4;
uniform sampler2D SAMPLER8;

uniform float ALPHA_THRESHOLD;

in VertexData {
	vec3 modelpoint;
	vec2 texcoords[2];
	vec4 color;
	vec3 point;
	vec3 normal;
	vec3 tangent;
	vec3 bitangent;
	vec3 eye;
};

const vec3 two = vec3(2.0);
const vec3 negHalf = vec3(-0.5);

vec3 eyeDir;

out vec4 fragColor;

/**
 * @brief Yield the parallax offset for the texture coordinate.
 */
vec2 BumpTexcoord(in float height) {
	return vec2(height * 0.04 - 0.02) * PARALLAX * eyeDir.xy;
}

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
void LightFragment(in vec4 diffuse, in vec3 lightmap, in vec3 normalmap, in float lightmapBumpScale, in float lightmapSpecularScale) {

	vec3 light = vec3(0.0);

#if MAX_LIGHTS
	/*
	 * Iterate the hardware light sources, accumulating dynamic lighting for
	 * this fragment. A light radius of 0.0 means break.
	 */
	for (int i = 0; i < MAX_LIGHTS; i++) {

		if (LIGHTS.RADIUS[i] == 0.0)
			break;

		vec3 delta = LIGHTS.ORIGIN[i] - point;
		float len = length(delta);

		if (len < LIGHTS.RADIUS[i]) {

			float lambert = dot(normalmap, normalize(delta));
			if (lambert > 0.0) {

				// windowed inverse square falloff
				float dist = len/LIGHTS.RADIUS[i];
				float falloff = clamp(1.0 - dist * dist * dist * dist, 0.0, 1.0);
				falloff = falloff * falloff;
				falloff = falloff / (dist * dist + 1.0);

				light += LIGHTS.COLOR[i] * falloff * lambert;
			}
		}
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

	fragColor.rgb = diffuse.rgb * ((diffuseLightmapColor + specularLightmapColor) + light);

	// lastly modulate the alpha channel by the color
	fragColor.a = diffuse.a * color.a;
}

/**
 * @brief Render caustics
 */
void CausticFragment(in vec3 lightmap) {
	if (CAUSTIC.ENABLE) {
		vec3 model_scale = vec3(0.024, 0.024, 0.016);
		float time_scale = 0.0006;
		float caustic_thickness = 0.02;
		float caustic_glow = 8.0;
		float caustic_intensity = 0.3;

		// grab raw 3d noise
		float factor = noise3d((modelpoint * model_scale) + (TIME * time_scale));

		// scale to make very close to -1.0 to 1.0 based on observational data
		factor = factor * (0.3515 * 2.0);

		// make the inner edges stronger, clamp to 0-1
		factor = clamp(pow((1 - abs(factor)) + caustic_thickness, caustic_glow), 0, 1);

		// start off with simple color * 0-1
		vec3 caustic_out = CAUSTIC.COLOR * factor * caustic_intensity;

		// multiply caustic color by lightmap, clamping so it doesn't go pure black
		caustic_out *= clamp((lightmap * 1.6) - 0.5, 0.1, 1.0);

		// add it up
		fragColor.rgb = clamp(fragColor.rgb + caustic_out, 0.0, 1.0);
	}
}

#define M_PI 3.1415926535897932384626433832795

struct SG {
    vec3 amplitude;
    vec3 axis;
    float sharpness;
};

struct ASG {
    vec3 amplitude;
    vec3 basisZ;
    vec3 basisX;
    vec3 basisY;
    float sharpnessX;
    float sharpnessY;
};

vec3 EvaluateSG(in SG sg, in vec3 dir) {
    return sg.amplitude * exp(sg.sharpness * (dot(dir, sg.axis) - 1.0));
}

vec3 EvaluateASG(in ASG asg, in vec3 dir) {
    float smoothTerm = clamp(dot(asg.basisZ, dir), 0.0, 1.0);
    float lambdaTerm = asg.sharpnessX * dot(dir, asg.basisX) * dot(dir, asg.basisX);
    float muTerm = asg.sharpnessY * dot(dir, asg.basisY) * dot(dir, asg.basisY);

    return asg.amplitude * smoothTerm * exp(-lambdaTerm - muTerm);
}

SG CosineLobeSG(in vec3 direction) {
    SG cosineLobe;

    cosineLobe.amplitude = vec3(1.17);
    cosineLobe.axis = direction;
    cosineLobe.sharpness = 2.133;

    return cosineLobe;
}

vec3 SGInnerProduct(in SG x, in SG y) {
    float umLength = length(x.sharpness * x.axis + y.sharpness * y.axis);
    vec3 expo = exp(umLength - x.sharpness - y.sharpness) * x.amplitude * y.amplitude;
    float other = 1.0 - exp(-2.0 * umLength);

    return (2.0 * M_PI * expo * other) / umLength;
}

vec3 SGIrradianceInnerProduct(in SG lightingLobe, in vec3 normal) {
    SG cosineLobe = CosineLobeSG(normal);
    return max(SGInnerProduct(lightingLobe, cosineLobe), 0.0);
}

SG DistributionTermSG(in vec3 direction, in float roughness) {
    SG distribution;
    distribution.axis = direction;

    float m2 = roughness * roughness;
    distribution.sharpness = 2 / m2;
    distribution.amplitude = vec3(1.0 / (M_PI * m2));

    return distribution;
}

SG WarpDistributionSG(in SG ndf, in vec3 view) {
    SG warp;

    warp.axis = reflect(-view, ndf.axis);
    warp.amplitude = ndf.amplitude;
    warp.sharpness = ndf.sharpness / (4.0 * max(dot(ndf.axis, view), 0.1));

    return warp;
}

float GGX_V1(in float m2, in float nDotX) {
    return 1.0 / (nDotX + sqrt(m2 + (1.0 - m2) * nDotX * nDotX));
}

vec3 SpecularTermSGWarp(in SG light, in vec3 normal, in float roughness, in vec3 view, in vec3 specAlbedo) {
    SG ndf = DistributionTermSG(normal, roughness);
    SG warpedNDF = WarpDistributionSG(ndf, view);

    vec3 result = SGInnerProduct(warpedNDF, light);

    float m2 = roughness * roughness;
    float nDotL = clamp(dot(normal, warpedNDF.axis), 0.0, 1.0);
    float nDotV = clamp(dot(normal, view), 0.0, 1.0);
    vec3 h = normalize(warpedNDF.axis + view);

    result *= GGX_V1(m2, nDotL) * GGX_V1(m2, nDotV);
    result *= specAlbedo + (1.0 - specAlbedo) * pow(1.0 - clamp(dot(warpedNDF.axis, h), 0.0, 1.0), 5.0);
    result *= clamp(dot(specAlbedo, vec3(333.0)), 0.0, 1.0);
    result *= nDotL;

    return max(result, 0.0);
}

ASG WarpDistributionASG(in SG ndf, in vec3 view) {
    ASG warp;

    warp.basisZ = reflect(-view, ndf.axis);
    warp.basisX = normalize(cross(ndf.axis, warp.basisZ));
    warp.basisY = normalize(cross(warp.basisZ, warp.basisX));

    float dotdiro = max(dot(view, ndf.axis), 0.1);

    warp.sharpnessX = ndf.sharpness / (8.0 * dotdiro * dotdiro);
    warp.sharpnessY = ndf.sharpness / 8.0;

    warp.amplitude = ndf.amplitude;

    return warp;
}

vec3 ConvolveASG_SG(in ASG asg, in SG sg) {
    float nu = sg.sharpness * 0.5f;

    ASG convolveASG;
    convolveASG.basisX = asg.basisX;
    convolveASG.basisY = asg.basisY;
    convolveASG.basisZ = asg.basisZ;

    convolveASG.sharpnessX = (nu * asg.sharpnessX) / (nu + asg.sharpnessX);
    convolveASG.sharpnessY = (nu * asg.sharpnessY) / (nu + asg.sharpnessY);

    convolveASG.amplitude = vec3(M_PI / sqrt((nu + asg.sharpnessX) * (nu + asg.sharpnessY)));

    return EvaluateASG(convolveASG, sg.axis) * sg.amplitude * asg.amplitude;
}

vec3 SpecularTermASGWarp(in SG light, in vec3 normal, in float roughness, in vec3 view, in vec3 specAlbedo) {
    SG ndf = DistributionTermSG(normal, roughness);
    ASG warpedNDF = WarpDistributionASG(ndf, view);

    vec3 result = ConvolveASG_SG(warpedNDF, light);

    float m2 = roughness * roughness;
    float nDotL = clamp(dot(normal, warpedNDF.basisZ), 0.0, 1.0);
    float nDotV = clamp(dot(normal, view), 0.0, 1.0);
    vec3 h = normalize(warpedNDF.basisZ + view);

    result *= GGX_V1(m2, nDotL) * GGX_V1(m2, nDotV);
    result *= specAlbedo + (1.0 - specAlbedo) * pow(1.0 - clamp(dot(warpedNDF.basisZ, h), 0.0, 1.0), 5.0);
    result *= clamp(dot(specAlbedo, vec3(333.0)), 0.0, 1.0);
    result *= nDotL;

    return max(result, 0.0);
}

/**
 * @brief Shader entry point.
 */
void main(void) {
	vec4 normalmap = vec4(normal, 1.0);

	if (NORMALMAP) {
		// resolve the initial normalmap sample
		normalmap = texture(SAMPLER3, texcoords[0]);

		normalmap.xyz = normalize(two * (normalmap.xyz + negHalf));
		normalmap.xyz = normalize(vec3(normalmap.x * BUMP, normalmap.y * BUMP, normalmap.z));
	}

	eyeDir = normalize(eye);

	vec4 diffuse = texture(SAMPLER0, texcoords[0]);
	float processedGrayscaleDiffuse = dot(diffuse.rgb * diffuse.a, vec3(0.299, 0.587, 0.114)) * 0.875 + 0.125;
	float guessedGlossValue = clamp(pow(processedGrayscaleDiffuse * 3.0, 4.0), 0.0, 1.0) * 0.875 + 0.125;

	vec3 axisData[32] = vec3[32](
		vec3(0.169358, 0.985431, 0.015625),
		vec3(-0.789756, -0.611628, 0.046875),
		vec3(0.993540, -0.082315, 0.078125),
		vec3(-0.675003, 0.729663, 0.109375),
		vec3(0.004828, -0.990051, 0.140625),
		vec3(0.661888, 0.729632, 0.171875),
		vec3(-0.974975, -0.090359, 0.203125),
		vec3(0.774371, -0.587722, 0.234375),
		vec3(-0.172552, 0.948509, 0.265625),
		vec3(-0.508595, -0.808206, 0.296875),
		vec3(0.911041, 0.249677, 0.328125),
		vec3(-0.830249, 0.426071, 0.359375),
		vec3(0.319998, -0.863142, 0.390625),
		vec3(0.341848, 0.839739, 0.421875),
		vec3(-0.805562, -0.381770, 0.453125),
		vec3(0.836028, -0.257757, 0.484375),
		vec3(-0.433219, 0.739224, 0.515625),
		vec3(-0.175782, -0.818553, 0.546875),
		vec3(0.665203, 0.472521, 0.578125),
		vec3(-0.786795, 0.098063, 0.609375),
		vec3(0.497695, -0.584722, 0.640625),
		vec3(0.026998, 0.740172, 0.671875),
		vec3(-0.499111, -0.506462, 0.703125),
		vec3(0.677859, 0.034655, 0.734375),
		vec3(-0.495907, 0.409749, 0.765625),
		vec3(0.083477, -0.598349, 0.796875),
		vec3(0.317899, 0.461681, 0.828125),
		vec3(-0.498325, -0.114660, 0.859375),
		vec3(0.395649, -0.224162, 0.890625),
		vec3(-0.119568, 0.368578, 0.921875),
		vec3(-0.125568, -0.275292, 0.953125),
		vec3(0.162100, 0.068770, 0.984375)
	);

	float sharpness = 21.657574;

	SG sg;

	vec3 lightmap = vec3(0.0);
	vec3 lightmapSpecular = vec3(0.0);

	vec4 lightmapColorHDR;

	for (int i = 0; i < 32; i++) {
		lightmapColorHDR = texture(SAMPLER1, vec3(texcoords[1], i));

		sg.amplitude = lightmapColorHDR.rgb * lightmapColorHDR.a;
		sg.axis = axisData[i];
		sg.sharpness = sharpness;

		lightmap += SGIrradianceInnerProduct(sg, normalmap.xyz) * 4.0;
		lightmapSpecular += SpecularTermASGWarp(sg, normalmap.xyz, (1.0 - guessedGlossValue) * 0.25, eyeDir, vec3(guessedGlossValue) * 2.0) * 8.0;
	}

	// vec3 lightmapSpecular = (HARDNESS * guessedGlossValue) * pow(clamp(dot(eyeDir, reflect(-eyeDirMod, normalmap.xyz)), 0.0078125, 1.0), (16.0 * guessedGlossValue) * SPECULAR) * EvaluateH4Color(normalize(reflect(-eyeDirMod, normalmap.xyz)), h0, h1, h2, h3);

	fragColor.rgb = (lightmap + lightmapSpecular + vec3(0.125)) * diffuse.rgb;
	fragColor.a = diffuse.a;

	/*
	// first resolve the flat shading
	vec3 lightmap = color.rgb;
	vec3 deluxemap = vec3(0.0, 0.0, 1.0);

	if (LIGHTMAP) {
#if DEBUG_LIGHTMAP_LAYER_INDEX > 0
		vec4 lightmapColorHDR = texture(SAMPLER1, vec3(texcoords[1], DEBUG_LIGHTMAP_LAYER_INDEX - 1));
#else
		vec4 lightmapColorHDR = texture(SAMPLER1, vec3(texcoords[1], 0));
#endif // DEBUG_LIGHTMAP_LAYER_INDEX
		lightmap = lightmapColorHDR.rgb * lightmapColorHDR.a;

		if (STAINMAP) {
			vec4 stain = texture(SAMPLER8, texcoords[1]);
			lightmap = mix(lightmap.rgb, stain.rgb, stain.a).rgb;
		}
	}

	// then resolve any bump mapping
	vec4 normalmap = vec4(normal, 1.0);
	vec2 parallax = vec2(0.0);
	
	float lightmapBumpScale = 1.0;
	float lightmapSpecularScale = 0.0;

	if (NORMALMAP) {
		eyeDir = normalize(eye);

		if (DELUXEMAP) {
			vec4 deluxeColorHDR = texture(SAMPLER1, vec3(texcoords[1], 1));
			deluxemap = deluxeColorHDR.rgb * deluxeColorHDR.a;

			deluxemap = normalize(two * (deluxemap + negHalf));
		}

		// resolve the initial normalmap sample
		normalmap = texture(SAMPLER3, texcoords[0]);

		// resolve the parallax offset from the heightmap
		parallax = BumpTexcoord(normalmap.w);

		// resample the normalmap at the parallax offset
		normalmap = texture(SAMPLER3, texcoords[0] + parallax);

		normalmap.xyz = normalize(two * (normalmap.xyz + negHalf));
		normalmap.xyz = normalize(vec3(normalmap.x * BUMP, normalmap.y * BUMP, normalmap.z));

		vec3 glossmap = vec3(0.5);

		if (GLOSSMAP) {
			glossmap = texture(SAMPLER4, texcoords[0] + parallax).rgb;
		} else if (DIFFUSE) {
			vec4 diffuse = texture(SAMPLER0, texcoords[0] + parallax);
			float processedGrayscaleDiffuse = dot(diffuse.rgb * diffuse.a, vec3(0.299, 0.587, 0.114)) * 0.875 + 0.125;
			float guessedGlossValue = clamp(pow(processedGrayscaleDiffuse * 3.0, 4.0), 0.0, 1.0) * 0.875 + 0.125;

			glossmap = vec3(guessedGlossValue);
		}

		// resolve the bumpmap modulation
		BumpFragment(deluxemap, normalmap.xyz, glossmap, lightmapBumpScale, lightmapSpecularScale);

		// and then transform the normalmap to model space for lighting
		normalmap.xyz = normalize(
			normalmap.x * normalize(tangent) +
			normalmap.y * normalize(bitangent) +
			normalmap.z * normalize(normal)
		);
	}

	vec4 diffuse = vec4(1.0);

	if (DIFFUSE) { // sample the diffuse texture, honoring the parallax offset
		diffuse = texture(SAMPLER0, texcoords[0] + parallax);

		// see if diffuse can be discarded because of alpha test
		if (diffuse.a < ALPHA_THRESHOLD)
			discard;

		TintFragment(diffuse, texcoords[0] + parallax);
	}

	// add any dynamic lighting to yield the final fragment color
	LightFragment(diffuse, lightmap, normalmap.xyz, lightmapBumpScale, lightmapSpecularScale);

	// underliquid caustics
	CausticFragment(lightmap);

#if DEBUG_LIGHTMAP_LAYER_INDEX == 0
	// tonemap
	fragColor.rgb *= exp(fragColor.rgb);
	fragColor.rgb /= fragColor.rgb + 0.825;

	// apply lightscale afterwards, because it should be done AFTER tonemapping
	fragColor.rgb *= LIGHT_SCALE;
#endif // DEBUG_LIGHTMAP_LAYER_INDEX == 0
	*/

	fragColor.rgb *= LIGHT_SCALE;

	// and fog
	FogFragment(length(point), fragColor);
}
