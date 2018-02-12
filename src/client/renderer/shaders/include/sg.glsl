#ifndef SG_HLSL
#define SG_HLSL

#include "include/constants.glsl"

// SphericalGaussian(dir) := amplitude * exp(sharpness * (dot(axis, dir) - 1.0f))
struct SG
{
    float3 amplitude;
    float3 axis;
    float sharpness;
};

// AnisotropicSphericalGaussian(dir) :=
// amplitude * exp(-sharpnessX * dot(basisX, dir)^2 - sharpnessY * dot(basisY, dir)^2)
struct ASG
{
    float3 amplitude;
    float3 basisX;              // Direction the ASG points
    float3 basisY;
    float3 basisZ;
    float sharpnessX;           // Scale of the X axis
    float sharpnessY;           // Scale of the Y axis
};

//-------------------------------------------------------------------------------------------------
// Helper for computing the GGX visibility term
//-------------------------------------------------------------------------------------------------
float GGX_V1(in float m2, in float nDotX)
{
    return 1.0f / (nDotX + sqrt(m2 + (1.0f - m2) * nDotX * nDotX));
}

//-------------------------------------------------------------------------------------------------
// Computes the GGX visibility term
//-------------------------------------------------------------------------------------------------
float GGXVisibility(in float m2, in float nDotL, in float nDotV)
{
    return GGX_V1(m2, nDotL) * GGX_V1(m2, nDotV);
}

//-------------------------------------------------------------------------------------------------
// Computes the specular term using a GGX microfacet distribution, with a matching
// geometry factor and visibility term. Based on "Microfacet Models for Refraction Through
// Rough Surfaces" [Walter 07]. m is roughness, n is the surface normal, h is the half vector,
// l is the direction to the light source, and specAlbedo is the RGB specular albedo
//-------------------------------------------------------------------------------------------------
float GGX_Specular(in float m, in float3 n, in float3 h, in float3 v, in float3 l)
{
    float nDotH = saturate(dot(n, h));
    float nDotL = saturate(dot(n, l));
    float nDotV = saturate(dot(n, v));

    float nDotH2 = nDotH * nDotH;
    float m2 = m * m;

    // Calculate the distribution term
    float d = m2 / (PI * pow(nDotH * nDotH * (m2 - 1.0f) + 1.0f, 2.0f));

    // Calculate the matching visibility term
    float v1i = GGX_V1(m2, nDotL);
    float v1o = GGX_V1(m2, nDotV);
    float vis = v1i * v1o;

    return d * GGXVisibility(m2, nDotL, nDotV);
}

float Phong_Specular(in float shininess, in float3 n, in float3 h, in float3 v, in float3 l)
{
    float3 r = reflect(-l, n);
    float specularAngle = saturate(dot(r, v));

    return pow(specularAngle, shininess * 16.0f) * (shininess * 4.0f);
}

float BlinnPhong_Specular(in float shininess, in float3 n, in float3 h, in float3 v, in float3 l)
{
    float specularAngle = saturate(dot(h, n));

    return pow(specularAngle, shininess * 16.0f) * (shininess * 4.0f);
}

//-------------------------------------------------------------------------------------------------
// Calculates the Fresnel factor using Schlick's approximation
//-------------------------------------------------------------------------------------------------
float3 Fresnel(in float3 specAlbedo, in float3 h, in float3 l)
{
    float3 fresnel = specAlbedo + (1.0f - specAlbedo) * pow((1.0f - saturate(dot(l, h))), 5.0f);

    // Fade out spec entirely when lower than 0.1% albedo
    fresnel *= saturate(dot(specAlbedo, float3(333.0f, 333.0f, 333.0f)));

    return fresnel;
}

//-------------------------------------------------------------------------------------------------
// Evaluates an SG given a direction on a unit sphere
//-------------------------------------------------------------------------------------------------
float3 EvaluateSG(in SG sg, in float3 dir)
{
    return sg.amplitude * exp(sg.sharpness * (dot(dir, sg.axis) - 1.0f));
}

//-------------------------------------------------------------------------------------------------
// Evaluates an ASG given a direction on a unit sphere
//-------------------------------------------------------------------------------------------------
float3 EvaluateASG(in ASG asg, in float3 dir)
{
    float smoothTerm = saturate(dot(asg.basisZ, dir));
    float lambdaTerm = asg.sharpnessX * dot(dir, asg.basisX) * dot(dir, asg.basisX);
    float muTerm = asg.sharpnessY * dot(dir, asg.basisY) * dot(dir, asg.basisY);
    return asg.amplitude * smoothTerm * exp(-lambdaTerm - muTerm);
}

//-------------------------------------------------------------------------------------------------
// Computes the vector product of two SG's, which produces a new SG. If the new SG is evaluated,
// with a direction 'v' the result is equal to SGx(v) * SGy(v).
//-------------------------------------------------------------------------------------------------
SG SGProduct(in SG x, in SG y)
{
    float3 um = (x.sharpness * x.axis + y.sharpness * y.axis) / (x.sharpness + y.sharpness);
    float umLength = length(um);
    float lm = x.sharpness + y.sharpness;

    SG res;
    res.axis = um * (1.0f / umLength);
    res.sharpness = lm * umLength;
    res.amplitude = x.amplitude * y.amplitude * exp(lm * (umLength - 1.0f));

    return res;
}

//-------------------------------------------------------------------------------------------------
// Computes the integral of an SG over the entire sphere
//-------------------------------------------------------------------------------------------------
float3 SGIntegral(in SG sg)
{
    float expTerm = 1.0f - exp(-2.0f * sg.sharpness);
    return PI2 * (sg.amplitude / sg.sharpness) * expTerm;
}

//-------------------------------------------------------------------------------------------------
// Computes the approximate integral of an SG over the entire sphere. The error vs. the
// non-approximate version decreases as sharpeness increases.
//-------------------------------------------------------------------------------------------------
float3 ApproximateSGIntegral(in SG sg)
{
    return PI2 * (sg.amplitude / sg.sharpness);
}

//-------------------------------------------------------------------------------------------------
// Computes the inner product of two SG's, which is equal to Integrate(SGx(v) * SGy(v) * dv).
//-------------------------------------------------------------------------------------------------
float3 SGInnerProduct(in SG x, in SG y)
{
    float umLength = length(x.sharpness * x.axis + y.sharpness * y.axis);
    float3 expo = exp(umLength - x.sharpness - y.sharpness) * x.amplitude * y.amplitude;
    float other = 1.0f - exp(-2.0f * umLength);
    return (PI2 * expo * other) / umLength;
}

//-------------------------------------------------------------------------------------------------
// Convolve an SG with an ASG
//-------------------------------------------------------------------------------------------------
float3 ConvolveASG_SG(in ASG asg, in SG sg) {
    // The ASG paper specifes an isotropic SG as exp(2 * nu * (dot(v, axis) - 1)),
    // so we must divide our SG sharpness by 2 in order to get the nup parameter expected by
    // the ASG formulas
    float nu = sg.sharpness * 0.5f;

    ASG convolveASG;
    convolveASG.basisX = asg.basisX;
    convolveASG.basisY = asg.basisY;
    convolveASG.basisZ = asg.basisZ;

    convolveASG.sharpnessX = (nu * asg.sharpnessX) / (nu + asg.sharpnessX);
    convolveASG.sharpnessY = (nu * asg.sharpnessY) / (nu + asg.sharpnessY);

    float amplitudeValue = PI / sqrt((nu + asg.sharpnessX) * (nu + asg.sharpnessY));
    convolveASG.amplitude = float3(amplitudeValue, amplitudeValue, amplitudeValue);

    return EvaluateASG(convolveASG, sg.axis) * sg.amplitude * asg.amplitude;
}

//-------------------------------------------------------------------------------------------------
// Returns an approximation of the clamped cosine lobe represented as an SG
//-------------------------------------------------------------------------------------------------
SG CosineLobeSG(in float3 direction)
{
    SG cosineLobe;
    cosineLobe.axis = direction;
    cosineLobe.sharpness = 2.133f;
    cosineLobe.amplitude = float3(1.17f, 1.17f, 1.17f);

    return cosineLobe;
}

//-------------------------------------------------------------------------------------------------
// Returns an SG approximation of the GGX NDF used in the specular BRDF. For a single-lobe
// approximation, the resulting NDF actually more closely resembles a Beckmann NDF.
//-------------------------------------------------------------------------------------------------
SG DistributionTermSG(in float3 direction, in float roughness)
{
    SG distribution;
    distribution.axis = direction;

    float m2 = roughness * roughness;
    distribution.sharpness = 2.0f / m2;

    float amplitudeValue = 1.0f / (PI * m2);
    distribution.amplitude = float3(amplitudeValue, amplitudeValue, amplitudeValue);

    return distribution;
}

//-------------------------------------------------------------------------------------------------
// Computes the approximate incident irradiance from a single SG lobe containing incoming radiance.
// The clamped cosine lobe is approximated as an SG, and convolved with the incoming radiance
// lobe using an SG inner product
//-------------------------------------------------------------------------------------------------
float3 SGIrradianceInnerProduct(in SG lightingLobe, in float3 normal)
{
    SG cosineLobe = CosineLobeSG(normal);
    return max(SGInnerProduct(lightingLobe, cosineLobe), 0.0f);
}

//-------------------------------------------------------------------------------------------------
// Computes the approximate incident irradiance from a single SG lobe containing incoming radiance.
// The SG is treated as a punctual light, with intensity equal to the integral of the SG.
//-------------------------------------------------------------------------------------------------
float3 SGIrradiancePunctual(in SG lightingLobe, in float3 normal)
{
    float cosineTerm = saturate(dot(lightingLobe.axis, normal));
    return cosineTerm * PI2 * (lightingLobe.amplitude) / lightingLobe.sharpness;
}

//-------------------------------------------------------------------------------------------------
// Computes the approximate incident irradiance from a single SG lobe containing incoming radiance.
// The irradiance is computed using a fitted approximation polynomial. This approximation
// and its implementation were provided by Stephen Hill.
//-------------------------------------------------------------------------------------------------
float3 SGIrradianceFitted(in SG lightingLobe, in float3 normal)
{
    float muDotN = dot(lightingLobe.axis, normal);
    float lambda = lightingLobe.sharpness;

    float c0 = 0.36f;
    float c1 = 1.0f / (4.0f * c0);

    float eml  = exp(-lambda);
    float em2l = eml * eml;
    float rl   = rcp(lambda);

    float scale = 1.0f + 2.0f * em2l - rl;
    float bias  = (eml - em2l) * rl - em2l;

    float x  = sqrt(1.0f - scale);
    float x0 = c0 * muDotN;
    float x1 = c1 * x;

    float n = x0 + x1;

    float y = (abs(x0) <= x1) ? n * n / x : saturate(muDotN);

    float normalizedIrradiance = scale * y + bias;

    return normalizedIrradiance * ApproximateSGIntegral(lightingLobe);
}

//-------------------------------------------------------------------------------------------------
// Computes the outputgoing radiance from a single SG lobe containing incoming radiance, using
// a Lambertian diffuse BRDF.
//-------------------------------------------------------------------------------------------------
float3 SGDiffuseInnerProduct(in SG lightingLobe, in float3 normal, in float3 albedo)
{
    float3 brdf = albedo / PI;
    return SGIrradianceInnerProduct(lightingLobe, normal) * brdf;
}

//-------------------------------------------------------------------------------------------------
// Computes the outputgoing radiance from a single SG lobe containing incoming radiance, using
// a Lambertian diffuse BRDF.
//-------------------------------------------------------------------------------------------------
float3 SGDiffusePunctual(in SG lightingLobe, in float3 normal, in float3 albedo)
{
    float3 brdf = albedo / PI;
    return SGIrradiancePunctual(lightingLobe, normal) * brdf;
}

//-------------------------------------------------------------------------------------------------
// Computes the outputgoing radiance from a single SG lobe containing incoming radiance, using
// a Lambertian diffuse BRDF.
//-------------------------------------------------------------------------------------------------
float3 SGDiffuseFitted(in SG lightingLobe, in float3 normal, in float3 albedo)
{
    float3 brdf = albedo / PI;
    return SGIrradianceFitted(lightingLobe, normal) * brdf;
}

//-------------------------------------------------------------------------------------------------
// Generate an SG that best represents the NDF SG but with it's axis oriented in the direction
// of the current BRDF slice. This will allow easier integration, because the SG\ASG are both
// in the same domain. Uses the warping operator from Wang et al.
//-------------------------------------------------------------------------------------------------
SG WarpDistributionSG(in SG ndf, in float3 view)
{
    SG warp;

    warp.axis = reflect(-view, ndf.axis);
    warp.amplitude = ndf.amplitude;
    warp.sharpness = ndf.sharpness / (4.0f * max(dot(ndf.axis, view), 0.1f));

    return warp;
}

//-------------------------------------------------------------------------------------------------
// Generate an ASG that best represents the NDF SG but with it's axis oriented in the direction
// of the current BRDF slice. This will allow easier integration, because the SG\ASG are both
// in the same domain.
//
// The warped NDF can be represented better as an ASG, so following Kun Xu from
// 'Anisotropic Spherical Gaussians' we change the SG to an ASG because the distribution of
// an NDF stretches at grazing angles.
//-------------------------------------------------------------------------------------------------
ASG WarpDistributionASG(in SG ndf, in float3 view)
{
    ASG warp;

    // Generate any orthonormal basis with Z pointing in the direction of the reflected view vector
    warp.basisZ = reflect(-view, ndf.axis);
    warp.basisX = normalize(cross(ndf.axis, warp.basisZ));
    warp.basisY = normalize(cross(warp.basisZ, warp.basisX));

    float dotDir = max(dot(view, ndf.axis), 0.1f);

    // Second derivative of the sharpness with respect to how far we are from basis axis direction
    warp.sharpnessX = ndf.sharpness / (8.0f * dotDir * dotDir);
    warp.sharpnessY = ndf.sharpness / 8.0f;

    warp.amplitude = ndf.amplitude;

    return warp;
}

//-------------------------------------------------------------------------------------------------
// Computes the specular reflectance from a single SG lobe containing incoming radiance
//-------------------------------------------------------------------------------------------------
float3 SpecularTermSGWarp(in SG light, in float3 normal, in float roughness,
                          in float3 view, in float3 specAlbedo)
{
    // Create an SG that approximates the NDF. Note that a single SG lobe is a poor fit for
    // the GGX NDF, since the GGX distribution has a longer tail. A sum of 3 SG's can more
    // closely match the shape of a GGX distribution, but it would also increase the cost
    // computing specular by a factor of 3.
    SG ndf = DistributionTermSG(normal, roughness);

    // Apply a warpring operation that will bring the SG from the half-angle domain the the
    // the lighting domain. The resulting lobe is another SG.
    SG warpedNDF = WarpDistributionSG(ndf, view);

     // Convolve the NDF with the SG light
    float3 output = SGInnerProduct(warpedNDF, light);

    // Evaluating the visibility term
    // output *= GGX_Specular(roughness, normal, normalize(view + warpedNDF.axis), view, warpedNDF.axis);
    output *= Phong_Specular(roughness, normal, normalize(view + warpedNDF.axis), view, warpedNDF.axis);
    
    // Fresnel evaluated at the center of our warped BRDF lobe
    output *= Fresnel(specAlbedo, normalize(view + warpedNDF.axis), warpedNDF.axis);
   
    // Cosine term evaluated at the center of our warped BRDF lobe
    output *= saturate(dot(normal, warpedNDF.axis));

    return max(output, 0.0f);
}

//-------------------------------------------------------------------------------------------------
// Computes the specular reflectance from a single SG lobe containing incoming radiance
//-------------------------------------------------------------------------------------------------
float3 SpecularTermASGWarp(in SG light, in float3 normal, in float roughness,
                           in float3 view, in float3 specAlbedo)
{
    // Create an SG that approximates the NDF. Note that a single SG lobe is a poor fit for
    // the GGX NDF, since the GGX distribution has a longer tail. A sum of 3 SG's can more
    // closely match the shape of a GGX distribution, but it would also increase the cost
    // computing specular by a factor of 3.
    SG ndf = DistributionTermSG(normal, roughness);

    // Apply a warpring operation that will bring the SG from the half-angle domain the the
    // the lighting domain. The resulting lobe is an ASG that's stretched along the viewing
    // direction in order to better match the actual shape of a GGX distribution.
    ASG warpedNDF = WarpDistributionASG(ndf, view);

    // Convolve the NDF with the light. Note, this is a integration of the NDF which is an ASG
    // with the light which is a SG. See Kun Xu 'Anisotropic Spherical Gaussians' section 4.3
    // for more details
    float3 output = ConvolveASG_SG(warpedNDF, light);

    // Evaluating the visibility term
    // output *= GGX_Specular(roughness, normal, normalize(view + warpedNDF.basisZ), view, warpedNDF.basisZ);
    output *= Phong_Specular(1.0f - roughness, normal, normalize(view + warpedNDF.basisZ), view, warpedNDF.basisZ);
    
    // Fresnel evaluated at the center of our warped BRDF lobe
    output *= Fresnel(specAlbedo, normalize(view + warpedNDF.basisZ), warpedNDF.basisZ);
    
    // Cosine term evaluated at the center of our warped BRDF lobe
    output *= saturate(dot(normal, warpedNDF.basisZ));

    /*
    // Parameters needed for evaluating the visibility term
    float m2 = roughness * roughness;
    float nDotL = saturate(dot(normal, warpedNDF.basisZ));
    float nDotV = saturate(dot(normal, view));
    float3 h = normalize(warpedNDF.basisZ + view);

    // The visibility term is evaluated at the center of our warped BRDF lobe
    output *= GGX_V1(m2, nDotL) * GGX_V1(m2, nDotV);

    // Fresnel evaluated at the center of our warped BRDF lobe
    output *= specAlbedo + (1.0f - specAlbedo) * pow((1.0f - saturate(dot(warpedNDF.basisZ, h))), 5.0f);

    // Fade out spec entirely when lower than 0.1% albedo
    output *= saturate(dot(specAlbedo, float3(333.0f, 333.0f, 333.0f)));

    // Cosine term evaluated at the center of our warped BRDF lobe
    output *= nDotL;
    */

    return max(output, 0.0f);
}

//-------------------------------------------------------------------------------------------------
// Computes an SG sharpness value such that all values within theta radians of the SG axis have
// a value greater than epsilon
//-------------------------------------------------------------------------------------------------
float SGSharpnessFromThreshold(in float amplitude, in float epsilon, in float cosTheta)
{
    return (log(epsilon) - log(amplitude)) / (cosTheta - 1.0f);
}

//-------------------------------------------------------------------------------------------------
// Returns an SG that can serve as an approximation for the incoming radiance from a spherical
// area light source
//-------------------------------------------------------------------------------------------------
SG MakeSphereSG(in float3 lightDir, in float radius, in float3 intensity, in float dist)
{
    SG sg;

    float r2 = radius * radius;
    float d2 = dist * dist;

    float lne = -2.230258509299f; // ln(0.1)
    sg.axis = normalize(lightDir);
    sg.sharpness = (-lne * d2) / r2;
    sg.amplitude = intensity;

    return sg;
}

#endif // !SG_HLSL
