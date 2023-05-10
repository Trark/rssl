/**********************************************************************
Copyright (c) 2023 Advanced Micro Devices, Inc. All rights reserved.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
********************************************************************/

#ifndef LIGHT_EVALUATION_HLSL
#define LIGHT_EVALUATION_HLSL

#include "lights.hlsl"
#include "../math/geometry.hlsl"
#include "../math/math_constants.hlsl"
#include "../math/sampling.hlsl"
#include "../math/pack.hlsl"

/*
// Requires the following data to be defined in any shader that uses this file
TextureCube g_EnvironmentBuffer;
Texture2D g_TextureMaps[] : register(space99);

SamplerState g_TextureSampler;
*/

/**
 * Get the emitted light from a given area light.
 * @param light       The light to be sampled.
 * @param barycentric The sampled barycentric coordinates on the surface.
 * @return The emitted light.
 */
float3 evaluateAreaLight(LightArea light, float2 barycentric)
{
    // Load initial values
    float3 emissivity = light.emissivity.xyz;

    // Get any light maps
    uint emissivityTex = asuint(light.emissivity.w);
    if (emissivityTex != uint(-1))
    {
        // Determine texture UVs
        float2 uv = interpolate(light.uv0, light.uv1, light.uv2, barycentric);
        emissivity *= g_TextureMaps[NonUniformResourceIndex(emissivityTex)].SampleLevel(g_TextureSampler, uv, 0.0f).xyz;
    }
    return emissivity;
}

/**
 * Get the emitted light from a given area light within a ray cone.
 * @param light       The light to be sampled.
 * @param barycentric The sampled barycentric coordinates on the triangle.
 * @param position    Current position on surface light is shining on.
 * @param solidAngle  Solid angle around view direction of visible ray cone.
 * @return The emitted light.
 */
float3 evaluateAreaLightCone(LightArea light, float2 barycentric, float3 position, float solidAngle)
{
    // Load initial values
    float3 emissivity = light.emissivity.xyz;

    // Get any light maps
    uint emissivityTex = asuint(light.emissivity.w);
    if (emissivityTex != uint(-1))
    {
        // Determine texture UVs
        float2 uv = interpolate(light.uv0, light.uv1, light.uv2, barycentric);

        // Determine position on surface of light
        float3 lightPosition = interpolate(light.v0.xyz, light.v1.xyz, light.v2.xyz, barycentric);

        // Approximate ray cone projection (ray tracing gems chapter 20)
        float3 direction = position - lightPosition;
        float directionLength = length(direction);
        direction /= directionLength;
        float width = solidAngle * directionLength;

        float2 edgeUV0 = light.uv1 - light.uv0;
        float2 edgeUV1 = light.uv2 - light.uv0;
        // Get texture dimensions in order to determine LOD of visible solid angle
        float2 size;
        g_TextureMaps[NonUniformResourceIndex(emissivityTex)].GetDimensions(size.x, size.y);
        float areaUV = size.x * size.y * abs(edgeUV0.x * edgeUV1.y - edgeUV1.x * edgeUV0.y);
        float offset = 0.5f * log2(areaUV);

        // Calculate lights surface normal vector
        float3 edge0 = light.v1.xyz - light.v0.xyz;
        float3 edge1 = light.v2.xyz - light.v0.xyz;
        float3 lightNormal = cross(edge0, edge1);
        float lightNormalLength = length(lightNormal);
        offset -= 0.5f * log2(lightNormalLength);
        lightNormal /= lightNormalLength;
        // Calculate surface area of triangle
        float lightArea = 0.5f * lightNormalLength;

        float dotDN = abs(dot(direction, lightNormal));
        width = min(lightArea, width); //Clamp so cannot be greater than actual size of triangle
        float angle = log2(width / dotDN);
        float lod = offset + angle;

        // Calculate texture LOD based on projected area
        emissivity *= g_TextureMaps[NonUniformResourceIndex(emissivityTex)].SampleLevel(g_TextureSampler, uv, lod).xyz;
    }
    return emissivity;
}

/**
 * Get lighting from the scene environment map.
 * @param light     The light to be sampled.
 * @param direction Normalised direction vector used to sample
 * @return The lighting for requested direction.
 */
float3 evaluateEnvironmentLight(LightEnvironment light, float3 direction)
{
    return g_EnvironmentBuffer.SampleLevel(g_TextureSampler, direction, 0.0f).xyz;
}

/**
 * Get lighting from the scene environment map within a ray cone.
 * @param light      The light to be sampled.
 * @param direction  Normalised direction vector used to sample.
 * @param solidAngle Solid angle around view direction of visible ray cone (must be less that 4pi/6 due to cubemap limitations).
 * @return The lighting for requested direction.
 */
float3 evaluateEnvironmentLightCone(LightEnvironment light, float3 direction, float solidAngle)
{
    // Convert solid angle to percentage of total light surface
    const float ratio = solidAngle * INV_FOUR_PI;
    // Calculate LOD based on total texture pixels and the visible percentage
    const float lod = log2(ratio) + light.lods;
    return g_EnvironmentBuffer.SampleLevel(g_TextureSampler, direction, lod).xyz;
}

/**
 * Get the incident light from a given point light.
 * @param light    The light to be sampled.
 * @param position The current surface position.
 * @return The visible light.
 */
float3 evaluatePointLight(LightPoint light, float3 position)
{
    // Calculate the distance between the light and the given point
    float distance = length(light.position - position);
    // The returned value is then attenuated by the lights falloff (modified to prevent issues with distance<1)
    float distMod = distance / light.range;
    float attenuation = saturate(1.0f - (distMod * distMod * distMod * distMod)) / (distance * distance);
    return light.intensity * attenuation.xxx;
}

/**
 * Get the incident light from a given spot light.
 * @param light    The light to be sampled.
 * @param position The current surface position.
 * @return The visible light.
 */
float3 evaluateSpotLight(LightSpot light, float3 position)
{
    // Calculate the distance between the light and the given point
    float3 lightDirectionFull = light.position - position;
    float distance = length(lightDirectionFull);
    float3 lightDirection = lightDirectionFull / distance;
    float lightAngle = dot(light.direction, lightDirection);

    // Cone attenuation
    // With r^2 falloff the intensity=((cos(angle)-cos(outer))/(cos(inner) - cos(outer))^2
    //  these terms are simplified and pre-calculated to allow for using a fma
    float angularAttenuation = saturate(lightAngle * light.angleCutoffScale + light.angleCutoffOffset);
    angularAttenuation *= angularAttenuation;

    // The returned value is then attenuated by the lights falloff (modified to prevent issues with distance<1)
    float distMod = distance / light.range;
    float attenuation = saturate(1.0f - (distMod * distMod * distMod * distMod)) / (distance * distance);
    attenuation *= angularAttenuation;
    return light.intensity * attenuation.xxx;
}

/**
 * Get the incident light from a given directional light.
 * @param light The light to be sampled.
 * @return The visible light.
 */
float3 evaluateDirectionalLight(LightDirectional light)
{
    // Direct lights just return constant irradiance
    return light.irradiance;
}

/**
 * Get the incident light from a given light.
 * @param selectedLight The light to be sampled.
 * @param position      The current surface position.
 * @param direction     Normalised direction vector used to evaluate light.
 * @return The visible light.
 */
float3 evaluateLight(Light selectedLight, float3 position, float3 direction)
{
#ifndef DISABLE_AREA_LIGHTS
#   if !defined(DISABLE_DELTA_LIGHTS) || !defined(DISABLE_ENVIRONMENT_LIGHTS)
    if (selectedLight.get_light_type() == kLight_Area)
#   endif
    {
        // Get the area light
        LightArea light = MakeLightArea(selectedLight);

        // TODO calculate intersection of ray (position, direction) with triangle to calculate UV
        //  This^ is going to be slow so potentially look at storing this value or just stick with the below hack
        float2 lightUV = float2(0.5f, 0.5f);

        // Evaluate the selected area light
        return evaluateAreaLight(light, lightUV);
    }
#   if !defined(DISABLE_DELTA_LIGHTS) || !defined(DISABLE_ENVIRONMENT_LIGHTS)
    else
#   endif
#endif
#ifndef DISABLE_DELTA_LIGHTS
    if (selectedLight.get_light_type() == kLight_Point)
    {
        // Get the point light
        LightPoint light = MakeLightPoint(selectedLight);

        // Evaluate the selected point light
        return evaluatePointLight(light, position);
    }
    else if (selectedLight.get_light_type() == kLight_Spot)
    {
        // Get the spot light
        LightSpot light = MakeLightSpot(selectedLight);

        // Evaluate the selected spot light
        return evaluateSpotLight(light, position);
    }
    else
#   ifndef DISABLE_ENVIRONMENT_LIGHTS
    if (selectedLight.get_light_type() == kLight_Direction)
#   endif
    {
        // Get the directional light
        LightDirectional light = MakeLightDirectional(selectedLight);

        // Evaluate the selected directional light
        return evaluateDirectionalLight(light);
    }
#   ifndef DISABLE_ENVIRONMENT_LIGHTS
    else
#   endif
#endif
#ifndef DISABLE_ENVIRONMENT_LIGHTS
    /*selectedLight.get_light_type() == kLight_Environment*/
    {
        // Get the environment light
        LightEnvironment light = MakeLightEnvironment(selectedLight);

        // Evaluate the environment map
        return evaluateEnvironmentLight(light, direction);
    }
#endif
#if defined(DISABLE_AREA_LIGHTS) && defined(DISABLE_DELTA_LIGHTS) && defined(DISABLE_ENVIRONMENT_LIGHTS)
    return 0.0f.xxx;
#endif
}

/**
 * Get the incident light from a given light within a ray cone.
 * @param selectedLight The light to be sampled.
 * @param position      The current surface position.
 * @param direction     Normalised direction vector used to evaluate light.
 * @param solidAngle    Solid angle around view direction of visible ray cone.
 * @return The visible light.
 */
float3 evaluateLightCone(Light selectedLight, float3 position, float3 direction, float solidAngle)
{
#ifndef DISABLE_AREA_LIGHTS
#   if !defined(DISABLE_DELTA_LIGHTS) || !defined(DISABLE_ENVIRONMENT_LIGHTS)
    if (selectedLight.get_light_type() == kLight_Area)
#   endif
    {
        // Get the area light
        LightArea light = MakeLightArea(selectedLight);

        // TODO calculate intersection of ray (position, direction) with triangle to calculate UV
        //  This^ is going to be slow so potentially look at storing this value or just stick with the below hack
        float2 lightUV = float2(0.5f, 0.5f);

        // Evaluate the selected area light
        return evaluateAreaLightCone(light, lightUV, position, solidAngle);
    }
#   if !defined(DISABLE_DELTA_LIGHTS) || !defined(DISABLE_ENVIRONMENT_LIGHTS)
    else
#   endif
#endif
#ifndef DISABLE_DELTA_LIGHTS
    if (selectedLight.get_light_type() == kLight_Point)
    {
        // Get the point light
        LightPoint light = MakeLightPoint(selectedLight);

        // Evaluate the selected point light
        return evaluatePointLight(light, position);
    }
    else if (selectedLight.get_light_type() == kLight_Spot)
    {
        // Get the spot light
        LightSpot light = MakeLightSpot(selectedLight);

        // Evaluate the selected spot light
        return evaluateSpotLight(light, position);
    }
    else
#   ifndef DISABLE_ENVIRONMENT_LIGHTS
    if (selectedLight.get_light_type() == kLight_Direction)
#   endif
    {
        // Get the directional light
        LightDirectional light = MakeLightDirectional(selectedLight);

        // Evaluate the selected directional light
        return evaluateDirectionalLight(light);
    }
#   ifndef DISABLE_ENVIRONMENT_LIGHTS
    else
#   endif
#endif
#ifndef DISABLE_ENVIRONMENT_LIGHTS
    /*selectedLight.get_light_type() == kLight_Environment*/
    {
        // Get the environment light
        LightEnvironment light = MakeLightEnvironment(selectedLight);

        // Evaluate the environment map
        return evaluateEnvironmentLightCone(light, direction, solidAngle);
    }
#endif
#if defined(DISABLE_AREA_LIGHTS) && defined(DISABLE_DELTA_LIGHTS) && defined(DISABLE_ENVIRONMENT_LIGHTS)
    return 0.0f.xxx;
#endif
}

/**
 * Get the incident light from a sampled light using a uv value returned from @sampleLightUnorm
 * @param selectedLight  The light that was sampled.
 * @param position       Current position on surface to get direction from.
 * @param sampleParams   UV values returned from @sampleLightUnorm.
 * @param lightDirection (Out) The direction to the sampled light.
 * @param lightPosition  (Out) The position of the sampled light (contains invalid data in case of directional or environment lights).
 * @return The visible light.
 */
float3 evaluateLightSampled(Light selectedLight, float3 position, float2 sampleParams, out float3 lightDirection, out float3 lightPosition)
{
#ifndef DISABLE_AREA_LIGHTS
#   if !defined(DISABLE_DELTA_LIGHTS) || !defined(DISABLE_ENVIRONMENT_LIGHTS)
    if (selectedLight.get_light_type() == kLight_Area)
#   endif
    {
        // Get the area light
        LightArea light = MakeLightArea(selectedLight);
        // Calculate direction
        lightPosition = interpolate(light.v0.xyz, light.v1.xyz, light.v2.xyz, sampleParams);
        lightDirection = normalize(lightPosition - position);
        // Evaluate the selected area light
        return evaluateAreaLight(light, sampleParams);
    }
#   if !defined(DISABLE_DELTA_LIGHTS) || !defined(DISABLE_ENVIRONMENT_LIGHTS)
    else
#   endif
#endif
#ifndef DISABLE_DELTA_LIGHTS
    if (selectedLight.get_light_type() == kLight_Point)
    {
        // Get the point light
        LightPoint light = MakeLightPoint(selectedLight);
        // Calculate direction
        lightPosition = light.position.xyz;
        lightDirection = normalize(lightPosition - position);
        // Evaluate the selected point light
        return evaluatePointLight(light, position);
    }
    else if (selectedLight.get_light_type() == kLight_Spot)
    {
        // Get the spot light
        LightSpot light = MakeLightSpot(selectedLight);
        // Calculate direction
        lightPosition = light.position.xyz;
        lightDirection = normalize(lightPosition - position);
        // Evaluate the selected spot light
        return evaluateSpotLight(light, position);
    }
    else
#   ifndef DISABLE_ENVIRONMENT_LIGHTS
    if (selectedLight.get_light_type() == kLight_Direction)
#   endif
    {
        // Get the directional light
        LightDirectional light = MakeLightDirectional(selectedLight);
        lightDirection = light.direction.xyz;
        // Evaluate the selected directional light
        return evaluateDirectionalLight(light);
    }
#   ifndef DISABLE_ENVIRONMENT_LIGHTS
    else
#   endif
#endif
#ifndef DISABLE_ENVIRONMENT_LIGHTS
    /*selectedLight.get_light_type() == kLight_Environment*/
    {
        // Convert stored uv back to direction
        lightDirection = MapToSphere(sampleParams);
        // Get the environment light
        LightEnvironment light = MakeLightEnvironment(selectedLight);
        // Evaluate the environment map
        return evaluateEnvironmentLight(light, lightDirection);
    }
#endif
#if defined(DISABLE_AREA_LIGHTS) && defined(DISABLE_DELTA_LIGHTS) && defined(DISABLE_ENVIRONMENT_LIGHTS)
    lightDirection = 0.0f.xxx;
    lightPosition = 0.0f.xxx;
    return 0.0f.xxx;
#endif
}

/**
 * Get the incident light from a sampled light using a uv value returned from @sampleLightUnorm
 * @param selectedLight  The light that was sampled.
 * @param position       Current position on surface to get direction from.
 * @param sampleParams   UV values returned from @sampleLightUnorm.
 * @param solidAngle     Solid angle of visible light surface, used for evaluating across light surface.
 * @param lightDirection (Out) The direction to the sampled light.
 * @param lightPosition  (Out) The position of the sampled light (contains invalid data in case of directional or environment lights).
 * @return The visible light.
 */
float3 evaluateLightConeSampled(Light selectedLight, float3 position, float2 sampleParams, float solidAngle, out float3 lightDirection, out float3 lightPosition)
{
#ifndef DISABLE_AREA_LIGHTS
#   if !defined(DISABLE_DELTA_LIGHTS) || !defined(DISABLE_ENVIRONMENT_LIGHTS)
    if (selectedLight.get_light_type() == kLight_Area)
#   endif
    {
        // Get the area light
        LightArea light = MakeLightArea(selectedLight);
        // Calculate direction
        lightPosition = interpolate(light.v0.xyz, light.v1.xyz, light.v2.xyz, sampleParams);
        lightDirection = normalize(lightPosition - position);
        // Evaluate the selected area light
        return evaluateAreaLightCone(light, sampleParams, position, solidAngle);
    }
#   if !defined(DISABLE_DELTA_LIGHTS) || !defined(DISABLE_ENVIRONMENT_LIGHTS)
    else
#   endif
#endif
#ifndef DISABLE_DELTA_LIGHTS
    if (selectedLight.get_light_type() == kLight_Point)
    {
        // Get the point light
        LightPoint light = MakeLightPoint(selectedLight);
        // Calculate direction
        lightPosition = light.position.xyz;
        lightDirection = normalize(lightPosition - position);
        // Evaluate the selected point light
        return evaluatePointLight(light, position);
    }
    else if (selectedLight.get_light_type() == kLight_Spot)
    {
        // Get the spot light
        LightSpot light = MakeLightSpot(selectedLight);
        // Calculate direction
        lightPosition = light.position.xyz;
        lightDirection = normalize(lightPosition - position);
        // Evaluate the selected spot light
        return evaluateSpotLight(light, position);
    }
    else
#   ifndef DISABLE_ENVIRONMENT_LIGHTS
    if (selectedLight.get_light_type() == kLight_Direction)
#   endif
    {
        // Get the directional light
        LightDirectional light = MakeLightDirectional(selectedLight);
        lightDirection = light.direction.xyz;
        // Evaluate the selected directional light
        return evaluateDirectionalLight(light);
    }
#   ifndef DISABLE_ENVIRONMENT_LIGHTS
    else
#   endif
#endif
#ifndef DISABLE_ENVIRONMENT_LIGHTS
    /*selectedLight.get_light_type() == kLight_Environment*/
    {
        // Convert stored uv back to direction
        lightDirection = MapToSphere(sampleParams);
        // Get the environment light
        LightEnvironment light = MakeLightEnvironment(selectedLight);
        // Evaluate the environment map
        return evaluateEnvironmentLightCone(light, lightDirection, solidAngle);
    }
#endif
#if defined(DISABLE_AREA_LIGHTS) && defined(DISABLE_DELTA_LIGHTS) && defined(DISABLE_ENVIRONMENT_LIGHTS)
    lightDirection = 0.0f.xxx;
    lightPosition = 0.0f.xxx;
    return 0.0f.xxx;
#endif
}

#endif // LIGHT_EVALUATION_HLSL
