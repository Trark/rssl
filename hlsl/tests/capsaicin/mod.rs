use super::compile_file;

pub const FILES: &[(&str, &str)] = &[
    source!("capsaicin/convolve_ibl.comp"),
    source!("capsaicin/convolve_ibl.comp"),
    source!("capsaicin/convolve_ibl.frag"),
    source!("capsaicin/convolve_ibl.vert"),
    source!("capsaicin/dump_copy_aov_to_buffer.comp"),
    source!("components/blue_noise_sampler/blue_noise_sampler.hlsl"),
    source!("components/light_sampler/gather_area_lights.geom"),
    source!("components/light_sampler/gather_area_lights.vert"),
    source!("components/light_sampler/light_sampler_uniform.hlsl"),
    source!("components/light_sampler/light_sampler.hlsl"),
    source!("components/light_sampler_bounds/light_sampler_bounds_shared.h"),
    source!("components/light_sampler_bounds/light_sampler_bounds.comp"),
    source!("components/light_sampler_bounds/light_sampler_bounds.hlsl"),
    source!("components/stratified_sampler/stratified_sampler.hlsl"),
    source!("lights/light_evaluation.hlsl"),
    source!("lights/light_sampling_volume.hlsl"),
    source!("lights/light_sampling.hlsl"),
    source!("lights/lights_shared.h"),
    source!("lights/lights.hlsl"),
    source!("lights/reservoir.hlsl"),
    source!("materials/material_evaluation.hlsl"),
    source!("materials/material_sampling.hlsl"),
    source!("materials/materials.hlsl"),
    source!("math/sampling.hlsl"),
    source!("math/color.hlsl"),
    source!("math/geometry.hlsl"),
    source!("math/hash.hlsl"),
    source!("math/math_constants.hlsl"),
    source!("math/math.hlsl"),
    source!("math/pack.hlsl"),
    source!("math/quaternion.hlsl"),
    source!("math/random.hlsl"),
    source!("math/sampling.hlsl"),
    source!("math/spherical_harmonics.hlsl"),
    source!("render_techniques/atmosphere/atmosphere.comp"),
    source!("render_techniques/atmosphere/atmosphere.hlsl"),
    source!("render_techniques/gi10/gi_denoiser.hlsl"),
    source!("render_techniques/gi10/gi10_shared.h"),
    source!("render_techniques/gi10/gi10.comp"),
    source!("render_techniques/gi10/gi10.frag"),
    source!("render_techniques/gi10/gi10.hlsl"),
    source!("render_techniques/gi10/gi10.vert"),
    source!("render_techniques/gi10/hash_grid_cache.hlsl"),
    source!("render_techniques/gi10/screen_probes.hlsl"),
    source!("render_techniques/gi10/world_space_restir.hlsl"),
    source!("render_techniques/path_tracer/reference_pt_shared.h"),
    source!("render_techniques/path_tracer/reference_pt.comp"),
    source!("render_techniques/skybox/skybox.frag"),
    source!("render_techniques/skybox/skybox.vert"),
    source!("render_techniques/ssgi/ssgi_debug.comp"),
    source!("render_techniques/ssgi/ssgi_shared.h"),
    source!("render_techniques/ssgi/ssgi.comp"),
    source!("render_techniques/taa/taa.comp"),
    source!("render_techniques/taa/update_history.comp"),
    source!("render_techniques/tone_mapping/tone_mapping.comp"),
    source!("render_techniques/visibility_buffer/disocclusion_mask.comp"),
    source!("render_techniques/visibility_buffer/visibility_buffer.frag"),
    source!("render_techniques/visibility_buffer/visibility_buffer.vert"),
    source!("utilities/gpu_reduce.comp"),
    source!("utilities/gpu_sort.comp"),
    source!("utilities/FFX_ParallelSort.h"),
    source!("gpu_shared.h"),
    source!("mesh.hlsl"),
];

#[test]
fn compile_convolve_ibl_comp() {
    compile_file("capsaicin/convolve_ibl.comp", FILES);
}

#[test]
fn compile_convolve_ibl_frag() {
    compile_file("capsaicin/convolve_ibl.frag", FILES);
}

#[test]
fn compile_convolve_ibl_vert() {
    compile_file("capsaicin/convolve_ibl.vert", FILES);
}

#[test]
fn compile_dump_copy_aov_to_buffer() {
    compile_file("capsaicin/dump_copy_aov_to_buffer.comp", FILES);
}

#[test]
fn compile_gather_area_lights_geom() {
    compile_file("components/light_sampler/gather_area_lights.geom", FILES);
}

#[test]
fn compile_gather_area_lights_vert() {
    compile_file("components/light_sampler/gather_area_lights.vert", FILES);
}

#[test]
fn compile_light_sampler_bounds() {
    compile_file(
        "components/light_sampler_bounds/light_sampler_bounds.comp",
        FILES,
    );
}

#[test]
fn compile_atmosphere() {
    compile_file("render_techniques/atmosphere/atmosphere.comp", FILES);
}

#[test]
fn compile_gi10_comp() {
    compile_file("render_techniques/gi10/gi10.comp", FILES);
}

#[test]
fn compile_gi10_frag() {
    compile_file("render_techniques/gi10/gi10.frag", FILES);
}

#[test]
fn compile_gi10_vert() {
    compile_file("render_techniques/gi10/gi10.vert", FILES);
}

#[test]
fn compile_skybox_frag() {
    compile_file("render_techniques/skybox/skybox.frag", FILES);
}

#[test]
fn compile_skybox_vert() {
    compile_file("render_techniques/skybox/skybox.vert", FILES);
}

#[test]
fn compile_ssgi_debug() {
    compile_file("render_techniques/ssgi/ssgi_debug.comp", FILES);
}

#[test]
fn compile_ssgi() {
    compile_file("render_techniques/ssgi/ssgi.comp", FILES);
}

#[test]
fn compile_taa() {
    compile_file("render_techniques/taa/taa.comp", FILES);
}

#[test]
fn compile_update_history() {
    compile_file("render_techniques/taa/update_history.comp", FILES);
}

#[test]
fn compile_tone_mapping() {
    compile_file("render_techniques/tone_mapping/tone_mapping.comp", FILES);
}

#[test]
fn compile_disocclusion_mask() {
    compile_file(
        "render_techniques/visibility_buffer/disocclusion_mask.comp",
        FILES,
    );
}

#[test]
fn compile_visibility_buffer_frag() {
    compile_file(
        "render_techniques/visibility_buffer/visibility_buffer.frag",
        FILES,
    );
}

#[test]
fn compile_visibility_buffer_vert() {
    compile_file(
        "render_techniques/visibility_buffer/visibility_buffer.vert",
        FILES,
    );
}

#[test]
fn compile_gpu_reduce() {
    compile_file("utilities/gpu_reduce.comp", FILES);
}
