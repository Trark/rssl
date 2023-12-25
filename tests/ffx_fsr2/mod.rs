use super::compile_file;

pub const FILES: &[(&str, &str)] = &[
    source!("ffx_common_types.h"),
    source!("ffx_core.h"),
    source!("ffx_core_cpu.h"),
    source!("ffx_core_gpu_common.h"),
    source!("ffx_core_gpu_common_half.h"),
    source!("ffx_core_hlsl.h"),
    source!("ffx_core_portability.h"),
    source!("ffx_fsr1.h"),
    source!("ffx_fsr2_accumulate.h"),
    source!("ffx_fsr2_accumulate_pass.hlsl"),
    source!("ffx_fsr2_autogen_reactive_pass.hlsl"),
    source!("ffx_fsr2_callbacks_hlsl.h"),
    source!("ffx_fsr2_common.h"),
    source!("ffx_fsr2_compute_luminance_pyramid.h"),
    source!("ffx_fsr2_compute_luminance_pyramid_pass.hlsl"),
    source!("ffx_fsr2_depth_clip.h"),
    source!("ffx_fsr2_depth_clip_pass.hlsl"),
    source!("ffx_fsr2_lock.h"),
    source!("ffx_fsr2_lock_pass.hlsl"),
    source!("ffx_fsr2_postprocess_lock_status.h"),
    source!("ffx_fsr2_rcas.h"),
    source!("ffx_fsr2_rcas_pass.hlsl"),
    source!("ffx_fsr2_reconstruct_dilated_velocity_and_previous_depth.h"),
    source!("ffx_fsr2_reconstruct_previous_depth_pass.hlsl"),
    source!("ffx_fsr2_reproject.h"),
    source!("ffx_fsr2_resources.h"),
    source!("ffx_fsr2_sample.h"),
    source!("ffx_fsr2_tcr_autogen.h"),
    source!("ffx_fsr2_tcr_autogen_pass.hlsl"),
    source!("ffx_fsr2_upsample.h"),
    source!("ffx_spd.h"),
];

#[test]
fn compile_accumulate_pass() {
    compile_file("ffx_fsr2_accumulate_pass.hlsl", FILES);
}

#[test]
fn compile_autogen_reactive_pass() {
    compile_file("ffx_fsr2_autogen_reactive_pass.hlsl", FILES);
}

#[test]
fn compile_compute_luminance_pyramid_pass() {
    compile_file("ffx_fsr2_compute_luminance_pyramid_pass.hlsl", FILES);
}

#[test]
fn compile_depth_clip_pass() {
    compile_file("ffx_fsr2_depth_clip_pass.hlsl", FILES);
}

#[test]
fn compile_lock_pass() {
    compile_file("ffx_fsr2_lock_pass.hlsl", FILES);
}

#[test]
fn compile_rcas_pass() {
    compile_file("ffx_fsr2_rcas_pass.hlsl", FILES);
}

#[test]
fn compile_reconstruct_previous_depth_pass() {
    compile_file("ffx_fsr2_reconstruct_previous_depth_pass.hlsl", FILES);
}

#[test]
fn compile_tcr_autogen_pass() {
    compile_file("ffx_fsr2_tcr_autogen_pass.hlsl", FILES);
}
