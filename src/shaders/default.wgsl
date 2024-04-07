@vertex
fn vs_main(@builtin(vertex_index) vertex_index: u32) -> @builtin(position) vec4f {
    const pos = array(
        vec2(0.0, 0.5),
        vec2(-0.5, -0.5),
        vec2(0.5, -0.5)
    );

    return vec4f(0.0, 0.0, 0, 0);
}

@fragment
fn fs_main() -> @location(0) vec4f {
    return vec4(1, 1, 1, 1);
}
