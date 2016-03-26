#version 150

in vec2 uv;
out vec4 color;

uniform sampler2D u_texture;

void main() {
  vec3 luma = vec3(0.299, 0.587, 0.114);
  vec4 rgb = texture(u_texture, uv);
  color = vec4(vec3(dot(rgb.rgb, luma)), rgb.a);
}
