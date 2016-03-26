#version 150

in vec2 uv;
out vec4 color;

uniform sampler2D u_texture;
uniform float u_amount;

void main() {
  vec4 value = texture(u_texture, uv);
  color.a = value.a;
  color.rgb = pow(value.rgb, vec3(1.0 / 2.2));
}
