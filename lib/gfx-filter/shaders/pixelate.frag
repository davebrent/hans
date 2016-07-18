#version 150

in vec2 uv;
out vec4 color;

uniform sampler2D u_texture;
uniform vec2 u_resolution;
uniform float u_amount;

void main() {
  float d = 1.0 / u_amount;
  float ar = u_resolution.x / u_resolution.y;

  float u = floor(uv.x / d) * d;
  d = ar / u_amount;
  float v = floor(uv.y / d) * d;

  color = texture(u_texture, vec2(u, v));
}
