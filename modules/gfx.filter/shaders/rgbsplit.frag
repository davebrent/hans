#version 150

in vec2 uv;
out vec4 color;

uniform sampler2D u_texture;
// Delta is the amount to shift the channels by
// uniform vec2 delta;

// Resolution is the width & height of the input texture
uniform vec2 u_resolution;

void main() {
  vec2 delta = vec2(50.0);
  vec2 dir = uv - vec2(0.5);
  float d = 0.7 * length(dir);
  normalize(dir);
  vec2 value = d * dir * delta;

  vec4 c1 = texture(u_texture, uv - value / u_resolution.x);
  vec4 c2 = texture(u_texture, uv);
  vec4 c3 = texture(u_texture, uv + value / u_resolution.y);

  color = vec4(c1.r, c2.g, c3.b, c1.a + c2.a + c3.b);
}
