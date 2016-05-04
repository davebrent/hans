#version 150

in vec2 uv;
out vec4 color;

uniform sampler2D u_texture;
uniform float u_amount;

void main() {
  vec4 c = texture(u_texture, uv);
  float r = c.r;
  float g = c.g;
  float b = c.b;
  c.r = min(1.0, (r * (1.0 - (0.607 * u_amount))) + (g * (0.769 * u_amount)) + (b * (0.189 * u_amount)));
  c.g = min(1.0, (r * 0.349 * u_amount) + (g * (1.0 - (0.314 * u_amount))) + (b * 0.168 * u_amount));
  c.b = min(1.0, (r * 0.272 * u_amount) + (g * 0.534 * u_amount) + (b * (1.0 - (0.869 * u_amount))));
  color = c;
}
