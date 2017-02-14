#version 400

in vec2 uv;
out vec4 color;

uniform sampler2D y_plane;
uniform sampler2D u_plane;
uniform sampler2D v_plane;

void main() {
  vec2 _uv = vec2(uv.x, 1.0 - uv.y);

  float y = texture(y_plane, _uv).r;
  float u = texture(u_plane, _uv).r;
  float v = texture(v_plane, _uv).r;

  y = 1.1643 * (y - 0.0625);
  u = u - 0.5;
  v = v - 0.5;

  float r = y + 1.5958 * v;
  float g = y - 0.39173 * u - 0.81290 * v;
  float b = y + 2.017 * u;

  color = vec4(r, g, b, 1.0);
}
