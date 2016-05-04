#version 150

in vec2 uv;
out vec4 color;

uniform sampler2D u_texture;
uniform vec2 u_resolution;

void main() {
  color = texture(u_texture, uv);
  color.rgb = 1.0 - color.rgb;
}
