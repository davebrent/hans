#version 400

in vec2 uv;
out vec4 color;

uniform sampler2D image;
uniform sampler2D displace;

void main () {
  vec2 amount = (texture(displace, uv).xy * 1.0) - 0.5;
  color = texture(image, uv + amount);
}
