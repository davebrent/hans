#version 150

in vec2 uv;
out vec4 color;

uniform sampler2D u_texture;
// uniform vec2 resolution;
// uniform float pixelSize;

void main() {
  vec2 resolution = vec2(4.0);
  float pixelSize = 8.0;

  vec2 p = uv;

  float pixelsPerRow = resolution.x / pixelSize;
  float pixelsPerCol = resolution.y / pixelSize;

  float pixelSizeX = 1.0 / pixelsPerRow;
  float dx = mod(p.x, pixelSizeX) - pixelSizeX *0.5;
  float pixelSizeY = 1.0 / pixelsPerCol;
  float dy = mod(p.y, pixelSizeY) - pixelSizeY * 0.5;
  float pixelSize = pixelSizeX;

  p.x -= dx;
  p.y -= dy;

  vec3 col = texture(u_texture, p).rgb;
  vec3 luma = vec3(0.299, 0.587, 0.114);
  float bright = dot(col.rgb, luma);

  float dist = sqrt(dx * dx + dy * dy);
  float rad = bright * pixelSize * 1.0;
  float m = step(dist, rad);

  vec3 col2 = mix(vec3(0.0), vec3(1.0), m);
  color = vec4(col2, 1.0);
}
