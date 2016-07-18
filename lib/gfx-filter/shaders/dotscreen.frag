#version 150

uniform sampler2D u_texture;
uniform vec2 u_resolution;

in vec2 uv;
out vec4 color;

vec2 center = .5 * u_resolution;
float angle = 1.57;
float scale = 1.;

float pattern() {
  float s = sin( angle ), c = cos( angle );
  vec2 tex = uv * u_resolution - center;
  vec2 point = vec2( c * tex.x - s * tex.y, s * tex.x + c * tex.y ) * scale;
  return ( sin( point.x ) * sin( point.y ) ) * 4.0;
}

void main() {
  vec4 c = texture( u_texture, uv );
  float average = ( c.r + c.g + c.b ) / 3.0;
  color = vec4( vec3( average * 10.0 - 5.0 + pattern() ), c.a );
}
