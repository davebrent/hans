#version 400

in vec2 uv;
out vec4 color;

uniform vec3 u_color;

void main () {
  //bool xe = abs(1.0 - uv.x) < 0.03 || abs(1.0 - uv.x) > 0.97;
  //bool ye = abs(1.0 - uv.y) < 0.03 || abs(1.0 - uv.y) > 0.97;

  //vec4 c = vec4(u_color.r, u_color.g, u_color.b, 0.6);
  //color = mix(c, vec4(1.0), float(xe || ye));
   color = vec4(uv.x * u_color.r, u_color.g, uv.y * u_color.b, 1.0);
}
