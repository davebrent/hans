#version 330 core

in vec3 v_position;
in vec3 v_normal;
// out vec4 color;
layout(location = 0) out vec4 color;
layout(location = 1) out vec4 normals;

uniform vec3 u_color;

void main() {
  color = vec4(u_color + (((v_normal + 1.0) * 0.5) * 0.5), 1.0);
  normals = vec4(v_normal, 1.0);
}
